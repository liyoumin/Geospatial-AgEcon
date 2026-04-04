library(readxl)
library(dplyr)
library(stringr)
library(tidyr)
library(readr)

in_file  <- "raw_state.xlsx"
out_file <- "state_panel0.csv"

# ---- 1) Clean Value: convert (D)/(H)/(L)/etc to NA, remove commas, numeric ----
clean_value <- function(x) {
  x <- as.character(x)
  x <- str_trim(x)
  x[x %in% c("(D)", "(Z)", "(NA)", "(S)", "(H)", "(L)", "NA", "", "NULL")] <- NA_character_
  x <- str_replace_all(x, ",", "")
  suppressWarnings(as.numeric(x))
}

# ---- 2) Read and standardize ----
raw <- read_excel(in_file) %>%
  rename_with(str_trim)

dat <- raw %>%
  filter(toupper(`Geo Level`) == "STATE") %>%   # ensure state level only
  transmute(
    year   = as.integer(Year),
    state  = str_to_title(as.character(State)),
    statefp = str_pad(as.character(`State ANSI`), width = 2, side = "left", pad = "0"),
    commodity = str_to_upper(as.character(Commodity)),
    data_item = str_to_upper(str_squish(as.character(`Data Item`))),
    value = clean_value(Value)
  )

# ---- 3) Map each Data Item to a clean variable name ----
# (You can add/remove items here as needed.)
dat2 <- dat %>%
  mutate(var = case_when(
    # HAY (alfalfa)
    data_item == "HAY, ALFALFA - ACRES HARVESTED" ~ "alf_acres",
    data_item == "HAY, ALFALFA - PRODUCTION, MEASURED IN TONS" ~ "alf_production",
    data_item == "HAY, ALFALFA, IRRIGATED - ACRES HARVESTED" ~ "irrigated_acres",
    data_item == "HAY, ALFALFA, NON-IRRIGATED - ACRES HARVESTED" ~ "non_irrigated_acres",
    data_item == "HAY, ALFALFA, IRRIGATED - PRODUCTION, MEASURED IN TONS" ~ "irrigated_production",
    data_item == "HAY, ALFALFA, NON-IRRIGATED - PRODUCTION, MEASURED IN TONS" ~ "non_irrigated_production",
    data_item == "HAY, ALFALFA - YIELD, MEASURED IN TONS / ACRE" ~ "alf_yield_usda",
    data_item == "HAY, ALFALFA, IRRIGATED - YIELD, MEASURED IN TONS / ACRE" ~ "irr_yield_usda",
    data_item == "HAY, ALFALFA, NON-IRRIGATED - YIELD, MEASURED IN TONS / ACRE" ~ "non_irr_yield_usda",
    
    # HAYLAGE (alfalfa) — optional but often useful
    data_item == "HAYLAGE, ALFALFA - ACRES HARVESTED" ~ "haylage_acres",
    data_item == "HAYLAGE, ALFALFA - PRODUCTION, MEASURED IN TONS" ~ "haylage_production",
    data_item == "HAYLAGE, ALFALFA, IRRIGATED - ACRES HARVESTED" ~ "haylage_irrigated_acres",
    data_item == "HAYLAGE, ALFALFA, IRRIGATED - OPERATIONS WITH AREA HARVESTED" ~ "haylage_irrigated_ops",
    
    TRUE ~ NA_character_
  )) %>%
  filter(!is.na(var)) %>%
  select(state, statefp, year, var, value)

# ---- 4) Wide panel: 1 row per state-year ----
state_panel0 <- dat2 %>%
  group_by(state, statefp, year, var) %>%
  summarise(value = sum(value, na.rm = TRUE), .groups = "drop") %>%  # safe if duplicates exist
  pivot_wider(names_from = var, values_from = value) %>%
  arrange(statefp, year)

# ---- 5) Fill missing non-irrigated (if not in file) using total - irrigated (when possible) ----
state_panel0 <- state_panel0 %>%
  mutate(
    non_irrigated_acres = if_else(
      is.na(non_irrigated_acres) & !is.na(alf_acres) & !is.na(irrigated_acres),
      pmax(alf_acres - irrigated_acres, 0),
      non_irrigated_acres
    ),
    non_irrigated_production = if_else(
      is.na(non_irrigated_production) & !is.na(alf_production) & !is.na(irrigated_production),
      pmax(alf_production - irrigated_production, 0),
      non_irrigated_production
    )
  )

# ---- 6) Compute irrigation share (state-level ratio) ----
# NOTE: With state totals only, this is acreage-weighted: irrigated_acres / alf_acres
state_panel0 <- state_panel0 %>%
  mutate(
    irr_share = if_else(!is.na(irrigated_acres) & !is.na(alf_acres) & alf_acres > 0,
                        irrigated_acres / alf_acres, NA_real_)
  )


# ---- 7) Compute yields; if USDA yield missing, fill using production/acres ----
state_panel0 <- state_panel0 %>%
  mutate(
    alf_yield = if_else(!is.na(alf_yield_usda), alf_yield_usda,
                        if_else(!is.na(alf_production) & !is.na(alf_acres) & alf_acres > 0,
                                alf_production / alf_acres, NA_real_)),
    
    irr_yield = if_else(!is.na(irr_yield_usda), irr_yield_usda,
                        if_else(!is.na(irrigated_production) & !is.na(irrigated_acres) & irrigated_acres > 0,
                                irrigated_production / irrigated_acres, NA_real_)),
    
    non_irr_yield = if_else(!is.na(non_irr_yield_usda), non_irr_yield_usda,
                            if_else(!is.na(non_irrigated_production) & !is.na(non_irrigated_acres) & non_irrigated_acres > 0,
                                    non_irrigated_production / non_irrigated_acres, NA_real_))
  ) %>%
  select(
    state, statefp, year,
    alf_acres, irrigated_acres, non_irrigated_acres,
    alf_production, irrigated_production, non_irrigated_production,
    irr_share,
    alf_yield, irr_yield, non_irr_yield,
    haylage_acres, haylage_production, haylage_irrigated_acres, haylage_irrigated_ops
  )

# ---- 8) Export ----
write_csv(state_panel0, out_file)

out_file

###
library(dplyr)
library(readr)

# ---- read data ----
df <- read_csv("df.csv", show_col_types = FALSE)
state_panel0 <- read_csv("state_panel0.csv", show_col_types = FALSE)
county_state_panel <- read_csv("county_state_panel.csv", show_col_types = FALSE)

# ---- helper functions ----
bad_text <- c("", "na", "n/a", "null", "d", ".", "nan", "inf", "-inf")

clean_num <- function(x) {
  x <- trimws(as.character(x))
  x[tolower(x) %in% bad_text] <- NA_character_
  x <- gsub(",", "", x)
  out <- suppressWarnings(as.numeric(x))
  out[!is.finite(out)] <- NA_real_
  out
}

norm_key <- function(dat) {
  dat %>%
    mutate(
      state = trimws(as.character(state)),
      year  = suppressWarnings(as.integer(as.character(year)))
    )
}

# ---- normalize keys ----
df <- norm_key(df)
state_panel0 <- norm_key(state_panel0)
county_state_panel <- norm_key(county_state_panel)

# ---- variables that can be filled into df ----
state_vars  <- setdiff(intersect(names(df), names(state_panel0)), c("state", "year"))
county_vars <- setdiff(intersect(names(df), names(county_state_panel)), c("state", "year"))
fill_vars   <- union(state_vars, county_vars)

# irrigated yield variables: use the larger value from the two source tables
irrigated_yield_vars <- intersect(
  fill_vars,
  c(
    "alf_irrigated_yield_tons_per_acre",
    "irrigated_yield_tons_per_acre_dry",
    "irrigated_yield_tons_per_acre_dry_basis_gravity",
    "irrigated_yield_tons_per_acre_dry_basis_pressure"
  )
)

# ---- clean numeric values ----
df <- df %>%
  mutate(across(any_of(fill_vars), clean_num))

state_panel0 <- state_panel0 %>%
  mutate(across(any_of(state_vars), clean_num))

county_state_panel <- county_state_panel %>%
  mutate(across(any_of(county_vars), clean_num))

# ---- join source tables to df ----
df2 <- df %>%
  left_join(
    state_panel0 %>%
      select(state, year, all_of(state_vars)) %>%
      rename_with(~ paste0(.x, "__state"), all_of(state_vars)),
    by = c("state", "year")
  ) %>%
  left_join(
    county_state_panel %>%
      select(state, year, all_of(county_vars)) %>%
      rename_with(~ paste0(.x, "__county"), all_of(county_vars)),
    by = c("state", "year")
  )

# ---- fill values ----
for (v in fill_vars) {
  
  s_col <- paste0(v, "__state")
  c_col <- paste0(v, "__county")
  
  s_val <- if (s_col %in% names(df2)) df2[[s_col]] else rep(NA_real_, nrow(df2))
  c_val <- if (c_col %in% names(df2)) df2[[c_col]] else rep(NA_real_, nrow(df2))
  
  old_val <- df2[[v]]
  need_fill <- is.na(old_val) | old_val == 0
  
  # normal variables: prefer state_panel0, then county_state_panel
  new_regular <- coalesce(s_val, c_val)
  
  # irrigated yield variables: use the larger valid value
  new_max <- pmax(s_val, c_val, na.rm = TRUE)
  new_max[is.infinite(new_max)] <- NA_real_
  
  df2[[v]] <- if (v %in% irrigated_yield_vars) {
    ifelse(need_fill, new_max, old_val)
  } else {
    ifelse(need_fill, new_regular, old_val)
  }
}

# ---- drop helper columns ----
df_out <- df2 %>%
  select(-ends_with("__state"), -ends_with("__county"))

# ---- optional: summary of updates ----
update_summary <- tibble(
  variable = fill_vars,
  n_updated = sapply(fill_vars, function(v) {
    old <- df[[v]]
    new <- df_out[[v]]
    sum((is.na(old) & !is.na(new)) | (!is.na(old) & !is.na(new) & old != new), na.rm = TRUE)
  })
) %>%
  arrange(desc(n_updated))

print(update_summary)

# ---- save ----
write_csv(df_out, "df_filled.csv")
write_csv(update_summary, "df_fill_summary.csv")

