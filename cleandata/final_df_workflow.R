
# ============================================================
# Build final state-year alfalfa panel
# ============================================================

library(dplyr)
library(tidyr)
library(readr)
library(stringr)
library(tibble)

# ============================================================
# 0. File paths
# ============================================================

base_dir <- "C:/Users/liyoumin/Desktop/Alfalfa drought"

state_panel_path  <- file.path(base_dir, "state_panel.csv")
county_panel_path <- file.path(base_dir, "county/county_state_panel.csv")
spei_path         <- file.path(base_dir, "alfalfa_spei_2005_2025.csv")
export_path       <- file.path(base_dir, "raw_export value GATS.csv")
price_path        <- file.path(base_dir, "raw_state_price.csv")
irr_path          <- file.path(base_dir, "raw_state_irr.csv")
irr_methods_path <- file.path(base_dir, "raw_irr_methods.csv")

# ============================================================
# 1. Helper functions
# ============================================================

clean_num <- function(x) {
  x <- trimws(as.character(x))
  x[tolower(x) %in% c("", "na", "n/a", "null", "none", ".", "nan", "inf", "-inf",
                      "d", "z", "s", "h", "l", "(d)", "(z)", "(s)", "(h)", "(l)", "(na)")] <- NA_character_
  x <- gsub(",", "", x)
  out <- suppressWarnings(as.numeric(x))
  out[!is.finite(out)] <- NA_real_
  out
}

sum_or_na <- function(x) {
  x <- clean_num(x)
  if (all(is.na(x))) NA_real_ else sum(x, na.rm = TRUE)
}

mean_or_na <- function(x) {
  x <- clean_num(x)
  if (all(is.na(x))) NA_real_ else mean(x, na.rm = TRUE)
}

first_nonmissing <- function(x) {
  x <- x[!is.na(x)]
  if (length(x) == 0) NA else x[1]
}

missing_or_zero <- function(x) {
  x <- clean_num(x)
  is.na(x) | x == 0
}

clean_statefp <- function(x) {
  x <- as.character(x)
  x <- str_squish(x)
  x[tolower(x) %in% c("", "na", "nan", "null")] <- NA_character_
  x <- str_replace_all(x, "\\.0$", "")
  ifelse(is.na(x), NA_character_, str_pad(x, width = 2, side = "left", pad = "0"))
}

clean_stub <- function(x) {
  x %>%
    str_remove_all("[()]") %>%
    str_to_lower() %>%
    str_replace_all("[^a-z0-9]+", "_") %>%
    str_replace_all("^_|_$", "")
}

state_xwalk <- tibble(
  state = str_to_title(c(state.name, "District Of Columbia")),
  stusps = c(state.abb, "DC"),
  statefp = c(
    "01", "02", "04", "05", "06", "08", "09", "10", "12", "13",
    "15", "16", "17", "18", "19", "20", "21", "22", "23", "24",
    "25", "26", "27", "28", "29", "30", "31", "32", "33", "34",
    "35", "36", "37", "38", "39", "40", "41", "42", "44", "45",
    "46", "47", "48", "49", "50", "51", "53", "54", "55", "56",
    "11"
  )
)

norm_panel_keys <- function(dat) {
  dat <- dat %>% rename_with(str_trim)

  if (!"state" %in% names(dat) && "State" %in% names(dat)) {
    dat <- dat %>% rename(state = all_of("State"))
  }
  if (!"year" %in% names(dat) && "Year" %in% names(dat)) {
    dat <- dat %>% rename(year = all_of("Year"))
  }
  if (!"statefp" %in% names(dat) && "State ANSI" %in% names(dat)) {
    dat <- dat %>% rename(statefp = all_of("State ANSI"))
  }

  if (!"year" %in% names(dat)) stop("No year column found. Need `year` or `Year`.")
  if (!"state" %in% names(dat)) dat$state <- NA_character_
  if (!"stusps" %in% names(dat)) dat$stusps <- NA_character_
  if (!"statefp" %in% names(dat)) dat$statefp <- NA_character_

  dat <- dat %>%
    mutate(
      year = suppressWarnings(as.integer(as.character(year))),
      state = str_to_title(str_squish(as.character(state))),
      stusps = str_to_upper(str_squish(as.character(stusps))),
      statefp = clean_statefp(statefp),
      state = ifelse(tolower(state) %in% c("", "na", "nan", "null"), NA_character_, state),
      stusps = ifelse(tolower(stusps) %in% c("", "na", "nan", "null"), NA_character_, stusps)
    )

  dat <- dat %>%
    left_join(state_xwalk, by = "statefp", suffix = c("", "__fp")) %>%
    mutate(
      state = coalesce(state, state__fp),
      stusps = coalesce(stusps, stusps__fp)
    ) %>%
    select(-any_of(c("state__fp", "stusps__fp")))

  dat <- dat %>%
    left_join(state_xwalk, by = "stusps", suffix = c("", "__abb")) %>%
    mutate(
      state = coalesce(state, state__abb),
      statefp = coalesce(statefp, statefp__abb)
    ) %>%
    select(-any_of(c("state__abb", "statefp__abb")))

  dat <- dat %>%
    left_join(state_xwalk, by = "state", suffix = c("", "__name")) %>%
    mutate(
      stusps = coalesce(stusps, stusps__name),
      statefp = coalesce(statefp, statefp__name)
    ) %>%
    select(-any_of(c("stusps__name", "statefp__name")))

  dat
}

# ============================================================
# 2. Read the two main panels
# ============================================================

state_panel <- read_csv(state_panel_path, show_col_types = FALSE) %>%
  norm_panel_keys()

county_state_panel <- read_csv(county_panel_path, show_col_types = FALSE) %>%
  norm_panel_keys()

# ============================================================
# 3. Map county_state_panel variables to state_panel names
# ============================================================

county_state_variable_map <- tibble::tribble(
  ~state_panel_variable,       ~county_state_panel_variable,
  "alf_acres",                "hay_alf_acres",
  "irrigated_acres",          "alf_irrigated_acres",
  "non_irrigated_acres",      "alf_non_irrigated_acres_harvested",
  "alf_production",           "hay_alf_tons",
  "irrigated_production",     "alf_irrigated_production_tons",
  "non_irrigated_production", "non_irrigated_production",
  "irr_share",                "irrigation_share",
  "alf_yield",                "hay_alfalfa_yield_tons_per_acre",
  "irr_yield",                "alf_irrigated_yield_tons_per_acre",
  "non_irr_yield",            "alf_non_irrigated_yield_tons_per_acre"
)

county_state_std <- county_state_panel %>%
  transmute(
    state,
    stusps,
    statefp,
    year,
    alf_acres = clean_num(hay_alf_acres),
    irrigated_acres = clean_num(alf_irrigated_acres),
    non_irrigated_acres = clean_num(alf_non_irrigated_acres_harvested),
    alf_production = clean_num(hay_alf_tons),
    irrigated_production = clean_num(alf_irrigated_production_tons),
    non_irrigated_production = clean_num(non_irrigated_production),
    irr_share = clean_num(irrigation_share),
    alf_yield = clean_num(hay_alfalfa_yield_tons_per_acre),
    irr_yield = clean_num(alf_irrigated_yield_tons_per_acre),
    non_irr_yield = clean_num(alf_non_irrigated_yield_tons_per_acre)
  ) %>%
  group_by(statefp, year) %>%
  summarise(
    state = first_nonmissing(state),
    stusps = first_nonmissing(stusps),
    alf_acres = sum_or_na(alf_acres),
    irrigated_acres = sum_or_na(irrigated_acres),
    non_irrigated_acres = sum_or_na(non_irrigated_acres),
    alf_production = sum_or_na(alf_production),
    irrigated_production = sum_or_na(irrigated_production),
    non_irrigated_production = sum_or_na(non_irrigated_production),
    .groups = "drop"
  ) %>%
  mutate(
    irr_share = ifelse(
      !is.na(irrigated_acres) & !is.na(alf_acres) & alf_acres > 0,
      irrigated_acres / alf_acres,
      NA_real_
    ),
    alf_yield = ifelse(
      !is.na(alf_production) & !is.na(alf_acres) & alf_acres > 0,
      alf_production / alf_acres,
      NA_real_
    ),
    irr_yield = ifelse(
      !is.na(irrigated_production) & !is.na(irrigated_acres) & irrigated_acres > 0,
      irrigated_production / irrigated_acres,
      NA_real_
    ),
    non_irr_yield = ifelse(
      !is.na(non_irrigated_production) & !is.na(non_irrigated_acres) & non_irrigated_acres > 0,
      non_irrigated_production / non_irrigated_acres,
      NA_real_
    )
  )

fill_missing_from_county <- function(base, county_std) {
  fill_vars <- c(
    "alf_acres", "irrigated_acres", "non_irrigated_acres",
    "alf_production", "irrigated_production", "non_irrigated_production",
    "irr_share", "alf_yield", "irr_yield", "non_irr_yield"
  )

  donor <- county_std %>%
    select(statefp, year, all_of(fill_vars)) %>%
    rename_with(~ paste0(.x, "__county"), all_of(fill_vars))

  out <- base %>% left_join(donor, by = c("statefp", "year"))

  for (v in fill_vars) {
    donor_col <- paste0(v, "__county")
    old <- clean_num(out[[v]])
    new <- clean_num(out[[donor_col]])

    # Rule:
    # 1) State-panel value is the primary source.
    # 2) Fill from county_state only when the state value is NA or 0.
    # 3) Do NOT fill from county_state when the county donor value is 0,
    #    because county aggregation can create structural zeros from missing/nonreported records.
    # 4) Exception: irr_share = 0 is meaningful, so county irr_share = 0 may be used.
    donor_valid <- !is.na(new) & (new != 0 | v == "irr_share")

    out[[v]] <- ifelse(
      (is.na(old) | old == 0) & donor_valid,
      new,
      old
    )
  }

  out %>% select(-ends_with("__county"))
}

# ============================================================
# 4. Build base df from SPEI
# ============================================================

spei <- read_csv(spei_path, show_col_types = FALSE) %>%
  norm_panel_keys() %>%
  transmute(
    state,
    stusps,
    statefp,
    year,
    alf_spei_mean = clean_num(speimean)
  ) %>%
  filter(year >= 2005, year <= 2025)

df_base <- spei %>%
  distinct(state, stusps, statefp, year, alf_spei_mean) %>%
  left_join(
    state_panel %>% select(-any_of(c("state", "stusps"))),
    by = c("statefp", "year")
  ) %>%
  norm_panel_keys()

state_panel_before_fill <- df_base

df <- fill_missing_from_county(df_base, county_state_std)

# ============================================================
# 5. Audit which county values were added
# ============================================================

fill_vars <- c(
  "alf_acres", "irrigated_acres", "non_irrigated_acres",
  "alf_production", "irrigated_production", "non_irrigated_production",
  "irr_share", "alf_yield", "irr_yield", "non_irr_yield"
)

county_fill_audit <- state_panel_before_fill %>%
  select(state, stusps, statefp, year, all_of(fill_vars)) %>%
  pivot_longer(
    cols = all_of(fill_vars),
    names_to = "variable",
    values_to = "state_old_value"
  ) %>%
  left_join(
    county_state_std %>%
      select(statefp, year, all_of(fill_vars)) %>%
      pivot_longer(
        cols = all_of(fill_vars),
        names_to = "variable",
        values_to = "county_state_value"
      ),
    by = c("statefp", "year", "variable")
  ) %>%
  left_join(
    df %>%
      select(statefp, year, all_of(fill_vars)) %>%
      pivot_longer(
        cols = all_of(fill_vars),
        names_to = "variable",
        values_to = "state_new_value"
      ),
    by = c("statefp", "year", "variable")
  ) %>%
  mutate(
    state_old_value = clean_num(state_old_value),
    county_state_value = clean_num(county_state_value),
    state_new_value = clean_num(state_new_value),
    county_donor_valid = !is.na(county_state_value) &
      (county_state_value != 0 | variable == "irr_share"),
    fill_action = case_when(
      (is.na(state_old_value) | state_old_value == 0) &
        county_donor_valid &
        !is.na(state_new_value) &
        abs(state_new_value - county_state_value) < 1e-8 ~ "filled_from_county_state",
      TRUE ~ NA_character_
    )
  ) %>%
  filter(!is.na(fill_action)) %>%
  arrange(statefp, year, variable)

county_fill_summary <- county_fill_audit %>%
  count(variable, name = "n_added") %>%
  arrange(desc(n_added))

# ============================================================
# 6. Read and reshape GATS export values
# ============================================================

export_year_row <- read_csv(
  export_path,
  skip = 3,
  n_max = 1,
  col_names = FALSE,
  show_col_types = FALSE
)

year_header <- as.character(unlist(export_year_row[1, ], use.names = FALSE))
year_cols <- which(year_header %in% as.character(2005:2025))
year_names <- year_header[year_cols]

export_raw <- read_csv(
  export_path,
  skip = 5,
  col_names = FALSE,
  show_col_types = FALSE,
  na = c("", "NA")
)

export_state <- export_raw %>%
  select(
    state = X2,
    partner = X4,
    hs_code = X6,
    product = X7,
    all_of(paste0("X", year_cols))
  ) %>%
  rename_with(~ year_names, all_of(paste0("X", year_cols))) %>%
  mutate(
    state = str_to_title(str_squish(as.character(state))),
    partner = str_squish(as.character(partner)),
    hs_code = as.character(hs_code),
    product = as.character(product)
  ) %>%
  filter(
    state %in% state_xwalk$state,
    partner == "World Total"
  ) %>%
  pivot_longer(
    cols = all_of(year_names),
    names_to = "year",
    values_to = "Export_Value"
  ) %>%
  mutate(
    year = as.integer(year),
    Export_Value = clean_num(Export_Value)
  ) %>%
  left_join(state_xwalk, by = "state") %>%
  group_by(statefp, year) %>%
  summarise(
    Export_Value = sum_or_na(Export_Value),
    .groups = "drop"
  )

# ============================================================
# 7. Read USDA alfalfa price and production value
# ============================================================

price_state <- read_csv(price_path, show_col_types = FALSE) %>%
  rename_with(str_trim) %>%
  filter(str_to_upper(`Geo Level`) == "STATE") %>%
  mutate(
    year = as.integer(Year),
    statefp = clean_statefp(`State ANSI`),
    data_item = str_to_upper(str_squish(`Data Item`)),
    value = clean_num(Value),
    var = case_when(
      str_detect(data_item, "ALFALFA - PRICE RECEIVED") &
        str_detect(data_item, fixed("$ / TON")) ~ "alf_price_usd_ton",
      str_detect(data_item, "ALFALFA - PRODUCTION") &
        str_detect(data_item, fixed("MEASURED IN $")) ~ "hay_alf_usd_usda",
      str_detect(data_item, "ALFALFA - SALES") &
        str_detect(data_item, "TONS") ~ "alf_sales_tons",
      TRUE ~ NA_character_
    )
  ) %>%
  filter(!is.na(var)) %>%
  group_by(statefp, year, var) %>%
  summarise(
    value = if (first(var) == "alf_price_usd_ton") mean_or_na(value) else sum_or_na(value),
    .groups = "drop"
  ) %>%
  pivot_wider(names_from = var, values_from = value)

# ============================================================
# 8. Read HAY & HAYLAGE irrigation raw data as separate contextual variables
#    IMPORTANT:
#    raw_state_irr.csv and raw_irr_methods.csv are HAY & HAYLAGE, not pure HAY.
#    Therefore, these variables are NOT used to overwrite/fill state_panel hay-only
#    alfalfa variables such as irrigated_acres, irr_share, alf_yield, etc.
#    They are kept separately with hh_alf_* prefixes.
# ============================================================

build_hh_alf_irr_wide <- function(path, variable_prefix) {
  raw <- read_csv(path, show_col_types = FALSE) %>%
    rename_with(str_trim) %>%
    filter(str_to_upper(`Geo Level`) == "STATE") %>%
    mutate(
      year = as.integer(Year),
      statefp = clean_statefp(`State ANSI`),
      commodity = str_to_upper(str_squish(Commodity)),
      data_item = str_to_upper(str_squish(`Data Item`)),
      domain = str_to_upper(str_squish(Domain)),
      domain_cat = str_to_upper(str_squish(`Domain Category`)),
      value = clean_num(Value)
    ) %>%
    filter(commodity == "HAY & HAYLAGE") %>%
    mutate(
      metric = case_when(
        # Check NON-IRRIGATED before IRRIGATED because "NON-IRRIGATED" contains the word "IRRIGATED".
        str_detect(data_item, "NON-IRRIGATED - ACRES HARVESTED") ~ "non_irrigated_acres",
        str_detect(data_item, "IRRIGATED - ACRES HARVESTED") ~ "irrigated_acres",
        str_detect(data_item, "IRRIGATED - WATER APPLIED") ~ "water_applied_af_acre",
        str_detect(data_item, "NON-IRRIGATED - YIELD") ~ "non_irr_yield_tons_per_acre",
        str_detect(data_item, "IRRIGATED - YIELD") ~ "irr_yield_tons_per_acre",
        str_detect(data_item, "ALFALFA - YIELD") ~ "alf_yield_tons_per_acre",
        TRUE ~ NA_character_
      ),
      subgroup = case_when(
        domain == "TOTAL" ~ "total",
        domain == "IRRIGATION METHOD, PRIMARY" ~ paste0(
          "method_",
          domain_cat %>%
            str_remove("IRRIGATION METHOD, PRIMARY:") %>%
            clean_stub()
        ),
        domain == "WATER SOURCE" ~ paste0(
          "source_",
          domain_cat %>%
            str_remove("WATER SOURCE:") %>%
            clean_stub()
        ),
        domain == "IRRIGATION STATUS" ~ paste0(
          "status_",
          domain_cat %>%
            str_remove("IRRIGATION STATUS:") %>%
            clean_stub()
        ),
        TRUE ~ NA_character_
      ),
      var = ifelse(
        !is.na(metric) & !is.na(subgroup),
        paste(variable_prefix, metric, subgroup, sep = "_"),
        NA_character_
      )
    ) %>%
    filter(!is.na(var), !is.na(statefp), !is.na(year)) %>%
    group_by(statefp, year, var) %>%
    summarise(
      value = if (str_detect(first(var), "acres")) sum_or_na(value) else mean_or_na(value),
      .groups = "drop"
    ) %>%
    pivot_wider(names_from = var, values_from = value)

  raw
}


# Prefix: hh_alf_stateirr_*
hh_alf_stateirr <- build_hh_alf_irr_wide(
  path = irr_path,
  variable_prefix = "hh_alf_stateirr"
)

# Prefix: hh_alf_methods_*
hh_alf_methods <- build_hh_alf_irr_wide(
  path = irr_methods_path,
  variable_prefix = "hh_alf_methods"
)

hh_alf_stateirr_added_summary <- tibble(
  source_file = "raw_state_irr.csv",
  added_variable = setdiff(names(hh_alf_stateirr), c("statefp", "year"))
)

hh_alf_methods_added_summary <- tibble(
  source_file = "raw_irr_methods.csv",
  added_variable = setdiff(names(hh_alf_methods), c("statefp", "year"))
)

hh_alf_variables_added_summary <- bind_rows(
  hh_alf_stateirr_added_summary,
  hh_alf_methods_added_summary
)

# ============================================================
# 9. Join everything and recompute derived variables
# ============================================================

df <- df %>%
  left_join(export_state, by = c("statefp", "year")) %>%
  left_join(price_state, by = c("statefp", "year")) %>%
  left_join(hh_alf_stateirr, by = c("statefp", "year")) %>%
  left_join(hh_alf_methods, by = c("statefp", "year")) %>%
  mutate(
    # IMPORTANT:
    # Do NOT use HAY & HAYLAGE irrigation variables to fill hay-only alfalfa state_panel values.
    # The following variables remain based only on state_panel + county_state_panel + internal calculations:
    # irrigated_acres, non_irrigated_acres, irr_share, alf_yield, irr_yield, non_irr_yield.

    # Fill non-irrigated acres and production only from hay-only core variables.
    non_irrigated_acres = ifelse(
      missing_or_zero(non_irrigated_acres) & !is.na(alf_acres) & !is.na(irrigated_acres),
      pmax(alf_acres - irrigated_acres, 0),
      non_irrigated_acres
    ),
    non_irrigated_production = ifelse(
      missing_or_zero(non_irrigated_production) & !is.na(alf_production) & !is.na(irrigated_production),
      pmax(alf_production - irrigated_production, 0),
      non_irrigated_production
    ),

    # Core hay-only irrigation share and yields.
    irr_share = ifelse(
      !is.na(irrigated_acres) & !is.na(alf_acres) & alf_acres > 0,
      irrigated_acres / alf_acres,
      irr_share
    ),
    alf_yield = ifelse(
      missing_or_zero(alf_yield) & !is.na(alf_production) & !is.na(alf_acres) & alf_acres > 0,
      alf_production / alf_acres,
      alf_yield
    ),
    irr_yield = ifelse(
      missing_or_zero(irr_yield) & !is.na(irrigated_production) & !is.na(irrigated_acres) & irrigated_acres > 0,
      irrigated_production / irrigated_acres,
      irr_yield
    ),
    non_irr_yield = ifelse(
      missing_or_zero(non_irr_yield) & !is.na(non_irrigated_production) & !is.na(non_irrigated_acres) & non_irrigated_acres > 0,
      non_irrigated_production / non_irrigated_acres,
      non_irr_yield
    ),

    # Production value: price is dollars/ton, production is tons.
    hay_alf_usd_calc = ifelse(
      !is.na(alf_price_usd_ton) & !is.na(alf_production),
      alf_price_usd_ton * alf_production,
      NA_real_
    ),
    hay_alf_usd = coalesce(hay_alf_usd_usda, hay_alf_usd_calc),

    # GATS export value is in thousand dollars.
    export_usd = Export_Value * 1000,

    # Export rate relative to state alfalfa production value.
    avg_exp_rate = ifelse(
      !is.na(export_usd) & !is.na(hay_alf_usd) & hay_alf_usd > 0,
      export_usd / hay_alf_usd,
      NA_real_
    ),

    west = ifelse(stusps %in% c("AZ", "CA", "CO", "ID", "MT", "NV", "NM", "OR", "UT", "WA", "WY"), 1L, 0L),
    midwest = ifelse(stusps %in% c("IL", "IN", "IA", "KS", "MI", "MN", "MO", "NE", "ND", "OH", "SD", "WI"), 1L, 0L)
  ) %>%
  arrange(statefp, year)

# ============================================================
# 10. Diagnostics and export
# ============================================================

missing_summary <- df %>%
  summarise(across(-any_of(c("state", "stusps", "statefp", "year")), ~ sum(is.na(.x)))) %>%
  pivot_longer(everything(), names_to = "variable", values_to = "n_missing") %>%
  arrange(desc(n_missing))

join_summary <- tibble(
  dataset = c(
    "final df",
    "SPEI base",
    "state_panel",
    "county_state_panel raw",
    "county_state_panel standardized",
    "county values added",
    "GATS export",
    "USDA price/value",
    "HAY & HAYLAGE raw_state_irr separate variables",
    "HAY & HAYLAGE raw_irr_methods separate variables"
  ),
  rows = c(
    nrow(df),
    nrow(spei),
    nrow(state_panel),
    nrow(county_state_panel),
    nrow(county_state_std),
    nrow(county_fill_audit),
    nrow(export_state),
    nrow(price_state),
    nrow(hh_alf_stateirr),
    nrow(hh_alf_methods)
  )
)

write_csv(df, file.path(base_dir, "df.csv"))
write_csv(missing_summary, file.path(base_dir, "df_missing_summary.csv"))
write_csv(join_summary, file.path(base_dir, "df_join_summary.csv"))
write_csv(county_fill_audit, file.path(base_dir, "county_state_added_values_detail.csv"))
write_csv(hh_alf_variables_added_summary, file.path(base_dir, "hay_haylage_variables_added.csv"))

cat("\nHAY & HAYLAGE variables were added separately. They were NOT used to fill hay-only state_panel variables.\n")
cat("\nAdded HAY & HAYLAGE variables:\n")
print(hh_alf_variables_added_summary, n = Inf)

cat("\nSaved files:\n")
cat(file.path(base_dir, "df.csv"), "\n")
cat(file.path(base_dir, "df_missing_summary.csv"), "\n")
cat(file.path(base_dir, "df_join_summary.csv"), "\n")
cat(file.path(base_dir, "county_state_added_values_detail.csv"), "\n")
cat(file.path(base_dir, "hay_haylage_variables_added.csv"), "\n")
cat("\nFinal rows:", nrow(df), "\n")
cat("Years:", min(df$year, na.rm = TRUE), "-", max(df$year, na.rm = TRUE), "\n")
