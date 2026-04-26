library(readxl)
library(dplyr)
library(stringr)
library(tidyr)
library(readr)

in_file  <- "raw_state.xlsx"
out_file <- "state_panel.csv"

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
    
    # HAYLAGE (alfalfa) — optional
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