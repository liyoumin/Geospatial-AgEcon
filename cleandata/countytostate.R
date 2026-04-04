library(dplyr)
library(stringr)

pick_one <- function(x) {
  stopifnot(length(x) == 1)
  x[[1]]
}

# --- ACRES ---
alf_acres_col <- names(county_panel) %>%
  str_subset("^HAY, ALFALFA - ACRES HARVESTED$") %>%
  pick_one()

irr_acres_col <- names(county_panel) %>%
  str_subset("^HAY, ALFALFA, IRRIGATED - ACRES HARVESTED$") %>%
  pick_one()

non_acres_col <- names(county_panel) %>%
  str_subset("^HAY, ALFALFA, NON-IRRIGATED - ACRES HARVESTED$") %>%
  pick_one()

# --- PRODUCTION ---
alf_prod_col <- names(county_panel) %>%
  str_subset("^HAY, ALFALFA - PRODUCTION") %>%
  pick_one()

irr_prod_col <- names(county_panel) %>%
  str_subset("^HAY, ALFALFA, IRRIGATED - PRODUCTION") %>%
  pick_one()

non_prod_col <- names(county_panel) %>%
  str_subset("^HAY, ALFALFA, NON-IRRIGATED - PRODUCTION") %>%
  pick_one()

# --- YIELDS ---
alf_yield_col <- names(county_panel) %>%
  str_subset("^HAY, ALFALFA - YIELD") %>%
  pick_one()

irr_yield_col <- names(county_panel) %>%
  str_subset("^HAY, ALFALFA, IRRIGATED - YIELD") %>%
  pick_one()

non_yield_col <- names(county_panel) %>%
  str_subset("^HAY, ALFALFA, NON-IRRIGATED - YIELD") %>%
  pick_one()

state_irr <- county_panel %>%
  group_by(state, year) %>%
  summarise(
    # irrigation share = unweighted county mean (valid counties only)
    irr_share = mean(irr_share, na.rm = TRUE),
    n_counties_irr_share = sum(!is.na(irr_share)),
    .groups = "drop"
  )
#### state panel
state_panel <- county_panel %>%
  group_by(state, year) %>%
  summarise(
    # --- acres (SUM) ---
    alf_acres           = sum(.data[[alf_acres_col]], na.rm = TRUE),
    irrigated_acres     = sum(.data[[irr_acres_col]], na.rm = TRUE),
    non_irrigated_acres = sum(.data[[non_acres_col]], na.rm = TRUE),
    
    # --- production (SUM) ---
    alf_production           = sum(.data[[alf_prod_col]], na.rm = TRUE),
    irrigated_production     = sum(.data[[irr_prod_col]], na.rm = TRUE),
    non_irrigated_production = sum(.data[[non_prod_col]], na.rm = TRUE),
    
    # --- irrigation share (MEAN over valid counties) ---
    irr_share = mean(irr_share, na.rm = TRUE),
    
    # --- yields (MEAN over valid counties) ---
    alf_yield     = mean(.data[[alf_yield_col]], na.rm = TRUE),
    irr_yield     = mean(.data[[irr_yield_col]], na.rm = TRUE),
    non_irr_yield = mean(.data[[non_yield_col]], na.rm = TRUE),
    
    .groups = "drop"
  ) %>%
  arrange(state, year)


# 1) Export state panel
write_csv(state_panel, "state_panel.csv")
