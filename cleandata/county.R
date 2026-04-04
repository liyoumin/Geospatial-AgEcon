install.packages("tidyverse")
library(tidyverse)
# Read data
acres <- read_csv("C:/Users/liyoumin/Desktop/Alfalfa-SPEI/County_alf_irr/cleandata/countyacres.csv", show_col_types = FALSE)
yield <- read_csv("C:/Users/liyoumin/Desktop/Alfalfa-SPEI/County_alf_irr/cleandata/countyield.csv", show_col_types = FALSE)

acres <- acres %>% janitor::clean_names()
yield <- yield %>% janitor::clean_names()

clean_usda <- function(df) {
  df %>%
    transmute(
      year   = as.integer(year),
      state  = str_to_title(state),
      county = str_to_title(county),
      data_item = data_item,
      value = value
    ) %>%
    mutate(
      value = str_replace_all(value, ",", ""),
      value = ifelse(value %in% c("(D)", "(L)", "(H)", ""), NA, value),
      value = as.numeric(value)
    )
}

acres_clean <- clean_usda(acres)
yield_clean <- clean_usda(yield)

combined_long_collapsed <- combined_long %>%
  group_by(year, state, county, data_item) %>%
  summarise(value = sum(value, na.rm = TRUE), .groups = "drop")

county_panel <- combined_long_collapsed %>%
  pivot_wider(
    names_from = data_item,
    values_from = value
  ) %>%
  arrange(state, county, year)

county_panel_clean_names <- county_panel %>%
  rename_with(~ make.names(.x))

write_csv(county_panel, "county_panel.csv")

#####
library(dplyr)
library(stringr)

# helper to grab exactly one column by pattern
pick_col <- function(df, pattern) {
  names(df)[str_detect(names(df), pattern)]
}

acres_total <- names(county_panel) %>%
  str_subset("^HAY, ALFALFA - ACRES HARVESTED$")
prod_total <- names(county_panel) %>%
  str_subset("^HAY, ALFALFA - PRODUCTION")
yield_total <- names(county_panel) %>%
  str_subset("^HAY, ALFALFA - YIELD")
``
pick_one <- function(x) {
  stopifnot(length(x) == 1)
  x[[1]]
}

acres_total <- pick_one(acres_total)
prod_total  <- pick_one(prod_total)
yield_total <- pick_one(yield_total)

county_panel <- county_panel %>%
  mutate(
    !!yield_total :=
      if_else(
        is.na(.data[[yield_total]]),
        .data[[prod_total]] / .data[[acres_total]],
        .data[[yield_total]]
      )
  )

###irr yield
acres_irrig <- names(county_panel) %>%
  str_subset("^HAY, ALFALFA, IRRIGATED - ACRES HARVESTED$") %>%
  pick_one()
``
prod_irrig <- names(county_panel) %>%
  str_subset("^HAY, ALFALFA, IRRIGATED - PRODUCTION") %>%
  pick_one()

yield_irrig <- names(county_panel) %>%
  str_subset("^HAY, ALFALFA, IRRIGATED - YIELD") %>%
  pick_one()
###non irr
acres_non <- names(county_panel) %>%
  str_subset("^HAY, ALFALFA, NON-IRRIGATED - ACRES HARVESTED$") %>%
  pick_one()

prod_non <- names(county_panel) %>%
  str_subset("^HAY, ALFALFA, NON-IRRIGATED - PRODUCTION") %>%
  pick_one()

yield_non <- names(county_panel) %>%
  str_subset("^HAY, ALFALFA, NON-IRRIGATED - YIELD") %>%
  pick_one()

### fill missing value
county_panel <- county_panel %>%
  mutate(
    !!yield_irrig :=
      if_else(
        is.na(.data[[yield_irrig]]),
        .data[[prod_irrig]] / .data[[acres_irrig]],
        .data[[yield_irrig]]
      )
  )
county_panel <- county_panel %>%
  mutate(
    !!yield_non :=
      if_else(
        is.na(.data[[yield_non]]),
        .data[[prod_non]] / .data[[acres_non]],
        .data[[yield_non]]
      )
  )

stopifnot(
  all(is.finite(county_panel[[yield_irrig]]) | is.na(county_panel[[yield_irrig]])),
  all(is.finite(county_panel[[yield_non]])  | is.na(county_panel[[yield_non]]))
)

summary(county_panel[[yield_irrig]])
summary(county_panel[[yield_non]])

### irrigation share
acres_total <- names(county_panel) %>%
  str_subset("^HAY, ALFALFA - ACRES HARVESTED$") %>%
  pick_one()

acres_irrig <- names(county_panel) %>%
  str_subset("^HAY, ALFALFA, IRRIGATED - ACRES HARVESTED$") %>%
  pick_one()

county_panel <- county_panel %>%
  mutate(
    irr_share = .data[[acres_irrig]] / .data[[acres_total]]
  )

summary(county_panel$irr_share)

# check bounds
summary(county_panel$irr_share < 0)
summary(county_panel$irr_share > 1)

write_csv(county_panel, "county_panel.csv")
