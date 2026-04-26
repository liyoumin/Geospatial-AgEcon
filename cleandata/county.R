install.packages("tidyverse")
library(tidyverse)
# Read data
acres <- read_csv("C:/Users/liyoumin/Desktop/Alfalfa-SPEI/County_alf_irr/cleandata/raw_county_acres.csv", show_col_types = FALSE)
yield <- read_csv("C:/Users/liyoumin/Desktop/Alfalfa-SPEI/County_alf_irr/cleandata/raw_countyield.csv", show_col_types = FALSE)

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




# ================================
# Add GEOID + county-year SPEI (Apr-Sep)
library(readr)
library(dplyr)
library(stringr)
library(sf)
library(tigris)
library(terra)
library(exactextractr)
library(ncdf4)

options(tigris_use_cache = TRUE)
# ================================
panel_file <- "county_panel.csv"   # make sure this is the county panel
nc_file    <- "nclimgrid-spei-pearson-03.nc"

# ----------------
# 1. Read panel and standardize names
# ----------------
panel <- read_csv(panel_file, show_col_types = FALSE) %>%
  rename_with(tolower)

names(panel)
# should include: year, state, county

# ----------------
# 2. Name cleaner
# ----------------
norm_name <- function(x) {
  x |>
    iconv(from = "", to = "ASCII//TRANSLIT") |>
    tolower() |>
    str_replace_all("\\bsaint\\b", "st") |>
    str_replace_all("\\bste\\b", "st") |>
    str_replace_all("\\bcity and borough\\b", "") |>
    str_replace_all("\\bcensus area\\b", "") |>
    str_replace_all("\\bindependent city\\b", "") |>
    str_replace_all("\\bcounty\\b", "") |>
    str_replace_all("\\bparish\\b", "") |>
    str_replace_all("\\bborough\\b", "") |>
    str_replace_all("\\bmunicipality\\b", "") |>
    str_replace_all("[^a-z0-9]", "")
}

panel2 <- panel %>%
  mutate(
    state_orig  = state,
    county_orig = county,
    state_key   = norm_name(state),
    county_key  = norm_name(county)
  )

# ----------------
# 3. County shapes with GEOID
# ----------------
us_counties <- tigris::counties(cb = TRUE, year = 2022, class = "sf") %>%
  st_transform(4326) %>%
  transmute(
    GEOID,
    state_map  = STATE_NAME,
    county_map = NAME,
    state_key  = norm_name(STATE_NAME),
    county_key = norm_name(NAME)
  )

panel_geoid <- panel2 %>%
  left_join(
    st_drop_geometry(us_counties) %>%
      distinct(state_key, county_key, GEOID, .keep_all = TRUE),
    by = c("state_key", "county_key")
  ) %>%
  mutate(GEOID = str_pad(GEOID, width = 5, side = "left", pad = "0"))

# ----------------
# 4. Unmatched check
# ----------------
unmatched <- panel_geoid %>%
  filter(is.na(GEOID)) %>%
  distinct(state_orig, county_orig) %>%
  arrange(state_orig, county_orig)

print(unmatched, n = 100)


# keep only counties that matched GEOID for extraction join
county_shapes <- us_counties %>%
  filter(GEOID %in% unique(na.omit(panel_geoid$GEOID))) %>%
  select(GEOID)

# ----------------
# 6. Read SPEI NOAA
# ----------------
spei <- rast(nc_file)

# helper to recover dates if terra does not automatically read them
get_nc_dates <- function(r, nc_file) {
  tt <- terra::time(r)
  
  if (!is.null(tt) && !all(is.na(tt))) {
    return(as.Date(tt))
  }
  
  nc <- nc_open(nc_file)
  on.exit(nc_close(nc))
  
  tvals  <- ncvar_get(nc, "time")
  tunits <- ncatt_get(nc, "time", "units")$value
  
  unit_txt   <- tolower(sub("\\s+since.*", "", tunits))
  origin_txt <- sub(".*since\\s+", "", tunits)
  origin_posix <- as.POSIXct(origin_txt, tz = "UTC")
  
  if (grepl("day", unit_txt)) {
    return(as.Date(origin_posix + tvals * 86400, tz = "UTC"))
  } else if (grepl("hour", unit_txt)) {
    return(as.Date(origin_posix + tvals * 3600, tz = "UTC"))
  } else if (grepl("month", unit_txt)) {
    origin_date <- as.Date(origin_posix)
    return(seq(origin_date, by = "month", length.out = length(tvals)))
  } else {
    stop("Unhandled time units in netCDF: ", tunits)
  }
}

dates <- get_nc_dates(spei, nc_file)
terra::time(spei) <- dates

# ----------------
# 7. Keep Apr-Sep only, then average by county-year
# ----------------
spei_dates <- as.Date(terra::time(spei))
spei_year  <- as.integer(format(spei_dates, "%Y"))
spei_month <- as.integer(format(spei_dates, "%m"))

yrs <- sort(unique(panel_geoid$year))

spei_county_year <- lapply(yrs, function(y) {
  idx <- which(spei_year == y & spei_month %in% 4:9)
  
  if (length(idx) == 0) return(NULL)
  
  r_y <- spei[[idx]]
  
  # average Apr-Sep raster for that year
  r_avg <- app(r_y, mean, na.rm = TRUE)
  
  # exact county mean
  val <- exact_extract(r_avg, county_shapes, "mean")
  
  tibble(
    GEOID = county_shapes$GEOID,
    year  = y,
    spei01_apr_sep = val
  )
}) %>%
  bind_rows()

# ----------------
# 8. Join SPEI back to panel
# ----------------
panel_final <- panel_geoid %>%
  left_join(spei_county_year, by = c("GEOID", "year")) %>%
  select(-state_key, -county_key)

# ----------------
# 9. Save
# ----------------
write_csv(panel_final, out_file)

cat("Done. Saved to:", out_file, "\n")
#++++++++++++++++++++++++++++++++++++++++++++++ to state
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


# Export state panel
write_csv(state_panel, "county_state_panel.csv")


