library(readr)
library(dplyr)
library(stringr)
library(tidyr)

workdir <- "C:/Users/liyoumin/Desktop/Alfalfa-SPEI/County_alf_irr"

hay_acre_file  <- file.path(workdir, "hay_alf_county.csv")
hay_yield_file <- file.path(workdir, "hay_county_irr_yield.csv")
haylage_file   <- file.path(workdir, "haylage_irr_acre_county.csv")

out_file <- file.path(workdir, "alfalfa_state_year_panel_wide.csv")

fips_map <- tibble(
  statefp = c("01","02","04","05","06","08","09","10","11","12","13","15","16","17","18","19",
              "20","21","22","23","24","25","26","27","28","29","30","31","32","33","34","35",
              "36","37","38","39","40","41","42","44","45","46","47","48","49","50","51","53",
              "54","55","56"),
  stusps = c("AL","AK","AZ","AR","CA","CO","CT","DE","DC","FL","GA","HI","ID","IL","IN","IA",
             "KS","KY","LA","ME","MD","MA","MI","MN","MS","MO","MT","NE","NV","NH","NJ","NM",
             "NY","NC","ND","OH","OK","OR","PA","RI","SC","SD","TN","TX","UT","VT","VA","WA",
             "WV","WI","WY")
)

clean_value <- function(x) {
  x <- as.character(x)
  x <- str_trim(x)
  x[x %in% c("(D)", "(Z)", "(NA)", "(S)", "NA", "", "NULL")] <- NA_character_
  x <- str_replace_all(x, ",", "")
  suppressWarnings(as.numeric(x))
}

prep_common <- function(df) {
  df %>%
    rename_with(str_trim) %>%
    mutate(
      Value_num   = clean_value(Value),
      statefp     = str_pad(as.character(`State ANSI`), width = 2, side = "left", pad = "0"),
      county_ansi = suppressWarnings(as.integer(`County ANSI`)),
      county_name = str_to_upper(str_trim(as.character(County))),
      state       = str_to_upper(str_trim(as.character(State))),
      data_item   = str_to_upper(str_trim(as.character(`Data Item`)))
    ) %>%
    left_join(fips_map, by = "statefp")
}

sum_state_year <- function(df, item_text, out_name) {
  df %>%
    filter(data_item == toupper(item_text)) %>%
    group_by(state, stusps, statefp, Year) %>%
    summarise(value = sum(Value_num, na.rm = TRUE), .groups = "drop") %>%
    rename(year = Year) %>%
    rename(!!out_name := value)
}

mean_state_year <- function(df, item_text, out_name) {
  df %>%
    filter(data_item == toupper(item_text)) %>%
    group_by(state, stusps, statefp, Year) %>%
    summarise(value = mean(Value_num, na.rm = TRUE), .groups = "drop") %>%
    rename(year = Year) %>%
    rename(!!out_name := value)
}

weighted_mean_state_year <- function(df_value, df_weight, value_item, weight_item, out_name) {
  merge_keys <- c("state", "stusps", "statefp", "Year", "county_name", "county_ansi")
  
  value_df <- df_value %>%
    filter(data_item == toupper(value_item)) %>%
    select(all_of(merge_keys), value = Value_num)
  
  weight_df <- df_weight %>%
    filter(data_item == toupper(weight_item)) %>%
    select(all_of(merge_keys), weight = Value_num)
  
  value_df %>%
    left_join(weight_df, by = merge_keys) %>%
    filter(!is.na(value), !is.na(weight), weight > 0) %>%
    group_by(state, stusps, statefp, Year) %>%
    summarise(value = weighted.mean(value, weight, na.rm = TRUE), .groups = "drop") %>%
    rename(year = Year) %>%
    rename(!!out_name := value)
}

hay_acre  <- read_csv(hay_acre_file, show_col_types = FALSE) %>% prep_common()
hay_yield <- read_csv(hay_yield_file, show_col_types = FALSE) %>% prep_common()
haylage   <- read_csv(haylage_file, show_col_types = FALSE) %>% prep_common()

# hay irrigated
hay_irr_acres <- sum_state_year(
  hay_acre,
  "HAY, ALFALFA, IRRIGATED - ACRES HARVESTED",
  "hay_irr_acres"
)


hay_irr_prod <- sum_state_year(
  hay_acre,
  "HAY, ALFALFA, IRRIGATED - PRODUCTION, MEASURED IN TONS",
  "alf_irr_prod_tons"
)

hay_irr_yield <- weighted_mean_state_year(
  df_value   = hay_yield,
  df_weight  = hay_acre,
  value_item = "HAY, ALFALFA, IRRIGATED - YIELD, MEASURED IN TONS / ACRE",
  weight_item= "HAY, ALFALFA, IRRIGATED - ACRES HARVESTED",
  out_name   = "alf_irr_yield_ton_acre"
)

# hay non-irrigated
alf_nonirr_acres <- sum_state_year(
  hay_acre,
  "HAY, ALFALFA, NON-IRRIGATED - ACRES HARVESTED",
  "alf_nonirr_acres"
)


alf_nonirr_prod <- sum_state_year(
  hay_acre,
  "HAY, ALFALFA, NON-IRRIGATED - PRODUCTION, MEASURED IN TONS",
  "alf_nonirr_prod_tons"
)

alf_nonirr_yield <- mean_state_year(
  hay_yield,
  "HAY, ALFALFA, NON-IRRIGATED - YIELD, MEASURED IN TONS / ACRE",
  "alf_nonirr_yield_ton_acre"
)

# haylage irrigated
haylage_irr_acres <- sum_state_year(
  haylage,
  "HAYLAGE, ALFALFA, IRRIGATED - ACRES HARVESTED",
  "haylage_irr_acres"
)

haylage_irr_ops <- sum_state_year(
  haylage,
  "HAYLAGE, ALFALFA, IRRIGATED - OPERATIONS WITH AREA HARVESTED",
  "haylage_irr_ops"
)

# merge all pieces
panel <- list(
  hay_irr_acres,
  hay_irr_prod,
  hay_irr_yield,
  alf_nonirr_acres,
  alf_nonirr_prod,
  alf_nonirr_yield,
  haylage_irr_acres,
  haylage_irr_ops
) %>%
  reduce(full_join, by = c("state", "stusps", "statefp", "year"))

# collapse overlapping irrigated measures using larger value
panel <- panel %>%
  mutate(
    alf_irr_acres = rowSums(
      cbind(hay_irr_acres, haylage_irr_acres),
      na.rm = TRUE
    ),
    alf_irr_ops = case_when(
      !is.na(hay_irr_ops) & !is.na(haylage_irr_ops) ~
        pmax(hay_irr_ops, haylage_irr_ops),
      !is.na(hay_irr_ops) ~ hay_irr_ops,
      !is.na(haylage_irr_ops) ~ haylage_irr_ops,
      TRUE ~ NA_real_
    )
  ) %>%
  select(
    state, stusps, statefp, year,
    alf_irr_acres,
    alf_irr_ops,
    alf_irr_prod_tons,
    alf_irr_yield_ton_acre,
    alf_nonirr_acres,
    alf_nonirr_prod_tons,
    alf_nonirr_yield_ton_acre
  ) %>%
  arrange(statefp, year)

write_csv(panel, out_file)

print(out_file)
print(head(panel, 20))

##==============================
spei_file <- file.path(workdir, "alfal_spei_panel_2005_2025.csv")
alf_file  <- file.path(workdir, "alfalfa_state_year_panel_wide.csv")
out_file  <- file.path(workdir, "alfalfa_state_year_panel_with_spei.csv")

# read
spei <- read_csv(spei_file, show_col_types = FALSE)
alf  <- read_csv(alf_file,  show_col_types = FALSE)

# clean keys
spei <- spei %>%
  mutate(
    stusps  = str_trim(stusps),
    statefp = str_pad(as.character(statefp), 2, pad = "0"),
    year    = as.integer(year)
  )

alf <- alf %>%
  mutate(
    stusps  = str_trim(stusps),
    statefp = str_pad(as.character(statefp), 2, pad = "0"),
    year    = as.integer(year)
  )

# rename SPEI variable if needed
if ("speimean" %in% names(spei)) {
  spei <- spei %>% rename(alf_spei_mean = speimean)
}

# keep only needed columns from SPEI
spei_small <- spei %>%
  select(statefp, stusps, year, alf_spei_mean)

# get master state list from both files
state_master <- bind_rows(
  alf  %>% select(statefp, stusps, state),
  spei %>% select(statefp, stusps) %>% mutate(state = NA_character_)
) %>%
  distinct(statefp, stusps, .keep_all = TRUE) %>%
  arrange(statefp)

# create full 2005-2025 panel skeleton
panel_skeleton <- expand_grid(
  state_master,
  year = 2005:2025
)

# merge both datasets onto skeleton
panel_merged <- panel_skeleton %>%
  left_join(
    alf,
    by = c("statefp", "stusps", "state", "year")
  ) %>%
  left_join(
    spei_small,
    by = c("statefp", "stusps", "year")
  ) %>%
  arrange(statefp, year)

# save
write_csv(panel_merged, out_file)

print(out_file)
print(dim(panel_merged))
print(head(panel_merged, 20))
