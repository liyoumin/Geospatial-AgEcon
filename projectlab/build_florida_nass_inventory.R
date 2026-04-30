# ==========================================================
# Florida_Census_Variable_Availability.R
# ==========================================================
suppressPackageStartupMessages({
  library(rnassqs)
  library(dplyr)
  library(tidyr)
  library(purrr)
  library(readr)
  library(stringr)
})

# ---- AUTH ----
rnassqs::nassqs_auth(key = Sys.getenv("NASS_API_KEY", "46437E2E-498B-3B15-9592-C42777E62DBE"))

YEARS  <- c(2002, 2007, 2013, 2018, 2023)
LEVELS <- c("STATE", "COUNTY") # COUNTY only used to detect availability
STATE  <- "FLORIDA"

# Small helper: robust param-values lookup (returns character(0) on error)
param_vals <- function(field, params) {
  out <- try(rnassqs::nassqs_param_values(field, params), silent = TRUE)
  if (inherits(out, "try-error") || is.null(out)) character(0) else unique(out)
}

# Parse numeric 'Value' safely (not strictly needed here, but handy)
num <- function(x) suppressWarnings(as.numeric(gsub(",", "", x)))

# Safe fetch that catches 50k errors; returns tibble()
safe_fetch <- function(p) {
  tryCatch({
    Sys.sleep(0.4)  # be nice to API
    rnassqs::nassqs(p)
  }, error = function(e) {
    attr(e, "message") <- conditionMessage(e)
    tibble()
  })
}

# Fetch layer with automatic splitting to stay under 50k:
# 1) sector_desc loop; 2) group_desc loop 3) if still too large, domain_desc loop
fetch_census_layered <- function(year, level) {
  base <- list(
    state_name    = STATE,
    source_desc   = "CENSUS",
    agg_level_desc= level,
    year          = as.character(year)
  )
  sectors <- param_vals("sector_desc", base)
  if (length(sectors) == 0) sectors <- c(NA_character_)  # fallback
  
  results <- list()
  for (sec in sectors) {
    p_sec <- base
    if (!is.na(sec)) p_sec$sector_desc <- sec
    
    groups <- param_vals("group_desc", p_sec)
    if (length(groups) == 0) groups <- c(NA_character_)
    
    for (grp in groups) {
      p_grp <- p_sec
      if (!is.na(grp)) p_grp$group_desc <- grp
      
      # Try a direct fetch for this sector+group
      dat <- safe_fetch(p_grp)
      
      # Heuristic: if result is suspiciously huge or empty due to cap, split by domain_desc
      if (nrow(dat) >= 50000 || nrow(dat) == 0) {
        domains <- param_vals("domain_desc", p_grp)
        if (length(domains) == 0) {
          # keep what we got (could be empty)
          results[[length(results) + 1]] <- dat
        } else {
          for (dom in domains) {
            p_dom <- p_grp
            p_dom$domain_desc <- dom
            chunk <- safe_fetch(p_dom)
            results[[length(results) + 1]] <- chunk
          }
        }
      } else {
        results[[length(results) + 1]] <- dat
      }
    }
  }
  bind_rows(results)
}

message("=== Pulling STATE-level (for year indicators) ===")
state_list <- list()
for (y in YEARS) {
  message("... Year ", y)
  d <- fetch_census_layered(y, "STATE")
  if (nrow(d) > 0) {
    d$year <- y
    d$agg_level_desc <- "STATE"
  }
  state_list[[as.character(y)]] <- d
}
state_all <- bind_rows(state_list)

message("=== Pulling COUNTY-level (to flag has_county_data) ===")
county_list <- list()
for (y in YEARS) {
  message("... Year ", y)
  d <- fetch_census_layered(y, "COUNTY")
  if (nrow(d) > 0) {
    d$year <- y
    d$agg_level_desc <- "COUNTY"
  }
  county_list[[as.character(y)]] <- d
}
county_all <- bind_rows(county_list)

# ---- Keep only the columns you want in the key ----
# Use group_desc, commodity_desc, short_desc to define a "variable"
pick <- function(df) {
  # Add any missing columns so select() never fails
  for (col in c("group_desc", "commodity_desc", "short_desc",
                "year", "agg_level_desc")) {
    if (!col %in% names(df)) df[[col]] <- NA_character_
  }
  
  df %>%
    select(group_desc, commodity_desc, short_desc, year, agg_level_desc) %>%
    distinct()
}

state_vars  <- pick(state_all)
county_vars <- pick(county_all)

# ---- Build has_county_data flag ----
county_flag <- county_vars %>%
  distinct(group_desc, commodity_desc, short_desc) %>%
  mutate(has_county_data = 1L)

# ---- Build year availability (STATE level presence) ----
state_year_wide <- state_vars %>%
  mutate(val = 1L) %>%
  select(group_desc, commodity_desc, short_desc, year, val) %>%
  distinct() %>%
  tidyr::pivot_wider(
    names_from = year,
    values_from = val,
    values_fill = 0
  )

# Ensure all five year columns exist even if absent in data
for (y in YEARS) {
  coly <- as.character(y)
  if (!coly %in% names(state_year_wide)) state_year_wide[[coly]] <- 0L
}

# ---- Join county flag ----
final <- state_year_wide %>%
  left_join(county_flag,
            by = c("group_desc", "commodity_desc", "short_desc")) %>%
  mutate(has_county_data = dplyr::coalesce(has_county_data, 0L)) %>%
  # Order columns exactly as requested
  select(group_desc, commodity_desc, short_desc,
         has_county_data, `2002`, `2007`, `2013`, `2018`, `2023`) %>%
  arrange(group_desc, commodity_desc, short_desc)

# ---- Write CSV ----
outfile <- "Florida_Census_Variable_Availability2.csv"
readr::write_csv(final, outfile)
message("✅ Wrote ", outfile, " with ", nrow(final), " rows.")








# ============================================================
#  fetch_florida_nass_full.R
#  --------------------------------------------
#  Pull ALL available USDA–NASS Quick Stats data  for Florida (STATE + COUNTY; SURVEY + CENSUS)
#  across all agricultural sectors.  Requires: rnassqs, dplyr, purrr, openxlsx, stringr, readr
# ============================================================

suppressPackageStartupMessages({
  library(rnassqs)
  library(dplyr)
  library(purrr)
  library(openxlsx)
  library(readr)
  library(stringr)
})

# ------------------------------------------------------------
# 1. Authentication
# ------------------------------------------------------------
# Get your personal API key from: https://www.nass.usda.gov/developer/
nassqs_auth(key = Sys.getenv("NASS_API_KEY", "46437E2E-498B-3B15-9592-C42777E62DBE"))
if (nchar(Sys.getenv("NASS_API_KEY","")) == 0) {
  message("⚠️  Using placeholder key. Replace PUT_YOUR_KEY_HERE with your real NASS API key.")
}

# ------------------------------------------------------------
# 2. Define query combinations (state/county × survey/census × sector)
# ------------------------------------------------------------
base_params <- expand.grid(
  agg_level_desc = c("STATE", "COUNTY"),
  source_desc    = c("SURVEY", "CENSUS"),
  sector_desc    = c("CROPS", "ANIMALS & PRODUCTS", "DEMOGRAPHICS", "ECONOMICS"),
  stringsAsFactors = FALSE
) %>%
  split(seq_len(nrow(.))) %>%
  lapply(function(x) as.list(c(state_name = "FLORIDA", x)))

message("✅ Built ", length(base_params), " query combinations.")

# ------------------------------------------------------------
# 3. Define helper functions
# ------------------------------------------------------------
parse_value_num <- function(x) {
  x2 <- gsub(",", "", x)
  suppressWarnings(as.numeric(x2))
}

fetch_qs <- function(p) {
  tryCatch({
    message("→ Querying: ", paste(names(p), p, collapse = "; "))
    Sys.sleep(1) # avoid API rate limit
    res <- rnassqs::nassqs(p)
    if (nrow(res) == 0) message("   ... no data returned.")
    res
  }, error = function(e) {
    message("   ❌ Error: ", e$message)
    tibble()
  })
}

# ------------------------------------------------------------
# 4. Fetch data for all query combinations (safe pagination)
# ------------------------------------------------------------
years <- 2000:2024  # adjust as needed

safe_fetch <- function(p, year) {
  p$year <- as.character(year)
  tryCatch({
    Sys.sleep(0.5)
    message("→ ", paste(names(p), p, collapse = "; "))
    res <- rnassqs::nassqs(p)
    if (nrow(res) == 0)
      message("   ... no data for ", year)
    res
  }, error = function(e) {
    message("   ❌ ", e$message)
    tibble()
  })
}

all_results <- list()

for (p in base_params) {
  message("\n===== ", paste(p, collapse = " | "), " =====")
  # break large queries by year
  yr_results <- map(years, ~safe_fetch(p, .x))
  all_results <- c(all_results, yr_results)
}

# ------------------------------------------------------------
# 5. Standardize columns (add any missing expected fields)
# ------------------------------------------------------------
expected_cols <- c(
  "source_desc","sector_desc","group_desc","commodity_desc","class_desc",
  "prodn_practice_desc","util_practice_desc","statisticcat_desc","unit_desc",
  "domain_desc","domaincat_desc","short_desc","reference_period_desc","freq_desc",
  "state_alpha","state_name","asd_desc","county_ansi","county_name",
  "year","Value","cv_percent","load_time","begin_code","end_code",
  "congr_district_code","watershed_code","region_desc","zip_5","country_code"
)

for (col in expected_cols) {
  if (!col %in% names(dat)) dat[[col]] <- NA_character_
}

dat <- dat %>%
  mutate(
    state_name  = if_else(is.na(state_name), "FLORIDA", state_name),
    state_alpha = if_else(is.na(state_alpha), "FL", state_alpha),
    Value_num   = parse_value_num(Value)
  ) %>%
  select(all_of(c(expected_cols, "Value_num")))

# ------------------------------------------------------------
# 6. Split state vs. county level
# ------------------------------------------------------------
dat_state  <- dat %>% filter(is.na(county_name) | county_name == "")
dat_county <- dat %>% filter(!is.na(county_name) & county_name != "")

message("✅ State-level rows: ", nrow(dat_state))
message("✅ County-level rows: ", nrow(dat_county))

# ------------------------------------------------------------
# 7. Write to Excel
# ------------------------------------------------------------
out_xlsx <- "Florida_NASS_Agriculture_All.xlsx"

if (file.exists(out_xlsx)) {
  wb <- loadWorkbook(out_xlsx)
  if ("State_Level" %in% names(wb)) removeWorksheet(wb, "State_Level")
  if ("County_Level" %in% names(wb)) removeWorksheet(wb, "County_Level")
} else {
  wb <- createWorkbook()
}

addWorksheet(wb, "State_Level")
addWorksheet(wb, "County_Level")

writeData(wb, "State_Level", dat_state)
writeData(wb, "County_Level", dat_county)
saveWorkbook(wb, out_xlsx, overwrite = TRUE)

message("✅ Done. Wrote ", out_xlsx,
        "\n    Rows (STATE): ", nrow(dat_state),
        " | Rows (COUNTY): ", nrow(dat_county))
# ------------------------------------------------------------
# END
# ------------------------------------------------------------
