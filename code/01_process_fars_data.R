# Import packages
library(tidyverse)
library(lubridate) # to convert year-month to year-quarter
source("code/utils.R")


# Read in `person.csv` & `accident.csv` from each year
year_directory <- list.files("data/data_raw")

person_filepaths <- paste0("data/data_raw/", year_directory, "/person.csv")
accident_filepaths <- paste0("data/data_raw/", year_directory, "/accident.csv")

person_data <- lapply(person_filepaths, read.csv)
accident_data <- lapply(accident_filepaths, read.csv)

# For each year, merge `person.csv` and `accident.csv`
accident_person_data <- list()
num_years <- length(year_directory)

for (i in 1:num_years) {
  accident_person_data[[year_directory[i]]] = 
    left_join(person_data[[i]], accident_data[[i]], by = "ST_CASE")
}

# Create df that contains data from ALL states and years
all_state_accident_person <-
  bind_rows(lapply(year_directory, function(year) {
    accident_person_data[[year]] %>%
      select(
        STATE = STATE.x,
        COUNTY = COUNTY.x,
        ST_CASE,
        YEAR,
        MONTH = MONTH.x,
        PER_NO, VEH_NO,
        AGE
      )
  }))

# Filter out missing data
all_state_accident_person <- 
  all_state_accident_person %>% filter(
       AGE != 999 & AGE != 998,                                     # FARS defines unknown / no report age as 998, 999
       VEH_NO != 0,                                                 # VEH_NO = 0 for non-motor vehicle occupants
       COUNTY != 0 & COUNTY != 997 & COUNTY != 998 & COUNTY != 999  # Missing county information coded as 0, 997, 998, 999
  )

# # Process all states from `accident_person_data` into monthly format ===========
# # Note: get_monthly_cases allows for age filtering, not
# # used in main analysis. Recycle of exploratory analyses by age?
# # Alaska (FIPS 2): ages 19–20
# alaska_accidents_monthly <- get_monthly_cases(02, 19, 20)
# 
# # Washington (FIPS 53): ages 19–20
# washington_accidents_monthly <- get_monthly_cases(53, 19, 20)
# 
# # Maine (FIPS 23): ages 19–20
# maine_accidents_monthly <- get_monthly_cases(23, 19, 20)
# 
# # Vermont (FIPS 50): No treated age group, omit
# vermont_accidents_monthly <- get_monthly_cases(50, 19, 20)
# 
# # Montana (FIPS 30): ages 18+
# montana_accidents_monthly <- get_monthly_cases(30, 18)
# 
# # Minnesota (FIPS 27): ages 19+
# minnesota_accidents_monthly <- get_monthly_cases(27, 19)
# 
# # New York (FIPS 36): ages 19+
# newyork_accidents_monthly <- get_monthly_cases(36, 19)
# 
# # Michigan (FIPS 26): ages 19+
# michigan_accidents_monthly <- get_monthly_cases(26, 19)
# 
# # North Dakota (FIPS 38): ages 19+
# northdakota_accidents_monthly <- get_monthly_cases(38, 19)
# 
# # Ohio (FIPS 39): ages 19+
# ohio_accidents_monthly <- get_monthly_cases(39, 19)
# 
# # Pennsylvania (FIPS 42): ages 19+
# pennsylvania_accidents_monthly <- get_monthly_cases(42, 19)
# 
# # New Hampshire (FIPS 33): ages 21+
# newhampshire_accidents_monthly <- get_monthly_cases(33, 21)
# 
# # Idaho (FIPS 16): ages 19+
# idaho_accidents_monthly <- get_monthly_cases(16, 19)
# 
# # Wisconsin (FIPS 55): ages 19+
# wisconsin_accidents_monthly <- get_monthly_cases(55, 19)
# 
# 
# 
# # Write out data (aggregated by month) to `data_processed/`
# write_csv(alaska_accidents_monthly,       "data_processed/alaska_accidents_monthly.csv")
# write_csv(washington_accidents_monthly,   "data_processed/washington_accidents_monthly.csv")
# write_csv(maine_accidents_monthly,        "data_processed/maine_accidents_monthly.csv")
# write_csv(vermont_accidents_monthly,    "data_processed/vermont_accidents_monthly.csv")
# write_csv(montana_accidents_monthly,      "data_processed/montana_accidents_monthly.csv")
# write_csv(minnesota_accidents_monthly,    "data_processed/minnesota_accidents_monthly.csv")
# write_csv(newyork_accidents_monthly,      "data_processed/newyork_accidents_monthly.csv")
# write_csv(michigan_accidents_monthly,     "data_processed/michigan_accidents_monthly.csv")
# write_csv(northdakota_accidents_monthly,  "data_processed/northdakota_accidents_monthly.csv")
# write_csv(ohio_accidents_monthly,         "data_processed/ohio_accidents_monthly.csv")
# write_csv(pennsylvania_accidents_monthly, "data_processed/pennsylvania_accidents_monthly.csv")
# write_csv(newhampshire_accidents_monthly, "data_processed/newhampshire_accidents_monthly.csv")
# write_csv(idaho_accidents_monthly,        "data_processed/idaho_accidents_monthly.csv")
# write_csv(wisconsin_accidents_monthly,    "data_processed/wisconsin_accidents_monthly.csv")
# 





# # Merge population files
# 
# # File paths
# pop_2010_2020_path <- "data_population/Single-Race Population Estimates 2010-2020.csv"
# pop_2020_2024_path <- "data_population/Single-Race Population Estimates 2020-2024.csv"
# 
# # Read + clean function
# read_pop_file <- function(path) {
#   read_csv(path, show_col_types = FALSE) %>%
#     janitor::clean_names() %>%
#     # Drop "Total" summary rows
#     filter(is.na(notes) | notes != "Total") %>%
#     # Keep only the columns we actually want
#     select(
#       county,
#       county_code,
#       year = yearly_july_1st_estimates,
#       population
#     ) %>%
#     mutate(
#       year = as.integer(year),
#       county_code = as.integer(county_code),
#       population = as.numeric(population)
#     )
# }
# 
# # Read both files
# pop_2010_2020 <- read_pop_file(pop_2010_2020_path)
# pop_2020_2024 <- read_pop_file(pop_2020_2024_path)
# 
# # Deduplicate 2020:
# # keep 2010-2019 from the first file
# # keep 2020-2024 from the second file
# pop_2010_2024 <- bind_rows(
#   pop_2010_2020 %>% filter(year <= 2019),
#   pop_2020_2024 %>% filter(year >= 2020)
# ) %>%
#   arrange(county_code, year)
# 
# # Safety check: no duplicates left
# dup_check <- pop_2010_2024 %>%
#   count(county_code, year) %>%
#   filter(n > 1)
# 
# print(dup_check)
# 
# # Optional: write merged file
# write_csv(pop_2010_2024, "data_population/Single-Race Population Estimates 2010-2024_deduped.csv")







# Poisson with offset(pop), all unique fatal accidents =========================
fatal_crashes_monthly <- all_state_accident_person %>%
  distinct(STATE, YEAR, MONTH, COUNTY, ST_CASE) %>%       # 1 row per crash
  count(STATE, YEAR, MONTH, COUNTY, name = "n_fatal_crashes") %>% 
  mutate(
    STATE = as.integer(STATE),
    COUNTY = as.integer(COUNTY),
    geoid = sprintf("%02d%03d", STATE, COUNTY) # 5-digit GEOID (state 2 digits + county 3 digits)
    ) %>% 
    arrange(STATE, COUNTY, YEAR, MONTH)

# Balance panel and write to csv ===============================================
fatal_crashes_monthly_balanced <- fatal_crashes_monthly %>%
  complete(
    nesting(STATE, COUNTY, geoid),           # All unique counties as unit
    YEAR = full_seq(YEAR, 1),                # All years
    MONTH = 1:12,                            # All months
    fill = list(n_fatal_crashes = 0)         # Zero crashes for missing
  )

fatal_crashes_monthly_balanced

# Write balanced panel to .csv 
write_csv(fatal_crashes_monthly_balanced, "data_processed/fatal_crashes_monthly.csv" )
