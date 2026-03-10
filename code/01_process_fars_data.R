# 01_process_fars_data.R ----
# Takes the accident.csv and person.csv files from FARS and cleans them up 
# into monthly fatal crashes count by state, year, and county.

# Import packages
library(tidyverse)

# Read in `person.csv` & `accident.csv` from each year
year_directory <- list.files("data/data_raw")

person_filepaths <- paste0("data/data_raw/", year_directory, "/person.csv")
accident_filepaths <- paste0("data/data_raw/", year_directory, "/accident.csv")

person_data <- lapply(person_filepaths, read.csv)
accident_data <- lapply(accident_filepaths, read.csv)

# For each year, merge `person.csv` and `accident.csv` ====
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

# All unique fatal accidents ====
fatal_crashes_monthly <- all_state_accident_person %>%
  distinct(STATE, YEAR, MONTH, COUNTY, ST_CASE) %>%       # 1 row per crash
  count(STATE, YEAR, MONTH, COUNTY, name = "n_fatal_crashes") %>% 
  mutate(
    STATE = as.integer(STATE),
    COUNTY = as.integer(COUNTY),
    geoid = sprintf("%02d%03d", STATE, COUNTY) # 5-digit GEOID (state 2 digits + county 3 digits)
    ) %>% 
    arrange(STATE, COUNTY, YEAR, MONTH)

# Balance panel and write to csv =====
fatal_crashes_monthly_balanced <- fatal_crashes_monthly %>%
  complete(
    nesting(STATE, COUNTY, geoid),           # All unique counties as unit
    YEAR = full_seq(YEAR, 1),                # All years
    MONTH = 1:12,                            # All months
    fill = list(n_fatal_crashes = 0)         # Zero crashes for missing
  )

# Write to CSV
write_csv(fatal_crashes_monthly_balanced, "data/data_processed/fatal_crashes_monthly.csv")
