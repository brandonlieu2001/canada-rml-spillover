# Import packages
library(tidyverse)
library(lubridate) # to convert year-month to year-quarter
source("code/utils.R")


# Read in `person.csv` & `accident.csv` from each year
year_directory <- list.files("data_raw")

person_filepaths <- paste0("data_raw/", year_directory, "/person.csv")
accident_filepaths <- paste0("data_raw/", year_directory, "/accident.csv")

person_data <- lapply(person_filepaths, read.csv)
accident_data <- lapply(accident_filepaths, read.csv)

# For each year, merge `person.csv` and `accident.csv`
accident_person_data <- list()
num_years <- length(year_directory)

for (i in 1:num_years) {
  accident_person_data[[year_directory[i]]] = 
    left_join(person_data[[i]], accident_data[[i]], by = "ST_CASE")
}

## Process all states into monthly format

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

# Alaska (FIPS 2): ages 19–20
alaska_accidents_monthly <- get_monthly_cases(2, 19, 20)

# Washington (FIPS 53): ages 19–20
washington_accidents_monthly <- get_monthly_cases(53, 19, 20)

# Maine (FIPS 23): ages 19–20
maine_accidents_monthly <- get_monthly_cases(23, 19, 20)

# Vermont (FIPS 50): No treated age group, omit
vermont_accidents_monthly <- get_monthly_cases(50, 19, 20)

# Montana (FIPS 30): ages 18+
montana_accidents_monthly <- get_monthly_cases(30, 18)

# Minnesota (FIPS 27): ages 19+
minnesota_accidents_monthly <- get_monthly_cases(27, 19)

# New York (FIPS 36): ages 19+
newyork_accidents_monthly <- get_monthly_cases(36, 19)

# Michigan (FIPS 26): ages 19+
michigan_accidents_monthly <- get_monthly_cases(26, 19)

# North Dakota (FIPS 38): ages 19+
northdakota_accidents_monthly <- get_monthly_cases(38, 19)

# Ohio (FIPS 39): ages 19+
ohio_accidents_monthly <- get_monthly_cases(39, 19)

# Pennsylvania (FIPS 42): ages 19+
pennsylvania_accidents_monthly <- get_monthly_cases(42, 19)

# New Hampshire (FIPS 33): ages 21+
newhampshire_accidents_monthly <- get_monthly_cases(33, 21)

# Idaho (FIPS 16): ages 19+
idaho_accidents_monthly <- get_monthly_cases(16, 19)

# Wisconsin (FIPS 55): ages 19+
wisconsin_accidents_monthly <- get_monthly_cases(55, 19)

# Write out data (aggregated by month) to `data_processed/`
write_csv(alaska_accidents_monthly,       "data_processed/alaska_accidents_monthly.csv")
write_csv(washington_accidents_monthly,   "data_processed/washington_accidents_monthly.csv")
write_csv(maine_accidents_monthly,        "data_processed/maine_accidents_monthly.csv")
write_csv(vermont_accidents_monthly,    "data_processed/vermont_accidents_monthly.csv")
write_csv(montana_accidents_monthly,      "data_processed/montana_accidents_monthly.csv")
write_csv(minnesota_accidents_monthly,    "data_processed/minnesota_accidents_monthly.csv")
write_csv(newyork_accidents_monthly,      "data_processed/newyork_accidents_monthly.csv")
write_csv(michigan_accidents_monthly,     "data_processed/michigan_accidents_monthly.csv")
write_csv(northdakota_accidents_monthly,  "data_processed/northdakota_accidents_monthly.csv")
write_csv(ohio_accidents_monthly,         "data_processed/ohio_accidents_monthly.csv")
write_csv(pennsylvania_accidents_monthly, "data_processed/pennsylvania_accidents_monthly.csv")
write_csv(newhampshire_accidents_monthly, "data_processed/newhampshire_accidents_monthly.csv")
write_csv(idaho_accidents_monthly,        "data_processed/idaho_accidents_monthly.csv")
write_csv(wisconsin_accidents_monthly,    "data_processed/wisconsin_accidents_monthly.csv")


