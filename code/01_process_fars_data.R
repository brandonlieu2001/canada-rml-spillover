## Import packages
library(tidyverse)

## Read in `person.csv` & `accident.csv` from each year
year_directory <- list.files("data_raw")

person_filepaths <- paste0("data_raw/", year_directory, "/person.csv")
accident_filepaths <- paste0("data_raw/", year_directory, "/accident.csv")

person_data <- lapply(person_filepaths, read.csv)
accident_data <- lapply(accident_filepaths, read.csv)

## For each year, merge `person.csv` and `accident.csv`
accident_person_data <- list()
num_years <- length(year_directory)

for (i in 1:num_years) {
  accident_person_data[[year_directory[i]]] = 
    left_join(person_data[[i]],  accident_data[[i]], by = "ST_CASE")
}

## Create Idaho data across all years
idaho = data.frame()

for (year in year_directory) {
  rows_to_add <- accident_person_data[[year]] %>% 
    select(STATE.x, COUNTY.x, ST_CASE, YEAR, MONTH.x, PER_NO,
           VEH_NO, PER_NO) %>% 
    filter(STATE.x == 16)
  idaho <- bind_rows(idaho, rows_to_add)
}

## Explore counts of border county in Idaho
idaho %>% filter(COUNTY.x == 21) %>% 
  group_by(YEAR, MONTH.x) %>% 
  summarise(n_accidents = n_distinct(ST_CASE))

## Write out Idaho data to `data_processed/`
write_csv(idaho, "data_processed/idaho_accident_person_data.csv")