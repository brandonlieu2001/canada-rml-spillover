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

## Create individual state data
