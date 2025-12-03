# Import packages
library(tidyverse)
library(lubridate) # to convert year-month to year-quarter

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
    left_join(person_data[[i]],  accident_data[[i]], by = "ST_CASE")
}

# Create Idaho data across all years
idaho = data.frame()
for (year in year_directory) {
  rows_to_add <- accident_person_data[[year]] %>% 
    select(STATE = STATE.x, COUNTY = COUNTY.x, ST_CASE, 
           YEAR, MONTH = MONTH.x,
           PER_NO, VEH_NO, ST_CASE, AGE) %>% 
    filter(STATE == 16)
  idaho <- bind_rows(idaho, rows_to_add)
}

# Filter data for accidents that only include vehicle occupants >= 19 years
# and no NAs
# VEH_NO = 0 indicates a non-vehicle occupant (e.g., pedestrian, cyclist)
idaho <- idaho %>% 
  filter(AGE >= 19 & !is.na(AGE) & VEH_NO != 0)

# Calculate the number of accidents in all Idaho counties over time
idaho_accidents <- idaho %>% 
  group_by(YEAR, MONTH, COUNTY) %>% 
  summarise(n_accidents = n_distinct(ST_CASE)) %>% 
  arrange(COUNTY, YEAR, MONTH)

# Create a full grid of all year-month-county combinations
all_years_months <- expand_grid(
  YEAR = unique(idaho$YEAR),
  MONTH = unique(idaho$MONTH),
  COUNTY = unique(idaho$COUNTY)
)

# Merge and fill missing counts with 0
idaho_accidents_full_monthly <- all_years_months %>%
  left_join(idaho_accidents, by = c("YEAR", "MONTH", "COUNTY")) %>%
  mutate(n_accidents = replace_na(n_accidents, 0)) %>%
  arrange(COUNTY, YEAR, MONTH)

# Write out Idaho data (aggregated by month) to `data_processed/`
write_csv(idaho_accidents_full_monthly, 
          "data_processed/idaho_accident_person_data.csv")


# Process all states into quarterly format ####################################

# Create df that contains data from all states and years
# all_state_accident_person = data.frame()
# for (year in year_directory) {
#   rows_to_add <- accident_person_data[[year]] %>% 
#     select(STATE = STATE.x, COUNTY = COUNTY.x, ST_CASE, 
#            YEAR, MONTH = MONTH.x,
#            PER_NO, VEH_NO, ST_CASE, AGE) %>% 
#     filter(STATE == 16)
#   idaho <- bind_rows(idaho, rows_to_add)
# }

# Exploratory plotting (monthly) ##############################################
# Add date column
idaho_accidents_full_monthly <- idaho_accidents_full_monthly %>% 
  mutate(DATE = paste(YEAR, MONTH, sep = "-"))

idaho_accidents_full_monthly$DATE <- 
  as.Date(paste0(idaho_accidents_full_monthly$DATE, "-01"))

# Plot using monthly data
ggplot(data = idaho_accidents_full_monthly,
       aes(x = DATE, 
           y = n_accidents, color = as.factor(COUNTY))) +
  geom_point() +
  scale_x_date(date_labels = "%Y", 
               date_breaks = "1 year") +
  geom_vline(xintercept = as.Date("2018-10-15"), 
             linetype = "dashed", color = "red", linewidth = 1) +
  labs(x = "Time (year-month)", 
       y = "Number of fatal accidents",
       color = "County Code",
       title = "Idaho: Fatal Accidents by County Per Month") +
  theme_bw()
