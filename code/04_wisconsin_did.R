## Import packages
library(tidyverse)
library(lubridate) # to convert year-month to year-quarter
library(zoo) # for as.yearqtr
library(patchwork)

# Wisconsin data cleaning
wisconsin = data.frame()
for (year in year_directory) {
  rows_to_add <- accident_person_data[[year]] %>% 
    select(STATE = STATE.x, COUNTY = COUNTY.x, ST_CASE, 
           YEAR, MONTH = MONTH.x,
           PER_NO, VEH_NO, ST_CASE, AGE) %>% 
    filter(STATE == 55)
  wisconsin <- bind_rows(wisconsin, rows_to_add)
}

# Filter data for accidents that only include vehicle occupants >= 19 years
wisconsin <- wisconsin %>% 
  filter(AGE >= 19 & !is.na(AGE) & VEH_NO != 0 & COUNTY != 0)

# Calculate the number of accidents in all Wisconsin counties over time
wisconsin_accidents <- wisconsin %>% 
  group_by(YEAR, MONTH, COUNTY) %>% 
  summarise(n_accidents = n_distinct(ST_CASE)) %>% 
  arrange(COUNTY, YEAR, MONTH)

# Create a full grid of all year-month-county combinations
all_years_months <- expand_grid(
  YEAR = unique(wisconsin$YEAR),
  MONTH = unique(wisconsin$MONTH),
  COUNTY = unique(wisconsin$COUNTY)
)

# Merge and fill missing counts with 0
wisconsin_accidents_full_monthly <- all_years_months %>%
  left_join(wisconsin_accidents, by = c("YEAR", "MONTH", "COUNTY")) %>%
  mutate(n_accidents = replace_na(n_accidents, 0)) %>%
  arrange(COUNTY, YEAR, MONTH)

# Write out Wisconsin data (aggregated by month) to `data_processed/`
write_csv(wisconsin_accidents_full_monthly, 
          "data_processed/wisconsin_accident_person_data.csv")

################################################################################
# Wisconsin DiD Analysis

# Import processed data
wisconsin_accidents_full_monthly <- 
  read.csv("data_processed/wisconsin_accident_person_data.csv")

# Add date column
wisconsin_accidents_full_monthly <- wisconsin_accidents_full_monthly %>% 
  mutate(DATE = as.Date(paste(YEAR, MONTH, "01", sep = "-")))

# Build quarterly dataset
wisconsin_accidents_full_quarterly <- wisconsin_accidents_full_monthly %>%
  mutate(QUARTER = quarter(DATE, with_year = FALSE)) %>%
  group_by(YEAR, QUARTER, COUNTY) %>%
  summarise(n_accidents = sum(n_accidents)) %>%
  arrange(COUNTY, YEAR, QUARTER) %>% 
  ungroup()

# Mutate data to difference-in-difference format
wisconsin_accidents_did_data <- wisconsin_accidents_full_quarterly %>%
  mutate(
    POST = ifelse((YEAR >= 2019 | 
                     YEAR == 2018 & QUARTER == 4), 1, 0),
    TREAT = ifelse(COUNTY %in% c(3, 7, 31, 51), 1, 0), # 3, 7, 31, 51 
  ) %>% 
  arrange(COUNTY, YEAR, QUARTER)

# Descriptive statistics
wisconsin_accidents_did_summary <- wisconsin_accidents_did_data %>%
  group_by(TREAT, POST) %>%
  summarise(
    mean_accidents = mean(n_accidents),
    sd_accidents = sd(n_accidents),
    n_observations = n() 
  )

# Plot DiD trends
wisconsin_accidents_plot <- wisconsin_accidents_did_data %>%
  group_by(TREAT, YEAR, QUARTER) %>%
  summarise(
    mean_accidents = mean(n_accidents),
    n_observations = n()
  ) 

# Convert date to year-quarter format
wisconsin_accidents_plot <- wisconsin_accidents_plot %>% 
  mutate(date = sprintf("%i-%i-01", YEAR, (3 * (QUARTER - 1) + 1)))

wi_did_plot <-
  ggplot(wisconsin_accidents_plot, 
         aes(x = as.Date(date), y = mean_accidents, group = TREAT,
             color = as.factor(TREAT))) +
  geom_point() +
  geom_line() +
  scale_x_date(date_labels = "%Y Q%1", date_breaks = "1 year") +
  
  geom_vline(xintercept = as.Date("2018-10-01"),
             linetype = "dashed", color = "black", linewidth = 1) +
  labs(
    title = "Quarterly Fatal Accident Counts, WI",
    x = "Year-quarter",
    y = "Average number of accidents",
    color = "Treatment Group"
  ) +
  theme_bw() +
  scale_color_manual(
    values = c("0" = "#0072B2", "1" = "#D55E00"),
    labels = c("0" = "Control counties (remaining 68 counties)",
               "1" = "Treatment counties (Douglas, Bayfield, Iron, Ashland)"))


# Add mean labels
wi_did_plot <- wi_did_plot + geom_label(
  data = wisconsin_accidents_did_summary,
  aes(
    x = as.Date(ifelse(POST == 0, "2016-04-01", "2022-04-01")), 
    y = mean_accidents,
    label = paste0("Mean = ", round(mean_accidents, 3)),
    color = as.factor(TREAT)),
  show.legend = FALSE
)
wi_did_plot

# Naive DiD regression for WI
model <- lm(n_accidents ~ TREAT + POST + (TREAT * POST), 
            data = wisconsin_accidents_did_data)
summary(model)

################################################################################

# WI + ID pooled DiD

idaho_accidents_did_data <- idaho_accidents_did_data %>%
  mutate(STATE = "ID")

wisconsin_accidents_did_data <- wisconsin_accidents_did_data %>%
  mutate(STATE = "WI")

WI_ID_did_data <- bind_rows(
  idaho_accidents_did_data,
  wisconsin_accidents_did_data
)

# Naive regression, WI + ID
both_model <- lm(n_accidents ~ TREAT * POST, data = WI_ID_did_data)
summary(both_model)

## Plot for both
idaho_accidents_plot <- idaho_accidents_plot %>%
  mutate(STATE = "ID")

wisconsin_accidents_plot <- wisconsin_accidents_plot %>%
  mutate(STATE = "WI")

WI_ID_did_plot <- bind_rows(
  idaho_accidents_did_data,
  wisconsin_accidents_did_data
) %>%
  mutate(date = as.Date(sprintf("%i-%i-01", YEAR, 3*(QUARTER-1)+1))) %>%
  group_by(TREAT, date) %>%
  summarise(
    mean_accidents = mean(n_accidents),
    .groups = "drop"
  )

combined_did_plot <-
  ggplot(WI_ID_did_plot, 
         aes(x = as.Date(date), y = mean_accidents, group = TREAT,
             color = as.factor(TREAT))) +
  geom_point() +
  geom_line() +
  scale_x_date(date_labels = "%Y Q%1", date_breaks = "1 year") +
  
  geom_vline(xintercept = as.Date("2018-10-01"),
             linetype = "dashed", color = "black", linewidth = 1) +
  labs(
    title = "Quarterly Fatal Accident Counts, WI+ID",
    x = "Year-quarter",
    y = "Average number of accidents",
    color = "Treatment Group"
  ) +
  theme_bw() +
  scale_color_manual(
    values = c("0" = "#0072B2", "1" = "#D55E00"),
    labels = c("0" = "Control counties",
               "1" = "Treatment counties"))
combined_did_plot

# summary for means
WI_ID_accidents_did_summary <- WI_ID_did_data %>%
  group_by(TREAT, POST) %>%
  summarise(
    mean_accidents = mean(n_accidents),
    sd_accidents = sd(n_accidents),
    n_observations = n() 
  )


# Add mean labels
combined_did_plot <-combined_did_plot + geom_label(
  data = WI_ID_accidents_did_summary,
  aes(
    x = as.Date(ifelse(POST == 0, "2016-04-01", "2022-04-01")), 
    y = mean_accidents + 0,
    label = paste0("Mean = ", round(mean_accidents, 3)),
    color = as.factor(TREAT)),
  show.legend = FALSE
)

stacked_plots <- id_did_plot / wi_did_plot / combined_did_plot
stacked_plots
