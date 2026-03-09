## Import packages
library(tidyverse)
library(lubridate) # to convert year-month to year-quarter
library(patchwork)

## Explore resolution of data (aggregated by month, quarter, and year)

# Import processed data
idaho_accidents_full_monthly <- 
  read.csv("data_processed/idaho_accident_person_data.csv")

# Build quarterly dataset
idaho_accidents_full_quarterly <- idaho_accidents_full_monthly %>%
  mutate(QUARTER = quarter(DATE, with_year = TRUE)) %>%
  group_by(QUARTER, COUNTY) %>%
  summarise(n_accidents = sum(n_accidents)) %>%
  arrange(COUNTY, QUARTER) %>% 
  ungroup()

# Build yearly dataset
idaho_accidents_full_yearly <- idaho_accidents_full_monthly %>%
  group_by(YEAR, COUNTY) %>%
  summarise(n_accidents = sum(n_accidents)) %>%
  arrange(COUNTY, YEAR) %>% 
  ungroup()

# Summary table of all three resolutions
idaho_accidents_summary <- data.frame(
  resolution = c("Monthly", "Quarterly", "Yearly"),
  n_observations = c(
    nrow(idaho_accidents_full_monthly),
    nrow(idaho_accidents_full_quarterly),
    nrow(idaho_accidents_full_yearly)
  ),
  max = c(
    idaho_accidents_full_monthly %>% summarise(max_month = max(n_accidents)) %>% pull(),
    idaho_accidents_full_quarterly %>% summarise(max_qtr = max(n_accidents)) %>% pull(),
    idaho_accidents_full_yearly %>% summarise(max_year = max(n_accidents)) %>% pull()
  ),
  percent_zero = c(
    idaho_accidents_full_monthly %>% summarise(pct_zero_month = mean(n_accidents == 0) * 100) %>% pull(),
    idaho_accidents_full_quarterly %>% summarise(pct_zero_qtr = mean(n_accidents == 0) * 100) %>% pull(),
    idaho_accidents_full_yearly %>% summarise(pct_zero_year = mean(n_accidents == 0) * 100) %>% pull()
  ),
  mean_count = c(
    idaho_accidents_full_monthly %>% summarise(avg_accidents_per_month = mean(n_accidents)) %>% pull(),
    idaho_accidents_full_quarterly %>% summarise(avg_accidents_per_qtr = mean(n_accidents)) %>% pull(),
    idaho_accidents_full_yearly %>% summarise(avg_accidents_per_year = mean(n_accidents)) %>% pull()
  ),
  median_count = c(
    idaho_accidents_full_monthly %>% summarise(median_accidents_per_month = median(n_accidents)) %>% pull(),
    idaho_accidents_full_quarterly %>% summarise(median_accidents_per_qtr = median(n_accidents)) %>% pull(),
    idaho_accidents_full_yearly %>% summarise(median_accidents_per_year = median(n_accidents)) %>% pull()
  ),
  sd = c(
    idaho_accidents_full_monthly %>% summarise(sd_month = sd(n_accidents)) %>% pull(),
    idaho_accidents_full_quarterly %>% summarise(sd_qtr = sd(n_accidents)) %>% pull(),
    idaho_accidents_full_yearly %>% summarise(sd_year = sd(n_accidents)) %>% pull()
  )
)

x_levels <- 0:35
# Histogram, resolution = month
monthly <- idaho_accidents_full_monthly %>%
  ggplot(aes(x = factor(n_accidents))) +
  geom_bar(aes(y = after_stat(100 * count / sum(count)))) +
  scale_y_continuous(breaks = seq(0, 100, 10), limits = c(0, 100)) +
  scale_x_discrete(limits = as.character(x_levels),
                   breaks = seq(0, 35, 2)) +
  labs(
    x = "Fatal crash count",
    y = "% of observations",
    title = paste0("Monthly resolution", " (n=", idaho_accidents_summary[1,2]
                   , ")")) +
  theme_bw()

# Histogram, resolution = quarterly
quarterly <- idaho_accidents_full_quarterly %>%
  ggplot(aes(x = factor(n_accidents))) +
  geom_bar(aes(y = after_stat(100 * count / sum(count)))) +
  scale_y_continuous(breaks = seq(0, 100, 10), limits = c(0, 100)) +
  scale_x_discrete(limits = as.character(x_levels),
                   breaks = seq(0, 35, 2)) +
  labs(x = "Fatal crash count", 
       y = "% of observations",
       title = paste0("Quarterly resolution", " (n=", 
                      idaho_accidents_summary[2,2]
                      , ")")) +
  theme_bw()

# Histogram, resolution = yearly
yearly <- idaho_accidents_full_yearly %>%
  ggplot(aes(x = factor(n_accidents))) +
  geom_bar(aes(y = after_stat(100 * count / sum(count)))) +
  scale_y_continuous(breaks = seq(0, 100, 10), limits = c(0, 100)) +
  scale_x_discrete(limits = as.character(x_levels),
                   breaks = seq(0, 35, 2)) +
  labs(x = "Fatal crash count", y = "% of observations", 
       title = paste0("Yearly resolution", " (n=", idaho_accidents_summary[3,2]
                      , ")")) +
  theme_bw()

monthly + quarterly + yearly
