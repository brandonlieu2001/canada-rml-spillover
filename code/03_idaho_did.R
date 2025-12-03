## Import packages
library(tidyverse)
library(lubridate)
library(zoo)

# Import processed data
idaho_accidents_full_monthly <- 
  read.csv("data_processed/idaho_accident_person_data.csv")

# Add date column
idaho_accidents_full_monthly <- idaho_accidents_full_monthly %>% 
  mutate(DATE = as.Date(paste(YEAR, MONTH, "01", sep = "-")))

# Build quarterly dataset
idaho_accidents_full_quarterly <- idaho_accidents_full_monthly %>%
  mutate(QUARTER = quarter(DATE, with_year = FALSE)) %>%
  group_by(YEAR, QUARTER, COUNTY) %>%
  summarise(n_accidents = sum(n_accidents)) %>%
  arrange(COUNTY, YEAR, QUARTER) %>% 
  ungroup()

# Mutate data to difference-in-difference format
idaho_accidents_did_data <- idaho_accidents_full_quarterly %>%
  mutate(
    POST = ifelse((YEAR >= 2019 | 
                     YEAR == 2018 & QUARTER == 4), 1, 0),
    TREAT = ifelse(COUNTY %in% c(21), 1, 0), # 21 = Boundary County
  ) %>% 
  arrange(COUNTY, YEAR, QUARTER)

# Descriptive statistics
idaho_accidents_did_summary <- idaho_accidents_did_data %>%
  group_by(TREAT, POST) %>%
  summarise(
    mean_accidents = mean(n_accidents),
    sd_accidents = sd(n_accidents),
    n_observations = n() 
  )

# Plot DiD trends
idaho_accidents_plot <- idaho_accidents_did_data %>%
  group_by(TREAT, YEAR, QUARTER) %>%
  summarise(
    mean_accidents = mean(n_accidents),
    n_observations = n()
  ) 

# Convert date to year-quarter format
idaho_accidents_plot <- idaho_accidents_plot %>% 
  mutate(date = sprintf("%i-%i-01", YEAR, (3 * (QUARTER - 1) + 1)))

id_did_plot <-
  ggplot(idaho_accidents_plot, 
         aes(x = as.Date(date), y = mean_accidents, group = TREAT,
               color = as.factor(TREAT))) +
  geom_point() +
  geom_line() +
  scale_x_date(date_labels = "%Y Q%1", date_breaks = "1 year") +
  
  geom_vline(xintercept = as.Date("2018-10-01"),
            linetype = "dashed", color = "black", linewidth = 1) +
  labs(
    title = "Quarterly Fatal Accident Counts, ID",
    x = "Year-quarter",
    y = "Average number of accidents",
    color = "Treatment Group"
  ) +
  theme_bw() +
  scale_color_manual(
    values = c("0" = "#0072B2", "1" = "#D55E00"),
    labels = c("0" = "Control counties (remaining 43 counties)",
               "1" = "Treatment county (Boundary County, ID)"))

# Add mean labels
id_did_plot <- id_did_plot + geom_label(
    data = idaho_accidents_did_summary,
    aes(
      x = as.Date(ifelse(POST == 0, "2016-04-01", "2022-04-01")), 
      y = mean_accidents + 0,
      label = paste0("Mean = ", round(mean_accidents, 3)),
      color = as.factor(TREAT)),
    show.legend = FALSE
  )

id_did_plot

# Naive DiD regression
model <- lm(n_accidents ~ TREAT + POST + (TREAT * POST), 
            data = idaho_accidents_did_data)
summary(model)
