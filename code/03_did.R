## Import packages
library(tidyverse)
library(lubridate) # to convert year-month to year-quarter
library(zoo) # for as.yearqtr

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

# Mutate data to difference-in-difference format
idaho_accidents_did_data <- idaho_accidents_full_quarterly %>%
  mutate(
    POST = ifelse(QUARTER >= 2018.4, 1, 0),
    TREAT = ifelse(COUNTY %in% c(21), 1, 0), # 21 = Boundary County
  )

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
  group_by(TREAT, QUARTER) %>%
  summarise(
    mean_accidents = mean(n_accidents),
    n_observations = n()
  )

did_plot <-
  ggplot(idaho_accidents_plot, aes(x = QUARTER, 
                                   y = mean_accidents, 
                                   color = as.factor(TREAT))) +
  geom_line() +
  geom_point() +
  geom_vline(xintercept = as.yearqtr("2018 Q4"),
             linetype = "dashed", color = "black", linewidth = 1) +
  annotate("text", x = as.yearqtr("2018 Q4"), y = 2.1,
           label = "Canada RML (Oct 2018)", 
           size = 3.5, hjust = -0.1, vjust = -0.5) +
  scale_x_yearqtr(format = "%Y Q%q", n = 10) +
  scale_y_continuous(limits = c(0, 2.2), breaks = seq(0, 2, 0.5)) +
  scale_color_manual(
    values = c("0" = "#0072B2", "1" = "#D55E00"),
    labels = c("0" = "Control counties (remaining 43 counties)",
               "1" = "Treatment county (Boundary County, ID)"),
  ) +
  labs(
    title = "Quarterly Fatal Accident Counts, Idaho",
    x = "Year-quarter",
    y = "Average number of accidents",
    color = "Treatment Group"
  ) +
  theme_bw()

# Add mean labels
did_plot + geom_label(
    data = idaho_accidents_did_summary,
    aes(
      x = ifelse(POST == 0, as.yearqtr("2016 Q2"), as.yearqtr("2022 Q2")), 
      y = mean_accidents + 0,
      label = paste0("Mean = ", round(mean_accidents, 3)),
      color = as.factor(TREAT)),
    show.legend = FALSE
  )
