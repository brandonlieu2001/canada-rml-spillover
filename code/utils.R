
# File: 01_process_fars_data.R =================================================

# Write function that pulls the monthly cases for a given state
get_monthly_cases <- function(state_fips, min_age, max_age = Inf) {
  # Select state and summarise number of accidents by month 
  state <- all_state_accident_person %>% 
    filter(STATE == state_fips,
           AGE != 999,          # FARS defines unknown age from 999 from 2008+
           VEH_NO != 0,         # VEH_NO = 0 for non-motor vehicle occupants
           COUNTY != 0,         # Some states have missing county information coded as 0
           
           AGE >= min_age,
           AGE <= max_age       # if max_age = Inf, this always resolves to TRUE
    ) %>%
    group_by(YEAR, MONTH, COUNTY) %>%
    summarise(n_accidents = n_distinct(ST_CASE))
  
  # Build state-specific full grid
  full_grid <- expand_grid(
    YEAR   = seq(min(state$YEAR), max(state$YEAR)), # full year range
    MONTH  = 1:12,                                  # all months
    COUNTY = unique(state$COUNTY)
  )
  
  # Join and fill zeroes
  state <- full_grid %>% 
    left_join(state, by = c("YEAR", "MONTH", "COUNTY")) %>%
    mutate(
      n_accidents = replace_na(n_accidents, 0),
      STATE = state_fips
    ) %>%
    arrange(COUNTY, YEAR, MONTH)
  
  return(state)
}

# File: 05_did_quarterly.R =====================================================

# 1. Build quarterly dataset from monthly dataset saved in .csv

build_quarterly_dataset <- function(df) {
  df %>%
    mutate(DATE = as.Date(paste(YEAR, MONTH, "01", sep = "-"))) %>%
    mutate(QUARTER = quarter(DATE, with_year = FALSE)) %>%
    group_by(YEAR, QUARTER, COUNTY) %>%
    summarise(n_accidents = sum(n_accidents)) %>%
    arrange(COUNTY, YEAR, QUARTER) %>% 
    ungroup()
}

# 2. Mutate dataset into DiD format
prepare_did_data <- function(df_quarterly, treat_fips) {
  
  df_quarterly %>%
    mutate(
      POST = ifelse((YEAR >= 2019 | (YEAR == 2018 & QUARTER == 4)), 1, 0),
      TREAT = ifelse(COUNTY %in% treat_fips, 1, 0)
    ) %>%
    arrange(COUNTY, YEAR, QUARTER)
}

# 3. Build DiD plot
make_did_plot <- function(did_data, title_string, control_label, treat_label) {
  
  # Compute plot-level summary
  plot_data <- did_data %>%
    group_by(TREAT, YEAR, QUARTER) %>%
    summarise(
      mean_accidents = mean(n_accidents),
      n_observations = n()
    ) %>%
    mutate(date = sprintf("%i-%i-01", YEAR, (3 * (QUARTER - 1) + 1)))
  
  # Summary for labels
  summary_data <- did_data %>%
    group_by(TREAT, POST) %>%
    summarise(mean_accidents = mean(n_accidents))
  
  # Build plot
  plot <-
    ggplot(plot_data, 
           aes(x = as.Date(date), y = mean_accidents, 
               group = TREAT, color = as.factor(TREAT))) +
    geom_point() +
    geom_line() +
    scale_x_date(date_labels = "%Y Q%1", date_breaks = "1 year") +
    geom_vline(xintercept = as.Date("2018-10-01"),
               linetype = "dashed", color = "black", linewidth = 1) +
    labs(
      title = title_string,
      x = "Year-quarter",
      y = "Average number of accidents",
      color = "Treatment Group"
    ) +
    theme_bw() +
    scale_color_manual(
      values = c("0" = "#0072B2", "1" = "#D55E00"),
      labels = c("0" = control_label, "1" = treat_label)
    ) +
    geom_label(
      data = summary_data,
      aes(
        x = as.Date(ifelse(POST == 0, "2016-04-01", "2022-04-01")), 
        y = mean_accidents,
        label = paste0("Mean = ", round(mean_accidents, 3)),
        color = as.factor(TREAT)
      ),
      show.legend = FALSE
    )
  
  return(plot)
}

# 4. Run naive DiD regression
run_naive_did <- function(did_data) {
  model <- lm(n_accidents ~ TREAT + POST + (TREAT * POST), 
              data = did_data)
  summary(model)
}
# File: 06_did_yearly.R =======================================================
