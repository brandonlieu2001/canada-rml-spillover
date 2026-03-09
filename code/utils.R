library(tidyverse)
library(tidycensus)

# File: 01_process_fars_data.R =================================================

# Write function that pulls the monthly cases for a given state AND age filtering
get_monthly_cases <- function(state_fips, min_age, max_age = Inf) {
  
  # Select state and summarise number of accidents by month 
  state <- all_state_accident_person %>% 
    filter(STATE == state_fips,
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
      fips = state_fips,
      # Convert fips to state, ensuring format is a string with sprintf()
      state_name = fips_codes$state_name[fips_codes$state_code == sprintf("%02d", as.integer(state_fips))][1]
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
  model <- lm(n_accidents ~ TREAT*POST, 
              data = did_data)
  summary(model)
}
# File: 06_did_yearly.R =======================================================
# YEARLY resolution where "year" starts in October (fiscal year)
# Example:
#   FY2010 = Jan–Sep 2010
#   FY2011 = Oct 2010–Sep 2011
# And Canada legalization (2018-10-01) is the start of FY2019.

# 1. Build yearly dataset (Oct-start fiscal year) from MONTHLY dataset saved in .csv
build_yearly_dataset_oct <- function(monthly_df) {
  monthly_df %>%
    mutate(
      # fiscal year label: months Oct–Dec roll into next year
      FY = ifelse(MONTH >= 10, YEAR + 1L, YEAR),
      # date representing the start of that fiscal year (Oct 1 of prior calendar year)
      FY_START_DATE = as.Date(sprintf("%i-10-01", FY - 1L))
    ) %>%
    group_by(FY, FY_START_DATE, COUNTY, state_name) %>%
    #group_by(FY, FY_START_DATE, COUNTY, ) %>%
    summarise(n_accidents = sum(n_accidents), .groups = "drop") %>%
    arrange(COUNTY, FY)
}

# 2. Mutate YEARLY dataset into DiD format (POST based on fiscal year)
prepare_did_data_yearly_oct <- function(df_yearly, treat_fips) {
  df_yearly %>%
    filter(FY <= 2023 & FY >= 2011) %>%   # DROP incomplete FY2024 and FY2010
    mutate(
      # FY2019 starts on 2018-10-01, so POST = 1 for FY >= 2019
      POST  = ifelse(FY >= 2019, 1, 0),
      TREAT = ifelse(COUNTY %in% treat_fips, 1, 0)
    ) %>%
    arrange(COUNTY, FY)
}

# 3. Build YEARLY DiD plot (x-axis is fiscal-year start date; vline at 2018-10-01)
make_did_plot_yearly <- function(did_data, title_string) {
  
  plot_data <- did_data %>%
    group_by(TREAT, FY) %>%
    summarise(
      mean_accidents = mean(n_accidents),
      n_observations = n(),
      .groups = "drop"
    )
  
  summary_data <- did_data %>%
    group_by(TREAT, POST) %>%
    summarise(mean_accidents = mean(n_accidents), .groups = "drop")
  
  ggplot(plot_data,
         aes(x = FY, y = mean_accidents,
             group = TREAT, color = as.factor(TREAT))) +
    geom_point() +
    geom_line() +
    geom_vline(
      xintercept = 2019,   # FY2019 starts Oct 2018
      linetype = "dashed",
      color = "black",
      linewidth = 1
    ) +
    scale_x_continuous(
      breaks = seq(min(plot_data$FY), max(plot_data$FY), by = 1)
    ) +
    labs(
      title = title_string,
      x = "Year (Oct-start)",
      y = "Average number of accidents",
      color = "Treatment Group"
    ) +
    theme_bw() +
    scale_color_manual(
      values = c("0" = "#0072B2", "1" = "#D55E00"),
      labels = c("0" = "Control counties", "1" = "Treatment counties")
    ) +
    geom_label(
      data = summary_data,
      aes(
        x = ifelse(POST == 0,
                   min(plot_data$FY) + 1,
                   max(plot_data$FY) - 1),
        y = mean_accidents,
        label = paste0("Mean = ", round(mean_accidents, 3)),
        color = as.factor(TREAT)
      ),
      show.legend = FALSE
    )
}

# File: 07_did_poisson_yearly.R ================================================
read_pop_file <- function(path) {
  read_csv(path, show_col_types = FALSE) %>%
    clean_names() %>% # Replace spaces with underscores with clean_names from janitor package
    select(           # Keep only the columns we actually want
      county,
      county_code,
      year = yearly_july_1st_estimates,
      population
    ) %>%
    mutate(
      year = as.integer(year),
      county_code = as.integer(county_code),
      population = as.numeric(population)
    ) %>% 
    filter(!(is.na(county) & is.na(county_code) & is.na(year) & is.na(population)))
}


read_laus_file <- function(path) {
  laus_file <- read_excel(path, skip = 1) %>% # Skip title row
    clean_names() %>% # Clean header names (replace spaces with underscores and lowercase)
    filter(state_fips_code %in% state_code_lookup$STATE) %>% 
    transmute(
      geoid = sprintf("%02d%03d", as.integer(state_fips_code), as.integer(county_fips_code)),
      FY = as.integer(year),     # `year` treated as FY, as these are annual average & FY defined as Oct-Sep
      unemployment_rate = as.numeric(unemployment_rate_percent)
    )
}

read_income_file <- function(path) {
  header_row_index <- identify_header_row_income(path)
  income_file <- read_excel(path, skip = header_row_index - 1) %>% # Skip all rows until the one before the header
    clean_names() %>%
    rename(
      state_fips_code  = any_of(c("state_fips_code", "state_fips")),
      county_fips_code = any_of(c("county_fips_code", "county_fips"))
    ) %>%
    mutate( # Cast as integer so filtering will work
      state_fips_code  = as.integer(state_fips_code), 
      county_fips_code = as.integer(county_fips_code)) %>% 
    filter(
      state_fips_code %in% state_code_lookup$STATE,
      county_fips_code != 0
    ) %>% 
    transmute(
      geoid = sprintf("%02d%03d", as.integer(state_fips_code), as.integer(county_fips_code)),
      FY = retrieve_year(path),          
      median_household_income = as.numeric(median_household_income)
    )
}



# Helper function that returns the row number where header is
identify_header_row_income <- function (path) {
  lines <-  read_excel(path, n_max = 5, col_names = FALSE) # Read in first 10 lines
  
  row_has_header_names <- apply(lines, 1, function(row) {
    row <- as.character(row)
    
    # each of these returns TRUE if ANY cell in the row contains the pattern
    has_state_fips  <- any(str_detect(row, "State FIPS"))
    has_county_fips <- any(str_detect(row, "County FIPS"))
    has_income      <- any(str_detect(row, "Median Household Income"))
    
    has_state_fips & has_county_fips & has_income
  })
  return(which(row_has_header_names)[1]) # which() gives index which is TRUE
}

# Helper function that retrieves the year from income file
retrieve_year <- function(path) {
  file <- read_excel(path, n_max = 1)
  description <- file[[1]][[1]] # Pulls string of the description that US Census Bureau includes
  year <- str_extract(description, "\\b(201[0-9]|202[0-3])\\b") # str_extract pulls first match, which is year of file
  year <- as.integer(year)
  return(year)
}
