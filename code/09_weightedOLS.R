# Run main spec with weighted OLS TWFE rather than poisson
library(tidyverse)
library(fixest)


# (1) Border = Physical border ====
# Read in analysis df for physical border
physicalborder_df <- read_csv("code/descriptive/physicalborder_df.csv") %>%
  mutate(rate_per_100000 = (n_crashes / pop_total) * 100000,
         FY = as.integer(as.character(FY)),
         time_to_treat = if_else(border == 1, FY - 2019L, 0L),
         FY_fe  = factor(FY),                    # for year fixed effects
         FY_num = as.integer(as.character(FY)),  # for linear trend slopes
         geoid = factor(geoid),
         STATE = factor(STATE),
         border = as.integer(border),
         post_canada = as.integer(post_canada),
         unemployment_rate = as.numeric(unemployment_rate),
         pop_total = as.numeric(pop_total),
         median_household_income_deflated = as.numeric(median_household_income_deflated)
  )

# county FE and year FE
main_control_unemployment_income <- feols(
  rate_per_100000 ~ border * post_canada + unemployment_rate + median_household_income_deflated 
  | geoid + FY_fe, # FEs
  cluster = ~ geoid,
  data    = physicalborder_df,
  weights = physicalborder_df$pop_total
)

etable(main_control_unemployment_income)


# state-specific linear trend
main_control_unemployment_income_statespecific <- feols(
  rate_per_100000 ~ border * post_canada + unemployment_rate + median_household_income_deflated 
  | geoid + FY_fe + STATE[FY_num], # FEs
  cluster = ~ geoid,
  data    = physicalborder_df,
  weights = physicalborder_df$pop_total
)

etable(main_control_unemployment_income_statespecific)


# county FE and state^year FE
main_control_unemployment_income_countyFE_stateyearFE <- feols(
  rate_per_100000 ~ border * post_canada + unemployment_rate + median_household_income_deflated 
  | geoid + STATE^FY_fe, # FEs
  cluster = ~ geoid,
  data    = physicalborder_df,
  weights = physicalborder_df$pop_total
)

etable(main_control_unemployment_income_countyFE_stateyearFE)



# (2) Border = within 50 miles ==== 
# Read in analysis df for physical border, 50 miles
physicalborder50_df <- read_csv("code/descriptive/physicalborder50_df.csv") %>%
  mutate(rate_per_100000 = (n_crashes / pop_total) * 100000,
         FY = as.integer(as.character(FY)),
         time_to_treat = if_else(border == 1, FY - 2019L, 0L),
         FY_fe  = factor(FY),                    # for year fixed effects
         FY_num = as.integer(as.character(FY)),  # for linear trend slopes
         geoid = factor(geoid),
         STATE = factor(STATE),
         border = as.integer(border),
         post_canada = as.integer(post_canada),
         unemployment_rate = as.numeric(unemployment_rate),
         pop_total = as.numeric(pop_total),
         median_household_income_deflated = as.numeric(median_household_income_deflated)
  )

# county FE and year FE
main_control_unemployment_income_50 <- feols(
  rate_per_100000 ~ border * post_canada + unemployment_rate + median_household_income_deflated 
  | geoid + FY_fe, # FEs
  cluster = ~ geoid,
  data    = physicalborder50_df,
  weights = physicalborder50_df$pop_total
)

etable(main_control_unemployment_income_50)


# state-specific linear trend
main_control_unemployment_income_statespecific_50 <- feols(
  rate_per_100000 ~ border * post_canada + unemployment_rate + median_household_income_deflated 
  | geoid + FY_fe + STATE[FY_num], # FEs
  cluster = ~ geoid,
  data    = physicalborder50_df,
  weights = physicalborder50_df$pop_total
)

etable(main_control_unemployment_income_statespecific_50)


# county FE and state^year FE
main_control_unemployment_income_countyFE_stateyearFE_50 <- feols(
  rate_per_100000 ~ border * post_canada + unemployment_rate + median_household_income_deflated 
  | geoid + STATE^FY_fe, # FEs
  cluster = ~ geoid,
  data    = physicalborder50_df,
  weights = physicalborder50_df$pop_total
)

etable(main_control_unemployment_income_countyFE_stateyearFE_50)


# (3) Border = within 100 miles ==== 
# Read in analysis df for physical border, 50 miles
physicalborder100_df <- read_csv("code/descriptive/physicalborder100_df.csv") %>%
  mutate(rate_per_100000 = (n_crashes / pop_total) * 100000,
         FY = as.integer(as.character(FY)),
         time_to_treat = if_else(border == 1, FY - 2019L, 0L),
         FY_fe  = factor(FY),                    # for year fixed effects
         FY_num = as.integer(as.character(FY)),  # for linear trend slopes
         geoid = factor(geoid),
         STATE = factor(STATE),
         border = as.integer(border),
         post_canada = as.integer(post_canada),
         unemployment_rate = as.numeric(unemployment_rate),
         pop_total = as.numeric(pop_total),
         median_household_income_deflated = as.numeric(median_household_income_deflated)
  )

# county FE and year FE
main_control_unemployment_income_100 <- feols(
  rate_per_100000 ~ border * post_canada + unemployment_rate + median_household_income_deflated 
  | geoid + FY_fe, # FEs
  cluster = ~ geoid,
  data    = physicalborder100_df,
  weights = physicalborder100_df$pop_total
)

etable(main_control_unemployment_income_100)


# state-specific linear trend
main_control_unemployment_income_statespecific_100 <- feols(
  rate_per_100000 ~ border * post_canada + unemployment_rate + median_household_income_deflated 
  | geoid + FE_fe + STATE[FY_num], # FEs
  cluster = ~ geoid,
  data    = physicalborder100_df,
  weights = physicalborder100_df$pop_total
)

etable(main_control_unemployment_income_statespecific_100)


# county FE and state^year FE
main_control_unemployment_income_countyFE_stateyearFE_100 <- feols(
  rate_per_100000 ~ border * post_canada + unemployment_rate + median_household_income_deflated 
  | geoid + STATE^FY_fe, # FEs
  cluster = ~ geoid,
  data    = physicalborder100_df,
  weights = physicalborder100_df$pop_total
)

etable(main_control_unemployment_income_countyFE_stateyearFE_100)

