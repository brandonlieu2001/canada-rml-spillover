library(tidyverse)
library(janitor)
library(readxl)
library(readr)
library(fixest)
source("code/utils.R")

# 0) Read in file & create treated (border) counties lookup & state lookup =====
fatal_crashes_monthly <- read_csv("data/data_processed/fatal_crashes_monthly.csv")   

treat_fips_lookup_physicalborder <-
  readRDS("data/data_treatment/treat_fips_lookup_physicalborder.rds")

treat_fips_lookup_within50 <-
  readRDS("data/data_treatment/treat_fips_lookup_within50.rds")

treat_fips_lookup_within100 <-
  readRDS("data/data_treatment/treat_fips_lookup_within100.rds")

# `treat_fips_lookup_bordercrossing` is from an early draft where BNL hand-checked PoE.
# treat_fips_lookup_bordercrossing <- list(
#   # "alaska" = c(185, 290, 240, 66, 282, 105, 230, 100, 110, 195, 275, 130),
#   "idaho" = c(21),
#   "maine" = c(7, 25, 3, 29),
#   "michigan" = c(147, 33, 163),
#   "minnesota" = c(69, 135, 77, 71, 137, 75, 31),
#   "montana" = c(53, 29, 35, 101, 51, 41, 5, 71, 105, 19, 91),
#   "newhampshire" = c(7),
#   "newyork" = c(29, 63, 45, 89, 33, 19),
#   "northdakota" = c(23, 13, 75, 9, 79, 95, 19, 67),
#   "ohio" = c(143),
#   "pennsylvania" = c(49),
#   "vermont" = c(11, 13, 9, 19),
#   "washington" = c(9, 55, 73, 47, 19, 65, 51)
#   # "wisconsin" = c(31, 7, 3, 51)
# )

state_code_lookup <- tribble(
  ~state_name,      ~STATE,
  # "alaska",           2,
  "washington",      53,
  "maine",           23,
  "vermont",         50,
  "montana",         30,
  "minnesota",       27,
  "newyork",         36,
  "michigan",        26,
  "northdakota",     38,
  "ohio",            39,
  "pennsylvania",    42,
  "newhampshire",    33,
  "idaho",           16
  # "wisconsin",       55
)


# 1) Build county x FY (Oct–Sep) crash counts from fatal_crashes_monthly =======
#  Note: fatal_crashes_monthly has: STATE, YEAR, MONTH, COUNTY, n_fatal_crashes
df_base <- fatal_crashes_monthly %>%
  filter(STATE %in% state_code_lookup$STATE) %>% # Filter to only 13 states bordering Canada (omitting Alaska)
  mutate(
    # Redefine year to align with Canada's legalization: Oct–Dec => next FY
    FY = if_else(MONTH >= 10, YEAR + 1L, YEAR),
    # Post period: FY2019 is first full FY starting from Oct2018
    post_canada = as.integer(FY >= 2019L),
  ) %>%
  filter(FY >= 2011L, FY <= 2023L) %>%  # keep only complete FYs
  group_by(STATE, COUNTY, geoid, FY, post_canada) %>%
  summarise(
    n_crashes = sum(n_fatal_crashes, na.rm = TRUE),
    .groups = "drop"
  )

# 2) Create border indicator column (treated counties) =========================
# Step 1. Convert new df, `treat_counties` with state, geoid (to be left_join'ed to df_base)
# Step 2. df_base %>% left_join(treat_counties) by state and geoid

# OLD. Treatment = counties only having border crossings
# treat_counties_bordercrossing <-
#   imap_dfr(treat_fips_lookup_bordercrossing, function(fips, state) {
#   tibble(state_name = state, COUNTY = as.integer(fips))}) %>% # Pivot treat_fips_lookup to long format
#   left_join(state_code_lookup, by = "state_name") %>%         # Add state fips as a column using state_code_lookup via left_join
#   transmute(                                                  # Create geoid and state to link to df_base, drop all other col (hence transmute & !mutate)
#     STATE = as.integer(STATE),
#     geoid = sprintf("%02d%03d", as.integer(STATE), as.integer(COUNTY)),
#     border = 1L)

# TODO: Turn into function where you can just specify the df to create
# A. Treatment = physical border (defined using shapefiles and international boundary)
treat_counties_physicalborder <-
  imap_dfr(treat_fips_lookup_physicalborder, function(fips, state) {
    tibble(state_name = state, COUNTY = as.integer(fips))}) %>% # Pivot treat_fips_lookup to long format
  left_join(state_code_lookup, by = "state_name") %>%         # Add state fips as a column using state_code_lookup via left_join
  transmute(                                                  # Create geoid and state to link to df_base, drop all other col (hence transmute & !mutate)
    STATE = as.integer(STATE),
    geoid = sprintf("%02d%03d", as.integer(STATE), as.integer(COUNTY)),
    border = 1L)


# B. Treatment = within 50 miles of physical border county (NBER County Distance Database)
treat_counties_within50 <-
  imap_dfr(treat_fips_lookup_within50, function(fips, state) {
    tibble(state_name = state, COUNTY = as.integer(fips))}) %>%
  left_join(state_code_lookup, by = "state_name") %>%
  transmute(
    STATE = as.integer(STATE),
    geoid = sprintf("%02d%03d", as.integer(STATE), as.integer(COUNTY)),
    border = 1L)


# C. Treatment = within 100 miles of physical border county (NBER County Distance Database)
treat_counties_within100 <-
  imap_dfr(treat_fips_lookup_within100, function(fips, state) {
    tibble(state_name = state, COUNTY = as.integer(fips))}) %>%
  left_join(state_code_lookup, by = "state_name") %>%
  transmute(
    STATE = as.integer(STATE),
    geoid = sprintf("%02d%03d", as.integer(STATE), as.integer(COUNTY)),
    border = 1L)

# Change here to vary treatment definition ********
df_base <- df_base %>%
  left_join(treat_counties_physicalborder, by = c("STATE", "geoid")) %>%
  mutate(border = if_else(is.na(border), 0L, border))

# 3) Build pop_2010_2024 and merge with df_base ================================
# File paths
pop_2010_2020_path <- "data/data_population/n_Single-Race Population Estimates 2010-2020.csv"
pop_2020_2024_path <- "data/data_population/n_Single-Race Population Estimates 2020-2024.csv"

# Read both files
pop_2010_2020 <- read_pop_file(pop_2010_2020_path)
pop_2020_2024 <- read_pop_file(pop_2020_2024_path)

# Deduplicate 2020:
# keep 2010-2019 from the first file
# keep 2020-2024 from the second file
pop_2010_2024 <- bind_rows(
  pop_2010_2020 %>% filter(year <= 2019),
  pop_2020_2024 %>% filter(year >= 2020)
) %>%
  arrange(county_code, year)

# Write merged file
write_csv(pop_2010_2024, "data/data_population/Single-Race Population Estimates 2010-2024_deduped.csv")

# Make sure county-id is the same as geoid in df_base so it can be merged
pop_df2 <- pop_2010_2024 %>%
  transmute(
    geoid = str_pad(as.character(county_code), 5, "left", "0"),
    FY = as.integer(year),     # Use July 1st population for FY (FY2019 = Oct2018–Sep2019 uses pop_total from July 1st, 2019)
    pop_total = as.numeric(population)
  )

# Merge population data with df_base (DiD dataset)
df_border_states <- df_base %>%
  left_join(pop_df2, by = c("geoid", "FY"))


# 4a) Main model: Poisson TWFE with offset log(pop_total), no controls ==========
df_border_states <- df_border_states %>%
  mutate(
    geoid = factor(geoid),
    FY     = factor(FY),
    STATE  = factor(STATE)
  )

main <- fepois(
  n_crashes ~ border * post_canada | geoid + FY,
  offset  = ~ log(pop_total),
  cluster = ~ geoid,
  data    = df_border_states
)

etable(main)
summary(main) 
exp(coeftable(main)) # IRR = 1.07
exp(confint(main, vcov = ~geoid)) # 95% CI

# 4b) Main model: control: unemployment rate ================================
laus_dir <- "data/data_employment"
laus_files <- list.files(laus_dir, full.names = TRUE)

# Build stacked LAUS dataset
laus_df <- map_dfr(laus_files, read_laus_file)

# Merge laus_df with df_border_states by `geoid` and `FY`
df_border_states <- df_border_states %>%
  mutate(
    FY = as.integer(as.character(FY)),
    geoid = as.character(geoid)
  ) %>%
  left_join(
    laus_df %>% mutate(FY = as.integer(FY), geoid = as.character(geoid)),
    by = c("geoid", "FY")
)

# Run main model again
df_border_states <- df_border_states %>%
  mutate(
    geoid = factor(geoid),
    FY    = factor(FY),
    STATE = factor(STATE),
    border = as.integer(border),
    post_canada = as.integer(post_canada),
    unemployment_rate = as.numeric(unemployment_rate),
    pop_total = as.numeric(pop_total)
  )

main_control_unemployment <- fepois(
  n_crashes ~ border * post_canada + unemployment_rate | geoid + FY,
  offset  = ~ log(pop_total),
  cluster = ~ geoid,
  data    = df_border_states
)

etable(main_control_unemployment)
summary(main_control_unemployment)
exp(coeftable(main_control_unemployment)) # IRR = 1.05
exp(confint(main_control_unemployment, vcov = ~geoid)) # 95% CI: 

# 4c) Main model: control: unemployment rate & MEDIAN HOUSEHOLD INCOME ======
income_dir <- "data/data_income"
income_files <- list.files(income_dir, full.names = TRUE)

# Build stacked income dataset
income_df <- map_dfr(income_files, read_income_file) 

# Deflate using base 1982-1984 from US BLS (https://data.bls.gov/pdq/SurveyOutputServlet)
cpi_data <- read_excel("data/data_cpi/SeriesReport-20260126144151_e02568.xlsx", skip = 10) %>% 
  clean_names() %>% 
  transmute(
    year = as.integer(year),
    annual_cpi = as.numeric(annual)
  )

# Set 2023 as reference year (e.g. after deflation, real dollars reported in 2023 real dollars)
reference_year_cpi <- cpi_data %>% filter(cpi_data$year == 2023) %>% 
  pull(annual_cpi)

income_df_deflated <- income_df %>%
  left_join(cpi_data, by = c("FY" = "year")) %>% 
  mutate(
    median_household_income_deflated = median_household_income * (reference_year_cpi / annual_cpi)
  ) %>% 
  select(geoid, FY, median_household_income_deflated)


# Merge income_df with df_border_states by `geoid` and `FY`
df_border_states <- df_border_states %>%
  mutate(
    FY = as.integer(as.character(FY)),
    geoid = as.character(geoid)
  ) %>%
  left_join(
    income_df_deflated %>% mutate(FY = as.integer(FY), geoid = as.character(geoid)),
    by = c("geoid", "FY")
  )

# Run main model again
df_border_states <- df_border_states %>%
  mutate(
    geoid = factor(geoid),
    STATE = factor(STATE),
    FY_fe  = factor(FY),                    # for year fixed effects
    FY_num = as.integer(as.character(FY)),  # for linear trend slopes
    border = as.integer(border),
    post_canada = as.integer(post_canada),
    unemployment_rate = as.numeric(unemployment_rate),
    pop_total = as.numeric(pop_total),
    median_household_income_deflated = as.numeric(median_household_income_deflated)
  )

main_control_unemployment_income <- fepois(
  n_crashes ~ border * post_canada + unemployment_rate + median_household_income_deflated | geoid + FY_fe,
  offset  = ~ log(pop_total),
  cluster = ~ geoid,
  data    = df_border_states
)

etable(main_control_unemployment_income)
summary(main_control_unemployment_income)
exp(coefficients(summary(main_control_unemployment_income)))  
exp(confint(main_control_unemployment_income, vcov = ~geoid)) 

# Main model, with state-specific trends
main_control_unemployment_income_statelinear <- fepois(
  n_crashes ~ border * post_canada + unemployment_rate + median_household_income_deflated
  | geoid + FY + STATE[FY_num],
  offset  = ~ log(pop_total),
  cluster = ~ geoid,
  data    = df_border_states
)

etable(main_control_unemployment_income_statelinear)
summary(main_control_unemployment_income_statelinear)
exp(coefficients(summary(main_control_unemployment_income_statelinear)))  # IRR = 1.03
exp(confint(main_control_unemployment_income_statelinear, vcov = ~geoid)) # 95% CI: [0.94, 1.12]

# Main model, with state^year FE
main_control_unemployment_income_stateyearFE <- fepois(
  n_crashes ~ border * post_canada + unemployment_rate + median_household_income_deflated 
  | geoid + STATE^FY_fe,
  offset  = ~ log(pop_total),
  cluster = ~ geoid,
  data    = df_border_states
)

etable(main_control_unemployment_income_stateyearFE)
summary(main_control_unemployment_income_stateyearFE)
exp(coefficients(summary(main_control_unemployment_income_stateyearFE)))  # IRR = 1.03
exp(confint(main_control_unemployment_income_stateyearFE, vcov = ~geoid)) # 95% CI: [0.94, 1.13]
