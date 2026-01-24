library(tidyr)
library(janitor)
library(readxl)

# 0) Read in file & create treated (border) counties lookup & state lookup =====
fatal_crashes_monthly <- read_csv("data/data_processed/fatal_crashes_monthly.csv")   # Note: these border counties are defined as those with actual border crossings & 
                                                                                # contains all fatal crashes across US states and DC
treat_fips_lookup <- list(
  # "alaska" = c(185, 290, 240, 66, 282, 105, 230, 100, 110, 195, 275, 130),
  "idaho" = c(21),
  "maine" = c(7, 25, 3, 29),
  "michigan" = c(147, 33, 163),
  "minnesota" = c(69, 135, 77, 71, 137, 75, 31),
  "montana" = c(53, 29, 35, 101, 51, 41, 5, 71, 105, 19, 91),
  "newhampshire" = c(7),
  "newyork" = c(29, 63, 45, 89, 33, 19),
  "northdakota" = c(23, 13, 75, 9, 79, 95, 19, 67),
  "ohio" = c(143),
  "pennsylvania" = c(49),
  "vermont" = c(11, 13, 9, 19),
  "washington" = c(9, 55, 73, 47, 19, 65, 51),
  "wisconsin" = c(31, 7, 3, 51)
)

state_code_lookup <- tibble::tribble(
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
  "idaho",           16,
  "wisconsin",       55
)

# 1) Build county x FY (Oct–Sep) crash counts from fatal_crashes_monthly =======
#  Note: fatal_crashes_monthly has: STATE, YEAR, MONTH, COUNTY, n_fatal_crashes
df_base <- fatal_crashes_monthly %>%
  filter(STATE %in% state_code_lookup$STATE) %>% # Filter to only 13 states bordering Canada (omitted Alaska)
  mutate(
    # Redefine year to align with Canada's legalization: Oct–Dec => next FY
    FY = if_else(MONTH >= 10, YEAR + 1L, YEAR),
    # Post period: FY2019 is first full FY after Oct2018
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

treat_counties <- 
  imap_dfr(treat_fips_lookup, function(fips, state) {        
  tibble(state_name = state, COUNTY = as.integer(fips))}) %>% # Pivot treat_fips_lookup to long format
  left_join(state_code_lookup, by = "state_name") %>%         # Add state fips as a column using state_code_lookup via left_join
  transmute(                                                  # Create geoid and state to link to df_base, drop all other col (hence transmute & !mutate)
    STATE = as.integer(STATE),
    geoid = sprintf("%02d%03d", as.integer(STATE), as.integer(COUNTY)),
    border = 1L) 

df_base <- df_base %>%
  left_join(treat_counties, by = c("STATE", "geoid")) %>%
  mutate(border = if_else(is.na(border), 0L, border))

# 3) Build pop_2010_2024 and merge with df_base ================================
# File paths
pop_2010_2020_path <- "data/data_population/n_Single-Race Population Estimates 2010-2020.csv"
pop_2020_2024_path <- "data/data_population/n_Single-Race Population Estimates 2020-2024.csv"

# Read + clean function
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


# 4) Main model: Poisson TWFE with offset log(pop_total), no controls ==========
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
summary(main) # IRR = 1.04
exp(confint(main, vcov = ~geoid)) # 95% CI: [0.92, 1.18]

# 5) Run main model, control: unemployment rate ================================
laus_dir <- "data/data_employment"
laus_files <- list.files(laus_dir, full.names = TRUE)

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

# Build stacked LAUS dataset (only your 14 states)
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
summary(main_control_unemployment) # IRR = 1.02
exp(confint(main_control_unemployment, vcov = ~geoid)) # 95% CI: [0.90, 1.16]


