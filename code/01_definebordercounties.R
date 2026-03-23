## 00_process_fars_data.R ----
## 
## Create three treatment definitions for main analysis 
## 1) physicalborder, 2) within 50 miles, 3) within 100 miles

## Import packages ----
library(dplyr)
library(readr)
library(sf)
library(tigris)
library(tidyverse)

## (1) Main specification treatment counties: ## Identify county polygons that physically intersect Canada
### a) Read US international boundary line and keep Canada segments ----
options(tigris_use_cache = TRUE)
canada_border <- st_read(
  "data_raw/tl_2023_us_internationalboundary/tl_2023_us_internationalboundary.shp") %>%
  filter(IBTYPE == "C") %>%
  st_make_valid() %>%
  st_transform(5070)

### b) Download all US counties as polygons (CONUS + AK, etc.) ----
counties_poly <- counties(cb = FALSE, year = 2020, class = "sf") %>% # cb = FALSE keeps more accurate boundaries
  st_make_valid() %>%
  st_transform(5070)

### c) Treated counties = counties whose polygon intersects the Canada boundary line ----
# `hits` is a list with length(counties_poly) with where each index corresponds to counties_poly index
# and is 1 if it intersects the canada_border and 0 if not
hits <- st_intersects(counties_poly, canada_border, sparse = TRUE) 

counties_poly <- counties_poly %>%
  mutate(border = as.integer(lengths(hits) > 0)) # create new column that is boolean, where 1 = intersected, 0 = not intersected

### d) Identify which fips are hits and store as treatfips (treatfips = GEOIDs for border-touching counties) ----
treatfips <- counties_poly %>%
  filter(border == 1) %>%
  pull(GEOID)

# Check which STATES get flagged
border_states <- counties_poly %>%
  filter(border == 1) %>%
  st_drop_geometry() %>%
  distinct(STATEFP)

### e) Add states name to counties_poly using tigris::fips_codes by creating crosswalk then merging with left_join() ----
states_xwalk <- fips_codes %>%
  distinct(state_code, state_name, state) %>%
  transmute(
    STATEFP   = state_code,            # col to join by: numeric FIPS
    state_nm = tolower(state_name)     # new col: full name
  )

counties_poly <- counties_poly %>%
  left_join(states_xwalk, by = "STATEFP") 

### f) Create LIST of treated counties for 07_did_poisson_yearly.R and save as .RDS file ----
treat_fips_lookup_physicalborder <- 
  counties_poly %>% 
  st_drop_geometry() %>% 
  transmute(
    state_nm   = tolower(gsub("\\s+", "", state_nm)),  # normalize state name (remove space, regex denotes \s as space and + to indicate one or more)
    countyfips = as.integer(COUNTYFP),
    border     = border
  ) %>% 
  filter(border == 1) %>% 
  distinct(state_nm, countyfips) %>%                  # guard against duplicates
  group_by(state_nm) %>% 
  summarise(countyfips = list(countyfips)) %>%  
  deframe()

# Count number of treated counties
n_treated <- treat_fips_lookup_physicalborder %>% unlist() %>% length() # 77 treated counties

saveRDS(
  treat_fips_lookup_physicalborder,
  "data/treat_fips_lookup_physicalborder.rds"
)


## (2) Looser specification treatment counties:Identify within 50 and 100 miles of sharing actual boundary ----
### (a) Read in NEBR County Distance Database files ### (https://www.nber.org/research/data/county-distance-database) ----
nber_dist <- read_csv("data_raw/sf12010countydistance100miles.csv")

### (b) Standardize FIPS stored in `treatfips`, list of countyfips, to 5-digit ----
### character strings.
treatfips <- sprintf("%05d", as.integer(treatfips))

### (c)  Minimum distance (miles) from each county to ANY treated county ----
### Standardize countyfips to 0-padded integers and dist_mi to numeric
nber_dist_std <- nber_dist %>%
  transmute(
    county1 = sprintf("%05d", as.integer(county1)),
    county2 = sprintf("%05d", as.integer(county2)),
    dist_mi = as.numeric(mi_to_county)
  )

### (d) Filter for distances of NON-treated counties to any treated county ----
min_to_treat_untreated <- nber_dist_std %>%
  mutate(
    c1_is_treat = county1 %in% treatfips,
    c2_is_treat = county2 %in% treatfips
  ) %>%
  filter(xor(c1_is_treat, c2_is_treat)) %>%   # ensures exactly one treated (drops treated-treated), no treated rows end up in `county`
  transmute(
    county = if_else(c2_is_treat, county1, county2),  # `county` is col of ALL untreated rows and their distance to any treated row.
    dist_mi = dist_mi
  ) %>%
  group_by(county) %>%
  summarise(dist_to_treat_mi = min(dist_mi, na.rm = TRUE), .groups = "drop") #  select the one with minimum distance to treated row for inclusion.

### (e) Explicitly add treated counties, with distance as 0 miles; ----
### Catches edge case: if treated county never appears as an untreated-treated pair, it won't appear in `min_to_treat_untreated` (happens to 23029 and 23003)
min_to_treat_treated <- tibble(
  county = treatfips,
  dist_to_treat_mi = 0
)

### (f) Combine min_to_treat_treated with min_to_treat_treated ----
min_to_treat <- bind_rows(min_to_treat_untreated, min_to_treat_treated) %>%
  distinct(county, .keep_all = TRUE) %>%  # safety if any treated slipped in anyway
  mutate(
    treated   = as.integer(county %in% treatfips),
    within50  = as.integer(dist_to_treat_mi <= 50),
    within100 = as.integer(dist_to_treat_mi <= 100)
  )

### (g) Create county_xwalk to link state_nm back to min_to_treat ----
county_xwalk <- counties_poly %>%
  st_drop_geometry() %>%
  transmute(
    geoid      = sprintf("%05d", as.integer(GEOID)),
    statefp    = sprintf("%02d", as.integer(STATEFP)),
    countyfips = as.integer(COUNTYFP),
    state_nm   = tolower(gsub("\\s+", "", state_nm))  # same normalization as used in main analysis file 07 (remove space and lowercase)
  )

### (h) Create function to filter min_to_treat to identify countyfips that are within 50mi and 100mi boundaries ----
make_state_list <- function(flag_col) {
  min_to_treat %>%
    filter(.data[[flag_col]] == 1) %>%         # Filter for within 50miles or within 100miles
    transmute(geoid = county) %>%             # Drop all cols but the county FIPS
    left_join(county_xwalk, by = "geoid") %>%  # Add the state name back by merging with `county_xwalk`
    distinct(state_nm, countyfips) %>%        # Select only distinct combinations state_nm & countyfips
    group_by(state_nm) %>%                     
    summarise(countyfips = list(countyfips), .groups = "drop") %>%
    deframe()
}

treat_fips_lookup_within50 <- make_state_list("within50")
treat_fips_lookup_within100<- make_state_list("within100")

### (j) Filter out additional states that were added to keep comparability in panel ----
analysis_states <- names(treat_fips_lookup_physicalborder)

treat_fips_lookup_within50 <-
  treat_fips_lookup_within50[names(treat_fips_lookup_within50) %in% analysis_states]
treat_fips_lookup_within100 <-
  treat_fips_lookup_within100[names(treat_fips_lookup_within100) %in% analysis_states]

### (k) Write out to RDS ----
saveRDS(
  treat_fips_lookup_within50,
  "data/treat_fips_lookup_within50.rds"
)

saveRDS(
  treat_fips_lookup_within100,
  "data/treat_fips_lookup_within100.rds"
)
