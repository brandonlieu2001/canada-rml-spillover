library(dplyr)
library(readr)
library(sf)
library(tigris)
library(tidyverse)

# (1) MAIN SPEC: Identify sharing actual boundary =================================
options(tigris_use_cache = TRUE)

# 1) Read US international boundary line and keep Canada segments
canada_border <- st_read(
  "data/archive/data_distance/tl_2023_us_internationalboundary/tl_2023_us_internationalboundary.shp",
  quiet = TRUE
) %>%
  filter(IBTYPE == "C") %>%
  st_make_valid() %>%
  st_transform(5070)

# 2) Download ALL counties as polygons (CONUS + AK, etc.)
# cb = FALSE keeps more accurate boundaries
counties_poly <- counties(cb = FALSE, year = 2020, class = "sf") %>%
  st_make_valid() %>%
  st_transform(5070)

# 3) Treated counties = counties whose polygon intersects the Canada boundary line
# `hits` is a list with length(counties_poly) with values of indices of canada_border that it intersects
hits <- st_intersects(counties_poly, canada_border, sparse = TRUE) 

counties_poly <- counties_poly %>%
  mutate(border = as.integer(lengths(hits) > 0))

# 4) treatfips = GEOIDs for border-touching counties
treatfips <- counties_poly %>%
  filter(border == 1) %>%
  pull(GEOID)

# Optional: quick check of which states get flagged
border_states <- counties_poly %>%
  filter(border == 1) %>%
  st_drop_geometry() %>%
  distinct(STATEFP) %>%
  arrange(STATEFP)

print(border_states)

# Add states name to counties_poly using tigris::fips_codes
states_xwalk <- fips_codes %>%
  distinct(state_code, state_name, state) %>%
  transmute(
    STATEFP   = state_code,   # numeric FIPS as character
    state_nm = tolower(state_name)     # full name
  )

counties_poly <- counties_poly %>%
  left_join(states_xwalk, by = "STATEFP") 

# Create list of treated counties for 07_did_poisson_yearly.R
treat_fips_lookup_physicalborder <- 
  counties_poly %>% 
  st_drop_geometry() %>% 
  transmute(
    state_nm   = tolower(gsub("\\s+", "", state_nm)),  # normalize state name
    countyfips = as.integer(COUNTYFP),
    border     = border
  ) %>% 
  filter(border == 1) %>% 
  distinct(state_nm, countyfips) %>%                  # guard against duplicates
  group_by(state_nm) %>% 
  summarise(countyfips = list(countyfips), .groups = "drop") %>% 
  deframe()

saveRDS(
  treat_fips_lookup_physicalborder,
  "data/data_treatment/treat_fips_lookup_physicalborder.rds"
)

# (2) LOOSE MAIN SPEC: Identify within 50 and 100 miles of sharing actual boundary ====
nber_dist <- read_csv("archive/data/data_distance/data_countydistance_nber/sf12010countydistance100miles.csv")

# 0) Standardize FIPS to 5-digit character strings ----

treatfips <- sprintf("%05d", as.integer(treatfips))

nber_dist_std <- nber_dist %>%
  transmute(
    county1 = sprintf("%05d", as.integer(county1)),
    county2 = sprintf("%05d", as.integer(county2)),
    dist_mi = as.numeric(mi_to_county)
  )

# 1) Minimum distance (miles) from each county to ANY treated county
nber_dist_std <- nber_dist_std %>%
  transmute(
    county1 = county1,
    county2 = county2,
    dist_mi = as.numeric(dist_mi)
  )

# Build min distance to treated for county1 using BOTH orientations

# 1) distances for NON-treated counties that have an edge to any treated county
min_to_treat_untreated <- nber_dist_std %>%
  mutate(
    c1_is_treat = county1 %in% treatfips,
    c2_is_treat = county2 %in% treatfips
  ) %>%
  filter(xor(c1_is_treat, c2_is_treat)) %>%   # ensures exactly one treated (drops treated-treated), no treated rows end up in `county`
  transmute(
    county = if_else(c2_is_treat, county1, county2),  # `county` is col of ALL untreated rows and their distance to any treated row. select the one with minimum distance to treated row for inclusion.
    dist_mi = dist_mi
  ) %>%
  group_by(county) %>%
  summarise(dist_to_treat_mi = min(dist_mi, na.rm = TRUE), .groups = "drop")

# 2) explicitly add treated counties with distance 0; 
# catches edge case: if treated county never appears as an untreated:treated pair, it won't appear in `min_to_treat_untreated` (happens to 23029 and 23003)
min_to_treat_treated <- tibble(
  county = treatfips,
  dist_to_treat_mi = 0
)

# 3) combine + flags
min_to_treat <- bind_rows(min_to_treat_untreated, min_to_treat_treated) %>%
  distinct(county, .keep_all = TRUE) %>%  # safety if any treated slipped in anyway
  mutate(
    treated   = as.integer(county %in% treatfips),
    within50  = as.integer(dist_to_treat_mi <= 50),
    within100 = as.integer(dist_to_treat_mi <= 100)
  )

# Create county_xwalk to link state_nm back to add state_nm to be used in analysis
county_xwalk <- counties_poly %>%
  st_drop_geometry() %>%
  transmute(
    geoid      = sprintf("%05d", as.integer(GEOID)),
    statefp    = sprintf("%02d", as.integer(STATEFP)),
    countyfips = as.integer(COUNTYFP),
    state_nm   = tolower(gsub("\\s+", "", state_nm))  # same normalization as used in main analysis file 07 (remove space and lowercase)
  )

make_state_list <- function(flag_col) {
  min_to_treat %>%
    filter(.data[[flag_col]] == 1) %>%
    transmute(geoid = county) %>%
    left_join(county_xwalk, by = "geoid") %>%
    distinct(state_nm, countyfips) %>%
    group_by(state_nm) %>%
    summarise(countyfips = list(countyfips), .groups = "drop") %>%
    deframe()
}

treat_fips_lookup_within50 <- make_state_list("within50")
treat_fips_lookup_within100<- make_state_list("within100")

# Filter out additional states that were added to keep comparability in panel
analysis_states <- names(treat_fips_lookup_physicalborder)
treat_fips_lookup_within100 <-
  treat_fips_lookup_within100[names(treat_fips_lookup_within100) %in% analysis_states]

# Write out to RDS
saveRDS(
  treat_fips_lookup_within50,
  "data/data_treatment/treat_fips_lookup_within50.rds"
)

saveRDS(
  treat_fips_lookup_within100,
  "data/data_treatment/treat_fips_lookup_within100.rds"
)
