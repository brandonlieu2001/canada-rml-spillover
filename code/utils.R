# 01_process_fars_data.R
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