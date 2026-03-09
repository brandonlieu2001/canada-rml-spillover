# Drop 2020

# (0) Create datasets
physicalborder_df <- read_csv("code/descriptive/physicalborder_df.csv") %>%
  mutate(FY = as.integer(as.character(FY)),
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

physicalborder_drop2020_df <- read_csv("code/descriptive/physicalborder_df.csv") %>%
  mutate(FY = as.integer(as.character(FY)),
         FY_fe  = factor(FY),                    # for year fixed effects
         FY_num = as.integer(as.character(FY)),  # for linear trend slopes
         geoid = factor(geoid),
         STATE = factor(STATE),
         border = as.integer(border),
         post_canada = as.integer(post_canada),
         unemployment_rate = as.numeric(unemployment_rate),
         pop_total = as.numeric(pop_total),
         median_household_income_deflated = as.numeric(median_household_income_deflated)
  ) %>% 
  filter(FY != 2020)

# Check 2020 has been dropped
table(physicalborder_drop2020_df$FY)

# (1) Run main analysis ====
# a. county FE and year FE
main_full <- fepois(n_crashes ~ border * post_canada + unemployment_rate + median_household_income_deflated
                    | geoid + FY_fe,  # FE
                    cluster = ~ geoid,
                    offset = ~ log(pop_total),
                    data = physicalborder_df)

etable(main_full)
exp(coefficients(summary(main_full)))  
exp(confint(main_full)) 

# b. county FE, year FE, and state-specific linear trend
main_full_ss <- fepois(n_crashes ~ border * post_canada + unemployment_rate + median_household_income_deflated
                    | geoid + FY + STATE[FY_num],  # FE
                    cluster = ~ geoid,
                    offset = ~ log(pop_total),
                    data = physicalborder_df)

etable(main_full_ss)
exp(coefficients(summary(main_full_ss)))
exp(confint(main_full_ss))



# c. county FE and state^year FE
main_full_strict <- fepois(n_crashes ~ border * post_canada + unemployment_rate + median_household_income_deflated
                       | geoid + STATE^FY_fe,  # FE
                       cluster = ~ geoid,
                       offset = ~ log(pop_total),
                       data = physicalborder_df)

etable(main_full_strict)
exp(coefficients(summary(main_full_strict)))  
exp(confint(main_full_strict)) 

# (2) Run with 2020 dropped ==== 
# a. county FE and year FE
main_full <- fepois(n_crashes ~ border * post_canada + unemployment_rate + median_household_income_deflated
                    | geoid + FY_fe,  # FE
                    cluster = ~ geoid,
                    offset = ~ log(pop_total),
                    data = physicalborder_drop2020_df)

etable(main_full)
exp(coefficients(summary(main_full)))  
exp(confint(main_full)) 

# b. county FE, year FE, and state-specific linear trend
main_full_ss <- fepois(n_crashes ~ border * post_canada + unemployment_rate + median_household_income_deflated
                       | geoid + FY_num + STATE[FY_num],  # FE
                       cluster = ~ geoid,
                       offset = ~ log(pop_total),
                       data = physicalborder_drop2020_df)

etable(main_full_ss)
exp(coefficients(summary(main_full_ss)))
exp(confint(main_full_ss))


# c. county FE and state^year FE
main_full_strict <- fepois(n_crashes ~ border * post_canada + unemployment_rate + median_household_income_deflated
                           | geoid + STATE^FY_fe,  # FE
                           cluster = ~ geoid,
                           offset = ~ log(pop_total),
                           data = physicalborder_drop2020_df)

etable(main_full_strict)
exp(coefficients(summary(main_full_strict)))  
exp(confint(main_full_strict)) 

