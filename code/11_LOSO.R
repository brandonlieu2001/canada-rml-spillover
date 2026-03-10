# leave on subject out (loso)

library(tidycensus)
library(tidyverse)
library(broom)
library(fixest)

# (0) Create datasets
physicalborder_df <- read_csv("code/descriptive/physicalborder_df.csv") %>%
  mutate(FY = as.integer(as.character(FY)),
         FY_fe  = factor(FY),                    # for year fixed effects
         FY_num = as.integer(as.character(FY)),  # for linear trend slopes
         geoid = factor(geoid),
         STATE = as.character(STATE),
         border = as.integer(border),
         post_canada = as.integer(post_canada),
         unemployment_rate = as.numeric(unemployment_rate),
         pop_total = as.numeric(pop_total),
         median_household_income_deflated = as.numeric(median_household_income_deflated)
  )

state_lookup <- fips_codes %>%
  select(state_code, state_name) %>%
  distinct()

physicalborder_df_statename <- physicalborder_df %>% 
  left_join(state_lookup, by = c("STATE" = "state_code"))

# full ==== 
main_full <- fepois(n_crashes ~ border:post_canada + unemployment_rate + median_household_income_deflated
                    | geoid + FY_fe,  # FE
                    cluster = ~ geoid,
                    offset = ~ log(pop_total),
                    data = physicalborder_df)

etable(main_full)
exp(coefficients(summary(main_full)))  
exp(confint(main_full))

# state by state ====

# 1) Define the model
fit_state_model <- function(df) {
  fepois(
    n_crashes ~ border * post_canada + unemployment_rate + median_household_income_deflated |
      geoid + FY,
    offset  = ~ log(pop_total),
    cluster = ~ geoid,
    data    = df
  )
}

# 2) Run it for each state (STATE is state FIPS in your data)
models_by_state <- physicalborder_df_statename %>%
  filter(!is.na(state_name)) %>%
  group_split(state_name) %>% # split each state into its own df
  set_names(map_chr(., ~ unique(.x$state_name))) %>% # rename them to their value in df$STATE
  map(~ fit_state_model(.x))

# 3) Extract the coefficient y (border:post_canada)
state_results <- imap_dfr(models_by_state, ~{
  ct <- coeftable(.x)
  tibble(
    STATE = .y,
    term  = "border:post_canada",
    estimate = ct["border:post_canada","Estimate"],
    se       = ct["border:post_canada","Std. Error"],
    p_value  = ct["border:post_canada","Pr(>|z|)"]
  )
})

# add IR and CI
state_results <- state_results %>% arrange(p_value) %>% mutate(irr = exp(estimate),
                                              ci_low = exp(estimate - 1.96*se),
                                              ci_high = exp(estimate + 1.96*se))

### loso
states <- sort(unique(physicalborder_df_statename$state_name)) # vector of state names

models_loso <- set_names(states) %>% # set the names so that each index of models_loso will be named after the state
  map(function(s) { 
    df_drop <- physicalborder_df_statename %>% filter(state_name != s) # filter out that and re run
    fit_state_model(df_drop)
  })

# imap allows .x (list elem) and .y (name of list elem), allowing ops to be done
loso_results <- imap_dfr(models_loso, ~{
  ct <- coeftable(.x)
  
  tibble(
    state_dropped = .y,
    estimate = ct["border:post_canada","Estimate"],
    se       = ct["border:post_canada","Std. Error"],
    p_value  = ct["border:post_canada","Pr(>|z|)"]
  )
}) %>%
  mutate(
    irr     = exp(estimate),
    ci_low  = exp(estimate - 1.96*se),
    ci_high = exp(estimate + 1.96*se)
  )

full <- tidy(main_full) %>%
  filter(term == "border:post_canada") %>%
  mutate(irr = exp(estimate))

full_irr <- full$irr

ggplot(loso_results, aes(x = irr, y = reorder(state_dropped, irr))) +
  geom_point(size = 1.5) +
  geom_errorbarh(aes(xmin = ci_low, xmax = ci_high), height = 0.2, width = 0.2) +
  geom_vline(xintercept = full_irr, linetype = "dashed", linewidth = 0.5) +
  geom_vline(xintercept = 1, linetype = "dotted") +
  annotate("text", x = full_irr, y = Inf, 
           label = paste0("full model: ", round(full_irr, 2)),
           vjust = 1, hjust = -0.1, size = 3) +
  labs(
    x = "IRR (95% CI)",
    y = "state dropped",
  ) +
  theme_classic() +
  theme(
    axis.title.y = element_text(margin = margin(r = 20)),
    axis.text  = element_text(color = "black", size = 8)
  )


