source("code/utils.R")
library(patchwork)
library(dplyr)
library(broom)
library(ggplot2)
library(fixest)

# 0. Create lookup table for which counties are treated ======================
treat_fips_lookup <- list(
  "alaska" = c(185, 290, 240, 66, 282, 105, 230, 100, 110, 195, 275, 130),
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
  "vermont" = c(11, 13, 9, 19, 13),
  "washington" = c(9, 55, 73, 47, 19, 65, 51),
  "wisconsin" = c(31, 7, 3, 51)
)

# 1 Run DiD on states separately (YEARLY, FY starts Oct) ====================
# 1a. Read in monthly files
processed_files <- paste0("data_processed/age_filtered_accidents/", (list.files("data_processed/age_filtered_accidents")))
state_names <- sub("_.*$", "", basename(processed_files))
states_monthly <- setNames(lapply(processed_files, read.csv), state_names)

# 1b. Mutate to yearly
states_yearly <- lapply(states_monthly, build_yearly_dataset_oct)

# 1c. Build DiD dataset
states_did <- imap(states_yearly, function(state_df, state_name) {
  prepare_did_data_yearly_oct(state_df, treat_fips = treat_fips_lookup[[state_name]])
})

# 1d. Build DiD plots
states_plot <- imap(states_did, function(state_did, state_name) {
  make_did_plot_yearly(state_did, 
                       title_string = paste("Yearly Fatal Accident Counts,", state_name))
  })

# 1e. Run naive DiD analysis, OLS on counts
states_model <- lapply(states_did, run_naive_did)

results_table <- bind_rows(
  imap(states_model, function(state_model, state_name) {
    tidy(state_model) %>% mutate(State = state_name) %>% filter(term == "TREAT:POST")
  }))

all_plots <-
  (states_plot[["alaska"]]       |  states_plot[["idaho"]])     /
  (states_plot[["maine"]]        |  states_plot[["michigan"]])  /
  (states_plot[["minnesota"]]    |  states_plot[["montana"]])   /
  (states_plot[["newhampshire"]] |  states_plot[["newyork"]])   /
  (states_plot[["northdakota"]]  |  states_plot[["ohio"]])      /
  (states_plot[["pennsylvania"]] |  states_plot[["vermont"]])   /
  (states_plot[["washington"]]   |  states_plot[["wisconsin"]]) +
  plot_layout(guides = 'collect')

  
all_plots

ggsave(
  filename = "all_states_did_graph_yearly.pdf",
  plot = all_plots,
  path = "output/",
  width = 15, height = 20, dpi = 300
)

# 2a. Run regression, pooled across all 14 states ============================

pooled_did <- bind_rows(states_did, .id = "state_name")

pooled_plot <- make_did_plot_yearly(
  pooled_did,
  title_string = "Yearly Fatal Accident Counts (14 Border States)"
)

pooled_plot

pooled_model <- lm(n_accidents ~ TREAT * POST, data = pooled_did)
print(summary(pooled_model))

# 2b. Run regression, removing VT, MI, PA, WI ===============================

pooled_did_drop4 <- pooled_did %>% 
  filter(!state_name %in% c("vermont", "michigan", "pennsylvania", "wisconsin"))

pooled_plot_drop4 <- make_did_plot_yearly(
  pooled_did_drop4,
  title_string = "Yearly Fatal Accident Counts (10 Border States)"
)

pooled_plot_drop4

pooled_model_drop4 <- lm(n_accidents ~ TREAT * POST, data = pooled_did_drop4)
print(summary(pooled_model_drop4))

# 3. TWFE ======================================================================
# Clustered SEs for to account for clustering by county
twfe_feols <- feols(n_accidents ~ TREAT*POST | COUNTY + FY,
                    data = pooled_did,
                    vcov = ~ COUNTY)   # cluster by county

etable(twfe_feols)

# 4. TWFE with event study =====================================================
df_es <- pooled_did_drop4 %>%
  mutate(
    g = if_else(TREAT == 1, 2019L, 0L)  # 2019 for treated, 0 for never-treated
  )

# Sanity check — this MUST show both groups
table(df_es$TREAT)
table(df_es$g)  # Also check g distribution

twfe_es <- feols(
  n_accidents ~ sunab(g, FY, ref.p = -1) | COUNTY + FY,
  data = df_es,
  vcov = ~ COUNTY
)

# Check the summary
summary(twfe_es)

iplot(
  twfe_es,
  xlim = c(-9, 5),
  ref.line = -1,
  xlab = "Event time (2019; 10/2018 to 9/2019)",
  ylab = "Effect on n_accidents",
  main = "TWFE Event-study, 10 states"
)
