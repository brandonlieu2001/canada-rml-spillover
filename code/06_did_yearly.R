source("code/utils.R")
library(patchwork)
library(dplyr)
library(broom)
library(ggplot2)

# 1a. Run DiD on states separately (YEARLY, FY starts Oct) ====================

# ID ========================================================================
idaho_accidents_full_monthly <- read.csv("data_processed/idaho_accident_person_data.csv")

id_yearly <- build_yearly_dataset_oct(idaho_accidents_full_monthly)
id_did <- prepare_did_data_yearly_oct(id_yearly, treat_fips = c(21))

id_plot <- make_did_plot_yearly(
  did_data = id_did,
  title_string = "Yearly (FY Oct-start) Fatal Accident Counts, ID",
  control_label = "Control counties",
  treat_label = "Treatment county"
)
id_plot

id_model <- run_naive_did(id_did)

# WA ========================================================================
wa_monthly <- read.csv("data_processed/washington_accidents_monthly.csv")

wa_yearly <- build_yearly_dataset_oct(wa_monthly)
wa_did <- prepare_did_data_yearly_oct(
  wa_yearly,
  treat_fips = c(9, 55, 73, 47, 19, 65, 51)
)

wa_plot <- make_did_plot_yearly(
  wa_did,
  "Yearly (FY Oct-start) Fatal Accident Counts, Washington",
  "Control counties",
  "Border counties"
)
wa_model <- run_naive_did(wa_did)

# MT ========================================================================
mt_monthly <- read.csv("data_processed/montana_accidents_monthly.csv")

mt_yearly <- build_yearly_dataset_oct(mt_monthly)
mt_did <- prepare_did_data_yearly_oct(
  mt_yearly,
  treat_fips = c(53, 29, 35, 101, 51, 41, 5, 71, 105, 19, 91)
)

mt_plot <- make_did_plot_yearly(
  mt_did,
  "Yearly (FY Oct-start) Fatal Accident Counts, Montana",
  "Control counties",
  "Border counties"
)
mt_model <- run_naive_did(mt_did)

# AL ========================================================================
al_monthly <- read.csv("data_processed/alaska_accidents_monthly.csv")

al_yearly <- build_yearly_dataset_oct(al_monthly)
al_did <- prepare_did_data_yearly_oct(
  al_yearly,
  treat_fips = c(185, 290, 240, 66, 282, 105, 230, 100, 110, 195, 275, 130)
)

al_plot <- make_did_plot_yearly(
  al_did,
  "Yearly (FY Oct-start) Fatal Accident Counts, Alaska",
  "Control counties",
  "Border counties"
)
al_model <- run_naive_did(al_did)

# ND ========================================================================
nd_monthly <- read.csv("data_processed/northdakota_accidents_monthly.csv")

nd_yearly <- build_yearly_dataset_oct(nd_monthly)
nd_did <- prepare_did_data_yearly_oct(
  nd_yearly,
  treat_fips = c(23, 13, 75, 9, 79, 95, 19, 67)
)

nd_plot <- make_did_plot_yearly(
  nd_did,
  "Yearly (FY Oct-start) Fatal Accident Counts, North Dakota",
  "Control counties",
  "Border counties"
)
nd_model <- run_naive_did(nd_did)

# MN ========================================================================
mn_monthly <- read.csv("data_processed/minnesota_accidents_monthly.csv")

mn_yearly <- build_yearly_dataset_oct(mn_monthly)
mn_did <- prepare_did_data_yearly_oct(
  mn_yearly,
  treat_fips = c(69, 135, 77, 71, 137, 75, 31)
)

mn_plot <- make_did_plot_yearly(
  mn_did,
  "Yearly (FY Oct-start) Fatal Accident Counts, Minnesota",
  "Control counties",
  "Border counties"
)
mn_model <- run_naive_did(mn_did)

# WI ========================================================================
wi_monthly <- read.csv("data_processed/wisconsin_accidents_monthly.csv")

wi_yearly <- build_yearly_dataset_oct(wi_monthly)
wi_did <- prepare_did_data_yearly_oct(
  wi_yearly,
  treat_fips = c(31, 7, 3, 51)
)

wi_plot <- make_did_plot_yearly(
  wi_did,
  "Yearly (FY Oct-start) Fatal Accident Counts, Wisconsin",
  "Control counties",
  "Border counties"
)
wi_model <- run_naive_did(wi_did)

# MI ========================================================================
mi_monthly <- read.csv("data_processed/michigan_accidents_monthly.csv")

mi_yearly <- build_yearly_dataset_oct(mi_monthly)
mi_did <- prepare_did_data_yearly_oct(
  mi_yearly,
  treat_fips = c(147, 33, 163)
)

mi_plot <- make_did_plot_yearly(
  mi_did,
  "Yearly (FY Oct-start) Fatal Accident Counts, Michigan",
  "Control counties",
  "Border counties"
)
mi_model <- run_naive_did(mi_did)

# OH ========================================================================
oh_monthly <- read.csv("data_processed/ohio_accidents_monthly.csv")

oh_yearly <- build_yearly_dataset_oct(oh_monthly)
oh_did <- prepare_did_data_yearly_oct(
  oh_yearly,
  treat_fips = c(143)
)

oh_plot <- make_did_plot_yearly(
  oh_did,
  "Yearly (FY Oct-start) Fatal Accident Counts, Ohio",
  "Control counties",
  "Border counties"
)
oh_model <- run_naive_did(oh_did)

# PA ========================================================================
pa_monthly <- read.csv("data_processed/pennsylvania_accidents_monthly.csv")

pa_yearly <- build_yearly_dataset_oct(pa_monthly)
pa_did <- prepare_did_data_yearly_oct(pa_yearly, treat_fips = c(49))

pa_plot <- make_did_plot_yearly(
  pa_did,
  "Yearly (FY Oct-start) Fatal Accident Counts, Pennsylvania",
  "Control counties",
  "Border counties"
)
pa_model <- run_naive_did(pa_did)

# NY ========================================================================
ny_monthly <- read.csv("data_processed/newyork_accidents_monthly.csv")

ny_yearly <- build_yearly_dataset_oct(ny_monthly)
ny_did <- prepare_did_data_yearly_oct(
  ny_yearly,
  treat_fips = c(29, 63, 45, 89, 33, 19)
)

ny_plot <- make_did_plot_yearly(
  ny_did,
  "Yearly (FY Oct-start) Fatal Accident Counts, New York",
  "Control counties",
  "Border counties"
)
ny_model <- run_naive_did(ny_did)

# VT ========================================================================
vt_monthly <- read.csv("data_processed/vermont_accidents_monthly.csv")

vt_yearly <- build_yearly_dataset_oct(vt_monthly)
vt_did <- prepare_did_data_yearly_oct(
  vt_yearly,
  treat_fips = c(11, 13, 9, 19, 13)
)

vt_plot <- make_did_plot_yearly(
  vt_did,
  "Yearly (FY Oct-start) Fatal Accident Counts, Vermont",
  "Control counties",
  "Border counties"
)
vt_model <- run_naive_did(vt_did)

# NH ========================================================================
nh_monthly <- read.csv("data_processed/newhampshire_accidents_monthly.csv")

nh_yearly <- build_yearly_dataset_oct(nh_monthly)
nh_did <- prepare_did_data_yearly_oct(nh_yearly, treat_fips = c(7))

nh_plot <- make_did_plot_yearly(
  nh_did,
  "Yearly (FY Oct-start) Fatal Accident Counts, New Hampshire",
  "Control counties",
  "Border counties"
)
nh_model <- run_naive_did(nh_did)

# ME ========================================================================
me_monthly <- read.csv("data_processed/maine_accidents_monthly.csv")

me_yearly <- build_yearly_dataset_oct(me_monthly)
me_did <- prepare_did_data_yearly_oct(
  me_yearly,
  treat_fips = c(7, 25, 3, 29)
)

me_plot <- make_did_plot_yearly(
  me_did,
  "Yearly (FY Oct-start) Fatal Accident Counts, Maine",
  "Control counties",
  "Border counties"
)
me_model <- run_naive_did(me_did)

# 1b. Summarize individual state DiD & plot ==================================

model_list <- list(
  Alaska        = al_model,
  Washington    = wa_model,
  Idaho         = id_model,
  Montana       = mt_model,
  NorthDakota   = nd_model,
  Minnesota     = mn_model,
  Wisconsin     = wi_model,
  Michigan      = mi_model,
  Ohio          = oh_model,
  Pennsylvania  = pa_model,
  NewYork       = ny_model,
  Vermont       = vt_model,
  NewHampshire  = nh_model,
  Maine         = me_model
)

results_table <- bind_rows(
  lapply(names(model_list), function(state) {
    broom::tidy(model_list[[state]]) %>% mutate(State = state)
  })
) %>%
  filter(term == "TREAT:POST") %>%
  select(State, estimate, std.error, statistic, p.value)

results_table

all_plots <-
  (al_plot      | wa_plot)    /
  (id_plot      | mt_plot)    /
  (nd_plot      | mn_plot)    /
  (wi_plot      | mi_plot)    /
  (oh_plot      | pa_plot)    /
  (ny_plot      | vt_plot)    /
  (nh_plot      | me_plot)

all_plots

ggsave(
  filename = "state_did_graph_yearly_fy_oct.pdf",
  plot = all_plots,
  path = "output/",
  width = 20, height = 35, dpi = 300
)

# 2a. Run regression, pooled across all 14 states ============================

pooled_did <- bind_rows(
  al_did, wa_did, id_did,
  mt_did, nd_did, mn_did,
  wi_did, mi_did, oh_did,
  pa_did, ny_did, vt_did,
  nh_did, me_did,
  .id = "STATE_ID"
)

pooled_did$STATE <-
  factor(pooled_did$STATE_ID, labels = c(
    "Alaska","Washington","Idaho",
    "Montana","North Dakota", "Minnesota",
    "Wisconsin","Michigan","Ohio",
    "Pennsylvania","New York","Vermont",
    "New Hampshire","Maine"
  ))

pooled_plot <- make_did_plot_yearly(
  pooled_did,
  title_string = "Yearly (FY Oct-start) Fatal Accident Counts (14 Border States)",
  control_label = "Control counties",
  treat_label = "Treatment county"
)

pooled_plot

pooled_model <- lm(n_accidents ~ TREAT * POST, data = pooled_did)
print(summary(pooled_model))

# 2b. Run regression, removing VT, MI, PA, WI ===============================

pooled_did_drop4 <- bind_rows(
  al_did, wa_did, id_did,
  mt_did, nd_did, mn_did,
  # wi_did,
  # mi_did,
  oh_did,
  # pa_did,
  ny_did,
  # vt_did,
  nh_did, me_did,
  .id = "STATE_ID"
)

pooled_did_drop4$STATE <-
  factor(pooled_did_drop4$STATE_ID, labels = c(
    "Alaska","Washington","Idaho",
    "Montana","North Dakota", "Minnesota",
    "Ohio",
    "New York",
    "New Hampshire","Maine"
  ))

pooled_plot_drop4 <- make_did_plot_yearly(
  pooled_did_drop4,
  title_string = "Yearly (Oct-start) Fatal Accident Counts (10 Border States)",
  control_label = "Control counties",
  treat_label = "Treatment county"
)

pooled_plot_drop4

pooled_model_drop4 <- lm(n_accidents ~ TREAT * POST, data = pooled_did_drop4)
print(summary(pooled_model_drop4))

# 3. TWFE ======================================================================
# Clustered SEs for to account for clustering by county
twfe_feols <- feols(n_accidents ~ TREAT*POST | COUNTY + FY,
                    data = pooled_did_drop4,
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
