source("code/utils.R")
library(fixest)
library(patchwork)

# 1a. Run DiD on states separately ==================================
# ID ========================================================================

# Import processed data
idaho_accidents_full_monthly <- 
  read.csv("data_processed/idaho_accidents_monthly.csv")

# Build quarterly dataset
id_quarterly <- build_quarterly_dataset(idaho_accidents_full_monthly)

# Prepare DiD dataset
id_did <- prepare_did_data(id_quarterly, treat_fips = c(21))

# Build and show plot
id_plot <- make_did_plot(
  did_data = id_did,
  title_string = "Quarterly Fatal Accident Counts, ID",
  control_label = "Control counties",
  treat_label = "Treatment county"
)

id_plot

# Run naive DiD regression
id_model <- run_naive_did(id_did)

# WA ========================================================================
wa_monthly <- read.csv("data_processed/washington_accidents_monthly.csv")
wa_quarterly <- build_quarterly_dataset(wa_monthly)
wa_did <- prepare_did_data(
  wa_quarterly,
  treat_fips = c(09, # Clallam (water)
                 55, # San Juan (water)
                 73, # Whatcom
                 47, # Okanogan
                 19, # Ferry
                 65, # Stevens
                 51  # Pend Oreille
  )
)
wa_plot <- make_did_plot(
  wa_did,
  "Quarterly Fatal Accident Counts, Washington",
  "Control counties",
  "Border counties"
)
wa_model <- run_naive_did(wa_did)

# MT ========================================================================
mt_monthly <- read.csv("data_processed/montana_accidents_monthly.csv")
mt_quarterly <- build_quarterly_dataset(mt_monthly)
mt_did <- prepare_did_data(
  mt_quarterly,
  treat_fips = c(53, # Lincoln
                 29, # Flathead
                 35, # Glacier
                 101, # Toole
                 51, # Liberty
                 41, # Hill
                 05, # Blaine
                 71, # Phillips
                 105, # Valley
                 19, # Daniels
                 91 # Sheridan
  )
)
mt_plot <- make_did_plot(
  mt_did,
  "Quarterly Fatal Accident Counts, Montana",
  "Control counties",
  "Border counties"
)
mt_model <- run_naive_did(mt_did)

# AL ========================================================================
al_monthly <- read.csv("data_processed/alaska_accidents_monthly.csv")
al_quarterly <- build_quarterly_dataset(al_monthly)
al_did <- prepare_did_data(
  al_quarterly,
  treat_fips = c(185, # North Slope Borough
                 290, # Yukon-Koyukuk Census
                 240, # Southeast Fairbanks Census
                 66,  # Copper River
                 282, # Yakutat
                 105, # Hoonah-Angoon
                 230, # Skagway
                 100, # Haines
                 110, # Juneau
                 195, # Petersburg
                 275, # Wrangell
                 130 # Ketchikan-Gateway
  )
)
al_plot <- make_did_plot(
  al_did,
  "Quarterly Fatal Accident Counts, Alaska",
  "Control counties",
  "Border counties"
)
al_model <- run_naive_did(al_did)

# ND ========================================================================
nd_monthly <- read.csv("data_processed/northdakota_accidents_monthly.csv")
nd_quarterly <- build_quarterly_dataset(nd_monthly)
nd_did <- prepare_did_data(
  nd_quarterly,
  treat_fips = c(23, # Divide
                 13, # Burke
                 75, # Renville
                 09, # Bottineau
                 79, # Rolette
                 95, # Towner
                 19, # Cavalier
                 67 # Pembina
                 )
)
nd_plot <- make_did_plot(
  nd_did,
  "Quarterly Fatal Accident Counts, North Dakota",
  "Control counties",
  "Border counties"
)
nd_model <- run_naive_did(nd_did)

# MN ========================================================================
mn_monthly <- read.csv("data_processed/minnesota_accidents_monthly.csv")
mn_quarterly <- build_quarterly_dataset(mn_monthly)
mn_did <- prepare_did_data(
  mn_quarterly,
  treat_fips = c(69,  # Kittson
                 135, # Roseau
                 77, # Lake of the Woods
                 71, # Koochiching
                 137, # St Louis
                 75, # Lake
                 31 # Cook
                 )
)
mn_plot <- make_did_plot(
  mn_did,
  "Quarterly Fatal Accident Counts, Minnesota",
  "Control counties",
  "Border counties"
)
mn_model <- run_naive_did(mn_did)

# WI ========================================================================
wi_monthly <- read.csv("data_processed/wisconsin_accidents_monthly.csv")
wi_quarterly <- build_quarterly_dataset(wi_monthly)
wi_did <- prepare_did_data(
  wi_quarterly,
  treat_fips = c(31, # Douglas
                 07, # Bayfield
                 03, # Ashland
                 51 # Iron
  )
)
wi_plot <- make_did_plot(
  wi_did,
  "Quarterly Fatal Accident Counts, Wisconsin",
  "Control counties",
  "Border counties"
)
wi_model <- run_naive_did(wi_did)

# MI ========================================================================

mi_monthly <- read.csv("data_processed/michigan_accidents_monthly.csv")
mi_quarterly <- build_quarterly_dataset(mi_monthly)
mi_did <- prepare_did_data(
  mi_quarterly,
  treat_fips = c(147, # St. Clair (water ferry), Port Huron (land)
                 33,  # Chippewa 
                 163 # Wayne
  )    
)
mi_plot <- make_did_plot(
  mi_did,
  "Quarterly Fatal Accident Counts, Michigan",
  "Control counties",
  "Border counties"
)
mi_model <- run_naive_did(mi_did)

# OH ========================================================================
oh_monthly <- read.csv("data_processed/ohio_accidents_monthly.csv")
oh_quarterly <- build_quarterly_dataset(oh_monthly)
oh_did <- prepare_did_data(
  oh_quarterly,
  treat_fips = c(143) # Sandusky (ferry)
)
oh_plot <- make_did_plot(
  oh_did,
  "Quarterly Fatal Accident Counts, Ohio",
  "Control counties",
  "Border counties"
)
oh_model <- run_naive_did(oh_did)

# PA =======================================================================
pa_monthly <- read.csv("data_processed/pennsylvania_accidents_monthly.csv")
pa_quarterly <- build_quarterly_dataset(pa_monthly)
pa_did <- prepare_did_data(pa_quarterly, treat_fips = c(49)) # Erie County, but no actual crossing here (should drop for causal identificaiton)
pa_plot <- make_did_plot(
  pa_did,
  "Quarterly Fatal Accident Counts, Pennsylvania",
  "Control counties",
  "Border counties"
)
pa_model <- run_naive_did(pa_did)

# NY =======================================================================
ny_monthly <- read.csv("data_processed/newyork_accidents_monthly.csv")
ny_quarterly <- build_quarterly_dataset(ny_monthly)
ny_did <- prepare_did_data(
  ny_quarterly,
  treat_fips = c(29, # Erie (land, Ontario)
                 63, # Niagara (land, Ontario)
                 45, # Jefferson (land, Ontario)
                 89, # St Lawrence (land, Ontario)
                 33, # Franklin (land, Quebec)
                 19 # Clinton (land, Quebec)
                 
  )
)
ny_plot <- make_did_plot(
  ny_did,
  "Quarterly Fatal Accident Counts, New York",
  "Control counties",
  "Border counties"
)
ny_model <- run_naive_did(ny_did)

# VT =======================================================================
vt_monthly <- read.csv("data_processed/vermont_accidents_monthly.csv")
vt_quarterly <- build_quarterly_dataset(vt_monthly)
vt_did <- prepare_did_data(
  vt_quarterly,
  treat_fips = c(11, # Franklin
                 13, # Grand Isle
                 9,  # Essex
                 19,  # Orleans
                 13   # Grand Isle (by rail)
                 )
                  
)
vt_plot <- make_did_plot(
  vt_did,
  "Quarterly Fatal Accident Counts, Vermont",
  "Control counties",
  "Border counties"
)
vt_model <- run_naive_did(vt_did)

# NH =======================================================================
nh_monthly <- read.csv("data_processed/newhampshire_accidents_monthly.csv")
nh_quarterly <- build_quarterly_dataset(nh_monthly)
nh_did <- prepare_did_data(nh_quarterly, treat_fips = c(07)) # Coos (land)
nh_plot <- make_did_plot(
  nh_did,
  "Quarterly Fatal Accident Counts, New Hampshire",
  "Control counties",
  "Border counties"
)
nh_model <- run_naive_did(nh_did)

# ME =======================================================================
me_monthly <- read.csv("data_processed/maine_accidents_monthly.csv")
me_quarterly <- build_quarterly_dataset(me_monthly)
me_did <- prepare_did_data(
  me_quarterly,
  treat_fips = c(7, # Franklin
                 25, # Somerset (commercial, logging use this route)
                 03, # Aroostook (commerical, loggers use this route)
                 29 # Washington (land)
  )
)

me_plot <- make_did_plot(
  me_did,
  "Quarterly Fatal Accident Counts, Maine",
  "Control counties",
  "Border counties"
)
me_model <- run_naive_did(me_did)


# 1b. Summarize individual state DiD & plot  ==================================
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
    broom::tidy(model_list[[state]]) %>% 
      mutate(State = state)
  })
)

# Reorder columns, only retrieve treat:post term
results_table <- results_table %>% 
  filter(term == "TREAT:POST") %>% 
  select(State, estimate, std.error, statistic, p.value)
  
results_table

# Combine all plots
all_plots <-
  (al_plot      | wa_plot)    /
  (id_plot      | mt_plot)    /
  (nd_plot      | mn_plot)    /
  (wi_plot      | mi_plot)    /
  (oh_plot      | pa_plot)    /
  (ny_plot      | vt_plot)    /
  (nh_plot      | me_plot)

all_plots
ggsave(filename = "state_did_graph.pdf", plot = all_plots, path = "output/",
       width = 20, height = 35, dpi = 300)



# 2a. Run regression, pooled across all 14 states =============================
pooled_did <- bind_rows(
  al_did, wa_did, id_did,
  mt_did, nd_did, mn_did,
  wi_did, mi_did, oh_did,
  pa_did, ny_did, vt_did,
  nh_did, me_did,
  .id = "STATE_ID"
)

# Must be in same order as the stacking of datasets in bind_rows() above
pooled_did$STATE <- 
  factor(pooled_did$STATE_ID,labels = c(
                             "Alaska","Washington","Idaho",
                             "Montana","North Dakota", "Minnesota",
                             "Wisconsin","Michigan","Ohio",
                             "Pennsylvania","New York","Vermont",
                             "New Hampshire","Maine"
                           )
)


# Plot pooled regression across 14 states
pooled_plot<- make_did_plot(pooled_did,
                            title_string = "Quarterly Fatal Accident Counts (14 Border States)",
                            control_label = "Control counties",
                            treat_label = "Treatment county")

# Run DiD regression
pooled_model <- lm(
  n_accidents ~ TREAT * POST,
  data = pooled_did
)
print(summary(pooled_model))



# 2b. Run regression, removing VT, MI, PA, WI =============================
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

# Must be in same order as the stacking of datasets in bind_rows() above
pooled_did_drop4$STATE <- 
  factor(pooled_did_drop4$STATE_ID,labels = c(
    "Alaska","Washington","Idaho",
    "Montana","North Dakota", "Minnesota",
    # "Wisconsin",
    # "Michigan",
    "Ohio",
    # "Pennsylvania",
    "New York",
    # "Vermont",
    "New Hampshire","Maine"
  )
  )


# Plot pooled regression across 14 states
pooled_plot_drop4<- make_did_plot(pooled_did_drop4,
                            title_string = "Quarterly Fatal Accident Counts (10 Border States)",
                            control_label = "Control counties",
                            treat_label = "Treatment county")

# Run DiD regression
pooled_model_drop4 <- lm(
  n_accidents ~ TREAT * POST,
  data = pooled_did_drop4
)
print(summary(pooled_model_drop4))

# 3. TWFE ==========================================================
pooled_did_drop4 <- pooled_did_drop4 %>%
  mutate(TIME = interaction(YEAR, QUARTER, drop = TRUE))  # e.g., "2018.4"


# 4. TWFE with event study =====================================================
# Clustered SEs 
twfe_feols <- feols(n_accidents ~ TREAT*POST | COUNTY + TIME,
                    data = pooled_did_drop4,
                    vcov = ~ COUNTY)   # cluster by county

etable(twfe_feols)
