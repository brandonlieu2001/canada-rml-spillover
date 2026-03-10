library(dplyr)
library(readr)
library(ggplot2)
library(patchwork)
library(fixest)

create_es_plot_df <- function(es_model) {
  es_coefs <- coeftable(es_model)
  es_rows <- grepl("^time_to_treat::", rownames(es_coefs))
  es_data <- es_coefs[es_rows, ]
  time_periods <- as.numeric(gsub("time_to_treat::([-0-9]+):border", "\\1", 
                                  rownames(es_data)))
  plot_data <- data.frame(
    time = time_periods,
    irr = exp(es_data[, "Estimate"]),
    ci_lower = exp(es_data[, "Estimate"] - 1.96 * es_data[, "Std. Error"]),
    ci_upper = exp(es_data[, "Estimate"] + 1.96 * es_data[, "Std. Error"])
  )
  
  # Since there's no staggered rollout, can just keep x-axis as year for interpretability
  plot_data <- plot_data %>% 
    mutate(FY = 2019L + time_periods)
  
  return(plot_data)
}
plot_es <- function(es_plot_data, title_label) {
  return(
    ggplot(es_plot_data, aes(x = FY, y = irr)) +
      
      annotate("rect", xmin = 2018.5, xmax = Inf, ymin = -Inf, ymax = Inf,
               fill = "gray90", alpha = 0.4) +
      
      geom_hline(yintercept = 1, linetype = "solid", color = "black", linewidth = 0.2) +
      geom_errorbar(aes(ymin = ci_lower, ymax = ci_upper), 
                    width = 0.2, color = "black", linewidth = 0.6) +
      
      geom_point(size = 2, color = "black") +
      geom_point(data = data.frame(FY = 2018, irr = 1), 
                 aes(x = FY, y = irr), 
                 shape = 16, size = 2, color = "black") +
      
      geom_vline(xintercept = 2018.5, linetype = "dashed", color = "gray40", linewidth = 0.4) +
      
      scale_x_continuous(breaks = seq(min(es_plot_data$FY), max(es_plot_data$FY), by = 1)) +
      
      scale_y_continuous(limits = c(0.6, 1.4),
                         labels = scales::label_number(accuracy = 0.1),
      ) +
      
      # Labels
      labs(
        x = "year",
        y = "IRR",
        title = title_label
      ) +
      theme_classic() +
      theme(
        panel.grid.major.y = element_line(color = "gray80", linewidth = 0.3),
        axis.title = element_text(color = "black", size = 8),
        axis.text  = element_text(color = "black", size = 8),
        # Draw left & bottom axes
        axis.line.x.bottom = element_line(color = "black", linewidth = 0.4),
        axis.line.y.left   = element_line(color = "black", linewidth = 0.4),
        plot.title = element_text(size = 9)
      )
  )
  
}

# Read in analysis df for each treatment definition in 07_did_poisson_yearly.R ====
physicalborder_df <- read_csv("code/descriptive/physicalborder_df.csv") %>%
  mutate(rate_per_100000 = (n_crashes / pop_total) * 100000,
         FY = as.integer(as.character(FY)),
         time_to_treat = if_else(border == 1, FY - 2019L, 0L),
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

physicalborder_50_df <- read_csv("code/descriptive/physicalborder50_df.csv") %>%
  mutate(rate_per_100000 = (n_crashes / pop_total) * 100000,
         FY = as.integer(as.character(FY)),
         time_to_treat = if_else(border == 1, FY - 2019L, 0L),
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

physicalborder_100_df <- read_csv("code/descriptive/physicalborder100_df.csv") %>%
  mutate(rate_per_100000 = (n_crashes / pop_total) * 100000,
         FY = as.integer(as.character(FY)),
         time_to_treat = if_else(border == 1, FY - 2019L, 0L),
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


# 1. Physical border counties only ====
# (1A) ES, county FE + year FE
physicalborder_es_countyFE_yearFE <- fepois(
  n_crashes ~ i(time_to_treat, border, ref = -1) +         # ref = 2018 (implementation - 1)
    unemployment_rate + median_household_income_deflated | # controls
    geoid + FY,
  offset  = ~ log(pop_total),
  cluster = ~ geoid,                                       # Clustered SE by county
  data    = physicalborder_df
)

summary(physicalborder_es_countyFE_yearFE)
etable(physicalborder_es_countyFE_yearFE)
exp(coefficients(physicalborder_es_countyFE_yearFE))

es_plot_df_physicalboundary <- create_es_plot_df(physicalborder_es_countyFE_yearFE)

View(es_plot_df_physicalboundary %>% mutate(irr = round(irr, 2),
                                            ci_lower = round(ci_lower, 2),
                                            ci_upper = round(ci_upper, 2)))

es_plot_physicalboundary <- plot_es(es_plot_df_physicalboundary, "county FE & year FE")
es_plot_physicalboundary

# (1B) ES, state-specific linear trend
physicalborder_es_statelinear <- fepois(
  n_crashes ~ i(time_to_treat, border, ref = -1) +         # ref = 2018 (implementation - 1)
    unemployment_rate + median_household_income_deflated | # controls
    geoid + FY + STATE[FY],
  offset  = ~ log(pop_total),
  cluster = ~ geoid,                                       # Clustered SE by county
  data    = physicalborder_df
)

summary(physicalborder_es_statelinear)
etable(physicalborder_es_statelinear)
exp(coefficients(physicalborder_es_statelinear))

es_plot_df_physicalboundary_statelinear <- create_es_plot_df(physicalborder_es_statelinear)

View(es_plot_df_physicalboundary_statelinear %>% mutate(irr = round(irr, 2),
                                            ci_lower = round(ci_lower, 2),
                                            ci_upper = round(ci_upper, 2)))

es_plot_physicalboundary_statelinear <- plot_es(es_plot_df_physicalboundary_statelinear, "state-specific linear trends")
es_plot_physicalboundary_statelinear

# (1C) ES, county FE and state^year FE
physicalborder_es_stateyearFE <- fepois(
  n_crashes ~ i(time_to_treat, border, ref = -1) +         # ref = 2018 (implementation - 1)
    unemployment_rate + median_household_income_deflated | # controls
    geoid + STATE^FY,
  offset  = ~ log(pop_total),
  cluster = ~ geoid,                                       # Clustered SE by county
  data    = physicalborder_df
)

summary(physicalborder_es_stateyearFE)
etable(physicalborder_es_stateyearFE)
exp(coefficients(physicalborder_es_stateyearFE))

es_plot_df_physicalboundary_stateyearFE <- create_es_plot_df(physicalborder_es_stateyearFE)

View(es_plot_df_physicalboundary_stateyearFE %>% mutate(irr = round(irr, 2),
                                                        ci_lower = round(ci_lower, 2),
                                                        ci_upper = round(ci_upper, 2)))


es_plot_physicalboundary_stateyearFE <- plot_es(es_plot_df_physicalboundary_stateyearFE, "county FE & state^year FE")
es_plot_physicalboundary_stateyearFE

# stack ES physical boundary plots

physicalboundary_plots <- es_plot_physicalboundary /
  es_plot_physicalboundary_statelinear / es_plot_physicalboundary_stateyearFE

physicalboundary_plots + 
  plot_annotation(
  title = "border = physical border county",
  theme = theme(plot.title = element_text(hjust = 0.5, size = 12, face = "bold"))
)

# Wald-test/F-statistic, joint hypothesis test for parellel trends
wald(physicalborder_es_countyFE_yearFE, keep = "time_to_treat::-[2-8]:border")

# Wald-test/F-statistic, joint hypothesis test for parellel trends
wald(physicalborder_es_statelinear, keep = "time_to_treat::-[2-8]:border")

# Wald-test/F-statistic, joint hypothesis test for parellel trends
wald(physicalborder_es_stateyearFE, keep = "time_to_treat::-[2-8]:border")

# 2. Within 50 mi of physical border county ====
# (2A) ES, county FE + year FE
physicalborder_50_es_countyFE_yearFE <- fepois(
  n_crashes ~ i(time_to_treat, border, ref = -1) +         # ref = 2018 (implementation - 1)
    unemployment_rate + median_household_income_deflated | # controls
    geoid + FY,
  offset  = ~ log(pop_total),
  cluster = ~ geoid,                                       # Clustered SE by county
  data    = physicalborder_50_df
)

summary(physicalborder_50_es_countyFE_yearFE)
etable(physicalborder_50_es_countyFE_yearFE)
exp(coefficients(physicalborder_50_es_countyFE_yearFE))

es_plot_df_physicalboundary_50 <- create_es_plot_df(physicalborder_50_es_countyFE_yearFE)
View(es_plot_df_physicalboundary_50  %>% mutate(irr = round(irr, 2),
                                                        ci_lower = round(ci_lower, 2),
                                                        ci_upper = round(ci_upper, 2)))

es_plot_physicalboundary_50 <- plot_es(es_plot_df_physicalboundary_50, "county FE & year FE")
es_plot_physicalboundary_50

# (2B) ES, state-specific linear trend
physicalborder_50_es_statelinear <- fepois(
  n_crashes ~ i(time_to_treat, border, ref = -1) +         # ref = 2018 (implementation - 1)
    unemployment_rate + median_household_income_deflated | # controls
    geoid + FY + STATE[FY],
  offset  = ~ log(pop_total),
  cluster = ~ geoid,                                       # Clustered SE by county
  data    = physicalborder_50_df
)

summary(physicalborder_50_es_statelinear)
etable(physicalborder_50_es_statelinear)
exp(coefficients(physicalborder_50_es_statelinear))

es_plot_df_physicalboundary_50_statelinear <- create_es_plot_df(physicalborder_50_es_statelinear)
View(es_plot_df_physicalboundary_50_statelinear  %>% mutate(irr = round(irr, 2),
                                                ci_lower = round(ci_lower, 2),
                                                ci_upper = round(ci_upper, 2)))

es_plot_physicalboundary_50_statelinear <- plot_es(es_plot_df_physicalboundary_50_statelinear, "state-specific linear trends")
es_plot_physicalboundary_50_statelinear

# (2C) ES, county FE and state^year FE
physicalborder_50_es_stateyearFE <- fepois(
  n_crashes ~ i(time_to_treat, border, ref = -1) +         # ref = 2018 (implementation - 1)
    unemployment_rate + median_household_income_deflated | # controls
    geoid + STATE^FY,
  offset  = ~ log(pop_total),
  cluster = ~ geoid,                                       # Clustered SE by county
  data    = physicalborder_50_df
)

summary(physicalborder_50_es_stateyearFE)
etable(physicalborder_50_es_stateyearFE)
exp(coefficients(physicalborder_50_es_stateyearFE))

es_plot_df_physicalboundary_50_stateyearFE <- create_es_plot_df(physicalborder_50_es_stateyearFE)
View(es_plot_df_physicalboundary_50_stateyearFE %>% mutate(irr = round(irr, 2),
                                                            ci_lower = round(ci_lower, 2),
                                                            ci_upper = round(ci_upper, 2)))

es_plot_physicalboundary_50_stateyearFE <- plot_es(es_plot_df_physicalboundary_50_stateyearFE, "county FE & state^year FE")
es_plot_physicalboundary_50_stateyearFE

# stack ES physical boundary plots

physicalboundary_50_plots <- es_plot_physicalboundary_50 / 
  es_plot_physicalboundary_50_statelinear / es_plot_physicalboundary_50_stateyearFE

physicalboundary_50_plots + 
  plot_annotation(
    title = "border = within 50 miles of physical border county",
    theme = theme(plot.title = element_text(hjust = 0.5, size = 12, face = "bold"))
  )


# 3. Within 100  mi of physical border county ====
# (3A)  ES, county FE + year FE
physicalborder_100_es_countyFE_yearFE <- fepois(
  n_crashes ~ i(time_to_treat, border, ref = -1) +         # ref = 2018 (implementation - 1)
    unemployment_rate + median_household_income_deflated | # controls
    geoid + FY,
  offset  = ~ log(pop_total),
  cluster = ~ geoid,                                       # Clustered SE by county
  data    = physicalborder_100_df
)

summary(physicalborder_100_es_countyFE_yearFE)
etable(physicalborder_100_es_countyFE_yearFE)
exp(coefficients(physicalborder_100_es_countyFE_yearFE))

es_plot_df_physicalboundary_100 <- create_es_plot_df(physicalborder_100_es_countyFE_yearFE)

View(es_plot_df_physicalboundary_100 %>% mutate(irr = round(irr, 2),
                                                           ci_lower = round(ci_lower, 2),
                                                           ci_upper = round(ci_upper, 2)))
es_plot_physicalboundary_100 <- plot_es(es_plot_df_physicalboundary_100, "county FE & year FE")
es_plot_physicalboundary_100

# (3B) ES, state-specific linear trend
physicalborder_100_es_statelinear <- fepois(
  n_crashes ~ i(time_to_treat, border, ref = -1) +         # ref = 2018 (implementation - 1)
    unemployment_rate + median_household_income_deflated | # controls
    geoid + FY + STATE[FY],
  offset  = ~ log(pop_total),
  cluster = ~ geoid,                                       # Clustered SE by county
  data    = physicalborder_100_df
)

summary(physicalborder_100_es_statelinear)
etable(physicalborder_100_es_statelinear)
exp(coefficients(physicalborder_100_es_statelinear))

es_plot_df_physicalboundary_100_statelinear <- create_es_plot_df(physicalborder_100_es_statelinear)


View(es_plot_df_physicalboundary_100_statelinear %>% mutate(irr = round(irr, 2),
                                                ci_lower = round(ci_lower, 2),
                                                ci_upper = round(ci_upper, 2)))
es_plot_physicalboundary_100_statelinear <- plot_es(es_plot_df_physicalboundary_100_statelinear, "state-specific linear trends")
es_plot_physicalboundary_100_statelinear

# (3C) ES, county FE and state^year FE
physicalborder_100_es_stateyearFE <- fepois(
  n_crashes ~ i(time_to_treat, border, ref = -1) +         # ref = 2018 (implementation - 1)
    unemployment_rate + median_household_income_deflated | # controls
    geoid + STATE^FY,
  offset  = ~ log(pop_total),
  cluster = ~ geoid,                                       # Clustered SE by county
  data    = physicalborder_100_df
)

summary(physicalborder_100_es_stateyearFE)
etable(physicalborder_100_es_stateyearFE)
exp(coefficients(physicalborder_100_es_stateyearFE))

es_plot_df_physicalboundary_100_stateyearFE <- create_es_plot_df(physicalborder_100_es_stateyearFE)

View(es_plot_df_physicalboundary_100_stateyearFE %>% mutate(irr = round(irr, 2),
                                                            ci_lower = round(ci_lower, 2),
                                                            ci_upper = round(ci_upper, 2)))

es_plot_physicalboundary_100_stateyearFE <- plot_es(es_plot_df_physicalboundary_100_stateyearFE, "county FE & state^year FE")
es_plot_physicalboundary_100_stateyearFE

# stack ES physical boundary plots

physicalboundary_100_plots <- es_plot_physicalboundary_100 /
  es_plot_physicalboundary_100_statelinear / es_plot_physicalboundary_100_stateyearFE

physicalboundary_100_plots + 
  plot_annotation(
    title = "border: within 100 miles of physical border county",
    theme = theme(plot.title = element_text(hjust = 0.5, size = 12, face = "bold"))
  )

# ==== 
physicalboundary_plots / physicalboundary_50_plots / physicalboundary_100_plots
