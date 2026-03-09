library(tidyverse)
library(fixest)

# Read in analysis df for main treatment definition in 07_did_poisson_yearly.R ====
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


natural <- fepois(
  n_crashes ~ border * post_canada + unemployment_rate + median_household_income_deflated | geoid + STATE^FY_fe,
  offset  = ~ log(pop_total),
  cluster = ~ geoid,
  data    = physicalborder_df
)

etable(natural)

# MDE change
physicalborder_df_1per <- physicalborder_df %>% 
  mutate(n_crashes_1per = if_else((border == 1 & post_canada == 1), n_crashes * 1.50, n_crashes))

df_1per <- fepois(
  n_crashes_1per ~ border * post_canada + unemployment_rate + median_household_income_deflated | geoid + STATE^FY_fe,
  offset  = ~ log(pop_total),
  cluster = ~ geoid,
  data    = physicalborder_df_1per
)

etable(df_1per)
coefs <- coeftable(df_1per)
param <- coefs["border:post_canada", ]
irr = exp(param["Estimate"])
ci_lower = exp(param["Estimate"] - 1.96 * param["Std. Error"])
ci_upper = exp(param["Estimate"] + 1.96 * param["Std. Error"])


# write functions do to this shizzz cuz im cool like dat and fresh
create_artificial <- function(percent) {
  physicalborder_df_per <- physicalborder_df %>% 
    mutate(n_crashes_per = if_else((border == 1 & post_canada == 1), n_crashes * percent, n_crashes))
  
  return(physicalborder_df_per)
}
run_artificial_model <- function(df, name) {
  mod <- fepois(
    n_crashes_per ~ border * post_canada + unemployment_rate + median_household_income_deflated | geoid + STATE^FY_fe,
    offset  = ~ log(pop_total),
    cluster = ~ geoid,
    data    = df
  )

  coefs <- coeftable(mod)
  param <- coefs["border:post_canada", ]
  irr = exp(param["Estimate"])
  ci_lower = exp(param["Estimate"] - 1.96 * param["Std. Error"])
  ci_upper = exp(param["Estimate"] + 1.96 * param["Std. Error"])
  
  output <- data.frame(percent = name, irr, ci_lower, ci_upper)
  
  return(output)
}

percents <- c(1.01, 1.02, 1.05, 1.10, 1.25, 1.50)
names(percents) <- paste0("df_", percents)

# Create df
per_df <- lapply(percents, create_artificial)

# Run models
outputs <- mapply(run_artificial_model, per_df, names(per_df), SIMPLIFY = FALSE)
plot_df <- bind_rows(outputs) %>%
  mutate(percent = as.numeric(sub("df_", "", percent)))

# 
# rng <- range(plot_df$irr, na.rm = TRUE)
# dist <- max(abs(rng - 1))
# 
# ggplot(plot_df, aes(x = percent, y = irr)) +
#   geom_point() +
#   geom_hline(yintercept = 1, linetype = "dashed") +
#   geom_errorbar(aes(ymin = ci_lower, ymax = ci_upper), 
#                 width = 0.01, color = "black", linewidth = 0.6) +
#   scale_y_continuous(limits = c(1 - dist, 1 + dist)) +
#   theme_classic() +
#   theme(
#     panel.grid.major.y = element_line(color = "gray80", linewidth = 0.3),
#     axis.title = element_text(color = "black", size = 8),
#     axis.text  = element_text(color = "black", size = 8),
#     axis.line.x.bottom = element_line(color = "black", linewidth = 0.4),
#     axis.line.y.left   = element_line(color = "black", linewidth = 0.4),
#     plot.title = element_text(size = 9)
#   )
#     