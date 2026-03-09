library(dplyr)
library(readr)
library(ggplot2)
library(patchwork)


# Read in analysis df for each treatment definition in 07_did_poisson_yearly.R
physicalborder_df <- read_csv("code/descriptive/physicalborder_df.csv") %>%
  mutate(rate_per_100000 = (n_crashes / pop_total) * 100000)

physicalborder_50_df <- read_csv("code/descriptive/physicalborder50_df.csv") %>%
  mutate(rate_per_100000 = (n_crashes / pop_total) * 100000)

physicalborder_100_df <- read_csv("code/descriptive/physicalborder100_df.csv") %>%
  mutate(rate_per_100000 = (n_crashes / pop_total) * 100000)


# (1) Summary table ====
summ_stats <- function(df) {
  df %>%
    filter(!is.na(rate_per_100000)) %>%
    summarise(
      N  = n(),
      M  = round(mean(rate_per_100000), 2),
      SD = round(sd(rate_per_100000), 2)
    )
}

summ_stats_by_border <- function(df) {
  df %>%
    filter(!is.na(rate_per_100000)) %>%
    group_by(border) %>%
    summarise(
      N  = n(),
      M  = round(mean(rate_per_100000), 2),
      SD = round(sd(rate_per_100000), 2),
      .groups = "drop"
    )
}

# Full: FY2011–FY2023
full_df <- physicalborder_df %>% filter(between(FY, 2011L, 2023L))

# Pre: FY2011–FY2018  (last full pre FY)
pre_df  <- physicalborder_df %>% filter(between(FY, 2011L, 2018L))

# Post: FY2019–FY2023 (first FY starting Oct 2018)
post_df <- physicalborder_df %>% filter(between(FY, 2019L, 2023L))

summ_full_all   <- summ_stats(full_df)
summ_full_pre   <- summ_stats(pre_df)
summ_full_post  <- summ_stats(post_df)

summ_border_all <- summ_stats_by_border(full_df)
summ_border_pre <- summ_stats_by_border(pre_df)
summ_border_post<- summ_stats_by_border(post_df)

bind_rows(
  summ_stats(full_df)  %>% mutate(period = "FY2011–FY2023", group = "All"),
  summ_stats(pre_df)   %>% mutate(period = "FY2011–FY2018", group = "All"),
  summ_stats(post_df)  %>% mutate(period = "FY2019–FY2023", group = "All"),
  summ_stats_by_border(full_df) %>% mutate(period = "FY2011–FY2023", group = if_else(border==1,"Border","Non-border")),
  summ_stats_by_border(pre_df)  %>% mutate(period = "FY2011–FY2018", group = if_else(border==1,"Border","Non-border")),
  summ_stats_by_border(post_df) %>% mutate(period = "FY2019–FY2023", group = if_else(border==1,"Border","Non-border"))
) %>% select(period, group, N, M, SD)

# (2) Trend graphs ====
# 1a) PHYSICAL BOUNDARY: collapse to yearly means for the 3 groups
plot_df <- physicalborder_df %>%
  mutate(group = if_else(border == 1, "border counties", "non-border counties")) %>%
  group_by(FY, group) %>%
  summarise(rate = mean(rate_per_100000, na.rm = TRUE), .groups = "drop") %>%
  bind_rows(
    physicalborder_df %>%
      group_by(FY) %>%
      summarise(rate = mean(rate_per_100000, na.rm = TRUE), .groups = "drop") %>%
      mutate(group = "border & non-border counties")
  )

# 1b) PHYSICAL BOUNDARY, WITHIN 50: collapse to yearly means for the 3 groups
plot_df_50 <- physicalborder_50_df %>%
  mutate(group = if_else(border == 1, "border counties", "non-border counties")) %>%
  group_by(FY, group) %>%
  summarise(rate = mean(rate_per_100000, na.rm = TRUE), .groups = "drop") %>%
  bind_rows(
    physicalborder_df %>%
      group_by(FY) %>%
      summarise(rate = mean(rate_per_100000, na.rm = TRUE), .groups = "drop") %>%
      mutate(group = "border & non-border counties")
  )

# 1c) PHYSICAL BOUNDARY, WITHIN 100: collapse to yearly means for the 3 groups
plot_df_100 <- physicalborder_100_df %>%
  mutate(group = if_else(border == 1, "border counties", "non-border counties")) %>%
  group_by(FY, group) %>%
  summarise(rate = mean(rate_per_100000, na.rm = TRUE), .groups = "drop") %>%
  bind_rows(
    physicalborder_df %>%
      group_by(FY) %>%
      summarise(rate = mean(rate_per_100000, na.rm = TRUE), .groups = "drop") %>%
      mutate(group = "border & non-border counties")
  )

# 2) Plot, physical boundary
plot <- ggplot(plot_df, aes(x = FY, y = rate, color = group, linetype = group)) +
  
  geom_line(linewidth = 0.7) +
  geom_point(size = 1.5, alpha = 0.5) +
  
  scale_color_manual(values = c(
    "border counties"     = "#D55E00",
    "non-border counties" = "#0072B2",  
    "border & non-border counties" = "#999999" 
  )) +
  
  geom_vline(xintercept = 2019, color = "black", linewidth = 0.5) +
  
  scale_linetype_manual(values = c(
    "border counties"        = "solid",
    "non-border counties"   = "twodash",
    "border & non-border counties" = "twodash"
  )) +
  
  scale_y_continuous(
    breaks = scales::pretty_breaks(n = 6),
    labels = scales::label_number(accuracy = 1),
    limits = c(10, 20)) + # for y axes to be the same

  # force every year to appear
  scale_x_continuous(
    breaks = sort(unique(plot_df$FY)),
  ) +
  
  labs(
    x = "year",
    y = "average county fatal crash rate",
    color = "",
    linetype = "",
    title = "border = physical border county; n, treated = 77 counties"
  ) +
  
  theme_classic() +
  theme(
    legend.position = "right",

    # Draw left & bottom axes
    axis.line.x.bottom = element_line(color = "black", linewidth = 0.4),
    axis.line.y.left   = element_line(color = "black", linewidth = 0.4),
    
    # Kill top/right axes
    axis.line.x.top    = element_blank(),
    axis.line.y.right  = element_blank(),
    
    axis.title = element_text(color = "black", size = 8),
    axis.text  = element_text(color = "black", size = 8),
    
    plot.title = element_text(size = 9)
    
  )

# 2b) Plot, physical boundary, 50 miles
plot_50mi <- ggplot(plot_df_50, aes(x = FY, y = rate, color = group, linetype = group)) +
  
  geom_line(linewidth = 0.7) +
  geom_point(size = 1.5, alpha = 0.5) +
  
  scale_color_manual(values = c(
    "border counties"     = "#D55E00",
    "non-border counties" = "#0072B2",  
    "border & non-border counties"        = "#999999" 
  )) +
  
  geom_vline(xintercept = 2019, color = "black", linewidth = 0.5) +
  
  scale_linetype_manual(values = c(
    "border counties"        = "solid",
    "non-border counties"   = "twodash",
    "border & non-border counties" = "twodash"
  )) +
  
  scale_y_continuous(
    breaks = scales::pretty_breaks(n = 6),
    labels = scales::label_number(accuracy = 1),
    limits = c(10, 20)) +
  
  # force every year to appear
  scale_x_continuous(
    breaks = sort(unique(plot_df_50$FY)),
  ) +
  
  labs(
    x = "year",
    y = "average county fatal crash rate",
    color = "",
    linetype = "",
    title = "border = within 50 miles of physical border county; n, treated = 147 counties"
  ) +
  
  theme_classic() +
  theme(
    legend.position = "right",
    
    # Draw left & bottom axes
    axis.line.x.bottom = element_line(color = "black", linewidth = 0.4),
    axis.line.y.left   = element_line(color = "black", linewidth = 0.4),
    
    # Kill top/right axes
    axis.line.x.top    = element_blank(),
    axis.line.y.right  = element_blank(),
    
    axis.title = element_text(color = "black", size = 8),
    axis.text  = element_text(color = "black", size = 8),
    
    plot.title = element_text(size = 9)
  )

# 2c) Plot, physical boundary, 100 miles

plot_100mi <- ggplot(plot_df_100, aes(x = FY, y = rate, color = group, linetype = group)) +
  
  geom_line(linewidth = 0.7) +
  geom_point(size = 1.5, alpha = 0.5) +
  
  scale_color_manual(values = c(
    "border counties"     = "#D55E00",
    "non-border counties" = "#0072B2",  
    "border & non-border counties" = "#999999" 
  )) +
  
  geom_vline(xintercept = 2019, color = "black", linewidth = 0.5) +
  
  scale_linetype_manual(values = c(
    "border counties"        = "solid",
    "non-border counties"   = "twodash",
    "border & non-border counties" = "twodash"
  )) +
  
  scale_y_continuous(
    breaks = scales::pretty_breaks(n = 6),
    labels = scales::label_number(accuracy = 1),
    limits = c(10, 20)) +
  
  # force every year to appear
  scale_x_continuous(
    breaks = sort(unique(plot_df_100$FY)),
  ) +
  
  labs(
    x = "year",
    y = "average county fatal crash rate",
    color = "",
    linetype = "",
    title = "border = within 100 miles of physical border county; n, treated = 298 counties"
  ) +
  
  theme_classic() +
  theme(
    legend.position = "right",
    
    # Draw left & bottom axes
    axis.line.x.bottom = element_line(color = "black", linewidth = 0.4),
    axis.line.y.left   = element_line(color = "black", linewidth = 0.4),
    
    # Kill top/right axes
    axis.line.x.top    = element_blank(),
    axis.line.y.right  = element_blank(),
    
    axis.title = element_text(color = "black", size = 8),
    axis.text  = element_text(color = "black", size = 8),
    
    plot.title = element_text(size = 9),
    )


stacked_figures <- (plot / plot_50mi / plot_100mi) + 
  plot_layout(guides = "collect")
stacked_figures  

