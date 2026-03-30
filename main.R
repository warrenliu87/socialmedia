library(tidyverse)
library(viridis)
library(scales)
df=read_rds('./clean_df.rds')

instagram_cols <- c(
  pink   = "#E1306C",
  purple = "#C13584",
  orange = "#F77737",
  yellow = "#FCAF45",
  blue   = "#405DE6"
)

theme_set(
  theme_minimal(base_size = 13) +
    theme(
      plot.title = element_text(face = "bold", size = 16),
      plot.subtitle = element_text(color = "gray30", size = 11),
      axis.title = element_text(face = "bold"),
      panel.grid.minor = element_blank(),
      legend.position = "right"
    )
)

set.seed(123)
df_plot1 <- df %>%
  filter(
    !is.na(sleep_hours_per_night),
    !is.na(daily_active_minutes_instagram),
    !is.na(perceived_stress_score)
  ) %>%
  slice_sample(n = 20000)
#Sleep vs Instagram use, colored by stress
p1 <- ggplot(
  df_plot1,
  aes(
    x = sleep_hours_per_night,
    y = daily_active_minutes_instagram,
    color = perceived_stress_score
  )
) +
  geom_point(alpha = 0.45, size = 1.6) +
  geom_smooth(method = "lm", se = TRUE, color = "#2D2D2D", linewidth = 1.1) +
  scale_color_gradientn(
    colors = c(instagram_cols["yellow"], instagram_cols["orange"], instagram_cols["pink"], instagram_cols["purple"]),
    name = "Stress score"
  ) +
  labs(
    title = "Sleep and Instagram Usage",
    subtitle = "Distribution of daily Instagram usage across sleep levels, with stress intensity shown by color",
    x = "Sleep hours per night",
    y = "Daily active minutes on Instagram"
  ) +
  theme(
    plot.title.position = "plot",
    panel.grid.major = element_line(color = "gray90")
  )

p1

#Average engagement by age group and income
age_breaks <- c(0, 18, 25, 35, 45, 55, 65, Inf)
age_labels <- c("<18", "18–24", "25–34", "35–44", "45–54", "55–64", "65+")

p2_data <- df %>%
  mutate(
    age_group = cut(age, breaks = age_breaks, labels = age_labels, right = FALSE),
    income_label = factor(
      income_num,
      levels = 0:4,
      labels = c("Low", "Lower-middle", "Middle", "Upper-middle", "High")
    )
  ) %>%
  filter(
    !is.na(age_group),
    !is.na(income_label),
    !is.na(user_engagement_score)
  ) %>%
  group_by(age_group, income_label) %>%
  summarise(
    avg_engagement = mean(user_engagement_score, na.rm = TRUE),
    .groups = "drop"
  )

p2 <- ggplot(p2_data, aes(x = income_label, y = age_group, fill = avg_engagement)) +
  geom_tile(color = "white", linewidth = 1) +
  geom_text(
    aes(label = round(avg_engagement, 2)),
    color = "white",
    fontface = "bold",
    size = 3.8
  ) +
  scale_fill_gradientn(
    colors = c(instagram_cols["blue"], instagram_cols["purple"], instagram_cols["pink"], instagram_cols["orange"], instagram_cols["yellow"]),
    name = "Avg engagement"
  ) +
  labs(
    title = "Average Engagement by Age Group and Income",
    subtitle = "A quick demographic view of where engagement is strongest",
    x = "Income level",
    y = "Age group"
  ) +
  theme(
    axis.text.x = element_text(angle = 20, hjust = 1),
    panel.grid = element_blank()
  )

p2
#followers vs following, colored by engagement
set.seed(123)

df_plot3 <- df %>%
  filter(
    !is.na(followers_count),
    !is.na(following_count),
    !is.na(user_engagement_score)
  ) %>%
  slice_sample(n = 300)

quantiles <- quantile(df_plot3$user_engagement_score, probs = c(0.05, 0.5, 0.95), na.rm = TRUE)

p3 <- ggplot(
  df_plot3,
  aes(
    x = following_count,
    y = followers_count,
    color = user_engagement_score
  )
) +
  geom_point(alpha = 0.5, size = 1.6) +
  geom_smooth(method = "lm", se = FALSE, color = "#2D2D2D", linewidth = 1) +
  scale_color_gradientn(
    colors = c("#405DE6", "#833AB4", "#E1306C", "#F77737", "#FCAF45"),
    values = scales::rescale(quantiles),
    name = "Engagement"
  ) +
  labs(
    title = "Followers vs Following",
    subtitle = "Color scale adjusted to highlight variation in engagement",
    x = "Following count",
    y = "Followers count"
  ) +
  theme_minimal(base_size = 13)

p3
#