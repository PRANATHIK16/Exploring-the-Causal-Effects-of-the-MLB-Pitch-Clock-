# Creating the Visuals for the Capacity Percentage

## Load packages for visuals
library(ggplot2)
library(knitr)
library(kableExtra)

## Create summary table
summary_table_Percentage <- filtered_data %>%
  group_by(Year) %>%
  summarize(
    Mean_percentage = mean(Spectator_Percentage, na.rm = TRUE),
    Median_percentage = median(Spectator_Percentage, na.rm = TRUE),
    Max_percentage = max(Spectator_Percentage, na.rm = TRUE),
    Min_percentage = min(Spectator_Percentage, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  rename(
    `Mean Capacity Pct.` = Mean_percentage,
    `Median Capacity Pct.` = Median_percentage,
    `Max Capacity Pct.` = Max_percentage,
    `Min Capacity Pct.` = Min_percentage
  ) %>%
  mutate(across(everything(), ~ round(.x * 100, 2)))

## Display summary table
summary_table_Percentage %>%
  kable(
    caption = "Summary Statistics for Stadium Capacity Pct. Per Season",
    align = c("c", rep("c", 4))
  ) %>%
  kable_styling(
    bootstrap_options = c("striped", "hover", "condensed"),
    full_width = FALSE,
    font_size = 14
  ) %>%
  row_spec(0, bold = TRUE)

## Create violin plot
ggplot(filtered_data, aes(x = factor(Year), y = Spectator_Percentage * 100)) +
  geom_violin(alpha = 0.5, fill = "steelblue") +
  labs(
    title = "Distribution of Stadium Capacity by Percentage",
    x = "Season",
    y = "Stadium Capacity (%)"
  ) +
  theme_classic() +
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1),
    plot.title = element_text(hjust = 0.5, face = "bold")
  ) +
  scale_y_continuous(labels = scales::label_percent(scale = 1))
