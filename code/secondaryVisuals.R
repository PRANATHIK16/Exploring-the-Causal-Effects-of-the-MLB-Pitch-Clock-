# Creating the Summary graph for spectator counts
## Downloading required packages
library(ggplot2)
library(dplyr)
library(knitr)
library(kableExtra)

## Creating Summary Table for Spectator Counts
summary_table_spectators <- final_data %>%
  group_by(Year) %>%
  summarize(
    Mean_Spectators = mean(`Spectator Count`, na.rm = TRUE),
    Median_Spectators = median(`Spectator Count`, na.rm = TRUE),
    Max_Spectators = max(`Spectator Count`, na.rm = TRUE),
    Min_Spectators = min(`Spectator Count`, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  rename(
    `Mean Spectators` = Mean_Spectators,
    `Median Spectators` = Median_Spectators,
    `Max Spectators` = Max_Spectators,
    `Min Spectators` = Min_Spectators
  )

## Displaying summary table for spectators
summary_table_spectators %>%
  kable(
    caption = "Summary Statistics for Spectator Counts by Year", 
    digits = 0,  # Using 0 digits since we're counting people
    align = c("c", rep("c", 4))
  ) %>%
  kable_styling(
    bootstrap_options = c("striped", "hover", "condensed"), 
    full_width = FALSE, 
    font_size = 14
  ) %>%
  row_spec(0, bold = TRUE)

## Creating violin plot for spectator counts
ggplot(final_data, aes(x = factor(Year), y = `Spectator Count`)) +
  geom_violin(alpha = 0.5, fill = "steelblue") +
  labs(
    title = "Distribution of Spectator Counts by Season",
    x = "Season",
    y = "Number of Spectators"
  ) +
  theme_classic() +
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1),
    plot.title = element_text(hjust = 0.5, face = "bold")
  ) +
  scale_y_continuous(labels = scales::comma)  # Format y-axis with commas for thousands
