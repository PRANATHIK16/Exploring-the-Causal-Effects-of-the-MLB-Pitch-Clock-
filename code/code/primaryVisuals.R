# Creating the Summarise graph for game duration
## Downloading required packages
library(ggplot2)
library(dplyr)
library(knitr)
library(kableExtra)


## Creating Summary Table

summary_table <- final_data %>%
  group_by(Year) %>%
  summarize(
    Mean_Length = mean(`Duration of Game`, na.rm = TRUE),
    Median_Length = median(`Duration of Game`, na.rm = TRUE),
    Max_Length = max(`Duration of Game`, na.rm = TRUE),
    Min_Length = min(`Duration of Game`, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  rename(
    `Mean Duration` = Mean_Length,
    `Median Duration` = Median_Length,
    `Max Duration` = Max_Length,
    `Min Duration` = Min_Length
  )

## Displaying summary table
summary_table %>%
  kable(
    caption = "Summary Statistics for Game Duration by Year", 
    digits = 2, 
    align = c("c", rep("c", 4))
  ) %>%
  kable_styling(
    bootstrap_options = c("striped", "hover", "condensed"), 
    full_width = FALSE, 
    font_size = 14
  ) %>%
  row_spec(0, bold = TRUE)


## Creating violin plot
ggplot(final_data, aes(x = factor(Year), y = `Duration of Game`)) +
  geom_violin(alpha = 0.5) +
  labs(
    title = "Density of Game Duration by Season",
    x = "Season",
    y = "Game Duration (minutes)",
  ) +
  theme_classic() +
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1)
  )
