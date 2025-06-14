## 08_questionnaire_analysis.R ------------------------------------------
fibro_questionnaire_df <- read.csv(QUESTIONNAIRE_CSV)

rl_scores_df <- full_trial_data %>% 
  filter(block == 6, correct == 1) %>% 
  group_by(participant) %>% summarise(rl_score = n(), .groups = "drop") %>% 
  filter(!is.nan(rl_score))

fibro_merged <- left_join(fibro_questionnaire_df, rl_scores_df, by = "participant")

## Regressions -----------------------------------------------------------
lm_full  <- lm(rl_score ~ Age + PCS_Total + FIQR_Total + PIPS_TOTAL, data = fibro_merged)
lm_basic <- lm(rl_score ~ PIPS_TOTAL + FIQR_Total, data = fibro_merged)

print(summary(lm_full))
print(summary(lm_basic))

# Plot scatter of PIPS_TOTAL as X, FIQR as Y, and color by rl_score
fibro_merged %>% 
  ggplot(aes(x = PIPS_TOTAL, y = FIQR_Total, color = rl_score)) +
  geom_point() +
  labs(title = "Scatter Plot of PIPS_TOTAL vs FIQR_Total",
       x = "PIPS Total", y = "FIQR Total", color = "RL Score") +
  theme_minimal() +
  scale_color_gradient(low = "blue", high = "red")

# Plot a 3d plot of PIPS, FIQR, and rl_score
library(plotly)
fibro_merged %>% 
  plot_ly(x = ~PIPS_TOTAL, y = ~FIQR_Total, z = ~rl_score, 
          type = "scatter3d", mode = "markers",
          marker = list(size = 5, color = ~rl_score)) %>%
  layout(title = "3D Scatter Plot of PIPS_TOTAL, FIQR_Total, and RL Score",
         scene = list(xaxis = list(title = "PIPS Total"),
                      yaxis = list(title = "FIQR Total"),
                      zaxis = list(title = "RL Score")))
