## 05_trial_analysis.R ---------------------------------------------------
## Three-way ANOVA (learners only)
anova_data_learners <- full_trial_data_learners %>% 
  group_by(participant, block, pair_type, group) %>% 
  summarise(choice_a_mean = mean(choice_a, na.rm = TRUE), .groups = "drop") %>% 
  mutate(across(c(block, pair_type, group), as.factor))

anova_model_learners <- aov(choice_a_mean ~ block * pair_type * group,
                            data = anova_data_learners)
print(summary(anova_model_learners))

## Post-hoc per block ----------------------------------------------------
trial_data_summary_df_united <- full_trial_data_learners %>% 
  group_by(participant, block, pair_type, group) %>% 
  summarise(choice_a_sum = sum(choice_a, na.rm = TRUE), .groups = "drop") %>% 
  rename(participant_type = group)

blocks <- unique(trial_data_summary_df_united$block)
emmeans_results_by_block <- vector("list", length(blocks))

for (b in blocks) {
  block_data <- filter(trial_data_summary_df_united, block == b)
  if (n_distinct(block_data$participant_type) < 2) next
  
  model <- aov(choice_a_sum ~ participant_type, data = block_data)
  emms  <- pairs(emmeans(model, ~ participant_type), adjust = "tukey")
  emmeans_results_by_block[[paste0("Block_", b)]] <- summary(emms)
}

## Plot ------------------------------------------------------------------
group_sizes <- trial_data_summary_df_united %>% 
  distinct(participant, participant_type) %>% count(participant_type)
labels_vec <- setNames(paste0(group_sizes$participant_type,
                              " (n = ", group_sizes$n, ")"),
                       group_sizes$participant_type)

implicit_plot_all_groups <- ggplot(trial_data_summary_df_united,
       aes(block, choice_a_sum,
           color = participant_type,
           linetype = pair_type,
           group = interaction(participant_type, pair_type))) +
  stat_summary(fun = mean, geom = "line", linewidth = 1.2) +
  stat_summary(fun = mean, geom = "point", size = 2) +
  stat_summary(fun.data = mean_se, geom = "errorbar", width = .2) +
  scale_y_continuous(limits = c(1, 10)) +
  scale_color_manual(labels = labels_vec,
                     values = c(Healthy = "#1b9e77",
                                Fibro   = "#d95f02",
                                `Healthy Stress` = "#7570b3")) +
  labs(title = "Choice-A Sum across Blocks",
       x = "Block", y = "Sum of Choice-A",
       color = "Group", linetype = "Pair Type") +
  theme(text = element_text(size = 14),
        legend.position = "bottom")
