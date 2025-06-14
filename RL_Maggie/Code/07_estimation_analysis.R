## 07_estimation_analysis.R ---------------------------------------------
## Plot high-minus-low probes -------------------------------------------
high_vs_low_df <- full_estimation_data_clean %>% 
  group_by(group, participant, block, pair_type, is_high_probe) %>% 
  summarise(mean_est = mean(estimation_response, na.rm = TRUE), .groups = "drop") %>% 
  pivot_wider(names_from = is_high_probe, values_from = mean_est,
              names_prefix = "is_high_") %>% 
  mutate(choice_diff = is_high_TRUE - is_high_FALSE)

group_block_diff <- high_vs_low_df %>% 
  group_by(group, block, pair_type) %>% 
  summarise(mean_choice_diff = mean(choice_diff, na.rm = TRUE),
            se = sd(choice_diff, na.rm = TRUE)/sqrt(n()), .groups = "drop")

labels_vec_exp <- group_block_diff %>% distinct(group) %>% 
  left_join(full_estimation_data_clean %>% 
              distinct(participant, group) %>% count(group),
            by = "group") %>% 
  { setNames(paste0(.$group, " (n = ", .$n, ")"), .$group) }

plot_estimation_all_groups <- ggplot(group_block_diff,
       aes(block, mean_choice_diff,
           color = group, linetype = pair_type,
           group = interaction(group, pair_type))) +
  geom_line(linewidth = 1.2) + geom_point(size = 3) +
  geom_errorbar(aes(ymin = mean_choice_diff - se,
                    ymax = mean_choice_diff + se), width = .2) +
  geom_hline(yintercept = 0, linetype = "dashed") +
  ylim(-50, 100) + theme_minimal() +
  scale_color_manual(labels = labels_vec_exp,
                     values = c(Healthy = "#1b9e77",
                                Fibro   = "#d95f02",
                                `Healthy Stress` = "#7570b3")) +
  labs(title  = "Explicit Estimations – All Groups",
       x      = "Block",
       y      = "Mean Estimation Δ (high − low)",
       color  = "Group",
       linetype = "Pair Type") +
  theme(text = element_text(size = 14),
        legend.position = "bottom")

## Three-way ANOVA -------------------------------------------------------
anova_data_explicit <- full_estimation_data %>% 
  group_by(participant, block, pair_type, group, is_high_probe) %>% 
  summarise(mean_est = mean(estimation_response, na.rm = TRUE), .groups = "drop") %>% 
  pivot_wider(names_from = is_high_probe, values_from = mean_est,
              names_prefix = "is_high_") %>% 
  mutate(estimation_diff = is_high_TRUE - is_high_FALSE,
         across(c(block, pair_type, group), as.factor))

anova_explicit <- aov(estimation_diff ~ block * pair_type * group,
                      data = anova_data_explicit)
print(summary(anova_explicit))

## Post-hoc --------------------------------------------------------------
explicit_sum <- full_estimation_data_clean %>% 
  group_by(participant, block, group, pair_type) %>% 
  summarise(mean_est = mean(estimation_response, na.rm = TRUE), .groups = "drop") %>% 
  rename(participant_type = group)

print(pairwise.t.test(explicit_sum$mean_est,
                      interaction(explicit_sum$block, explicit_sum$participant_type),
                      p.adjust.method = "bonferroni"))
print(pairwise.t.test(explicit_sum$mean_est,
                      interaction(explicit_sum$participant_type, explicit_sum$pair_type),
                      p.adjust.method = "bonferroni"))
