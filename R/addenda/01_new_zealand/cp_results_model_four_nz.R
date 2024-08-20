


# read data ----
results_model_four_nz_periods <- read_csv(file = path_results_model_four_nz_periods_processed)



# create plots ----

# R
data_plot_model_four_nz_periods_R <- results_model_four_nz_periods %>%
  filter(period != "all") %>%
  dplyr::select(c(period, mutation_proba, testing_proba, R_estimate, R_lower_cred_int, R_upper_cred_int)) %>%
  mutate(offset_x_1 = rep(x = c(-0.15, 0, 0.15), each = 12),
         offset_x_2 = rep(rep(x = c(0.02, 0, -0.02), each = 4), times = 3)) %>%
  mutate(x = as.numeric(period) + offset_x_1 + offset_x_2)

plot_model_four_nz_periods_R <- ggplot(data = data_plot_model_four_nz_periods_R) +
  geom_point(aes(x = x, y = R_estimate, color = as.factor(testing_proba), shape = as.factor(mutation_proba))) +
  geom_errorbar(mapping = aes(x = x, y = R_estimate, ymin = R_lower_cred_int, ymax = R_upper_cred_int, color = as.factor(testing_proba)),
                linewidth = 0.5,
                width  = 0.02) +
  geom_hline(yintercept = 1, linetype = "dashed", color = "black", linewidth = 0.5) +
  xlab(label = NULL) +
  ylab(label = "R estimate") +
  scale_x_continuous(breaks = 1:4,
                     labels = c("Apr-May 2020", "Jun-Dec 2020", "Jan-Apr 2021", "May-Jul 2021")) +
  scale_y_continuous(breaks = c(0, 0.25, 0.5, 0.75, 1, 1.25),
                     limits = c(0, 1.3)) +
  scale_color_manual(name = "Testing probability",
                     breaks = sort(unique(data_plot_model_four_nz_periods_R$testing_proba)),
                     values = c("#002628", "#008A8A", "#95B1B0")) +
  scale_shape_manual(name = "Mutation probability",
                     breaks = sort(unique(data_plot_model_four_nz_periods_R$mutation_proba)),
                     values = c(17, 16, 15)) +
  theme_bw() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1),
        legend.position = "bottom",
        legend.box = "horizontal",
        legend.margin = margin(t = 0, r = 0, b = 0, l = 0, unit = "cm"),
        legend.key = element_rect(fill = "white"))

ggsave(plot = plot_model_four_nz_periods_R,
       filename = paste0("plots/new_zealand/model_four/plot_model_four_nz_periods_R.png"),
       width = 7, height = 7, units = c("in"))



# k
data_plot_model_four_nz_periods_k <- results_model_four_nz_periods %>%
  filter(period != "all") %>%
  dplyr::select(c(period, mutation_proba, testing_proba, k_estimate, k_lower_cred_int, k_upper_cred_int)) %>%
  mutate(offset_x_1 = rep(x = c(-0.15, 0, 0.15), each = 12),
         offset_x_2 = rep(rep(x = c(0.02, 0, -0.02), each = 4), times = 3)) %>%
  mutate(x = as.numeric(period) + offset_x_1 + offset_x_2)

plot_model_four_nz_periods_k <- ggplot(data = data_plot_model_four_nz_periods_k) +
  geom_point(aes(x = x, y = k_estimate, color = as.factor(testing_proba), shape = as.factor(mutation_proba))) +
  geom_errorbar(mapping = aes(x = x, y = k_estimate, ymin = k_lower_cred_int, ymax = k_upper_cred_int, color = as.factor(testing_proba)),
                linewidth = 0.5,
                width  = 0.02) +
  xlab(label = NULL) +
  ylab(label = "k estimate") +
  scale_x_continuous(breaks = 1:4,
                     labels = c("Apr-May 2020", "Jun-Dec 2020", "Jan-Apr 2021", "May-Jul 2021")) +
  scale_y_continuous(minor_breaks = seq(0.1, 2, by = 0.1),
                     breaks = c(0.1, 0.2, 0.3, 0.4, 0.5, 1, 1.5, 2),
                     limits = c(0.1, 2),
                     trans = "log2") +
  scale_color_manual(name = "Testing probability",
                     breaks = sort(unique(data_plot_model_four_nz_periods_k$testing_proba)),
                     values = c("#002628", "#008A8A", "#95B1B0")) +
  scale_shape_manual(name = "Mutation probability",
                     breaks = sort(unique(data_plot_model_four_nz_periods_R$mutation_proba)),
                     values = c(17, 16, 15)) +
  theme_bw() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1),
        legend.position = "bottom",
        legend.box = "horizontal",
        legend.margin = margin(t = 0, r = 0, b = 0, l = 0, unit = "cm"),
        legend.key = element_rect(fill = "white"))

ggsave(plot = plot_model_four_nz_periods_k,
       filename = paste0("plots/new_zealand/model_four/plot_model_four_nz_periods_k.png"),
       width = 7, height = 7, units = c("in"))


data_plot_model_four_nz_complete_period_k <- results_model_four_nz_periods %>%
  filter(period == "all") %>%
  dplyr::select(c(period, mutation_proba, testing_proba, k_estimate, k_lower_cred_int, k_upper_cred_int)) %>%
  mutate(x0 = rep(x = c(-0.15, 0, 0.15), each = 3),
         offset_x = rep(x = c(0.02, 0, -0.02), times = 3)) %>%
  mutate(x = x0 + offset_x)

plot_model_four_nz_complete_period_k <- ggplot(data = data_plot_model_four_nz_complete_period_k) +
  geom_point(aes(x = x, y = k_estimate, color = as.factor(testing_proba), shape = as.factor(mutation_proba))) +
  geom_errorbar(mapping = aes(x = x, y = k_estimate, ymin = k_lower_cred_int, ymax = k_upper_cred_int, color = as.factor(testing_proba)),
                linewidth = 0.5,
                width  = 0.02) +
  xlab(label = NULL) +
  ylab(label = "k estimate") +
  scale_x_continuous(breaks = 1:4,
                     labels = c("Apr-May 2020", "Jun-Dec 2020", "Jan-Apr 2021", "May-Jul 2021")) +
  scale_y_continuous(minor_breaks = seq(0.1, 2, by = 0.1),
                     breaks = c(0.1, 0.2, 0.3, 0.4, 0.5, 1, 1.5, 2),
                     limits = c(0.1, 2),
                     trans = "log2") +
  scale_color_manual(name = "Testing probability",
                     breaks = sort(unique(data_plot_model_four_nz_complete_period_k$testing_proba)),
                     values = c("#002628", "#008A8A", "#95B1B0")) +
  scale_shape_manual(name = "Mutation probability",
                     breaks = sort(unique(data_plot_model_four_nz_periods_R$mutation_proba)),
                     values = c(17, 16, 15)) +
  theme_bw() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1),
        legend.position = "bottom",
        legend.box = "horizontal",
        legend.margin = margin(t = 0, r = 0, b = 0, l = 0, unit = "cm"),
        legend.key = element_rect(fill = "white"))

ggsave(plot = plot_model_four_nz_complete_period_k,
       filename = paste0("plots/new_zealand/model_four/plot_model_four_nz_complete_period_k.png"),
       width = 7, height = 7, units = c("in"))


