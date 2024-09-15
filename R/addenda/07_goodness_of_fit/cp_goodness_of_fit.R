
# create plots of goodness of fit test

# read and process data ----

# read number of clusters of each size per country and month
data_cluster_sizes_ch_2021_months <- read_csv(file = path_data_cluster_sizes_ch_2021_months)
data_cluster_sizes_dk_2021_months <- read_csv(file = path_data_cluster_sizes_dk_2021_months)
data_cluster_sizes_de_2021_months <- read_csv(file = path_data_cluster_sizes_de_2021_months)

# read results of fitted probability distribution
data_distribution_mean_ch_dk_de_2021_months <- read_csv(file = path_results_goodness_fit_mean_model_one_ch_dk_de)
data_distribution_low_ch_dk_de_2021_months <- read_csv(file = path_results_goodness_fit_low_model_one_ch_dk_de)
data_distribution_high_ch_dk_de_2021_months <- read_csv(file = path_results_goodness_fit_high_model_one_ch_dk_de)

N_plot <- 100


# Switzerland ----
data_distribution_mean_ch_2021_months_plot <- data_distribution_mean_ch_dk_de_2021_months %>%
  filter(size <= N_plot, country == "Switzerland")
data_distribution_low_ch_2021_months_plot <- data_distribution_low_ch_dk_de_2021_months %>%
  filter(size <= N_plot, country == "Switzerland")
data_distribution_high_ch_2021_months_plot <- data_distribution_high_ch_dk_de_2021_months %>%
  filter(size <= N_plot, country == "Switzerland")

data_distribution_ch_2021_months_plot <- data_distribution_mean_ch_2021_months_plot %>%
  mutate(probability_low = data_distribution_low_ch_2021_months_plot$probability,
         probability_high = data_distribution_high_ch_2021_months_plot$probability)

data_cluster_sizes_ch_2021_months_plot <- data_cluster_sizes_ch_2021_months %>% filter(size <= N_plot)

plot_goodness_fit_ch <- ggplot() +
  geom_ribbon(data = data_distribution_ch_2021_months_plot,
              aes(x=size, ymin=probability_low, ymax=probability_high, fill="col_95_pred_int"), alpha=0.6) +
  geom_line(data = data_distribution_mean_ch_2021_months_plot,
            aes(x = size, y = probability, group = month, color = "col_mean"), linewidth = 1) +
  geom_point(data = data_cluster_sizes_ch_2021_months_plot,
             aes(x = size, y = percentage, color = "col_data")) +
  scale_color_manual(name = NULL,
                     breaks = c("col_data", "col_mean"),
                     values = c("black", "dodgerblue2"),
                     labels = c("Data", "Probability density function based on mean estimates")) +
  scale_fill_manual(name = NULL,
                    breaks = c("col_95_pred_int"),
                    values = c("lightskyblue"),
                    labels = c("Range of probability density functions based on 2.5% and 97.5% quantile estimates")) +
  scale_x_continuous(name = "Cluster size",
                     limits = c(1, N_plot)) +
  scale_y_continuous(name = "Probability",
                     breaks = c(1e-10, 1e-9, 1e-8, 1e-7, 1e-6, 1e-5, 1e-4, 0.001, 0.01, 0.1, 1),
                     limits = c(min(data_distribution_mean_ch_2021_months_plot$probability,
                                    data_distribution_low_ch_2021_months_plot$probability,
                                    data_distribution_high_ch_2021_months_plot$probability,
                                    data_cluster_sizes_ch_2021_months_plot$percentage) / sqrt(2), 1),
                     trans = "log2")  +
  facet_wrap(facets = vars(factor(month, levels = 1:12)), 
             scales = "free",
             nrow = 4,
             ncol = 3,
             labeller = as_labeller(c("1" = "January", "2" = "February", "3" = "March", "4" = "April",
                                      "5" = "May", "6" = "June", "7" = "July", "8" = "August",
                                      "9" = "September", "10" = "October", "11" = "November", "12" = "December"))) +
  theme_bw() +
  theme(legend.position = "bottom",
        legend.box = "vertical") +
  guides(colour = guide_legend(order = 1), 
         fill = guide_legend(order = 2))



# Denmark ----

data_distribution_mean_dk_2021_months_plot <- data_distribution_mean_ch_dk_de_2021_months %>%
  filter(size <= N_plot, country == "Denmark")
data_distribution_low_dk_2021_months_plot <- data_distribution_low_ch_dk_de_2021_months %>%
  filter(size <= N_plot, country == "Denmark")
data_distribution_high_dk_2021_months_plot <- data_distribution_high_ch_dk_de_2021_months %>%
  filter(size <= N_plot, country == "Denmark")

data_distribution_dk_2021_months_plot <- data_distribution_mean_dk_2021_months_plot %>%
  mutate(probability_low = data_distribution_low_dk_2021_months_plot$probability,
         probability_high = data_distribution_high_dk_2021_months_plot$probability)

data_cluster_sizes_dk_2021_months_plot <- data_cluster_sizes_dk_2021_months %>% filter(size <= N_plot)

plot_goodness_fit_dk <- ggplot() +
  geom_ribbon(data = data_distribution_dk_2021_months_plot,
              aes(x=size, ymin=probability_low, ymax=probability_high, fill="col_95_pred_int"), alpha=0.6) +
  geom_line(data = data_distribution_mean_dk_2021_months_plot,
            aes(x = size, y = probability, group = month, color = "col_mean"), linewidth = 1) +
  geom_point(data = data_cluster_sizes_dk_2021_months_plot,
             aes(x = size, y = percentage, color = "col_data")) +
  scale_color_manual(name = NULL,
                     breaks = c("col_data", "col_mean"),
                     values = c("black", "dodgerblue2"),
                     labels = c("Data", "Probability density function based on mean estimates")) +
  scale_fill_manual(name = NULL,
                    breaks = c("col_95_pred_int"),
                    values = c("lightskyblue"),
                    labels = c("Range of probability density functions based on 2.5% and 97.5% quantile estimates")) +
  scale_x_continuous(name = "Cluster size",
                     limits = c(1, N_plot)) +
  scale_y_continuous(name = "Probability",
                     breaks = c(1e-10, 1e-9, 1e-8, 1e-7, 1e-6, 1e-5, 1e-4, 0.001, 0.01, 0.1, 1),
                     limits = c(min(data_distribution_mean_dk_2021_months_plot$probability,
                                    data_distribution_low_dk_2021_months_plot$probability,
                                    data_distribution_high_dk_2021_months_plot$probability,
                                    data_cluster_sizes_dk_2021_months_plot$percentage) / sqrt(2), 1),
                     trans = "log2")  +
  facet_wrap(facets = vars(factor(month, levels = 1:12)), 
             scales = "free",
             nrow = 4,
             ncol = 3,
             labeller = as_labeller(c("1" = "January", "2" = "February", "3" = "March", "4" = "April",
                                      "5" = "May", "6" = "June", "7" = "July", "8" = "August",
                                      "9" = "September", "10" = "October", "11" = "November", "12" = "December"))) +
  theme_bw() +
  theme(legend.position = "bottom",
        legend.box = "vertical") +
  guides(colour = guide_legend(order = 1), 
         fill = guide_legend(order = 2))



# Germany ----

data_distribution_mean_de_2021_months_plot <- data_distribution_mean_ch_dk_de_2021_months %>%
  filter(size <= N_plot, country == "Germany")
data_distribution_low_de_2021_months_plot <- data_distribution_low_ch_dk_de_2021_months %>%
  filter(size <= N_plot, country == "Germany")
data_distribution_high_de_2021_months_plot <- data_distribution_high_ch_dk_de_2021_months %>%
  filter(size <= N_plot, country == "Germany")

data_distribution_de_2021_months_plot <- data_distribution_mean_de_2021_months_plot %>%
  mutate(probability_low = data_distribution_low_de_2021_months_plot$probability,
         probability_high = data_distribution_high_de_2021_months_plot$probability)

data_cluster_sizes_de_2021_months_plot <- data_cluster_sizes_de_2021_months %>% filter(size <= N_plot)

plot_goodness_fit_de <- ggplot() +
  geom_ribbon(data = data_distribution_de_2021_months_plot,
              aes(x=size, ymin=probability_low, ymax=probability_high, fill="col_95_pred_int"), alpha=0.6) +
  geom_line(data = data_distribution_mean_de_2021_months_plot,
            aes(x = size, y = probability, group = month, color = "col_mean"), linewidth = 1) +
  geom_point(data = data_cluster_sizes_de_2021_months_plot,
             aes(x = size, y = percentage, color = "col_data")) +
  scale_color_manual(name = NULL,
                     breaks = c("col_data", "col_mean"),
                     values = c("black", "dodgerblue2"),
                     labels = c("Data", "Probability density function based on mean estimates")) +
  scale_fill_manual(name = NULL,
                    breaks = c("col_95_pred_int"),
                    values = c("lightskyblue"),
                    labels = c("Range of probability density functions based on 2.5% and 97.5% quantile estimates")) +
  scale_x_continuous(name = "Cluster size",
                     limits = c(1, N_plot)) +
  scale_y_continuous(name = "Probability",
                     breaks = c(1e-10, 1e-9, 1e-8, 1e-7, 1e-6, 1e-5, 1e-4, 0.001, 0.01, 0.1, 1),
                     limits = c(min(data_distribution_mean_de_2021_months_plot$probability,
                                    data_distribution_low_de_2021_months_plot$probability,
                                    data_distribution_high_de_2021_months_plot$probability,
                                    data_cluster_sizes_de_2021_months_plot$percentage) / sqrt(2), 1),
                     trans = "log2")  +
  facet_wrap(facets = vars(factor(month, levels = 1:12)), 
             scales = "free",
             nrow = 4,
             ncol = 3,
             labeller = as_labeller(c("1" = "January", "2" = "February", "3" = "March", "4" = "April",
                                      "5" = "May", "6" = "June", "7" = "July", "8" = "August",
                                      "9" = "September", "10" = "October", "11" = "November", "12" = "December"))) +
  theme_bw() +
  theme(legend.position = "bottom",
        legend.box = "vertical") +
  guides(colour = guide_legend(order = 1), 
         fill = guide_legend(order = 2)) 

