


sim_input_max_cluster_size <- 2500

simulated_clusters_0 <- estRodis_simulate_cluster_sizes(
  n_clusters = 3000,
  max_cluster_size = sim_input_max_cluster_size,
  R = 1.5,
  k = 0.5,
  yearly_mutation_rate = 14,
  mean_generation_interval = 5.2,
  testing_proba = 0.8,
  sequencing_proba = 0.4
)

N <- sim_input_max_cluster_size - 1

cluster_size_distribution_0 <- distribution_cluster_size_testing(N,
                                                                 Re = 1.5,
                                                                 k = 0.5,
                                                                 p_mut = 1 - exp(-14*5.2/365.25),
                                                                 p_detec = 0.8*0.4)

cluster_size_distribution <- tibble(size = 1:sim_input_max_cluster_size,
                                    probability = c(cluster_size_distribution_0, 1 - sum(cluster_size_distribution_0)))

simulated_clusters <- simulated_clusters_0 %>%
  left_join(cluster_size_distribution, by = "size")



N <- 10000

N_plot <- 500

plot_different_Re <- ggplot() +
  geom_line(aes(x = 1:N_plot, y = distribution_cluster_size_testing(800, 0.6, 0.3, 0.3, 0.7)[1:N_plot]), color = "red") +
  geom_line(aes(x = 1:N_plot, y = distribution_cluster_size_testing(800, 1.2, 0.3, 0.3, 0.7)[1:N_plot]), color = "blue") +
  geom_line(aes(x = 1:N_plot, y = distribution_cluster_size_testing(800, 1.8, 0.3, 0.3, 0.7)[1:N_plot]), color = "green") +
  scale_x_continuous(name = "Cluster size") +
  scale_y_continuous(name = "Probability",
                     trans = "log10") +
  ggtitle("Different effective reproduction number") +
  theme_bw()

plot_different_k <- ggplot() +
  geom_line(aes(x = 1:N_plot, y = distribution_cluster_size_testing(800, 1.2, 0.01, 0.3, 0.7)[1:N_plot]), color = "red") +
  geom_line(aes(x = 1:N_plot, y = distribution_cluster_size_testing(800, 1.2, 0.3, 0.3, 0.7)[1:N_plot]), color = "blue") +
  geom_line(aes(x = 1:N_plot, y = distribution_cluster_size_testing(800, 1.2, 1, 0.3, 0.7)[1:N_plot]), color = "green") +
  scale_x_continuous(name = "Cluster size") +
  scale_y_continuous(name = "Probability",
                     trans = "log10") +
  ggtitle("Different dispersion number") +
  theme_bw()

plot_different_p_mut <- ggplot() +
  geom_line(aes(x = 1:N_plot, y = distribution_cluster_size_testing(800, 1.2, 0.3, 0.1, 0.7)[1:N_plot]), color = "red") +
  geom_line(aes(x = 1:N_plot, y = distribution_cluster_size_testing(800, 1.2, 0.3, 0.3, 0.7)[1:N_plot]), color = "blue") +
  geom_line(aes(x = 1:N_plot, y = distribution_cluster_size_testing(800, 1.2, 0.3, 0.5, 0.7)[1:N_plot]), color = "green") +
  scale_x_continuous(name = "Cluster size") +
  scale_y_continuous(name = "Probability",
                     trans = "log10") +
  ggtitle("Different mutation probability") +
  theme_bw()

plot_different_p_detec <- ggplot() +
  geom_line(aes(x = 1:N_plot, y = distribution_cluster_size_testing(800, 1.2, 0.3, 0.3, 0.1)[1:N_plot]), color = "red") +
  geom_line(aes(x = 1:N_plot, y = distribution_cluster_size_testing(800, 1.2, 0.3, 0.3, 0.5)[1:N_plot]), color = "blue") +
  geom_line(aes(x = 1:N_plot, y = distribution_cluster_size_testing(800, 1.2, 0.3, 0.3, 0.9)[1:N_plot]), color = "green") +
  scale_x_continuous(name = "Cluster size") +
  scale_y_continuous(name = "Probability",
                     trans = "log10") +
  ggtitle("Different detection probability") +
  theme_bw()

plot_grid_different_input <- plot_grid(plot_different_Re, plot_different_k, plot_different_p_mut, plot_different_p_detec,
                                       nrow = 2)


N_plot <- 5

plot_different_Re_a <- ggplot() +
  geom_line(aes(x = 1:N_plot, y = distribution_cluster_size_testing(800, 0.6, 0.3, 0.3, 0.7)[1:N_plot]), color = "red") +
  geom_line(aes(x = 1:N_plot, y = distribution_cluster_size_testing(800, 1.2, 0.3, 0.3, 0.7)[1:N_plot]), color = "blue") +
  geom_line(aes(x = 1:N_plot, y = distribution_cluster_size_testing(800, 1.8, 0.3, 0.3, 0.7)[1:N_plot]), color = "green") +
  scale_x_continuous(name = "Cluster size") +
  scale_y_continuous(name = "Probability",
                     limits = c(0,1)) +
  ggtitle("Different effective reproduction number") +
  theme_bw()

plot_different_k_a <- ggplot() +
  geom_line(aes(x = 1:N_plot, y = distribution_cluster_size_testing(800, 1.2, 0.01, 0.3, 0.7)[1:N_plot]), color = "red") +
  geom_line(aes(x = 1:N_plot, y = distribution_cluster_size_testing(800, 1.2, 0.3, 0.3, 0.7)[1:N_plot]), color = "blue") +
  geom_line(aes(x = 1:N_plot, y = distribution_cluster_size_testing(800, 1.2, 1, 0.3, 0.7)[1:N_plot]), color = "green") +
  scale_x_continuous(name = "Cluster size") +
  scale_y_continuous(name = "Probability",
                     limits = c(0,1)) +
  ggtitle("Different dispersion number") +
  theme_bw()

plot_different_p_mut_a <- ggplot() +
  geom_line(aes(x = 1:N_plot, y = distribution_cluster_size_testing(800, 1.2, 0.3, 0.1, 0.7)[1:N_plot]), color = "red") +
  geom_line(aes(x = 1:N_plot, y = distribution_cluster_size_testing(800, 1.2, 0.3, 0.3, 0.7)[1:N_plot]), color = "blue") +
  geom_line(aes(x = 1:N_plot, y = distribution_cluster_size_testing(800, 1.2, 0.3, 0.5, 0.7)[1:N_plot]), color = "green") +
  scale_x_continuous(name = "Cluster size") +
  scale_y_continuous(name = "Probability",
                     limits = c(0,1)) +
  ggtitle("Different mutation probability") +
  theme_bw()

plot_different_p_detec_a <- ggplot() +
  geom_line(aes(x = 1:N_plot, y = distribution_cluster_size_testing(800, 1.2, 0.3, 0.3, 0.1)[1:N_plot]), color = "red") +
  geom_line(aes(x = 1:N_plot, y = distribution_cluster_size_testing(800, 1.2, 0.3, 0.3, 0.5)[1:N_plot]), color = "blue") +
  geom_line(aes(x = 1:N_plot, y = distribution_cluster_size_testing(800, 1.2, 0.3, 0.3, 0.9)[1:N_plot]), color = "green") +
  scale_x_continuous(name = "Cluster size") +
  scale_y_continuous(name = "Probability",
                     limits = c(0,1)) +
  ggtitle("Different detection probability") +
  theme_bw()

plot_grid_different_input_a <- plot_grid(plot_different_Re_a, plot_different_k_a, plot_different_p_mut_a, plot_different_p_detec_a,
                                         nrow = 2)


