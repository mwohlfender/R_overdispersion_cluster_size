
# create plots of prior distributions of R, k and p_test

# effective reproduction number R ----

prior_R_gamma_shape <- 10
prior_R_gamma_rate <- 10

plot_prior_R <- ggplot(data = data.frame(x = c(0, 3)), mapping = aes(x = x)) +
  stat_function(fun = dgamma, n = 3000, args = list(shape = prior_R_gamma_shape, rate = prior_R_gamma_rate), xlim = c(0, 3), linewidth = 1.5) + 
  scale_x_continuous(breaks = seq(from = 0, to = 3, by = 0.5),
                     labels = seq(from = 0, to = 3, by = 0.5),
                     limits = c(-0.001, 3.001)) +
  scale_y_continuous(breaks = seq(from = 0, to = 2, by = 0.2),
                     labels = seq(from = 0, to = 2, by = 0.2),
                     limits = c(-0.001, 2.001)) +
  theme_bw() + 
  theme(axis.title.x = element_blank(), 
        axis.title.y = element_blank())


# plot_prior_R_title <- plot_prior_R +
#   ggtitle(label = paste0("Prior distribution of R: Gamma(", prior_R_gamma_shape, ",", prior_R_gamma_rate, ")")) +
#   theme(plot.caption = element_text(hjust = 0)) +
#   labs(caption = "Figure SX: Prior distribution of the effective reproduction number.")



# dispersion parameter k ----

prior_k_gamma_shape <- 5
prior_k_gamma_rate <- 10

plot_prior_k <- ggplot(data = data.frame(x = c(0, 3)), mapping = aes(x = x)) +
  stat_function(fun = dgamma, n = 3000, args = list(shape = prior_k_gamma_shape, rate = prior_k_gamma_rate), xlim = c(0, 3), linewidth = 1.5) + 
  scale_x_continuous(breaks = seq(from = 0, to = 3, by = 0.5),
                     labels = seq(from = 0, to = 3, by = 0.5),
                     limits = c(-0.001, 3.001)) +
  scale_y_continuous(breaks = seq(from = 0, to = 2, by = 0.2),
                     labels = seq(from = 0, to = 2, by = 0.2),
                     limits = c(-0.001, 2.001)) +
  theme_bw() + 
  theme(axis.title.x = element_blank(), 
        axis.title.y = element_blank())


# plot_prior_k_title <- plot_prior_k +
#   ggtitle(label = paste0("Prior distribution of k: Gamma(", prior_k_gamma_shape, ",", prior_k_gamma_rate, ")")) +
#   theme(plot.caption = element_text(hjust = 0)) +
#   labs(caption = "Figure SX: Prior distribution of the dispersion parameter.")



# yearly mutation rate ----

prior_mut_normal_mean <- 14
prior_mut_normal_sd <- 0.5

plot_prior_mut_rate <- ggplot(data = data.frame(x = c(11, 17)), mapping = aes(x = x)) +
  stat_function(fun = dnorm, n = 3000, args = list(mean = prior_mut_normal_mean, sd = prior_mut_normal_sd), xlim = c(11, 17), linewidth = 1.5) + 
  scale_x_continuous(breaks = seq(from = 11, to = 17, by = 1),
                     labels = seq(from = 11, to = 17, by = 1),
                     limits = c(10.999, 17.001)) +
  scale_y_continuous(breaks = seq(from = 0, to = 1, by = 0.2),
                     labels = seq(from = 0, to = 1, by = 0.2),
                     limits = c(-0.001, 1.001)) +
  theme_bw() + 
  theme(axis.title.x = element_blank(), 
        axis.title.y = element_blank())


# plot_prior_mut_rate_title <- plot_prior_mut_rate +
#   ggtitle(label = paste0("Prior distribution of yearly mutation rate: Normal(", prior_mut_normal_mean, ",", prior_mut_normal_sd, ")")) +
#   theme(plot.caption = element_text(hjust = 0)) +
#   labs(caption = "Figure SX: Prior distribution of the yearly mutation rate.")



# mutation probability ----

prior_p_mut_alpha <- 27
prior_p_mut_beta <- 68

plot_prior_p_mut <- ggplot(data = data.frame(x = c(0, 1)), mapping = aes(x = x)) +
  stat_function(fun = dbeta, n = 3000, args = list(shape1 = prior_p_mut_alpha, shape2 = prior_p_mut_beta), xlim = c(0, 1), linewidth = 1.5) + 
  scale_x_continuous(breaks = seq(from = 0, to = 1, by = 0.25),
                     labels = seq(from = 0, to = 1, by = 0.25),
                     limits = c(-0.001, 1.001)) +
  scale_y_continuous(breaks = seq(from = 0, to = 10, by = 2),
                     labels = seq(from = 0, to = 10, by = 2),
                     limits = c(-0.001, 10.001)) +
  theme_bw() + 
  theme(axis.title.x = element_blank(), 
        axis.title.y = element_blank())



# testing probability ----

prior_p_test_alpha <- 1
prior_p_test_beta <- 3
prior_p_test_p <- 0.05
prior_p_test_q <- 1

plot_prior_p_test <- ggplot(data = data.frame(x = c(0, 1)), mapping = aes(x = x)) +
  stat_function(fun = estRodis_scaled_beta_distribution_pdf, n = 3000, args = list(alpha = prior_p_test_alpha, beta  = prior_p_test_beta, p = prior_p_test_p, q = prior_p_test_q), xlim = c(0, 1), linewidth = 1.5) + 
  scale_x_continuous(breaks = seq(from = 0, to = 1, by = 0.1),
                     labels = seq(from = 0, to = 1, by = 0.1),
                     limits = c(-0.001, 1.001)) +
  scale_y_continuous(breaks = seq(from = 0, to = 4, by = 0.5),
                     labels = seq(from = 0, to = 4, by = 0.5),
                     limits = c(-0.001, 4.001)) +
  theme_bw() + 
  theme(axis.title.x = element_blank(), 
        axis.title.y = element_blank())


# plot_prior_p_test_title <- plot_prior_p_test +
#   ggtitle(label = paste0("Prior distribution of p_test: ScaledBeta(", prior_p_test_alpha, ",", prior_p_test_beta, ") on [", prior_p_test_p, ",", prior_p_test_q,"]")) +
#   theme(plot.caption = element_text(hjust = 0)) +
#   labs(caption = "Figure SX: Prior distribution of the testing probability.")



# create plot grid ----
plot_grid_prior_distributions <- plot_grid(plotlist = list(plot_prior_R + theme(plot.margin = margin(10, 10, 0, 10)), 
                                                           plot_prior_k + theme(plot.margin = margin(10, 10, 0, 10)), 
                                                           plot_prior_mut_rate + theme(plot.margin = margin(10, 10, 10, 10)), 
                                                           plot_prior_p_test + theme(plot.margin = margin(10, 10, 10, 10))),
                                           rel_widths = c(1, 1, 1, 1),
                                           rel_heights = c(1, 1, 1, 1),
                                           labels = c("A", "B", "C", "D"))

ggsave(plot = plot_grid_prior_distributions, filename = "plots/prior_distributions/grid_prior_distributions.png", width = 6, height = 6, units = c("in"))


plot_grid_prior_distributions_a <- plot_grid(plotlist = list(plot_prior_R + theme(plot.margin = margin(10, 10, 0, 10)), 
                                                             plot_prior_k + theme(plot.margin = margin(10, 10, 0, 10)), 
                                                             plot_prior_p_mut + theme(plot.margin = margin(10, 10, 10, 10)), 
                                                             plot_prior_p_test + theme(plot.margin = margin(10, 10, 10, 10))),
                                             rel_widths = c(1, 1, 1, 1),
                                             rel_heights = c(1, 1, 1, 1),
                                             labels = c("A", "B", "C", "D"))

ggsave(plot = plot_grid_prior_distributions_a, filename = "plots/prior_distributions/grid_prior_distributions_a.png", width = 6, height = 6, units = c("in"))


plot_grid_prior_distributions_all <- plot_grid(plotlist = list(plot_prior_R + theme(plot.margin = margin(10, 10, 0, 10)), 
                                                               plot_prior_k + theme(plot.margin = margin(10, 10, 0, 10)), 
                                                               plot_prior_p_mut + theme(plot.margin = margin(10, 10, 0, 10)),
                                                               plot_prior_mut_rate + theme(plot.margin = margin(10, 10, 0, 10)), 
                                                               plot_prior_p_test + theme(plot.margin = margin(10, 10, 10, 10))),
                                               nrow = 3,
                                               rel_widths = c(1, 1, 1, 1, 1),
                                               rel_heights = c(1, 1, 1, 1, 1),
                                               labels = c("A", "B", "C", "D", "E"))

ggsave(plot = plot_grid_prior_distributions_all, filename = "plots/prior_distributions/grid_prior_distributions_all.png", width = 6, height = 6, units = c("in"))

