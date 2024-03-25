
# simulation study
# create results of simulation study (model one)



# define parameters for plots
ratio_width_height <- 4/3
image_width_in <- 7.3


# read data ----

# parameter grid for simulation study
data_sim_parameters_grid <- read_csv(path_data_sim_parameters_grid)

# determine range of parameters contained in `data_sim_parameters_grid`
R_range <- sort(unique(data_sim_parameters_grid$R))
k_range <- sort(unique(data_sim_parameters_grid$k))
yearly_mutation_rate_range <- sort(unique(data_sim_parameters_grid$yearly_mutation_rate))
mean_generation_interval_range <- sort(unique(data_sim_parameters_grid$mean_generation_interval))
testing_proba_range <- sort(unique(data_sim_parameters_grid$testing_proba))
sequencing_proba_range <- sort(unique(data_sim_parameters_grid$sequencing_proba))
n_clusters_range <- sort(unique(data_sim_parameters_grid$n_clusters))
max_cluster_size_range <- sort(unique(data_sim_parameters_grid$max_cluster_size))

# results of parameter estimations
results <- read_csv(path_results_sim_processed)



# process data ----

# `results:` add column `t_total` (sum of columns `t_w_max` and `t_s_max`)
results <- results %>% mutate(t_total = t_w_max + t_s_max)

# bind together the data frames `grid_parameters` and `results`
data_results_0 <- cbind(data_sim_parameters_grid, results)

# `data_results_0`:
# (a) filter to rows were a result was obtained (t_total > 0)
# (b) filter to rows without divergent transitions
# (c) filter to rows with all Rhat values in interval [0.95, 1.05]
# (d) determine whether true values of R, k and testing probability are contained in the 95% credibility interval
data_results <- data_results_0 %>%
  filter(t_total > 0) %>%
  filter(n_divergent_transitions == 0) %>%
  filter(max_Rhat <= 1.05) %>%
  filter(min_Rhat >= 0.95) %>%
  mutate(R_in_ci = (R_lower_cred_int  <= R & R <= R_upper_cred_int),
         k_in_ci = (k_lower_cred_int  <= k & k <= k_upper_cred_int),
         testing_proba_in_ci = (testing_proba_lower_cred_int  <= testing_proba & testing_proba <= testing_proba_upper_cred_int))

# `data_results_0`:
# (a) filter to rows were a result was obtained (t_total > 0)
# (b) filter to rows with either
# (b1) at least one divergent transition
# (b2) one Rhat value below 0.95
# (b3) one Rhat value above 1.05
data_results_failed <- data_results_0  %>%
  filter(t_total > 0) %>%
  filter(n_divergent_transitions >= 1 | max_Rhat > 1.05 | min_Rhat < 0.95) 
 
summary_results <- tibble(total = nrow(data_results_0),
                          success = nrow(data_results),
                          failed = nrow(data_results_failed),
                          not_finished = nrow(data_results_0 %>% filter(t_total == 0)))


data_plots <- expand.grid(R = R_range,
                          k = k_range,
                          yearly_mutation_rate = yearly_mutation_rate_range,
                          mean_generation_interval = mean_generation_interval_range,
                          testing_proba = testing_proba_range,
                          sequencing_proba = sequencing_proba_range,
                          n_clusters = n_clusters_range,
                          max_cluster_size = max_cluster_size_range)

n_runs_per_param_combination <- nrow(data_sim_parameters_grid) / nrow(data_plots)

data_plots <- data_plots %>%
  mutate(rmse_R = -1,
         rmse_k = -1,
         rmse_testing_proba = -1,
         R_in_ci = 0,
         k_in_ci = 0,
         testing_proba_in_ci = 0,
         n_estimates = 0,
         mean_t_total = 0)

for (ii in 1:nrow(data_plots)) {
  
  data_results_filtered <- data_results %>% filter(R == data_plots$R[ii], 
                                                   k == data_plots$k[ii],
                                                   yearly_mutation_rate == data_plots$yearly_mutation_rate[ii],
                                                   mean_generation_interval == data_plots$mean_generation_interval[ii],
                                                   testing_proba == data_plots$testing_proba[ii],
                                                   sequencing_proba == data_plots$sequencing_proba[ii],
                                                   n_clusters == data_plots$n_clusters[ii],
                                                   max_cluster_size == data_plots$max_cluster_size[ii])
  
  if (nrow(data_results_filtered) >= 1) {
    
    data_plots$rmse_R[ii] <- Metrics::rmse(data_results_filtered$R_estimate, data_results_filtered$R) 
    data_plots$rmse_k[ii] <- Metrics::rmse(data_results_filtered$k_estimate, data_results_filtered$k)
    data_plots$rmse_testing_proba[ii] <- Metrics::rmse(data_results_filtered$testing_proba_estimate, data_results_filtered$testing_proba) 
    data_plots$R_in_ci[ii] <- sum(data_results_filtered$R_in_ci) / nrow(data_results_filtered)
    data_plots$k_in_ci[ii] <- sum(data_results_filtered$k_in_ci) / nrow(data_results_filtered)
    data_plots$testing_proba_in_ci[ii] <- sum(data_results_filtered$testing_proba_in_ci) / nrow(data_results_filtered)
    data_plots$n_estimates[ii] <- nrow(data_results_filtered)
    data_plots$mean_t_total[ii] <- mean(data_results_filtered$t_total)
    
  }
  
}



# create plots ----
for (ii in 1:length(n_clusters_range)) {
  
  for (jj in 1:length(max_cluster_size_range)) {
    
    data_plot_completed_estimates_temp <- data_plots %>%
      filter(rmse_R >= 0) %>%
      filter(rmse_k >= 0) %>%
      filter(rmse_testing_proba >= 0) %>%
      filter(n_clusters == n_clusters_range[ii]) %>%
      filter(max_cluster_size == max_cluster_size_range[jj])
    
    data_plot_all_estimates_temp <- data_plots %>%
      filter(n_clusters == n_clusters_range[ii]) %>%
      filter(max_cluster_size == max_cluster_size_range[jj])
    
    # facet grid of raster plots of root mean square error of estimate of R
    # local axes: sequencing probability and testing probability
    # global axes: R and k
    
    plot_raster_R_rmse <- ggplot(data = data_plot_completed_estimates_temp) +
      geom_raster(aes(x = factor(sequencing_proba), y = factor(testing_proba), fill = rmse_R)) +
      ggtitle("Effective reproduction number") +
      xlab("sequencing probability") +
      ylab("testing probability") +
      facet_grid(k~R, labeller = label_both) +
      scale_fill_viridis_c(name = "RMSE") +
      theme_bw() +
      theme(legend.position="bottom")
    
    ggsave(plot = plot_raster_R_rmse,
           filename = paste0("plots/simulation/plot_raster_R_rmse_", n_clusters_range[ii], "_", max_cluster_size_range[jj], ".png"),
           width = image_width_in, height = image_width_in/ratio_width_height, units = c("in"))
    
    # facet grid of raster plots of coverage of true value of R by estimated credible intervals
    # local axes: sequencing probability and testing probability
    # global axes: R and k
    
    plot_raster_R_coverage <- ggplot(data = data_plot_completed_estimates_temp) +
      geom_raster(aes(x = factor(sequencing_proba), y = factor(testing_proba), fill = R_in_ci)) +
      ggtitle("Effective reproduction number") +
      xlab("sequencing probability") +
      ylab("testing probability") +
      facet_grid(k~R, labeller = label_both) +
      scale_fill_viridis_c(name = "coverage") +
      theme_bw() +
      theme(legend.position="bottom")
    
    ggsave(plot = plot_raster_R_coverage,
           filename = paste0("plots/simulation/plot_raster_R_coverage_", n_clusters_range[ii], "_", max_cluster_size_range[jj], ".png"),
           width = image_width_in, height = image_width_in/ratio_width_height, units = c("in"))
    
    # facet grid of raster plots of root mean square error of estimate of k
    # local axes: sequencing probability and testing probability
    # global axes: R and k
    
    plot_raster_k_rmse <- ggplot(data = data_plot_completed_estimates_temp) +
      # geom_raster(aes(x = factor(sequencing_proba), y = factor(testing_proba), fill = rmse_k)) +
      geom_raster(aes(x = factor(sequencing_proba), y = factor(testing_proba), fill = log(rmse_k, base = 10))) +
      ggtitle("Dispersion parameter") +
      xlab("sequencing probability") +
      ylab("testing probability") +
      facet_grid(k~R, labeller = label_both) +
      # scale_fill_viridis_c(name = "RMSE (k)")
      scale_fill_viridis_c(name = expression(paste(log[10], ~ "(RMSE)"))) +
      theme_bw() +
      theme(legend.position="bottom")
    
    ggsave(plot = plot_raster_k_rmse,
           filename = paste0("plots/simulation/plot_raster_k_rmse_", n_clusters_range[ii], "_", max_cluster_size_range[jj], ".png"),
           width = image_width_in, height = image_width_in/ratio_width_height, units = c("in"))
    
    # facet grid of raster plots of coverage of true value of R by estimated credible intervals
    # local axes: sequencing probability and testing probability
    # global axes: R and k
    
    plot_raster_k_coverage <- ggplot(data = data_plot_completed_estimates_temp) +
      geom_raster(aes(x = factor(sequencing_proba), y = factor(testing_proba), fill = k_in_ci)) +
      ggtitle("Dispersion parameter") +
      xlab("sequencing probability") +
      ylab("testing probability") +
      facet_grid(k~R, labeller = label_both) +
      scale_fill_viridis_c(name = "coverage") +
      theme_bw() +
      theme(legend.position="bottom")
    
    ggsave(plot = plot_raster_k_coverage,
           filename = paste0("plots/simulation/plot_raster_k_coverage_", n_clusters_range[ii], "_", max_cluster_size_range[jj], ".png"),
           width = image_width_in, height = image_width_in/ratio_width_height, units = c("in"))
    
    # facet grid of raster plots of root mean square error of estimate of testing probability
    # local axes: sequencing probability and testing probability
    # global axes: R and k
    
    plot_raster_testing_proba_rmse <- ggplot(data = data_plot_completed_estimates_temp) +
      geom_raster(aes(x = factor(sequencing_proba), y = factor(testing_proba), fill = rmse_testing_proba)) +
      ggtitle("Testing probability") +
      xlab("sequencing probability") +
      ylab("testing probability") +
      facet_grid(k~R, labeller = label_both) +
      scale_fill_viridis_c(name = "RMSE") +
      theme_bw() +
      theme(legend.position="bottom")
    
    ggsave(plot = plot_raster_testing_proba_rmse,
           filename = paste0("plots/simulation/plot_raster_testing_proba_rmse_", n_clusters_range[ii], "_", max_cluster_size_range[jj], ".png"),
           width = image_width_in, height = image_width_in/ratio_width_height, units = c("in"))
    
    # facet grid of raster plots of coverage of true value of testing probability by estimated credible intervals
    # local axes: sequencing probability and testing probability
    # global axes: R and k
    
    plot_raster_testing_proba_coverage <- ggplot(data = data_plot_completed_estimates_temp) +
      geom_raster(aes(x = factor(sequencing_proba), y = factor(testing_proba), fill = testing_proba_in_ci)) +
      ggtitle("Testing probability") +
      xlab("sequencing probability") +
      ylab("testing probability") +
      facet_grid(k~R, labeller = label_both) +
      scale_fill_viridis_c(name = "coverage") +
      theme_bw() +
      theme(legend.position="bottom")
    
    ggsave(plot = plot_raster_testing_proba_coverage,
           filename = paste0("plots/simulation/plot_raster_testing_proba_coverage_", n_clusters_range[ii], "_", max_cluster_size_range[jj], ".png"),
           width = image_width_in, height = image_width_in/ratio_width_height, units = c("in"))
    
    # facet grid of raster plots of number of successful parameter estimates per parameter combination
    # local axes: sequencing probability and testing probability
    # global axes: R and k
    
    plot_raster_n_estimates <- ggplot(data = data_plot_all_estimates_temp) +
      geom_raster(aes(x = factor(sequencing_proba), y = factor(testing_proba), fill = n_estimates/n_runs_per_param_combination)) +
      ggtitle("Number of estimates") +
      xlab("sequencing probability") +
      ylab("testing probability") +
      facet_grid(k~R, labeller = label_both) +
      scale_fill_viridis_c(name = "successful estimates") +
      theme_bw() +
      theme(legend.position="bottom")
    
    ggsave(plot = plot_raster_n_estimates,
           filename = paste0("plots/simulation/plot_raster_n_estimates_", n_clusters_range[ii], "_", max_cluster_size_range[jj], ".png"),
           width = image_width_in, height = image_width_in/ratio_width_height, units = c("in"))
    
    # facet grid of raster plots of total run time
    # local axes: sequencing probability and testing probability
    # global axes: R and k
    
    plot_raster_run_time <- ggplot(data = data_plot_completed_estimates_temp) +
      geom_raster(aes(x = factor(sequencing_proba), y = factor(testing_proba), fill = mean_t_total)) +
      ggtitle("Run time") +
      xlab("sequencing probability") +
      ylab("testing probability") +
      facet_grid(k~R, labeller = label_both) +
      scale_fill_viridis_c(name = "run time") +
      theme_bw()
    
    ggsave(plot = plot_raster_run_time,
           filename = paste0("plots/simulation/plot_raster_run_time_", n_clusters_range[ii], "_", max_cluster_size_range[jj], ".png"),
           width = image_width_in, height = image_width_in/ratio_width_height, units = c("in"))
    
    # plot grid of `plot_raster_R_rmse`, `plot_raster_k_rmse` and `plot_raster_testing_proba_rmse`
    
    plot_raster_R_k_testing_proba_rmse <- plot_grid(plot_raster_R_rmse +
                                                      theme(plot.margin = unit(c(0,0,0,0), "in")),
                                                    plot_raster_k_rmse +
                                                      theme(plot.margin = unit(c(0,0,0,0), "in")),
                                                    plot_raster_testing_proba_rmse +
                                                      theme(plot.margin = unit(c(0,0,0,0), "in")),
                                                    labels = c("A", "B", "C"),
                                                    nrow = 3)
    
    ggsave(plot = plot_raster_R_k_testing_proba_rmse,
           filename = paste0("plots/simulation/plot_raster_R_k_testing_proba_rmse_", n_clusters_range[ii], "_", max_cluster_size_range[jj], ".png"),
           width = 7.3, height = 10.7, units = c("in"))
    
    ggsave(plot = plot_raster_R_k_testing_proba_rmse,
           filename = "plots/paper/figure_bayesian_validation.pdf",
           width = 7.3, height = 10.7, units = c("in"))
    
    # plot grid of `plot_raster_R_coverage`, `plot_raster_k_coverage` and `plot_raster_testing_proba_coverage`
    
    plot_raster_R_k_testing_proba_coverage <- plot_grid(plot_raster_R_coverage +
                                                      theme(plot.margin = unit(c(0,0,0,0), "in")),
                                                    plot_raster_k_coverage +
                                                      theme(plot.margin = unit(c(0,0,0,0), "in")),
                                                    plot_raster_testing_proba_coverage +
                                                      theme(plot.margin = unit(c(0,0,0,0), "in")),
                                                    labels = c("A", "B", "C"),
                                                    nrow = 3)
    
    ggsave(plot = plot_raster_R_k_testing_proba_coverage,
           filename = paste0("plots/simulation/plot_raster_R_k_testing_proba_coverage_", n_clusters_range[ii], "_", max_cluster_size_range[jj], ".png"),
           width = 7.3, height = 10.7, units = c("in"))
    
    plot_raster_R_k_testing_proba_coverage_a <- plot_grid(plot_raster_R_coverage +
                                                          theme(plot.margin = unit(c(0,0,0,0), "in")),
                                                        plot_raster_k_coverage +
                                                          theme(plot.margin = unit(c(0,0,0,0), "in")),
                                                        labels = c("A", "B"),
                                                        nrow = 2)
    
    plot_raster_R_k_testing_proba_coverage_b <- plot_grid(plot_raster_testing_proba_coverage +
                                                            theme(plot.margin = unit(c(0,0,0,0), "in")),
                                                          labels = c("C"),
                                                          nrow = 1)
    
  }
  
}


# print summary of results
# how many estimated were successful, how many not
print(summary_results)


