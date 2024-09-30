
# simulation study
# create results of simulation study using model five



# define useful functions ----

# define horizontal lines for plots of testing probability
create_h_lines_testing_proba <- function(values_testing_proba, width) {
  
  list_hlines <- list()
  
  for (ii in values_testing_proba) {
    
    list_hlines <- append(list_hlines, geom_segment(x = ii-width, y = ii, xend = ii+width, yend = ii, linetype = "solid", linewidth = 0.35))
    
  }
  
  return(list_hlines)
  
}


# define color transform for coefficient of variation
fun_transform_color_cv <- function(x) {
  return(log(x, base = 10))
  # return(x^(1/3))
}

fun_inv_transform_color_cv <- function(x) {
  return(10^x)
  # return(ifelse(x<0, 0, x^3))
}



# define parameters for plots ----
ratio_width_height <- 4/3
image_width_in <- 7.3
width_offset <- 0.035



# read data ----

# parameter grid for simulation study
data_sim_parameters_grid_model_five <- read_csv(path_data_sim_parameters_grid_model_five)

# determine range of parameters contained in `data_sim_parameters_grid_model_five`
R_range <- sort(unique(data_sim_parameters_grid_model_five$R))
k_range <- sort(unique(data_sim_parameters_grid_model_five$k))
mutation_proba_range <- sort(unique(data_sim_parameters_grid_model_five$mutation_proba))
testing_proba_range <- sort(unique(data_sim_parameters_grid_model_five$testing_proba))
sequencing_proba_range <- sort(unique(data_sim_parameters_grid_model_five$sequencing_proba))
n_clusters_range <- sort(unique(data_sim_parameters_grid_model_five$n_clusters))
max_cluster_size_range <- sort(unique(data_sim_parameters_grid_model_five$max_cluster_size))

# results of parameter estimations
results_v1 <- read_csv(path_results_sim_processed_model_five_v1)
results_v2 <- read_csv(path_results_sim_processed_model_five_v2)



# process data and create plots ----

# plots of number of estimates and run time and plots of rmse, coefficient of variance and coverage of R, k and testing probability ----

# `results_v1:` add column `t_total` (sum of columns `t_w_max` and `t_s_max`)
results_v1 <- results_v1 %>% mutate(t_total = t_w_max + t_s_max)

# bind together the data frames `grid_parameters` and `results_v1`
data_results_0 <- cbind(data_sim_parameters_grid_model_five, results_v1)

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
                          mutation_proba = mutation_proba_range,
                          testing_proba = testing_proba_range,
                          sequencing_proba = sequencing_proba_range,
                          n_clusters = n_clusters_range,
                          max_cluster_size = max_cluster_size_range)

n_runs_per_param_combination <- nrow(data_sim_parameters_grid_model_five) / nrow(data_plots)

data_plots <- data_plots %>%
  mutate(rmse_R = -1,
         rmse_k = -1,
         rmse_testing_proba = -1,
         cv_R = -1,
         cv_k = -1,
         cv_testing_proba = -1,
         R_in_ci = 0,
         k_in_ci = 0,
         testing_proba_in_ci = 0,
         n_estimates = 0,
         mean_t_total = 0)

for (ii in 1:nrow(data_plots)) {
  
  data_results_filtered <- data_results %>% filter(R == data_plots$R[ii], 
                                                   k == data_plots$k[ii],
                                                   mutation_proba == data_plots$mutation_proba[ii],
                                                   testing_proba == data_plots$testing_proba[ii],
                                                   sequencing_proba == data_plots$sequencing_proba[ii],
                                                   n_clusters == data_plots$n_clusters[ii],
                                                   max_cluster_size == data_plots$max_cluster_size[ii])
  
  if (nrow(data_results_filtered) >= 1) {
    
    data_plots$rmse_R[ii] <- Metrics::rmse(actual = data_results_filtered$R, predicted = data_results_filtered$R_estimate) 
    data_plots$rmse_k[ii] <- Metrics::rmse(actual = data_results_filtered$k, predicted = data_results_filtered$k_estimate)
    data_plots$rmse_testing_proba[ii] <- Metrics::rmse(actual = data_results_filtered$testing_proba, predicted = data_results_filtered$testing_proba_estimate) 
    data_plots$cv_R[ii] <- 100 * data_plots$rmse_R[ii] / mean(data_results_filtered$R)
    data_plots$cv_k[ii] <- 100 * data_plots$rmse_k[ii] / mean(data_results_filtered$k)
    data_plots$cv_testing_proba[ii] <- 100 * data_plots$rmse_testing_proba[ii] / mean(data_results_filtered$testing_proba)
    data_plots$R_in_ci[ii] <- sum(data_results_filtered$R_in_ci) / nrow(data_results_filtered)
    data_plots$k_in_ci[ii] <- sum(data_results_filtered$k_in_ci) / nrow(data_results_filtered)
    data_plots$testing_proba_in_ci[ii] <- sum(data_results_filtered$testing_proba_in_ci) / nrow(data_results_filtered)
    data_plots$n_estimates[ii] <- nrow(data_results_filtered)
    data_plots$mean_t_total[ii] <- mean(data_results_filtered$t_total)
    
  }
  
}


# create plots
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
    
    plot_sim_R_rmse <- ggplot(data = data_plot_completed_estimates_temp) +
      geom_raster(aes(x = factor(sequencing_proba), y = factor(testing_proba), fill = rmse_R)) +
      ggtitle("Effective reproduction number") +
      xlab("Sequencing probability") +
      ylab("Testing probability") +
      facet_grid(k~R, labeller = label_both) +
      scale_fill_viridis_c(name = "RMSE") +
      theme_bw() +
      theme(legend.position="bottom",
            axis.text.x = element_text(color="black"),
            axis.text.y = element_text(color="black"))
    
    ggsave(plot = plot_sim_R_rmse,
           filename = paste0("plots/simulation/model_five/plot_sim_model_five_R_rmse_", n_clusters_range[ii], "_", max_cluster_size_range[jj], ".pdf"),
           width = image_width_in, height = image_width_in/ratio_width_height, units = c("in"), bg = "white")
    
    # facet grid of raster plots of coefficient of variance of estimate of R
    # local axes: sequencing probability and testing probability
    # global axes: R and k
    
    # plot_raster_R_cv <- ggplot(data = data_plot_completed_estimates_temp) +
    #   geom_raster(aes(x = factor(sequencing_proba), y = factor(testing_proba), fill = cv_R)) +
    #   ggtitle("Effective reproduction number") +
    #   xlab("Sequencing probability") +
    #   ylab("Testing probability") +
    #   facet_grid(k~R, labeller = label_both) +
    #   scale_fill_viridis_c(name = "CV") +
    #   theme_bw() +
    #   theme(legend.position="bottom")
    
    plot_raster_R_cv <- ggplot(data = data_plot_completed_estimates_temp) +
      geom_raster(aes(x = factor(sequencing_proba), y = factor(testing_proba), fill = cv_R)) +
      ggtitle("Effective reproduction number") +
      xlab("Sequencing probability") +
      ylab("Testing probability") +
      facet_grid(k~R, labeller = label_both) +
      scale_fill_viridis_c(name = "CV",
                           breaks = c(1, 10, 100),
                           limits = c(0.3, 900),
                           transform = scales::trans_new("transform_color_cv", fun_transform_color_cv, fun_inv_transform_color_cv)) +
      theme_bw() +
      theme(legend.position="bottom",
            axis.text.x = element_text(color="black"),
            axis.text.y = element_text(color="black"))
    
    ggsave(plot = plot_raster_R_cv,
           filename = paste0("plots/simulation/model_five/plot_sim_model_five_R_cv_", n_clusters_range[ii], "_", max_cluster_size_range[jj], ".pdf"),
           width = image_width_in, height = image_width_in/ratio_width_height, units = c("in"), bg = "white")
    
    # facet grid of raster plots of coverage of true value of R by estimated credible intervals
    # local axes: sequencing probability and testing probability
    # global axes: R and k
    
    plot_sim_R_coverage <- ggplot(data = data_plot_completed_estimates_temp) +
      geom_raster(aes(x = factor(sequencing_proba), y = factor(testing_proba), fill = R_in_ci)) +
      ggtitle("Effective reproduction number") +
      xlab("Sequencing probability") +
      ylab("Testing probability") +
      facet_grid(k~R, labeller = label_both) +
      scale_fill_viridis_c(name = "Coverage",
                           breaks = c(0, 0.25, 0.5, 0.75, 1),
                           limits = c(0, 1)) +
      theme_bw() +
      theme(legend.position="bottom",
            axis.text.x = element_text(color="black"),
            axis.text.y = element_text(color="black"))
    
    ggsave(plot = plot_sim_R_coverage,
           filename = paste0("plots/simulation/model_five/plot_sim_model_five_R_coverage_", n_clusters_range[ii], "_", max_cluster_size_range[jj], ".pdf"),
           width = image_width_in, height = image_width_in/ratio_width_height, units = c("in"), bg = "white")
    
    # facet grid of raster plots of root mean square error of estimate of k
    # local axes: sequencing probability and testing probability
    # global axes: R and k
    
    plot_sim_k_rmse <- ggplot(data = data_plot_completed_estimates_temp) +
      # geom_raster(aes(x = factor(sequencing_proba), y = factor(testing_proba), fill = rmse_k)) +
      geom_raster(aes(x = factor(sequencing_proba), y = factor(testing_proba), fill = log(rmse_k, base = 10))) +
      ggtitle("Dispersion parameter") +
      xlab("Sequencing probability") +
      ylab("Testing probability") +
      facet_grid(k~R, labeller = label_both) +
      # scale_fill_viridis_c(name = "RMSE")
      scale_fill_viridis_c(name = expression(paste(log[10], ~ "(RMSE)"))) +
      theme_bw() +
      theme(legend.position="bottom",
            axis.text.x = element_text(color="black"),
            axis.text.y = element_text(color="black"))
    
    ggsave(plot = plot_sim_k_rmse,
           filename = paste0("plots/simulation/model_five/plot_sim_model_five_k_rmse_", n_clusters_range[ii], "_", max_cluster_size_range[jj], ".pdf"),
           width = image_width_in, height = image_width_in/ratio_width_height, units = c("in"), bg = "white")
    
    # facet grid of raster plots of coefficient of variance of estimate of k
    # local axes: sequencing probability and testing probability
    # global axes: R and k
    
    # plot_raster_k_cv <- ggplot(data = data_plot_completed_estimates_temp) +
    #   geom_raster(aes(x = factor(sequencing_proba), y = factor(testing_proba), fill = cv_k)) +
    #   # geom_raster(aes(x = factor(sequencing_proba), y = factor(testing_proba), fill = log(cv_k, base = 10))) +
    #   ggtitle("Dispersion parameter") +
    #   xlab("Sequencing probability") +
    #   ylab("Testing probability") +
    #   facet_grid(k~R, labeller = label_both) +
    #   # scale_fill_viridis_c(name = "CV") +
    #   scale_fill_viridis_c(name = "CV", trans = "log") +
    #   # scale_fill_viridis_c(name = expression(paste(log[10], ~ "(CV)"))) +
    #   theme_bw() +
    #   theme(legend.position="bottom")
    
    plot_raster_k_cv <- ggplot(data = data_plot_completed_estimates_temp) +
      geom_raster(aes(x = factor(sequencing_proba), y = factor(testing_proba), fill = cv_k)) +
      ggtitle("Dispersion parameter") +
      xlab("Sequencing probability") +
      ylab("Testing probability") +
      facet_grid(k~R, labeller = label_both) +
      scale_fill_viridis_c(name = "CV",
                           breaks = c(1, 10, 100),
                           limits = c(0.3, 900),
                           transform = scales::trans_new("transform_color_cv", fun_transform_color_cv, fun_inv_transform_color_cv)) +
      theme_bw() +
      theme(legend.position="bottom",
            axis.text.x = element_text(color="black"),
            axis.text.y = element_text(color="black"))
    
    ggsave(plot = plot_raster_k_cv,
           filename = paste0("plots/simulation/model_five/plot_sim_model_five_k_cv_", n_clusters_range[ii], "_", max_cluster_size_range[jj], ".pdf"),
           width = image_width_in, height = image_width_in/ratio_width_height, units = c("in"), bg = "white")
    
    # facet grid of raster plots of coverage of true value of R by estimated credible intervals
    # local axes: sequencing probability and testing probability
    # global axes: R and k
    
    plot_sim_k_coverage <- ggplot(data = data_plot_completed_estimates_temp) +
      geom_raster(aes(x = factor(sequencing_proba), y = factor(testing_proba), fill = k_in_ci)) +
      ggtitle("Dispersion parameter") +
      xlab("Sequencing probability") +
      ylab("Testing probability") +
      facet_grid(k~R, labeller = label_both) +
      scale_fill_viridis_c(name = "Coverage",
                           breaks = c(0, 0.25, 0.5, 0.75, 1),
                           limits = c(0, 1)) +
      theme_bw() +
      theme(legend.position="bottom",
            axis.text.x = element_text(color="black"),
            axis.text.y = element_text(color="black"))
    
    ggsave(plot = plot_sim_k_coverage,
           filename = paste0("plots/simulation/model_five/plot_sim_model_five_k_coverage_", n_clusters_range[ii], "_", max_cluster_size_range[jj], ".pdf"),
           width = image_width_in, height = image_width_in/ratio_width_height, units = c("in"), bg = "white")
    
    # facet grid of raster plots of root mean square error of estimate of testing probability
    # local axes: sequencing probability and testing probability
    # global axes: R and k
    
    plot_sim_testing_proba_rmse <- ggplot(data = data_plot_completed_estimates_temp) +
      geom_raster(aes(x = factor(sequencing_proba), y = factor(testing_proba), fill = rmse_testing_proba)) +
      ggtitle("Testing probability") +
      xlab("Sequencing probability") +
      ylab("Testing probability") +
      facet_grid(k~R, labeller = label_both) +
      scale_fill_viridis_c(name = "RMSE") +
      theme_bw() +
      theme(legend.position="bottom",
            axis.text.x = element_text(color="black"),
            axis.text.y = element_text(color="black"))
    
    ggsave(plot = plot_sim_testing_proba_rmse,
           filename = paste0("plots/simulation/model_five/plot_sim_model_five_testing_proba_rmse_", n_clusters_range[ii], "_", max_cluster_size_range[jj], ".pdf"),
           width = image_width_in, height = image_width_in/ratio_width_height, units = c("in"), bg = "white")
    
    # facet grid of raster plots of coefficient of variance of estimate of testing probability
    # local axes: sequencing probability and testing probability
    # global axes: R and k
    
    # plot_raster_testing_proba_cv <- ggplot(data = data_plot_completed_estimates_temp) +
    #   # geom_raster(aes(x = factor(sequencing_proba), y = factor(testing_proba), fill = cv_testing_proba)) +
    #   geom_raster(aes(x = factor(sequencing_proba), y = factor(testing_proba), fill = log(cv_testing_proba, base = 10))) +
    #   ggtitle("Testing probability") +
    #   xlab("Sequencing probability") +
    #   ylab("Testing probability") +
    #   facet_grid(k~R, labeller = label_both) +
    #   # scale_fill_viridis_c(name = "CV") +
    #   scale_fill_viridis_c(name = expression(paste(log[10], ~ "(CV)"))) +
    #   theme_bw() +
    #   theme(legend.position="bottom")
    
    plot_raster_testing_proba_cv <- ggplot(data = data_plot_completed_estimates_temp) +
      geom_raster(aes(x = factor(sequencing_proba), y = factor(testing_proba), fill = cv_testing_proba)) +
      ggtitle("Testing probability") +
      xlab("Sequencing probability") +
      ylab("Testing probability") +
      facet_grid(k~R, labeller = label_both) +
      scale_fill_viridis_c(name = "CV",
                           breaks = c(1, 10, 100),
                           limits = c(0.3, 900),
                           transform = scales::trans_new("transform_color_cv", fun_transform_color_cv, fun_inv_transform_color_cv)) +
      theme_bw() +
      theme(legend.position="bottom",
            axis.text.x = element_text(color="black"),
            axis.text.y = element_text(color="black"))
    
    ggsave(plot = plot_raster_testing_proba_cv,
           filename = paste0("plots/simulation/model_five/plot_sim_model_five_testing_proba_cv_", n_clusters_range[ii], "_", max_cluster_size_range[jj], ".pdf"),
           width = image_width_in, height = image_width_in/ratio_width_height, units = c("in"), bg = "white")
    
    # facet grid of raster plots of coverage of true value of testing probability by estimated credible intervals
    # local axes: sequencing probability and testing probability
    # global axes: R and k
    
    plot_sim_testing_proba_coverage <- ggplot(data = data_plot_completed_estimates_temp) +
      geom_raster(aes(x = factor(sequencing_proba), y = factor(testing_proba), fill = testing_proba_in_ci)) +
      ggtitle("Testing probability") +
      xlab("Sequencing probability") +
      ylab("Testing probability") +
      facet_grid(k~R, labeller = label_both) +
      scale_fill_viridis_c(name = "Coverage",
                           breaks = c(0, 0.25, 0.5, 0.75, 1),
                           limits = c(0, 1)) +
      theme_bw() +
      theme(legend.position="bottom",
            axis.text.x = element_text(color="black"),
            axis.text.y = element_text(color="black"))
    
    ggsave(plot = plot_sim_testing_proba_coverage,
           filename = paste0("plots/simulation/model_five/plot_sim_model_five_testing_proba_coverage_", n_clusters_range[ii], "_", max_cluster_size_range[jj], ".pdf"),
           width = image_width_in, height = image_width_in/ratio_width_height, units = c("in"), bg = "white")
    
    # facet grid of raster plots of number of successful parameter estimates per parameter combination
    # local axes: sequencing probability and testing probability
    # global axes: R and k
    
    plot_raster_n_estimates <- ggplot(data = data_plot_all_estimates_temp) +
      geom_raster(aes(x = factor(sequencing_proba), y = factor(testing_proba), fill = n_estimates/n_runs_per_param_combination)) +
      ggtitle("Number of estimates") +
      xlab("Sequencing probability") +
      ylab("Testing probability") +
      facet_grid(k~R, labeller = label_both) +
      scale_fill_viridis_c(name = "successful estimates") +
      theme_bw() +
      theme(legend.position="bottom",
            axis.text.x = element_text(color="black"),
            axis.text.y = element_text(color="black"))
    
    ggsave(plot = plot_raster_n_estimates,
           filename = paste0("plots/simulation/model_five/plot_sim_model_five_n_estimates_", n_clusters_range[ii], "_", max_cluster_size_range[jj], ".pdf"),
           width = image_width_in, height = image_width_in/ratio_width_height, units = c("in"), bg = "white")
    
    # facet grid of raster plots of total run time
    # local axes: sequencing probability and testing probability
    # global axes: R and k
    
    plot_raster_run_time <- ggplot(data = data_plot_completed_estimates_temp) +
      geom_raster(aes(x = factor(sequencing_proba), y = factor(testing_proba), fill = mean_t_total)) +
      ggtitle("Run time") +
      xlab("Sequencing probability") +
      ylab("Testing probability") +
      facet_grid(k~R, labeller = label_both) +
      scale_fill_viridis_c(name = "run time") +
      theme_bw() +
      theme(legend.position="bottom",
            axis.text.x = element_text(color="black"),
            axis.text.y = element_text(color="black"))
    
    ggsave(plot = plot_raster_run_time,
           filename = paste0("plots/simulation/model_five/plot_sim_model_five_run_time_", n_clusters_range[ii], "_", max_cluster_size_range[jj], ".pdf"),
           width = image_width_in, height = image_width_in/ratio_width_height, units = c("in"), bg = "white")
    
    # plot grid of `plot_sim_R_rmse`, `plot_sim_k_rmse` and `plot_sim_testing_proba_rmse`
    
    plot_sim_R_k_testing_proba_rmse <- plot_grid(plot_sim_R_rmse +
                                                   theme(axis.text=element_text(size=7, color="black"),
                                                         legend.text=element_text(size=7),
                                                         plot.margin = unit(c(0,0,0,0), "in")),
                                                 plot_sim_k_rmse +
                                                   theme(axis.text=element_text(size=7, color="black"),
                                                         legend.text=element_text(size=7),
                                                         plot.margin = unit(c(0,0,0,0), "in")),
                                                 plot_sim_testing_proba_rmse +
                                                   theme(axis.text=element_text(size=7, color="black"),
                                                         legend.text=element_text(size=7),
                                                         plot.margin = unit(c(0,0,0,0), "in")),
                                                 labels = c("A", "B", "C"),
                                                 rel_heights = c(1, 1, 1, 0.15),
                                                 nrow = 3)
    
    ggsave(plot = plot_sim_R_k_testing_proba_rmse,
           filename = paste0("plots/simulation/model_five/plot_sim_model_five_R_k_testing_proba_rmse_", n_clusters_range[ii], "_", max_cluster_size_range[jj], ".pdf"),
           width = 7.3, height = 10.7, units = c("in"), bg = "white")
    
    # plot grid of `plot_raster_R_cv`, `plot_raster_k_cv` and `plot_raster_testing_proba_cv`
    legend_cv <- get_legend(plot_raster_R_cv)
    
    plot_sim_R_k_testing_proba_cv <- plot_grid(plot_raster_R_cv +
                                                 guides(fill = "none") +
                                                 theme(plot.margin = unit(c(0,0,0.1,0), "in")),
                                               plot_raster_k_cv +
                                                 guides(fill = "none") +
                                                 theme(plot.margin = unit(c(0,0,0.1,0), "in")),
                                               plot_raster_testing_proba_cv +
                                                 guides(fill = "none") +
                                                 theme(plot.margin = unit(c(0,0,0.1,0), "in")),
                                               as_ggplot(legend_cv),
                                               labels = c("A", "B", "C", ""),
                                               rel_heights = c(1, 1, 1, 0.15),
                                               nrow = 4)
    
    ggsave(plot = plot_sim_R_k_testing_proba_cv,
           filename = paste0("plots/simulation/model_five/plot_sim_model_five_R_k_testing_proba_cv_", n_clusters_range[ii], "_", max_cluster_size_range[jj], ".pdf"),
           width = 7.3, height = 10.7, units = c("in"), bg = "white")
    
    # plot grid of `plot_sim_R_coverage`, `plot_sim_k_coverage` and `plot_sim_testing_proba_coverage`
    legend_coverage <- get_legend(plot_sim_R_coverage)
    
    plot_sim_R_k_testing_proba_coverage <- plot_grid(plot_sim_R_coverage +
                                                       guides(fill = "none") +
                                                       theme(plot.margin = unit(c(0,0,0,0), "in")),
                                                     plot_sim_k_coverage +
                                                       guides(fill = "none") +
                                                       theme(plot.margin = unit(c(0,0,0,0), "in")),
                                                     plot_sim_testing_proba_coverage +
                                                       guides(fill = "none") +
                                                       theme(plot.margin = unit(c(0,0,0,0), "in")),
                                                     as_ggplot(legend_coverage),
                                                     labels = c("A", "B", "C", ""),
                                                     rel_heights = c(1, 1, 1, 0.25),
                                                     nrow = 4)
    
    ggsave(plot = plot_sim_R_k_testing_proba_coverage,
           filename = paste0("plots/simulation/model_five/plot_sim_model_five_R_k_testing_proba_coverage_", n_clusters_range[ii], "_", max_cluster_size_range[jj], ".pdf"),
           width = 7.3, height = 10.7, units = c("in"), bg = "white")
    
  }
  
}


# print summary of results
# how many estimated were successful, how many not
print(summary_results)



# plots of pooled samples of R, k and testing probability ----
results_v2$cord_x_testing_proba <- results_v2$testing_proba +
  width_offset * rescale(x = match(x = results_v2$sequencing_proba, table = sequencing_proba_range) - (length(sequencing_proba_range)+1)/2, to = c(-1,1))

results_v2$cord_x_sequencing_proba <- results_v2$sequencing_proba +
  width_offset * rescale(x = match(x = results_v2$testing_proba, table = testing_proba_range) - (length(testing_proba_range)+1)/2, to = c(-1,1))


for (ii in 1:length(n_clusters_range)) {
  
  for (jj in 1:length(max_cluster_size_range)) {
    
    # plot R
    data_plot_R <- results_v2 %>%
      filter(R_estimate != 0)
    
    plot_sim_pooled_R <- ggplot(data = data_plot_R) +
      # geom_hline(aes(yintercept = R), linetype = "dashed") +
      geom_segment(aes(x = 0, xend = 1, y = R, yend = R), linetype = "solid", linewidth = 0.35) +
      geom_point(aes(x = cord_x_testing_proba, y = R_estimate, color = as.factor(sequencing_proba)), size = 0.6) +
      geom_errorbar(mapping = aes(x = cord_x_testing_proba, y = R_estimate, ymin = R_lower_cred_int, ymax = R_upper_cred_int, color = as.factor(sequencing_proba)),
                    linewidth = 0.3,
                    width  = 0.02) +
      ggtitle("Effective reproduction number") +
      scale_x_continuous(name = "Testing probability",
                         limits = c(0, 1),
                         breaks = c(0, 0.1, 0.2, 0.4, 0.6, 0.8, 1),
                         labels = c("0", "0.1", "0.2", "0.4", "0.6", "0.8", "1")) +
      scale_y_continuous(name = expression(paste("Estimated", ~ R[e])),
                         limits = c(0.2, 1.4),
                         breaks = c(0.1, 0.3, 0.5, 0.7, 0.9, 1.1, 1.3, 1.5)) +
      scale_colour_viridis_d(name = "Sequencing probability") +
      facet_grid(k~R, labeller = label_both) + 
      theme_bw() +
      theme(legend.position = "bottom",
            legend.box = "horizontal",
            axis.text.x = element_text(color="black"),
            axis.text.y = element_text(color="black"))
    
    ggsave(plot = plot_sim_pooled_R,
           filename = paste0("plots/simulation/model_five/plot_sim_model_five_R_pooled_", n_clusters_range[ii], "_", max_cluster_size_range[jj], ".pdf"),
           width = image_width_in, height = image_width_in/ratio_width_height, units = c("in"), bg = "white")
    
    
    # plot k
    data_plot_k <- results_v2 %>%
      filter(k_estimate != 0)
    
    plot_sim_pooled_k <- ggplot(data = data_plot_k) +
      # geom_hline(aes(yintercept = k), linetype = "dashed") +
      geom_segment(aes(x = 0, xend = 1, y = k, yend = k), linetype = "solid", linewidth = 0.35) +
      geom_point(aes(x = cord_x_testing_proba, y = k_estimate, color = as.factor(sequencing_proba)), size = 0.6) +
      geom_errorbar(mapping = aes(x = cord_x_testing_proba, y = k_estimate, ymin = k_lower_cred_int, ymax = k_upper_cred_int, color = as.factor(sequencing_proba)),
                    linewidth = 0.3,
                    width  = 0.02) +
      ggtitle("Dispersion parameter") +
      scale_x_continuous(name = "Testing probability",
                         limits = c(0, 1),
                         breaks = c(0, 0.1, 0.2, 0.4, 0.6, 0.8, 1),
                         labels = c("0", "0.1", "0.2", "0.4", "0.6", "0.8", "1")) +
      # scale_y_continuous(name = "Estimated dispersion parameter",
      #                    limits = c(0, 3.2),
      #                    breaks = c(0.1, 0.5, 1, 2, 3)) +
      scale_y_continuous(name = "Estimated dispersion parameter",
                         limits = c(0.05, 3.2),
                         breaks = c(0.1, 0.3, 0.5, 1, 2, 3),
                         trans='log') +
      scale_colour_viridis_d(name = "Sequencing probability") +
      facet_grid(k~R, labeller = label_both) + 
      theme_bw() +
      theme(legend.position = "bottom",
            legend.box = "horizontal",
            axis.text.x = element_text(color="black"),
            axis.text.y = element_text(color="black"))
    
    ggsave(plot = plot_sim_pooled_k,
           filename = paste0("plots/simulation/model_five/plot_sim_model_five_k_pooled_", n_clusters_range[ii], "_", max_cluster_size_range[jj], ".pdf"),
           width = image_width_in, height = image_width_in/ratio_width_height, units = c("in"), bg = "white")
    
    
    # plot testing probability
    data_plot_testing_proba <- results_v2 %>%
      filter(testing_proba_estimate != 0)
    
    plot_sim_pooled_testing_proba <- ggplot(data = data_plot_testing_proba) +
      create_h_lines_testing_proba(values_testing_proba = testing_proba_range, width = 1.6 * width_offset) +
      geom_point(aes(x = cord_x_testing_proba, y = testing_proba_estimate, color = as.factor(sequencing_proba)), size = 0.6) +
      geom_errorbar(mapping = aes(x = cord_x_testing_proba, y = testing_proba_estimate,
                                  ymin = testing_proba_lower_cred_int, ymax = testing_proba_upper_cred_int, color = as.factor(sequencing_proba)),
                    linewidth = 0.3,
                    width  = 0.02) +
      ggtitle("Testing probability") +
      scale_x_continuous(name = "Testing probability",
                         limits = c(0, 1),
                         breaks = c(0, 0.1, 0.2, 0.4, 0.6, 0.8, 1),
                         labels = c("0", "0.1", "0.2", "0.4", "0.6", "0.8", "1")) +
      scale_y_continuous(name = "Estimated testing probability",
                         limits = c(0, 1),
                         breaks = c(0, 0.2, 0.4, 0.6, 0.8, 1),
                         labels = c("0", "0.2", "0.4", "0.6", "0.8", "1")) +
      scale_colour_viridis_d(name = "Sequencing probability") +
      facet_grid(k~R, labeller = label_both) + 
      theme_bw() +
      theme(legend.position = "bottom",
            legend.box = "horizontal",
            axis.text.x = element_text(color="black"),
            axis.text.y = element_text(color="black"))
    
    ggsave(plot = plot_sim_pooled_testing_proba,
           filename = paste0("plots/simulation/model_five/plot_sim_model_five_testing_proba_pooled_", n_clusters_range[ii], "_", max_cluster_size_range[jj], ".pdf"),
           width = image_width_in, height = image_width_in/ratio_width_height, units = c("in"), bg = "white")
    
    
    # plot grid of `plot_sim_pooled_R`, `plot_sim_pooled_k` and `plot_sim_pooled_testing_proba`
    legend_sequencing_proba <- get_legend(plot_sim_pooled_R)
    
    plot_grid_sim_pooled_R_k_testing_proba <- plot_grid(plot_sim_pooled_R +
                                                          guides(color = "none") +
                                                          theme(plot.margin = unit(c(0,0,0.1,0.1), "in"),
                                                                axis.text.x = element_text(size = 4.5, color="black"),
                                                                axis.text.y = element_text(size = 4.5, color="black")),
                                                        plot_sim_pooled_k +
                                                          guides(color = "none") +
                                                          theme(plot.margin = unit(c(0,0,0.1,0.1), "in"),
                                                                axis.text.x = element_text(size = 4.5, color="black"),
                                                                axis.text.y = element_text(size = 4.5, color="black")),
                                                        plot_sim_pooled_testing_proba +
                                                          guides(color = "none") +
                                                          theme(plot.margin = unit(c(0,0,0.1,0.1), "in"),
                                                                axis.text.x = element_text(size = 4.5, color="black"),
                                                                axis.text.y = element_text(size = 4.5, color="black")),
                                                        as_ggplot(legend_sequencing_proba),
                                                        labels = c("A", "B", "C", ""),
                                                        rel_heights = c(1, 1, 1, 0.1),
                                                        nrow = 4)
    
    ggsave(plot = plot_grid_sim_pooled_R_k_testing_proba,
           filename = paste0("plots/simulation/model_five/plot_sim_model_five_R_k_testing_proba_pooled_", n_clusters_range[ii], "_", max_cluster_size_range[jj], ".pdf"),
           width = 7.3, height = 10.7, units = c("in"), bg = "white")
    
    ggsave(plot = plot_grid_sim_pooled_R_k_testing_proba,
           filename = "plots/paper/figure_bayesian_validation_model_five_new.pdf",
           width = 7.3, height = 9, units = c("in"), bg = "white")
    
  }
  
}


