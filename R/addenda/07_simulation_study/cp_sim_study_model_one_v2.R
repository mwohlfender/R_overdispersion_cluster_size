
# simulation study
# create results of simulation study (model one)



# define parameters for plots
ratio_width_height <- 4/3
image_width_in <- 7.3


# define useful functions ----
create_h_lines_testing_proba <- function(values_testing_proba, width) {
  
  list_hlines <- list()
  
  for (ii in values_testing_proba) {
    
    list_hlines <- append(list_hlines, geom_segment(x = ii-width, y = ii, xend = ii+width, yend = ii, linetype = "solid", linewidth = 0.35))
    
  }
  
  return(list_hlines)
  
}


# read data ----

# parameter grid for simulation study
data_sim_parameters_grid_model_one <- read_csv(path_data_sim_parameters_grid_model_one)

# determine range of parameters contained in `data_sim_parameters_grid_model_one`
R_range <- sort(unique(data_sim_parameters_grid_model_one$R))
k_range <- sort(unique(data_sim_parameters_grid_model_one$k))
yearly_mutation_rate_range <- sort(unique(data_sim_parameters_grid_model_one$yearly_mutation_rate))
mean_generation_interval_range <- sort(unique(data_sim_parameters_grid_model_one$mean_generation_interval))
testing_proba_range <- sort(unique(data_sim_parameters_grid_model_one$testing_proba))
sequencing_proba_range <- sort(unique(data_sim_parameters_grid_model_one$sequencing_proba))
n_clusters_range <- sort(unique(data_sim_parameters_grid_model_one$n_clusters))
max_cluster_size_range <- sort(unique(data_sim_parameters_grid_model_one$max_cluster_size))

# results of parameter estimations
results <- read_csv(path_results_sim_processed_model_one_v2)


width_offset <- 0.035

results$cord_x_testing_proba <- results$testing_proba +
  width_offset * rescale(x = match(x = results$sequencing_proba, table = sequencing_proba_range) - (length(sequencing_proba_range)+1)/2, to = c(-1,1))

results$cord_x_sequencing_proba <- results$sequencing_proba +
  width_offset * rescale(x = match(x = results$testing_proba, table = testing_proba_range) - (length(testing_proba_range)+1)/2, to = c(-1,1))


# create plots ----
for (ii in 1:length(n_clusters_range)) {
  
  for (jj in 1:length(max_cluster_size_range)) {
    
    # plot R ----
    data_plot_R <- results %>%
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
    
    
    # plot k ----
    data_plot_k <- results %>%
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
    
    
    # plot testing probability ----
    data_plot_testing_proba <- results %>%
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
    
    
    # plot grid ----
    
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
           filename = paste0("plots/simulation/plot_raster_sim_model_one_R_k_testing_proba_", n_clusters_range[ii], "_", max_cluster_size_range[jj], "_v2.pdf"),
           width = 7.3, height = 9.0, units = c("in"), bg = "white")
    
    ggsave(plot = plot_grid_sim_pooled_R_k_testing_proba,
           filename = "plots/paper/figure_bayesian_validation_model_one_new.pdf",
           width = 7.3, height = 9, units = c("in"))

  }
  
}


