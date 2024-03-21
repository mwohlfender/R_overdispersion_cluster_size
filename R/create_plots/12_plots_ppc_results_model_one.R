
# posterior predictive check of model 1
# summarize results, create plots


# define paths
paths_data_cluster_sizes_countries_2021_months <- c(path_data_cluster_sizes_ch_2021_months, path_data_cluster_sizes_dk_2021_months, path_data_cluster_sizes_de_2021_months)
paths_results_post_pred <- c(path_results_post_pred_model_one_ch_raw, path_results_post_pred_model_one_dk_raw, path_results_post_pred_model_one_de_raw)

# load data
data_parameters_post_pred <- read_csv(path_data_post_pred_model_one_parameters_grid)

# number of combinations of parameters, 
# for each vector of parameters each entry is sampled uniformly at random and independently from the other entries of the vector 
# from the samples of the posterior distribution of the respective parameter
n_parameter_combinations <- max(as.numeric(data_parameters_post_pred$index_param_comb))

# number of times simulation of clusters is repeated for each combination of parameters
n_repetitions <- max(as.numeric(data_parameters_post_pred$index_repetition))

# first month
m0 <- ymd("2021-01-01")

# list of months
months <- c("January", "February", "March", "April", "May", "June", "July", "August", "September", "October", "November", "December")

# countries
countries <- unique(data_parameters_post_pred$country)
n_countries <- length(countries)



if (!(file.exists(path_results_post_pred_model_one_ch_dk_de_processed)) | do_new_ppc) {
  
  # data frame to store results of posterior predictive check
  results_post_pred <- data.frame(matrix(data=0, nrow=n_countries*12*max(data_parameters_post_pred$max_cluster_size), ncol=n_parameter_combinations*n_repetitions + 6))
  names(results_post_pred) <- c("country", "month", "size", "frequency_data", "quantile_2_5", "quantile_97_5", unlist(lapply(X = 1:(n_parameter_combinations * n_repetitions), 
                                                                                                                             FUN = function(x) paste0("frequency_sim_", 
                                                                                                                                                      formatC(ceiling(x / n_repetitions), width=max(ceiling(log(x=n_parameter_combinations+1, base=10)),1), flag="0"), "_", 
                                                                                                                                                      formatC(((x-1) %% n_repetitions)+1, width=max(ceiling(log(x=n_repetitions+1, base=10)),1), flag="0")))))
  
  results_post_pred <- results_post_pred |> mutate(country=rep(countries, each=12*max(data_parameters_post_pred$max_cluster_size)),
                                                   size=rep(1:max(data_parameters_post_pred$max_cluster_size), times=n_countries*12))
  
  
  # go through all countries
  for (cc in 1:n_countries) {
    
    # read data: monthly numbers of clusters of different sizes during 2021 from country
    data_cluster_sizes_country_2021_months <- read_csv(file = paths_data_cluster_sizes_countries_2021_months[cc])
    
    
    for (mm in 1:12) {
      
      results_post_pred_cc_mm <- data.frame(matrix(data=0, nrow=max(data_parameters_post_pred$max_cluster_size), ncol=n_parameter_combinations*n_repetitions + 6))
      names(results_post_pred_cc_mm) <- names(results_post_pred)
      
      results_post_pred_cc_mm <- results_post_pred_cc_mm |> mutate(country=countries[cc],
                                                                   month=mm,
                                                                   size=1:max(data_parameters_post_pred$max_cluster_size))
      
      n_clusters_cc_mm <- data_cluster_sizes_country_2021_months |> filter(month == mm) |> dplyr::select(c("size", "frequency"))
      
      results_post_pred_cc_mm$frequency_data[n_clusters_cc_mm$size] <- n_clusters_cc_mm$frequency
      
      for (ii in 1:n_parameter_combinations) {
        
        for (jj in 1:n_repetitions) {
          
          # read simulation results
          path_data_cc_mm_ii_jj <- paste0(paths_results_post_pred[cc], 
                                          formatC(mm, width=2, flag="0"), "_",
                                          formatC(ii, width=max(ceiling(log(x=n_parameter_combinations+1, base=10)),1), flag="0"), "_",
                                          formatC(jj, width=max(ceiling(log(x=n_repetitions+1, base=10)),1), flag="0"), ".csv")
          
          simulated_clusters_cc_mm_ii_jj <- read_csv(path_data_cc_mm_ii_jj) 
          
          name_column_data_cc_mm_ii_jj <- paste0("frequency_sim_",
                                                 formatC(ii, width=max(ceiling(log(x=n_parameter_combinations+1, base=10)),1), flag="0"), "_",
                                                 formatC(jj, width=max(ceiling(log(x=n_repetitions+1, base=10)),1), flag="0"))
          
          results_post_pred_cc_mm[simulated_clusters_cc_mm_ii_jj$size, name_column_data_cc_mm_ii_jj] <- simulated_clusters_cc_mm_ii_jj$frequency
          
        }
        
      }
      
      results_post_pred_cc_mm <- results_post_pred_cc_mm |> mutate(quantile_2_5 = unlist(lapply(X = 1:nrow(results_post_pred_cc_mm),
                                                                                                FUN = function(x) quantile(x = as.numeric(unlist(results_post_pred_cc_mm[x, 7:(n_parameter_combinations * n_repetitions+6)])), probs = 0.025))))
      
      results_post_pred_cc_mm <- results_post_pred_cc_mm |> mutate(quantile_97_5 = unlist(lapply(X = 1:nrow(results_post_pred_cc_mm),
                                                                                                 FUN = function(x) quantile(x = as.numeric(unlist(results_post_pred_cc_mm[x, 7:(n_parameter_combinations * n_repetitions+6)])), probs = 0.975))))
      
      # determine size of largest cluster
      size_largest_cluster <- max(results_post_pred_cc_mm |> filter(rowSums(across(c(-country, -month, -size, -quantile_2_5, -quantile_97_5))) >= 1) |> dplyr::select(size))
      
      # set entry in column "size" of those cluster sizes larger than size_largest_cluster to 0 (is used later to remove these rows)
      results_post_pred_cc_mm <- results_post_pred_cc_mm |> mutate(size = (size <= (size_largest_cluster + 1)) * size)
      
      # integrate results_post_pred_cc_mm into results_post_pred
      index_start <- (cc - 1)*12*max(data_parameters_post_pred$max_cluster_size) + (mm-1)*max(data_parameters_post_pred$max_cluster_size)
      
      results_post_pred[(index_start+1):(index_start+max(data_parameters_post_pred$max_cluster_size)),] <- results_post_pred_cc_mm
      
    }
    
  }
  
  results_post_pred <- results_post_pred |> filter(size >= 1)
  
  # save results_post_pred
  write_csv(results_post_pred, file = path_results_post_pred_model_one_ch_dk_de_processed)
  
}



results_post_pred <- read_csv(path_results_post_pred_model_one_ch_dk_de_processed)


# create plots ----

for (cc in 1:n_countries) {
  
  # single months
  for (mm in 1:12) {
    
    data_plot <- results_post_pred |> filter(country == countries[cc], month == mm)
    
    plot_comparison_clusters_quantile_country_month <- ggplot(data = data_plot) +
      geom_ribbon(aes(x=size, ymin=quantile_2_5,ymax=quantile_97_5),fill="skyblue",alpha=.6) +
      geom_line(aes(x=size, y=frequency_data, colour="col_data"), linewidth=0.25) +
      scale_color_manual(name = NULL,
                         breaks = c("col_data", "col_2_5", "col_97_5"),
                         values = c("dodgerblue2", "firebrick2", "springgreen2"),
                         labels = c("Data", "simulation: 2.5% quantile", "simulation: 97.5% quantile")) +
      scale_fill_manual(name = NULL,
                        breaks = c("col_95_pred_int"),
                        values = c("skyblue"),
                        labels = c("95% Prediction interval")) +
      ggtitle(paste0(toupper(substr(countries[cc], 1, 1)), substr(countries[cc], 2, nchar(countries[cc])), ": ", months[mm])) +
      scale_y_log10() +
      xlab(label="Cluster size") +
      ylab(label="Frequency") +
      theme_bw() +
      theme(legend.position="bottom")
    
    path_plot <- paste0("plots/", countries[cc], "/posterior_predictive_check/model_one/plot_ppc_model_one_", countries[cc], "_", formatC(mm, width=2, flag="0"), ".png")
    
    ggsave(filename=path_plot, plot=plot_comparison_clusters_quantile_country_month, width=7, units="in")
    
  }
  
}



# whole year 2021 ----

# Switzerland ----
plot_comparison_clusters_ch_quantile_facet <- ggplot(data = results_post_pred |> filter(country=="switzerland")) +
  geom_ribbon(aes(x=size, ymin=quantile_2_5, ymax=quantile_97_5, fill="col_95_pred_int"), alpha=0.6) +
  geom_line(aes(x=size, y=frequency_data, colour="col_data"), linewidth=0.25) +  
  scale_color_manual(name = NULL,
                     breaks = c("col_data", "col_2_5", "col_97_5"),
                     values = c("dodgerblue2", "firebrick2", "springgreen2"),
                     labels = c("Data", "simulation: 2.5% quantile", "simulation: 97.5% quantile")) +
  scale_fill_manual(name = NULL,
                    breaks = c("col_95_pred_int"),
                    values = c("lightskyblue"),
                    labels = c("95% Prediction interval")) +
  # ggtitle("Switzerland") +
  scale_y_log10() +
  xlab(label="Cluster size") +
  ylab(label="Frequency") +
  theme_bw() +
  theme(legend.position="bottom") +
  facet_wrap(facets = vars(factor(month, levels = 1:12)), 
             scales = "free",
             nrow = 4,
             ncol = 3,
             labeller = as_labeller(c("1" = "January", "2" = "February", "3" = "March", "4" = "April",
                                      "5" = "May", "6" = "June", "7" = "July", "8" = "August",
                                      "9" = "September", "10" = "October", "11" = "November", "12" = "December")))

# ggsave(filename="plots/switzerland/posterior_predictive_check/model_one/plot_ppc_model_one_switzerland.png", plot=plot_comparison_clusters_ch_quantile_facet, width=7, units="in")


plot_comparison_clusters_ch_quantile_facet_with_title <- plot_comparison_clusters_ch_quantile_facet +
  ggtitle("Switzerland")

ggsave(filename="plots/switzerland/posterior_predictive_check/model_one/plot_ppc_model_one_switzerland_with_title.png", plot=plot_comparison_clusters_ch_quantile_facet_with_title, width=7, units="in")



# Denmark ----
plot_comparison_clusters_dk_quantile_facet <- ggplot(data = results_post_pred |> filter(country=="denmark")) +
  geom_ribbon(aes(x=size, ymin=quantile_2_5, ymax=quantile_97_5, fill="col_95_pred_int"), alpha=0.6) +
  geom_line(aes(x=size, y=frequency_data, colour="col_data"), linewidth=0.25) +  
  scale_color_manual(name = NULL,
                     breaks = c("col_data", "col_2_5", "col_97_5"),
                     values = c("dodgerblue2", "firebrick2", "springgreen2"),
                     labels = c("Data", "simulation: 2.5% quantile", "simulation: 97.5% quantile")) +
  scale_fill_manual(name = NULL,
                    breaks = c("col_95_pred_int"),
                    values = c("lightskyblue"),
                    labels = c("95% Prediction interval")) +
  scale_y_log10() +
  xlab(label="Cluster size") +
  ylab(label="Frequency") +
  theme_bw() +
  theme(legend.position="bottom") +
  facet_wrap(facets = vars(factor(month, levels = 1:12)), 
             scales = "free",
             nrow = 4,
             ncol = 3,
             labeller = as_labeller(c("1" = "January", "2" = "February", "3" = "March", "4" = "April",
                                      "5" = "May", "6" = "June", "7" = "July", "8" = "August",
                                      "9" = "September", "10" = "October", "11" = "November", "12" = "December")))

# ggsave(filename="plots/denmark/posterior_predictive_check/model_one/plot_ppc_model_one_denmark.png", plot=plot_comparison_clusters_dk_quantile_facet, width=7, units="in")


plot_comparison_clusters_dk_quantile_facet_with_title <- plot_comparison_clusters_dk_quantile_facet +
  ggtitle("Denmark")

ggsave(filename="plots/denmark/posterior_predictive_check/model_one/plot_ppc_model_one_denmark_with_title.png", plot=plot_comparison_clusters_dk_quantile_facet_with_title, width=7, units="in")


plot_comparison_clusters_dk_quantile_facet_jan_apr <- ggplot(data = results_post_pred |> filter(country=="denmark" & month <= 4)) +
  geom_ribbon(aes(x=size, ymin=quantile_2_5, ymax=quantile_97_5, fill="col_95_pred_int"), alpha=0.6) +
  geom_line(aes(x=size, y=frequency_data, colour="col_data"), linewidth=0.25) +  
  scale_color_manual(name = NULL,
                     breaks = c("col_data", "col_2_5", "col_97_5"),
                     values = c("dodgerblue2", "firebrick2", "springgreen2"),
                     labels = c("Data (Denmark)", "simulation: 2.5% quantile", "simulation: 97.5% quantile")) +
  scale_fill_manual(name = NULL,
                    breaks = c("col_95_pred_int"),
                    values = c("lightskyblue"),
                    labels = c("95% Prediction interval")) +
  scale_y_log10() +
  xlab(label="Cluster size") +
  ylab(label="Frequency") +
  theme_bw() +
  theme(legend.position="bottom") +
  facet_wrap(facets = vars(factor(month, levels = 1:4)), 
             scales = "free",
             nrow = 1,
             ncol = 4,
             labeller = as_labeller(c("1" = "January", "2" = "February", "3" = "March", "4" = "April")))


# Germany ----
plot_comparison_clusters_de_quantile_facet <- ggplot(data = results_post_pred |> filter(country=="germany")) +
  geom_ribbon(aes(x=size, ymin=quantile_2_5, ymax=quantile_97_5, fill="col_95_pred_int"), alpha=0.6) +
  geom_line(aes(x=size, y=frequency_data, colour="col_data"), linewidth=0.25) +
  scale_color_manual(name = NULL,
                     breaks = c("col_data", "col_2_5", "col_97_5"),
                     values = c("dodgerblue2", "firebrick2", "springgreen2"),
                     labels = c("Data", "simulation: 2.5% quantile", "simulation: 97.5% quantile")) +
  scale_fill_manual(name = NULL,
                    breaks = c("col_95_pred_int"),
                    values = c("lightskyblue"),
                    labels = c("95% Prediction interval")) +
  scale_y_log10() +
  xlab(label="Cluster size") +
  ylab(label="Frequency") +
  theme_bw() +
  theme(legend.position="bottom") +
  facet_wrap(facets = vars(factor(month, levels = 1:12)), 
             scales = "free",
             nrow = 4,
             ncol = 3,
             labeller = as_labeller(c("1" = "January", "2" = "February", "3" = "March", "4" = "April",
                                      "5" = "May", "6" = "June", "7" = "July", "8" = "August",
                                      "9" = "September", "10" = "October", "11" = "November", "12" = "December")))

# ggsave(filename="plots/germany/posterior_predictive_check/model_one/plot_ppc_model_one_germany.png", plot=plot_comparison_clusters_de_quantile_facet, width=7, units="in")


plot_comparison_clusters_de_quantile_facet_with_title <- plot_comparison_clusters_de_quantile_facet +
  ggtitle("Germany")

ggsave(filename="plots/germany/posterior_predictive_check/model_one/plot_ppc_model_one_germany_with_title.png", plot=plot_comparison_clusters_de_quantile_facet_with_title, width=7, units="in")


