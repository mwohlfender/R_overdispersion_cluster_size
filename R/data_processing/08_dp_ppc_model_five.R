
# posterior predictive check of model 5
# summarize results


# define paths ----
paths_data_cluster_sizes_countries_2021_months <- c(path_data_cluster_sizes_ch_2021_months, path_data_cluster_sizes_dk_2021_months, path_data_cluster_sizes_de_2021_months)
paths_results_post_pred <- c(path_results_post_pred_model_five_ch_raw, path_results_post_pred_model_five_dk_raw, path_results_post_pred_model_five_de_raw)



# load data ----
data_parameters_post_pred <- read_csv(path_data_post_pred_model_five_parameters_grid)



# process data ----

# number of combinations of parameters, 
# for each vector of parameters each entry is sampled uniformly at random and independently from the other entries of the vector 
# from the samples of the posterior distribution of the respective parameter
n_parameter_combinations <- max(as.numeric(data_parameters_post_pred$index_param_comb))

# number of times simulation of clusters is repeated for each combination of parameters
n_repetitions <- max(as.numeric(data_parameters_post_pred$index_repetition))

# list of months
months <- c("January", "February", "March", "April", "May", "June", "July", "August", "September", "October", "November", "December")

# countries
countries <- unique(data_parameters_post_pred$country)
n_countries <- length(countries)


if (!(file.exists(path_results_post_pred_model_five_ch_dk_de_processed)) | do_new_ppc) {
  
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
      # size_largest_cluster <- max(results_post_pred_cc_mm |> filter(rowSums(across(c(-country, -month, -size, -quantile_2_5, -quantile_97_5))) >= 1) |> dplyr::select(size))
      
      size_largest_cluster <- custom_round(max(results_post_pred_cc_mm |> filter(frequency_data >= 1) |> dplyr::select(size)), 50)
      
      # set entry in column "size" of those cluster sizes larger than size_largest_cluster to 0 (is used later to remove these rows)
      results_post_pred_cc_mm <- results_post_pred_cc_mm |> mutate(size = (size <= (size_largest_cluster + 1)) * size)
      
      # integrate results_post_pred_cc_mm into results_post_pred
      index_start <- (cc - 1)*12*max(data_parameters_post_pred$max_cluster_size) + (mm-1)*max(data_parameters_post_pred$max_cluster_size)
      
      results_post_pred[(index_start+1):(index_start+max(data_parameters_post_pred$max_cluster_size)),] <- results_post_pred_cc_mm
      
    }
    
  }
  
  results_post_pred <- results_post_pred |> filter(size >= 1)
  
  # save results_post_pred
  write_csv(results_post_pred, file = path_results_post_pred_model_five_ch_dk_de_processed)
  
}

