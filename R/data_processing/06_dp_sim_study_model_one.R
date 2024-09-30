
# simulation study
# process results of simulation study using model one


# read data ----

# parameter grid for simulation study
data_sim_parameters_grid_model_one <- read_csv(path_data_sim_parameters_grid_model_one)



# process data ----

n_zeros <- ceiling(log(nrow(data_sim_parameters_grid_model_one), base=10))

# read results of parameter estimations ----
if (!(file.exists(path_results_sim_processed_model_one_v1)) | do_new_sim) {
  
  results <- data.frame(matrix(data = 0, nrow = nrow(data_sim_parameters_grid_model_one), ncol = 37))
  names(results) <- c("n_divergent_transitions", "min_Rhat", "max_Rhat",
                      "t_c1_w", "t_c2_w", "t_c3_w", "t_c4_w", "t_w_max",
                      "t_c1_s", "t_c2_s", "t_c3_s", "t_c4_s", "t_s_max",
                      "R_estimate", "R_lower_cred_int", "R_upper_cred_int", "R_Rhat",
                      "k_estimate", "k_lower_cred_int", "k_upper_cred_int", "k_Rhat",
                      "number_yearly_mutations_estimate", "number_yearly_mutations_lower_cred_int", 
                      "number_yearly_mutations_upper_cred_int", "number_yearly_mutations_Rhat",
                      "mutation_proba_estimate", "mutation_proba_lower_cred_int", "mutation_proba_upper_cred_int", "mutation_proba_Rhat",
                      "testing_proba_estimate", "testing_proba_lower_cred_int", "testing_proba_upper_cred_int", "testing_proba_Rhat",
                      "detection_proba_estimate", "detection_proba_lower_cred_int", "detection_proba_upper_cred_int", "detection_proba_Rhat")
  
  for (ii in 1:nrow(data_sim_parameters_grid_model_one)) {
    
    if (file.exists(paste0(path_results_sim_raw_model_one, str_pad(ii, n_zeros, pad = "0"), ".rds"))) {
      
      fit_ii <- readRDS(paste0(path_results_sim_raw_model_one, str_pad(ii, n_zeros, pad = "0"), ".rds"))
      
      results$n_divergent_transitions[ii] <- get_num_divergent(fit_ii)
      
      results[ii, 4:13] <- c(get_elapsed_time(fit_ii)[,1][1:4], max(get_elapsed_time(fit_ii)[,1][1:4]),
                             get_elapsed_time(fit_ii)[,2][1:4], max(get_elapsed_time(fit_ii)[,2][1:4]))
      
      results$R_estimate[ii] <- get_posterior_mean(fit_ii, par = c("R"))[, "mean-all chains"]
      results$R_lower_cred_int[ii] <- as.data.frame(summary(fit_ii)[[1]])[c("R"), "2.5%"]
      results$R_upper_cred_int[ii] <- as.data.frame(summary(fit_ii)[[1]])[c("R"), "97.5%"]
      results$R_Rhat[ii] <- as.data.frame(summary(fit_ii)[[1]])[c("R"), "Rhat"]
      
      results$k_estimate[ii] <- get_posterior_mean(fit_ii, par = c("k"))[, "mean-all chains"]
      results$k_lower_cred_int[ii] <- as.data.frame(summary(fit_ii)[[1]])[c("k"), "2.5%"]
      results$k_upper_cred_int[ii] <- as.data.frame(summary(fit_ii)[[1]])[c("k"), "97.5%"]
      results$k_Rhat[ii] <- as.data.frame(summary(fit_ii)[[1]])[c("k"), "Rhat"]
      
      results$number_yearly_mutations_estimate[ii] <- get_posterior_mean(fit_ii, par = c("number_yearly_mutations"))[, "mean-all chains"]
      results$number_yearly_mutations_lower_cred_int[ii] <- as.data.frame(summary(fit_ii)[[1]])[c("number_yearly_mutations"), "2.5%"]
      results$number_yearly_mutations_upper_cred_int[ii] <- as.data.frame(summary(fit_ii)[[1]])[c("number_yearly_mutations"), "97.5%"]
      results$number_yearly_mutations_Rhat[ii] <- as.data.frame(summary(fit_ii)[[1]])[c("number_yearly_mutations"), "Rhat"]
      
      results$mutation_proba_estimate[ii] <- get_posterior_mean(fit_ii, par = c("mutation_proba"))[, "mean-all chains"]
      results$mutation_proba_lower_cred_int[ii] <- as.data.frame(summary(fit_ii)[[1]])[c("mutation_proba"), "2.5%"]
      results$mutation_proba_upper_cred_int[ii] <- as.data.frame(summary(fit_ii)[[1]])[c("mutation_proba"), "97.5%"]
      results$mutation_proba_Rhat[ii] <- as.data.frame(summary(fit_ii)[[1]])[c("mutation_proba"), "Rhat"]
      
      results$testing_proba_estimate[ii] <- get_posterior_mean(fit_ii, par = c("testing_proba"))[, "mean-all chains"]
      results$testing_proba_lower_cred_int[ii] <- as.data.frame(summary(fit_ii)[[1]])[c("testing_proba"), "2.5%"]
      results$testing_proba_upper_cred_int[ii] <- as.data.frame(summary(fit_ii)[[1]])[c("testing_proba"), "97.5%"]
      results$testing_proba_Rhat[ii] <- as.data.frame(summary(fit_ii)[[1]])[c("testing_proba"), "Rhat"]
      
      results$detection_proba_estimate[ii] <- get_posterior_mean(fit_ii, par = c("detection_proba"))[, "mean-all chains"]
      results$detection_proba_lower_cred_int[ii] <- as.data.frame(summary(fit_ii)[[1]])[c("detection_proba"), "2.5%"]
      results$detection_proba_upper_cred_int[ii] <- as.data.frame(summary(fit_ii)[[1]])[c("detection_proba"), "97.5%"]
      results$detection_proba_Rhat[ii] <- as.data.frame(summary(fit_ii)[[1]])[c("detection_proba"), "Rhat"]
      
      results$min_Rhat[ii] <- min(results$R_Rhat[ii], results$k_Rhat[ii], results$number_yearly_mutations_Rhat[ii], 
                                  results$mutation_proba_Rhat[ii], results$testing_proba_Rhat[ii], results$detection_proba_Rhat[ii])
      
      results$max_Rhat[ii] <- max(results$R_Rhat[ii], results$k_Rhat[ii], results$number_yearly_mutations_Rhat[ii], 
                                  results$mutation_proba_Rhat[ii], results$testing_proba_Rhat[ii], results$detection_proba_Rhat[ii])
      
    }
    
  }
  
  write_csv(x = results, file = path_results_sim_processed_model_one_v1)
  
}

