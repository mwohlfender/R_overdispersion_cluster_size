
# simulation study
# process results of simulation study using model five



# read parameter grid for simulation study ----
data_sim_parameters_grid_model_five <- read_csv(path_data_sim_parameters_grid_model_five)

n_iterations <- max(data_sim_parameters_grid_model_five$iteration)
n_zeros <- ceiling(log(nrow(data_sim_parameters_grid_model_five), base=10))



# read and process results of parameter estimations for number of estimates and run time plots
# and rmse, coefficient of variance and coverage plots of R, k and testing probability ----
if (!(file.exists(path_results_sim_processed_model_five_v1)) | do_new_sim) {
  
  results <- data.frame(matrix(data = 0, nrow = nrow(data_sim_parameters_grid_model_five), ncol = 33))
  names(results) <- c("n_divergent_transitions", "min_Rhat", "max_Rhat",
                      "t_c1_w", "t_c2_w", "t_c3_w", "t_c4_w", "t_w_max",
                      "t_c1_s", "t_c2_s", "t_c3_s", "t_c4_s", "t_s_max",
                      "R_estimate", "R_lower_cred_int", "R_upper_cred_int", "R_Rhat",
                      "k_estimate", "k_lower_cred_int", "k_upper_cred_int", "k_Rhat",
                      "mutation_proba_estimate", "mutation_proba_lower_cred_int", "mutation_proba_upper_cred_int", "mutation_proba_Rhat",
                      "testing_proba_estimate", "testing_proba_lower_cred_int", "testing_proba_upper_cred_int", "testing_proba_Rhat",
                      "detection_proba_estimate", "detection_proba_lower_cred_int", "detection_proba_upper_cred_int", "detection_proba_Rhat")
  
  for (ii in 1:nrow(data_sim_parameters_grid_model_five)) {
    
    if (ii %% 30 == 0) {
      
      print(ii)
      
    }
    
    if (file.exists(paste0(path_results_sim_raw_model_five, str_pad(ii, n_zeros, pad = "0"), ".rds"))) {
      
      fit_ii <- readRDS(paste0(path_results_sim_raw_model_five, str_pad(ii, n_zeros, pad = "0"), ".rds"))
      
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
  
  write_csv(x = results, file = path_results_sim_processed_model_five_v1)
  
}



# read and process results of parameter estimations for plots of pooled estimates of R, k and testing probability ----
if (!(file.exists(path_results_sim_processed_model_five_v2)) | do_new_sim) {
  
  prob_lower_cred_int <- 0.025
  prob_upper_cred_int <- 0.975
  
  results <- data_sim_parameters_grid_model_five %>%
    filter(iteration == 1) %>%
    dplyr::select(-c("iteration")) %>%
    mutate(n_divergent_transitions = 0,
           R_estimate = 0, R_lower_cred_int = 0, R_upper_cred_int = 0, min_R_Rhat = 0, max_R_Rhat = 0,
           k_estimate = 0, k_lower_cred_int = 0, k_upper_cred_int = 0, min_k_Rhat = 0, max_k_Rhat = 0,
           mutation_proba_estimate = 0, mutation_proba_lower_cred_int = 0, mutation_proba_upper_cred_int = 0,
           min_mutation_proba_Rhat = 0, max_mutation_proba_Rhat = 0,
           testing_proba_estimate = 0, testing_proba_lower_cred_int = 0, testing_proba_upper_cred_int = 0,
           min_testing_proba_Rhat = 0, max_testing_proba_Rhat = 0,
           detection_proba_estimate = 0, detection_proba_lower_cred_int = 0, detection_proba_upper_cred_int = 0,
           min_detection_proba_Rhat = 0, max_detection_proba_Rhat = 0)
  
  
  for (ii in 1:nrow(results)) {
    
    if (ii %% 30 == 0) {
      
      print(ii)
      
    }
    
    n_divergent_transitions_ii <- 0
    
    samples_R_ii <- c()
    min_R_Rhat_ii <- Inf
    max_R_Rhat_ii <- -Inf
    
    samples_k_ii <- c()
    min_k_Rhat_ii <- Inf
    max_k_Rhat_ii <- -Inf
    
    samples_mutation_proba_ii <- c()
    min_mutation_proba_Rhat_ii <- Inf
    max_mutation_proba_Rhat_ii <- -Inf
    
    samples_testing_proba_ii <- c()
    min_testing_proba_Rhat_ii <- Inf
    max_testing_proba_Rhat_ii <- -Inf
    
    samples_detection_proba_ii <- c()
    min_detection_proba_Rhat_ii <- Inf
    max_detection_proba_Rhat_ii <- -Inf
    
    for (jj in 1:n_iterations) {
      
      if (file.exists(paste0(path_results_sim_raw_model_five, str_pad(ii + (jj-1) * nrow(results), n_zeros, pad = "0"), ".rds"))) {
        
        fit_ii_jj <- readRDS(paste0(path_results_sim_raw_model_five, str_pad(ii + (jj-1) * nrow(results), n_zeros, pad = "0"), ".rds"))
        
        n_divergent_transitions_ii <- n_divergent_transitions_ii + get_num_divergent(fit_ii_jj)
        
        samples_R_ii <- c(samples_R_ii, rstan::extract(fit_ii_jj, permuted = FALSE, inc_warmup = FALSE)[,,1][1:2000])
        min_R_Rhat_ii <- min(min_R_Rhat_ii, as.data.frame(summary(fit_ii_jj)[[1]])[c("R"), "Rhat"])
        max_R_Rhat_ii <- max(max_R_Rhat_ii, as.data.frame(summary(fit_ii_jj)[[1]])[c("R"), "Rhat"])
        
        samples_k_ii <- c(samples_k_ii, rstan::extract(fit_ii_jj, permuted = FALSE, inc_warmup = FALSE)[,,2][1:2000])
        min_k_Rhat_ii <- min(min_k_Rhat_ii, as.data.frame(summary(fit_ii_jj)[[1]])[c("k"), "Rhat"])
        max_k_Rhat_ii <- max(max_k_Rhat_ii, as.data.frame(summary(fit_ii_jj)[[1]])[c("k"), "Rhat"])
        
        samples_mutation_proba_ii <- c(samples_mutation_proba_ii, rstan::extract(fit_ii_jj, permuted = FALSE, inc_warmup = FALSE)[,,3][1:2000])
        min_mutation_proba_Rhat_ii <- min(min_mutation_proba_Rhat_ii, as.data.frame(summary(fit_ii_jj)[[1]])[c("mutation_proba"), "Rhat"])
        max_mutation_proba_Rhat_ii <- max(max_mutation_proba_Rhat_ii, as.data.frame(summary(fit_ii_jj)[[1]])[c("mutation_proba"), "Rhat"])
        
        samples_testing_proba_ii <- c(samples_testing_proba_ii, rstan::extract(fit_ii_jj, permuted = FALSE, inc_warmup = FALSE)[,,4][1:2000])
        min_testing_proba_Rhat_ii <- min(min_testing_proba_Rhat_ii, as.data.frame(summary(fit_ii_jj)[[1]])[c("testing_proba"), "Rhat"])
        max_testing_proba_Rhat_ii <- max(max_testing_proba_Rhat_ii, as.data.frame(summary(fit_ii_jj)[[1]])[c("testing_proba"), "Rhat"])
        
        samples_detection_proba_ii <- c(samples_detection_proba_ii, rstan::extract(fit_ii_jj, permuted = FALSE, inc_warmup = FALSE)[,,5][1:2000])
        min_detection_proba_Rhat_ii <- min(min_detection_proba_Rhat_ii, as.data.frame(summary(fit_ii_jj)[[1]])[c("detection_proba"), "Rhat"])
        max_detection_proba_Rhat_ii <- max(max_detection_proba_Rhat_ii, as.data.frame(summary(fit_ii_jj)[[1]])[c("detection_proba"), "Rhat"])
        
      }
      
    }
    
    results$n_divergent_transitions <- n_divergent_transitions_ii
    
    if (length(samples_R_ii > 0)) {
      
      results$R_estimate[ii] <- mean(x = samples_R_ii)
      results$R_lower_cred_int[ii] <- quantile(x = samples_R_ii, probs = prob_lower_cred_int)
      results$R_upper_cred_int[ii] <- quantile(x = samples_R_ii, probs = prob_upper_cred_int)
      results$min_R_Rhat[ii] <- min_R_Rhat_ii
      results$max_R_Rhat[ii] <- max_R_Rhat_ii
      
    }
    
    if (length(samples_k_ii > 0)) {
      
      results$k_estimate[ii] <- mean(x = samples_k_ii)
      results$k_lower_cred_int[ii] <- quantile(x = samples_k_ii, probs = prob_lower_cred_int)
      results$k_upper_cred_int[ii] <- quantile(x = samples_k_ii, probs = prob_upper_cred_int)
      results$min_k_Rhat[ii] <- min_k_Rhat_ii
      results$max_k_Rhat[ii] <- max_k_Rhat_ii
      
    }
    
    if (length(samples_mutation_proba_ii > 0)) {
      
      results$mutation_proba_estimate[ii] <- mean(x = samples_mutation_proba_ii)
      results$mutation_proba_lower_cred_int[ii] <- quantile(x = samples_mutation_proba_ii, probs = prob_lower_cred_int)
      results$mutation_proba_upper_cred_int[ii] <- quantile(x = samples_mutation_proba_ii, probs = prob_upper_cred_int)
      results$min_mutation_proba_Rhat[ii] <- min_mutation_proba_Rhat_ii
      results$max_mutation_proba_Rhat[ii] <- max_mutation_proba_Rhat_ii
      
    }
    
    if (length(samples_testing_proba_ii > 0)) {
      
      results$testing_proba_estimate[ii] <- mean(x = samples_testing_proba_ii)
      results$testing_proba_lower_cred_int[ii] <- quantile(x = samples_testing_proba_ii, probs = prob_lower_cred_int)
      results$testing_proba_upper_cred_int[ii] <- quantile(x = samples_testing_proba_ii, probs = prob_upper_cred_int)
      results$min_testing_proba_Rhat[ii] <- min_testing_proba_Rhat_ii
      results$max_testing_proba_Rhat[ii] <- max_testing_proba_Rhat_ii
      
    }
    
    if (length(samples_detection_proba_ii > 0)) {
      
      results$detection_proba_estimate[ii] <- mean(x = samples_detection_proba_ii)
      results$detection_proba_lower_cred_int[ii] <- quantile(x = samples_detection_proba_ii, probs = prob_lower_cred_int)
      results$detection_proba_upper_cred_int[ii] <- quantile(x = samples_detection_proba_ii, probs = prob_upper_cred_int)
      results$min_detection_proba_Rhat[ii] <- min_detection_proba_Rhat_ii
      results$max_detection_proba_Rhat[ii] <- max_detection_proba_Rhat_ii
      
    }
    
  }
  
  write_csv(x = results, file = path_results_sim_processed_model_five_v2)
  
}


