
# apply model four to data from New Zealand



# read and process data from New Zealand ----

# read cluster data
data_nz_0 <- readRDS(path_data_clusters_nz_raw)

# `data_nz_0`:
# summarize omicron and non-omicron cases
data_nz <- data_nz_0 %>%
  group_by(period, cluster_size) %>%
  summarize(n_clusters = sum(n_clusters)) %>%
  ungroup()

# `data_nz_0`:
# summarize omicron and non-omicron cases and periods
data_nz_complete <- data_nz_0 %>%
  group_by(cluster_size) %>%
  summarize(n_clusters = sum(n_clusters)) %>%
  ungroup()

# `data_nz`:
# determine unique values of the column `period` and sort them
periods_nz <- sort(unique(data_nz %>% pull(period)))

# read mutation probability
data_mut_proba_pre_omicron <- readRDS(path_mutation_probas_sarscov2_pre_omicron)

list_mutation_proba <- sort(1- c(data_mut_proba_pre_omicron$p_trans_before_mut,
                                 data_mut_proba_pre_omicron$lower_p_trans_before_mut,
                                 data_mut_proba_pre_omicron$upper_p_trans_before_mut))

list_testing_proba <- c(0.5, 0.8, 1.0)

# read sequencing probabilities from New Zealand
sequencing_probas_nz_periods_0 <- readRDS(path_sequencing_probas_nz_periods)

# `sequencing_probas_nz_periods_0`:
# extract column `prop_cases_sequenced`
sequencing_probas_nz_periods <- sequencing_probas_nz_periods_0 %>% pull(prop_cases_sequenced)

# overall sequencing probability
sequencing_proba_nz <- sum(sequencing_probas_nz_periods_0 %>% pull(n_sequences)) / sum(sequencing_probas_nz_periods_0 %>% pull(n_cases))



# apply model 4: low mutation probability and testing probability 0.5 ----
for (ii in 1:length(periods_nz)) {
  
  path_results_parameter_estimations_model_four_temp <- paste0(path_results_model_four_nz, "p_mut_low_p_test_050_", ii, ".rds")
  
  if (!(file.exists(path_results_parameter_estimations_model_four_temp) | do_new_pe_nz)) {
    
    data_nz_period <- data_nz %>% filter(period == periods_nz[ii])
    
    results_parameter_estimations_model_four_temp <- estRodis_estimate_parameters_four(clusters_size = data_nz_period %>% pull("cluster_size"),
                                                                                       clusters_freq = data_nz_period %>% pull("n_clusters"),
                                                                                       prior_r = c(10, 10),
                                                                                       prior_k = c(5, 10),
                                                                                       mutation_proba = list_mutation_proba[1],
                                                                                       testing_proba = 0.5,
                                                                                       sequencing_proba = sequencing_probas_nz_periods[ii],
                                                                                       warmup = 500,
                                                                                       iter = 1000,
                                                                                       chains = 4,
                                                                                       cores = 4,
                                                                                       thin = 1,
                                                                                       control = list(adapt_delta = 0.99),
                                                                                       init = lapply(1:4, FUN = function(x) estRodis_init_params_model_four()))
    
    # save results of parameter estimation
    saveRDS(results_parameter_estimations_model_four_temp, file = path_results_parameter_estimations_model_four_temp)
    
  }
  
}


path_results_parameter_estimations_model_four_temp <- paste0(path_results_model_four_nz, "p_mut_low_p_test_050_all.rds")

if (!(file.exists(path_results_parameter_estimations_model_four_temp) | do_new_pe_nz)) {
  
  results_parameter_estimations_model_four_temp <- estRodis_estimate_parameters_four(clusters_size = data_nz_complete %>% pull("cluster_size"),
                                                                                     clusters_freq = data_nz_complete %>% pull("n_clusters"),
                                                                                     prior_r = c(10, 10),
                                                                                     prior_k = c(5, 10),
                                                                                     mutation_proba = list_mutation_proba[1],
                                                                                     testing_proba = 0.5,
                                                                                     sequencing_proba = sequencing_proba_nz,
                                                                                     warmup = 500,
                                                                                     iter = 1000,
                                                                                     chains = 4,
                                                                                     cores = 4,
                                                                                     thin = 1,
                                                                                     control = list(adapt_delta = 0.99),
                                                                                     init = lapply(1:4, FUN = function(x) estRodis_init_params_model_four()))
  
  # save results of parameter estimation
  saveRDS(results_parameter_estimations_model_four_temp, file = path_results_parameter_estimations_model_four_temp)
  
}


# apply model 4: low mutation probability and testing probability 0.8 ----
for (ii in 1:length(periods_nz)) {
  
  path_results_parameter_estimations_model_four_temp <- paste0(path_results_model_four_nz, "p_mut_low_p_test_080_", ii, ".rds")
  
  if (!(file.exists(path_results_parameter_estimations_model_four_temp) | do_new_pe_nz)) {
    
    data_nz_period <- data_nz %>% filter(period == periods_nz[ii])
    
    results_parameter_estimations_model_four_temp <- estRodis_estimate_parameters_four(clusters_size = data_nz_period %>% pull("cluster_size"),
                                                                                       clusters_freq = data_nz_period %>% pull("n_clusters"),
                                                                                       prior_r = c(10, 10),
                                                                                       prior_k = c(5, 10),
                                                                                       mutation_proba = list_mutation_proba[1],
                                                                                       testing_proba = 0.8,
                                                                                       sequencing_proba = sequencing_probas_nz_periods[ii],
                                                                                       warmup = 500,
                                                                                       iter = 1000,
                                                                                       chains = 4,
                                                                                       cores = 4,
                                                                                       thin = 1,
                                                                                       control = list(adapt_delta = 0.99),
                                                                                       init = lapply(1:4, FUN = function(x) estRodis_init_params_model_four()))
    
    # save results of parameter estimation
    saveRDS(results_parameter_estimations_model_four_temp, file = path_results_parameter_estimations_model_four_temp)
    
  }
  
}


path_results_parameter_estimations_model_four_temp <- paste0(path_results_model_four_nz, "p_mut_low_p_test_080_all.rds")

if (!(file.exists(path_results_parameter_estimations_model_four_temp) | do_new_pe_nz)) {
  
  results_parameter_estimations_model_four_temp <- estRodis_estimate_parameters_four(clusters_size = data_nz_complete %>% pull("cluster_size"),
                                                                                     clusters_freq = data_nz_complete %>% pull("n_clusters"),
                                                                                     prior_r = c(10, 10),
                                                                                     prior_k = c(5, 10),
                                                                                     mutation_proba = list_mutation_proba[1],
                                                                                     testing_proba = 0.8,
                                                                                     sequencing_proba = sequencing_proba_nz,
                                                                                     warmup = 500,
                                                                                     iter = 1000,
                                                                                     chains = 4,
                                                                                     cores = 4,
                                                                                     thin = 1,
                                                                                     control = list(adapt_delta = 0.99),
                                                                                     init = lapply(1:4, FUN = function(x) estRodis_init_params_model_four()))
  
  # save results of parameter estimation
  saveRDS(results_parameter_estimations_model_four_temp, file = path_results_parameter_estimations_model_four_temp)
  
}


# apply model 4: low mutation probability and testing probability 1.0 ----
for (ii in 1:length(periods_nz)) {
  
  path_results_parameter_estimations_model_four_temp <- paste0(path_results_model_four_nz, "p_mut_low_p_test_100_", ii, ".rds")
  
  if (!(file.exists(path_results_parameter_estimations_model_four_temp) | do_new_pe_nz)) {
    
    data_nz_period <- data_nz %>% filter(period == periods_nz[ii])
    
    results_parameter_estimations_model_four_temp <- estRodis_estimate_parameters_four(clusters_size = data_nz_period %>% pull("cluster_size"),
                                                                                       clusters_freq = data_nz_period %>% pull("n_clusters"),
                                                                                       prior_r = c(10, 10),
                                                                                       prior_k = c(5, 10),
                                                                                       mutation_proba = list_mutation_proba[1],
                                                                                       testing_proba = 1.0,
                                                                                       sequencing_proba = sequencing_probas_nz_periods[ii],
                                                                                       warmup = 500,
                                                                                       iter = 1000,
                                                                                       chains = 4,
                                                                                       cores = 4,
                                                                                       thin = 1,
                                                                                       control = list(adapt_delta = 0.99),
                                                                                       init = lapply(1:4, FUN = function(x) estRodis_init_params_model_four()))
    
    # save results of parameter estimation
    saveRDS(results_parameter_estimations_model_four_temp, file = path_results_parameter_estimations_model_four_temp)
    
  }
  
}


path_results_parameter_estimations_model_four_temp <- paste0(path_results_model_four_nz, "p_mut_low_p_test_100_all.rds")

if (!(file.exists(path_results_parameter_estimations_model_four_temp) | do_new_pe_nz)) {
  
  results_parameter_estimations_model_four_temp <- estRodis_estimate_parameters_four(clusters_size = data_nz_complete %>% pull("cluster_size"),
                                                                                     clusters_freq = data_nz_complete %>% pull("n_clusters"),
                                                                                     prior_r = c(10, 10),
                                                                                     prior_k = c(5, 10),
                                                                                     mutation_proba = list_mutation_proba[1],
                                                                                     testing_proba = 1.0,
                                                                                     sequencing_proba = sequencing_proba_nz,
                                                                                     warmup = 500,
                                                                                     iter = 1000,
                                                                                     chains = 4,
                                                                                     cores = 4,
                                                                                     thin = 1,
                                                                                     control = list(adapt_delta = 0.99),
                                                                                     init = lapply(1:4, FUN = function(x) estRodis_init_params_model_four()))
  
  # save results of parameter estimation
  saveRDS(results_parameter_estimations_model_four_temp, file = path_results_parameter_estimations_model_four_temp)
  
}


# apply model 4: central mutation probability and testing probability 0.5 ----
for (ii in 1:length(periods_nz)) {
  
  path_results_parameter_estimations_model_four_temp <- paste0(path_results_model_four_nz, "p_mut_central_p_test_050_", ii, ".rds")
  
  if (!(file.exists(path_results_parameter_estimations_model_four_temp) | do_new_pe_nz)) {
    
    data_nz_period <- data_nz %>% filter(period == periods_nz[ii])
    
    results_parameter_estimations_model_four_temp <- estRodis_estimate_parameters_four(clusters_size = data_nz_period %>% pull("cluster_size"),
                                                                                       clusters_freq = data_nz_period %>% pull("n_clusters"),
                                                                                       prior_r = c(10, 10),
                                                                                       prior_k = c(5, 10),
                                                                                       mutation_proba = list_mutation_proba[2],
                                                                                       testing_proba = 0.5,
                                                                                       sequencing_proba = sequencing_probas_nz_periods[ii],
                                                                                       warmup = 500,
                                                                                       iter = 1000,
                                                                                       chains = 4,
                                                                                       cores = 4,
                                                                                       thin = 1,
                                                                                       control = list(adapt_delta = 0.99),
                                                                                       init = lapply(1:4, FUN = function(x) estRodis_init_params_model_four()))
    
    # save results of parameter estimation
    saveRDS(results_parameter_estimations_model_four_temp, file = path_results_parameter_estimations_model_four_temp)
    
  }
  
}


path_results_parameter_estimations_model_four_temp <- paste0(path_results_model_four_nz, "p_mut_central_p_test_050_all.rds")

if (!(file.exists(path_results_parameter_estimations_model_four_temp) | do_new_pe_nz)) {
  
  results_parameter_estimations_model_four_temp <- estRodis_estimate_parameters_four(clusters_size = data_nz_complete %>% pull("cluster_size"),
                                                                                     clusters_freq = data_nz_complete %>% pull("n_clusters"),
                                                                                     prior_r = c(10, 10),
                                                                                     prior_k = c(5, 10),
                                                                                     mutation_proba = list_mutation_proba[2],
                                                                                     testing_proba = 0.5,
                                                                                     sequencing_proba = sequencing_proba_nz,
                                                                                     warmup = 500,
                                                                                     iter = 1000,
                                                                                     chains = 4,
                                                                                     cores = 4,
                                                                                     thin = 1,
                                                                                     control = list(adapt_delta = 0.99),
                                                                                     init = lapply(1:4, FUN = function(x) estRodis_init_params_model_four()))
  
  # save results of parameter estimation
  saveRDS(results_parameter_estimations_model_four_temp, file = path_results_parameter_estimations_model_four_temp)
  
}


# apply model 4: central mutation probability and testing probability 0.8 ----
for (ii in 1:length(periods_nz)) {
  
  path_results_parameter_estimations_model_four_temp <- paste0(path_results_model_four_nz, "p_mut_central_p_test_080_", ii, ".rds")
  
  if (!(file.exists(path_results_parameter_estimations_model_four_temp) | do_new_pe_nz)) {
    
    data_nz_period <- data_nz %>% filter(period == periods_nz[ii])
    
    results_parameter_estimations_model_four_temp <- estRodis_estimate_parameters_four(clusters_size = data_nz_period %>% pull("cluster_size"),
                                                                                       clusters_freq = data_nz_period %>% pull("n_clusters"),
                                                                                       prior_r = c(10, 10),
                                                                                       prior_k = c(5, 10),
                                                                                       mutation_proba = list_mutation_proba[2],
                                                                                       testing_proba = 0.8,
                                                                                       sequencing_proba = sequencing_probas_nz_periods[ii],
                                                                                       warmup = 500,
                                                                                       iter = 1000,
                                                                                       chains = 4,
                                                                                       cores = 4,
                                                                                       thin = 1,
                                                                                       control = list(adapt_delta = 0.99),
                                                                                       init = lapply(1:4, FUN = function(x) estRodis_init_params_model_four()))
    
    # save results of parameter estimation
    saveRDS(results_parameter_estimations_model_four_temp, file = path_results_parameter_estimations_model_four_temp)
    
  }
  
}


path_results_parameter_estimations_model_four_temp <- paste0(path_results_model_four_nz, "p_mut_central_p_test_080_all.rds")

if (!(file.exists(path_results_parameter_estimations_model_four_temp) | do_new_pe_nz)) {
  
  results_parameter_estimations_model_four_temp <- estRodis_estimate_parameters_four(clusters_size = data_nz_complete %>% pull("cluster_size"),
                                                                                     clusters_freq = data_nz_complete %>% pull("n_clusters"),
                                                                                     prior_r = c(10, 10),
                                                                                     prior_k = c(5, 10),
                                                                                     mutation_proba = list_mutation_proba[2],
                                                                                     testing_proba = 0.8,
                                                                                     sequencing_proba = sequencing_proba_nz,
                                                                                     warmup = 500,
                                                                                     iter = 1000,
                                                                                     chains = 4,
                                                                                     cores = 4,
                                                                                     thin = 1,
                                                                                     control = list(adapt_delta = 0.99),
                                                                                     init = lapply(1:4, FUN = function(x) estRodis_init_params_model_four()))
  
  # save results of parameter estimation
  saveRDS(results_parameter_estimations_model_four_temp, file = path_results_parameter_estimations_model_four_temp)
  
}

# apply model 4: central mutation probability and testing probability 1.0 ----
for (ii in 1:length(periods_nz)) {
  
  path_results_parameter_estimations_model_four_temp <- paste0(path_results_model_four_nz, "p_mut_central_p_test_100_", ii, ".rds")
  
  if (!(file.exists(path_results_parameter_estimations_model_four_temp) | do_new_pe_nz)) {
    
    data_nz_period <- data_nz %>% filter(period == periods_nz[ii])
    
    results_parameter_estimations_model_four_temp <- estRodis_estimate_parameters_four(clusters_size = data_nz_period %>% pull("cluster_size"),
                                                                                       clusters_freq = data_nz_period %>% pull("n_clusters"),
                                                                                       prior_r = c(10, 10),
                                                                                       prior_k = c(5, 10),
                                                                                       mutation_proba = list_mutation_proba[2],
                                                                                       testing_proba = 1.0,
                                                                                       sequencing_proba = sequencing_probas_nz_periods[ii],
                                                                                       warmup = 500,
                                                                                       iter = 1000,
                                                                                       chains = 4,
                                                                                       cores = 4,
                                                                                       thin = 1,
                                                                                       control = list(adapt_delta = 0.99),
                                                                                       init = lapply(1:4, FUN = function(x) estRodis_init_params_model_four()))
    
    # save results of parameter estimation
    saveRDS(results_parameter_estimations_model_four_temp, file = path_results_parameter_estimations_model_four_temp)
    
  }
  
}


path_results_parameter_estimations_model_four_temp <- paste0(path_results_model_four_nz, "p_mut_central_p_test_100_all.rds")

if (!(file.exists(path_results_parameter_estimations_model_four_temp) | do_new_pe_nz)) {
  
  results_parameter_estimations_model_four_temp <- estRodis_estimate_parameters_four(clusters_size = data_nz_complete %>% pull("cluster_size"),
                                                                                     clusters_freq = data_nz_complete %>% pull("n_clusters"),
                                                                                     prior_r = c(10, 10),
                                                                                     prior_k = c(5, 10),
                                                                                     mutation_proba = list_mutation_proba[2],
                                                                                     testing_proba = 1.0,
                                                                                     sequencing_proba = sequencing_proba_nz,
                                                                                     warmup = 500,
                                                                                     iter = 1000,
                                                                                     chains = 4,
                                                                                     cores = 4,
                                                                                     thin = 1,
                                                                                     control = list(adapt_delta = 0.99),
                                                                                     init = lapply(1:4, FUN = function(x) estRodis_init_params_model_four()))
  
  # save results of parameter estimation
  saveRDS(results_parameter_estimations_model_four_temp, file = path_results_parameter_estimations_model_four_temp)
  
}


# apply model 4: high mutation probability and testing probability 0.5 ----
for (ii in 1:length(periods_nz)) {
  
  path_results_parameter_estimations_model_four_temp <- paste0(path_results_model_four_nz, "p_mut_high_p_test_050_", ii, ".rds")
  
  if (!(file.exists(path_results_parameter_estimations_model_four_temp) | do_new_pe_nz)) {
    
    data_nz_period <- data_nz %>% filter(period == periods_nz[ii])
    
    results_parameter_estimations_model_four_temp <- estRodis_estimate_parameters_four(clusters_size = data_nz_period %>% pull("cluster_size"),
                                                                                       clusters_freq = data_nz_period %>% pull("n_clusters"),
                                                                                       prior_r = c(10, 10),
                                                                                       prior_k = c(5, 10),
                                                                                       mutation_proba = list_mutation_proba[3],
                                                                                       testing_proba = 0.5,
                                                                                       sequencing_proba = sequencing_probas_nz_periods[ii],
                                                                                       warmup = 500,
                                                                                       iter = 1000,
                                                                                       chains = 4,
                                                                                       cores = 4,
                                                                                       thin = 1,
                                                                                       control = list(adapt_delta = 0.99),
                                                                                       init = lapply(1:4, FUN = function(x) estRodis_init_params_model_four()))
    
    # save results of parameter estimation
    saveRDS(results_parameter_estimations_model_four_temp, file = path_results_parameter_estimations_model_four_temp)
    
  }
  
}


path_results_parameter_estimations_model_four_temp <- paste0(path_results_model_four_nz, "p_mut_high_p_test_050_all.rds")

if (!(file.exists(path_results_parameter_estimations_model_four_temp) | do_new_pe_nz)) {
  
  results_parameter_estimations_model_four_temp <- estRodis_estimate_parameters_four(clusters_size = data_nz_complete %>% pull("cluster_size"),
                                                                                     clusters_freq = data_nz_complete %>% pull("n_clusters"),
                                                                                     prior_r = c(10, 10),
                                                                                     prior_k = c(5, 10),
                                                                                     mutation_proba = list_mutation_proba[3],
                                                                                     testing_proba = 0.5,
                                                                                     sequencing_proba = sequencing_proba_nz,
                                                                                     warmup = 500,
                                                                                     iter = 1000,
                                                                                     chains = 4,
                                                                                     cores = 4,
                                                                                     thin = 1,
                                                                                     control = list(adapt_delta = 0.99),
                                                                                     init = lapply(1:4, FUN = function(x) estRodis_init_params_model_four()))
  
  # save results of parameter estimation
  saveRDS(results_parameter_estimations_model_four_temp, file = path_results_parameter_estimations_model_four_temp)
  
}


# apply model 4: high mutation probability and testing probability 0.8 ----
for (ii in 1:length(periods_nz)) {
  
  path_results_parameter_estimations_model_four_temp <- paste0(path_results_model_four_nz, "p_mut_high_p_test_080_", ii, ".rds")
  
  if (!(file.exists(path_results_parameter_estimations_model_four_temp) | do_new_pe_nz)) {
    
    data_nz_period <- data_nz %>% filter(period == periods_nz[ii])
    
    results_parameter_estimations_model_four_temp <- estRodis_estimate_parameters_four(clusters_size = data_nz_period %>% pull("cluster_size"),
                                                                                       clusters_freq = data_nz_period %>% pull("n_clusters"),
                                                                                       prior_r = c(10, 10),
                                                                                       prior_k = c(5, 10),
                                                                                       mutation_proba = list_mutation_proba[3],
                                                                                       testing_proba = 0.8,
                                                                                       sequencing_proba = sequencing_probas_nz_periods[ii],
                                                                                       warmup = 500,
                                                                                       iter = 1000,
                                                                                       chains = 4,
                                                                                       cores = 4,
                                                                                       thin = 1,
                                                                                       control = list(adapt_delta = 0.99),
                                                                                       init = lapply(1:4, FUN = function(x) estRodis_init_params_model_four()))
    
    # save results of parameter estimation
    saveRDS(results_parameter_estimations_model_four_temp, file = path_results_parameter_estimations_model_four_temp)
    
  }
  
}


path_results_parameter_estimations_model_four_temp <- paste0(path_results_model_four_nz, "p_mut_high_p_test_080_all.rds")

if (!(file.exists(path_results_parameter_estimations_model_four_temp) | do_new_pe_nz)) {
  
  results_parameter_estimations_model_four_temp <- estRodis_estimate_parameters_four(clusters_size = data_nz_complete %>% pull("cluster_size"),
                                                                                     clusters_freq = data_nz_complete %>% pull("n_clusters"),
                                                                                     prior_r = c(10, 10),
                                                                                     prior_k = c(5, 10),
                                                                                     mutation_proba = list_mutation_proba[3],
                                                                                     testing_proba = 0.8,
                                                                                     sequencing_proba = sequencing_proba_nz,
                                                                                     warmup = 500,
                                                                                     iter = 1000,
                                                                                     chains = 4,
                                                                                     cores = 4,
                                                                                     thin = 1,
                                                                                     control = list(adapt_delta = 0.99),
                                                                                     init = lapply(1:4, FUN = function(x) estRodis_init_params_model_four()))
  
  # save results of parameter estimation
  saveRDS(results_parameter_estimations_model_four_temp, file = path_results_parameter_estimations_model_four_temp)
  
}


# apply model 4: high mutation probability and testing probability 1.0 ----
for (ii in 1:length(periods_nz)) {
  
  path_results_parameter_estimations_model_four_temp <- paste0(path_results_model_four_nz, "p_mut_high_p_test_100_", ii, ".rds")
  
  if (!(file.exists(path_results_parameter_estimations_model_four_temp) | do_new_pe_nz)) {
    
    data_nz_period <- data_nz %>% filter(period == periods_nz[ii])
    
    results_parameter_estimations_model_four_temp <- estRodis_estimate_parameters_four(clusters_size = data_nz_period %>% pull("cluster_size"),
                                                                                       clusters_freq = data_nz_period %>% pull("n_clusters"),
                                                                                       prior_r = c(10, 10),
                                                                                       prior_k = c(5, 10),
                                                                                       mutation_proba = list_mutation_proba[3],
                                                                                       testing_proba = 1.0,
                                                                                       sequencing_proba = sequencing_probas_nz_periods[ii],
                                                                                       warmup = 500,
                                                                                       iter = 1000,
                                                                                       chains = 4,
                                                                                       cores = 4,
                                                                                       thin = 1,
                                                                                       control = list(adapt_delta = 0.99),
                                                                                       init = lapply(1:4, FUN = function(x) estRodis_init_params_model_four()))
    
    # save results of parameter estimation
    saveRDS(results_parameter_estimations_model_four_temp, file = path_results_parameter_estimations_model_four_temp)
    
  }
  
}


path_results_parameter_estimations_model_four_temp <- paste0(path_results_model_four_nz, "p_mut_high_p_test_100_all.rds")

if (!(file.exists(path_results_parameter_estimations_model_four_temp) | do_new_pe_nz)) {
  
  results_parameter_estimations_model_four_temp <- estRodis_estimate_parameters_four(clusters_size = data_nz_complete %>% pull("cluster_size"),
                                                                                     clusters_freq = data_nz_complete %>% pull("n_clusters"),
                                                                                     prior_r = c(10, 10),
                                                                                     prior_k = c(5, 10),
                                                                                     mutation_proba = list_mutation_proba[3],
                                                                                     testing_proba = 1.0,
                                                                                     sequencing_proba = sequencing_proba_nz,
                                                                                     warmup = 500,
                                                                                     iter = 1000,
                                                                                     chains = 4,
                                                                                     cores = 4,
                                                                                     thin = 1,
                                                                                     control = list(adapt_delta = 0.99),
                                                                                     init = lapply(1:4, FUN = function(x) estRodis_init_params_model_four()))
  
  # save results of parameter estimation
  saveRDS(results_parameter_estimations_model_four_temp, file = path_results_parameter_estimations_model_four_temp)
  
}


