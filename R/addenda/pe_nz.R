

# read data from new zealand ----
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

# read sequencing probabilities from new zealand
sequencing_probas_nz_periods_0 <- readRDS(path_sequencing_probas_nz_periods)

# `sequencing_probas_nz_periods_0`:
# extract column `prop_cases_sequenced`
sequencing_probas_nz_periods <- sequencing_probas_nz_periods_0 %>% pull(prop_cases_sequenced)

# overall sequencing probability
sequencing_proba_nz <- sum(sequencing_probas_nz_periods_0 %>% pull(n_sequences)) / sum(sequencing_probas_nz_periods_0 %>% pull(n_cases))



# apply model one ----
for (ii in 1:length(periods_nz)) {
  
  data_nz_period <- data_nz %>% filter(period == periods_nz[ii])
  
  results_parameter_estimation_model_one <- estRodis_estimate_parameters_one(clusters_size = data_nz_period %>% pull("cluster_size"),
                                                                             clusters_freq = data_nz_period %>% pull("n_clusters"),
                                                                             sequencing_proba = sequencing_probas_nz_periods[ii],
                                                                             warmup = 500,
                                                                             iter = 1000,
                                                                             chains = 4,
                                                                             cores = 4,
                                                                             thin = 1,
                                                                             control = list(adapt_delta = 0.99),
                                                                             init = lapply(1:4, FUN = function(x) estRodis_init_params_model_one()))
  
  # save results of parameter estimation
  saveRDS(results_parameter_estimation_model_one, file = paste0("results/new_zealand/parameter_estimations/model_one/parameter_estimates_model_one_new_zealand_", ii, ".rds"))
  
}


results_parameter_estimation_model_one <- estRodis_estimate_parameters_one(clusters_size = data_nz_complete %>% pull("cluster_size"),
                                                                           clusters_freq = data_nz_complete %>% pull("n_clusters"),
                                                                           sequencing_proba = sequencing_proba_nz,
                                                                           warmup = 500,
                                                                           iter = 1000,
                                                                           chains = 4,
                                                                           cores = 4,
                                                                           thin = 1,
                                                                           control = list(adapt_delta = 0.99),
                                                                           init = lapply(1:4, FUN = function(x) estRodis_init_params_model_one()))

# save results of parameter estimation
saveRDS(results_parameter_estimation_model_one, file = paste0("results/new_zealand/parameter_estimations/model_one/parameter_estimates_model_one_new_zealand_all.rds"))



# apply model two ----
for (ii in 1:length(periods_nz)) {
  
  data_nz_period <- data_nz %>% filter(period == periods_nz[ii])
  
  results_parameter_estimation_model_two <- estRodis_estimate_parameters_two(clusters_size = data_nz_period %>% pull("cluster_size"),
                                                                             clusters_freq = data_nz_period %>% pull("n_clusters"),
                                                                             testing_proba = 0.5,
                                                                             sequencing_proba = sequencing_probas_nz_periods[ii],
                                                                             warmup = 500,
                                                                             iter = 1000,
                                                                             chains = 4,
                                                                             cores = 4,
                                                                             thin = 1,
                                                                             control = list(adapt_delta = 0.99),
                                                                             init = lapply(1:4, FUN = function(x) estRodis_init_params_model_two()))
  
  # save results of parameter estimation
  saveRDS(results_parameter_estimation_model_two, file = paste0("results/new_zealand/parameter_estimations/model_two/parameter_estimates_model_two_new_zealand_p_test_050_", ii, ".rds"))
  
}


results_parameter_estimation_model_two <- estRodis_estimate_parameters_two(clusters_size = data_nz_complete %>% pull("cluster_size"),
                                                                           clusters_freq = data_nz_complete %>% pull("n_clusters"),
                                                                           testing_proba = 0.5,
                                                                           sequencing_proba = sequencing_proba_nz,
                                                                           warmup = 500,
                                                                           iter = 1000,
                                                                           chains = 4,
                                                                           cores = 4,
                                                                           thin = 1,
                                                                           control = list(adapt_delta = 0.99),
                                                                           init = lapply(1:4, FUN = function(x) estRodis_init_params_model_two()))

# save results of parameter estimation
saveRDS(results_parameter_estimation_model_two, file = paste0("results/new_zealand/parameter_estimations/model_two/parameter_estimates_model_two_new_zealand_p_test_050_all.rds"))


for (ii in 1:length(periods_nz)) {
  
  data_nz_period <- data_nz %>% filter(period == periods_nz[ii])
  
  results_parameter_estimation_model_two <- estRodis_estimate_parameters_two(clusters_size = data_nz_period %>% pull("cluster_size"),
                                                                             clusters_freq = data_nz_period %>% pull("n_clusters"),
                                                                             testing_proba = 0.8,
                                                                             sequencing_proba = sequencing_probas_nz_periods[ii],
                                                                             warmup = 500,
                                                                             iter = 1000,
                                                                             chains = 4,
                                                                             cores = 4,
                                                                             thin = 1,
                                                                             control = list(adapt_delta = 0.99),
                                                                             init = lapply(1:4, FUN = function(x) estRodis_init_params_model_two()))
  
  # save results of parameter estimation
  saveRDS(results_parameter_estimation_model_two, file = paste0("results/new_zealand/parameter_estimations/model_two/parameter_estimates_model_two_new_zealand_p_test_080_", ii, ".rds"))
  
}


results_parameter_estimation_model_two <- estRodis_estimate_parameters_two(clusters_size = data_nz_complete %>% pull("cluster_size"),
                                                                           clusters_freq = data_nz_complete %>% pull("n_clusters"),
                                                                           testing_proba = 0.8,
                                                                           sequencing_proba = sequencing_proba_nz,
                                                                           warmup = 500,
                                                                           iter = 1000,
                                                                           chains = 4,
                                                                           cores = 4,
                                                                           thin = 1,
                                                                           control = list(adapt_delta = 0.99),
                                                                           init = lapply(1:4, FUN = function(x) estRodis_init_params_model_two()))

# save results of parameter estimation
saveRDS(results_parameter_estimation_model_two, file = paste0("results/new_zealand/parameter_estimations/model_two/parameter_estimates_model_two_new_zealand_p_test_080_all.rds"))


for (ii in 1:length(periods_nz)) {
  
  data_nz_period <- data_nz %>% filter(period == periods_nz[ii])
  
  results_parameter_estimation_model_two <- estRodis_estimate_parameters_two(clusters_size = data_nz_period %>% pull("cluster_size"),
                                                                             clusters_freq = data_nz_period %>% pull("n_clusters"),
                                                                             testing_proba = 1,
                                                                             sequencing_proba = sequencing_probas_nz_periods[ii],
                                                                             warmup = 500,
                                                                             iter = 1000,
                                                                             chains = 4,
                                                                             cores = 4,
                                                                             thin = 1,
                                                                             control = list(adapt_delta = 0.99),
                                                                             init = lapply(1:4, FUN = function(x) estRodis_init_params_model_two()))
  
  # save results of parameter estimation
  saveRDS(results_parameter_estimation_model_two, file = paste0("results/new_zealand/parameter_estimations/model_two/parameter_estimates_model_two_new_zealand_p_test_100_", ii, ".rds"))
  
}


results_parameter_estimation_model_two <- estRodis_estimate_parameters_two(clusters_size = data_nz_complete %>% pull("cluster_size"),
                                                                           clusters_freq = data_nz_complete %>% pull("n_clusters"),
                                                                           testing_proba = 1.0,
                                                                           sequencing_proba = sequencing_proba_nz,
                                                                           warmup = 500,
                                                                           iter = 1000,
                                                                           chains = 4,
                                                                           cores = 4,
                                                                           thin = 1,
                                                                           control = list(adapt_delta = 0.99),
                                                                           init = lapply(1:4, FUN = function(x) estRodis_init_params_model_two()))

# save results of parameter estimation
saveRDS(results_parameter_estimation_model_two, file = paste0("results/new_zealand/parameter_estimations/model_two/parameter_estimates_model_two_new_zealand_p_test_100_all.rds"))


