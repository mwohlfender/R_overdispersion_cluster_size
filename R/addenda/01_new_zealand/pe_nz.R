

# read mutation probability
data_mut_proba_0 <- readRDS("R/addenda/df_p_trans_before_mut_with_uncertainty.rds") 

data_mut_proba_pre_omicron <- data_mut_proba_0 %>% 
  filter(pathogen %in% c("SARS-CoV-2 (pre-Omicron)"))

data_mut_proba_omicron <- data_mut_proba_0 %>% 
  filter(pathogen %in% c("SARS-CoV-2 (Omicron)")) 


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


list_mutation_proba <- sort(1- c(data_mut_proba_pre_omicron$p_trans_before_mut,
                                 data_mut_proba_pre_omicron$lower_p_trans_before_mut,
                                 data_mut_proba_pre_omicron$upper_p_trans_before_mut))
list_testing_proba <- c(0.5, 0.8, 1.0)

# 
# # apply model one ----
# for (ii in 1:length(periods_nz)) {
#   
#   data_nz_period <- data_nz %>% filter(period == periods_nz[ii])
#   
#   results_parameter_estimation_model_one <- estRodis_estimate_parameters_one(clusters_size = data_nz_period %>% pull("cluster_size"),
#                                                                              clusters_freq = data_nz_period %>% pull("n_clusters"),
#                                                                              prior_r = c(10, 10),
#                                                                              prior_k = c(5, 10),
#                                                                              mean_generation_interval  = 5.2,
#                                                                              prior_number_yearly_mutations = c(14, 0.5),
#                                                                              prior_testing = c(1, 3, 0.05, 1),
#                                                                              sequencing_proba = sequencing_probas_nz_periods[ii],
#                                                                              warmup = 500,
#                                                                              iter = 1000,
#                                                                              chains = 4,
#                                                                              cores = 4,
#                                                                              thin = 1,
#                                                                              control = list(adapt_delta = 0.99),
#                                                                              init = lapply(1:4, FUN = function(x) estRodis_init_params_model_one()))
#   
#   # save results of parameter estimation
#   saveRDS(results_parameter_estimation_model_one, file = paste0("results/new_zealand/parameter_estimations/model_one/parameter_estimates_model_one_new_zealand_", ii, ".rds"))
#   
# }
# 
# 
# results_parameter_estimation_model_one <- estRodis_estimate_parameters_one(clusters_size = data_nz_complete %>% pull("cluster_size"),
#                                                                            clusters_freq = data_nz_complete %>% pull("n_clusters"),
#                                                                            prior_r = c(10, 10),
#                                                                            prior_k = c(5, 10),
#                                                                            mean_generation_interval  = 5.2,
#                                                                            prior_number_yearly_mutations = c(14, 0.5),
#                                                                            prior_testing = c(1, 3, 0.05, 1),
#                                                                            sequencing_proba = sequencing_proba_nz,
#                                                                            warmup = 500,
#                                                                            iter = 1000,
#                                                                            chains = 4,
#                                                                            cores = 4,
#                                                                            thin = 1,
#                                                                            control = list(adapt_delta = 0.99),
#                                                                            init = lapply(1:4, FUN = function(x) estRodis_init_params_model_one()))
# 
# # save results of parameter estimation
# saveRDS(results_parameter_estimation_model_one, file = paste0("results/new_zealand/parameter_estimations/model_one/parameter_estimates_model_one_new_zealand_all.rds"))
# 


# # apply model two ----
# 
# # testing probability 0.5
# for (ii in 1:length(periods_nz)) {
#   
#   data_nz_period <- data_nz %>% filter(period == periods_nz[ii])
#   
#   results_parameter_estimation_model_two <- estRodis_estimate_parameters_two(clusters_size = data_nz_period %>% pull("cluster_size"),
#                                                                              clusters_freq = data_nz_period %>% pull("n_clusters"),
#                                                                              prior_r = c(10, 10),
#                                                                              prior_k = c(5, 10),
#                                                                              mean_generation_interval  = 5.2,
#                                                                              prior_number_yearly_mutations = c(14, 0.5),
#                                                                              testing_proba = 0.5,
#                                                                              sequencing_proba = sequencing_probas_nz_periods[ii],
#                                                                              warmup = 500,
#                                                                              iter = 1000,
#                                                                              chains = 4,
#                                                                              cores = 4,
#                                                                              thin = 1,
#                                                                              control = list(adapt_delta = 0.99),
#                                                                              init = lapply(1:4, FUN = function(x) estRodis_init_params_model_two()))
#   
#   # save results of parameter estimation
#   saveRDS(results_parameter_estimation_model_two, file = paste0("results/new_zealand/parameter_estimations/model_two/parameter_estimates_model_two_new_zealand_p_test_050_", ii, ".rds"))
#   
# }
# 
# 
# results_parameter_estimation_model_two <- estRodis_estimate_parameters_two(clusters_size = data_nz_complete %>% pull("cluster_size"),
#                                                                            clusters_freq = data_nz_complete %>% pull("n_clusters"),
#                                                                            prior_r = c(10, 10),
#                                                                            prior_k = c(5, 10),
#                                                                            mean_generation_interval  = 5.2,
#                                                                            prior_number_yearly_mutations = c(14, 0.5),
#                                                                            testing_proba = 0.5,
#                                                                            sequencing_proba = sequencing_proba_nz,
#                                                                            warmup = 500,
#                                                                            iter = 1000,
#                                                                            chains = 4,
#                                                                            cores = 4,
#                                                                            thin = 1,
#                                                                            control = list(adapt_delta = 0.99),
#                                                                            init = lapply(1:4, FUN = function(x) estRodis_init_params_model_two()))
# 
# # save results of parameter estimation
# saveRDS(results_parameter_estimation_model_two, file = paste0("results/new_zealand/parameter_estimations/model_two/parameter_estimates_model_two_new_zealand_p_test_050_all.rds"))
# 
# 
# # testing probability 0.8
# for (ii in 1:length(periods_nz)) {
#   
#   data_nz_period <- data_nz %>% filter(period == periods_nz[ii])
#   
#   results_parameter_estimation_model_two <- estRodis_estimate_parameters_two(clusters_size = data_nz_period %>% pull("cluster_size"),
#                                                                              clusters_freq = data_nz_period %>% pull("n_clusters"),
#                                                                              prior_r = c(10, 10),
#                                                                              prior_k = c(5, 10),
#                                                                              mean_generation_interval  = 5.2,
#                                                                              prior_number_yearly_mutations = c(14, 0.5),
#                                                                              testing_proba = 0.8,
#                                                                              sequencing_proba = sequencing_probas_nz_periods[ii],
#                                                                              warmup = 500,
#                                                                              iter = 1000,
#                                                                              chains = 4,
#                                                                              cores = 4,
#                                                                              thin = 1,
#                                                                              control = list(adapt_delta = 0.99),
#                                                                              init = lapply(1:4, FUN = function(x) estRodis_init_params_model_two()))
#   
#   # save results of parameter estimation
#   saveRDS(results_parameter_estimation_model_two, file = paste0("results/new_zealand/parameter_estimations/model_two/parameter_estimates_model_two_new_zealand_p_test_080_", ii, ".rds"))
#   
# }
# 
# 
# results_parameter_estimation_model_two <- estRodis_estimate_parameters_two(clusters_size = data_nz_complete %>% pull("cluster_size"),
#                                                                            clusters_freq = data_nz_complete %>% pull("n_clusters"),
#                                                                            prior_r = c(10, 10),
#                                                                            prior_k = c(5, 10),
#                                                                            mean_generation_interval  = 5.2,
#                                                                            prior_number_yearly_mutations = c(14, 0.5),
#                                                                            testing_proba = 0.8,
#                                                                            sequencing_proba = sequencing_proba_nz,
#                                                                            warmup = 500,
#                                                                            iter = 1000,
#                                                                            chains = 4,
#                                                                            cores = 4,
#                                                                            thin = 1,
#                                                                            control = list(adapt_delta = 0.99),
#                                                                            init = lapply(1:4, FUN = function(x) estRodis_init_params_model_two()))
# 
# # save results of parameter estimation
# saveRDS(results_parameter_estimation_model_two, file = paste0("results/new_zealand/parameter_estimations/model_two/parameter_estimates_model_two_new_zealand_p_test_080_all.rds"))
# 
# 
# # testing probability 1.0
# for (ii in 1:length(periods_nz)) {
#   
#   data_nz_period <- data_nz %>% filter(period == periods_nz[ii])
#   
#   results_parameter_estimation_model_two <- estRodis_estimate_parameters_two(clusters_size = data_nz_period %>% pull("cluster_size"),
#                                                                              clusters_freq = data_nz_period %>% pull("n_clusters"),
#                                                                              prior_r = c(10, 10),
#                                                                              prior_k = c(5, 10),
#                                                                              mean_generation_interval  = 5.2,
#                                                                              prior_number_yearly_mutations = c(14, 0.5),
#                                                                              testing_proba = 1.0,
#                                                                              sequencing_proba = sequencing_probas_nz_periods[ii],
#                                                                              warmup = 500,
#                                                                              iter = 1000,
#                                                                              chains = 4,
#                                                                              cores = 4,
#                                                                              thin = 1,
#                                                                              control = list(adapt_delta = 0.99),
#                                                                              init = lapply(1:4, FUN = function(x) estRodis_init_params_model_two()))
#   
#   # save results of parameter estimation
#   saveRDS(results_parameter_estimation_model_two, file = paste0("results/new_zealand/parameter_estimations/model_two/parameter_estimates_model_two_new_zealand_p_test_100_", ii, ".rds"))
#   
# }
# 
# 
# results_parameter_estimation_model_two <- estRodis_estimate_parameters_two(clusters_size = data_nz_complete %>% pull("cluster_size"),
#                                                                            clusters_freq = data_nz_complete %>% pull("n_clusters"),
#                                                                            prior_r = c(10, 10),
#                                                                            prior_k = c(5, 10),
#                                                                            mean_generation_interval  = 5.2,
#                                                                            prior_number_yearly_mutations = c(14, 0.5),
#                                                                            testing_proba = 1.0,
#                                                                            sequencing_proba = sequencing_proba_nz,
#                                                                            warmup = 500,
#                                                                            iter = 1000,
#                                                                            chains = 4,
#                                                                            cores = 4,
#                                                                            thin = 1,
#                                                                            control = list(adapt_delta = 0.99),
#                                                                            init = lapply(1:4, FUN = function(x) estRodis_init_params_model_two()))
# 
# # save results of parameter estimation
# saveRDS(results_parameter_estimation_model_two, file = paste0("results/new_zealand/parameter_estimations/model_two/parameter_estimates_model_two_new_zealand_p_test_100_all.rds"))
# 


# apply model four ----

# low mutation probability and testing probability 0.5
for (ii in 1:length(periods_nz)) {
  
  data_nz_period <- data_nz %>% filter(period == periods_nz[ii])
  
  results_parameter_estimation_model_four <- estRodis_estimate_parameters_four(clusters_size = data_nz_period %>% pull("cluster_size"),
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
  saveRDS(results_parameter_estimation_model_four, file = paste0("results/new_zealand/parameter_estimations/model_four/parameter_estimates_model_four_new_zealand_p_mut_low_p_test_050_", ii, ".rds"))
  
}


results_parameter_estimation_model_four <- estRodis_estimate_parameters_four(clusters_size = data_nz_complete %>% pull("cluster_size"),
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
saveRDS(results_parameter_estimation_model_four, file = paste0("results/new_zealand/parameter_estimations/model_four/parameter_estimates_model_four_new_zealand_p_mut_low_p_test_050_all.rds"))


# low mutation probability and testing probability 0.8
for (ii in 1:length(periods_nz)) {
  
  data_nz_period <- data_nz %>% filter(period == periods_nz[ii])
  
  results_parameter_estimation_model_four <- estRodis_estimate_parameters_four(clusters_size = data_nz_period %>% pull("cluster_size"),
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
  saveRDS(results_parameter_estimation_model_four, file = paste0("results/new_zealand/parameter_estimations/model_four/parameter_estimates_model_four_new_zealand_p_mut_low_p_test_080_", ii, ".rds"))
  
}


results_parameter_estimation_model_four <- estRodis_estimate_parameters_four(clusters_size = data_nz_complete %>% pull("cluster_size"),
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
saveRDS(results_parameter_estimation_model_four, file = paste0("results/new_zealand/parameter_estimations/model_four/parameter_estimates_model_four_new_zealand_p_mut_low_p_test_080_all.rds"))


# low mutation probability and testing probability 1.0
for (ii in 1:length(periods_nz)) {
  
  data_nz_period <- data_nz %>% filter(period == periods_nz[ii])
  
  results_parameter_estimation_model_four <- estRodis_estimate_parameters_four(clusters_size = data_nz_period %>% pull("cluster_size"),
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
  saveRDS(results_parameter_estimation_model_four, file = paste0("results/new_zealand/parameter_estimations/model_four/parameter_estimates_model_four_new_zealand_p_mut_low_p_test_100_", ii, ".rds"))
  
}


results_parameter_estimation_model_four <- estRodis_estimate_parameters_four(clusters_size = data_nz_complete %>% pull("cluster_size"),
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
saveRDS(results_parameter_estimation_model_four, file = paste0("results/new_zealand/parameter_estimations/model_four/parameter_estimates_model_four_new_zealand_p_mut_low_p_test_100_all.rds"))


# central mutation probability and testing probability 0.5
for (ii in 1:length(periods_nz)) {
  
  data_nz_period <- data_nz %>% filter(period == periods_nz[ii])
  
  results_parameter_estimation_model_four <- estRodis_estimate_parameters_four(clusters_size = data_nz_period %>% pull("cluster_size"),
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
  saveRDS(results_parameter_estimation_model_four, file = paste0("results/new_zealand/parameter_estimations/model_four/parameter_estimates_model_four_new_zealand_p_mut_central_p_test_050_", ii, ".rds"))
  
}


results_parameter_estimation_model_four <- estRodis_estimate_parameters_four(clusters_size = data_nz_complete %>% pull("cluster_size"),
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
saveRDS(results_parameter_estimation_model_four, file = paste0("results/new_zealand/parameter_estimations/model_four/parameter_estimates_model_four_new_zealand_p_mut_central_p_test_050_all.rds"))


# central mutation probability and testing probability 0.8
for (ii in 1:length(periods_nz)) {
  
  data_nz_period <- data_nz %>% filter(period == periods_nz[ii])
  
  results_parameter_estimation_model_four <- estRodis_estimate_parameters_four(clusters_size = data_nz_period %>% pull("cluster_size"),
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
  saveRDS(results_parameter_estimation_model_four, file = paste0("results/new_zealand/parameter_estimations/model_four/parameter_estimates_model_four_new_zealand_p_mut_central_p_test_080_", ii, ".rds"))
  
}


results_parameter_estimation_model_four <- estRodis_estimate_parameters_four(clusters_size = data_nz_complete %>% pull("cluster_size"),
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
saveRDS(results_parameter_estimation_model_four, file = paste0("results/new_zealand/parameter_estimations/model_four/parameter_estimates_model_four_new_zealand_p_mut_central_p_test_080_all.rds"))


# central mutation probability and testing probability 1.0
for (ii in 1:length(periods_nz)) {
  
  data_nz_period <- data_nz %>% filter(period == periods_nz[ii])
  
  results_parameter_estimation_model_four <- estRodis_estimate_parameters_four(clusters_size = data_nz_period %>% pull("cluster_size"),
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
  saveRDS(results_parameter_estimation_model_four, file = paste0("results/new_zealand/parameter_estimations/model_four/parameter_estimates_model_four_new_zealand_p_mut_central_p_test_100_", ii, ".rds"))
  
}


results_parameter_estimation_model_four <- estRodis_estimate_parameters_four(clusters_size = data_nz_complete %>% pull("cluster_size"),
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
saveRDS(results_parameter_estimation_model_four, file = paste0("results/new_zealand/parameter_estimations/model_four/parameter_estimates_model_four_new_zealand_p_mut_central_p_test_100_all.rds"))


# high mutation probability and testing probability 0.5
for (ii in 1:length(periods_nz)) {
  
  data_nz_period <- data_nz %>% filter(period == periods_nz[ii])
  
  results_parameter_estimation_model_four <- estRodis_estimate_parameters_four(clusters_size = data_nz_period %>% pull("cluster_size"),
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
  saveRDS(results_parameter_estimation_model_four, file = paste0("results/new_zealand/parameter_estimations/model_four/parameter_estimates_model_four_new_zealand_p_mut_high_p_test_050_", ii, ".rds"))
  
}


results_parameter_estimation_model_four <- estRodis_estimate_parameters_four(clusters_size = data_nz_complete %>% pull("cluster_size"),
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
saveRDS(results_parameter_estimation_model_four, file = paste0("results/new_zealand/parameter_estimations/model_four/parameter_estimates_model_four_new_zealand_p_mut_high_p_test_050_all.rds"))


# high mutation probability and testing probability 0.8
for (ii in 1:length(periods_nz)) {
  
  data_nz_period <- data_nz %>% filter(period == periods_nz[ii])
  
  results_parameter_estimation_model_four <- estRodis_estimate_parameters_four(clusters_size = data_nz_period %>% pull("cluster_size"),
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
  saveRDS(results_parameter_estimation_model_four, file = paste0("results/new_zealand/parameter_estimations/model_four/parameter_estimates_model_four_new_zealand_p_mut_high_p_test_080_", ii, ".rds"))
  
}


results_parameter_estimation_model_four <- estRodis_estimate_parameters_four(clusters_size = data_nz_complete %>% pull("cluster_size"),
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
saveRDS(results_parameter_estimation_model_four, file = paste0("results/new_zealand/parameter_estimations/model_four/parameter_estimates_model_four_new_zealand_p_mut_high_p_test_080_all.rds"))


# high mutation probability and testing probability 1.0
for (ii in 1:length(periods_nz)) {
  
  data_nz_period <- data_nz %>% filter(period == periods_nz[ii])
  
  results_parameter_estimation_model_four <- estRodis_estimate_parameters_four(clusters_size = data_nz_period %>% pull("cluster_size"),
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
  saveRDS(results_parameter_estimation_model_four, file = paste0("results/new_zealand/parameter_estimations/model_four/parameter_estimates_model_four_new_zealand_p_mut_high_p_test_100_", ii, ".rds"))
  
}


results_parameter_estimation_model_four <- estRodis_estimate_parameters_four(clusters_size = data_nz_complete %>% pull("cluster_size"),
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
saveRDS(results_parameter_estimation_model_four, file = paste0("results/new_zealand/parameter_estimations/model_four/parameter_estimates_model_four_new_zealand_p_mut_high_p_test_100_all.rds"))


# # apply model six ----
# 
# # testing probability 0.5
# for (ii in 1:length(periods_nz)) {
#   
#   data_nz_period <- data_nz %>% filter(period == periods_nz[ii])
#   
#   results_parameter_estimation_model_six <- estRodis_estimate_parameters_six(clusters_size = data_nz_period %>% pull("cluster_size"),
#                                                                              clusters_freq = data_nz_period %>% pull("n_clusters"),
#                                                                              prior_r = c(10, 10),
#                                                                              prior_k = c(5, 10),
#                                                                              prior_mutation = c(27, 68),
#                                                                              testing_proba = 0.5,
#                                                                              sequencing_proba = sequencing_probas_nz_periods[ii],
#                                                                              warmup = 500,
#                                                                              iter = 1000,
#                                                                              chains = 4,
#                                                                              cores = 4,
#                                                                              thin = 1,
#                                                                              control = list(adapt_delta = 0.99),
#                                                                              init = lapply(1:4, FUN = function(x) estRodis_init_params_model_six()))
#   
#   # save results of parameter estimation
#   saveRDS(results_parameter_estimation_model_six, file = paste0("results/new_zealand/parameter_estimations/model_six/parameter_estimates_model_six_new_zealand_p_test_050_", ii, ".rds"))
#   
# }
# 
# 
# results_parameter_estimation_model_six <- estRodis_estimate_parameters_six(clusters_size = data_nz_complete %>% pull("cluster_size"),
#                                                                            clusters_freq = data_nz_complete %>% pull("n_clusters"),
#                                                                            prior_r = c(10, 10),
#                                                                            prior_k = c(5, 10),
#                                                                            prior_mutation = c(27, 68),
#                                                                            testing_proba = 0.5,
#                                                                            sequencing_proba = sequencing_proba_nz,
#                                                                            warmup = 500,
#                                                                            iter = 1000,
#                                                                            chains = 4,
#                                                                            cores = 4,
#                                                                            thin = 1,
#                                                                            control = list(adapt_delta = 0.99),
#                                                                            init = lapply(1:4, FUN = function(x) estRodis_init_params_model_six()))
# 
# # save results of parameter estimation
# saveRDS(results_parameter_estimation_model_six, file = paste0("results/new_zealand/parameter_estimations/model_six/parameter_estimates_model_six_new_zealand_p_test_050_all.rds"))
# 
# 
# # testing probability 0.8
# for (ii in 1:length(periods_nz)) {
#   
#   data_nz_period <- data_nz %>% filter(period == periods_nz[ii])
#   
#   results_parameter_estimation_model_six <- estRodis_estimate_parameters_six(clusters_size = data_nz_period %>% pull("cluster_size"),
#                                                                              clusters_freq = data_nz_period %>% pull("n_clusters"),
#                                                                              prior_r = c(10, 10),
#                                                                              prior_k = c(5, 10),
#                                                                              prior_mutation = c(27, 68),
#                                                                              testing_proba = 0.8,
#                                                                              sequencing_proba = sequencing_probas_nz_periods[ii],
#                                                                              warmup = 500,
#                                                                              iter = 1000,
#                                                                              chains = 4,
#                                                                              cores = 4,
#                                                                              thin = 1,
#                                                                              control = list(adapt_delta = 0.99),
#                                                                              init = lapply(1:4, FUN = function(x) estRodis_init_params_model_six()))
#   
#   # save results of parameter estimation
#   saveRDS(results_parameter_estimation_model_six, file = paste0("results/new_zealand/parameter_estimations/model_six/parameter_estimates_model_six_new_zealand_p_test_080_", ii, ".rds"))
#   
# }
# 
# 
# # testing probability 1.0
# results_parameter_estimation_model_six <- estRodis_estimate_parameters_six(clusters_size = data_nz_complete %>% pull("cluster_size"),
#                                                                            clusters_freq = data_nz_complete %>% pull("n_clusters"),
#                                                                            prior_r = c(10, 10),
#                                                                            prior_k = c(5, 10),
#                                                                            prior_mutation = c(27, 68),
#                                                                            testing_proba = 0.8,
#                                                                            sequencing_proba = sequencing_proba_nz,
#                                                                            warmup = 500,
#                                                                            iter = 1000,
#                                                                            chains = 4,
#                                                                            cores = 4,
#                                                                            thin = 1,
#                                                                            control = list(adapt_delta = 0.99),
#                                                                            init = lapply(1:4, FUN = function(x) estRodis_init_params_model_six()))
# 
# # save results of parameter estimation
# saveRDS(results_parameter_estimation_model_six, file = paste0("results/new_zealand/parameter_estimations/model_six/parameter_estimates_model_six_new_zealand_p_test_080_all.rds"))
# 
# 
# for (ii in 1:length(periods_nz)) {
#   
#   data_nz_period <- data_nz %>% filter(period == periods_nz[ii])
#   
#   results_parameter_estimation_model_six <- estRodis_estimate_parameters_six(clusters_size = data_nz_period %>% pull("cluster_size"),
#                                                                              clusters_freq = data_nz_period %>% pull("n_clusters"),
#                                                                              prior_r = c(10, 10),
#                                                                              prior_k = c(5, 10),
#                                                                              prior_mutation = c(27, 68),
#                                                                              testing_proba = 1.0,
#                                                                              sequencing_proba = sequencing_probas_nz_periods[ii],
#                                                                              warmup = 500,
#                                                                              iter = 1000,
#                                                                              chains = 4,
#                                                                              cores = 4,
#                                                                              thin = 1,
#                                                                              control = list(adapt_delta = 0.99),
#                                                                              init = lapply(1:4, FUN = function(x) estRodis_init_params_model_six()))
#   
#   # save results of parameter estimation
#   saveRDS(results_parameter_estimation_model_six, file = paste0("results/new_zealand/parameter_estimations/model_six/parameter_estimates_model_six_new_zealand_p_test_100_", ii, ".rds"))
#   
# }
# 
# 
# results_parameter_estimation_model_six <- estRodis_estimate_parameters_six(clusters_size = data_nz_complete %>% pull("cluster_size"),
#                                                                            clusters_freq = data_nz_complete %>% pull("n_clusters"),
#                                                                            prior_r = c(10, 10),
#                                                                            prior_k = c(5, 10),
#                                                                            prior_mutation = c(27, 68),
#                                                                            testing_proba = 1.0,
#                                                                            sequencing_proba = sequencing_proba_nz,
#                                                                            warmup = 500,
#                                                                            iter = 1000,
#                                                                            chains = 4,
#                                                                            cores = 4,
#                                                                            thin = 1,
#                                                                            control = list(adapt_delta = 0.99),
#                                                                            init = lapply(1:4, FUN = function(x) estRodis_init_params_model_six()))
# 
# # save results of parameter estimation
# saveRDS(results_parameter_estimation_model_six, file = paste0("results/new_zealand/parameter_estimations/model_six/parameter_estimates_model_six_new_zealand_p_test_100_all.rds"))
