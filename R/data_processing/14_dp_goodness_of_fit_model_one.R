
# read summary tables of the number of confirmed cases, the number of sequences and the number of clusters 
data_cases_sequences_clusters_ch_2021_months <- read_csv(file = path_data_cases_sequences_clusters_ch_2021_months)
data_cases_sequences_clusters_dk_2021_months <- read_csv(file = path_data_cases_sequences_clusters_dk_2021_months)
data_cases_sequences_clusters_de_2021_months <- read_csv(file = path_data_cases_sequences_clusters_de_2021_months)

data_cases_sequences_clusters_ch_dk_de_2021_months <- rbind(data_cases_sequences_clusters_ch_2021_months %>% mutate(country = "Switzerland"),
                                                            data_cases_sequences_clusters_dk_2021_months %>% mutate(country = "Denmark"),
                                                            data_cases_sequences_clusters_de_2021_months %>% mutate(country = "Germany")) %>%
  filter(Month != "Total")

# read results of parameter estimates
results_model_one_ch_dk_de_2021_months <- read_csv(file = path_results_model_one_ch_dk_de_processed)

list_months <- c("January", "February", "March", "April", "May", "June", "July", "August", "September", "October", "November", "December")


data_distribution_mean_ch_dk_de_2021_months <- data.frame(matrix(nrow = 0, ncol = 4)) 
names(data_distribution_mean_ch_dk_de_2021_months) <- c("month", "country", "size", "probability")

for (ii in 1:nrow(results_model_one_ch_dk_de_2021_months)) {
  
  max_cluster_size_ii <- data_cases_sequences_clusters_ch_dk_de_2021_months %>%
    filter(country == results_model_one_ch_dk_de_2021_months$country[ii],
           Month == list_months[results_model_one_ch_dk_de_2021_months$month[ii]]) %>%
    pull(Solc)
  
  data_distribution_mean_ch_dk_de_2021_month_ii <- data.frame(month = rep(x = results_model_one_ch_dk_de_2021_months$month[ii], times = max_cluster_size_ii),
                                                              country = rep(x = results_model_one_ch_dk_de_2021_months$country[ii]),
                                                              size = 1:max_cluster_size_ii,
                                                              probability = distribution_cluster_size_testing(N = max_cluster_size_ii,
                                                                                                              Re = results_model_one_ch_dk_de_2021_months$R_estimate[ii],
                                                                                                              k = results_model_one_ch_dk_de_2021_months$k_estimate[ii],
                                                                                                              p_mut = results_model_one_ch_dk_de_2021_months$mutation_proba_estimate[ii],
                                                                                                              p_detec = results_model_one_ch_dk_de_2021_months$detection_proba_estimate[ii]))
  
  data_distribution_mean_ch_dk_de_2021_months <- tibble(rbind(data_distribution_mean_ch_dk_de_2021_months, data_distribution_mean_ch_dk_de_2021_month_ii))
  
  print(ii)
  
}

write_csv(x = data_distribution_mean_ch_dk_de_2021_months,
          file = path_results_goodness_fit_mean_model_one_ch_dk_de)


data_distribution_low_ch_dk_de_2021_months <- data.frame(matrix(nrow = 0, ncol = 4)) 
names(data_distribution_low_ch_dk_de_2021_months) <- c("month", "country", "size", "probability")

for (ii in 1:nrow(results_model_one_ch_dk_de_2021_months)) {
  
  max_cluster_size_ii <- data_cases_sequences_clusters_ch_dk_de_2021_months %>%
    filter(country == results_model_one_ch_dk_de_2021_months$country[ii],
           Month == list_months[results_model_one_ch_dk_de_2021_months$month[ii]]) %>%
    pull(Solc)
  
  data_distribution_low_ch_dk_de_2021_month_ii <- data.frame(month = rep(x = results_model_one_ch_dk_de_2021_months$month[ii], times = max_cluster_size_ii),
                                                             country = rep(x = results_model_one_ch_dk_de_2021_months$country[ii]),
                                                             size = 1:max_cluster_size_ii,
                                                             probability = distribution_cluster_size_testing(N = max_cluster_size_ii,
                                                                                                             Re = results_model_one_ch_dk_de_2021_months$R_lower_cred_int[ii],
                                                                                                             k = results_model_one_ch_dk_de_2021_months$k_upper_cred_int[ii],
                                                                                                             p_mut = results_model_one_ch_dk_de_2021_months$mutation_proba_upper_cred_int[ii],
                                                                                                             p_detec = results_model_one_ch_dk_de_2021_months$detection_proba_lower_cred_int[ii]))
  
  data_distribution_low_ch_dk_de_2021_months <- tibble(rbind(data_distribution_low_ch_dk_de_2021_months, data_distribution_low_ch_dk_de_2021_month_ii))
  
  print(ii)
  
}

write_csv(x = data_distribution_low_ch_dk_de_2021_months,
          file = path_results_goodness_fit_low_model_one_ch_dk_de)


data_distribution_high_ch_dk_de_2021_months <- data.frame(matrix(nrow = 0, ncol = 4)) 
names(data_distribution_high_ch_dk_de_2021_months) <- c("month", "country", "size", "probability")

for (ii in 1:nrow(results_model_one_ch_dk_de_2021_months)) {
  
  max_cluster_size_ii <- data_cases_sequences_clusters_ch_dk_de_2021_months %>%
    filter(country == results_model_one_ch_dk_de_2021_months$country[ii],
           Month == list_months[results_model_one_ch_dk_de_2021_months$month[ii]]) %>%
    pull(Solc)
  
  data_distribution_high_ch_dk_de_2021_month_ii <- data.frame(month = rep(x = results_model_one_ch_dk_de_2021_months$month[ii], times = max_cluster_size_ii),
                                                              country = rep(x = results_model_one_ch_dk_de_2021_months$country[ii]),
                                                              size = 1:max_cluster_size_ii,
                                                              probability = distribution_cluster_size_testing(N = max_cluster_size_ii,
                                                                                                              Re = results_model_one_ch_dk_de_2021_months$R_upper_cred_int[ii],
                                                                                                              k = results_model_one_ch_dk_de_2021_months$k_lower_cred_int[ii],
                                                                                                              p_mut = results_model_one_ch_dk_de_2021_months$mutation_proba_lower_cred_int[ii],
                                                                                                              p_detec = results_model_one_ch_dk_de_2021_months$detection_proba_upper_cred_int[ii]))
  
  data_distribution_high_ch_dk_de_2021_months <- tibble(rbind(data_distribution_high_ch_dk_de_2021_months, data_distribution_high_ch_dk_de_2021_month_ii))
  
  print(ii)
  
}

write_csv(x = data_distribution_high_ch_dk_de_2021_months,
          file = path_results_goodness_fit_high_model_one_ch_dk_de)

