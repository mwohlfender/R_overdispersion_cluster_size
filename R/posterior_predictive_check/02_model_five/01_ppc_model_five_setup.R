
# posterior predictive check
# define parameters for simulation


# define paths
paths_data_cases_sequences_clusters_countries_2021_months <- c(path_data_cases_sequences_clusters_ch_2021_months, path_data_cases_sequences_clusters_dk_2021_months, path_data_cases_sequences_clusters_de_2021_months)
paths_results_model_five <- c(path_results_model_five_ch, path_results_model_five_dk, path_results_model_five_de)

# number of combinations of parameters, 
# for each vector of parameters each entry is sampled uniformly at random and independently from the other entries of the vector 
# from the samples of the posterior distribution of the respective parameter
n_parameter_combinations <- 50

# number of times simulation of clusters is repeated for each combination of parameters
n_repetitions <- 1

# first month
m0 <- ymd("2021-01-01")

# countries
countries <- c("switzerland", "denmark", "germany")
n_countries <- length(countries)

# define output, here the combinations of parameters for the simulations will be stored
data_parameters <- data.frame(matrix(data=0, nrow=n_countries*12*n_parameter_combinations*n_repetitions, ncol=11))
names(data_parameters) <- c("country", "month", "index_param_comb", "index_repetition", "n_clusters", "max_cluster_size", "R", "k", "mutation_proba", "testing_proba", "sequencing_proba")

data_parameters <- data_parameters %>% mutate(country=rep(countries, each=12*n_parameter_combinations*n_repetitions),
                                              month=formatC(rep(rep(1:12, each=n_parameter_combinations*n_repetitions), times=n_countries), width=2, flag="0"),
                                              index_param_comb=formatC(rep(rep(1:n_parameter_combinations, each=n_repetitions), times=n_countries*12), width=max(ceiling(log(x=n_parameter_combinations+1, base=10)),1), flag="0"),
                                              index_repetition=formatC(rep(1:n_repetitions, times=n_countries*12*n_parameter_combinations), width=max(ceiling(log(x=n_repetitions+1, base=10)),1), flag="0"),
                                              max_cluster_size=2500)

# read estimated monthly sequencing probabilities of Switzerland, Denmark and Germany during 2021
sequencing_probas_ch_2021_months <- read_csv(file = path_sequencing_probas_ch_2021_months)
sequencing_probas_dk_2021_months <- read_csv(file = path_sequencing_probas_dk_2021_months)
sequencing_probas_de_2021_months <- read_csv(file = path_sequencing_probas_de_2021_months)


# go through countries
for (cc in 1:n_countries) {
  
  # read cluster data from country
  data_cases_sequences_clusters_cc <- read_delim(file = paths_data_cases_sequences_clusters_countries_2021_months[cc])
  
  if (cc == 1) {
    
    sequencing_probas_cc <- sequencing_probas_ch_2021_months %>% pull(seq_proba_ch)
    
  }
  
  if (cc == 2) {
    
    sequencing_probas_cc <- sequencing_probas_dk_2021_months %>% pull(seq_proba_dk) 
    
  }
  
  if (cc == 3) {
    
    sequencing_probas_cc <- sequencing_probas_de_2021_months %>% pull(seq_proba_de)
    
  }
  
  # go through all months 
  for (mm in 1:12) {
    
    data_results_details_cc_mm <- readRDS(file = paste0(paths_results_model_five[cc], as.character(mm), ".rds"))
    
    data_results_details_cc_mm_samples <- rstan::extract(data_results_details_cc_mm, permuted = FALSE, inc_warmup = FALSE)
    
    n_samples <- nrow(data_results_details_cc_mm_samples[,,1]) * ncol(data_results_details_cc_mm_samples[,,1])
    
    parameters <- data.frame(n_clusters = rep(data_cases_sequences_clusters_cc$Noc[mm], n_parameter_combinations),
                             R = sample(data_results_details_cc_mm_samples[,,1][1:n_samples], n_parameter_combinations),
                             k = sample(data_results_details_cc_mm_samples[,,2][1:n_samples], n_parameter_combinations),
                             mutation_proba = sample(data_results_details_cc_mm_samples[,,3][1:n_samples], n_parameter_combinations),
                             testing_proba = sample(data_results_details_cc_mm_samples[,,4][1:n_samples], n_parameter_combinations),
                             sequencing_proba = rep(sequencing_probas_cc[mm], n_parameter_combinations))
    
    parameters <- parameters %>% slice(rep(1:nrow(parameters), each=n_repetitions))
    
    index_start <- (cc-1)*12*n_parameter_combinations*n_repetitions + (mm-1)*n_parameter_combinations*n_repetitions
    
    data_parameters[(index_start+1):(index_start+n_parameter_combinations*n_repetitions),
                    c("n_clusters", "R", "k", "mutation_proba", "testing_proba", "sequencing_proba")] <- parameters
    
  }
  
}

write_csv(data_parameters, file = path_data_post_pred_model_five_parameters_grid)

write.table(1:nrow(data_parameters), file = path_data_post_pred_model_five_index_parameters, sep = "", append=F, row.names=F, col.names=F)
