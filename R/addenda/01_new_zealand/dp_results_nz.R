

# read sequencing probabilities from New Zealand ----
sequencing_probas_nz_periods_0 <- readRDS(path_sequencing_probas_nz_periods)

# overall sequencing probability
sequencing_proba_nz <- sum(sequencing_probas_nz_periods_0 %>% pull(n_sequences)) / sum(sequencing_probas_nz_periods_0 %>% pull(n_cases))



# read results of parameter estimations of model 1 ----

# New Zealand
if (file.exists(paste0(path_results_model_one_nz, "all.rds"))) {
  
  results_model_one_nz_all <- readRDS(paste0(path_results_model_one_nz, "all.rds"))
  
} else {
  
  results_model_one_nz_all <- 0
  
}

if (file.exists(paste0(path_results_model_one_nz, "1.rds"))) {
  
  results_model_one_nz_period_01 <- readRDS(paste0(path_results_model_one_nz, "1.rds"))
  
} else {
  
  results_model_one_nz_period_01 <- 0
  
}

if (file.exists(paste0(path_results_model_one_nz, "2.rds"))) {
  
  results_model_one_nz_period_02 <- readRDS(paste0(path_results_model_one_nz, "2.rds"))
  
} else {
  
  results_model_one_nz_period_02 <- 0
  
}

if (file.exists(paste0(path_results_model_one_nz, "3.rds"))) {
  
  results_model_one_nz_period_03 <- readRDS(paste0(path_results_model_one_nz, "3.rds"))
  
} else {
  
  results_model_one_nz_period_03 <- 0
  
}

if (file.exists(paste0(path_results_model_one_nz, "4.rds"))) {
  
  results_model_one_nz_period_04 <- readRDS(paste0(path_results_model_one_nz, "4.rds"))
  
} else {
  
  results_model_one_nz_period_04 <- 0
  
}

# create list of results
list_results_model_one_nz_periods <- list(results_model_one_nz_all,
                                          results_model_one_nz_period_01, results_model_one_nz_period_02,
                                          results_model_one_nz_period_03, results_model_one_nz_period_04)

# create summary of results of parameter estimations of model 1
results_model_one_nz_periods <- data.frame(matrix(data = 0, nrow = 5, ncol = 27))
names(results_model_one_nz_periods) <- c("period", "country", "sequencing_proba", 
                                         "R_estimate", "R_lower_cred_int", "R_upper_cred_int", "R_Rhat",
                                         "k_estimate", "k_lower_cred_int", "k_upper_cred_int", "k_Rhat",
                                         "number_yearly_mutations_estimate", "number_yearly_mutations_lower_cred_int", 
                                         "number_yearly_mutations_upper_cred_int", "number_yearly_mutations_Rhat",
                                         "mutation_proba_estimate", "mutation_proba_lower_cred_int", "mutation_proba_upper_cred_int", "mutation_proba_Rhat",
                                         "testing_proba_estimate", "testing_proba_lower_cred_int", "testing_proba_upper_cred_int", "testing_proba_Rhat",
                                         "detection_proba_estimate", "detection_proba_lower_cred_int", "detection_proba_upper_cred_int", "detection_proba_Rhat")

results_model_one_nz_periods <- results_model_one_nz_periods %>%
  mutate(period = c("all", as.character(1:4)),
         country = rep(x = "New Zealand", times = 5),
         sequencing_proba = c(sequencing_proba_nz, sequencing_probas_nz_periods_0 %>% pull(prop_cases_sequenced)))

results_model_one_nz_n_divergent <- data.frame(matrix(data = 0, nrow = 5, ncol = 3))
names(results_model_one_nz_n_divergent) <- c("period", "country", "n_divergent")

results_model_one_nz_n_divergent <- results_model_one_nz_n_divergent %>%
  mutate(period = c("all", as.character(1:4)),
         country = rep(x = "New Zealand", times = 5))

for(ii in 1:5) {
  
  results_ii <- list_results_model_one_nz_periods[[ii]]
  
  if (!(is.numeric(results_ii))) {
    
    results_model_one_nz_periods$R_estimate[ii] <- get_posterior_mean(results_ii, par = c("R"))[, "mean-all chains"]
    results_model_one_nz_periods$R_lower_cred_int[ii] <- as.data.frame(summary(results_ii)[[1]])[c("R"), "2.5%"]
    results_model_one_nz_periods$R_upper_cred_int[ii] <- as.data.frame(summary(results_ii)[[1]])[c("R"), "97.5%"]
    results_model_one_nz_periods$R_Rhat[ii] <- as.data.frame(summary(results_ii)[[1]])[c("R"), "Rhat"]
    
    results_model_one_nz_periods$k_estimate[ii] <- get_posterior_mean(results_ii, par = c("k"))[, "mean-all chains"]
    results_model_one_nz_periods$k_lower_cred_int[ii] <- as.data.frame(summary(results_ii)[[1]])[c("k"), "2.5%"]
    results_model_one_nz_periods$k_upper_cred_int[ii] <- as.data.frame(summary(results_ii)[[1]])[c("k"), "97.5%"]
    results_model_one_nz_periods$k_Rhat[ii] <- as.data.frame(summary(results_ii)[[1]])[c("k"), "Rhat"]
    
    results_model_one_nz_periods$number_yearly_mutations_estimate[ii] <- get_posterior_mean(results_ii, par = c("number_yearly_mutations"))[, "mean-all chains"]
    results_model_one_nz_periods$number_yearly_mutations_lower_cred_int[ii] <- as.data.frame(summary(results_ii)[[1]])[c("number_yearly_mutations"), "2.5%"]
    results_model_one_nz_periods$number_yearly_mutations_upper_cred_int[ii] <- as.data.frame(summary(results_ii)[[1]])[c("number_yearly_mutations"), "97.5%"]
    results_model_one_nz_periods$number_yearly_mutations_Rhat[ii] <- as.data.frame(summary(results_ii)[[1]])[c("number_yearly_mutations"), "Rhat"]
    
    results_model_one_nz_periods$mutation_proba_estimate[ii] <- get_posterior_mean(results_ii, par = c("mutation_proba"))[, "mean-all chains"]
    results_model_one_nz_periods$mutation_proba_lower_cred_int[ii] <- as.data.frame(summary(results_ii)[[1]])[c("mutation_proba"), "2.5%"]
    results_model_one_nz_periods$mutation_proba_upper_cred_int[ii] <- as.data.frame(summary(results_ii)[[1]])[c("mutation_proba"), "97.5%"]
    results_model_one_nz_periods$mutation_proba_Rhat[ii] <- as.data.frame(summary(results_ii)[[1]])[c("mutation_proba"), "Rhat"]
    
    results_model_one_nz_periods$testing_proba_estimate[ii] <- get_posterior_mean(results_ii, par = c("testing_proba"))[, "mean-all chains"]
    results_model_one_nz_periods$testing_proba_lower_cred_int[ii] <- as.data.frame(summary(results_ii)[[1]])[c("testing_proba"), "2.5%"]
    results_model_one_nz_periods$testing_proba_upper_cred_int[ii] <- as.data.frame(summary(results_ii)[[1]])[c("testing_proba"), "97.5%"]
    results_model_one_nz_periods$testing_proba_Rhat[ii] <- as.data.frame(summary(results_ii)[[1]])[c("testing_proba"), "Rhat"]
    
    results_model_one_nz_periods$detection_proba_estimate[ii] <- get_posterior_mean(results_ii, par = c("detection_proba"))[, "mean-all chains"]
    results_model_one_nz_periods$detection_proba_lower_cred_int[ii] <- as.data.frame(summary(results_ii)[[1]])[c("detection_proba"), "2.5%"]
    results_model_one_nz_periods$detection_proba_upper_cred_int[ii] <- as.data.frame(summary(results_ii)[[1]])[c("detection_proba"), "97.5%"]
    results_model_one_nz_periods$detection_proba_Rhat[ii] <- as.data.frame(summary(results_ii)[[1]])[c("detection_proba"), "Rhat"]
    
    results_model_one_nz_n_divergent$n_divergent[ii] <- get_num_divergent(results_ii)
    
    ggsave(plot = rstan::traceplot(results_ii, inc_warmup = TRUE),
           filename = paste0("plots/new_zealand/model_one/traceplot_model_one_nz_period_", results_model_one_nz_periods$period[ii], ".png"))
    
  }
  
}

# save `results_model_one_nz_periods` as csv file
write_csv(x = results_model_one_nz_periods, file = path_results_model_one_nz_periods_processed)



# read results of parameter estimations of model 2 ----

# New Zealand

# p_test = 0.50 ----
if (file.exists(paste0(path_results_model_two_nz, "p_test_050_all.rds"))) {
  
  results_model_two_nz_p_test_050_all <- readRDS(paste0(path_results_model_two_nz, "p_test_050_all.rds"))
  
} else {
  
  results_model_two_nz_p_test_050_all <- 0
  
}

if (file.exists(paste0(path_results_model_two_nz, "p_test_050_1.rds"))) {
  
  results_model_two_nz_p_test_050_period_01 <- readRDS(paste0(path_results_model_two_nz, "p_test_050_1.rds"))
  
} else {
  
  results_model_two_nz_p_test_050_period_01 <- 0
  
}

if (file.exists(paste0(path_results_model_two_nz, "p_test_050_2.rds"))) {
  
  results_model_two_nz_p_test_050_period_02 <- readRDS(paste0(path_results_model_two_nz, "p_test_050_2.rds"))
  
} else {
  
  results_model_two_nz_p_test_050_period_02 <- 0
  
}

if (file.exists(paste0(path_results_model_two_nz, "p_test_050_3.rds"))) {
  
  results_model_two_nz_p_test_050_period_03 <- readRDS(paste0(path_results_model_two_nz, "p_test_050_3.rds"))
  
} else {
  
  results_model_two_nz_p_test_050_period_03 <- 0
  
}

if (file.exists(paste0(path_results_model_two_nz, "p_test_050_4.rds"))) {
  
  results_model_two_nz_p_test_050_period_04 <- readRDS(paste0(path_results_model_two_nz, "p_test_050_4.rds"))
  
} else {
  
  results_model_two_nz_p_test_050_period_04 <- 0
  
}

# create list of results
list_results_model_two_nz_p_test_050_periods <- list(results_model_two_nz_p_test_050_all,
                                                     results_model_two_nz_p_test_050_period_01, results_model_two_nz_p_test_050_period_02,
                                                     results_model_two_nz_p_test_050_period_03, results_model_two_nz_p_test_050_period_04)

# create summary of results of parameter estimations
results_model_two_nz_p_test_050_periods <- data.frame(matrix(data = 0, nrow = 5, ncol = 21))
names(results_model_two_nz_p_test_050_periods) <- c("period", "country", "sequencing_proba", 
                                                    "R_estimate", "R_lower_cred_int", "R_upper_cred_int", "R_Rhat",
                                                    "k_estimate", "k_lower_cred_int", "k_upper_cred_int", "k_Rhat",
                                                    "number_yearly_mutations_estimate", "number_yearly_mutations_lower_cred_int", 
                                                    "number_yearly_mutations_upper_cred_int", "number_yearly_mutations_Rhat",
                                                    "mutation_proba_estimate", "mutation_proba_lower_cred_int", "mutation_proba_upper_cred_int", "mutation_proba_Rhat",
                                                    "testing_proba",
                                                    "detection_proba")

results_model_two_nz_p_test_050_periods <- results_model_two_nz_p_test_050_periods %>%
  mutate(period = c("all", as.character(1:4)),
         country = rep(x = "New Zealand", times = 5),
         testing_proba = 0.50,
         sequencing_proba = c(sequencing_proba_nz, sequencing_probas_nz_periods_0 %>% pull(prop_cases_sequenced)))

results_model_two_nz_p_test_050_periods <- results_model_two_nz_p_test_050_periods %>%
  mutate(detection_proba = testing_proba * sequencing_proba)

results_model_two_nz_p_test_050_n_divergent <- data.frame(matrix(data = 0, nrow = 5, ncol = 3))
names(results_model_two_nz_p_test_050_n_divergent) <- c("period", "country", "n_divergent")

results_model_two_nz_p_test_050_n_divergent <- results_model_two_nz_p_test_050_n_divergent %>%
  mutate(period = c("all", as.character(1:4)),
         country = rep(x = "New Zealand", times = 5))

for(ii in 1:5) {
  
  results_ii <- list_results_model_two_nz_p_test_050_periods[[ii]]
  
  if (!(is.numeric(results_ii))) {
    
    results_model_two_nz_p_test_050_periods$R_estimate[ii] <- get_posterior_mean(results_ii, par = c("R"))[, "mean-all chains"]
    results_model_two_nz_p_test_050_periods$R_lower_cred_int[ii] <- as.data.frame(summary(results_ii)[[1]])[c("R"), "2.5%"]
    results_model_two_nz_p_test_050_periods$R_upper_cred_int[ii] <- as.data.frame(summary(results_ii)[[1]])[c("R"), "97.5%"]
    results_model_two_nz_p_test_050_periods$R_Rhat[ii] <- as.data.frame(summary(results_ii)[[1]])[c("R"), "Rhat"]
    
    results_model_two_nz_p_test_050_periods$k_estimate[ii] <- get_posterior_mean(results_ii, par = c("k"))[, "mean-all chains"]
    results_model_two_nz_p_test_050_periods$k_lower_cred_int[ii] <- as.data.frame(summary(results_ii)[[1]])[c("k"), "2.5%"]
    results_model_two_nz_p_test_050_periods$k_upper_cred_int[ii] <- as.data.frame(summary(results_ii)[[1]])[c("k"), "97.5%"]
    results_model_two_nz_p_test_050_periods$k_Rhat[ii] <- as.data.frame(summary(results_ii)[[1]])[c("k"), "Rhat"]
    
    results_model_two_nz_p_test_050_periods$number_yearly_mutations_estimate[ii] <- get_posterior_mean(results_ii, par = c("number_yearly_mutations"))[, "mean-all chains"]
    results_model_two_nz_p_test_050_periods$number_yearly_mutations_lower_cred_int[ii] <- as.data.frame(summary(results_ii)[[1]])[c("number_yearly_mutations"), "2.5%"]
    results_model_two_nz_p_test_050_periods$number_yearly_mutations_upper_cred_int[ii] <- as.data.frame(summary(results_ii)[[1]])[c("number_yearly_mutations"), "97.5%"]
    results_model_two_nz_p_test_050_periods$number_yearly_mutations_Rhat[ii] <- as.data.frame(summary(results_ii)[[1]])[c("number_yearly_mutations"), "Rhat"]
    
    results_model_two_nz_p_test_050_periods$mutation_proba_estimate[ii] <- get_posterior_mean(results_ii, par = c("mutation_proba"))[, "mean-all chains"]
    results_model_two_nz_p_test_050_periods$mutation_proba_lower_cred_int[ii] <- as.data.frame(summary(results_ii)[[1]])[c("mutation_proba"), "2.5%"]
    results_model_two_nz_p_test_050_periods$mutation_proba_upper_cred_int[ii] <- as.data.frame(summary(results_ii)[[1]])[c("mutation_proba"), "97.5%"]
    results_model_two_nz_p_test_050_periods$mutation_proba_Rhat[ii] <- as.data.frame(summary(results_ii)[[1]])[c("mutation_proba"), "Rhat"]
    
    results_model_two_nz_p_test_050_n_divergent$n_divergent[ii] <- get_num_divergent(results_ii)
    
    ggsave(plot = rstan::traceplot(results_ii, inc_warmup = TRUE),
           filename = paste0("plots/new_zealand/model_two/traceplot_model_two_nz_p_test_050_period_", results_model_two_nz_p_test_050_periods$period[ii], ".png"))
    
  }
  
}


# p_test = 0.80 ----
if (file.exists(paste0(path_results_model_two_nz, "p_test_080_all.rds"))) {
  
  results_model_two_nz_p_test_080_all <- readRDS(paste0(path_results_model_two_nz, "p_test_080_all.rds"))
  
} else {
  
  results_model_two_nz_p_test_080_all <- 0
  
}

if (file.exists(paste0(path_results_model_two_nz, "p_test_080_1.rds"))) {
  
  results_model_two_nz_p_test_080_period_01 <- readRDS(paste0(path_results_model_two_nz, "p_test_080_1.rds"))
  
} else {
  
  results_model_two_nz_p_test_080_period_01 <- 0
  
}

if (file.exists(paste0(path_results_model_two_nz, "p_test_080_2.rds"))) {
  
  results_model_two_nz_p_test_080_period_02 <- readRDS(paste0(path_results_model_two_nz, "p_test_080_2.rds"))
  
} else {
  
  results_model_two_nz_p_test_080_period_02 <- 0
  
}

if (file.exists(paste0(path_results_model_two_nz, "p_test_080_3.rds"))) {
  
  results_model_two_nz_p_test_080_period_03 <- readRDS(paste0(path_results_model_two_nz, "p_test_080_3.rds"))
  
} else {
  
  results_model_two_nz_p_test_080_period_03 <- 0
  
}

if (file.exists(paste0(path_results_model_two_nz, "p_test_080_4.rds"))) {
  
  results_model_two_nz_p_test_080_period_04 <- readRDS(paste0(path_results_model_two_nz, "p_test_080_4.rds"))
  
} else {
  
  results_model_two_nz_p_test_080_period_04 <- 0
  
}

# create list of results
list_results_model_two_nz_p_test_080_periods <- list(results_model_two_nz_p_test_080_all,
                                                     results_model_two_nz_p_test_080_period_01, results_model_two_nz_p_test_080_period_02,
                                                     results_model_two_nz_p_test_080_period_03, results_model_two_nz_p_test_080_period_04)

# create summary of results of parameter estimations
results_model_two_nz_p_test_080_periods <- data.frame(matrix(data = 0, nrow = 5, ncol = 21))
names(results_model_two_nz_p_test_080_periods) <- c("period", "country", "sequencing_proba", 
                                                    "R_estimate", "R_lower_cred_int", "R_upper_cred_int", "R_Rhat",
                                                    "k_estimate", "k_lower_cred_int", "k_upper_cred_int", "k_Rhat",
                                                    "number_yearly_mutations_estimate", "number_yearly_mutations_lower_cred_int", 
                                                    "number_yearly_mutations_upper_cred_int", "number_yearly_mutations_Rhat",
                                                    "mutation_proba_estimate", "mutation_proba_lower_cred_int", "mutation_proba_upper_cred_int", "mutation_proba_Rhat",
                                                    "testing_proba",
                                                    "detection_proba")

results_model_two_nz_p_test_080_periods <- results_model_two_nz_p_test_080_periods %>%
  mutate(period = c("all", as.character(1:4)),
         country = rep(x = "New Zealand", times = 5),
         testing_proba = 0.80,
         sequencing_proba = c(sequencing_proba_nz, sequencing_probas_nz_periods_0 %>% pull(prop_cases_sequenced)))

results_model_two_nz_p_test_080_periods <- results_model_two_nz_p_test_080_periods %>%
  mutate(detection_proba = testing_proba * sequencing_proba)

results_model_two_nz_p_test_080_n_divergent <- data.frame(matrix(data = 0, nrow = 5, ncol = 3))
names(results_model_two_nz_p_test_080_n_divergent) <- c("period", "country", "n_divergent")

results_model_two_nz_p_test_080_n_divergent <- results_model_two_nz_p_test_080_n_divergent %>%
  mutate(period = c("all", as.character(1:4)),
         country = rep(x = "New Zealand", times = 5))

for(ii in 1:5) {
  
  results_ii <- list_results_model_two_nz_p_test_080_periods[[ii]]
  
  if (!(is.numeric(results_ii))) {
    
    results_model_two_nz_p_test_080_periods$R_estimate[ii] <- get_posterior_mean(results_ii, par = c("R"))[, "mean-all chains"]
    results_model_two_nz_p_test_080_periods$R_lower_cred_int[ii] <- as.data.frame(summary(results_ii)[[1]])[c("R"), "2.5%"]
    results_model_two_nz_p_test_080_periods$R_upper_cred_int[ii] <- as.data.frame(summary(results_ii)[[1]])[c("R"), "97.5%"]
    results_model_two_nz_p_test_080_periods$R_Rhat[ii] <- as.data.frame(summary(results_ii)[[1]])[c("R"), "Rhat"]
    
    results_model_two_nz_p_test_080_periods$k_estimate[ii] <- get_posterior_mean(results_ii, par = c("k"))[, "mean-all chains"]
    results_model_two_nz_p_test_080_periods$k_lower_cred_int[ii] <- as.data.frame(summary(results_ii)[[1]])[c("k"), "2.5%"]
    results_model_two_nz_p_test_080_periods$k_upper_cred_int[ii] <- as.data.frame(summary(results_ii)[[1]])[c("k"), "97.5%"]
    results_model_two_nz_p_test_080_periods$k_Rhat[ii] <- as.data.frame(summary(results_ii)[[1]])[c("k"), "Rhat"]
    
    results_model_two_nz_p_test_080_periods$number_yearly_mutations_estimate[ii] <- get_posterior_mean(results_ii, par = c("number_yearly_mutations"))[, "mean-all chains"]
    results_model_two_nz_p_test_080_periods$number_yearly_mutations_lower_cred_int[ii] <- as.data.frame(summary(results_ii)[[1]])[c("number_yearly_mutations"), "2.5%"]
    results_model_two_nz_p_test_080_periods$number_yearly_mutations_upper_cred_int[ii] <- as.data.frame(summary(results_ii)[[1]])[c("number_yearly_mutations"), "97.5%"]
    results_model_two_nz_p_test_080_periods$number_yearly_mutations_Rhat[ii] <- as.data.frame(summary(results_ii)[[1]])[c("number_yearly_mutations"), "Rhat"]
    
    results_model_two_nz_p_test_080_periods$mutation_proba_estimate[ii] <- get_posterior_mean(results_ii, par = c("mutation_proba"))[, "mean-all chains"]
    results_model_two_nz_p_test_080_periods$mutation_proba_lower_cred_int[ii] <- as.data.frame(summary(results_ii)[[1]])[c("mutation_proba"), "2.5%"]
    results_model_two_nz_p_test_080_periods$mutation_proba_upper_cred_int[ii] <- as.data.frame(summary(results_ii)[[1]])[c("mutation_proba"), "97.5%"]
    results_model_two_nz_p_test_080_periods$mutation_proba_Rhat[ii] <- as.data.frame(summary(results_ii)[[1]])[c("mutation_proba"), "Rhat"]
    
    results_model_two_nz_p_test_080_n_divergent$n_divergent[ii] <- get_num_divergent(results_ii)
    
    ggsave(plot = rstan::traceplot(results_ii, inc_warmup = TRUE),
           filename = paste0("plots/new_zealand/model_two/traceplot_model_two_nz_p_test_080_period_", ii, ".png"))
    
  }
  
}


# p_test = 1.00 ----
if (file.exists(paste0(path_results_model_two_nz, "p_test_100_all.rds"))) {
  
  results_model_two_nz_p_test_100_all <- readRDS(paste0(path_results_model_two_nz, "p_test_100_all.rds"))
  
} else {
  
  results_model_two_nz_p_test_100_all <- 0
  
}

if (file.exists(paste0(path_results_model_two_nz, "p_test_100_1.rds"))) {
  
  results_model_two_nz_p_test_100_period_01 <- readRDS(paste0(path_results_model_two_nz, "p_test_100_1.rds"))
  
} else {
  
  results_model_two_nz_p_test_100_period_01 <- 0
  
}

if (file.exists(paste0(path_results_model_two_nz, "p_test_100_2.rds"))) {
  
  results_model_two_nz_p_test_100_period_02 <- readRDS(paste0(path_results_model_two_nz, "p_test_100_2.rds"))
  
} else {
  
  results_model_two_nz_p_test_100_period_02 <- 0
  
}

if (file.exists(paste0(path_results_model_two_nz, "p_test_100_3.rds"))) {
  
  results_model_two_nz_p_test_100_period_03 <- readRDS(paste0(path_results_model_two_nz, "p_test_100_3.rds"))
  
} else {
  
  results_model_two_nz_p_test_100_period_03 <- 0
  
}

if (file.exists(paste0(path_results_model_two_nz, "p_test_100_4.rds"))) {
  
  results_model_two_nz_p_test_100_period_04 <- readRDS(paste0(path_results_model_two_nz, "p_test_100_4.rds"))
  
} else {
  
  results_model_two_nz_p_test_100_period_04 <- 0
  
}

# create list of results
list_results_model_two_nz_p_test_100_periods <- list(results_model_two_nz_p_test_100_all,
                                                     results_model_two_nz_p_test_100_period_01, results_model_two_nz_p_test_100_period_02,
                                                     results_model_two_nz_p_test_100_period_03, results_model_two_nz_p_test_100_period_04)

# create summary of results of parameter estimations
results_model_two_nz_p_test_100_periods <- data.frame(matrix(data = 0, nrow = 5, ncol = 21))
names(results_model_two_nz_p_test_100_periods) <- c("period", "country", "sequencing_proba", 
                                                    "R_estimate", "R_lower_cred_int", "R_upper_cred_int", "R_Rhat",
                                                    "k_estimate", "k_lower_cred_int", "k_upper_cred_int", "k_Rhat",
                                                    "number_yearly_mutations_estimate", "number_yearly_mutations_lower_cred_int", 
                                                    "number_yearly_mutations_upper_cred_int", "number_yearly_mutations_Rhat",
                                                    "mutation_proba_estimate", "mutation_proba_lower_cred_int", "mutation_proba_upper_cred_int", "mutation_proba_Rhat",
                                                    "testing_proba",
                                                    "detection_proba")

results_model_two_nz_p_test_100_periods <- results_model_two_nz_p_test_100_periods %>%
  mutate(period = c("all", as.character(1:4)),
         country = rep(x = "New Zealand", times = 5),
         testing_proba = 1.00,
         sequencing_proba = c(sequencing_proba_nz, sequencing_probas_nz_periods_0 %>% pull(prop_cases_sequenced)))

results_model_two_nz_p_test_100_periods <- results_model_two_nz_p_test_100_periods %>%
  mutate(detection_proba = testing_proba * sequencing_proba)

results_model_two_nz_p_test_100_n_divergent <- data.frame(matrix(data = 0, nrow = 5, ncol = 3))
names(results_model_two_nz_p_test_100_n_divergent) <- c("period", "country", "n_divergent")

results_model_two_nz_p_test_100_n_divergent <- results_model_two_nz_p_test_100_n_divergent %>%
  mutate(period = c("all", as.character(1:4)),
         country = rep(x = "New Zealand", times = 5))

for(ii in 1:5) {
  
  results_ii <- list_results_model_two_nz_p_test_100_periods[[ii]]
  
  if (!(is.numeric(results_ii))) {
    
    results_model_two_nz_p_test_100_periods$R_estimate[ii] <- get_posterior_mean(results_ii, par = c("R"))[, "mean-all chains"]
    results_model_two_nz_p_test_100_periods$R_lower_cred_int[ii] <- as.data.frame(summary(results_ii)[[1]])[c("R"), "2.5%"]
    results_model_two_nz_p_test_100_periods$R_upper_cred_int[ii] <- as.data.frame(summary(results_ii)[[1]])[c("R"), "97.5%"]
    results_model_two_nz_p_test_100_periods$R_Rhat[ii] <- as.data.frame(summary(results_ii)[[1]])[c("R"), "Rhat"]
    
    results_model_two_nz_p_test_100_periods$k_estimate[ii] <- get_posterior_mean(results_ii, par = c("k"))[, "mean-all chains"]
    results_model_two_nz_p_test_100_periods$k_lower_cred_int[ii] <- as.data.frame(summary(results_ii)[[1]])[c("k"), "2.5%"]
    results_model_two_nz_p_test_100_periods$k_upper_cred_int[ii] <- as.data.frame(summary(results_ii)[[1]])[c("k"), "97.5%"]
    results_model_two_nz_p_test_100_periods$k_Rhat[ii] <- as.data.frame(summary(results_ii)[[1]])[c("k"), "Rhat"]
    
    results_model_two_nz_p_test_100_periods$number_yearly_mutations_estimate[ii] <- get_posterior_mean(results_ii, par = c("number_yearly_mutations"))[, "mean-all chains"]
    results_model_two_nz_p_test_100_periods$number_yearly_mutations_lower_cred_int[ii] <- as.data.frame(summary(results_ii)[[1]])[c("number_yearly_mutations"), "2.5%"]
    results_model_two_nz_p_test_100_periods$number_yearly_mutations_upper_cred_int[ii] <- as.data.frame(summary(results_ii)[[1]])[c("number_yearly_mutations"), "97.5%"]
    results_model_two_nz_p_test_100_periods$number_yearly_mutations_Rhat[ii] <- as.data.frame(summary(results_ii)[[1]])[c("number_yearly_mutations"), "Rhat"]
    
    results_model_two_nz_p_test_100_periods$mutation_proba_estimate[ii] <- get_posterior_mean(results_ii, par = c("mutation_proba"))[, "mean-all chains"]
    results_model_two_nz_p_test_100_periods$mutation_proba_lower_cred_int[ii] <- as.data.frame(summary(results_ii)[[1]])[c("mutation_proba"), "2.5%"]
    results_model_two_nz_p_test_100_periods$mutation_proba_upper_cred_int[ii] <- as.data.frame(summary(results_ii)[[1]])[c("mutation_proba"), "97.5%"]
    results_model_two_nz_p_test_100_periods$mutation_proba_Rhat[ii] <- as.data.frame(summary(results_ii)[[1]])[c("mutation_proba"), "Rhat"]
    
    results_model_two_nz_p_test_100_n_divergent$n_divergent[ii] <- get_num_divergent(results_ii)
    
    ggsave(plot = rstan::traceplot(results_ii, inc_warmup = TRUE),
           filename = paste0("plots/new_zealand/model_two/traceplot_model_two_nz_p_test_100_period_", ii, ".png"))
    
  }
  
}


# create summary of results of parameter estimations
results_model_two_nz_periods <- rbind(results_model_two_nz_p_test_050_periods,
                                      results_model_two_nz_p_test_080_periods,
                                      results_model_two_nz_p_test_100_periods)


# save `results_model_two_nz_periods` as csv file
write_csv(results_model_two_nz_periods, file = path_results_model_two_nz_periods_processed)



# read results of parameter estimations of model 4 ----

# New Zealand

# p_test = 0.50 ----
if (file.exists(paste0(path_results_model_four_nz, "p_test_050_all.rds"))) {
  
  results_model_four_nz_p_test_050_all <- readRDS(paste0(path_results_model_four_nz, "p_test_050_all.rds"))
  
} else {
  
  results_model_four_nz_p_test_050_all <- 0
  
}

if (file.exists(paste0(path_results_model_four_nz, "p_test_050_1.rds"))) {
  
  results_model_four_nz_p_test_050_period_01 <- readRDS(paste0(path_results_model_four_nz, "p_test_050_1.rds"))
  
} else {
  
  results_model_four_nz_p_test_050_period_01 <- 0
  
}

if (file.exists(paste0(path_results_model_four_nz, "p_test_050_2.rds"))) {
  
  results_model_four_nz_p_test_050_period_02 <- readRDS(paste0(path_results_model_four_nz, "p_test_050_2.rds"))
  
} else {
  
  results_model_four_nz_p_test_050_period_02 <- 0
  
}

if (file.exists(paste0(path_results_model_four_nz, "p_test_050_3.rds"))) {
  
  results_model_four_nz_p_test_050_period_03 <- readRDS(paste0(path_results_model_four_nz, "p_test_050_3.rds"))
  
} else {
  
  results_model_four_nz_p_test_050_period_03 <- 0
  
}

if (file.exists(paste0(path_results_model_four_nz, "p_test_050_4.rds"))) {
  
  results_model_four_nz_p_test_050_period_04 <- readRDS(paste0(path_results_model_four_nz, "p_test_050_4.rds"))
  
} else {
  
  results_model_four_nz_p_test_050_period_04 <- 0
  
}

# create list of results
list_results_model_four_nz_p_test_050_periods <- list(results_model_four_nz_p_test_050_all,
                                                      results_model_four_nz_p_test_050_period_01, results_model_four_nz_p_test_050_period_02,
                                                      results_model_four_nz_p_test_050_period_03, results_model_four_nz_p_test_050_period_04)

# create summary of results of parameter estimations
results_model_four_nz_p_test_050_periods <- data.frame(matrix(data = 0, nrow = 5, ncol = 14))
names(results_model_four_nz_p_test_050_periods) <- c("period", "country", "sequencing_proba", 
                                                     "R_estimate", "R_lower_cred_int", "R_upper_cred_int", "R_Rhat",
                                                     "k_estimate", "k_lower_cred_int", "k_upper_cred_int", "k_Rhat",
                                                     "mutation_proba", "testing_proba", "detection_proba")

results_model_four_nz_p_test_050_periods <- results_model_four_nz_p_test_050_periods %>%
  mutate(period = c("all", as.character(1:4)),
         country = rep(x = "New Zealand", times = 5),
         mutation_proba = 0.2811,
         testing_proba = 0.50,
         sequencing_proba = c(sequencing_proba_nz, sequencing_probas_nz_periods_0 %>% pull(prop_cases_sequenced)))

results_model_four_nz_p_test_050_periods <- results_model_four_nz_p_test_050_periods %>%
  mutate(detection_proba = testing_proba * sequencing_proba)

results_model_four_nz_p_test_050_n_divergent <- data.frame(matrix(data = 0, nrow = 5, ncol = 3))
names(results_model_four_nz_p_test_050_n_divergent) <- c("period", "country", "n_divergent")

results_model_four_nz_p_test_050_n_divergent <- results_model_four_nz_p_test_050_n_divergent %>%
  mutate(period = c("all", as.character(1:4)),
         country = rep(x = "New Zealand", times = 5))

for(ii in 1:5) {
  
  results_ii <- list_results_model_four_nz_p_test_050_periods[[ii]]
  
  if (!(is.numeric(results_ii))) {
    
    results_model_four_nz_p_test_050_periods$R_estimate[ii] <- get_posterior_mean(results_ii, par = c("R"))[, "mean-all chains"]
    results_model_four_nz_p_test_050_periods$R_lower_cred_int[ii] <- as.data.frame(summary(results_ii)[[1]])[c("R"), "2.5%"]
    results_model_four_nz_p_test_050_periods$R_upper_cred_int[ii] <- as.data.frame(summary(results_ii)[[1]])[c("R"), "97.5%"]
    results_model_four_nz_p_test_050_periods$R_Rhat[ii] <- as.data.frame(summary(results_ii)[[1]])[c("R"), "Rhat"]
    
    results_model_four_nz_p_test_050_periods$k_estimate[ii] <- get_posterior_mean(results_ii, par = c("k"))[, "mean-all chains"]
    results_model_four_nz_p_test_050_periods$k_lower_cred_int[ii] <- as.data.frame(summary(results_ii)[[1]])[c("k"), "2.5%"]
    results_model_four_nz_p_test_050_periods$k_upper_cred_int[ii] <- as.data.frame(summary(results_ii)[[1]])[c("k"), "97.5%"]
    results_model_four_nz_p_test_050_periods$k_Rhat[ii] <- as.data.frame(summary(results_ii)[[1]])[c("k"), "Rhat"]
    
    results_model_four_nz_p_test_050_n_divergent$n_divergent[ii] <- get_num_divergent(results_ii)
    
    ggsave(plot = rstan::traceplot(results_ii, inc_warmup = TRUE),
           filename = paste0("plots/new_zealand/model_four/traceplot_model_four_nz_p_test_050_period_", results_model_four_nz_p_test_050_periods$period[ii], ".png"))
    
  }
  
}


# p_test = 0.80 ----
if (file.exists(paste0(path_results_model_four_nz, "p_test_080_all.rds"))) {
  
  results_model_four_nz_p_test_080_all <- readRDS(paste0(path_results_model_four_nz, "p_test_080_all.rds"))
  
} else {
  
  results_model_four_nz_p_test_080_all <- 0
  
}

if (file.exists(paste0(path_results_model_four_nz, "p_test_080_1.rds"))) {
  
  results_model_four_nz_p_test_080_period_01 <- readRDS(paste0(path_results_model_four_nz, "p_test_080_1.rds"))
  
} else {
  
  results_model_four_nz_p_test_080_period_01 <- 0
  
}

if (file.exists(paste0(path_results_model_four_nz, "p_test_080_2.rds"))) {
  
  results_model_four_nz_p_test_080_period_02 <- readRDS(paste0(path_results_model_four_nz, "p_test_080_2.rds"))
  
} else {
  
  results_model_four_nz_p_test_080_period_02 <- 0
  
}

if (file.exists(paste0(path_results_model_four_nz, "p_test_080_3.rds"))) {
  
  results_model_four_nz_p_test_080_period_03 <- readRDS(paste0(path_results_model_four_nz, "p_test_080_3.rds"))
  
} else {
  
  results_model_four_nz_p_test_080_period_03 <- 0
  
}

if (file.exists(paste0(path_results_model_four_nz, "p_test_080_4.rds"))) {
  
  results_model_four_nz_p_test_080_period_04 <- readRDS(paste0(path_results_model_four_nz, "p_test_080_4.rds"))
  
} else {
  
  results_model_four_nz_p_test_080_period_04 <- 0
  
}

# create list of results
list_results_model_four_nz_p_test_080_periods <- list(results_model_four_nz_p_test_080_all,
                                                      results_model_four_nz_p_test_080_period_01, results_model_four_nz_p_test_080_period_02,
                                                      results_model_four_nz_p_test_080_period_03, results_model_four_nz_p_test_080_period_04)

# create summary of results of parameter estimations
results_model_four_nz_p_test_080_periods <- data.frame(matrix(data = 0, nrow = 5, ncol = 14))
names(results_model_four_nz_p_test_080_periods) <- c("period", "country", "sequencing_proba", 
                                                     "R_estimate", "R_lower_cred_int", "R_upper_cred_int", "R_Rhat",
                                                     "k_estimate", "k_lower_cred_int", "k_upper_cred_int", "k_Rhat",
                                                     "mutation_proba", "testing_proba", "detection_proba")

results_model_four_nz_p_test_080_periods <- results_model_four_nz_p_test_080_periods %>%
  mutate(period = c("all", as.character(1:4)),
         country = rep(x = "New Zealand", times = 5),
         mutation_proba = 0.2811,
         testing_proba = 0.80,
         sequencing_proba = c(sequencing_proba_nz, sequencing_probas_nz_periods_0 %>% pull(prop_cases_sequenced)))

results_model_four_nz_p_test_080_periods <- results_model_four_nz_p_test_080_periods %>%
  mutate(detection_proba = testing_proba * sequencing_proba)

results_model_four_nz_p_test_080_n_divergent <- data.frame(matrix(data = 0, nrow = 5, ncol = 3))
names(results_model_four_nz_p_test_080_n_divergent) <- c("period", "country", "n_divergent")

results_model_four_nz_p_test_080_n_divergent <- results_model_four_nz_p_test_080_n_divergent %>%
  mutate(period = c("all", as.character(1:4)),
         country = rep(x = "New Zealand", times = 5))

for(ii in 1:5) {
  
  results_ii <- list_results_model_four_nz_p_test_080_periods[[ii]]
  
  if (!(is.numeric(results_ii))) {
    
    results_model_four_nz_p_test_080_periods$R_estimate[ii] <- get_posterior_mean(results_ii, par = c("R"))[, "mean-all chains"]
    results_model_four_nz_p_test_080_periods$R_lower_cred_int[ii] <- as.data.frame(summary(results_ii)[[1]])[c("R"), "2.5%"]
    results_model_four_nz_p_test_080_periods$R_upper_cred_int[ii] <- as.data.frame(summary(results_ii)[[1]])[c("R"), "97.5%"]
    results_model_four_nz_p_test_080_periods$R_Rhat[ii] <- as.data.frame(summary(results_ii)[[1]])[c("R"), "Rhat"]
    
    results_model_four_nz_p_test_080_periods$k_estimate[ii] <- get_posterior_mean(results_ii, par = c("k"))[, "mean-all chains"]
    results_model_four_nz_p_test_080_periods$k_lower_cred_int[ii] <- as.data.frame(summary(results_ii)[[1]])[c("k"), "2.5%"]
    results_model_four_nz_p_test_080_periods$k_upper_cred_int[ii] <- as.data.frame(summary(results_ii)[[1]])[c("k"), "97.5%"]
    results_model_four_nz_p_test_080_periods$k_Rhat[ii] <- as.data.frame(summary(results_ii)[[1]])[c("k"), "Rhat"]
    
    results_model_four_nz_p_test_080_n_divergent$n_divergent[ii] <- get_num_divergent(results_ii)
    
    ggsave(plot = rstan::traceplot(results_ii, inc_warmup = TRUE),
           filename = paste0("plots/new_zealand/model_four/traceplot_model_four_nz_p_test_080_period_", ii, ".png"))
    
  }
  
}


# p_test = 1.00 ----
if (file.exists(paste0(path_results_model_four_nz, "p_test_100_all.rds"))) {
  
  results_model_four_nz_p_test_100_all <- readRDS(paste0(path_results_model_four_nz, "p_test_100_all.rds"))
  
} else {
  
  results_model_four_nz_p_test_100_all <- 0
  
}

if (file.exists(paste0(path_results_model_four_nz, "p_test_100_1.rds"))) {
  
  results_model_four_nz_p_test_100_period_01 <- readRDS(paste0(path_results_model_four_nz, "p_test_100_1.rds"))
  
} else {
  
  results_model_four_nz_p_test_100_period_01 <- 0
  
}

if (file.exists(paste0(path_results_model_four_nz, "p_test_100_2.rds"))) {
  
  results_model_four_nz_p_test_100_period_02 <- readRDS(paste0(path_results_model_four_nz, "p_test_100_2.rds"))
  
} else {
  
  results_model_four_nz_p_test_100_period_02 <- 0
  
}

if (file.exists(paste0(path_results_model_four_nz, "p_test_100_3.rds"))) {
  
  results_model_four_nz_p_test_100_period_03 <- readRDS(paste0(path_results_model_four_nz, "p_test_100_3.rds"))
  
} else {
  
  results_model_four_nz_p_test_100_period_03 <- 0
  
}

if (file.exists(paste0(path_results_model_four_nz, "p_test_100_4.rds"))) {
  
  results_model_four_nz_p_test_100_period_04 <- readRDS(paste0(path_results_model_four_nz, "p_test_100_4.rds"))
  
} else {
  
  results_model_four_nz_p_test_100_period_04 <- 0
  
}

# create list of results
list_results_model_four_nz_p_test_100_periods <- list(results_model_four_nz_p_test_100_all,
                                                      results_model_four_nz_p_test_100_period_01, results_model_four_nz_p_test_100_period_02,
                                                      results_model_four_nz_p_test_100_period_03, results_model_four_nz_p_test_100_period_04)

# create summary of results of parameter estimations
results_model_four_nz_p_test_100_periods <- data.frame(matrix(data = 0, nrow = 5, ncol = 14))
names(results_model_four_nz_p_test_100_periods) <- c("period", "country", "sequencing_proba", 
                                                     "R_estimate", "R_lower_cred_int", "R_upper_cred_int", "R_Rhat",
                                                     "k_estimate", "k_lower_cred_int", "k_upper_cred_int", "k_Rhat",
                                                     "mutation_proba", "testing_proba", "detection_proba")

results_model_four_nz_p_test_100_periods <- results_model_four_nz_p_test_100_periods %>%
  mutate(period = c("all", as.character(1:4)),
         country = rep(x = "New Zealand", times = 5),
         mutation_proba = 0.2811,
         testing_proba = 1.00,
         sequencing_proba = c(sequencing_proba_nz, sequencing_probas_nz_periods_0 %>% pull(prop_cases_sequenced)))

results_model_four_nz_p_test_100_periods <- results_model_four_nz_p_test_100_periods %>%
  mutate(detection_proba = testing_proba * sequencing_proba)

results_model_four_nz_p_test_100_n_divergent <- data.frame(matrix(data = 0, nrow = 5, ncol = 3))
names(results_model_four_nz_p_test_100_n_divergent) <- c("period", "country", "n_divergent")

results_model_four_nz_p_test_100_n_divergent <- results_model_four_nz_p_test_100_n_divergent %>%
  mutate(period = c("all", as.character(1:4)),
         country = rep(x = "New Zealand", times = 5))

for(ii in 1:5) {
  
  results_ii <- list_results_model_four_nz_p_test_100_periods[[ii]]
  
  if (!(is.numeric(results_ii))) {
    
    results_model_four_nz_p_test_100_periods$R_estimate[ii] <- get_posterior_mean(results_ii, par = c("R"))[, "mean-all chains"]
    results_model_four_nz_p_test_100_periods$R_lower_cred_int[ii] <- as.data.frame(summary(results_ii)[[1]])[c("R"), "2.5%"]
    results_model_four_nz_p_test_100_periods$R_upper_cred_int[ii] <- as.data.frame(summary(results_ii)[[1]])[c("R"), "97.5%"]
    results_model_four_nz_p_test_100_periods$R_Rhat[ii] <- as.data.frame(summary(results_ii)[[1]])[c("R"), "Rhat"]
    
    results_model_four_nz_p_test_100_periods$k_estimate[ii] <- get_posterior_mean(results_ii, par = c("k"))[, "mean-all chains"]
    results_model_four_nz_p_test_100_periods$k_lower_cred_int[ii] <- as.data.frame(summary(results_ii)[[1]])[c("k"), "2.5%"]
    results_model_four_nz_p_test_100_periods$k_upper_cred_int[ii] <- as.data.frame(summary(results_ii)[[1]])[c("k"), "97.5%"]
    results_model_four_nz_p_test_100_periods$k_Rhat[ii] <- as.data.frame(summary(results_ii)[[1]])[c("k"), "Rhat"]
    
    results_model_four_nz_p_test_100_n_divergent$n_divergent[ii] <- get_num_divergent(results_ii)
    
    ggsave(plot = rstan::traceplot(results_ii, inc_warmup = TRUE),
           filename = paste0("plots/new_zealand/model_four/traceplot_model_four_nz_p_test_100_period_", ii, ".png"))
    
  }
  
}


# create summary of results of parameter estimations
results_model_four_nz_periods <- rbind(results_model_four_nz_p_test_050_periods,
                                       results_model_four_nz_p_test_080_periods,
                                       results_model_four_nz_p_test_100_periods)


# save `results_model_four_nz_periods` as csv file
write_csv(results_model_four_nz_periods, file = path_results_model_four_nz_periods_processed)



# read results of parameter estimations of model 6 ----

# New Zealand

# p_test = 0.50 ----
if (file.exists(paste0(path_results_model_six_nz, "p_test_050_all.rds"))) {
  
  results_model_six_nz_p_test_050_all <- readRDS(paste0(path_results_model_six_nz, "p_test_050_all.rds"))
  
} else {
  
  results_model_six_nz_p_test_050_all <- 0
  
}

if (file.exists(paste0(path_results_model_six_nz, "p_test_050_1.rds"))) {
  
  results_model_six_nz_p_test_050_period_01 <- readRDS(paste0(path_results_model_six_nz, "p_test_050_1.rds"))
  
} else {
  
  results_model_six_nz_p_test_050_period_01 <- 0
  
}

if (file.exists(paste0(path_results_model_six_nz, "p_test_050_2.rds"))) {
  
  results_model_six_nz_p_test_050_period_02 <- readRDS(paste0(path_results_model_six_nz, "p_test_050_2.rds"))
  
} else {
  
  results_model_six_nz_p_test_050_period_02 <- 0
  
}

if (file.exists(paste0(path_results_model_six_nz, "p_test_050_3.rds"))) {
  
  results_model_six_nz_p_test_050_period_03 <- readRDS(paste0(path_results_model_six_nz, "p_test_050_3.rds"))
  
} else {
  
  results_model_six_nz_p_test_050_period_03 <- 0
  
}

if (file.exists(paste0(path_results_model_six_nz, "p_test_050_4.rds"))) {
  
  results_model_six_nz_p_test_050_period_04 <- readRDS(paste0(path_results_model_six_nz, "p_test_050_4.rds"))
  
} else {
  
  results_model_six_nz_p_test_050_period_04 <- 0
  
}

# create list of results
list_results_model_six_nz_p_test_050_periods <- list(results_model_six_nz_p_test_050_all,
                                                     results_model_six_nz_p_test_050_period_01, results_model_six_nz_p_test_050_period_02,
                                                     results_model_six_nz_p_test_050_period_03, results_model_six_nz_p_test_050_period_04)

# create summary of results of parameter estimations
results_model_six_nz_p_test_050_periods <- data.frame(matrix(data = 0, nrow = 5, ncol = 17))
names(results_model_six_nz_p_test_050_periods) <- c("period", "country", "sequencing_proba", 
                                                    "R_estimate", "R_lower_cred_int", "R_upper_cred_int", "R_Rhat",
                                                    "k_estimate", "k_lower_cred_int", "k_upper_cred_int", "k_Rhat",
                                                    "mutation_proba_estimate", "mutation_proba_lower_cred_int", "mutation_proba_upper_cred_int", "mutation_proba_Rhat",
                                                    "testing_proba", "detection_proba")

results_model_six_nz_p_test_050_periods <- results_model_six_nz_p_test_050_periods %>%
  mutate(period = c("all", as.character(1:4)),
         country = rep(x = "New Zealand", times = 5),
         mutation_proba = 0.2811,
         testing_proba = 0.50,
         sequencing_proba = c(sequencing_proba_nz, sequencing_probas_nz_periods_0 %>% pull(prop_cases_sequenced)))

results_model_six_nz_p_test_050_periods <- results_model_six_nz_p_test_050_periods %>%
  mutate(detection_proba = testing_proba * sequencing_proba)

results_model_six_nz_p_test_050_n_divergent <- data.frame(matrix(data = 0, nrow = 5, ncol = 3))
names(results_model_six_nz_p_test_050_n_divergent) <- c("period", "country", "n_divergent")

results_model_six_nz_p_test_050_n_divergent <- results_model_six_nz_p_test_050_n_divergent %>%
  mutate(period = c("all", as.character(1:4)),
         country = rep(x = "New Zealand", times = 5))

for(ii in 1:5) {
  
  results_ii <- list_results_model_six_nz_p_test_050_periods[[ii]]
  
  if (!(is.numeric(results_ii))) {
    
    results_model_six_nz_p_test_050_periods$R_estimate[ii] <- get_posterior_mean(results_ii, par = c("R"))[, "mean-all chains"]
    results_model_six_nz_p_test_050_periods$R_lower_cred_int[ii] <- as.data.frame(summary(results_ii)[[1]])[c("R"), "2.5%"]
    results_model_six_nz_p_test_050_periods$R_upper_cred_int[ii] <- as.data.frame(summary(results_ii)[[1]])[c("R"), "97.5%"]
    results_model_six_nz_p_test_050_periods$R_Rhat[ii] <- as.data.frame(summary(results_ii)[[1]])[c("R"), "Rhat"]
    
    results_model_six_nz_p_test_050_periods$k_estimate[ii] <- get_posterior_mean(results_ii, par = c("k"))[, "mean-all chains"]
    results_model_six_nz_p_test_050_periods$k_lower_cred_int[ii] <- as.data.frame(summary(results_ii)[[1]])[c("k"), "2.5%"]
    results_model_six_nz_p_test_050_periods$k_upper_cred_int[ii] <- as.data.frame(summary(results_ii)[[1]])[c("k"), "97.5%"]
    results_model_six_nz_p_test_050_periods$k_Rhat[ii] <- as.data.frame(summary(results_ii)[[1]])[c("k"), "Rhat"]
    
    results_model_six_nz_p_test_050_periods$mutation_proba_estimate[ii] <- get_posterior_mean(results_ii, par = c("mutation_proba"))[, "mean-all chains"]
    results_model_six_nz_p_test_050_periods$mutation_proba_lower_cred_int[ii] <- as.data.frame(summary(results_ii)[[1]])[c("mutation_proba"), "2.5%"]
    results_model_six_nz_p_test_050_periods$mutation_proba_upper_cred_int[ii] <- as.data.frame(summary(results_ii)[[1]])[c("mutation_proba"), "97.5%"]
    results_model_six_nz_p_test_050_periods$mutation_proba_Rhat[ii] <- as.data.frame(summary(results_ii)[[1]])[c("mutation_proba"), "Rhat"]
    
    results_model_six_nz_p_test_050_n_divergent$n_divergent[ii] <- get_num_divergent(results_ii)
    
    ggsave(plot = rstan::traceplot(results_ii, inc_warmup = TRUE),
           filename = paste0("plots/new_zealand/model_six/traceplot_model_six_nz_p_test_050_period_", results_model_six_nz_p_test_050_periods$period[ii], ".png"))
    
  }
  
}


# p_test = 0.80 ----
if (file.exists(paste0(path_results_model_six_nz, "p_test_080_all.rds"))) {
  
  results_model_six_nz_p_test_080_all <- readRDS(paste0(path_results_model_six_nz, "p_test_080_all.rds"))
  
} else {
  
  results_model_six_nz_p_test_080_all <- 0
  
}

if (file.exists(paste0(path_results_model_six_nz, "p_test_080_1.rds"))) {
  
  results_model_six_nz_p_test_080_period_01 <- readRDS(paste0(path_results_model_six_nz, "p_test_080_1.rds"))
  
} else {
  
  results_model_six_nz_p_test_080_period_01 <- 0
  
}

if (file.exists(paste0(path_results_model_six_nz, "p_test_080_2.rds"))) {
  
  results_model_six_nz_p_test_080_period_02 <- readRDS(paste0(path_results_model_six_nz, "p_test_080_2.rds"))
  
} else {
  
  results_model_six_nz_p_test_080_period_02 <- 0
  
}

if (file.exists(paste0(path_results_model_six_nz, "p_test_080_3.rds"))) {
  
  results_model_six_nz_p_test_080_period_03 <- readRDS(paste0(path_results_model_six_nz, "p_test_080_3.rds"))
  
} else {
  
  results_model_six_nz_p_test_080_period_03 <- 0
  
}

if (file.exists(paste0(path_results_model_six_nz, "p_test_080_4.rds"))) {
  
  results_model_six_nz_p_test_080_period_04 <- readRDS(paste0(path_results_model_six_nz, "p_test_080_4.rds"))
  
} else {
  
  results_model_six_nz_p_test_080_period_04 <- 0
  
}

# create list of results
list_results_model_six_nz_p_test_080_periods <- list(results_model_six_nz_p_test_080_all,
                                                     results_model_six_nz_p_test_080_period_01, results_model_six_nz_p_test_080_period_02,
                                                     results_model_six_nz_p_test_080_period_03, results_model_six_nz_p_test_080_period_04)

# create summary of results of parameter estimations
results_model_six_nz_p_test_080_periods <- data.frame(matrix(data = 0, nrow = 5, ncol = 17))
names(results_model_six_nz_p_test_080_periods) <- c("period", "country", "sequencing_proba", 
                                                    "R_estimate", "R_lower_cred_int", "R_upper_cred_int", "R_Rhat",
                                                    "k_estimate", "k_lower_cred_int", "k_upper_cred_int", "k_Rhat",
                                                    "mutation_proba_estimate", "mutation_proba_lower_cred_int", "mutation_proba_upper_cred_int", "mutation_proba_Rhat",
                                                    "testing_proba", "detection_proba")

results_model_six_nz_p_test_080_periods <- results_model_six_nz_p_test_080_periods %>%
  mutate(period = c("all", as.character(1:4)),
         country = rep(x = "New Zealand", times = 5),
         mutation_proba = 0.2811,
         testing_proba = 0.80,
         sequencing_proba = c(sequencing_proba_nz, sequencing_probas_nz_periods_0 %>% pull(prop_cases_sequenced)))

results_model_six_nz_p_test_080_periods <- results_model_six_nz_p_test_080_periods %>%
  mutate(detection_proba = testing_proba * sequencing_proba)

results_model_six_nz_p_test_080_n_divergent <- data.frame(matrix(data = 0, nrow = 5, ncol = 3))
names(results_model_six_nz_p_test_080_n_divergent) <- c("period", "country", "n_divergent")

results_model_six_nz_p_test_080_n_divergent <- results_model_six_nz_p_test_080_n_divergent %>%
  mutate(period = c("all", as.character(1:4)),
         country = rep(x = "New Zealand", times = 5))

for(ii in 1:5) {
  
  results_ii <- list_results_model_six_nz_p_test_080_periods[[ii]]
  
  if (!(is.numeric(results_ii))) {
    
    results_model_six_nz_p_test_080_periods$R_estimate[ii] <- get_posterior_mean(results_ii, par = c("R"))[, "mean-all chains"]
    results_model_six_nz_p_test_080_periods$R_lower_cred_int[ii] <- as.data.frame(summary(results_ii)[[1]])[c("R"), "2.5%"]
    results_model_six_nz_p_test_080_periods$R_upper_cred_int[ii] <- as.data.frame(summary(results_ii)[[1]])[c("R"), "97.5%"]
    results_model_six_nz_p_test_080_periods$R_Rhat[ii] <- as.data.frame(summary(results_ii)[[1]])[c("R"), "Rhat"]
    
    results_model_six_nz_p_test_080_periods$k_estimate[ii] <- get_posterior_mean(results_ii, par = c("k"))[, "mean-all chains"]
    results_model_six_nz_p_test_080_periods$k_lower_cred_int[ii] <- as.data.frame(summary(results_ii)[[1]])[c("k"), "2.5%"]
    results_model_six_nz_p_test_080_periods$k_upper_cred_int[ii] <- as.data.frame(summary(results_ii)[[1]])[c("k"), "97.5%"]
    results_model_six_nz_p_test_080_periods$k_Rhat[ii] <- as.data.frame(summary(results_ii)[[1]])[c("k"), "Rhat"]
    
    results_model_six_nz_p_test_080_periods$mutation_proba_estimate[ii] <- get_posterior_mean(results_ii, par = c("mutation_proba"))[, "mean-all chains"]
    results_model_six_nz_p_test_080_periods$mutation_proba_lower_cred_int[ii] <- as.data.frame(summary(results_ii)[[1]])[c("mutation_proba"), "2.5%"]
    results_model_six_nz_p_test_080_periods$mutation_proba_upper_cred_int[ii] <- as.data.frame(summary(results_ii)[[1]])[c("mutation_proba"), "97.5%"]
    results_model_six_nz_p_test_080_periods$mutation_proba_Rhat[ii] <- as.data.frame(summary(results_ii)[[1]])[c("mutation_proba"), "Rhat"]
    
    results_model_six_nz_p_test_080_n_divergent$n_divergent[ii] <- get_num_divergent(results_ii)
    
    ggsave(plot = rstan::traceplot(results_ii, inc_warmup = TRUE),
           filename = paste0("plots/new_zealand/model_six/traceplot_model_six_nz_p_test_080_period_", ii, ".png"))
    
  }
  
}


# p_test = 1.00 ----
if (file.exists(paste0(path_results_model_six_nz, "p_test_100_all.rds"))) {
  
  results_model_six_nz_p_test_100_all <- readRDS(paste0(path_results_model_six_nz, "p_test_100_all.rds"))
  
} else {
  
  results_model_six_nz_p_test_100_all <- 0
  
}

if (file.exists(paste0(path_results_model_six_nz, "p_test_100_1.rds"))) {
  
  results_model_six_nz_p_test_100_period_01 <- readRDS(paste0(path_results_model_six_nz, "p_test_100_1.rds"))
  
} else {
  
  results_model_six_nz_p_test_100_period_01 <- 0
  
}

if (file.exists(paste0(path_results_model_six_nz, "p_test_100_2.rds"))) {
  
  results_model_six_nz_p_test_100_period_02 <- readRDS(paste0(path_results_model_six_nz, "p_test_100_2.rds"))
  
} else {
  
  results_model_six_nz_p_test_100_period_02 <- 0
  
}

if (file.exists(paste0(path_results_model_six_nz, "p_test_100_3.rds"))) {
  
  results_model_six_nz_p_test_100_period_03 <- readRDS(paste0(path_results_model_six_nz, "p_test_100_3.rds"))
  
} else {
  
  results_model_six_nz_p_test_100_period_03 <- 0
  
}

if (file.exists(paste0(path_results_model_six_nz, "p_test_100_4.rds"))) {
  
  results_model_six_nz_p_test_100_period_04 <- readRDS(paste0(path_results_model_six_nz, "p_test_100_4.rds"))
  
} else {
  
  results_model_six_nz_p_test_100_period_04 <- 0
  
}

# create list of results
list_results_model_six_nz_p_test_100_periods <- list(results_model_six_nz_p_test_100_all,
                                                     results_model_six_nz_p_test_100_period_01, results_model_six_nz_p_test_100_period_02,
                                                     results_model_six_nz_p_test_100_period_03, results_model_six_nz_p_test_100_period_04)

# create summary of results of parameter estimations
results_model_six_nz_p_test_100_periods <- data.frame(matrix(data = 0, nrow = 5, ncol = 17))
names(results_model_six_nz_p_test_100_periods) <- c("period", "country", "sequencing_proba", 
                                                    "R_estimate", "R_lower_cred_int", "R_upper_cred_int", "R_Rhat",
                                                    "k_estimate", "k_lower_cred_int", "k_upper_cred_int", "k_Rhat",
                                                    "mutation_proba_estimate", "mutation_proba_lower_cred_int", "mutation_proba_upper_cred_int", "mutation_proba_Rhat",
                                                    "testing_proba", "detection_proba")

results_model_six_nz_p_test_100_periods <- results_model_six_nz_p_test_100_periods %>%
  mutate(period = c("all", as.character(1:4)),
         country = rep(x = "New Zealand", times = 5),
         mutation_proba = 0.2811,
         testing_proba = 1.00,
         sequencing_proba = c(sequencing_proba_nz, sequencing_probas_nz_periods_0 %>% pull(prop_cases_sequenced)))

results_model_six_nz_p_test_100_periods <- results_model_six_nz_p_test_100_periods %>%
  mutate(detection_proba = testing_proba * sequencing_proba)

results_model_six_nz_p_test_100_n_divergent <- data.frame(matrix(data = 0, nrow = 5, ncol = 3))
names(results_model_six_nz_p_test_100_n_divergent) <- c("period", "country", "n_divergent")

results_model_six_nz_p_test_100_n_divergent <- results_model_six_nz_p_test_100_n_divergent %>%
  mutate(period = c("all", as.character(1:4)),
         country = rep(x = "New Zealand", times = 5))

for(ii in 1:5) {
  
  results_ii <- list_results_model_six_nz_p_test_100_periods[[ii]]
  
  if (!(is.numeric(results_ii))) {
    
    results_model_six_nz_p_test_100_periods$R_estimate[ii] <- get_posterior_mean(results_ii, par = c("R"))[, "mean-all chains"]
    results_model_six_nz_p_test_100_periods$R_lower_cred_int[ii] <- as.data.frame(summary(results_ii)[[1]])[c("R"), "2.5%"]
    results_model_six_nz_p_test_100_periods$R_upper_cred_int[ii] <- as.data.frame(summary(results_ii)[[1]])[c("R"), "97.5%"]
    results_model_six_nz_p_test_100_periods$R_Rhat[ii] <- as.data.frame(summary(results_ii)[[1]])[c("R"), "Rhat"]
    
    results_model_six_nz_p_test_100_periods$k_estimate[ii] <- get_posterior_mean(results_ii, par = c("k"))[, "mean-all chains"]
    results_model_six_nz_p_test_100_periods$k_lower_cred_int[ii] <- as.data.frame(summary(results_ii)[[1]])[c("k"), "2.5%"]
    results_model_six_nz_p_test_100_periods$k_upper_cred_int[ii] <- as.data.frame(summary(results_ii)[[1]])[c("k"), "97.5%"]
    results_model_six_nz_p_test_100_periods$k_Rhat[ii] <- as.data.frame(summary(results_ii)[[1]])[c("k"), "Rhat"]
    
    results_model_six_nz_p_test_100_periods$mutation_proba_estimate[ii] <- get_posterior_mean(results_ii, par = c("mutation_proba"))[, "mean-all chains"]
    results_model_six_nz_p_test_100_periods$mutation_proba_lower_cred_int[ii] <- as.data.frame(summary(results_ii)[[1]])[c("mutation_proba"), "2.5%"]
    results_model_six_nz_p_test_100_periods$mutation_proba_upper_cred_int[ii] <- as.data.frame(summary(results_ii)[[1]])[c("mutation_proba"), "97.5%"]
    results_model_six_nz_p_test_100_periods$mutation_proba_Rhat[ii] <- as.data.frame(summary(results_ii)[[1]])[c("mutation_proba"), "Rhat"]
    
    results_model_six_nz_p_test_100_n_divergent$n_divergent[ii] <- get_num_divergent(results_ii)
    
    ggsave(plot = rstan::traceplot(results_ii, inc_warmup = TRUE),
           filename = paste0("plots/new_zealand/model_six/traceplot_model_six_nz_p_test_100_period_", ii, ".png"))
    
  }
  
}


# create summary of results of parameter estimations
results_model_six_nz_periods <- rbind(results_model_six_nz_p_test_050_periods,
                                      results_model_six_nz_p_test_080_periods,
                                      results_model_six_nz_p_test_100_periods)


# save `results_model_six_nz_periods` as csv file
write_csv(results_model_six_nz_periods, file = path_results_model_six_nz_periods_processed)


