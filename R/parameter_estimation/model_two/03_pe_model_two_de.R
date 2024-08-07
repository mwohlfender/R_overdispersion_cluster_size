
# Project: R and k
# Estimate parameters from cluster data from Germany (model 2) (constant value for testing probability)
# This R-script is run on the high performance computing cluster of the University of Bern, UBELIX.



# load libraries ----
library(tidyverse)
library(estRodis)



# define paths ----

# set working directory
setwd(dir = "/storage/homefs/mw22f082/projects/genomic_trees/genomic_trees_ubelix_XXX/R_and_k/")

# path where the monthly number of clusters of different sizes of Germany during 2021 is stored
path_data_cluster_sizes_de_months <- "data/germany/processed/data_cluster_sizes_de_2021_months.csv"

# path where the estimated monthly sequencing probabilities of Germany during 2021 are stored
path_sequencing_probas_de_2021_months <- "data/germany/processed/sequencing_probas_de_2021_months.csv"



# define parameters ----

# month for which parameters shall be estimated
month_0 <- as.numeric(commandArgs(trailingOnly = TRUE)[1])

# determine testing probabilities for model two
testing_probas_model_two_de_2021_months <- c(0.576, 0.576, 0.576, 0.576, 0.576, 0.576, 0.576, 0.576, 0.35, 0.35, 0.35, 0.35)



# read data ----

# read monthly number of clusters of different sizes of Germany during 2021
data_cluster_sizes_de_months_0 <- read_csv(file = path_data_cluster_sizes_de_months)

# read estimated monthly sequencing probabilities of Germany during 2021
sequencing_probas_de_2021_months_0 <- read_csv(file = path_sequencing_probas_de_2021_months)



# process data ----

# determine number of clusters of different sizes of Germany during the month for which parameters shall be estimated
data_cluster_sizes_de_month <- data_cluster_sizes_de_months_0 |> filter(month == month_0)

# determine sequencing probabilities 
sequencing_probas_de_2021_months <- sequencing_probas_de_2021_months_0 |> pull("seq_proba_de")



# estimate parameters ----

# activate parallel computing on the different chains
options(mc.cores = parallelly::availableCores())

print(parallelly::availableCores())

# run model
results_parameter_estimation <- estRodis_estimate_parameters_two(clusters_size = data_cluster_sizes_de_month |> pull("size"),
                                                                 clusters_freq = data_cluster_sizes_de_month |> pull("frequency"),
                                                                 testing_proba = testing_probas_model_two_de_2021_months[month_0],
                                                                 sequencing_proba = sequencing_probas_de_2021_months[month_0],
                                                                 warmup = 500,
                                                                 iter = 1000,
                                                                 chains = 4,
                                                                 cores = 4,
                                                                 thin = 1,
                                                                 control = list(adapt_delta = 0.99),
                                                                 init = lapply(1:4, FUN = function(x) estRodis_init_params_model_two()),
                                                                 sample_file = paste0("results/parameter_estimates_model_two_germany_", month_0, "_sample"),
                                                                 diagnostic_file = paste0("results/parameter_estimates_model_two_germany_", month_0, "_dia"))

# save results of parameter estimation
saveRDS(results_parameter_estimation, file = paste0("results/parameter_estimates_model_two_germany_", month_0, ".rds"))

# save session info
writeLines(capture.output(sessionInfo()), con = paste0("sessionInfo_model_two_germany_", month_0, ".txt"))


