
# posterior predictive check of results of model 1
# do simulations (run in parallel)
# This R-script is run on the high performance computing cluster of the University of Bern, UBELIX.



# load libraries ----
library(tidyverse)
library(estRodis)



# define paths ----

# set working directory
setwd(dir = "/storage/homefs/mw22f082/projects/genomic_trees/paper/ppc/model_one/")

# path where parameter grid for posterior predictive check is stored
path_data_post_pred_parameters_grid <- "data/data_parameters_ppc_model_one.csv"


# define parameters ----

# define row of data_parameters from which parameters for simulations shall be taken
index <- as.numeric(commandArgs(trailingOnly = TRUE)[1])



# read data ----

# read parameters for simulation of identical sequence clusters
data_parameters <- read_csv(path_data_post_pred_parameters_grid)

# simulate identical sequence clusters
simulated_clusters <- estRodis_simulate_cluster_sizes(n_clusters = data_parameters$n_clusters[index],
                                                      max_cluster_size = data_parameters$max_cluster_size[index],
                                                      R = data_parameters$R[index],
                                                      k = data_parameters$k[index],
                                                      yearly_mutation_rate = data_parameters$yearly_mutation_rate[index],
                                                      mean_generation_interval = data_parameters$mean_generation_interval[index],
                                                      testing_proba = data_parameters$testing_proba[index],
                                                      sequencing_proba = data_parameters$sequencing_proba[index]) 

result <- simulated_clusters |> dplyr::select(c("size", "frequency"))

# save results
write_csv(result, file = paste0("results/", data_parameters$country[index],
                                "/simulated_clusters_model_one_", 
                                data_parameters$country[index], "_", 
                                data_parameters$month[index], "_",
                                data_parameters$index_param_comb[index], "_", 
                                data_parameters$index_repetition[index], ".csv"))


