
# posterior predictive check of results of model 5
# do simulations (run in parallel)
# This R-script is run on the high performance computing cluster of the University of Bern, UBELIX.



# load libraries ----
library(tidyverse)
library(estRodis)



# define paths ----

# set working directory
setwd(dir = "/storage/homefs/mw22f082/projects/genomic_trees/revisions/ppc/model_five/")

# path where parameter grid for posterior predictive check is stored
path_data_post_pred_parameters_grid <- "data/data_parameters_ppc_model_five.csv"


# define parameters ----

# define row of data_parameters from which parameters for simulations shall be taken
index <- as.numeric(commandArgs(trailingOnly = TRUE)[1])



# read data ----

# read parameters for simulation of identical sequence clusters
data_parameters <- read_csv(path_data_post_pred_parameters_grid)

# simulate identical sequence clusters
simulated_clusters <- estRodis_simulate_cluster_sizes_v2(n_clusters = data_parameters$n_clusters[index],
                                                         max_cluster_size = data_parameters$max_cluster_size[index],
                                                         R = data_parameters$R[index],
                                                         k = data_parameters$k[index],
                                                         mutation_proba = data_parameters$mutation_proba[index],
                                                         testing_proba = data_parameters$testing_proba[index],
                                                         sequencing_proba = data_parameters$sequencing_proba[index]) 

result <- simulated_clusters %>% dplyr::select(c("size", "frequency"))

# save results
write_csv(result, file = paste0("results/", data_parameters$country[index],
                                "/simulated_clusters_model_five_", 
                                data_parameters$country[index], "_", 
                                data_parameters$month[index], "_",
                                data_parameters$index_param_comb[index], "_", 
                                data_parameters$index_repetition[index], ".csv"))


