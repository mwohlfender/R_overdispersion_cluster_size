
# simulation study model five
# simulate data (run in parallel)
# This R-script is run on the high performance computing cluster of the University of Bern, UBELIX.



# load libraries ----
library(tidyverse)
library(estRodis)



# define paths ----

# set working directory
setwd(dir = "/storage/homefs/mw22f082/projects/genomic_trees/revisions/simulation_study/model_five/")

# path where parameter grid for simulation study is stored
path_data_sim_parameters_grid_model_five <- "data/parameters_grid_simulation_model_five.csv"



# define parameters ----

# define row of data_parameters from which parameters for simulations shall be taken
index <- as.numeric(commandArgs(trailingOnly = TRUE)[1])



# read data ----

# read parameters for simulation study
data_parameters <- read_csv(path_data_sim_parameters_grid_model_five)

# simulate identical sequence clusters ----
simulated_clusters <- estRodis_simulate_cluster_sizes_v2(n_clusters = data_parameters$n_clusters[index],
                                                         max_cluster_size = data_parameters$max_cluster_size[index],
                                                         R = data_parameters$R[index],
                                                         k = data_parameters$k[index],
                                                         mutation_proba = data_parameters$mutation_proba[index],
                                                         testing_proba = data_parameters$testing_proba[index],
                                                         sequencing_proba = data_parameters$sequencing_proba[index]) 

simulated_clusters <- simulated_clusters %>% dplyr::select(c("size", "frequency"))

# save results of identical sequence cluster simulation
write_csv(x = simulated_clusters, file = paste0("results/data_simulation/simulated_clusters_model_five_",
                                                str_pad(index, ceiling(log(nrow(data_parameters), base = 10)), pad = "0"), ".csv"))








