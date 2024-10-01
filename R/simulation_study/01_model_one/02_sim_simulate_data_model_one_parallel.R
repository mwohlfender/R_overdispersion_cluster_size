
# simulation study model one
# simulate data (run in parallel)
# This R-script is run on the high performance computing cluster of the University of Bern, UBELIX.



# load libraries ----
library(tidyverse)
library(estRodis)



# define paths ----

# set working directory
setwd(dir = "/storage/homefs/mw22f082/projects/genomic_trees/paper/simulation_study/model_one/")

# path where parameter grid for simulation study is stored
path_data_sim_parameters_grid_model_one <- "data/parameters_grid_simulation_model_one.csv"



# define parameters ----

# define row of data_parameters from which parameters for simulations shall be taken
index <- as.numeric(commandArgs(trailingOnly = TRUE)[1])



# read data ----

# read parameters for simulation study
data_parameters <- read_csv(path_data_sim_parameters_grid_model_one)

# simulate identical sequence clusters ----
simulated_clusters <- estRodis_simulate_cluster_sizes(n_clusters = data_parameters$n_clusters[index],
                                                      max_cluster_size = data_parameters$max_cluster_size[index],
                                                      R = data_parameters$R[index],
                                                      k = data_parameters$k[index],
                                                      yearly_mutation_rate = data_parameters$yearly_mutation_rate[index],
                                                      mean_generation_interval = data_parameters$mean_generation_interval[index],
                                                      testing_proba = data_parameters$testing_proba[index],
                                                      sequencing_proba = data_parameters$sequencing_proba[index]) 

simulated_clusters <- simulated_clusters %>% dplyr::select(c("size", "frequency"))

# save results of identical sequence cluster simulation
write_csv(x = simulated_clusters, file = paste0("results/data_simulation/simulated_clusters_model_one_",
                                                str_pad(index, ceiling(log(nrow(data_parameters), base = 10)), pad = "0"), ".csv"))








