
# simulation study model five
# estimate parameters using model five (run in parallel)
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

# read simulated identical sequence clusters
simulated_clusters <- read_csv(file = paste0("results/data_simulation/simulated_clusters_model_five_",
                                             str_pad(index, ceiling(log(nrow(data_parameters), base = 10)), pad = "0"), ".csv"))



# estimate parameters ----

# activate parallel computing on the different chains
options(mc.cores = parallelly::availableCores())

# run model
results_parameter_estimation <- estRodis_estimate_parameters_five(clusters_size = simulated_clusters %>% pull("size"),
                                                                  clusters_freq = simulated_clusters %>% pull("frequency"),
                                                                  sequencing_proba = data_parameters$sequencing_proba[index],
                                                                  warmup = 500,
                                                                  iter = 1000,
                                                                  chains = 4,
                                                                  cores = 4,
                                                                  thin = 1,
                                                                  control = list(adapt_delta = 0.99),
                                                                  init = lapply(1:4, FUN = function(x) estRodis_init_params_model_five()),
                                                                  sample_file = paste0("results/parameter_estimation/parameter_estimates_model_five_sim_",
                                                                                       str_pad(index, ceiling(log(nrow(data_parameters), base = 10)), pad = "0"), "_sample"),
                                                                  diagnostic_file = paste0("results/parameter_estimation/parameter_estimates_model_five_sim_",
                                                                                           str_pad(index, ceiling(log(nrow(data_parameters), base = 10)), pad = "0"), "_dia"))

# save results of parameter estimation
saveRDS(results_parameter_estimation, file = paste0("results/parameter_estimation/parameter_estimates_model_five_sim_",
                                                    str_pad(index, ceiling(log(nrow(data_parameters), base = 10)), pad = "0"), ".rds"))



