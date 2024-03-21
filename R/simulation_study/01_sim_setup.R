
# define parameters ----

# define how many times data shall be simulated and parameters estimated per combination of parameters
n_repetitions <- 10

# define parameters for simulations
R_range <- c(0.7, 0.9, 1.1, 1.3)
k_range <- c(0.1, 0.3, 0.5)
yearly_mutation_rate_range <- c(14)
mean_generation_interval_range <- c(5.2)
testing_proba_range <- c(0.1, 0.2, 0.4, 0.6, 0.8)
sequencing_proba_range <- c(0.01, 0.1, 0.3, 0.5)
n_clusters_range <- c(3000)
max_cluster_size_range <- c(1500)
iterations <- seq(from = 1, to = n_repetitions, by = 1)



# create parameter grid ----
parameters_grid <- expand.grid(R = R_range,
                               k = k_range,
                               yearly_mutation_rate = yearly_mutation_rate_range,
                               mean_generation_interval = mean_generation_interval_range,
                               testing_proba = testing_proba_range,
                               sequencing_proba = sequencing_proba_range,
                               n_clusters = n_clusters_range,
                               max_cluster_size = max_cluster_size_range,
                               iteration = iterations)

parameters_grid <- parameters_grid |> mutate(mutation_proba = round(1 - exp(- yearly_mutation_rate / 365.25 * mean_generation_interval), digits = 6),
                                             detection_proba = round(testing_proba * sequencing_proba, digits = 6))



# save results ----

# write parameter grid to csv file
write_csv(x = parameters_grid, file = path_data_sim_parameters_grid)

# write list of indices to txt file
write.table(1:nrow(parameters_grid), file = path_data_sim_indices, sep = "", append = FALSE, row.names = FALSE, col.names = FALSE)
write.table(1:nrow(parameters_grid), file = path_param_est_indices, sep = "", append = FALSE, row.names = FALSE, col.names = FALSE)



