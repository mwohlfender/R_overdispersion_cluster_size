

# remark: set values of `do_new_load_data`, `do_new_sim`, `do_new_ppc` in `setup.R`
# to define which parts of the data processing shall be done from scratch again


# preprocess data: clusters
source("R/data_processing/01_dp_preprocess_data_clusters.R", echo = FALSE)

# preprocess data: cases, sequences and clusters
source("R/data_processing/02_dp_data_cases_sequences_clusters.R", echo = FALSE)

# process data: results of parameter estimations using model one
source("R/data_processing/03_dp_results_model_one.R", echo = FALSE)

# process data: results of parameter estimations using model two
source("R/data_processing/04_dp_results_model_two.R", echo = FALSE)

# process data: reference estimate of effective reproduction number
source("R/data_processing/05_dp_estimated_r_e.R", echo = FALSE)

# process data: results of simulation study of model one
source("R/data_processing/06_dp_sim_study_model_one.R", echo = FALSE)

# process data: results of posterior predictive check of model one
source("R/data_processing/07_dp_ppc_model_one.R", echo = FALSE)

# create plots of prior distributions
source("R/create_plots/01_plots_prior_distributions.R", echo = FALSE)

# create plots of data used for parameter estimation for individual countries
source("R/create_plots/02_plots_results_data_individual_countries.R", echo = FALSE)

# create plots of data used for parameter estimation for all countries
source("R/create_plots/03_plots_results_data_all_countries.R", echo = FALSE)

# create plots of parameter estimations for individual countries with model one
source("R/create_plots/04_plots_results_model_one_individual_countries.R", echo = FALSE)

# create plots of parameter estimations for all countries with model one
source("R/create_plots/05_plots_results_model_one_all_countries.R", echo = FALSE)

# create plots of parameter estimations for individual countries with model two
source("R/create_plots/06_plots_results_model_two_individual_countries.R", echo = FALSE)

# create plots of parameter estimations for all countries with model two
source("R/create_plots/07_plots_results_model_two_all_countries.R", echo = FALSE)

# create plots of parameter estimations for individual countries with models one and two
source("R/create_plots/08_plots_results_models_one_two_individual_countries.R", echo = FALSE)

# create plots of parameter estimations for all countries with models one and two
source("R/create_plots/09_plots_results_models_one_two_all_countries.R", echo = FALSE)

# create plot of transmission tree
source("R/create_plots/10_plots_model_transmission_tree.R", echo = FALSE)

# create plot of results of simulation study of model one
source("R/create_plots/11_plots_sim_results_model_one.R", echo = FALSE)

# create plots of results of posterior predictive check of model one
source("R/create_plots/12_plots_ppc_results_model_one.R", echo = FALSE)

# create plot of results of simulation study of model one
source("R/addenda/06_simulation_study/cp_sim_study_model_one_v2.R", echo = FALSE)

# create overview table of data: number of cases, sequences and clusters
source("R/create_tables/01_ct_overview_data.R", echo = FALSE)

# create overview table of results of parameter estimation with model one
source("R/create_tables/02_ct_results_model_one.R", echo = FALSE)

# create overview table of results of parameter estimation with model two
source("R/create_tables/03_ct_results_model_two.R", echo = FALSE)






