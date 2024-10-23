

# remark: set values of `do_new_load_data`, `do_new_sim`, `do_new_ppc` in `setup.R`
# to define which parts of the data processing shall be done from scratch again

# files that need not to be run to create all results presented in the supplementary material are commented out


# data processing ----

# preprocess data from Switzerland, Denmark and Germany: clusters
source("R/data_processing/01_dp_preprocess_data_clusters.R", echo = FALSE)

# preprocess data from Switzerland, Denmark and Germany: cases, sequences and clusters
source("R/data_processing/02_dp_data_cases_sequences_clusters.R", echo = FALSE)

# # process data: results of simulation study of model one
# source("R/data_processing/03_dp_sim_study_model_one.R", echo = FALSE)

# process data: results of simulation study of model five
source("R/data_processing/04_dp_sim_study_model_five.R", echo = FALSE)

# process data: results of parameter estimations with model one for Switzerland, Denmark and Germany
source("R/data_processing/05_dp_results_model_one.R", echo = FALSE)

# process data: results of parameter estimations with model two for Switzerland, Denmark and Germany
source("R/data_processing/06_dp_results_model_two.R", echo = FALSE)

# process data: results of parameter estimations with model three for Switzerland, Denmark and Germany
source("R/data_processing/07_dp_results_model_three.R", echo = FALSE)

# process data: results of parameter estimations with model four for Switzerland, Denmark and Germany
source("R/data_processing/08_dp_results_model_four.R", echo = FALSE)

# process data: results of parameter estimations with model five for Switzerland, Denmark and Germany
source("R/data_processing/09_dp_results_model_five.R", echo = FALSE)

# process data: results of parameter estimations with model six for Switzerland, Denmark and Germany
source("R/data_processing/10_dp_results_model_six.R", echo = FALSE)

# process data: reference estimate of effective reproduction number for Switzerland, Denmark and Germany
source("R/data_processing/11_dp_estimated_r_e.R", echo = FALSE)

# # process data: results of posterior predictive check of model one for Switzerland, Denmark and Germany
# source("R/data_processing/12_dp_ppc_model_one.R", echo = FALSE)

# process data: results of posterior predictive check of model five for Switzerland, Denmark and Germany
source("R/data_processing/13_dp_ppc_model_five.R", echo = FALSE)

# # process data: results of goodness of fit check of model one for Switzerland, Denmark and Germany
# source("R/data_processing/14_dp_goodness_of_fit_model_one.R", echo = FALSE)

# process data: results of goodness of fit check of model five for Switzerland, Denmark and Germany
source("R/data_processing/15_dp_goodness_of_fit_model_five.R", echo = FALSE)

# process data from New Zealand: clusters, mutation probability and sequencing probability
source("R/data_processing/16_dp_cluster_mut_proba_seq_proba_nz.R", echo = FALSE)

# process data: results of parameter estimations with model four for New Zealand
source("R/data_processing/17_dp_results_model_four_nz.R", echo = FALSE)



# create plots ----

# create plot of transmission tree
source("R/create_plots/cp_01_model_transmission_tree.R", echo = FALSE)

# create data overview plots for Switzerland, Denmark and Germany individually
source("R/create_plots/cp_02_results_data_individual_countries.R", echo = FALSE)

# create data overview plots for Switzerland, Denmark and Germany together
source("R/create_plots/cp_03_results_data_multiple_countries.R", echo = FALSE)

# create plots of prior distributions
source("R/create_plots/cp_04_prior_distributions.R", echo = FALSE)

# # create plots of results of simulation study of model one
# source("R/create_plots/cp_05_sim_results_model_one.R", echo = FALSE)

# create plots of results of simulation study of model five
source("R/create_plots/cp_06_sim_results_model_five.R", echo = FALSE)

# # create plots of results of parameter estimations with model one for Switzerland, Denmark and Germany individually
# source("R/create_plots/cp_07_results_model_one_individual_countries.R", echo = FALSE)

# create plots of results of parameter estimations with model one for Switzerland, Denmark and Germany together
source("R/create_plots/cp_08_results_model_one_multiple_countries.R", echo = FALSE)

# # create plots of results of parameter estimations with model two for Switzerland, Denmark and Germany individually
# source("R/create_plots/cp_09_results_model_two_individual_countries.R", echo = FALSE)

# create plots of results of parameter estimations with model two for Switzerland, Denmark and Germany together
source("R/create_plots/cp_10_results_model_two_multiple_countries.R", echo = FALSE)

# create plots of results of parameter estimations with model three for Switzerland, Denmark and Germany together
source("R/create_plots/cp_11_results_model_three_multiple_countries.R", echo = FALSE)

# create plots of results of parameter estimations with model four for Switzerland, Denmark and Germany together
source("R/create_plots/cp_12_results_model_four_multiple_countries.R", echo = FALSE)

# create plots of results of parameter estimations with model five for Switzerland, Denmark and Germany together
source("R/create_plots/cp_13_results_model_five_multiple_countries.R", echo = FALSE)

# create plots of results of parameter estimations with model six for Switzerland, Denmark and Germany together
source("R/create_plots/cp_14_results_model_six_multiple_countries.R", echo = FALSE)

# # create plots of results of parameter estimations with models one and two for Switzerland, Denmark and Germany individually
# source("R/create_plots/cp_15_results_models_one_two_individual_countries.R", echo = FALSE)

# # create plots of results of parameter estimations with models one and two for Switzerland, Denmark and Germany together
# source("R/create_plots/cp_16_results_models_one_two_multiple_countries.R", echo = FALSE)

# # create plots of results of parameter estimations with models one and three for Switzerland, Denmark and Germany together
# source("R/create_plots/cp_17_results_models_one_three_multiple_countries.R", echo = FALSE)

# # create plots of results of parameter estimations with models one and four for Switzerland, Denmark and Germany together
# source("R/create_plots/cp_18_results_models_one_four_multiple_countries.R", echo = FALSE)

# # create plots of results of parameter estimations with models one and five for Switzerland, Denmark and Germany together
# source("R/create_plots/cp_19_results_models_one_five_multiple_countries.R", echo = FALSE)

# # create plots of results of parameter estimations with models one and six for Switzerland, Denmark and Germany together
# source("R/create_plots/cp_20_results_models_one_six_multiple_countries.R", echo = FALSE)

# create plots of results of parameter estimations with models five and one for Switzerland, Denmark and Germany together
source("R/create_plots/cp_21_results_models_five_one_multiple_countries.R", echo = FALSE)

# create plots of results of parameter estimations with models five and two for Switzerland, Denmark and Germany together
source("R/create_plots/cp_22_results_models_five_two_multiple_countries.R", echo = FALSE)

# create plots of results of parameter estimations with models five and three for Switzerland, Denmark and Germany together
source("R/create_plots/cp_23_results_models_five_three_multiple_countries.R", echo = FALSE)

# create plots of results of parameter estimations with models five and four for Switzerland, Denmark and Germany together
source("R/create_plots/cp_24_results_models_five_four_multiple_countries.R", echo = FALSE)

# create plots of results of parameter estimations with models five and six for Switzerland, Denmark and Germany together
source("R/create_plots/cp_25_results_models_five_six_multiple_countries.R", echo = FALSE)

# # create plots of results of posterior predictive check of model one
# source("R/create_plots/cp_26_ppc_results_model_one.R", echo = FALSE)

# create plots of results of posterior predictive check of model five
source("R/create_plots/cp_27_ppc_results_model_five.R", echo = FALSE)

# # create plots of results of goodness of fit check of model one
# source("R/create_plots/cp_28_goodness_of_fit_model_one.R", echo = FALSE)

# create plots of results of goodness of fit check of model five
source("R/create_plots/cp_29_goodness_of_fit_model_five.R", echo = FALSE)

# create plots of results of parameter estimations with model four for New Zealand
source("R/create_plots/cp_30_results_model_four_nz.R", echo = FALSE)



# create tables ----

# create overview tables of data (number of cases, sequences and clusters) for Switzerland, Denmark and Germany individually
source("R/create_tables/01_ct_overview_data.R", echo = FALSE)

# create overview tables of results of parameter estimations with model one for Switzerland, Denmark and Germany individually
source("R/create_tables/02_ct_results_model_one.R", echo = FALSE)

# create overview tables of results of parameter estimations with model two for Switzerland, Denmark and Germany individually
source("R/create_tables/03_ct_results_model_two.R", echo = FALSE)

# create overview tables of results of parameter estimations with model three for Switzerland, Denmark and Germany individually
source("R/create_tables/04_ct_results_model_three.R", echo = FALSE)

# create overview tables of results of parameter estimations with model four for Switzerland, Denmark and Germany individually
source("R/create_tables/05_ct_results_model_four.R", echo = FALSE)

# create overview tables of results of parameter estimations with model five for Switzerland, Denmark and Germany individually
source("R/create_tables/06_ct_results_model_five.R", echo = FALSE)

# create overview tables of results of parameter estimations with model six for Switzerland, Denmark and Germany individually
source("R/create_tables/07_ct_results_model_six.R", echo = FALSE)

# create overview tables of results of parameter estimations with model four for New Zealand
source("R/create_tables/08_ct_results_model_four_nz.R", echo = FALSE)

