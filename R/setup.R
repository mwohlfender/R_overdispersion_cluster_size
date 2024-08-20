

# install packages ----

# install stable version of estRodis package
# devtools::install_github("mwohlfender/estRodis@v0.0.1-zeta", ref = "main", force = TRUE)

# install newest version of estRodis package
# devtools::install_github("mwohlfender/estRodis", ref = "main", force = TRUE)
# devtools::install_github(repo = "mwohlfender/estRodis", ref = "revisions", force = TRUE)


# load libraries ----
library(colorspace)
library(cowplot)
library(flextable)
library(ggraph)
library(ggpubr)
library(ggtext)
library(lubridate)
library(Metrics)
library(paletteer)
library(patchwork)
library(rjson)
library(rstan)
library(scales)
library(tidyverse)
library(tidygraph)
library(viridis)
library(stats)

library(estRodis)

ls("package:estRodis")




# source functions ----
path_script <- "R/functions/"
files <- dir(path = path_script)
lapply(X = files, FUN = function(x) {source(paste0(path_script, x), echo = FALSE)})



# indicate whether certain parts of the analysis shall be done from scratch again
# (no stored intermediate results of data processing used)

# load and process data 
do_new_load_data <- FALSE

# process results of simulation study
do_new_sim <- FALSE

# process results of posterior predictive check
do_new_ppc <- FALSE



# define paths ----

# paths where raw identical sequence cluster data is stored
# obtained from gisaid (https://gisaid.org/)
path_data_clusters_ch_raw <- "data/switzerland/raw/Switzerland_cluster_distribution_dates_100whole.tsv"
path_data_clusters_dk_raw <- "data/denmark/raw/Denmark_cluster_distribution_dates_100whole.tsv"
path_data_clusters_de_raw <- "data/germany/raw/Germany_cluster_distribution_dates_100whole.tsv"
path_data_clusters_nz_raw <- "data/new_zealand/raw/df_cluster_by_period_NZ.rds"

# paths where processed identical sequence cluster data is stored 
path_data_clusters_ch_processed <- "data/switzerland/processed/data_clusters_ch_processed.csv"
path_data_clusters_dk_processed <- "data/denmark/processed/data_clusters_dk_processed.csv"
path_data_clusters_de_processed <- "data/germany/processed/data_clusters_de_processed.csv"

# paths where the dates of sampling of all sequences contained in the identical sequence clusters are stored
path_data_sequences_dates_ch_2021 <- "data/switzerland/raw/switzerland_date_only.csv"
path_data_sequences_dates_dk_2021 <- "data/denmark/raw/denmark_date_only.csv"
path_data_sequences_dates_de_2021 <- "data/germany/raw/germany_date_only.csv"


# path where raw data about shares of different virus variants is stored
# obtained from CoVariants (https://covariants.org/per-country?country=Germany&country=Denmark&country=Switzerland)
path_data_variants_shares_ch_dk_de_raw <- "data/all_countries/covariants/raw/data_variants_shares_ch_dk_de_raw.csv"

# path where processed data about shares of different virus variants will be stored
path_data_variants_shares_ch_dk_de_processed <- "data/all_countries/covariants/processed/data_variants_shares_ch_dk_de_processed.csv"


# path where raw new confirmed cases data will be stored
# obtained from 
# (1) For Switzerland: FOPH (https://www.covid19.admin.ch/en/overview)
# (2) For Denmark: Statens Serums Institut (https://experience.arcgis.com/experience/220fef27d07d438889d651cc2e00076c/page/Covid-19-Regionalt/)
# (3) For Germany: Robert Koch Institut (https://github.com/robert-koch-institut/COVID-19_7-Tage-Inzidenz_in_Deutschland/blob/main/)
path_data_new_confirmed_cases_ch_raw <- "data/switzerland/raw/data_new_confirmed_cases_ch_raw.csv"
path_data_new_confirmed_cases_dk_raw <- "data/denmark/raw/data_new_confirmed_cases_dk_raw.csv"
path_data_new_confirmed_cases_de_raw <- "data/germany/raw/data_new_confirmed_cases_de_raw.csv"

# path where processed new confirmed cases data will be stored
path_data_new_confirmed_cases_ch_processed <- "data/switzerland/processed/data_new_confirmed_cases_ch_processed.csv"
path_data_new_confirmed_cases_dk_processed <- "data/denmark/processed/data_new_confirmed_cases_dk_processed.csv"
path_data_new_confirmed_cases_de_processed <- "data/germany/processed/data_new_confirmed_cases_de_processed.csv"



# paths where raw data of estimates of R_e based on confirmed cases are stored
path_data_r_e_ch_raw <- "data/switzerland/raw/data_r_e_ch_raw.csv"
path_data_r_e_dk_raw <- "data/denmark/raw/data_r_e_dk_raw.csv"
path_data_r_e_de_raw <- "data/germany/raw/data_r_e_de_raw.csv"

# paths where processed data of estimates of R_e based on confirmed cases are stored
path_data_r_e_ch_processed <- "data/switzerland/processed/data_r_e_ch_processed.csv"
path_data_r_e_dk_processed <- "data/denmark/processed/data_r_e_dk_processed.csv"
path_data_r_e_de_processed <- "data/germany/processed/data_r_e_de_processed.csv"



# paths where data summary tables of the number of confirmed cases, the number of sequences  and the number of clusters will be stored
path_data_cases_sequences_clusters_ch_2021_months <- "data/switzerland/processed/data_cases_sequences_clusters_ch_2021_months.csv"
path_data_cases_sequences_clusters_dk_2021_months <- "data/denmark/processed/data_cases_sequences_clusters_dk_2021_months.csv"
path_data_cases_sequences_clusters_de_2021_months <- "data/germany/processed/data_cases_sequences_clusters_de_2021_months.csv"

# paths where the monthly numbers of clusters of different sizes in Switzerland, Denmark and Germany during 2021 will be stored
path_data_cluster_sizes_ch_2021_months <- "data/switzerland/processed/data_cluster_sizes_ch_2021_months.csv"
path_data_cluster_sizes_dk_2021_months <- "data/denmark/processed/data_cluster_sizes_dk_2021_months.csv"
path_data_cluster_sizes_de_2021_months <- "data/germany/processed/data_cluster_sizes_de_2021_months.csv"


# paths where the estimated monthly sequencing probabilities of Switzerland, Denmark and Germany during 2021 are stored
path_sequencing_probas_ch_2021_months <- "data/switzerland/processed/sequencing_probas_ch_2021_months.csv"
path_sequencing_probas_dk_2021_months <- "data/denmark/processed/sequencing_probas_dk_2021_months.csv"
path_sequencing_probas_de_2021_months <- "data/germany/processed/sequencing_probas_de_2021_months.csv"
path_sequencing_probas_nz_periods <- "data/new_zealand/raw/df_prop_sequenced_per_period.rds"


# paths where results of parameter estimations of model 1 (prior distribution for testing probability) are stored
path_results_model_one_ch <- "results/switzerland/parameter_estimations/model_one/parameter_estimates_model_one_switzerland_"
path_results_model_one_dk <- "results/denmark/parameter_estimations/model_one/parameter_estimates_model_one_denmark_"
path_results_model_one_de <- "results/germany/parameter_estimations/model_one/parameter_estimates_model_one_germany_"
path_results_model_one_nz <- "results/new_zealand/parameter_estimations/model_one/parameter_estimates_model_one_new_zealand_"

# paths where overview of results of parameter estimations of model 1 (prior distribution for testing probability) will be stored
path_results_model_one_ch_dk_de_processed <- "results/all_countries/parameter_estimations/model_one/results_model_one_ch_dk_de_2021_months.csv"
path_results_model_one_nz_periods_processed <- "results/new_zealand/parameter_estimations/model_one/results_model_one_nz_periods.csv"


# paths where results of parameter estimations of model 2 (constant value for testing probability) are stored
path_results_model_two_ch <- "results/switzerland/parameter_estimations/model_two/parameter_estimates_model_two_switzerland_"
path_results_model_two_dk <- "results/denmark/parameter_estimations/model_two/parameter_estimates_model_two_denmark_"
path_results_model_two_de <- "results/germany/parameter_estimations/model_two/parameter_estimates_model_two_germany_"
path_results_model_two_nz <- "results/new_zealand/parameter_estimations/model_two/parameter_estimates_model_two_new_zealand_"

# paths where overview of results of parameter estimations of model 2 (constant value for testing probability) will be stored
path_results_model_two_ch_dk_de_processed <- "results/all_countries/parameter_estimations/model_two/results_model_two_ch_dk_de_2021_months.csv"
path_results_model_two_nz_periods_processed <- "results/new_zealand/parameter_estimations/model_two/results_model_two_nz_periods.csv"


# paths where results of parameter estimations of model 3
# (prior distributions for R, k and testing probability and constant value for mutation probability) are stored
path_results_model_three_ch <- "results/switzerland/parameter_estimations/model_three/parameter_estimates_model_three_switzerland_"
path_results_model_three_dk <- "results/denmark/parameter_estimations/model_three/parameter_estimates_model_three_denmark_"
path_results_model_three_de <- "results/germany/parameter_estimations/model_three/parameter_estimates_model_three_germany_"
path_results_model_three_nz <- "results/new_zealand/parameter_estimations/model_three/parameter_estimates_model_three_new_zealand_"

# paths where overview of results of parameter estimations of model 3
# (prior distributions for R, k and testing probability and constant value for mutation probability) will be stored
path_results_model_three_ch_dk_de_processed <- "results/all_countries/parameter_estimations/model_three/results_model_three_ch_dk_de_2021_months.csv"
path_results_model_three_nz_periods_processed <- "results/new_zealand/parameter_estimations/model_three/results_model_three_nz_periods.csv"


# path where results of parameter estimations of model 4
# (prior distributions for R and k and constant values for mutation probability and testing probability) are stored
path_results_model_four_ch <- "results/switzerland/parameter_estimations/model_four/parameter_estimates_model_four_switzerland_"
path_results_model_four_dk <- "results/denmark/parameter_estimations/model_four/parameter_estimates_model_four_denmark_"
path_results_model_four_de <- "results/germany/parameter_estimations/model_four/parameter_estimates_model_four_germany_"
path_results_model_four_nz <- "results/new_zealand/parameter_estimations/model_four/parameter_estimates_model_four_new_zealand_"

# paths where overview of results of parameter estimations of model 4
# (prior distributions for R and k and constant values for mutation probability and testing probability) will be stored
path_results_model_four_ch_dk_de_processed <- "results/all_countries/parameter_estimations/model_four/results_model_four_ch_dk_de_2021_months.csv"
path_results_model_four_nz_periods_processed <- "results/new_zealand/parameter_estimations/model_four/results_model_four_nz_periods.csv"


# paths where results of parameter estimations of model 5
# (prior distributions for R, k, mutation probability and testing probability) are stored
path_results_model_five_ch <- "results/switzerland/parameter_estimations/model_five/parameter_estimates_model_five_switzerland_"
path_results_model_five_dk <- "results/denmark/parameter_estimations/model_five/parameter_estimates_model_five_denmark_"
path_results_model_five_de <- "results/germany/parameter_estimations/model_five/parameter_estimates_model_five_germany_"
path_results_model_five_nz <- "results/new_zealand/parameter_estimations/model_five/parameter_estimates_model_five_new_zealand_"

# path where overview of results of parameter estimations of model 5
# (prior distributions for R, k, mutation probability and testing probability) will be stored
path_results_model_five_ch_dk_de_processed <- "results/all_countries/parameter_estimations/model_five/results_model_five_ch_dk_de_2021_months.csv"
path_results_model_five_nz_periods_processed <- "results/new_zealand/parameter_estimations/model_five/results_model_five_nz_periods.csv"


# path where results of parameter estimations of model 6
# (prior distributions for R, k and mutation probability and constant value for testing probability) are stored
path_results_model_six_ch <- "results/switzerland/parameter_estimations/model_six/parameter_estimates_model_six_switzerland_"
path_results_model_six_dk <- "results/denmark/parameter_estimations/model_six/parameter_estimates_model_six_denmark_"
path_results_model_six_de <- "results/germany/parameter_estimations/model_six/parameter_estimates_model_six_germany_"
path_results_model_six_nz <- "results/new_zealand/parameter_estimations/model_six/parameter_estimates_model_six_new_zealand_"

# path where overview of results of parameter estimations of model 6
# (prior distributions for R, k and mutation probability and constant value for testing probability) will be stored
path_results_model_six_ch_dk_de_processed <- "results/all_countries/parameter_estimations/model_six/results_model_six_ch_dk_de_2021_months.csv"
path_results_model_six_nz_periods_processed <- "results/new_zealand/parameter_estimations/model_six/results_model_six_nz_periods.csv"



# paths where parameter grid and list of indices for posterior predictive of model 1 check are stored
path_data_post_pred_model_one_parameters_grid <- "data/all_countries/posterior_predictive_check/model_one/data_parameters_ppc_model_one.csv"
path_data_post_pred_model_one_index_parameters <- "data/all_countries/posterior_predictive_check/model_one/index_parameters_ppc_model_one.txt"

# paths where simulation results of posterior predictive check of model 1 are stored
path_results_post_pred_model_one_ch_raw <- "results/switzerland/posterior_predictive_check/model_one/simulated_clusters_model_one_switzerland_"
path_results_post_pred_model_one_dk_raw <- "results/denmark/posterior_predictive_check/model_one/simulated_clusters_model_one_denmark_"
path_results_post_pred_model_one_de_raw <- "results/germany/posterior_predictive_check/model_one/simulated_clusters_model_one_germany_"

# path where processed results of posterior predictive check of model 1 are stored
path_results_post_pred_model_one_ch_dk_de_processed <- "results/all_countries/posterior_predictive_check/model_one/results_ppc_model_one_ch_dk_de.csv"



# paths where parameter grid and list of indices for simulation study of model 1 are stored 
path_data_sim_parameters_grid <-  "data/simulation/parameters_grid_simulation.csv"
path_data_sim_indices <- "data/simulation/indices_simulation.txt"
path_param_est_indices <- "data/simulation/indices_estimation.txt"

# path where parameter estimation results of simulation study of model 1 are stored
path_results_sim_raw <- "results/simulation/raw/parameter_estimates_model_one_sim_"

# paths where processed results of simulation study of model 1  are stored
path_results_sim_processed <- "results/simulation/processed/results_sim_model_one_processed.csv"
path_results_sim_processed_v2 <- "results/simulation/processed/results_sim_model_one_processed_v2.csv"

