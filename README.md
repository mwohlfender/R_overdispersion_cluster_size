# Estimating $R_0$ and overdispersion in secondary cases from the sequence cluster size distribution of SARS-CoV-2
This repository contains the code of the statistical analysis of the paper "Estimating $R_0$ and overdispersion in secondary cases from the sequence cluster size distribution of SARS-CoV-2" by Emma Hodcroft et al. (ADD LINK TO PAPER).

## (A) Overview of content of repository
The aim of this repository is to provide everything necessary to reproduce the statistical analysis of the paper cited above. All files (R-scripts as well as data) used to obtain the results are contained in this repository. Furthermore, the complete simulated data used for the validation of the model as well as all stanfit files containing the results of the parameter estimation can be found in this repository.

## (B) How to run

## (C) Further remarks

### R package estRodis
The simulation of identical sequence clusters as well as the models to estimate parameters from the sequence cluster size distribution are implemented as functions in an R-package, called estRodis.
The estRodis package can be found here: https://github.com/mwohlfender/estRodis_test

### Emma Hodcroft's sc2_k repository

### Conventions
* Whenever "model_one" is mentioned in comments in the code, this refers to the standard model developed in the paper and "model_two" refers to the alternative model described in the section "Sensitivity analysis" of the supplementary material.

## (D) Detailed information on content of repository

### D.1 R

#### D.1.a setup
Load all necessary R-packages and define paths. `setup.R` needs to be run first.

#### D.1.b main
Contains all steps needed to process data and results and to create figures and tables.

#### D.1.c functions
Custom functions for creating plots. All files in this folder are sourced when running `setup.R`.

#### D.1.d data processing
All R-scripts covering the processing of data and results.

#### D.1.e simulation study
* `01_sim_setup.R` setting up the simulation study: define parameter combinations for which clusters of identical sequences shall be simulated
* `02_sim_simulate_data_parallel.R` simulation of identical sequence clusters (run in parallel on the high performance computing cluster of the University of Bern, UBELIX)
* `03_sim_estimate_parameters_model_one_parallel.R` estimation of parameters from simulated data (run in parallel on the high performance computing cluster of the University of Bern, UBELIX)

#### D.1.f parameter estimation
All R-scripts to estimate parameters from data from Switzerland, Denmark and Germany using the main (model one) or the alternative model (mdoel two). These files were run in parallel on the high performance computing cluster of the University of Bern, UBELIX.

#### D.1.g posterior predictive check
* `01_ppc_model_one_setup.R` setting up the posterior predictive check: define parameter combinations for which clusters of identical sequences shall be simulated
* `02_ppc_model_one_simulations_parallel.R` simulation of identical sequence clusters (run in parallel on the high performance computing cluster of the University of Bern, UBELIX)

#### D.1.h create plots
All R-scripts covering the creation of figures (contained in paper and supplementary material).

#### D.1.i create tables
All R-scripts covering the creation of overview tables of data and results (contained in supplementary material).

### D.2 data
The repository contains the following data files (see folder `data`):

#### D.2.a Switzerland

##### raw
* `Switzerland_cluster_distribution_dates_100whole.tsv` distribution of size of identical sequence clusters (obtained from (ADD LINK TO EMMA'S sc2_k REPOSITORY))
* `data_new_confirmed_cases_ch_raw.csv` number of new confirmed cases  (obtained from https://www.covid19.admin.ch/en/overview)
* `data_r_e_ch_raw.csv` estimate of effective reproduction number on daily basis based on number of confirmed cases (obtained from https://github.com/covid-19-Re/dailyRe-Data)
* `switzerland_date_only.csv` date of sampling of all sequences contained in identical sequence clusters used for the analysis

##### processed
* `data_cases_sequences_clusters_ch_2021_months.csv` overview of number of confirmed cases, number of sequences sampled, number of clusters and size of largest cluster per month in 2021
* `data_cluster_sizes_ch_2021_months.csv` number of clusters of each size in each month of 2021
* `data_clusters_ch_processed.csv` distribution of size of identical sequence clusters, clusters without a valid smapling date eliminated 
* `data_new_confirmed_cases_ch_processed.csv` number of new confirmed cases, filtered to 2021
* `data_r_e_ch_processed.csv` estimate of effective reproduction number on daily basis based on number of confirmed cases, filtered to 2021
* `sequencing_probas_ch_2021_months.csv` probability of a confirmed case being sequenced on monthly basis during 2021

#### D.2.b Denmark

##### raw
* `Denmark_cluster_distribution_dates_100whole.tsv` distribution of size of identical sequence clusters (obtained from (ADD LINK TO EMMA'S sc2_k REPOSITORY))
* `data_new_confirmed_cases_dk_raw.csv` number of new confirmed cases (obtained from https://experience.arcgis.com/experience/220fef27d07d438889d651cc2e00076c/page/Covid-19-Regionalt/)
* `data_r_e_dk_raw.csv` estimate of effective reproduction number on daily basis based on number of confirmed cases (obtained from https://github.com/covid-19-Re/dailyRe-Data)
* `denmark_date_only.csv` date of sampling of all sequences contained in identical sequence clusters used for the analysis

##### processed
* `data_cases_sequences_clusters_dk_2021_months.csv` overview of number of confirmed cases, number of sequences sampled, number of clusters and size of largest cluster per month in 2021
* `data_cluster_sizes_dk_2021_months.csv` number of clusters of each size in each month of 2021
* `data_clusters_dk_processed.csv` distribution of size of identical sequence clusters, clusters without a valid smapling date eliminated 
* `data_new_confirmed_cases_dk_processed.csv` number of new confirmed cases, filtered to 2021
* `data_r_e_dk_processed.csv` estimate of effective reproduction number on daily basis based on number of confirmed cases, filtered to 2021
* `sequencing_probas_dk_2021_months.csv` probability of a confirmed case being sequenced on monthly basis during 2021

#### D.2.c Germany

##### raw
* `Germany_cluster_distribution_dates_100whole.tsv` distribution of size of identical sequence clusters (obtained from (ADD LINK TO EMMA'S sc2_k REPOSITORY))
* `data_new_confirmed_cases_de_raw.csv` number of new confirmed cases (obtained from https://www.rki.de/DE/Content/InfAZ/N/Neuartiges_Coronavirus/Situationsberichte/COVID-19-Trends/COVID-19-Trends.html?__blob=publicationFile#/home)
* `data_r_e_de_raw.csv` estimate of effective reproduction number on daily basis based on number of confirmed cases (obtained from https://github.com/covid-19-Re/dailyRe-Data)
* `germany_date_only.csv` date of sampling of all sequences contained in identical sequence clusters used for the analysis

##### processed
* `data_cases_sequences_clusters_de_2021_months.csv` overview of number of confirmed cases, number of sequences sampled, number of clusters and size of largest cluster per month in 2021
* `data_cluster_sizes_de_2021_months.csv` number of clusters of each size in each month of 2021
* `data_clusters_de_processed.csv` distribution of size of identical sequence clusters, clusters without a valid smapling date eliminated 
* `data_new_confirmed_cases_de_processed.csv` number of new confirmed cases, filtered to 2021
* `data_r_e_de_processed.csv` estimate of effective reproduction number on daily basis based on number of confirmed cases, filtered to 2021
* `sequencing_probas_de_2021_months.csv` probability of a confirmed case being sequenced on monthly basis during 2021

#### D.2.d all countries

##### covariants

###### raw
* `data_variants_shares_ch_dk_de_raw.csv` shares of SARS-CoV-2 variants (alpha, delta, omicron and other) among sequences on bi-weekly interval during 2021 (obtained from https://covariants.org/per-country?country=Germany&country=Denmark&country=Switzerland)

###### processed
* `data_variants_shares_ch_dk_de_processed.csv` shares of SARS-CoV-2 variants (alpha, delta, omicron and other) among sequences on bi-weekly interval during 2021 and auxiliary variables needed for plotting

##### posterior predictive check
* `data_parameters_ppc_model_one.csv` all parameter combinations for which identical sequence clusters were simulated during the posterior predictive check
* `index_parameters_ppc_model_one.txt` auxiliary file needed during the posterior predictive check for the parallel execution of cluster simulation

#### D.2.e simulation
* `parameters_grid_simulation.csv` all parameter combinations for which identical sequence clusters were simulated during the simulation study
* `indices_simulation.txt` and `indices_estimation.txt` auxiliary files needed during the simulation study for the parallel execution of cluster simulation, respectively parameter estimation
* `simulated_clusters` number of simulated clusters of each size for each combination of parameters contained in `parameters_grid_simulation.csv`


### D.3 results


### D.4 plots


### D.5 supplementary_material











