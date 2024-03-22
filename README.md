# Estimating $R_0$ and overdispersion in secondary cases from the sequence cluster size distribution of SARS-CoV-2
This repository contains the code of the statistical analysis of the paper "Estimating $R_0$ and overdispersion in secondary cases from the sequence cluster size distribution of SARS-CoV-2" by Emma Hodcroft et al. (ADD LINK TO PAPER).

## Overview of content of repository
The aim of this repository is to provide everything necessary to reproduce the statistical analysis of the paper cited above. All files (R-scripts as well as data) used to obtain the results are contained in this repository. Furthermore, the complete simulated data used for the validation of the model as well as all stanfit files containing the results of the parameter estimation can be found in this repository.

## How to run

## Further remarks

### R package estRodis

### Emma Hodcroft's sc2_k repository

## Detailed information on content of repository

### data
The repository contains the following data files (see folder `data`):

#### Switzerland

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

#### Denmark

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

#### Germany

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

#### all countries

##### raw
* `data_variants_shares_ch_dk_de_raw.csv` shares of SARS-CoV-2 variants (alpha, delta, omicron and other) among sequences on bi-weekly interval during 2021 (obtained from https://covariants.org/per-country?country=Germany&country=Denmark&country=Switzerland)

##### processed
* `data_variants_shares_ch_dk_de_processed.csv` shares of SARS-CoV-2 variants (alpha, delta, omicron and other) among sequences on bi-weekly interval during 2021 and auxiliary variables needed for plotting

#### simulation
* `parameters_grid_simulation.csv` all parameter combinations for which identical sequence clusters were simulated for the simulation study
* `indices_simulation.txt` and `indices_estimation.txt` auxiliary files needed  for the parallel execution of cluster simulation, respectively parameter estimation
* `simulated_clusters` number of simulated clusters of each size for each combination of parameters contained in `parameters_grid_simulation.csv`

### R


### results


### plots


### supplementary_material











