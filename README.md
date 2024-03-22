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
* `data_new_confirmed_cases_ch_raw.csv` number of new confirmed cases (obtained from https://github.com/covid-19-Re/dailyRe-Data)
* `data_r_e_ch_raw.csv` estimate of effective reproduction number on daily basis based on number of confirmed cases (obtained from https://www.covid19.admin.ch/en/overview)
* `switzerland_date_only.csv` date of sampling of all sequences contained in identical sequence clusters used for the analysis

##### processed
* `data_cases_sequences_clusters_ch_2021_months.csv`
   overview of number of confirmed cases, number of sequences sampled, number of clusters and size of largest cluster per month
* `data_cluster_sizes_ch_2021_months.csv`
   number of clusters of each size in each month of 2021
* `data_clusters_ch_processed.csv` processed 
* `data_new_confirmed_cases_ch_processed.csv`
* `data_r_e_ch_processed.csv`
* `sequencing_probas_ch_2021_months.csv`

#### Denmark

##### raw

##### processed

#### Germany

##### raw

##### processed


### R


### results


### plots


### supplementary_material











