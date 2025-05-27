# Estimating $R_e$ and overdispersion in secondary cases from the size of identical sequence clusters of SARS-CoV-2
This repository contains the code of the statistical analysis of the paper "Estimating $R_e$ and overdispersion in secondary cases from the size of identical sequence clusters of SARS-CoV-2" by Emma Hodcroft et al. published in [PLOS Computational Biology](https://journals.plos.org/ploscompbiol/article?id=10.1371/journal.pcbi.1012960).

## (A) Overview of content of repository
The aim of this repository is to provide everything necessary to reproduce the statistical analysis of the paper cited above. All files (R-scripts as well as data) used to obtain the results are contained in this repository. Furthermore, the complete simulated data used for the validation of the model as well as all stanfit files containing the results of the parameter estimation can be found in this repository.

## (B) How to run
* The whole R code is structured in an R-project (`R_overdispersion_cluster_size.Rproj`).
* Before running any other R file, the file `setup.R` (contained in folder `R`) needs to be run. In this file, all paths to data and results files are defined (with respect to the path of `R_overdispersion_cluster_size.Rproj`).
* R files are grouped by topic (data processing, creating plots, ...).
* Running the file `main.R` (contained in folder `R`) calls all R scripts necessary to redo the processing of data and results as well as the creation of plots and tables. Data and result files need to be stored at the paths defined in `setup.R`.
* Parameter estimation, both from simulated data and from data from Switzerland, Denmark and Germany, has been run on the high performance computing cluster of the University of Bern, UBELIX.
* Simulation of clusters for the simulation study and for the posterior predictive check have been run on the high performance computing cluster of the University of Bern, UBELIX.

## (C) Further remarks

### C.1 R package estRodis
The simulation of identical sequence clusters as well as the models to estimate parameters from the sequence cluster size distribution are implemented as functions in an R-package, called estRodis.
The estRodis package can be found here: [GitHub Martin Wohlfender: estRodis](https://github.com/mwohlfender/estRodis)

### C.2 Emma Hodcroft's sc2_k repository
The structuring of sequence data into clusters of identical sequences was done by Emma Hodcroft. Her code can be found here: [GitHub Emma Hodcroft: sc2_rk_public](https://github.com/emmahodcroft/sc2_rk_public)

### C.3 Name convention
Whenever "model five" is mentioned in comments in the code, this refers to the standard model presented in the paper. "model one", "model two", "model three", "model four" and "model six" refer to the alternative models described in the section "Sensitivity analysis" of the supplementary material of the paper "Estimating $R_e$ and overdispersion in secondary cases from the size of identical sequence clusters of SARS-CoV-2" by Emma Hodcroft et al.

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

(a) model one
* `01_sim_setup_model_one.R` setting up the simulation study: define parameter combinations for which clusters of identical sequences shall be simulated
* `02_sim_simulate_data_model_one_parallel.R` simulation of identical sequence clusters (run in parallel on the high performance computing cluster of the University of Bern, UBELIX)
* `03_sim_estimate_parameters_model_one_parallel.R` estimation of parameters from simulated data using model one (run in parallel on the high performance computing cluster of the University of Bern, UBELIX)

(b) model five
* `01_sim_setup_model_five.R` setting up the simulation study: define parameter combinations for which clusters of identical sequences shall be simulated
* `02_sim_simulate_data_model_five_parallel.R` simulation of identical sequence clusters (run in parallel on the high performance computing cluster of the University of Bern, UBELIX)
* `03_sim_estimate_parameters_model_five_parallel.R` estimation of parameters from simulated data using model five (run in parallel on the high performance computing cluster of the University of Bern, UBELIX)

#### D.1.f parameter estimation
All R-scripts to estimate parameters from data from Switzerland, Denmark and Germany using the main (model five) or the alternative models (models one, two, three four and six). These files were run in parallel on the high performance computing cluster of the University of Bern, UBELIX.

#### D.1.g posterior predictive check

(a) model one
* `01_ppc_model_one_setup.R` setting up the posterior predictive check: define parameter combinations for which clusters of identical sequences shall be simulated
* `02_ppc_model_one_simulations_parallel.R` simulation of identical sequence clusters (run in parallel on the high performance computing cluster of the University of Bern, UBELIX)

(b) model five
* `01_ppc_model_five_setup.R` setting up the posterior predictive check: define parameter combinations for which clusters of identical sequences shall be simulated
* `02_ppc_model_five_simulations_parallel.R` simulation of identical sequence clusters (run in parallel on the high performance computing cluster of the University of Bern, UBELIX)

#### D.1.h create plots
All R-scripts covering the creation of figures (contained in paper and supplementary material).

#### D.1.i create tables
All R-scripts covering the creation of overview tables of data and results (contained in supplementary material).

#### D.1.j varia
R-scripts used to do some minor extra analysis.

* `clusters_months.R` check how many clusters extend across more than one month.
* `summary_statistics.R` basic description of cluster data set (number of identical sequence clusters, number of cases in identical sequence clusters, fraction of identical sequence clusters that are of size one and average cluster size)

### D.2 data
The repository contains the following data files (see folder `data`):

#### D.2.a Switzerland

##### raw
* `Switzerland_cluster_distribution_dates_100whole.tsv` distribution of size of identical sequence clusters (obtained from [GitHub Emma Hodcroft: sc2_rk_public](https://github.com/emmahodcroft/sc2_rk_public))
* `data_new_confirmed_cases_ch_raw.csv` number of new confirmed cases  (obtained from [COVID-⁠19 Switzerland](https://www.covid19.admin.ch/en/overview))
* `data_r_e_ch_raw.csv` estimate of effective reproduction number on daily basis based on number of confirmed cases (obtained from [GitHub covid-19-Re: dailyRe-Data](https://github.com/covid-19-Re/dailyRe-Data))
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
* `Denmark_cluster_distribution_dates_100whole.tsv` distribution of size of identical sequence clusters (obtained from [GitHub Emma Hodcroft: sc2_rk_public](https://github.com/emmahodcroft/sc2_rk_public))
* `data_new_confirmed_cases_dk_raw.csv` number of new confirmed cases (obtained from [Statens Serum Institut](https://experience.arcgis.com/experience/220fef27d07d438889d651cc2e00076c/page/Covid-19-Regionalt/))
* `data_r_e_dk_raw.csv` estimate of effective reproduction number on daily basis based on number of confirmed cases (obtained from [GitHub covid-19-Re: dailyRe-Data](https://github.com/covid-19-Re/dailyRe-Data))
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
* `Germany_cluster_distribution_dates_100whole.tsv` distribution of size of identical sequence clusters (obtained from [GitHub Emma Hodcroft: sc2_rk_public](https://github.com/emmahodcroft/sc2_rk_public))
* `data_new_confirmed_cases_de_raw.csv` number of new confirmed cases (obtained from [GitHub Robert Koch Institut: COVID-19_7-Tage-Inzidenz_in_Deutschland](https://github.com/robert-koch-institut/COVID-19_7-Tage-Inzidenz_in_Deutschland))
* `data_r_e_de_raw.csv` estimate of effective reproduction number on daily basis based on number of confirmed cases (obtained from [GitHub covid-19-Re: dailyRe-Data](https://github.com/covid-19-Re/dailyRe-Data))
* `germany_date_only.csv` date of sampling of all sequences contained in identical sequence clusters used for the analysis

##### processed
* `data_cases_sequences_clusters_de_2021_months.csv` overview of number of confirmed cases, number of sequences sampled, number of clusters and size of largest cluster per month in 2021
* `data_cluster_sizes_de_2021_months.csv` number of clusters of each size in each month of 2021
* `data_clusters_de_processed.csv` distribution of size of identical sequence clusters, clusters without a valid smapling date eliminated 
* `data_new_confirmed_cases_de_processed.csv` number of new confirmed cases, filtered to 2021
* `data_r_e_de_processed.csv` estimate of effective reproduction number on daily basis based on number of confirmed cases, filtered to 2021
* `sequencing_probas_de_2021_months.csv` probability of a confirmed case being sequenced on monthly basis during 2021

#### D.2.d New Zealand

##### raw
* `df_cluster_by_period_NZ.rds` number of clusters of each size in different periods (April 2020 - July 2021) (obtained from [GitHub CecileTK: size-genetic-clusters](https://github.com/blab/size-genetic-clusters/tree/main/data/ncov_NZ))
* `df_p_trans_before_mut_with_uncertainty.rds` probability of mutation before transmission for different pathogens (obtained from [GitHub CecileTK: size-genetic-clusters](https://github.com/blab/size-genetic-clusters/tree/main/results/proba_trans_before_mut))
* `df_prop_sequenced_per_period.rds` probability of a confirmed case being sequenced for each period (obtained from [GitHub CecileTK: size-genetic-clusters](https://github.com/blab/size-genetic-clusters/tree/main/data/ncov_NZ))

##### processed
* `mutation_probas_sarscov2_omicron.rds` probability of mutation before transmission for Omicron variant of SARS-CoV-2
* `mutation_probas_sarscov2_pre_omicron.rds` probability of mutation before transmission before Omicron variant of SARS-CoV-2

#### D.2.e multiple countries

##### covariants

###### raw
* `data_variants_shares_ch_dk_de_raw.csv` shares of SARS-CoV-2 variants (alpha, delta, omicron and other) among sequences on bi-weekly interval during 2021 (obtained from [CoVariants](https://covariants.org/per-country?country=Germany&country=Denmark&country=Switzerland))

###### processed
* `data_variants_shares_ch_dk_de_processed.csv` shares of SARS-CoV-2 variants (alpha, delta, omicron and other) among sequences on bi-weekly interval during 2021 and auxiliary variables needed for plotting

##### posterior predictive check

(a) model one
* `data_parameters_ppc_model_one.csv` all parameter combinations for which identical sequence clusters were simulated during the posterior predictive check
* `index_parameters_ppc_model_one.txt` auxiliary file needed during the posterior predictive check for the parallel execution of cluster simulation

(b) model five
* `data_parameters_ppc_model_five.csv` all parameter combinations for which identical sequence clusters were simulated during the posterior predictive check
* `index_parameters_ppc_model_five.txt` auxiliary file needed during the posterior predictive check for the parallel execution of cluster simulation

#### D.2.f simulation

(a) model one
* `parameters_grid_simulation_model_one.csv` all parameter combinations for which identical sequence clusters were simulated during the simulation study
* `indices_simulation_model_one.txt` and `indices_estimation_model_one.txt` auxiliary files needed during the simulation study for the parallel execution of cluster simulation, respectively parameter estimation
* `simulated_clusters` simulated data based on parameters defined in `parameters_grid_simulation_model_one.csv`

(b) model five
* `parameters_grid_simulation_model_five.csv` all parameter combinations for which identical sequence clusters were simulated during the simulation study
* `indices_simulation_model_five.txt` and `indices_estimation_model_five.txt` auxiliary files needed during the simulation study for the parallel execution of cluster simulation, respectively parameter estimation
* `simulated_clusters` simulated data based on parameters defined in `parameters_grid_simulation_model_five.csv`

### D.3 results

#### D.3.a Switzerland
stanfit files containing the results of the parameter estimation from data of Switzerland using models one, two, three, four, five and six

#### D.3.b Denmark
stanfit files containing the results of the parameter estimation from data of Denmark using models one, two, three, four, five and six

#### D.3.c Germany
stanfit files containing the results of the parameter estimation from data of Germany using models one, two, three, four, five and six

#### D.3.d multiple countries

##### parameter estimations
* `results_model_one_ch_dk_de_2021_months.csv` summary of the results of the parameter estimation from data of Switzerland, Denmark and Germany using model one
* `results_model_two_ch_dk_de_2021_months.csv` summary of the results of the parameter estimation from data of Switzerland, Denmark and Germany using model two
* `results_model_three_ch_dk_de_2021_months.csv` summary of the results of the parameter estimation from data of Switzerland, Denmark and Germany using model three
* `results_model_four_ch_dk_de_2021_months.csv` summary of the results of the parameter estimation from data of Switzerland, Denmark and Germany using model four
* `results_model_five_ch_dk_de_2021_months.csv` summary of the results of the parameter estimation from data of Switzerland, Denmark and Germany using model five
* `results_model_six_ch_dk_de_2021_months.csv` summary of the results of the parameter estimation from data of Switzerland, Denmark and Germany using model six

##### posterior predictive check
* `results_ppc_model_one_ch_dk_de.csv` summary of the results of the posterior predictive check of the results of the parameter estimation from data of Switzerland, Denmark and Germany using model one
* `results_ppc_model_five_ch_dk_de.csv` summary of the results of the posterior predictive check of the results of the parameter estimation from data of Switzerland, Denmark and Germany using model five

##### goodness fit

(a) model one
* `results_goodness_fit_mean_model_one_ch_dk_de.csv` cluster size distribution parametrized by mean estimates of parameters
* `results_goodness_fit_low_model_one_ch_dk_de.csv` cluster size distribution parametrized by 2.5\% and 97.5\% quantile estimates of parameters (lower limit)
* `results_goodness_fit_high_model_one_ch_dk_de.csv` cluster size distribution parametrized by 2.5\% and 97.5\% quantile estimates of parameters (upper limit)

(b) model five
* `results_goodness_fit_mean_model_five_ch_dk_de.csv` cluster size distribution parametrized by mean estimates of parameters
* `results_goodness_fit_low_model_five_ch_dk_de.csv` cluster size distribution parametrized by 2.5\% and 97.5\% quantile estimates of parameters (lower limit)
* `results_goodness_fit_high_model_five_ch_dk_de.csv` cluster size distribution parametrized by 2.5\% and 97.5\% quantile estimates of parameters (upper limit)

#### D.3.e simulation

(a) model one

##### raw
stanfit files containing the results of the parameter estimation from simulated data using model one (see `data/simulation/01_model_one/simulated_clusters`)

##### processed
* `results_sim_model_one_processed.csv` summary of the results of the parameter estimation from simulated data using model one

(b) model five

##### raw
stanfit files containing the results of the parameter estimation from simulated data using model five (see `data/simulation/02_model_five/simulated_clusters`)

##### processed
* `results_sim_model_five_processed.csv` summary of the results of the parameter estimation from simulated data using model five

### D.4 plots
Graphical and tabular representations of data, model and results of simulation study, parameter estimation, posterior predictive check and goodness of fit check both for Switzerland, Denmark and Germany individually and for all three countries together and parameter estimation for New Zealand.


