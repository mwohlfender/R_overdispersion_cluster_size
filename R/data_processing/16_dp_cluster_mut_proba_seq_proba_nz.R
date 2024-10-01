


# read cluster data from New Zealand ----
if (!(file.exists(path_data_clusters_nz_raw))) {
  
  data_nz_0 <- readRDS(gzcon(url("https://github.com/blab/size-genetic-clusters/raw/7e5a333e0b1d74993b33b0e540eb511ad1a8aa78/data/ncov_NZ/df_cluster_by_period_NZ.rds")))
  
  saveRDS(object = data_nz_0, file = path_data_clusters_nz_raw)
  
}



# read mutation probabilities of SARS-CoV-2 ----
if (!(file.exists(path_mutation_probas_diseases))) {
  
  mutation_probas_diseases <- readRDS(gzcon(url("https://github.com/blab/size-genetic-clusters/raw/7e5a333e0b1d74993b33b0e540eb511ad1a8aa78/results/proba_trans_before_mut/df_p_trans_before_mut_with_uncertainty.rds")))
  
  saveRDS(object = mutation_probas_diseases, file = path_mutation_probas_diseases)
  
  data_mut_proba_sarscov2_pre_omicron <- mutation_probas_diseases %>% 
    filter(pathogen %in% c("SARS-CoV-2 (pre-Omicron)"))
  
  saveRDS(object = data_mut_proba_sarscov2_pre_omicron, file = path_mutation_probas_sarscov2_pre_omicron)
  
  data_mut_proba_sarscov2_omicron <- mutation_probas_diseases %>% 
    filter(pathogen %in% c("SARS-CoV-2 (Omicron)")) 
  
  saveRDS(object = data_mut_proba_sarscov2_omicron, file = path_mutation_probas_sarscov2_omicron)
  
}



# read sequencing probabilities from New Zealand ----
if (!(file.exists(path_sequencing_probas_nz_periods))) {
  
  sequencing_probas_nz_periods_0 <- readRDS(gzcon(url("https://github.com/blab/size-genetic-clusters/raw/7e5a333e0b1d74993b33b0e540eb511ad1a8aa78/data/ncov_NZ/df_prop_sequenced_per_period.rds")))
  
  saveRDS(object = sequencing_probas_nz_periods_0, file = path_sequencing_probas_nz_periods)
  
}


