
# Project: R and k

# Preprocess cluster data from Switzerland, Denmark and Germany



# read cluster data ----

# read cluster data from Switzerland
data_clusters_0_ch <- read_delim(file = path_data_clusters_ch_raw, delim = "\t")

# read cluster data from Denmark
data_clusters_0_dk <- read_delim(file = path_data_clusters_dk_raw, delim = "\t")

# read cluster data from Germany
data_clusters_0_de <- read_delim(file = path_data_clusters_de_raw, delim = "\t")



# process cluster data ----

# `data_clusters_0_ch:`
# (a) filter to clusters who have at least one valid date
# (b) filter to clusters with finite value in column `counts`
data_clusters_ch <- data_clusters_0_ch |> filter(!(invalidDates == counts) & is.finite(counts))

# `data_clusters_0_dk:`
# (a) filter to clusters who have at least one valid date
# (b) filter to clusters with finite value in column `counts`
data_clusters_dk <- data_clusters_0_dk |> filter(!(invalidDates == counts) & is.finite(counts))

# `data_clusters_0_de:`
# (a) filter to clusters who have at least one valid date
# (b) filter to clusters with finite value in column `counts`
data_clusters_de <- data_clusters_0_de |> filter(!(invalidDates == counts) & is.finite(counts))



# have a look at the impact of the filtering 
overview <- data.frame(country = c("Switzerland", "Denmark", "Germany"),
                       n_clusters_0 = c(nrow(data_clusters_0_ch), nrow(data_clusters_0_dk), nrow(data_clusters_0_de)),
                       n_sequences_0 = c(sum(data_clusters_0_ch$counts), sum(data_clusters_0_dk$counts), sum(data_clusters_0_de$counts)),
                       n_clusters = c(nrow(data_clusters_ch), nrow(data_clusters_dk), nrow(data_clusters_de)),
                       n_sequences = c(sum(data_clusters_ch$counts), sum(data_clusters_dk$counts), sum(data_clusters_de$counts))) %>%
  mutate(difference_clusters = n_clusters_0 - n_clusters,
         difference_sequences = n_sequences_0 - n_sequences)

print(overview)



# write processed cluster data to csv files ----

# write processed cluster data from Switzerland to a csv file
write_csv(x = data_clusters_ch, file = path_data_clusters_ch_processed)

# write processed cluster data from Denmark to a csv file
write_csv(x = data_clusters_dk, file = path_data_clusters_dk_processed)

# write processed cluster data from Germany to a csv file
write_csv(x = data_clusters_de, file = path_data_clusters_de_processed)

