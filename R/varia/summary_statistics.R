
# read cluster data ----

# read cluster data from Switzerland
data_clusters_ch <- read_delim(file = path_data_clusters_ch_processed, delim = ",")

# read cluster data from Denmark
data_clusters_dk <- read_delim(file = path_data_clusters_dk_processed, delim = ",")

# read cluster data from Germany
data_clusters_de <- read_delim(file = path_data_clusters_de_processed, delim = ",")



# determine number of clusters in 2021 ----
clusters_ch_2021 <- data_clusters_ch %>% filter(!((maxDate < ymd("2021-01-01")) | (minDate >= ymd("2022-01-01"))))
clusters_dk_2021 <- data_clusters_dk %>% filter(!((maxDate < ymd("2021-01-01")) | (minDate >= ymd("2022-01-01"))))
clusters_de_2021 <- data_clusters_de %>% filter(!((maxDate < ymd("2021-01-01")) | (minDate >= ymd("2022-01-01"))))

average_cluster_size_ch_2021 <- sum(clusters_ch_2021$counts) / nrow(clusters_ch_2021)
average_cluster_size_dk_2021 <- sum(clusters_dk_2021$counts) / nrow(clusters_dk_2021)
average_cluster_size_de_2021 <- sum(clusters_de_2021$counts) / nrow(clusters_de_2021)

fraction_clusters_size_one_ch <- sum(clusters_ch_2021$counts == 1) / nrow(clusters_ch_2021)
fraction_clusters_size_one_dk <- sum(clusters_dk_2021$counts == 1) / nrow(clusters_dk_2021)
fraction_clusters_size_one_de <- sum(clusters_de_2021$counts == 1) / nrow(clusters_de_2021)


# determine number of clusters as well as average cluster size in Switzerland, Denmark and Germany in 2021
summary_clusters <- tibble(country = c("Switzerland", "Denmark", "Germany"),
                           n_clusters = c(nrow(clusters_ch_2021), nrow(clusters_dk_2021), nrow(clusters_de_2021)),
                           n_cases_in_clusters = c(sum(clusters_ch_2021$counts), sum(clusters_dk_2021$counts), sum(clusters_de_2021$counts)),
                           fraction_clusters_size_one = c(fraction_clusters_size_one_ch, fraction_clusters_size_one_dk, fraction_clusters_size_one_de)) %>%
  mutate(average_cluster_size = n_cases_in_clusters / n_clusters)
