
# In this script we check how many clusters extend across more than one month.


# read cluster data from Switzerland, Denmark and Germany
data_clusters_ch <- read_delim(file = path_data_clusters_ch_processed, delim = ",")
data_clusters_dk <- read_delim(file = path_data_clusters_dk_processed, delim = ",")
data_clusters_de <- read_delim(file = path_data_clusters_de_processed, delim = ",")


# filter cluster data from Switzerland, Denmark and Germany to clusters that contain at least one case sampled in 2021
# and determine across how many months the clusters have spread and how many of these months are in 2021
data_clusters_ch_2021 <- data_clusters_ch %>%
  filter(!(maxDate < ymd("2021-01-01") | minDate >= ymd("2022-01-01"))) %>%
  mutate(minDate_month = 12*(year(minDate)-2021) + month(minDate),
         maxDate_month = 12*(year(maxDate)-2021) + month(maxDate)) %>%
  mutate(n_months_spread = maxDate_month - minDate_month + 1) %>%
  rowwise() %>%
  mutate(n_months_spread_2021 = min(maxDate_month, 12) - max(minDate_month, 1) + 1)

data_clusters_dk_2021 <- data_clusters_dk %>%
  filter(!(maxDate < ymd("2021-01-01") | minDate >= ymd("2022-01-01"))) %>%
  mutate(minDate_month = 12*(year(minDate)-2021) + month(minDate),
         maxDate_month = 12*(year(maxDate)-2021) + month(maxDate)) %>%
  mutate(n_months_spread = maxDate_month - minDate_month + 1) %>%
  rowwise() %>%
  mutate(n_months_spread_2021 = min(maxDate_month, 12) - max(minDate_month, 1) + 1)

data_clusters_de_2021 <- data_clusters_de %>%
  filter(!(maxDate < ymd("2021-01-01") | minDate >= ymd("2022-01-01"))) %>%
  mutate(minDate_month = 12*(year(minDate)-2021) + month(minDate),
         maxDate_month = 12*(year(maxDate)-2021) + month(maxDate)) %>%
  mutate(n_months_spread = maxDate_month - minDate_month + 1) %>%
  rowwise() %>%
  mutate(n_months_spread_2021 = min(maxDate_month, 12) - max(minDate_month, 1) + 1)


# process cluster data: 
# for each country and each observed cluster size determine
# (1) the percentage of clusters not extending beyond one month
# (2) mean, standard deviation and median of the number of months across which clusters extended
# (3) maximal and minimal number of months across which clusters extended
overview_clusters_size_accross_months_ch_2021 <- data_clusters_ch_2021 %>%
  group_by(counts) %>%
  summarize(perc_n_months_spread_equal_one = sum(n_months_spread == 1) / n(),
            mean_n_months_spread = mean(n_months_spread),
            sd_n_months_spread = sd(n_months_spread),
            median_n_months_spread = median(n_months_spread),
            min_n_months_spread = min(n_months_spread),
            max_n_months_spread = max(n_months_spread))

overview_clusters_size_accross_months_dk_2021 <- data_clusters_dk_2021 %>%
  group_by(counts) %>%
  summarize(perc_n_months_spread_equal_one = sum(n_months_spread == 1) / n(),
            mean_n_months_spread = mean(n_months_spread),
            sd_n_months_spread = sd(n_months_spread),
            median_n_months_spread = median(n_months_spread),
            min_n_months_spread = min(n_months_spread),
            max_n_months_spread = max(n_months_spread))

overview_clusters_size_accross_months_de_2021 <- data_clusters_de_2021 %>%
  group_by(counts) %>%
  summarize(perc_n_months_spread_equal_one = sum(n_months_spread == 1) / n(),
            mean_n_months_spread = mean(n_months_spread),
            sd_n_months_spread = sd(n_months_spread),
            median_n_months_spread = median(n_months_spread),
            min_n_months_spread = min(n_months_spread),
            max_n_months_spread = max(n_months_spread))

# summarize cluster data of all three countries:
# for each country determine
# (1) percentage of clusters of size one
# (2) percentage of clusters not extending beyond one month
# (3) percentage of clusters assigned to exactly one month
# (4) percentage of clusters of size at least two not extending beyond one month
# (5) mean and standard deviation of the number of months across which clusters extended
# (6) maximal number of months across which clusters extended
# (7) date last sequence was sampled
overview_clusters_accross_months_ch_dk_de_2021 <- tibble(country = c("Switzerland", "Denmark", "Germany"),
                                                         perc_cluster_size_one = c(sum(data_clusters_ch_2021$counts == 1) / nrow(data_clusters_ch_2021),
                                                                                   sum(data_clusters_dk_2021$counts == 1) / nrow(data_clusters_dk_2021),
                                                                                   sum(data_clusters_de_2021$counts == 1) / nrow(data_clusters_de_2021)),
                                                         perc_in_one_month = c(sum(data_clusters_ch_2021$n_months_spread == 1) / nrow(data_clusters_ch_2021),
                                                                               sum(data_clusters_dk_2021$n_months_spread == 1) / nrow(data_clusters_dk_2021),
                                                                               sum(data_clusters_de_2021$n_months_spread == 1) / nrow(data_clusters_de_2021)),
                                                         perc_in_one_month_2021 = c(sum(data_clusters_ch_2021$n_months_spread_2021 == 1) / nrow(data_clusters_ch_2021),
                                                                                    sum(data_clusters_dk_2021$n_months_spread_2021 == 1) / nrow(data_clusters_dk_2021),
                                                                                    sum(data_clusters_de_2021$n_months_spread_2021 == 1) / nrow(data_clusters_de_2021)),
                                                         perc_big_clusters_in_one_month = c(nrow(data_clusters_ch_2021 %>% filter(counts > 1 & n_months_spread == 1)) / nrow(data_clusters_ch_2021 %>% filter(counts > 1)),
                                                                                            nrow(data_clusters_dk_2021 %>% filter(counts > 1 & n_months_spread == 1)) / nrow(data_clusters_dk_2021 %>% filter(counts > 1)),
                                                                                            nrow(data_clusters_de_2021 %>% filter(counts > 1 & n_months_spread == 1)) / nrow(data_clusters_de_2021 %>% filter(counts > 1))),
                                                         mean_n_months_spread = c(mean(data_clusters_ch_2021$n_months_spread),
                                                                                  mean(data_clusters_dk_2021$n_months_spread),
                                                                                  mean(data_clusters_de_2021$n_months_spread)),
                                                         sd_n_months_spread = c(sd(data_clusters_ch_2021$n_months_spread),
                                                                                sd(data_clusters_dk_2021$n_months_spread),
                                                                                sd(data_clusters_de_2021$n_months_spread)),
                                                         max_n_months_spread = c(max(data_clusters_ch_2021$n_months_spread),
                                                                                 max(data_clusters_dk_2021$n_months_spread),
                                                                                 max(data_clusters_de_2021$n_months_spread)),
                                                         max_maxDate = c(max(data_clusters_ch_2021$maxDate),
                                                                         max(data_clusters_dk_2021$maxDate),
                                                                         max(data_clusters_de_2021$maxDate)))

