# Project: R and k

# Create summary of data:
# (a) number of confirmed cases
# (b) number of sequences
# (c) number of clusters
# (d) number of cases in clusters and 
# (e) size of largest cluster 
# per month in Denmark, Germany and Switzerland

# results are presented in tables in supplementary material



# define constants ----
list_months_2021_days <- c(31, 28, 31, 30, 31, 30, 31, 31, 30, 31, 30, 31)



# read cluster data ----

# read cluster data from Switzerland, Denmark and Germany
data_clusters_ch <- read_delim(file = path_data_clusters_ch_processed, delim = ",")
data_clusters_dk <- read_delim(file = path_data_clusters_dk_processed, delim = ",")
data_clusters_de <- read_delim(file = path_data_clusters_de_processed, delim = ",")

# filter cluster data from Switzerland, Denmark and Germany to clusters that contain at least one case sampled in 2021
data_clusters_ch_2021 <- data_clusters_ch %>%
  filter(!(maxDate < ymd("2021-01-01") | minDate >= ymd("2022-01-01")))

data_clusters_dk_2021 <- data_clusters_dk %>%
  filter(!(maxDate < ymd("2021-01-01") | minDate >= ymd("2022-01-01")))

data_clusters_de_2021 <- data_clusters_de %>%
  filter(!(maxDate < ymd("2021-01-01") | minDate >= ymd("2022-01-01")))


# read sampling dates of sequences contained clusters from Switzerland, Denmark and Germany that contain at least one case sampled in 2021
data_sequences_dates_ch_2021_0 <- read_delim(path_data_sequences_dates_ch_2021, delim = ",")
data_sequences_dates_dk_2021_0 <- read_delim(path_data_sequences_dates_dk_2021, delim = ",")
data_sequences_dates_de_2021_0 <- read_delim(path_data_sequences_dates_de_2021, delim = ",")

# check format of values in column `date` of `data_sequences_dates_ch_2021_0`, `data_sequences_dates_dk_2021_0` and `data_sequences_dates_de_2021_0`
data_sequences_dates_ch_2021_0_format <- as.data.frame(table(n_characters_date = nchar(as.character(data_sequences_dates_ch_2021_0$date))))
data_sequences_dates_dk_2021_0_format <- as.data.frame(table(n_characters_date = nchar(as.character(data_sequences_dates_dk_2021_0$date))))
data_sequences_dates_de_2021_0_format <- as.data.frame(table(n_characters_date = nchar(as.character(data_sequences_dates_de_2021_0$date))))

data_sequences_dates_ch_2021_0_YYYY <- as.data.frame(table(year = data_sequences_dates_ch_2021_0 %>% filter(nchar(as.character(date)) == 4) %>% pull(date)))
data_sequences_dates_dk_2021_0_YYYY <- as.data.frame(table(year = data_sequences_dates_dk_2021_0 %>% filter(nchar(as.character(date)) == 4) %>% pull(date)))
data_sequences_dates_de_2021_0_YYYY <- as.data.frame(table(year = data_sequences_dates_de_2021_0 %>% filter(nchar(as.character(date)) == 4) %>% pull(date)))

data_sequences_dates_ch_2021_0_YYYY_MM <- as.data.frame(table(month = data_sequences_dates_ch_2021_0 %>% filter(nchar(as.character(date)) == 7) %>% pull(date)))
data_sequences_dates_dk_2021_0_YYYY_MM <- as.data.frame(table(month = data_sequences_dates_dk_2021_0 %>% filter(nchar(as.character(date)) == 7) %>% pull(date)))
data_sequences_dates_de_2021_0_YYYY_MM <- as.data.frame(table(month = data_sequences_dates_de_2021_0 %>% filter(nchar(as.character(date)) == 7) %>% pull(date)))

# All values in column `date` of `data_sequences_dates_ch_2021_0`, `data_sequences_dates_dk_2021_0` and `data_sequences_dates_de_2021_0`
# are either in format YYYY, YYYY-MM or YYYY-MM-DD. As we would like to determine how many cases we sequenced in each month,
# we filter to dates in format YYYY-MM or YYYY-MM-DD.

data_sequences_dates_ch_2021 <- data_sequences_dates_ch_2021_0 %>%
  filter(nchar(as.character(date)) >= 7) %>%
  mutate(month = paste0(substring(text = date, first = 1, last = 4), substring(text = date, first = 6, last = 7)))

data_sequences_dates_dk_2021 <- data_sequences_dates_dk_2021_0 %>%
  filter(nchar(as.character(date)) >= 7) %>%
  mutate(month = paste0(substring(text = date, first = 1, last = 4), substring(text = date, first = 6, last = 7)))

data_sequences_dates_de_2021 <- data_sequences_dates_de_2021_0  %>%
  filter(nchar(as.character(date)) >= 7) %>%
  mutate(month = paste0(substring(text = date, first = 1, last = 4), substring(text = date, first = 6, last = 7)))

# overview of the number of rows of `data_sequences_dates_ch_2021_0`, `data_sequences_dates_dk_2021_0` and `data_sequences_dates_de_2021_0`
# and `data_sequences_dates_ch_2021`, `data_sequences_dates_dk_2021` and `data_sequences_dates_de_2021`
overview_dates <- data.frame(country = c("Switzerland", "Denmark", "Germany"),
                             n_dates_0 = c(nrow(data_sequences_dates_ch_2021_0), nrow(data_sequences_dates_dk_2021_0), nrow(data_sequences_dates_de_2021_0)),
                             n_dates_valid = c(nrow(data_sequences_dates_ch_2021), nrow(data_sequences_dates_dk_2021), nrow(data_sequences_dates_de_2021)))

# determine the number of sequences sampled in each month of 2021 with a valid date for Switzerland, Denmark and Germany
data_sequences_dates_ch_2021_months <- tibble(as.data.frame(table(month = data_sequences_dates_ch_2021$month))) %>%
  mutate(month = as.numeric(as.character(month))) %>% 
  filter(month >= 202101 & month <= 202112)

data_sequences_dates_dk_2021_months <- tibble(as.data.frame(table(month = data_sequences_dates_dk_2021$month))) %>%
  mutate(month = as.numeric(as.character(month))) %>% 
  filter(month >= 202101 & month <= 202112) 

data_sequences_dates_de_2021_months <- tibble(as.data.frame(table(month = data_sequences_dates_de_2021$month))) %>%
  mutate(month = as.numeric(as.character(month))) %>% 
  filter(month >= 202101 & month <= 202112) 

data_sequences_ch_dk_de_2021_months <- tibble(month = 1:12,
                                              n_sequences_ch = data_sequences_dates_ch_2021_months$Freq,
                                              n_sequences_dk = data_sequences_dates_dk_2021_months$Freq,
                                              n_sequences_de = data_sequences_dates_de_2021_months$Freq)



# read and process data about shares of different virus variants ----
# obtained from CoVariants (https://covariants.org/per-country?country=Germany&country=Denmark&country=Switzerland)
if (!(file.exists(path_data_variants_shares_ch_dk_de_processed)) | do_new_load_data) {
  
  # read raw data about shares of different virus variants
  data_variants_shares_ch_dk_de_0 <- read_csv(file = path_data_variants_shares_ch_dk_de_raw)
  
  # `data_variants_shares_ch_dk_de_0`:
  # (a) format columns `start_time` and `end_time` as date
  # (b) add column `Others` (other variants than Alpha, Delta and Omicron)
  data_variants_shares_ch_dk_de <- data_variants_shares_ch_dk_de_0 %>%
    mutate(across(ends_with("time"), ~ ymd(.x)),
           Others = round(1 - Alpha - Delta - Omicron, 2))
  
  # `data_variants_shares_ch_dk_de`:
  # (a) add columns `months_start_time_float` and `months_end_time_float` 
  # (conversion of columns `start_time` and `end_time` to floating point values (number of months))
  data_variants_shares_ch_dk_de <- data_variants_shares_ch_dk_de %>%
    mutate(months_start_time_float = 12*(year(start_time) - 2021) + month(start_time) - 1 + (day(start_time) - 1) / list_months_2021_days[month(start_time)],
           months_end_time_float = 12*(year(end_time) - 2021) + month(end_time) - 1 + (day(end_time) - 1) / list_months_2021_days[month(end_time)])
  
  # write `data_variants_shares_ch_dk_de` to a csv file
  write_csv(data_variants_shares_ch_dk_de,
            file = path_data_variants_shares_ch_dk_de_processed)
  
}

data_variants_shares_ch_dk_de <- read_csv(file = path_data_variants_shares_ch_dk_de_processed)



# read new confirmed cases data ----

# read new confirmed cases data from Switzerland

if (!(file.exists(path_data_new_confirmed_cases_ch_processed)) | do_new_load_data) {
  
  # read new confirmed cases data from Switzerland (https://www.covid19.admin.ch/en/overview)
  bag <- httr::GET("https://www.covid19.admin.ch/api/data/context")
  bag <- rjson::fromJSON(rawToChar(bag$content))
  data_new_confirmed_cases_ch_0 <- read_csv(bag$sources$individual$csv$daily$cases)
  
  # write `data_new_confirmed_cases_ch_0` to a csv file
  write_csv(x = data_new_confirmed_cases_ch_0, file = path_data_new_confirmed_cases_ch_raw)
  
  # process new confirmed cases data from Switzerland
  # filter to data of the whole of Switzerland of the year 2021 and group it by month
  data_new_confirmed_cases_ch <- data_new_confirmed_cases_ch_0 %>%
    filter(geoRegion == "CH" & datum >= ymd("2021-01-01") & datum <= ymd("2021-12-31")) %>%
    dplyr::select(c("datum", "entries")) %>%
    rename(date = datum, new_cases = entries) %>%
    group_by(month = month(date)) %>%
    summarize(new_cases = sum(new_cases))
  
  # write `data_new_confirmed_cases_ch` to a csv file
  write_csv(x = data_new_confirmed_cases_ch, file = path_data_new_confirmed_cases_ch_processed)
  
}

data_new_confirmed_cases_ch <- read_csv(path_data_new_confirmed_cases_ch_processed)


# read new confirmed cases data from Denmark

if (!(file.exists(path_data_new_confirmed_cases_dk_processed)) | do_new_load_data) {
  
  # read new confirmed cases data from Denmark (https://experience.arcgis.com/experience/220fef27d07d438889d651cc2e00076c/page/Covid-19-Regionalt/)
  url <- "https://sseruminstitut.maps.arcgis.com/sharing/rest/content/items/67a8aedc7ff9466e8920951540c2097f/data"
  
  destination <- "data/denmark/raw/data_covid_ssi.zip"
  
  download.file(url, destination, mode = "wb") 
  
  unzip(zipfile = "data/denmark/raw/data_covid_ssi.zip",
        exdir = "data/denmark/raw/",
        files = "03_bekraeftede_tilfaelde_doede_indlagte_pr_dag_pr_koen.csv")
  
  file.rename(from = "data/denmark/raw/03_bekraeftede_tilfaelde_doede_indlagte_pr_dag_pr_koen.csv",
              to = path_data_new_confirmed_cases_dk_raw)
  
  file.remove("data/denmark/raw/data_covid_ssi.zip")
  
  # process new confirmed cases data from Denmark
  # filter to data of the whole of Switzerland of the year 2021 and group it by month
  data_new_confirmed_cases_dk <- read_delim(file = path_data_new_confirmed_cases_dk_raw,
                                            delim = ";",
                                            locale = locale(encoding = "latin1")) %>%
    rename(c("region_code" = "Regionskode", "region_name" = "Region", "date" = "Prøvetagningsdato", "sex" = "Køn",
             "cases" = "Bekræftede tilfælde i alt", "deaths" = "Døde", "hospital_stays" = "Indlæggelser",
             "deaths_cumulative" = "Kummuleret antal døde", "cases_cumulative" = "Kummuleret antal bekræftede tilfælde",
             "hospital_stays_cumulative" = "Kummuleret antal indlæggelser", "timestamp" = "Timestamp")) %>%
    group_by(date) %>%
    summarize(new_cases = sum(cases)) %>%
    filter(year(date) == 2021) %>% 
    group_by(month = month(date)) %>%
    summarize(new_cases = sum(new_cases))
  
  # write `data_new_confirmed_cases_dk` to a csv file
  write_csv(x = data_new_confirmed_cases_dk, file = path_data_new_confirmed_cases_dk_processed)
  
}

data_new_confirmed_cases_dk <- read_csv(path_data_new_confirmed_cases_dk_processed)


# read new confirmed cases data from Germany

if (!(file.exists(path_data_new_confirmed_cases_de_processed)) | do_new_load_data) {
  
  # read new confirmed cases data from Germany (https://robert-koch-institut.github.io/COVID-19_7-Tage-Inzidenz_in_Deutschland/)
  data_new_confirmed_cases_de_0 <- read_csv("https://raw.githubusercontent.com/robert-koch-institut/COVID-19_7-Tage-Inzidenz_in_Deutschland/main/COVID-19-Faelle_7-Tage-Inzidenz_Deutschland.csv")
  
  # write data_new_confirmed_cases_de_0 to a csv file
  write_csv(x = data_new_confirmed_cases_de_0, file = path_data_new_confirmed_cases_de_raw)
  
  # process new confirmed cases data from Germany
  # filter to data of age group "00+" of the year 2021 and group it by month
  data_new_confirmed_cases_de <- data_new_confirmed_cases_de_0 %>%
    filter(Altersgruppe == "00+" & Meldedatum >= ymd("2021-01-01") & Meldedatum <= ymd("2021-12-31")) %>%
    dplyr::select(c("Meldedatum", "Faelle_neu")) %>%
    rename("date" = "Meldedatum", "new_cases" = "Faelle_neu") %>%
    group_by(month = month(date)) %>%
    summarize(new_cases = sum(new_cases))
  
  # write data_new_confirmed_cases_de to a csv file
  write_csv(x = data_new_confirmed_cases_de, file = path_data_new_confirmed_cases_de_processed)
  
}

data_new_confirmed_cases_de <- read_csv(path_data_new_confirmed_cases_de_processed)



# create data summary tables of the number of confirmed cases and the number of sequences ----

# list of months
list_months <- c("January", "February", "March", "April", "May", "June", "July", "August", "September", "October", "November", "December")

# determine number of clusters, number of cases in clusters and maximal size of clusters of Switzerland for each month of 2021
data_cases_sequences_clusters_ch_2021_months <- data.frame(matrix(data = 0, nrow = 12, ncol = 7))
names(data_cases_sequences_clusters_ch_2021_months) <- c("month", "n_confirmed_cases", "n_sequences", "n_cases_in_clusters", "n_clusters", "seq_proba_n_sequences", "max_size_clusters")

data_cases_sequences_clusters_ch_2021_months$n_sequences <- data_sequences_ch_dk_de_2021_months$n_sequences_ch

# determine number of cluster of Switzerland of different sizes for each month of 2021
data_cluster_sizes_ch_2021_months <- data.frame(matrix(nrow = 0, ncol = 3)) 
names(data_cluster_sizes_ch_2021_months) <- c("size", "month", "frequency")

for (ii in 1:12) {
  
  t1 <- ymd("2021-01-01") + months(ii-1)
  t2 <- t1 + months(1)
  
  clusters <- data_clusters_ch %>% filter(!((maxDate < t1) | (minDate >= t2))) %>% dplyr::select(counts)
  
  data_cases_sequences_clusters_ch_2021_months$month[ii] <- list_months[ii]
  
  data_cases_sequences_clusters_ch_2021_months$n_confirmed_cases[ii] <- data_new_confirmed_cases_ch$new_cases[ii]
  
  data_cases_sequences_clusters_ch_2021_months$n_cases_in_clusters[ii] <- sum(clusters)
  
  data_cases_sequences_clusters_ch_2021_months$n_clusters[ii] <- nrow(clusters)
  
  data_cases_sequences_clusters_ch_2021_months$seq_proba_n_sequences[ii] <- data_cases_sequences_clusters_ch_2021_months$n_sequences[ii] / data_cases_sequences_clusters_ch_2021_months$n_confirmed_cases[ii]
  
  data_cases_sequences_clusters_ch_2021_months$max_size_clusters[ii] <- max(clusters)
  
  data_cluster_sizes_ch_2021_month_ii <- as.data.frame(table(clusters)) %>% mutate(counts = as.numeric(as.character(counts))) %>% rename("size" = "counts", "frequency" = "Freq")
  
  data_cluster_sizes_ch_2021_months <- bind_rows(data_cluster_sizes_ch_2021_months, data_cluster_sizes_ch_2021_month_ii %>% mutate(month = ii))
  
}

data_cases_sequences_clusters_ch_2021_months <- bind_rows(data_cases_sequences_clusters_ch_2021_months, data.frame(month = "Total", 
                                                                                                                   n_confirmed_cases = sum(data_cases_sequences_clusters_ch_2021_months$n_confirmed_cases),
                                                                                                                   n_sequences = sum(data_cases_sequences_clusters_ch_2021_months$n_sequences),
                                                                                                                   n_cases_in_clusters = sum(data_cases_sequences_clusters_ch_2021_months$n_cases_in_clusters),
                                                                                                                   n_clusters = sum(data_cases_sequences_clusters_ch_2021_months$n_clusters),
                                                                                                                   seq_proba_n_sequences = sum(data_cases_sequences_clusters_ch_2021_months$n_sequences) / sum(data_cases_sequences_clusters_ch_2021_months$n_confirmed_cases),
                                                                                                                   max_size_clusters = max(data_cases_sequences_clusters_ch_2021_months$max_size_clusters)))

names(data_cases_sequences_clusters_ch_2021_months) <- c("Month", "Nocc", "Nos", "Nosic", "Noc", "Nos/Nocc", "Solc")


# determine number of clusters, number of cases in clusters and maximal size of clusters of Denmark for each month of 2021
data_cases_sequences_clusters_dk_2021_months <- data.frame(matrix(data = 0, nrow = 12, ncol = 7))
names(data_cases_sequences_clusters_dk_2021_months) <- c("month", "n_confirmed_cases", "n_sequences", "n_cases_in_clusters", "n_clusters", "seq_proba_n_sequences", "max_size_clusters")

data_cases_sequences_clusters_dk_2021_months$n_sequences <- data_sequences_ch_dk_de_2021_months$n_sequences_dk

# determine number of cluster of Denmark of different sizes for each month of 2021
data_cluster_sizes_dk_2021_months <- data.frame(matrix(nrow = 0, ncol = 3)) 
names(data_cluster_sizes_dk_2021_months) <- c("size", "month", "frequency")

for (ii in 1:12) {
  
  t1 <- ymd("2021-01-01") + months(ii-1)
  t2 <- t1 + months(1)
  
  clusters <- data_clusters_dk %>% filter(!((maxDate < t1) | (minDate >= t2))) %>% dplyr::select(counts)
  
  data_cases_sequences_clusters_dk_2021_months$month[ii] <- list_months[ii]
  
  data_cases_sequences_clusters_dk_2021_months$n_confirmed_cases[ii] <- data_new_confirmed_cases_dk$new_cases[ii]
  
  data_cases_sequences_clusters_dk_2021_months$n_cases_in_clusters[ii] <- sum(clusters)
  
  data_cases_sequences_clusters_dk_2021_months$n_clusters[ii] <- nrow(clusters)
  
  data_cases_sequences_clusters_dk_2021_months$seq_proba_n_sequences[ii] <- data_cases_sequences_clusters_dk_2021_months$n_sequences[ii] / data_cases_sequences_clusters_dk_2021_months$n_confirmed_cases[ii]
  
  data_cases_sequences_clusters_dk_2021_months$max_size_clusters[ii] <- max(clusters)
  
  data_cluster_sizes_dk_2021_month_ii <- as.data.frame(table(clusters)) %>% mutate(counts = as.numeric(as.character(counts))) %>% rename("size" = "counts", "frequency" = "Freq")
  
  data_cluster_sizes_dk_2021_months <- bind_rows(data_cluster_sizes_dk_2021_months, data_cluster_sizes_dk_2021_month_ii %>% mutate(month = ii))
  
}

data_cases_sequences_clusters_dk_2021_months <- bind_rows(data_cases_sequences_clusters_dk_2021_months, data.frame(month = "Total", 
                                                                                                                   n_confirmed_cases = sum(data_cases_sequences_clusters_dk_2021_months$n_confirmed_cases),
                                                                                                                   n_sequences = sum(data_cases_sequences_clusters_dk_2021_months$n_sequences),
                                                                                                                   n_cases_in_clusters = sum(data_cases_sequences_clusters_dk_2021_months$n_cases_in_clusters),
                                                                                                                   n_clusters = sum(data_cases_sequences_clusters_dk_2021_months$n_clusters),
                                                                                                                   seq_proba_n_sequences = sum(data_cases_sequences_clusters_dk_2021_months$n_sequences) / sum(data_cases_sequences_clusters_dk_2021_months$n_confirmed_cases),
                                                                                                                   max_size_clusters = max(data_cases_sequences_clusters_dk_2021_months$max_size_clusters)))

names(data_cases_sequences_clusters_dk_2021_months) <- c("Month", "Nocc", "Nos", "Nosic", "Noc", "Nos/Nocc", "Solc")


# determine number of clusters, number of cases in clusters and maximal size of clusters of Germany for each month of 2021
data_cases_sequences_clusters_de_2021_months <- data.frame(matrix(data = 0, nrow = 12, ncol = 7))
names(data_cases_sequences_clusters_de_2021_months) <- c("month", "n_confirmed_cases", "n_sequences", "n_cases_in_clusters", "n_clusters", "seq_proba_n_sequences", "max_size_clusters")

data_cases_sequences_clusters_de_2021_months$n_sequences <- data_sequences_ch_dk_de_2021_months$n_sequences_de

# determine number of cluster of Germany of different sizes for each month of 2021
data_cluster_sizes_de_2021_months <- data.frame(matrix(nrow = 0, ncol = 3)) 
names(data_cluster_sizes_de_2021_months) <- c("size", "month", "frequency")

for (ii in 1:12) {
  
  t1 <- ymd("2021-01-01") + months(ii-1)
  t2 <- t1 + months(1)
  
  clusters <- data_clusters_de %>% filter(!((maxDate < t1) | (minDate >= t2))) %>% dplyr::select(counts)
  
  data_cases_sequences_clusters_de_2021_months$month[ii] <- list_months[ii]
  
  data_cases_sequences_clusters_de_2021_months$n_confirmed_cases[ii] <- data_new_confirmed_cases_de$new_cases[ii]
  
  data_cases_sequences_clusters_de_2021_months$n_cases_in_clusters[ii] <- sum(clusters)
  
  data_cases_sequences_clusters_de_2021_months$n_clusters[ii] <- nrow(clusters)
  
  data_cases_sequences_clusters_de_2021_months$seq_proba_n_sequences[ii] <- data_cases_sequences_clusters_de_2021_months$n_sequences[ii] / data_cases_sequences_clusters_de_2021_months$n_confirmed_cases[ii]
  
  data_cases_sequences_clusters_de_2021_months$max_size_clusters[ii] <- max(clusters)
  
  data_cluster_sizes_de_2021_month_ii <- as.data.frame(table(clusters)) %>% mutate(counts = as.numeric(as.character(counts))) %>% rename("size" = "counts", "frequency" = "Freq")
  
  data_cluster_sizes_de_2021_months <- bind_rows(data_cluster_sizes_de_2021_months, data_cluster_sizes_de_2021_month_ii %>% mutate(month = ii))
  
}

data_cases_sequences_clusters_de_2021_months <- bind_rows(data_cases_sequences_clusters_de_2021_months, data.frame(month = "Total", 
                                                                                                                   n_confirmed_cases = sum(data_cases_sequences_clusters_de_2021_months$n_confirmed_cases),
                                                                                                                   n_sequences = sum(data_cases_sequences_clusters_de_2021_months$n_sequences),
                                                                                                                   n_cases_in_clusters = sum(data_cases_sequences_clusters_de_2021_months$n_cases_in_clusters),
                                                                                                                   n_clusters = sum(data_cases_sequences_clusters_de_2021_months$n_clusters),
                                                                                                                   seq_proba_n_sequences = sum(data_cases_sequences_clusters_de_2021_months$n_sequences) / sum(data_cases_sequences_clusters_de_2021_months$n_confirmed_cases),
                                                                                                                   max_size_clusters = max(data_cases_sequences_clusters_de_2021_months$max_size_clusters)))

names(data_cases_sequences_clusters_de_2021_months) <- c("Month", "Nocc", "Nos", "Nosic", "Noc", "Nos/Nocc", "Solc")



# determine sequencing probabilities for Switzerland, Denmark and Germany ----
# These values will be used for the parameter estimation.
sequencing_probabilities_ch <- data.frame(seq_proba_ch = data_cases_sequences_clusters_ch_2021_months %>% filter(Month != "Total") %>% pull("Nos/Nocc"))
sequencing_probabilities_dk <- data.frame(seq_proba_dk = data_cases_sequences_clusters_dk_2021_months %>% filter(Month != "Total") %>% pull("Nos/Nocc"))
sequencing_probabilities_de <- data.frame(seq_proba_de = data_cases_sequences_clusters_de_2021_months %>% filter(Month != "Total") %>% pull("Nos/Nocc"))


# save data summary tables of the number of confirmed cases and the number of sequences as csv-files
write_csv(data_cases_sequences_clusters_ch_2021_months, file = path_data_cases_sequences_clusters_ch_2021_months)
write_csv(data_cases_sequences_clusters_dk_2021_months, file = path_data_cases_sequences_clusters_dk_2021_months)
write_csv(data_cases_sequences_clusters_de_2021_months, file = path_data_cases_sequences_clusters_de_2021_months)


write_csv(data_cluster_sizes_ch_2021_months, file = path_data_cluster_sizes_ch_2021_months)
write_csv(data_cluster_sizes_dk_2021_months, file = path_data_cluster_sizes_dk_2021_months)
write_csv(data_cluster_sizes_de_2021_months, file = path_data_cluster_sizes_de_2021_months)


# save the sequencing probabilities as csv-files
write_csv(sequencing_probabilities_ch, file = path_sequencing_probas_ch_2021_months)
write_csv(sequencing_probabilities_dk, file = path_sequencing_probas_dk_2021_months)
write_csv(sequencing_probabilities_de, file = path_sequencing_probas_de_2021_months)



# have a look at the number of sequences and clusters per year ----
clusters_ch_2021 <- data_clusters_ch %>% filter(!((maxDate < ymd("2021-01-01")) | (minDate >= ymd("2022-01-01")))) %>% dplyr::select(counts)
print(sprintf("Switzerland: %d sequences in %d clusters", sum(clusters_ch_2021$counts), nrow(clusters_ch_2021)))

clusters_dk_2021 <- data_clusters_dk %>% filter(!((maxDate < ymd("2021-01-01")) | (minDate >= ymd("2022-01-01")))) %>% dplyr::select(counts)
print(sprintf("Denmark: %d sequences in %d clusters", sum(clusters_dk_2021$counts), nrow(clusters_dk_2021)))

clusters_de_2021 <- data_clusters_de %>% filter(!((maxDate < ymd("2021-01-01")) | (minDate >= ymd("2022-01-01")))) %>% dplyr::select(counts)
print(sprintf("Germany: %d sequences in %d clusters", sum(clusters_de_2021$counts), nrow(clusters_de_2021)))
