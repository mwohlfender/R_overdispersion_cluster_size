

# define paths ----

# define names of tables
name_table_cases_sequences_clusters_ch_2021_months <- "table_cases_sequences_clusters_ch_2021_months"
name_table_cases_sequences_clusters_dk_2021_months <- "table_cases_sequences_clusters_dk_2021_months"
name_table_cases_sequences_clusters_de_2021_months <- "table_cases_sequences_clusters_de_2021_months"

name_table_cluster_sizes_intervals_ch_2021_months <- "table_cluster_sizes_intervals_ch_2021_months"
name_table_cluster_sizes_intervals_dk_2021_months <- "table_cluster_sizes_intervals_dk_2021_months"
name_table_cluster_sizes_intervals_de_2021_months <- "table_cluster_sizes_intervals_de_2021_months"



# read data ----

# number of confirmed cases and number of sequences per country and month
data_cases_sequences_clusters_ch_2021_months_0 <- read_csv(file = path_data_cases_sequences_clusters_ch_2021_months)
data_cases_sequences_clusters_dk_2021_months_0 <- read_csv(file = path_data_cases_sequences_clusters_dk_2021_months)
data_cases_sequences_clusters_de_2021_months_0 <- read_csv(file = path_data_cases_sequences_clusters_de_2021_months)

# read number of clusters of each size per country and month
data_cluster_sizes_ch_2021_months <- read_csv(file = path_data_cluster_sizes_ch_2021_months)
data_cluster_sizes_dk_2021_months <- read_csv(file = path_data_cluster_sizes_dk_2021_months)
data_cluster_sizes_de_2021_months <- read_csv(file = path_data_cluster_sizes_de_2021_months)



# process data ----

# round elements of the columns "Nos/Nocc"
data_cases_sequences_clusters_ch_2021_months <- data_cases_sequences_clusters_ch_2021_months_0 %>% mutate(`Nos/Nocc` = round(`Nos/Nocc`, 4))
data_cases_sequences_clusters_dk_2021_months <- data_cases_sequences_clusters_dk_2021_months_0 %>% mutate(`Nos/Nocc` = round(`Nos/Nocc`, 4))
data_cases_sequences_clusters_de_2021_months <- data_cases_sequences_clusters_de_2021_months_0 %>% mutate(`Nos/Nocc` = round(`Nos/Nocc`, 4))



# define parameters ----

# list of months
list_months <- c("January", "February", "March", "April", "May", "June", "July", "August", "September", "October", "November", "December")



# create tables ----
set_flextable_defaults(background.color = "white")

# Switzerland

# overview table of cases, sequences and clusters of Switzerland by month of 2021 
table_cases_sequences_clusters_ch_2021_months <- flextable(data_cases_sequences_clusters_ch_2021_months)
table_cases_sequences_clusters_ch_2021_months <- theme_vanilla(table_cases_sequences_clusters_ch_2021_months)
table_cases_sequences_clusters_ch_2021_months <- width(table_cases_sequences_clusters_ch_2021_months, j = 1:7, width = dim_pretty(table_cases_sequences_clusters_ch_2021_months)$widths * 6.3 / sum(dim_pretty(table_cases_sequences_clusters_ch_2021_months)$widths), unit = "in")

flextable::save_as_image(x = table_cases_sequences_clusters_ch_2021_months, path = paste0("plots/switzerland/tables/", name_table_cases_sequences_clusters_ch_2021_months, ".png"))


# table of cluster sizes of Switzerland by month of 2021 (absolute numbers)
data_cluster_sizes_intervals_ch_2021_months <- data_cluster_sizes_ch_2021_months %>% mutate(size_interval = factor(cut(x = size, breaks = c(0,1,2,3,4,5,10,50,100,max(size)), labels = c("1", "2", "3", "4", "5", "6-10", "11-50", "51-100", paste0("101-", max(size)))))) %>% group_by(size_interval, month) %>% summarise(n = sum(frequency)) %>% spread(month, n) %>% {\(.) {replace(.,is.na(.), 0)}}()
data_cluster_sizes_intervals_ch_2021_months <- as_tibble(t(data_cluster_sizes_intervals_ch_2021_months[,-1]), .name_repair = c( "universal")) %>% rename_all(~as.character(data_cluster_sizes_intervals_ch_2021_months$size_interval)) %>% mutate_at(1:9, as.numeric)
data_cluster_sizes_intervals_ch_2021_months <- data_cluster_sizes_intervals_ch_2021_months %>% mutate(All = rowSums(data_cluster_sizes_intervals_ch_2021_months), Month = list_months) %>% select(Month, everything())
data_cluster_sizes_intervals_ch_2021_months <- data_cluster_sizes_intervals_ch_2021_months %>% bind_rows(tibble(data.frame(matrix(data = c("Total", colSums(data_cluster_sizes_intervals_ch_2021_months[,-1])), nrow = 1, ncol = 11))) %>% mutate_at(2:11, as.numeric) %>% rename_all(~names(data_cluster_sizes_intervals_ch_2021_months)))

table_cluster_sizes_intervals_ch_2021_months <- flextable(data_cluster_sizes_intervals_ch_2021_months)
table_cluster_sizes_intervals_ch_2021_months <- theme_vanilla(table_cluster_sizes_intervals_ch_2021_months)
table_cluster_sizes_intervals_ch_2021_months <- add_header_row(table_cluster_sizes_intervals_ch_2021_months,
                                                               colwidths = c(1,10),
                                                               values = c("", "Cluster size"))
table_cluster_sizes_intervals_ch_2021_months <- align(table_cluster_sizes_intervals_ch_2021_months, j = 1, align = "left", part = "all")
table_cluster_sizes_intervals_ch_2021_months <- align(table_cluster_sizes_intervals_ch_2021_months, j = 2:11, align = "right", part = "all")
table_cluster_sizes_intervals_ch_2021_months <- align(table_cluster_sizes_intervals_ch_2021_months, i = 1, j = 1:11, align = "center", part = "header")
table_cluster_sizes_intervals_ch_2021_months <- width(table_cluster_sizes_intervals_ch_2021_months, j = 1:11,
                                                      width = dim_pretty(table_cluster_sizes_intervals_ch_2021_months)$widths, unit = "in")

flextable::save_as_image(x = table_cluster_sizes_intervals_ch_2021_months, path = paste0("plots/switzerland/tables/", name_table_cluster_sizes_intervals_ch_2021_months, ".png"))


# table of cluster sizes of Switzerland by month of 2021 (percentage)
data_cluster_sizes_intervals_ch_2021_months_percentage <- data_cluster_sizes_intervals_ch_2021_months %>% mutate_at(2:11, ~ ./All) %>% mutate_at(2:11, ~round(.,4)) %>% mutate_at(2:10, ~format(., nsmall = 4, scientific = FALSE))

table_cluster_sizes_intervals_ch_2021_months_percentage <- flextable(data_cluster_sizes_intervals_ch_2021_months_percentage)
table_cluster_sizes_intervals_ch_2021_months_percentage <- theme_vanilla(table_cluster_sizes_intervals_ch_2021_months_percentage)
table_cluster_sizes_intervals_ch_2021_months_percentage <- add_header_row(table_cluster_sizes_intervals_ch_2021_months_percentage,
                                                                          colwidths = c(1,10),
                                                                          values = c("", "Cluster size"))
table_cluster_sizes_intervals_ch_2021_months_percentage <- align(table_cluster_sizes_intervals_ch_2021_months_percentage, j = 1, align = "left", part = "all")
table_cluster_sizes_intervals_ch_2021_months_percentage <- align(table_cluster_sizes_intervals_ch_2021_months_percentage, j = 2:11, align = "right", part = "all")
table_cluster_sizes_intervals_ch_2021_months_percentage <- align(table_cluster_sizes_intervals_ch_2021_months_percentage, i = 1, j = 1:11, align = "center", part = "header")
table_cluster_sizes_intervals_ch_2021_months_percentage <- width(table_cluster_sizes_intervals_ch_2021_months_percentage, j = 1:11,
                                                                 width = dim_pretty(table_cluster_sizes_intervals_ch_2021_months_percentage)$widths, unit = "in")

flextable::save_as_image(x = table_cluster_sizes_intervals_ch_2021_months_percentage, path = paste0("plots/switzerland/tables/", name_table_cluster_sizes_intervals_ch_2021_months, "_percentage.png"))


# Denmark

# overview table of cases, sequences and clusters of Denmark by month of 2021 
table_cases_sequences_clusters_dk_2021_months <- flextable(data_cases_sequences_clusters_dk_2021_months)
table_cases_sequences_clusters_dk_2021_months <- theme_vanilla(table_cases_sequences_clusters_dk_2021_months)
table_cases_sequences_clusters_dk_2021_months <- width(table_cases_sequences_clusters_dk_2021_months, j = 1:7, width = dim_pretty(table_cases_sequences_clusters_dk_2021_months)$widths * 6.3 / sum(dim_pretty(table_cases_sequences_clusters_dk_2021_months)$widths), unit = "in")

flextable::save_as_image(x = table_cases_sequences_clusters_dk_2021_months, path = paste0("plots/denmark/tables/", name_table_cases_sequences_clusters_dk_2021_months, ".png"))


# table of cluster sizes of Denmark by month of 2021 (absolute numbers)
data_cluster_sizes_intervals_dk_2021_months <- data_cluster_sizes_dk_2021_months %>% mutate(size_interval = factor(cut(x = size, breaks = c(0,1,2,3,4,5,10,50,100,max(size)), labels = c("1", "2", "3", "4", "5", "6-10", "11-50", "51-100", paste0("101-", max(size)))))) %>% group_by(size_interval, month) %>% summarise(n = sum(frequency)) %>% spread(month, n) %>% {\(.) {replace(.,is.na(.), 0)}}()
data_cluster_sizes_intervals_dk_2021_months <- as_tibble(t(data_cluster_sizes_intervals_dk_2021_months[,-1]), .name_repair = c( "universal")) %>% rename_all(~as.character(data_cluster_sizes_intervals_dk_2021_months$size_interval)) %>% mutate_at(1:9, as.numeric)
data_cluster_sizes_intervals_dk_2021_months <- data_cluster_sizes_intervals_dk_2021_months %>% mutate(All = rowSums(data_cluster_sizes_intervals_dk_2021_months), Month = list_months) %>% select(Month, everything())
data_cluster_sizes_intervals_dk_2021_months <- data_cluster_sizes_intervals_dk_2021_months %>% bind_rows(tibble(data.frame(matrix(data = c("Total", colSums(data_cluster_sizes_intervals_dk_2021_months[,-1])), nrow = 1, ncol = 11))) %>% mutate_at(2:11, as.numeric) %>% rename_all(~names(data_cluster_sizes_intervals_dk_2021_months)))

table_cluster_sizes_intervals_dk_2021_months <- flextable(data_cluster_sizes_intervals_dk_2021_months)
table_cluster_sizes_intervals_dk_2021_months <- theme_vanilla(table_cluster_sizes_intervals_dk_2021_months)
table_cluster_sizes_intervals_dk_2021_months <- add_header_row(table_cluster_sizes_intervals_dk_2021_months,
                                                               colwidths = c(1,10),
                                                               values = c("", "Cluster size"))
table_cluster_sizes_intervals_dk_2021_months <- align(table_cluster_sizes_intervals_dk_2021_months, j = 1, align = "left", part = "all")
table_cluster_sizes_intervals_dk_2021_months <- align(table_cluster_sizes_intervals_dk_2021_months, j = 2:11, align = "right", part = "all")
table_cluster_sizes_intervals_dk_2021_months <- align(table_cluster_sizes_intervals_dk_2021_months, i = 1, j = 1:11, align = "center", part = "header")
table_cluster_sizes_intervals_dk_2021_months <- width(table_cluster_sizes_intervals_dk_2021_months, j = 1:11,
                                                      width = dim_pretty(table_cluster_sizes_intervals_dk_2021_months)$widths, unit = "in")

flextable::save_as_image(x = table_cluster_sizes_intervals_dk_2021_months, path = paste0("plots/denmark/tables/", name_table_cluster_sizes_intervals_dk_2021_months, ".png"))


# table of cluster sizes of Denmark by month of 2021 (percentage)
data_cluster_sizes_intervals_dk_2021_months_percentage <- data_cluster_sizes_intervals_dk_2021_months %>% mutate_at(2:11, ~ ./All) %>% mutate_at(2:11, ~round(.,4)) %>% mutate_at(2:10, ~format(., nsmall = 4, scientific = FALSE))

table_cluster_sizes_intervals_dk_2021_months_percentage <- flextable(data_cluster_sizes_intervals_dk_2021_months_percentage)
table_cluster_sizes_intervals_dk_2021_months_percentage <- theme_vanilla(table_cluster_sizes_intervals_dk_2021_months_percentage)
table_cluster_sizes_intervals_dk_2021_months_percentage <- add_header_row(table_cluster_sizes_intervals_dk_2021_months_percentage,
                                                                          colwidths = c(1,10),
                                                                          values = c("", "Cluster size"))
table_cluster_sizes_intervals_dk_2021_months_percentage <- align(table_cluster_sizes_intervals_dk_2021_months_percentage, j = 1, align = "left", part = "all")
table_cluster_sizes_intervals_dk_2021_months_percentage <- align(table_cluster_sizes_intervals_dk_2021_months_percentage, j = 2:11, align = "right", part = "all")
table_cluster_sizes_intervals_dk_2021_months_percentage <- align(table_cluster_sizes_intervals_dk_2021_months_percentage, i = 1, j = 1:11, align = "center", part = "header")
table_cluster_sizes_intervals_dk_2021_months_percentage <- width(table_cluster_sizes_intervals_dk_2021_months_percentage, j = 1:11,
                                                                 width = dim_pretty(table_cluster_sizes_intervals_dk_2021_months_percentage)$widths, unit = "in")

flextable::save_as_image(x = table_cluster_sizes_intervals_dk_2021_months_percentage, path = paste0("plots/denmark/tables/", name_table_cluster_sizes_intervals_dk_2021_months, "_percentage.png"))



# Germany

# overview table of cases, sequences and clusters of Germany by month of 2021 
table_cases_sequences_clusters_de_2021_months <- flextable(data_cases_sequences_clusters_de_2021_months)
table_cases_sequences_clusters_de_2021_months <- theme_vanilla(table_cases_sequences_clusters_de_2021_months)
table_cases_sequences_clusters_de_2021_months <- width(table_cases_sequences_clusters_de_2021_months, j = 1:7, width = dim_pretty(table_cases_sequences_clusters_de_2021_months)$widths * 6.3 / sum(dim_pretty(table_cases_sequences_clusters_de_2021_months)$widths), unit = "in")

flextable::save_as_image(x = table_cases_sequences_clusters_de_2021_months, path = paste0("plots/germany/tables/", name_table_cases_sequences_clusters_de_2021_months, ".png"))


# table of cluster sizes of Germany by month of 2021 (absolute numbers)
data_cluster_sizes_intervals_de_2021_months <- data_cluster_sizes_de_2021_months %>% mutate(size_interval = factor(cut(x = size, breaks = c(0,1,2,3,4,5,10,50,100,max(size)), labels = c("1", "2", "3", "4", "5", "6-10", "11-50", "51-100", paste0("101-", max(size)))))) %>% group_by(size_interval, month) %>% summarise(n = sum(frequency)) %>% spread(month, n) %>% {\(.) {replace(.,is.na(.), 0)}}()
data_cluster_sizes_intervals_de_2021_months <- as_tibble(t(data_cluster_sizes_intervals_de_2021_months[,-1]), .name_repair = c( "universal")) %>% rename_all(~as.character(data_cluster_sizes_intervals_de_2021_months$size_interval)) %>% mutate_at(1:9, as.numeric)
data_cluster_sizes_intervals_de_2021_months <- data_cluster_sizes_intervals_de_2021_months %>% mutate(All = rowSums(data_cluster_sizes_intervals_de_2021_months), Month = list_months) %>% select(Month, everything())
data_cluster_sizes_intervals_de_2021_months <- data_cluster_sizes_intervals_de_2021_months %>% bind_rows(tibble(data.frame(matrix(data = c("Total", colSums(data_cluster_sizes_intervals_de_2021_months[,-1])), nrow = 1, ncol = 11))) %>% mutate_at(2:11, as.numeric) %>% rename_all(~names(data_cluster_sizes_intervals_de_2021_months)))

table_cluster_sizes_intervals_de_2021_months <- flextable(data_cluster_sizes_intervals_de_2021_months)
table_cluster_sizes_intervals_de_2021_months <- theme_vanilla(table_cluster_sizes_intervals_de_2021_months)
table_cluster_sizes_intervals_de_2021_months <- add_header_row(table_cluster_sizes_intervals_de_2021_months,
                                                               colwidths = c(1,10),
                                                               values = c("", "Cluster size"))
table_cluster_sizes_intervals_de_2021_months <- align(table_cluster_sizes_intervals_de_2021_months, j = 1, align = "left", part = "all")
table_cluster_sizes_intervals_de_2021_months <- align(table_cluster_sizes_intervals_de_2021_months, j = 2:11, align = "right", part = "all")
table_cluster_sizes_intervals_de_2021_months <- align(table_cluster_sizes_intervals_de_2021_months, i = 1, j = 1:11, align = "center", part = "header")
table_cluster_sizes_intervals_de_2021_months <- width(table_cluster_sizes_intervals_de_2021_months, j = 1:11,
                                                      width = dim_pretty(table_cluster_sizes_intervals_de_2021_months)$widths, unit = "in")

flextable::save_as_image(x = table_cluster_sizes_intervals_de_2021_months, path = paste0("plots/germany/tables/", name_table_cluster_sizes_intervals_de_2021_months, ".png"))


# table of cluster sizes of Germany by month of 2021 (percentage)
data_cluster_sizes_intervals_de_2021_months_percentage <- data_cluster_sizes_intervals_de_2021_months %>% mutate_at(2:11, ~ ./All) %>% mutate_at(2:11, ~round(.,4)) %>% mutate_at(2:10, ~format(., nsmall = 4, scientific = FALSE))

table_cluster_sizes_intervals_de_2021_months_percentage <- flextable(data_cluster_sizes_intervals_de_2021_months_percentage)
table_cluster_sizes_intervals_de_2021_months_percentage <- theme_vanilla(table_cluster_sizes_intervals_de_2021_months_percentage)
table_cluster_sizes_intervals_de_2021_months_percentage <- add_header_row(table_cluster_sizes_intervals_de_2021_months_percentage,
                                                                          colwidths = c(1,10),
                                                                          values = c("", "Cluster size"))
table_cluster_sizes_intervals_de_2021_months_percentage <- align(table_cluster_sizes_intervals_de_2021_months_percentage, j = 1, align = "left", part = "all")
table_cluster_sizes_intervals_de_2021_months_percentage <- align(table_cluster_sizes_intervals_de_2021_months_percentage, j = 2:11, align = "right", part = "all")
table_cluster_sizes_intervals_de_2021_months_percentage <- align(table_cluster_sizes_intervals_de_2021_months_percentage, i = 1, j = 1:11, align = "center", part = "header")
table_cluster_sizes_intervals_de_2021_months_percentage <- width(table_cluster_sizes_intervals_de_2021_months_percentage, j = 1:11,
                                                                 width = dim_pretty(table_cluster_sizes_intervals_de_2021_months_percentage)$widths, unit = "in")

flextable::save_as_image(x = table_cluster_sizes_intervals_de_2021_months_percentage, path = paste0("plots/germany/tables/", name_table_cluster_sizes_intervals_de_2021_months, "_percentage.png"))


