
# create data overview plots for Switzerland, Denmark and Germany individually


# read and process data ----

# read variant data
data_variants_ch_dk_de <- read_csv(file = path_data_variants_shares_ch_dk_de_processed)


# read summary tables of the number of confirmed cases, the number of sequences and the number of clusters 
data_cases_sequences_clusters_ch_2021_months <- read_csv(file = path_data_cases_sequences_clusters_ch_2021_months)
data_cases_sequences_clusters_dk_2021_months <- read_csv(file = path_data_cases_sequences_clusters_dk_2021_months)
data_cases_sequences_clusters_de_2021_months <- read_csv(file = path_data_cases_sequences_clusters_de_2021_months)

# bind `data_cases_sequences_clusters_ch_2021_months`, `data_cases_sequences_clusters_dk_2021_months` and `data_cases_sequences_clusters_de_2021_months`
data_cases_clusters_ch_dk_de <- bind_rows(data_cases_sequences_clusters_ch_2021_months %>% filter(Month != "Total") %>% rename("month_name" = "Month") %>% mutate(month = 1:12, country = "Switzerland"), 
                                          data_cases_sequences_clusters_dk_2021_months %>% filter(Month != "Total") %>% rename("month_name" = "Month") %>% mutate(month = 1:12, country = "Denmark"), 
                                          data_cases_sequences_clusters_de_2021_months %>% filter(Month != "Total") %>% rename("month_name" = "Month") %>% mutate(month = 1:12, country = "Germany"))

data_results <- data_cases_clusters_ch_dk_de %>% mutate(model = "A", offset = 0.0)

color_scale_values <- c("black")
color_scale_labels <- c("Model 1")



# create plots ----
ratio_width_height <- 4/3
image_width_in <- 7.3
list_months <- c("Jan", "Feb", "Mar", "Apr", "May", "Jun", "Jul", "Aug", "Sep", "Oct", "Nov", "Dec")
colors_variants <- paletteer_c("viridis::plasma", n = 4)
list_countries <- c("Switzerland", "Denmark", "Germany")



for (ii in 1:length(list_countries)) {
  
  country_upper_case_ii <- list_countries[ii]
  country_lower_case_ii <- tolower(list_countries[ii])
  
  data_results_country <- data_results %>% filter(country == list_countries[ii])
  data_variants_ch_dk_de_country <- data_variants_ch_dk_de %>% filter(country == list_countries[ii])
  
  
  # size of largest cluster ----
  plot_title <- paste0(country_upper_case_ii, ": Size of largest cluster during 2021")
  
  
  # without variants
  plot_results_size_largest_cluster_country_ii <- create_plot_result(data_plot_results = data_results_country,
                                                                     data_estimate = data_results_country$Solc,
                                                                     data_lower_cred_int = data_results_country$Solc,
                                                                     data_upper_cred_int = data_results_country$Solc,
                                                                     plot_color_scale_values = color_scale_values, 
                                                                     plot_color_scale_labels = color_scale_labels,
                                                                     scale_y_from = 0,
                                                                     scale_y_to = 1500,
                                                                     scale_y_by = 150,
                                                                     add_line_at_one = FALSE,
                                                                     label_y = "Size of largest cluster")
  
  # ggsave(filename = paste0("plots/", country_lower_case_ii, "/data/plot_results_size_largest_cluster.png"),
  #        plot = plot_results_size_largest_cluster_country_ii, width = image_width_in, height = image_width_in / ratio_width_height, units = "in")
  
  
  plot_results_size_largest_cluster_country_ii_with_title <- plot_results_size_largest_cluster_country_ii +
    ggtitle(plot_title)
  
  ggsave(filename = paste0("plots/", country_lower_case_ii, "/data/plot_results_size_largest_cluster_with_title.png"),
         plot = plot_results_size_largest_cluster_country_ii_with_title, width = image_width_in, height = image_width_in / ratio_width_height, units = "in")
  
  
  # with variants
  plot_results_size_largest_cluster_variants_country_ii <- create_plot_result_variants(data_plot_results = data_results_country,
                                                                                       data_plot_variants = data_variants_ch_dk_de_country,
                                                                                       data_estimate = data_results_country$Solc,
                                                                                       data_lower_cred_int = data_results_country$Solc,
                                                                                       data_upper_cred_int = data_results_country$Solc,
                                                                                       plot_color_scale_values = color_scale_values, 
                                                                                       plot_color_scale_labels = color_scale_labels,
                                                                                       scale_y_from = 0,
                                                                                       scale_y_to = 1500,
                                                                                       scale_y_by = 150,
                                                                                       add_line_at_one = FALSE,
                                                                                       label_y = "Size of largest cluster")
  
  # ggsave(filename = paste0("plots/", country_lower_case_ii, "/data/plot_results_size_largest_cluster_variants.png"),
  #        plot = plot_results_size_largest_cluster_variants_country_ii, width = image_width_in, height = image_width_in / ratio_width_height, units = "in")
  
  
  plot_results_size_largest_cluster_variants_country_ii_with_title <- plot_results_size_largest_cluster_variants_country_ii +
    ggtitle(plot_title)
  
  ggsave(filename = paste0("plots/", country_lower_case_ii, "/data/plot_results_size_largest_cluster_variants_with_title.png"),
         plot = plot_results_size_largest_cluster_variants_country_ii_with_title, width = image_width_in, height = image_width_in / ratio_width_height, units = "in")
  
  
  
  # number of clusters ----
  plot_title <- paste0(country_upper_case_ii, ": Number of clusters during 2021")
  
  
  # without variants
  plot_results_n_clusters_country_ii <- create_plot_result(data_plot_results = data_results_country,
                                                           data_estimate = data_results_country$Noc,
                                                           data_lower_cred_int = data_results_country$Noc,
                                                           data_upper_cred_int = data_results_country$Noc,
                                                           plot_color_scale_values = color_scale_values, 
                                                           plot_color_scale_labels = color_scale_labels,
                                                           scale_y_from = 0,
                                                           scale_y_to = 40000,
                                                           scale_y_by = 2000,
                                                           add_line_at_one = FALSE,
                                                           label_y = "Number of clusters")
  
  # ggsave(filename = paste0("plots/", country_lower_case_ii, "/data/plot_results_n_clusters.png"),
  #        plot = plot_results_n_clusters_country_ii, width = image_width_in, height = image_width_in / ratio_width_height, units = "in")
  
  
  plot_results_n_clusters_country_ii_with_title <- plot_results_n_clusters_country_ii +
    ggtitle(plot_title)
  
  ggsave(filename = paste0("plots/", country_lower_case_ii, "/data/plot_results_n_clusters_with_title.png"),
         plot = plot_results_n_clusters_country_ii_with_title, width = image_width_in, height = image_width_in / ratio_width_height, units = "in")
  
  
  # with variants
  plot_results_n_clusters_variants_country_ii <- create_plot_result_variants(data_plot_results = data_results_country,
                                                                             data_plot_variants = data_variants_ch_dk_de_country,
                                                                             data_estimate = data_results_country$Noc,
                                                                             data_lower_cred_int = data_results_country$Noc,
                                                                             data_upper_cred_int = data_results_country$Noc,
                                                                             plot_color_scale_values = color_scale_values, 
                                                                             plot_color_scale_labels = color_scale_labels,
                                                                             scale_y_from = 0,
                                                                             scale_y_to = 40000,
                                                                             scale_y_by = 2000,
                                                                             add_line_at_one = FALSE,
                                                                             label_y = "Number of clusters")
  
  # ggsave(filename = paste0("plots/", country_lower_case_ii, "/data/plot_results_n_clusters_variants.png"),
  #        plot = plot_results_n_clusters_variants_country_ii, width = image_width_in, height = image_width_in / ratio_width_height, units = "in")
  
  
  plot_results_n_clusters_variants_country_ii_with_title <- plot_results_n_clusters_variants_country_ii +
    ggtitle(plot_title)
  
  ggsave(filename = paste0("plots/", country_lower_case_ii, "/data/plot_results_n_clusters_variants_with_title.png"),
         plot = plot_results_n_clusters_variants_country_ii_with_title, width = image_width_in, height = image_width_in / ratio_width_height, units = "in")
  
  
  
  # sequencing probability ----
  plot_title <- paste0(country_upper_case_ii, ": Sequencing probability during 2021")
  
  
  # without variants
  plot_results_sequencing_probability_country_ii <- create_plot_result(data_plot_results = data_results_country,
                                                                       data_estimate = data_results_country$`Nos/Nocc`,
                                                                       data_lower_cred_int = data_results_country$`Nos/Nocc`,
                                                                       data_upper_cred_int = data_results_country$`Nos/Nocc`,
                                                                       plot_color_scale_values = color_scale_values, 
                                                                       plot_color_scale_labels = color_scale_labels,
                                                                       scale_y_from = 0,
                                                                       scale_y_to = 1,
                                                                       scale_y_by = 0.1,
                                                                       add_line_at_one = FALSE,
                                                                       label_y = "Sequencing probability")
  
  # ggsave(filename = paste0("plots/", country_lower_case_ii, "/data/plot_results_sequencing_probability.png"),
  #        plot = plot_results_sequencing_probability_country_ii, width = image_width_in, height = image_width_in / ratio_width_height, units = "in")
  
  
  plot_results_sequencing_probability_country_ii_with_title <- plot_results_sequencing_probability_country_ii +
    ggtitle(plot_title)
  
  ggsave(filename = paste0("plots/", country_lower_case_ii, "/data/plot_results_sequencing_probability_with_title.png"),
         plot = plot_results_sequencing_probability_country_ii_with_title, width = image_width_in, height = image_width_in / ratio_width_height, units = "in")
  
  
  # with variants
  plot_results_sequencing_probability_variants_country_ii <- create_plot_result_variants(data_plot_results = data_results_country,
                                                                                         data_plot_variants = data_variants_ch_dk_de_country,
                                                                                         data_estimate = data_results_country$`Nos/Nocc`,
                                                                                         data_lower_cred_int = data_results_country$`Nos/Nocc`,
                                                                                         data_upper_cred_int = data_results_country$`Nos/Nocc`,
                                                                                         plot_color_scale_values = color_scale_values, 
                                                                                         plot_color_scale_labels = color_scale_labels,
                                                                                         scale_y_from = 0,
                                                                                         scale_y_to = 1,
                                                                                         scale_y_by = 0.1,
                                                                                         add_line_at_one = FALSE,
                                                                                         label_y = "Sequencing probability")
  
  # ggsave(filename = paste0("plots/", country_lower_case_ii, "/data/plot_results_sequencing_probability_variants.png"),
  #        plot = plot_results_sequencing_probability_variants_country_ii, width = image_width_in, height = image_width_in / ratio_width_height, units = "in")
  
  
  plot_results_sequencing_probability_variants_country_ii_with_title <- plot_results_sequencing_probability_variants_country_ii +
    ggtitle(plot_title)
  
  ggsave(filename = paste0("plots/", country_lower_case_ii, "/data/plot_results_sequencing_probability_variants_with_title.png"),
         plot = plot_results_sequencing_probability_variants_country_ii_with_title, width = image_width_in, height = image_width_in / ratio_width_height, units = "in")
  
}

