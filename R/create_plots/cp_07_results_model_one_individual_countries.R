
# plot results of model one for individual countries


# read and process data ----

# read variant data
data_variants_ch_dk_de <- read_csv(file = path_data_variants_shares_ch_dk_de_processed)

# read estimates of R_e based on confirmed cases
data_r_e_ch <- read_csv(file = path_data_r_e_ch_processed)
data_r_e_dk <- read_csv(file = path_data_r_e_dk_processed)
data_r_e_de <- read_csv(file = path_data_r_e_de_processed)

data_r_e_ch_dk_de <- rbind(data_r_e_ch,
                           data_r_e_dk,
                           data_r_e_de)

# read results of parameter estimates
results_model_one_ch_dk_de_2021_months_0 <- read_csv(file = path_results_model_one_ch_dk_de_processed)

results_model_one_ch_dk_de_2021_months <- results_model_one_ch_dk_de_2021_months_0 %>% mutate(model = "A", offset = 0.0)


data_results <- results_model_one_ch_dk_de_2021_months

color_scale_values_1 <- c("black")
color_scale_labels_1 <- c("Estimate")

color_scale_values_2 <- c("black", "steelblue")
color_scale_labels_2 <- c("Estimate", "External estimate")



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
  data_r_e_ch_dk_de_country <- data_r_e_ch_dk_de %>% filter(country == list_countries[ii])
  
  
  
  # effective reproduction number ----
  plot_title <- bquote(.(country_upper_case_ii) ~ ": Estimations of the effective reproduction number" ~ R[e] ~ "during 2021")
  
  # without variants
  plot_results_model_one_R_e_country_ii_1 <- create_plot_result(data_plot_results = data_results_country,
                                                                data_estimate = data_results_country$R_estimate,
                                                                data_lower_cred_int = data_results_country$R_lower_cred_int,
                                                                data_upper_cred_int = data_results_country$R_upper_cred_int,
                                                                plot_color_scale_values = color_scale_values_1,
                                                                plot_color_scale_labels = color_scale_labels_1,
                                                                scale_y_from = 0,
                                                                scale_y_to = 1.5,
                                                                scale_y_by = 0.1,
                                                                add_line_at_one = TRUE,
                                                                label_y = expression(paste("Estimated", ~ R[e])))
  
  # ggsave(filename = paste0("plots/", country_lower_case_ii, "/model_one/plot_results_model_one_R_e_1.png"),
  #        plot = plot_results_model_one_R_e_country_ii_1, width = image_width_in, height = image_width_in / ratio_width_height, units = "in")
  
  
  plot_results_model_one_R_e_country_ii_with_title_1 <- plot_results_model_one_R_e_country_ii_1 +
    ggtitle(plot_title) +
    theme(plot.title = element_text(size = 12))
  
  ggsave(filename = paste0("plots/", country_lower_case_ii, "/model_one/plot_results_model_one_R_e_with_title_1.png"),
         plot = plot_results_model_one_R_e_country_ii_with_title_1, width = image_width_in, height = image_width_in / ratio_width_height, units = "in")
  
  
  plot_results_model_one_R_e_country_ii_2 <- create_plot_result(data_plot_results = data_results_country,
                                                                data_plot_other_estimates = data_r_e_ch_dk_de_country,
                                                                data_estimate = data_results_country$R_estimate,
                                                                data_lower_cred_int = data_results_country$R_lower_cred_int,
                                                                data_upper_cred_int = data_results_country$R_upper_cred_int,
                                                                plot_color_scale_values = color_scale_values_2,
                                                                plot_color_scale_labels = color_scale_labels_2,
                                                                scale_y_from = 0,
                                                                scale_y_to = 2.0,
                                                                scale_y_by = 0.2,
                                                                add_line_at_one = TRUE,
                                                                label_y = expression(paste("Estimated", ~ R[e])))
  
  # ggsave(filename = paste0("plots/", country_lower_case_ii, "/model_one/plot_results_model_one_R_e_2.png"),
  #        plot = plot_results_model_one_R_e_country_ii_2, width = image_width_in, height = image_width_in / ratio_width_height, units = "in")
  
  
  plot_results_model_one_R_e_country_ii_with_title_2 <- plot_results_model_one_R_e_country_ii_2 +
    ggtitle(plot_title) +
    theme(plot.title = element_text(size = 12))
  
  ggsave(filename = paste0("plots/", country_lower_case_ii, "/model_one/plot_results_model_one_R_e_with_title_2.png"),
         plot = plot_results_model_one_R_e_country_ii_with_title_2, width = image_width_in, height = image_width_in / ratio_width_height, units = "in")
  
  
  # with variants
  plot_results_model_one_R_e_variants_country_ii_1 <- create_plot_result_variants(data_plot_results = data_results_country,
                                                                                  data_plot_variants = data_variants_ch_dk_de_country,
                                                                                  data_estimate = data_results_country$R_estimate,
                                                                                  data_lower_cred_int = data_results_country$R_lower_cred_int,
                                                                                  data_upper_cred_int = data_results_country$R_upper_cred_int,
                                                                                  plot_color_scale_values = color_scale_values_1,
                                                                                  plot_color_scale_labels = color_scale_labels_1,
                                                                                  scale_y_from = 0,
                                                                                  scale_y_to = 1.5,
                                                                                  scale_y_by = 0.1,
                                                                                  add_line_at_one = TRUE,
                                                                                  label_y = expression(paste("Estimated", ~ R[e])))
  
  # ggsave(filename = paste0("plots/", country_lower_case_ii, "/model_one/plot_results_model_one_R_e_variants_1.png"),
  #        plot = plot_results_model_one_R_e_variants_country_ii_1, width = image_width_in, height = image_width_in / ratio_width_height, units = "in")
  
  plot_results_model_one_R_e_variants_country_ii_with_title_1 <- plot_results_model_one_R_e_variants_country_ii_1 +
    ggtitle(plot_title) +
    theme(plot.title = element_text(size = 12))
  
  ggsave(filename = paste0("plots/", country_lower_case_ii, "/model_one/plot_results_model_one_R_e_variants_with_title_1.png"),
         plot = plot_results_model_one_R_e_variants_country_ii_with_title_1, width = image_width_in, height = image_width_in / ratio_width_height, units = "in")
  
  
  plot_results_model_one_R_e_variants_country_ii_2 <- create_plot_result_variants(data_plot_results = data_results_country,
                                                                                  data_plot_variants = data_variants_ch_dk_de_country,
                                                                                  data_plot_other_estimates = data_r_e_ch_dk_de_country,
                                                                                  data_estimate = data_results_country$R_estimate,
                                                                                  data_lower_cred_int = data_results_country$R_lower_cred_int,
                                                                                  data_upper_cred_int = data_results_country$R_upper_cred_int,
                                                                                  plot_color_scale_values = color_scale_values_2,
                                                                                  plot_color_scale_labels = color_scale_labels_2,
                                                                                  scale_y_from = 0,
                                                                                  scale_y_to = 2.0,
                                                                                  scale_y_by = 0.2,
                                                                                  add_line_at_one = TRUE,
                                                                                  label_y = expression(paste("Estimated", ~ R[e])))
  
  # ggsave(filename = paste0("plots/", country_lower_case_ii, "/model_one/plot_results_model_one_R_e_variants_2.png"),
  #        plot = plot_results_model_one_R_e_variants_country_ii_2, width = image_width_in, height = image_width_in / ratio_width_height, units = "in")
  
  plot_results_model_one_R_e_variants_country_ii_with_title_2 <- plot_results_model_one_R_e_variants_country_ii_2 +
    ggtitle(plot_title) +
    theme(plot.title = element_text(size = 12))
  
  ggsave(filename = paste0("plots/", country_lower_case_ii, "/model_one/plot_results_model_one_R_e_variants_with_title_2.png"),
         plot = plot_results_model_one_R_e_variants_country_ii_with_title_2, width = image_width_in, height = image_width_in / ratio_width_height, units = "in")
  
  
  
  # genomic reproduction number ----
  plot_title <- bquote(.(country_upper_case_ii) ~ ": Estimations of the genomic reproduction number" ~ R[g] ~ "during 2021")
  
  # without variants
  plot_results_model_one_R_g_country_ii <- create_plot_result(data_plot_results = data_results_country,
                                                              data_estimate = data_results_country$R_estimate * (1 - data_results_country$mutation_proba_estimate),
                                                              data_lower_cred_int = data_results_country$R_estimate * (1 - data_results_country$mutation_proba_estimate),
                                                              data_upper_cred_int = data_results_country$R_estimate * (1 - data_results_country$mutation_proba_estimate),
                                                              plot_color_scale_values = color_scale_values_1,
                                                              plot_color_scale_labels = color_scale_labels_1,
                                                              scale_y_from = 0,
                                                              scale_y_to = 1.5,
                                                              scale_y_by = 0.1,
                                                              add_line_at_one = TRUE,
                                                              label_y = expression(paste("Estimated", ~ R[g])))
  
  # ggsave(filename = paste0("plots/", country_lower_case_ii, "/model_one/plot_results_model_one_R_g.png"),
  #        plot = plot_results_model_one_R_g_country_ii, width = image_width_in, height = image_width_in / ratio_width_height, units = "in")
  
  
  plot_results_model_one_R_g_country_ii_with_title <- plot_results_model_one_R_g_country_ii +
    ggtitle(plot_title) +
    theme(plot.title = element_text(size = 12))
  
  ggsave(filename = paste0("plots/", country_lower_case_ii, "/model_one/plot_results_model_one_R_g_with_title.png"),
         plot = plot_results_model_one_R_g_country_ii_with_title, width = image_width_in, height = image_width_in / ratio_width_height, units = "in")
  
  
  # with variants
  plot_results_model_one_R_g_variants_country_ii <- create_plot_result_variants(data_plot_results = data_results_country,
                                                                                data_plot_variants = data_variants_ch_dk_de_country,
                                                                                data_estimate = data_results_country$R_estimate * (1 - data_results_country$mutation_proba_estimate),
                                                                                data_lower_cred_int = data_results_country$R_estimate * (1 - data_results_country$mutation_proba_estimate),
                                                                                data_upper_cred_int = data_results_country$R_estimate * (1 - data_results_country$mutation_proba_estimate),
                                                                                plot_color_scale_values = color_scale_values_1,
                                                                                plot_color_scale_labels = color_scale_labels_1,
                                                                                scale_y_from = 0,
                                                                                scale_y_to = 1.5,
                                                                                scale_y_by = 0.1,
                                                                                add_line_at_one = TRUE,
                                                                                label_y = expression(paste("Estimated", ~ R[g])))
  
  # ggsave(filename = paste0("plots/", country_lower_case_ii, "/model_one/plot_results_model_one_R_g_variants.png"),
  #        plot = plot_results_model_one_R_g_variants_country_ii, width = image_width_in, height = image_width_in / ratio_width_height, units = "in")
  
  
  plot_results_model_one_R_g_variants_country_ii_with_title <- plot_results_model_one_R_g_variants_country_ii +
    ggtitle(plot_title) +
    theme(plot.title = element_text(size = 12))
  
  ggsave(filename = paste0("plots/", country_lower_case_ii, "/model_one/plot_results_model_one_R_g_variants_with_title.png"),
         plot = plot_results_model_one_R_g_variants_country_ii_with_title, width = image_width_in, height = image_width_in / ratio_width_height, units = "in")
  
  
  
  # dispersion parameter ----
  plot_title <- paste0(country_upper_case_ii, ": Estimations of the dispersion parameter during 2021")
  
  # without variants
  plot_results_model_one_k_country_ii <- create_plot_result(data_plot_results = data_results_country,
                                                            data_estimate = data_results_country$k_estimate,
                                                            data_lower_cred_int = data_results_country$k_lower_cred_int,
                                                            data_upper_cred_int = data_results_country$k_upper_cred_int,
                                                            plot_color_scale_values = color_scale_values_1,
                                                            plot_color_scale_labels = color_scale_labels_1,
                                                            scale_y_from = 0,
                                                            scale_y_to = 0.8,
                                                            scale_y_by = 0.1,
                                                            add_line_at_one = FALSE,
                                                            label_y = "Estimated dispersion parameter")
  
  # ggsave(filename = paste0("plots/", country_lower_case_ii, "/model_one/plot_results_model_one_k.png"),
  #        plot = plot_results_model_one_k_country_ii, width = image_width_in, height = image_width_in / ratio_width_height, units = "in")
  
  
  plot_results_model_one_k_country_ii_with_title <- plot_results_model_one_k_country_ii +
    ggtitle(plot_title) +
    theme(plot.title = element_text(size = 12))
  
  ggsave(filename = paste0("plots/", country_lower_case_ii, "/model_one/plot_results_model_one_k_with_title.png"),
         plot = plot_results_model_one_k_country_ii_with_title, width = image_width_in, height = image_width_in / ratio_width_height, units = "in")
  
  
  # with variants
  plot_results_model_one_k_variants_country_ii <- create_plot_result_variants(data_plot_results = data_results_country,
                                                                              data_plot_variants = data_variants_ch_dk_de_country,
                                                                              data_estimate = data_results_country$k_estimate,
                                                                              data_lower_cred_int = data_results_country$k_lower_cred_int,
                                                                              data_upper_cred_int = data_results_country$k_upper_cred_int,
                                                                              plot_color_scale_values = color_scale_values_1,
                                                                              plot_color_scale_labels = color_scale_labels_1,
                                                                              scale_y_from = 0,
                                                                              scale_y_to = 0.8,
                                                                              scale_y_by = 0.1,
                                                                              add_line_at_one = FALSE,
                                                                              label_y = "Estimated dispersion parameter")
  
  # ggsave(filename = paste0("plots/", country_lower_case_ii, "/model_one/plot_results_model_one_k_variants.png"),
  #        plot = plot_results_model_one_k_variants_country_ii, width = image_width_in, height = image_width_in / ratio_width_height, units = "in")
  
  
  plot_results_model_one_k_variants_country_ii_with_title <- plot_results_model_one_k_variants_country_ii +
    ggtitle(plot_title) +
    theme(plot.title = element_text(size = 12))
  
  ggsave(filename = paste0("plots/", country_lower_case_ii, "/model_one/plot_results_model_one_k_variants_with_title.png"),
         plot = plot_results_model_one_k_variants_country_ii_with_title, width = image_width_in, height = image_width_in / ratio_width_height, units = "in")
  
  
  
  # yearly mutation rate ----
  plot_title <- paste0(country_upper_case_ii, ": Estimations of the number of yearly mutations during 2021")
  
  # without variants
  plot_results_model_one_number_yearly_mutations_country_ii <- create_plot_result(data_plot_results = data_results_country,
                                                                                  data_estimate = data_results_country$number_yearly_mutations_estimate,
                                                                                  data_lower_cred_int = data_results_country$number_yearly_mutations_lower_cred_int,
                                                                                  data_upper_cred_int = data_results_country$number_yearly_mutations_upper_cred_int,
                                                                                  plot_color_scale_values = color_scale_values_1,
                                                                                  plot_color_scale_labels = color_scale_labels_1,
                                                                                  scale_y_from = 0.0,
                                                                                  scale_y_to = 20.0,
                                                                                  scale_y_by = 1.0,
                                                                                  add_line_at_one = FALSE,
                                                                                  label_y = "Estimated number of yearly mutations")
  
  # ggsave(filename = paste0("plots/", country_lower_case_ii, "/model_one/plot_results_model_one_number_yearly_mutations.png"),
  #        plot = plot_results_model_one_number_yearly_mutations_country_ii, width = image_width_in, height = image_width_in / ratio_width_height, units = "in")
  
  
  plot_results_model_one_number_yearly_mutations_country_ii_with_title <- plot_results_model_one_number_yearly_mutations_country_ii +
    ggtitle(plot_title) +
    theme(plot.title = element_text(size = 12))
  
  ggsave(filename = paste0("plots/", country_lower_case_ii, "/model_one/plot_results_model_one_number_yearly_mutation_with_titles.png"),
         plot = plot_results_model_one_number_yearly_mutations_country_ii_with_title, width = image_width_in, height = image_width_in / ratio_width_height, units = "in")
  
  
  # with variants
  plot_results_model_one_number_yearly_mutations_variants_country_ii <- create_plot_result_variants(data_plot_results = data_results_country,
                                                                                                    data_plot_variants = data_variants_ch_dk_de_country,
                                                                                                    data_estimate = data_results_country$number_yearly_mutations_estimate,
                                                                                                    data_lower_cred_int = data_results_country$number_yearly_mutations_lower_cred_int,
                                                                                                    data_upper_cred_int = data_results_country$number_yearly_mutations_upper_cred_int,
                                                                                                    plot_color_scale_values = color_scale_values_1,
                                                                                                    plot_color_scale_labels = color_scale_labels_1,
                                                                                                    scale_y_from = 0.0,
                                                                                                    scale_y_to = 20.0,
                                                                                                    scale_y_by = 1.0,
                                                                                                    add_line_at_one = FALSE,
                                                                                                    label_y = "Estimated number of yearly mutations")
  
  # ggsave(filename = paste0("plots/", country_lower_case_ii, "/model_one/plot_results_model_one_number_yearly_mutations_variants.png"),
  #        plot = plot_results_model_one_number_yearly_mutations_variants_country_ii, width = image_width_in, height = image_width_in / ratio_width_height, units = "in")
  
  
  plot_results_model_one_number_yearly_mutations_variants_country_ii_with_title <- plot_results_model_one_number_yearly_mutations_variants_country_ii +
    ggtitle(plot_title) +
    theme(plot.title = element_text(size = 12))
  
  ggsave(filename = paste0("plots/", country_lower_case_ii, "/model_one/plot_results_model_one_number_yearly_mutations_variants_with_title.png"),
         plot = plot_results_model_one_number_yearly_mutations_variants_country_ii_with_title, width = image_width_in, height = image_width_in / ratio_width_height, units = "in")
  
  
  
  # mutation probability ----
  plot_title <- paste0(country_upper_case_ii, ": Estimations of the mutation probability during 2021")
  
  # without variants
  plot_results_model_one_mutation_probability_country_ii <- create_plot_result(data_plot_results = data_results_country,
                                                                               data_estimate = data_results_country$mutation_proba_estimate,
                                                                               data_lower_cred_int = data_results_country$mutation_proba_lower_cred_int,
                                                                               data_upper_cred_int = data_results_country$mutation_proba_upper_cred_int,
                                                                               plot_color_scale_values = color_scale_values_1,
                                                                               plot_color_scale_labels = color_scale_labels_1,
                                                                               scale_y_from = 0,
                                                                               scale_y_to = 1.0,
                                                                               scale_y_by = 0.1,
                                                                               add_line_at_one = FALSE,
                                                                               label_y = "Estimated mutation probability")
  
  # ggsave(filename = paste0("plots/", country_lower_case_ii, "/model_one/plot_results_model_one_mutation_probability.png"),
  #        plot = plot_results_model_one_mutation_probability_country_ii, width = image_width_in, height = image_width_in / ratio_width_height, units = "in")
  
  
  plot_results_model_one_mutation_probability_country_ii_with_title <- plot_results_model_one_mutation_probability_country_ii +
    ggtitle(plot_title) +
    theme(plot.title = element_text(size = 12))
  
  ggsave(filename = paste0("plots/", country_lower_case_ii, "/model_one/plot_results_model_one_mutation_probability_with_title.png"),
         plot = plot_results_model_one_mutation_probability_country_ii_with_title, width = image_width_in, height = image_width_in / ratio_width_height, units = "in")
  
  
  # with variants
  plot_results_model_one_mutation_probability_variants_country_ii <- create_plot_result_variants(data_plot_results = data_results_country,
                                                                                                 data_plot_variants = data_variants_ch_dk_de_country,
                                                                                                 data_estimate = data_results_country$mutation_proba_estimate,
                                                                                                 data_lower_cred_int = data_results_country$mutation_proba_lower_cred_int,
                                                                                                 data_upper_cred_int = data_results_country$mutation_proba_upper_cred_int,
                                                                                                 plot_color_scale_values = color_scale_values_1,
                                                                                                 plot_color_scale_labels = color_scale_labels_1,
                                                                                                 scale_y_from = 0,
                                                                                                 scale_y_to = 1.0,
                                                                                                 scale_y_by = 0.1,
                                                                                                 add_line_at_one = FALSE,
                                                                                                 label_y = "Estimated mutation probability")
  
  # ggsave(filename = paste0("plots/", country_lower_case_ii, "/model_one/plot_results_model_one_mutation_probability_variants.png"),
  #        plot = plot_results_model_one_mutation_probability_variants_country_ii, width = image_width_in, height = image_width_in / ratio_width_height, units = "in")
  
  
  plot_results_model_one_mutation_probability_variants_country_ii_with_title <- plot_results_model_one_mutation_probability_variants_country_ii +
    ggtitle(plot_title) +
    theme(plot.title = element_text(size = 12))
  
  ggsave(filename = paste0("plots/", country_lower_case_ii, "/model_one/plot_results_model_one_mutation_probability_variants_with_title.png"),
         plot = plot_results_model_one_mutation_probability_variants_country_ii_with_title, width = image_width_in, height = image_width_in / ratio_width_height, units = "in")
  
  
  
  # testing probability ----
  plot_title <- paste0(country_upper_case_ii, ": Estimations of the testing probability during 2021")
  
  # without variants
  plot_results_model_one_testing_probability_country_ii <- create_plot_result(data_plot_results = data_results_country,
                                                                              data_estimate = data_results_country$testing_proba_estimate,
                                                                              data_lower_cred_int = data_results_country$testing_proba_lower_cred_int,
                                                                              data_upper_cred_int = data_results_country$testing_proba_upper_cred_int,
                                                                              plot_color_scale_values = color_scale_values_1,
                                                                              plot_color_scale_labels = color_scale_labels_1,
                                                                              scale_y_from = 0,
                                                                              scale_y_to = 1.0,
                                                                              scale_y_by = 0.1,
                                                                              add_line_at_one = FALSE,
                                                                              label_y = "Estimated testing probability")
  
  # ggsave(filename = paste0("plots/", country_lower_case_ii, "/model_one/plot_results_model_one_testing_probability.png"),
  #        plot = plot_results_model_one_testing_probability_country_ii, width = image_width_in, height = image_width_in / ratio_width_height, units = "in")
  
  
  plot_results_model_one_testing_probability_country_ii_with_title <- plot_results_model_one_testing_probability_country_ii +
    ggtitle(plot_title) +
    theme(plot.title = element_text(size = 12))
  
  ggsave(filename = paste0("plots/", country_lower_case_ii, "/model_one/plot_results_model_one_testing_probability_with_title.png"),
         plot = plot_results_model_one_testing_probability_country_ii_with_title, width = image_width_in, height = image_width_in / ratio_width_height, units = "in")
  
  
  # with variants
  plot_results_model_one_testing_probability_variants_country_ii <- create_plot_result_variants(data_plot_results = data_results_country,
                                                                                                data_plot_variants = data_variants_ch_dk_de_country,
                                                                                                data_estimate = data_results_country$testing_proba_estimate,
                                                                                                data_lower_cred_int = data_results_country$testing_proba_lower_cred_int,
                                                                                                data_upper_cred_int = data_results_country$testing_proba_upper_cred_int,
                                                                                                plot_color_scale_values = color_scale_values_1,
                                                                                                plot_color_scale_labels = color_scale_labels_1,
                                                                                                scale_y_from = 0,
                                                                                                scale_y_to = 1.0,
                                                                                                scale_y_by = 0.1,
                                                                                                add_line_at_one = FALSE,
                                                                                                label_y = "Estimated testing probability")
  
  # ggsave(filename = paste0("plots/", country_lower_case_ii, "/model_one/plot_results_model_one_testing_probability_variants.png"),
  #        plot = plot_results_model_one_testing_probability_variants_country_ii, width = image_width_in, height = image_width_in / ratio_width_height, units = "in")
  
  
  plot_results_model_one_testing_probability_variants_country_ii_with_title <- plot_results_model_one_testing_probability_variants_country_ii +
    ggtitle(plot_title) +
    theme(plot.title = element_text(size = 12))
  
  ggsave(filename = paste0("plots/", country_lower_case_ii, "/model_one/plot_results_model_one_testing_probability_variants_with_title.png"),
         plot = plot_results_model_one_testing_probability_variants_country_ii_with_title, width = image_width_in, height = image_width_in / ratio_width_height, units = "in")
  
  
  
  # detection probability ----
  plot_title <- paste0(country_upper_case_ii, ": Estimations of the detection probability during 2021")
  
  # without variants
  plot_results_model_one_detection_probability_country_ii <- create_plot_result(data_plot_results = data_results_country,
                                                                                data_estimate = data_results_country$detection_proba_estimate,
                                                                                data_lower_cred_int = data_results_country$detection_proba_lower_cred_int,
                                                                                data_upper_cred_int = data_results_country$detection_proba_upper_cred_int,
                                                                                plot_color_scale_values = color_scale_values_1,
                                                                                plot_color_scale_labels = color_scale_labels_1,
                                                                                scale_y_from = 0,
                                                                                scale_y_to = 1.0,
                                                                                scale_y_by = 0.1,
                                                                                add_line_at_one = FALSE,
                                                                                label_y = "Estimated detection probability")
  
  # ggsave(filename = paste0("plots/", country_lower_case_ii, "/model_one/plot_results_model_one_detection_probability.png"),
  #        plot = plot_results_model_one_detection_probability_country_ii, width = image_width_in, height = image_width_in / ratio_width_height, units = "in")
  
  
  plot_results_model_one_detection_probability_country_ii_with_title <- plot_results_model_one_detection_probability_country_ii +
    ggtitle(plot_title) +
    theme(plot.title = element_text(size = 12))
  
  ggsave(filename = paste0("plots/", country_lower_case_ii, "/model_one/plot_results_model_one_detection_probability_with_title.png"),
         plot = plot_results_model_one_detection_probability_country_ii_with_title, width = image_width_in, height = image_width_in / ratio_width_height, units = "in")
  
  
  # with variants
  plot_results_model_one_detection_probability_variants_country_ii <- create_plot_result_variants(data_plot_results = data_results_country,
                                                                                                  data_plot_variants = data_variants_ch_dk_de_country,
                                                                                                  data_estimate = data_results_country$detection_proba_estimate,
                                                                                                  data_lower_cred_int = data_results_country$detection_proba_lower_cred_int,
                                                                                                  data_upper_cred_int = data_results_country$detection_proba_upper_cred_int,
                                                                                                  plot_color_scale_values = color_scale_values_1,
                                                                                                  plot_color_scale_labels = color_scale_labels_1,
                                                                                                  scale_y_from = 0,
                                                                                                  scale_y_to = 1.0,
                                                                                                  scale_y_by = 0.1,
                                                                                                  add_line_at_one = FALSE,
                                                                                                  label_y = "Estimated detection probability")
  
  # ggsave(filename = paste0("plots/", country_lower_case_ii, "/model_one/plot_results_model_one_detection_probability_variants.png"),
  #        plot = plot_results_model_one_detection_probability_variants_country_ii, width = image_width_in, height = image_width_in / ratio_width_height, units = "in")
  
  
  plot_results_model_one_detection_probability_variants_country_ii_with_title <- plot_results_model_one_detection_probability_variants_country_ii +
    ggtitle(plot_title) +
    theme(plot.title = element_text(size = 12))
  
  ggsave(filename = paste0("plots/", country_lower_case_ii, "/model_one/plot_results_model_one_detection_probability_variants_with_title.png"),
         plot = plot_results_model_one_detection_probability_variants_country_ii_with_title, width = image_width_in, height = image_width_in / ratio_width_height, units = "in")
  
}









