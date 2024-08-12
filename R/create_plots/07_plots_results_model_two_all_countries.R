
# plot results of model two for all countries


# read and preprocess data ----

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
results_model_two_ch_dk_de_2021_months_0 <- read_csv(file = path_results_model_two_ch_dk_de_processed)

results_model_two_ch_dk_de_2021_months <- results_model_two_ch_dk_de_2021_months_0 %>% 
  mutate(testing_proba_lower_cred_int = testing_proba,
         testing_proba_upper_cred_int = testing_proba,
         testing_proba_Rhat = 1,
         detection_proba_lower_cred_int = detection_proba,
         detection_proba_upper_cred_int = detection_proba,
         detection_proba_Rhat = 1) %>%
  rename("testing_proba_estimate" = "testing_proba",
         "detection_proba_estimate" = "detection_proba") %>%
  mutate(model = "A", offset = 0.0)


data_results <- results_model_two_ch_dk_de_2021_months

color_scale_values_1 <- c("black")
color_scale_labels_1 <- c("Model 2")

color_scale_values_2 <- c("black", "steelblue")
color_scale_labels_2 <- c("Estimate", "External estimate")



# create plots ----
ratio_width_height <- 4/3
image_width_in <- 7.3
list_months <- c("Jan", "Feb", "Mar", "Apr", "May", "Jun", "Jul", "Aug", "Sep", "Oct", "Nov", "Dec")
colors_variants <- paletteer_c("viridis::plasma", n = 4)



# effective reproduction number ----
plot_title <- expression(paste("Estimations of the effective reproduction number",  ~ R[e], ~ "during 2021"))

# without variants
plot_results_model_two_R_e_all_countries_1 <- create_plot_result_all_countries(data_plot_results = data_results,
                                                                               data_estimate = data_results$R_estimate,
                                                                               data_lower_cred_int = data_results$R_lower_cred_int,
                                                                               data_upper_cred_int = data_results$R_upper_cred_int,
                                                                               plot_color_scale_values = color_scale_values_1, 
                                                                               plot_color_scale_labels = color_scale_labels_1,
                                                                               scale_y_from = 0,
                                                                               scale_y_to = 1.5,
                                                                               scale_y_by = 0.1,
                                                                               add_line_at_one = TRUE,
                                                                               label_y = expression(paste("Estimated", ~ R[e])))

# ggsave(filename = "plots/all_countries/model_two/plot_results_model_two_R_e_1.png",
#        plot = plot_results_model_two_R_e_all_countries_1, width = image_width_in, height = image_width_in / ratio_width_height, units = "in")


plot_results_model_two_R_e_all_countries_with_title_1 <- plot_results_model_two_R_e_all_countries_1 +
  ggtitle(plot_title)

ggsave(filename = "plots/all_countries/model_two/plot_results_model_two_R_e_with_title_1.png",
       plot = plot_results_model_two_R_e_all_countries_with_title_1, width = image_width_in, height = image_width_in / ratio_width_height, units = "in")


plot_results_model_two_R_e_all_countries_2 <- create_plot_result_all_countries(data_plot_results = data_results,
                                                                               data_plot_other_estimates = data_r_e_ch_dk_de,
                                                                               data_estimate = data_results$R_estimate,
                                                                               data_lower_cred_int = data_results$R_lower_cred_int,
                                                                               data_upper_cred_int = data_results$R_upper_cred_int,
                                                                               plot_color_scale_values = color_scale_values_2, 
                                                                               plot_color_scale_labels = color_scale_labels_2,
                                                                               scale_y_from = 0,
                                                                               scale_y_to = 2.0,
                                                                               scale_y_by = 0.2,
                                                                               add_line_at_one = TRUE,
                                                                               label_y = expression(paste("Estimated", ~ R[e])))

# ggsave(filename = "plots/all_countries/model_two/plot_results_model_two_R_e_2.png",
#        plot = plot_results_model_two_R_e_all_countries_2, width = image_width_in, height = image_width_in / ratio_width_height, units = "in")


plot_results_model_two_R_e_all_countries_with_title_2 <- plot_results_model_two_R_e_all_countries_2 +
  ggtitle(plot_title)

ggsave(filename = "plots/all_countries/model_two/plot_results_model_two_R_e_with_title_2.png",
       plot = plot_results_model_two_R_e_all_countries_with_title_2, width = image_width_in, height = image_width_in / ratio_width_height, units = "in")


# with variants
plot_results_model_two_R_e_variants_all_countries_1 <- create_plot_result_variants_all_countries(data_plot_results = data_results,
                                                                                                 data_plot_variants = data_variants_ch_dk_de,
                                                                                                 data_estimate = data_results$R_estimate,
                                                                                                 data_lower_cred_int = data_results$R_lower_cred_int,
                                                                                                 data_upper_cred_int = data_results$R_upper_cred_int,
                                                                                                 plot_color_scale_values = color_scale_values_1, 
                                                                                                 plot_color_scale_labels = color_scale_labels_1,
                                                                                                 scale_y_from = 0,
                                                                                                 scale_y_to = 1.5,
                                                                                                 scale_y_by = 0.1,
                                                                                                 add_line_at_one = TRUE,
                                                                                                 label_y = expression(paste("Estimated", ~ R[e])))

# ggsave(filename = "plots/all_countries/model_two/plot_results_model_two_R_e_variants_1.png",
#        plot = plot_results_model_two_R_e_variants_all_countries_1, width = image_width_in, height = image_width_in / ratio_width_height, units = "in")


plot_results_model_two_R_e_variants_all_countries_with_title_1 <- plot_results_model_two_R_e_variants_all_countries_1 +
  ggtitle(plot_title)

ggsave(filename = "plots/all_countries/model_two/plot_results_model_two_R_e_variants_with_title_1.png",
       plot = plot_results_model_two_R_e_variants_all_countries_with_title_1, width = image_width_in, height = image_width_in / ratio_width_height, units = "in")


plot_results_model_two_R_e_variants_all_countries_2 <- create_plot_result_variants_all_countries(data_plot_results = data_results,
                                                                                                 data_plot_variants = data_variants_ch_dk_de,
                                                                                                 data_plot_other_estimates = data_r_e_ch_dk_de,
                                                                                                 data_estimate = data_results$R_estimate,
                                                                                                 data_lower_cred_int = data_results$R_lower_cred_int,
                                                                                                 data_upper_cred_int = data_results$R_upper_cred_int,
                                                                                                 plot_color_scale_values = color_scale_values_2, 
                                                                                                 plot_color_scale_labels = color_scale_labels_2,
                                                                                                 scale_y_from = 0,
                                                                                                 scale_y_to = 2.0,
                                                                                                 scale_y_by = 0.2,
                                                                                                 add_line_at_one = TRUE,
                                                                                                 label_y = expression(paste("Estimated", ~ R[e])))

# ggsave(filename = "plots/all_countries/model_two/plot_results_model_two_R_e_variants_2.png",
#        plot = plot_results_model_two_R_e_variants_all_countries_2, width = image_width_in, height = image_width_in / ratio_width_height, units = "in")


plot_results_model_two_R_e_variants_all_countries_with_title_2 <- plot_results_model_two_R_e_variants_all_countries_2 +
  ggtitle(plot_title)

ggsave(filename = "plots/all_countries/model_two/plot_results_model_two_R_e_variants_with_title_2.png",
       plot = plot_results_model_two_R_e_variants_all_countries_with_title_2, width = image_width_in, height = image_width_in / ratio_width_height, units = "in")



# genomic reproduction number ----
plot_title <- expression(paste("Estimations of the genomic reproduction number",  ~ R[g], ~ "during 2021"))

# without variants
plot_results_model_two_R_g_all_countries <- create_plot_result_all_countries(data_plot_results = data_results,
                                                                             data_estimate = data_results$R_estimate * (1 - data_results$mutation_proba_estimate),
                                                                             data_lower_cred_int = data_results$R_estimate * (1 - data_results$mutation_proba_estimate),
                                                                             data_upper_cred_int = data_results$R_estimate * (1 - data_results$mutation_proba_estimate),
                                                                             plot_color_scale_values = color_scale_values_1, 
                                                                             plot_color_scale_labels = color_scale_labels_1,
                                                                             scale_y_from = 0,
                                                                             scale_y_to = 1.5,
                                                                             scale_y_by = 0.1,
                                                                             add_line_at_one = TRUE,
                                                                             label_y = expression(paste("Estimated", ~ R[g])))

# ggsave(filename = "plots/all_countries/model_two/plot_results_model_two_R_g.png",
#        plot = plot_results_model_two_R_g_all_countries, width = image_width_in, height = image_width_in / ratio_width_height, units = "in")


plot_results_model_two_R_g_all_countries_with_title <- plot_results_model_two_R_g_all_countries +
  ggtitle(plot_title)

ggsave(filename = "plots/all_countries/model_two/plot_results_model_two_R_g_with_title.png",
       plot = plot_results_model_two_R_g_all_countries_with_title, width = image_width_in, height = image_width_in / ratio_width_height, units = "in")


# with variants
plot_results_model_two_R_g_variants_all_countries <- create_plot_result_variants_all_countries(data_plot_results = data_results,
                                                                                               data_plot_variants = data_variants_ch_dk_de,
                                                                                               data_estimate = data_results$R_estimate * (1 - data_results$mutation_proba_estimate),
                                                                                               data_lower_cred_int = data_results$R_estimate * (1 - data_results$mutation_proba_estimate),
                                                                                               data_upper_cred_int = data_results$R_estimate * (1 - data_results$mutation_proba_estimate),
                                                                                               plot_color_scale_values = color_scale_values_1, 
                                                                                               plot_color_scale_labels = color_scale_labels_1,
                                                                                               scale_y_from = 0,
                                                                                               scale_y_to = 1.5,
                                                                                               scale_y_by = 0.1,
                                                                                               add_line_at_one = TRUE,
                                                                                               label_y = expression(paste("Estimated", ~ R[g])))

# ggsave(filename = "plots/all_countries/model_two/plot_results_model_two_R_g_variants.png",
#        plot = plot_results_model_two_R_g_variants_all_countries, width = image_width_in, height = image_width_in / ratio_width_height, units = "in")


plot_results_model_two_R_g_variants_all_countries_with_title <- plot_results_model_two_R_g_variants_all_countries +
  ggtitle(plot_title)

ggsave(filename = "plots/all_countries/model_two/plot_results_model_two_R_g_variants_with_title.png",
       plot = plot_results_model_two_R_g_variants_all_countries_with_title, width = image_width_in, height = image_width_in / ratio_width_height, units = "in")



# dispersion parameter ----
plot_title <- "Estimations of the dispersion parameter during 2021"

# without variants
plot_results_model_two_k_all_countries <- create_plot_result_all_countries(data_plot_results = data_results,
                                                                           data_estimate = data_results$k_estimate,
                                                                           data_lower_cred_int = data_results$k_lower_cred_int,
                                                                           data_upper_cred_int = data_results$k_upper_cred_int,
                                                                           plot_color_scale_values = color_scale_values_1, 
                                                                           plot_color_scale_labels = color_scale_labels_1,
                                                                           scale_y_from = 0,
                                                                           scale_y_to = 0.8,
                                                                           scale_y_by = 0.1,
                                                                           add_line_at_one = FALSE,
                                                                           label_y = "Estimated dispersion parameter")

# ggsave(filename = "plots/all_countries/model_two/plot_results_model_two_k.png",
#        plot = plot_results_model_two_k_all_countries, width = image_width_in, height = image_width_in / ratio_width_height, units = "in")


plot_results_model_two_k_all_countries_with_title <- plot_results_model_two_k_all_countries +
  ggtitle(plot_title)

ggsave(filename = "plots/all_countries/model_two/plot_results_model_two_k_with_title.png",
       plot = plot_results_model_two_k_all_countries_with_title, width = image_width_in, height = image_width_in / ratio_width_height, units = "in")


# with variants
plot_results_model_two_k_variants_all_countries <- create_plot_result_variants_all_countries(data_plot_results = data_results,
                                                                                             data_plot_variants = data_variants_ch_dk_de,
                                                                                             data_estimate = data_results$k_estimate,
                                                                                             data_lower_cred_int = data_results$k_lower_cred_int,
                                                                                             data_upper_cred_int = data_results$k_upper_cred_int,
                                                                                             plot_color_scale_values = color_scale_values_1, 
                                                                                             plot_color_scale_labels = color_scale_labels_1,
                                                                                             scale_y_from = 0,
                                                                                             scale_y_to = 0.8,
                                                                                             scale_y_by = 0.1,
                                                                                             add_line_at_one = FALSE,
                                                                                             label_y = "Estimated dispersion parameter")

# ggsave(filename = "plots/all_countries/model_two/plot_results_model_two_k_variants.png",
#        plot = plot_results_model_two_k_variants_all_countries, width = image_width_in, height = image_width_in / ratio_width_height, units = "in")


plot_results_model_two_k_variants_all_countries_with_title <- plot_results_model_two_k_variants_all_countries +
  ggtitle(plot_title)

ggsave(filename = "plots/all_countries/model_two/plot_results_model_two_k_variants_with_title.png",
       plot = plot_results_model_two_k_variants_all_countries_with_title, width = image_width_in, height = image_width_in / ratio_width_height, units = "in")



# yearly mutation rate ----
plot_title <- "Estimations of the number of yearly mutations during 2021"

# without variants
plot_results_model_two_number_yearly_mutations_all_countries <- create_plot_result_all_countries(data_plot_results = data_results,
                                                                                                 data_estimate = data_results$number_yearly_mutations_estimate,
                                                                                                 data_lower_cred_int = data_results$number_yearly_mutations_lower_cred_int,
                                                                                                 data_upper_cred_int = data_results$number_yearly_mutations_upper_cred_int,
                                                                                                 plot_color_scale_values = color_scale_values_1, 
                                                                                                 plot_color_scale_labels = color_scale_labels_1,
                                                                                                 scale_y_from = 0.0,
                                                                                                 scale_y_to = 20.0,
                                                                                                 scale_y_by = 1.0,
                                                                                                 add_line_at_one = FALSE,
                                                                                                 label_y = "Estimated number of yearly mutations")

# ggsave(filename = "plots/all_countries/model_two/plot_results_model_two_number_yearly_mutations.png",
#        plot = plot_results_model_two_number_yearly_mutations_all_countries, width = image_width_in, height = image_width_in / ratio_width_height, units = "in")


plot_results_model_two_number_yearly_mutations_all_countries_with_title <- plot_results_model_two_number_yearly_mutations_all_countries +
  ggtitle(plot_title)

ggsave(filename = "plots/all_countries/model_two/plot_results_model_two_number_yearly_mutation_with_titles.png",
       plot = plot_results_model_two_number_yearly_mutations_all_countries_with_title, width = image_width_in, height = image_width_in / ratio_width_height, units = "in")


# with variants
plot_results_model_two_number_yearly_mutations_variants_all_countries <- create_plot_result_variants_all_countries(data_plot_results = data_results,
                                                                                                                   data_plot_variants = data_variants_ch_dk_de,
                                                                                                                   data_estimate = data_results$number_yearly_mutations_estimate,
                                                                                                                   data_lower_cred_int = data_results$number_yearly_mutations_lower_cred_int,
                                                                                                                   data_upper_cred_int = data_results$number_yearly_mutations_upper_cred_int,
                                                                                                                   plot_color_scale_values = color_scale_values_1, 
                                                                                                                   plot_color_scale_labels = color_scale_labels_1,
                                                                                                                   scale_y_from = 0.0,
                                                                                                                   scale_y_to = 20.0,
                                                                                                                   scale_y_by = 1.0,
                                                                                                                   add_line_at_one = FALSE,
                                                                                                                   label_y = "Estimated number of yearly mutations")

# ggsave(filename = "plots/all_countries/model_two/plot_results_model_two_number_yearly_mutations_variants.png",
#        plot = plot_results_model_two_number_yearly_mutations_variants_all_countries, width = image_width_in, height = image_width_in / ratio_width_height, units = "in")


plot_results_model_two_number_yearly_mutations_variants_all_countries_with_title <- plot_results_model_two_number_yearly_mutations_variants_all_countries +
  ggtitle(plot_title)

ggsave(filename = "plots/all_countries/model_two/plot_results_model_two_number_yearly_mutations_variants_with_title.png",
       plot = plot_results_model_two_number_yearly_mutations_variants_all_countries_with_title, width = image_width_in, height = image_width_in / ratio_width_height, units = "in")



# mutation probability ----
plot_title <- "Estimations of the mutation probability during 2021"

# without variants
plot_results_model_two_mutation_probability_all_countries <- create_plot_result_all_countries(data_plot_results = data_results,
                                                                                              data_estimate = data_results$mutation_proba_estimate,
                                                                                              data_lower_cred_int = data_results$mutation_proba_lower_cred_int,
                                                                                              data_upper_cred_int = data_results$mutation_proba_upper_cred_int,
                                                                                              plot_color_scale_values = color_scale_values_1, 
                                                                                              plot_color_scale_labels = color_scale_labels_1,
                                                                                              scale_y_from = 0,
                                                                                              scale_y_to = 1.0,
                                                                                              scale_y_by = 0.1,
                                                                                              add_line_at_one = FALSE,
                                                                                              label_y = "Estimated mutation probability")

# ggsave(filename = "plots/all_countries/model_two/plot_results_model_two_mutation_probability.png",
#        plot = plot_results_model_two_mutation_probability_all_countries, width = image_width_in, height = image_width_in / ratio_width_height, units = "in")


plot_results_model_two_mutation_probability_all_countries_with_title <- plot_results_model_two_mutation_probability_all_countries +
  ggtitle(plot_title)

ggsave(filename = "plots/all_countries/model_two/plot_results_model_two_mutation_probability_with_title.png",
       plot = plot_results_model_two_mutation_probability_all_countries_with_title, width = image_width_in, height = image_width_in / ratio_width_height, units = "in")


# with variants
plot_results_model_two_mutation_probability_variants_all_countries <- create_plot_result_variants_all_countries(data_plot_results = data_results,
                                                                                                                data_plot_variants = data_variants_ch_dk_de,
                                                                                                                data_estimate = data_results$mutation_proba_estimate,
                                                                                                                data_lower_cred_int = data_results$mutation_proba_lower_cred_int,
                                                                                                                data_upper_cred_int = data_results$mutation_proba_upper_cred_int,
                                                                                                                plot_color_scale_values = color_scale_values_1, 
                                                                                                                plot_color_scale_labels = color_scale_labels_1,
                                                                                                                scale_y_from = 0,
                                                                                                                scale_y_to = 1.0,
                                                                                                                scale_y_by = 0.1,
                                                                                                                add_line_at_one = FALSE,
                                                                                                                label_y = "Estimated mutation probability")

# ggsave(filename = "plots/all_countries/model_two/plot_results_model_two_mutation_probability_variants.png",
#        plot = plot_results_model_two_mutation_probability_variants_all_countries, width = image_width_in, height = image_width_in / ratio_width_height, units = "in")


plot_results_model_two_mutation_probability_variants_all_countries_with_title <- plot_results_model_two_mutation_probability_variants_all_countries +
  ggtitle(plot_title)

ggsave(filename = "plots/all_countries/model_two/plot_results_model_two_mutation_probability_variants_with_title.png",
       plot = plot_results_model_two_mutation_probability_variants_all_countries_with_title, width = image_width_in, height = image_width_in / ratio_width_height, units = "in")



# testing probability ----
plot_title <- "Estimations of the testing probability during 2021"

# without variants
plot_results_model_two_testing_probability_all_countries <- create_plot_result_all_countries(data_plot_results = data_results,
                                                                                             data_estimate = data_results$testing_proba_estimate,
                                                                                             data_lower_cred_int = data_results$testing_proba_lower_cred_int,
                                                                                             data_upper_cred_int = data_results$testing_proba_upper_cred_int,
                                                                                             plot_color_scale_values = color_scale_values_1, 
                                                                                             plot_color_scale_labels = color_scale_labels_1,
                                                                                             scale_y_from = 0,
                                                                                             scale_y_to = 1.0,
                                                                                             scale_y_by = 0.1,
                                                                                             add_line_at_one = FALSE,
                                                                                             label_y = "Estimated testing probability")

# ggsave(filename = "plots/all_countries/model_two/plot_results_model_two_testing_probability.png",
#        plot = plot_results_model_two_testing_probability_all_countries, width = image_width_in, height = image_width_in / ratio_width_height, units = "in")


plot_results_model_two_testing_probability_all_countries_with_title <- plot_results_model_two_testing_probability_all_countries +
  ggtitle(plot_title)

ggsave(filename = "plots/all_countries/model_two/plot_results_model_two_testing_probability_with_title.png",
       plot = plot_results_model_two_testing_probability_all_countries_with_title, width = image_width_in, height = image_width_in / ratio_width_height, units = "in")


# with variants
plot_results_model_two_testing_probability_variants_all_countries <- create_plot_result_variants_all_countries(data_plot_results = data_results,
                                                                                                               data_plot_variants = data_variants_ch_dk_de,
                                                                                                               data_estimate = data_results$testing_proba_estimate,
                                                                                                               data_lower_cred_int = data_results$testing_proba_lower_cred_int,
                                                                                                               data_upper_cred_int = data_results$testing_proba_upper_cred_int,
                                                                                                               plot_color_scale_values = color_scale_values_1, 
                                                                                                               plot_color_scale_labels = color_scale_labels_1,
                                                                                                               scale_y_from = 0,
                                                                                                               scale_y_to = 1.0,
                                                                                                               scale_y_by = 0.1,
                                                                                                               add_line_at_one = FALSE,
                                                                                                               label_y = "Estimated testing probability")

# ggsave(filename = "plots/all_countries/model_two/plot_results_model_two_testing_probability_variants.png",
#        plot = plot_results_model_two_testing_probability_variants_all_countries, width = image_width_in, height = image_width_in / ratio_width_height, units = "in")


plot_results_model_two_testing_probability_variants_all_countries_with_title <- plot_results_model_two_testing_probability_variants_all_countries +
  ggtitle(plot_title)

ggsave(filename = "plots/all_countries/model_two/plot_results_model_two_testing_probability_variants_with_title.png",
       plot = plot_results_model_two_testing_probability_variants_all_countries_with_title, width = image_width_in, height = image_width_in / ratio_width_height, units = "in")



# detection probability ----
plot_title <- "Estimations of the detection probability during 2021"

# without variants
plot_results_model_two_detection_probability_all_countries <- create_plot_result_all_countries(data_plot_results = data_results,
                                                                                               data_estimate = data_results$detection_proba_estimate,
                                                                                               data_lower_cred_int = data_results$detection_proba_lower_cred_int,
                                                                                               data_upper_cred_int = data_results$detection_proba_upper_cred_int,
                                                                                               plot_color_scale_values = color_scale_values_1, 
                                                                                               plot_color_scale_labels = color_scale_labels_1,
                                                                                               scale_y_from = 0,
                                                                                               scale_y_to = 1.0,
                                                                                               scale_y_by = 0.1,
                                                                                               add_line_at_one = FALSE,
                                                                                               label_y = "Estimated detection probability")

# ggsave(filename = "plots/all_countries/model_two/plot_results_model_two_detection_probability.png",
#        plot = plot_results_model_two_detection_probability_all_countries, width = image_width_in, height = image_width_in / ratio_width_height, units = "in")


plot_results_model_two_detection_probability_all_countries_with_title <- plot_results_model_two_detection_probability_all_countries +
  ggtitle(plot_title)

ggsave(filename = "plots/all_countries/model_two/plot_results_model_two_detection_probability_with_title.png",
       plot = plot_results_model_two_detection_probability_all_countries_with_title, width = image_width_in, height = image_width_in / ratio_width_height, units = "in")


# with variants
plot_results_model_two_detection_probability_variants_all_countries <- create_plot_result_variants_all_countries(data_plot_results = data_results,
                                                                                                                 data_plot_variants = data_variants_ch_dk_de,
                                                                                                                 data_estimate = data_results$detection_proba_estimate,
                                                                                                                 data_lower_cred_int = data_results$detection_proba_lower_cred_int,
                                                                                                                 data_upper_cred_int = data_results$detection_proba_upper_cred_int,
                                                                                                                 plot_color_scale_values = color_scale_values_1, 
                                                                                                                 plot_color_scale_labels = color_scale_labels_1,
                                                                                                                 scale_y_from = 0,
                                                                                                                 scale_y_to = 1.0,
                                                                                                                 scale_y_by = 0.1,
                                                                                                                 add_line_at_one = FALSE,
                                                                                                                 label_y = "Estimated detection probability")

# ggsave(filename = "plots/all_countries/model_two/plot_results_model_two_detection_probability_variants.png",
#        plot = plot_results_model_two_detection_probability_variants_all_countries, width = image_width_in, height = image_width_in / ratio_width_height, units = "in")


plot_results_model_two_detection_probability_variants_all_countries_with_title <- plot_results_model_two_detection_probability_variants_all_countries +
  ggtitle(plot_title)

ggsave(filename = "plots/all_countries/model_two/plot_results_model_two_detection_probability_variants_with_title.png",
       plot = plot_results_model_two_detection_probability_variants_all_countries_with_title, width = image_width_in, height = image_width_in / ratio_width_height, units = "in")




