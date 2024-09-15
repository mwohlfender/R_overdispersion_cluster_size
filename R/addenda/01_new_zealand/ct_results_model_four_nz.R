

# define paths ----

# define names of tables
name_table_results_model_four_nz_periods_1 <- "table_results_model_four_nz_periods_1"
name_table_results_model_four_nz_periods_1_a <- "table_results_model_four_nz_periods_1_a"
name_table_results_model_four_nz_periods_1_b <- "table_results_model_four_nz_periods_1_b"
name_table_results_model_four_nz_periods_2 <- "table_results_model_four_nz_periods_2"
name_table_results_model_four_nz_periods_2_a <- "table_results_model_four_nz_periods_2_a"
name_table_results_model_four_nz_periods_2_b <- "table_results_model_four_nz_periods_2_b"



# read data ----
results_model_four_nz_periods_0 <- read_csv(file = path_results_model_four_nz_periods_processed)



# process data ----
results_model_four_nz_periods <- results_model_four_nz_periods_0 %>%
  filter(period != "all")


# create tables ----
set_flextable_defaults(background.color = "white")

# Switzerland 
data_results_model_four_nz_periods_1 <- data.frame(I = as.character(1:36),
                                                   A = rep(x = c("Apr-May 2020", "Jun-Dec 2020", "Jan-Apr 2021", "May-Jul 2021"), times = 9),
                                                   B = paste0(format(round(results_model_four_nz_periods$R_estimate, 3), nsmall = 3), " [",
                                                              format(round(results_model_four_nz_periods$R_lower_cred_int, 3), nsmall = 3), "-",
                                                              format(round(results_model_four_nz_periods$R_upper_cred_int, 3), nsmall = 3), "]"),
                                                   C = paste0(format(round(results_model_four_nz_periods$k_estimate, 3), nsmall = 3), " [",
                                                              format(round(results_model_four_nz_periods$k_lower_cred_int, 3), nsmall = 3), "-",
                                                              format(round(results_model_four_nz_periods$k_upper_cred_int, 3), nsmall = 3), "]"))

names(data_results_model_four_nz_periods_1) <- c("Index", "Period", "Effective reproduction number", "Dispersion parameter")

table_results_model_four_nz_periods_1 <- flextable(data_results_model_four_nz_periods_1)
table_results_model_four_nz_periods_1 <- theme_vanilla(table_results_model_four_nz_periods_1)
table_results_model_four_nz_periods_1 <- width(table_results_model_four_nz_periods_1, j = 1:4, width = c(0.6, 1.1, 2.5, 2.1), unit = "in")

flextable::save_as_image(x = table_results_model_four_nz_periods_1, path = paste0("plots/new_zealand/tables/", name_table_results_model_four_nz_periods_1, ".png"))


data_results_model_four_nz_periods_1_a <- data.frame(I = as.character(1:24),
                                                     A = rep(x = c("Apr-May 2020", "Jun-Dec 2020", "Jan-Apr 2021", "May-Jul 2021"), times = 6),
                                                     B = paste0(format(round(results_model_four_nz_periods$R_estimate[1:24], 3), nsmall = 3), " [",
                                                                format(round(results_model_four_nz_periods$R_lower_cred_int[1:24], 3), nsmall = 3), "-",
                                                                format(round(results_model_four_nz_periods$R_upper_cred_int[1:24], 3), nsmall = 3), "]"),
                                                     C = paste0(format(round(results_model_four_nz_periods$k_estimate[1:24], 3), nsmall = 3), " [",
                                                                format(round(results_model_four_nz_periods$k_lower_cred_int[1:24], 3), nsmall = 3), "-",
                                                                format(round(results_model_four_nz_periods$k_upper_cred_int[1:24], 3), nsmall = 3), "]"))

names(data_results_model_four_nz_periods_1_a) <- c("Index", "Period", "Effective reproduction number", "Dispersion parameter")

table_results_model_four_nz_periods_1_a <- flextable(data_results_model_four_nz_periods_1_a)
table_results_model_four_nz_periods_1_a <- theme_vanilla(table_results_model_four_nz_periods_1_a)
table_results_model_four_nz_periods_1_a <- width(table_results_model_four_nz_periods_1_a, j = 1:4, width = c(0.6, 1.1, 2.5, 2.1), unit = "in")

flextable::save_as_image(x = table_results_model_four_nz_periods_1_a, path = paste0("plots/new_zealand/tables/", name_table_results_model_four_nz_periods_1_a, ".png"))


data_results_model_four_nz_periods_1_b <- data.frame(I = as.character(25:36),
                                                     A = rep(x = c("Apr-May 2020", "Jun-Dec 2020", "Jan-Apr 2021", "May-Jul 2021"), times = 3),
                                                     B = paste0(format(round(results_model_four_nz_periods$R_estimate[25:36], 3), nsmall = 3), " [",
                                                                format(round(results_model_four_nz_periods$R_lower_cred_int[25:36], 3), nsmall = 3), "-",
                                                                format(round(results_model_four_nz_periods$R_upper_cred_int[25:36], 3), nsmall = 3), "]"),
                                                     C = paste0(format(round(results_model_four_nz_periods$k_estimate[25:36], 3), nsmall = 3), " [",
                                                                format(round(results_model_four_nz_periods$k_lower_cred_int[25:36], 3), nsmall = 3), "-",
                                                                format(round(results_model_four_nz_periods$k_upper_cred_int[25:36], 3), nsmall = 3), "]"))

names(data_results_model_four_nz_periods_1_b) <- c("Index", "Period", "Effective reproduction number", "Dispersion parameter")

table_results_model_four_nz_periods_1_b <- flextable(data_results_model_four_nz_periods_1_b)
table_results_model_four_nz_periods_1_b <- theme_vanilla(table_results_model_four_nz_periods_1_b)
table_results_model_four_nz_periods_1_b <- width(table_results_model_four_nz_periods_1_b, j = 1:4, width = c(0.6, 1.1, 2.5, 2.1), unit = "in")

flextable::save_as_image(x = table_results_model_four_nz_periods_1_b, path = paste0("plots/new_zealand/tables/", name_table_results_model_four_nz_periods_1_b, ".png"))


data_results_model_four_nz_periods_2 <- data.frame(I = as.character(1:36),
                                                   A = rep(x = c("Apr-May 2020", "Jun-Dec 2020", "Jan-Apr 2021", "May-Jul 2021"), times = 9),
                                                   B = paste0(format(round(results_model_four_nz_periods$mutation_proba, 3), nsmall = 3)),
                                                   C = paste0(format(round(results_model_four_nz_periods$testing_proba, 3), nsmall = 3)),
                                                   D = paste0(format(round(results_model_four_nz_periods$detection_proba, 3), nsmall = 3)))

names(data_results_model_four_nz_periods_2) <- c("Index", "Period", "Mutation probability", "Testing probability", "Detection probability")

table_results_model_four_nz_periods_2 <- flextable(data_results_model_four_nz_periods_2)
table_results_model_four_nz_periods_2 <- theme_vanilla(table_results_model_four_nz_periods_2)
table_results_model_four_nz_periods_2 <- width(table_results_model_four_nz_periods_2, j = 1:5, width = c(0.4, 1.1, 1.6, 1.525, 1.675), unit = "in")

flextable::save_as_image(x = table_results_model_four_nz_periods_2, path = paste0("plots/new_zealand/tables/", name_table_results_model_four_nz_periods_2, ".png"))


data_results_model_four_nz_periods_2_a <- data.frame(I = as.character(1:12),
                                                     A = rep(x = c("Apr-May 2020", "Jun-Dec 2020", "Jan-Apr 2021", "May-Jul 2021"), times = 3),
                                                     B = paste0(format(round(results_model_four_nz_periods$mutation_proba[1:12], 3), nsmall = 3)),
                                                     C = paste0(format(round(results_model_four_nz_periods$testing_proba[1:12], 3), nsmall = 3)),
                                                     D = paste0(format(round(results_model_four_nz_periods$detection_proba[1:12], 3), nsmall = 3)))

names(data_results_model_four_nz_periods_2_a) <- c("Index", "Period", "Mutation probability", "Testing probability", "Detection probability")

table_results_model_four_nz_periods_2_a <- flextable(data_results_model_four_nz_periods_2_a)
table_results_model_four_nz_periods_2_a <- theme_vanilla(table_results_model_four_nz_periods_2_a)
table_results_model_four_nz_periods_2_a <- width(table_results_model_four_nz_periods_2_a, j = 1:5, width = c(0.4, 1.1, 1.6, 1.525, 1.675), unit = "in")

flextable::save_as_image(x = table_results_model_four_nz_periods_2_a, path = paste0("plots/new_zealand/tables/", name_table_results_model_four_nz_periods_2_a, ".png"))


data_results_model_four_nz_periods_2_b <- data.frame(I = as.character(13:36),
                                                     A = rep(x = c("Apr-May 2020", "Jun-Dec 2020", "Jan-Apr 2021", "May-Jul 2021"), times = 6),
                                                     B = paste0(format(round(results_model_four_nz_periods$mutation_proba[13:36], 3), nsmall = 3)),
                                                     C = paste0(format(round(results_model_four_nz_periods$testing_proba[13:36], 3), nsmall = 3)),
                                                     D = paste0(format(round(results_model_four_nz_periods$detection_proba[13:36], 3), nsmall = 3)))

names(data_results_model_four_nz_periods_2_b) <- c("Index", "Period", "Mutation probability", "Testing probability", "Detection probability")

table_results_model_four_nz_periods_2_b <- flextable(data_results_model_four_nz_periods_2_b)
table_results_model_four_nz_periods_2_b <- theme_vanilla(table_results_model_four_nz_periods_2_b)
table_results_model_four_nz_periods_2_b <- width(table_results_model_four_nz_periods_2_b, j = 1:5, width = c(0.4, 1.1, 1.6, 1.525, 1.675), unit = "in")

flextable::save_as_image(x = table_results_model_four_nz_periods_2_b, path = paste0("plots/new_zealand/tables/", name_table_results_model_four_nz_periods_2_b, ".png"))
