

# define paths ----

# define names of tables
name_table_results_model_one_ch_2021_months_1 <- "table_results_model_one_ch_2021_months_1"
name_table_results_model_one_dk_2021_months_1 <- "table_results_model_one_dk_2021_months_1"
name_table_results_model_one_de_2021_months_1 <- "table_results_model_one_de_2021_months_1"

name_table_results_model_one_ch_2021_months_2 <- "table_results_model_one_ch_2021_months_2"
name_table_results_model_one_dk_2021_months_2 <- "table_results_model_one_dk_2021_months_2"
name_table_results_model_one_de_2021_months_2 <- "table_results_model_one_de_2021_months_2"



# read data ----
results_model_one_ch_dk_de_2021_months <- read_csv(file = path_results_model_one_ch_dk_de_processed)



# process data ----
results_model_one_ch_2021_months <- results_model_one_ch_dk_de_2021_months |> filter(country == "Switzerland")
results_model_one_dk_2021_months <- results_model_one_ch_dk_de_2021_months |> filter(country == "Denmark")
results_model_one_de_2021_months <- results_model_one_ch_dk_de_2021_months |> filter(country == "Germany")



# create tables ----
set_flextable_defaults(background.color = "white")

# Switzerland 
data_results_model_one_ch_2021_months_1 <- data.frame(A = c("January", "February", "March", "April", "May", "June",
                                                            "July", "August", "September", "October", "November", "December"),
                                                      B = paste0(format(round(results_model_one_ch_2021_months$R_estimate, 3), nsmall = 3), " [",
                                                                 format(round(results_model_one_ch_2021_months$R_lower_cred_int, 3), nsmall = 3), "-",
                                                                 format(round(results_model_one_ch_2021_months$R_upper_cred_int, 3), nsmall = 3), "]"),
                                                      C = paste0(format(round(results_model_one_ch_2021_months$k_estimate, 3), nsmall = 3), " [",
                                                                 format(round(results_model_one_ch_2021_months$k_lower_cred_int, 3), nsmall = 3), "-",
                                                                 format(round(results_model_one_ch_2021_months$k_upper_cred_int, 3), nsmall = 3), "]"),
                                                      D = paste0(format(round(results_model_one_ch_2021_months$testing_proba_estimate, 3), nsmall = 3), " [",
                                                                 format(round(results_model_one_ch_2021_months$testing_proba_lower_cred_int, 3), nsmall = 3), "-",
                                                                 format(round(results_model_one_ch_2021_months$testing_proba_upper_cred_int, 3), nsmall = 3), "]"))

names(data_results_model_one_ch_2021_months_1) <- c("Month", "Effective reproduction number", "Dispersion parameter", "Testing probability")

table_results_model_one_ch_2021_months_1 <- flextable(data_results_model_one_ch_2021_months_1)
table_results_model_one_ch_2021_months_1 <- theme_vanilla(table_results_model_one_ch_2021_months_1)
table_results_model_one_ch_2021_months_1 <- width(table_results_model_one_ch_2021_months_1, j = 1:4, width = c(0.9, 1.8, 1.8, 1.8), unit = "in")

flextable::save_as_image(x = table_results_model_one_ch_2021_months_1, path = paste0("plots/switzerland/tables/", name_table_results_model_one_ch_2021_months_1, ".png"))


data_results_model_one_ch_2021_months_2 <- data.frame(A = c("January", "February", "March", "April", "May", "June",
                                                            "July", "August", "September", "October", "November", "December"),
                                                      B = paste0(format(round(results_model_one_ch_2021_months$number_yearly_mutations_estimate, 3), nsmall = 3), " [",
                                                                 format(round(results_model_one_ch_2021_months$number_yearly_mutations_lower_cred_int, 3), nsmall = 3), "-",
                                                                 format(round(results_model_one_ch_2021_months$number_yearly_mutations_upper_cred_int, 3), nsmall = 3), "]"),
                                                      C = paste0(format(round(results_model_one_ch_2021_months$mutation_proba_estimate, 3), nsmall = 3), " [",
                                                                 format(round(results_model_one_ch_2021_months$mutation_proba_lower_cred_int, 3), nsmall = 3), "-",
                                                                 format(round(results_model_one_ch_2021_months$mutation_proba_upper_cred_int, 3), nsmall = 3), "]"),
                                                      D = paste0(format(round(results_model_one_ch_2021_months$detection_proba_estimate, 3), nsmall = 3), " [",
                                                                 format(round(results_model_one_ch_2021_months$detection_proba_lower_cred_int, 3), nsmall = 3), "-",
                                                                 format(round(results_model_one_ch_2021_months$detection_proba_upper_cred_int, 3), nsmall = 3), "]"))

names(data_results_model_one_ch_2021_months_2) <- c("Month", "Yearly mutation rate", "Mutation probability", "Detection probability")

table_results_model_one_ch_2021_months_2 <- flextable(data_results_model_one_ch_2021_months_2)
table_results_model_one_ch_2021_months_2 <- theme_vanilla(table_results_model_one_ch_2021_months_2)
table_results_model_one_ch_2021_months_2 <- width(table_results_model_one_ch_2021_months_2, j = 1:4, width = c(0.9, 1.8, 1.8, 1.8), unit = "in")

flextable::save_as_image(x = table_results_model_one_ch_2021_months_2, path = paste0("plots/switzerland/tables/", name_table_results_model_one_ch_2021_months_2, ".png"))



# Denmark
data_results_model_one_dk_2021_months_1 <- data.frame(A = c("January", "February", "March", "April", "May", "June",
                                                            "July", "August", "September", "October", "November", "December"),
                                                      B = paste0(format(round(results_model_one_dk_2021_months$R_estimate, 3), nsmall = 3), " [",
                                                                 format(round(results_model_one_dk_2021_months$R_lower_cred_int, 3), nsmall = 3), "-",
                                                                 format(round(results_model_one_dk_2021_months$R_upper_cred_int, 3), nsmall = 3), "]"),
                                                      C = paste0(format(round(results_model_one_dk_2021_months$k_estimate, 3), nsmall = 3), " [",
                                                                 format(round(results_model_one_dk_2021_months$k_lower_cred_int, 3), nsmall = 3), "-",
                                                                 format(round(results_model_one_dk_2021_months$k_upper_cred_int, 3), nsmall = 3), "]"),
                                                      D = paste0(format(round(results_model_one_dk_2021_months$testing_proba_estimate, 3), nsmall = 3), " [",
                                                                 format(round(results_model_one_dk_2021_months$testing_proba_lower_cred_int, 3), nsmall = 3), "-",
                                                                 format(round(results_model_one_dk_2021_months$testing_proba_upper_cred_int, 3), nsmall = 3), "]"))

names(data_results_model_one_dk_2021_months_1) <- c("Month", "Effective reproduction number", "Dispersion parameter", "Testing probability")

table_results_model_one_dk_2021_months_1 <- flextable(data_results_model_one_dk_2021_months_1)
table_results_model_one_dk_2021_months_1 <- theme_vanilla(table_results_model_one_dk_2021_months_1)
table_results_model_one_dk_2021_months_1 <- width(table_results_model_one_dk_2021_months_1, j = 1:4, width = c(0.9, 1.8, 1.8, 1.8), unit = "in")

flextable::save_as_image(x = table_results_model_one_dk_2021_months_1, path = paste0("plots/denmark/tables/", name_table_results_model_one_dk_2021_months_1, ".png"))


data_results_model_one_dk_2021_months_2 <- data.frame(A = c("January", "February", "March", "April", "May", "June",
                                                            "July", "August", "September", "October", "November", "December"),
                                                      B = paste0(format(round(results_model_one_dk_2021_months$number_yearly_mutations_estimate, 3), nsmall = 3), " [",
                                                                 format(round(results_model_one_dk_2021_months$number_yearly_mutations_lower_cred_int, 3), nsmall = 3), "-",
                                                                 format(round(results_model_one_dk_2021_months$number_yearly_mutations_upper_cred_int, 3), nsmall = 3), "]"),
                                                      C = paste0(format(round(results_model_one_dk_2021_months$mutation_proba_estimate, 3), nsmall = 3), " [",
                                                                 format(round(results_model_one_dk_2021_months$mutation_proba_lower_cred_int, 3), nsmall = 3), "-",
                                                                 format(round(results_model_one_dk_2021_months$mutation_proba_upper_cred_int, 3), nsmall = 3), "]"),
                                                      D = paste0(format(round(results_model_one_dk_2021_months$detection_proba_estimate, 3), nsmall = 3), " [",
                                                                 format(round(results_model_one_dk_2021_months$detection_proba_lower_cred_int, 3), nsmall = 3), "-",
                                                                 format(round(results_model_one_dk_2021_months$detection_proba_upper_cred_int, 3), nsmall = 3), "]"))

names(data_results_model_one_dk_2021_months_2) <- c("Month", "Yearly mutation rate", "Mutation probability", "Detection probability")

table_results_model_one_dk_2021_months_2 <- flextable(data_results_model_one_dk_2021_months_2)
table_results_model_one_dk_2021_months_2 <- theme_vanilla(table_results_model_one_dk_2021_months_2)
table_results_model_one_dk_2021_months_2 <- width(table_results_model_one_dk_2021_months_2, j = 1:4, width = c(0.9, 1.8, 1.8, 1.8), unit = "in")

flextable::save_as_image(x = table_results_model_one_dk_2021_months_2, path = paste0("plots/denmark/tables/", name_table_results_model_one_dk_2021_months_2, ".png"))



# Germany
data_results_model_one_de_2021_months_1 <- data.frame(A = c("January", "February", "March", "April", "May", "June",
                                                            "July", "August", "September", "October", "November", "December"),
                                                      B = paste0(format(round(results_model_one_de_2021_months$R_estimate, 3), nsmall = 3), " [",
                                                                 format(round(results_model_one_de_2021_months$R_lower_cred_int, 3), nsmall = 3), "-",
                                                                 format(round(results_model_one_de_2021_months$R_upper_cred_int, 3), nsmall = 3), "]"),
                                                      C = paste0(format(round(results_model_one_de_2021_months$k_estimate, 3), nsmall = 3), " [",
                                                                 format(round(results_model_one_de_2021_months$k_lower_cred_int, 3), nsmall = 3), "-",
                                                                 format(round(results_model_one_de_2021_months$k_upper_cred_int, 3), nsmall = 3), "]"),
                                                      D = paste0(format(round(results_model_one_de_2021_months$testing_proba_estimate, 3), nsmall = 3), " [",
                                                                 format(round(results_model_one_de_2021_months$testing_proba_lower_cred_int, 3), nsmall = 3), "-",
                                                                 format(round(results_model_one_de_2021_months$testing_proba_upper_cred_int, 3), nsmall = 3), "]"))

names(data_results_model_one_de_2021_months_1) <- c("Month", "Effective reproduction number", "Dispersion parameter", "Testing probability")

table_results_model_one_de_2021_months_1 <- flextable(data_results_model_one_de_2021_months_1)
table_results_model_one_de_2021_months_1 <- theme_vanilla(table_results_model_one_de_2021_months_1)
table_results_model_one_de_2021_months_1 <- width(table_results_model_one_de_2021_months_1, j = 1:4, width = c(0.9, 1.8, 1.8, 1.8), unit = "in")

flextable::save_as_image(x = table_results_model_one_de_2021_months_1, path = paste0("plots/germany/tables/", name_table_results_model_one_de_2021_months_1, ".png"))


data_results_model_one_de_2021_months_2 <- data.frame(A = c("January", "February", "March", "April", "May", "June",
                                                            "July", "August", "September", "October", "November", "December"),
                                                      B = paste0(format(round(results_model_one_de_2021_months$number_yearly_mutations_estimate, 3), nsmall = 3), " [",
                                                                 format(round(results_model_one_de_2021_months$number_yearly_mutations_lower_cred_int, 3), nsmall = 3), "-",
                                                                 format(round(results_model_one_de_2021_months$number_yearly_mutations_upper_cred_int, 3), nsmall = 3), "]"),
                                                      C = paste0(format(round(results_model_one_de_2021_months$mutation_proba_estimate, 3), nsmall = 3), " [",
                                                                 format(round(results_model_one_de_2021_months$mutation_proba_lower_cred_int, 3), nsmall = 3), "-",
                                                                 format(round(results_model_one_de_2021_months$mutation_proba_upper_cred_int, 3), nsmall = 3), "]"),
                                                      D = paste0(format(round(results_model_one_de_2021_months$detection_proba_estimate, 3), nsmall = 3), " [",
                                                                 format(round(results_model_one_de_2021_months$detection_proba_lower_cred_int, 3), nsmall = 3), "-",
                                                                 format(round(results_model_one_de_2021_months$detection_proba_upper_cred_int, 3), nsmall = 3), "]"))

names(data_results_model_one_de_2021_months_2) <- c("Month", "Yearly mutation rate", "Mutation probability", "Detection probability")

table_results_model_one_de_2021_months_2 <- flextable(data_results_model_one_de_2021_months_2)
table_results_model_one_de_2021_months_2 <- theme_vanilla(table_results_model_one_de_2021_months_2)
table_results_model_one_de_2021_months_2 <- width(table_results_model_one_de_2021_months_2, j = 1:4, width = c(0.9, 1.8, 1.8, 1.8), unit = "in")

flextable::save_as_image(x = table_results_model_one_de_2021_months_2, path = paste0("plots/germany/tables/", name_table_results_model_one_de_2021_months_2, ".png"))


