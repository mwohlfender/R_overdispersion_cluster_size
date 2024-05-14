
# create data overview plots for all countries


# read and process data ----

# read variant data
data_variants_ch_dk_de <- read_csv(file = path_data_variants_shares_ch_dk_de_processed)


# read monthly number of clusters of different sizes during 2021
data_cluster_sizes_ch_months <- read_csv(file = path_data_cluster_sizes_ch_2021_months)
data_cluster_sizes_dk_months <- read_csv(file = path_data_cluster_sizes_dk_2021_months)
data_cluster_sizes_de_months <- read_csv(file = path_data_cluster_sizes_de_2021_months)

# convert to long format such that each row corresponds to a single cluster
data_ch_2021_rep <- as.data.frame(lapply(data_cluster_sizes_ch_months, rep, data_cluster_sizes_ch_months$frequency))
data_dk_2021_rep <- as.data.frame(lapply(data_cluster_sizes_dk_months, rep, data_cluster_sizes_dk_months$frequency))
data_de_2021_rep <- as.data.frame(lapply(data_cluster_sizes_de_months, rep, data_cluster_sizes_de_months$frequency))

# bind `data_ch_2021_rep`, `data_dk_2021_rep` and `data_de_2021_rep`
data_all_countries_2021 <- bind_rows(data_ch_2021_rep %>% mutate(country = "CH"), 
                                     data_dk_2021_rep %>% mutate(country = "DK"), 
                                     data_de_2021_rep %>% mutate(country = "DE"))

# create data summary for plot
summ_2021 <- data_all_countries_2021 %>%
  mutate(month=factor(month)) %>% 
  group_by(month,country) %>% 
  summarise(max_c=max(size),
            mean_c=mean(size),
            perc90=quantile(size,.9),
            perc95=quantile(size,.95),
            perc99=quantile(size,.99),
            num=n())

summ_2021_mean9099 <- summ_2021 %>% mutate(month = as.character(month),
                                           mean_c = as.character(mean_c),
                                           perc90 = as.character(perc90),
                                           perc99 = as.character(perc99)) %>% 
  dplyr::select(c("month", "country", "mean_c", "perc90", "perc99"))

summ_2021_mean9099_long <- gather(summ_2021_mean9099,
                                  key = "value", value = "cluster_size", mean_c, perc90, perc99) %>%
  mutate(cluster_size = as.numeric(cluster_size))


# read summary tables of the number of confirmed cases, the number of sequences and the number of clusters 
data_cases_sequences_clusters_ch_2021_months <- read_csv(file = path_data_cases_sequences_clusters_ch_2021_months)
data_cases_sequences_clusters_dk_2021_months <- read_csv(file = path_data_cases_sequences_clusters_dk_2021_months)
data_cases_sequences_clusters_de_2021_months <- read_csv(file = path_data_cases_sequences_clusters_de_2021_months)

# bind `data_cases_sequences_clusters_ch_2021_months`, `data_cases_sequences_clusters_dk_2021_months` and `data_cases_sequences_clusters_de_2021_months`
data_cases_clusters_ch_dk_de <- bind_rows(data_cases_sequences_clusters_ch_2021_months %>% filter(Month != "Total") %>% rename("month_name" = "Month") %>% mutate(month = 1:12, country = "Switzerland"), 
                                          data_cases_sequences_clusters_dk_2021_months %>% filter(Month != "Total") %>% rename("month_name" = "Month") %>% mutate(month = 1:12, country = "Denmark"), 
                                          data_cases_sequences_clusters_de_2021_months %>% filter(Month != "Total") %>% rename("month_name" = "Month") %>% mutate(month = 1:12, country = "Germany"))

data_results <- data_cases_clusters_ch_dk_de %>% mutate(model = "A", offset = 0.0)



# create plots ----
color_scale_values <- c("black")
color_scale_labels <- c("Model 1")
ratio_width_height <- 4/3
image_width_in <- 7.3
list_months <- c("Jan", "Feb", "Mar", "Apr", "May", "Jun", "Jul", "Aug", "Sep", "Oct", "Nov", "Dec")
colors_variants <- paletteer_c("viridis::plasma", n = 4)



# size of largest cluster ----
plot_title <- "Size of largest cluster during 2021"

# without variants
plot_results_size_largest_cluster_all_countries <- create_plot_result_all_countries(data_plot_results = data_results,
                                                                                    data_estimate = data_results$Solc,
                                                                                    data_lower_cred_int = data_results$Solc,
                                                                                    data_upper_cred_int = data_results$Solc,
                                                                                    plot_color_scale_values = color_scale_values, 
                                                                                    plot_color_scale_labels = color_scale_labels,
                                                                                    scale_y_from = 0,
                                                                                    scale_y_to = 1500,
                                                                                    scale_y_by = 150,
                                                                                    add_line_at_one = FALSE,
                                                                                    label_y = "Size of largest cluster")

# ggsave(filename = "plots/all_countries/data/plot_results_size_largest_cluster.png",
#        plot = plot_results_size_largest_cluster_all_countries, width = image_width_in, height = image_width_in / ratio_width_height, units = "in")


plot_results_size_largest_cluster_all_countries_with_title <- plot_results_size_largest_cluster_all_countries +
  ggtitle(plot_title)

ggsave(filename = "plots/all_countries/data/plot_results_size_largest_cluster_with_title.png",
       plot = plot_results_size_largest_cluster_all_countries_with_title, width = image_width_in, height = image_width_in / ratio_width_height, units = "in")


# with variants
plot_results_size_largest_cluster_variants_all_countries <- create_plot_result_variants_all_countries(data_plot_results = data_results,
                                                                                                      data_plot_variants = data_variants_ch_dk_de,
                                                                                                      data_estimate = data_results$Solc,
                                                                                                      data_lower_cred_int = data_results$Solc,
                                                                                                      data_upper_cred_int = data_results$Solc,
                                                                                                      plot_color_scale_values = color_scale_values, 
                                                                                                      plot_color_scale_labels = color_scale_labels,
                                                                                                      scale_y_from = 0,
                                                                                                      scale_y_to = 1500,
                                                                                                      scale_y_by = 150,
                                                                                                      add_line_at_one = FALSE,
                                                                                                      label_y = "Size of largest cluster")

# ggsave(filename = "plots/all_countries/data/plot_results_size_largest_cluster_variants.png",
#        plot = plot_results_size_largest_cluster_variants_all_countries, width = image_width_in, height = image_width_in / ratio_width_height, units = "in")


plot_results_size_largest_cluster_variants_all_countries_with_title <- plot_results_size_largest_cluster_variants_all_countries +
  ggtitle(plot_title)

ggsave(filename = "plots/all_countries/data/plot_results_size_largest_cluster_variants_with_title.png",
       plot = plot_results_size_largest_cluster_variants_all_countries_with_title, width = image_width_in, height = image_width_in / ratio_width_height, units = "in")



# number of clusters ----
plot_title <- "Number of clusters during 2021"

# without variants
plot_results_n_clusters_all_countries <- create_plot_result_all_countries(data_plot_results = data_results,
                                                                          data_estimate = data_results$Noc,
                                                                          data_lower_cred_int = data_results$Noc,
                                                                          data_upper_cred_int = data_results$Noc,
                                                                          plot_color_scale_values = color_scale_values, 
                                                                          plot_color_scale_labels = color_scale_labels,
                                                                          scale_y_from = 0,
                                                                          scale_y_to = 40000,
                                                                          scale_y_by = 2000,
                                                                          add_line_at_one = FALSE,
                                                                          label_y = "Number of clusters")

# ggsave(filename = "plots/all_countries/data/plot_results_n_clusters.png",
#        plot = plot_results_n_clusters_all_countries, width = image_width_in, height = image_width_in / ratio_width_height, units = "in")


plot_results_n_clusters_all_countries_with_title <- plot_results_n_clusters_all_countries +
  ggtitle(plot_title)

ggsave(filename = "plots/all_countries/data/plot_results_n_clusters_with_title.png",
       plot = plot_results_n_clusters_all_countries_with_title, width = image_width_in, height = image_width_in / ratio_width_height, units = "in")


# with variants
plot_results_n_clusters_variants_all_countries <- create_plot_result_variants_all_countries(data_plot_results = data_results,
                                                                                            data_plot_variants = data_variants_ch_dk_de,
                                                                                            data_estimate = data_results$Noc,
                                                                                            data_lower_cred_int = data_results$Noc,
                                                                                            data_upper_cred_int = data_results$Noc,
                                                                                            plot_color_scale_values = color_scale_values, 
                                                                                            plot_color_scale_labels = color_scale_labels,
                                                                                            scale_y_from = 0,
                                                                                            scale_y_to = 40000,
                                                                                            scale_y_by = 2000,
                                                                                            add_line_at_one = FALSE,
                                                                                            label_y = "Number of clusters")

# ggsave(filename = "plots/all_countries/data/plot_results_n_clusters_variants.png",
#        plot = plot_results_n_clusters_variants_all_countries, width = image_width_in, height = image_width_in / ratio_width_height, units = "in")


plot_results_n_clusters_variants_all_countries_with_title <- plot_results_n_clusters_variants_all_countries +
  ggtitle(plot_title)

ggsave(filename = "plots/all_countries/data/plot_results_n_clusters_variants_with_title.png",
       plot = plot_results_n_clusters_variants_all_countries_with_title, width = image_width_in, height = image_width_in / ratio_width_height, units = "in")



# sequencing probability ----
plot_title <- "Sequencing probability during 2021"

# without variants
plot_results_sequencing_probability_all_countries <- create_plot_result_all_countries(data_plot_results = data_results,
                                                                                      data_estimate = data_results$`Nos/Nocc`,
                                                                                      data_lower_cred_int = data_results$`Nos/Nocc`,
                                                                                      data_upper_cred_int = data_results$`Nos/Nocc`,
                                                                                      plot_color_scale_values = color_scale_values, 
                                                                                      plot_color_scale_labels = color_scale_labels,
                                                                                      scale_y_from = 0,
                                                                                      scale_y_to = 1,
                                                                                      scale_y_by = 0.1,
                                                                                      add_line_at_one = FALSE,
                                                                                      label_y = "Sequencing probability")

# ggsave(filename = "plots/all_countries/data/plot_results_sequencing_probability.png",
#        plot = plot_results_sequencing_probability_all_countries, width = image_width_in, height = image_width_in / ratio_width_height, units = "in")


plot_results_sequencing_probability_all_countries_with_title <- plot_results_sequencing_probability_all_countries +
  ggtitle(plot_title)

ggsave(filename = "plots/all_countries/data/plot_results_sequencing_probability_with_title.png",
       plot = plot_results_sequencing_probability_all_countries_with_title, width = image_width_in, height = image_width_in / ratio_width_height, units = "in")


# with variants
plot_results_sequencing_probability_variants_all_countries <- create_plot_result_variants_all_countries(data_plot_results = data_results,
                                                                                                        data_plot_variants = data_variants_ch_dk_de,
                                                                                                        data_estimate = data_results$`Nos/Nocc`,
                                                                                                        data_lower_cred_int = data_results$`Nos/Nocc`,
                                                                                                        data_upper_cred_int = data_results$`Nos/Nocc`,
                                                                                                        plot_color_scale_values = color_scale_values, 
                                                                                                        plot_color_scale_labels = color_scale_labels,
                                                                                                        scale_y_from = 0,
                                                                                                        scale_y_to = 1,
                                                                                                        scale_y_by = 0.1,
                                                                                                        add_line_at_one = FALSE,
                                                                                                        label_y = "Sequencing probability")

# ggsave(filename = "plots/all_countries/data/plot_results_sequencing_probability_variants.png",
#        plot = plot_results_sequencing_probability_variants_all_countries, width = image_width_in, height = image_width_in / ratio_width_height, units = "in")


plot_results_sequencing_probability_variants_all_countries_with_title <- plot_results_sequencing_probability_variants_all_countries +
  ggtitle(plot_title)

ggsave(filename = "plots/all_countries/data/plot_results_sequencing_probability_variants_with_title.png",
       plot = plot_results_sequencing_probability_variants_all_countries_with_title, width = image_width_in, height = image_width_in / ratio_width_height, units = "in")



# overview of cluster data ----
plot_violin_clusters_2021 <- ggplot(data=data_all_countries_2021) +
  # geom_violin(aes(x=factor(month), y=size), fill="dodgerblue", alpha=.6) +
  geom_segment(data = summ_2021, aes(x = factor(month), xend = factor(month), y = 1, yend = max_c)) +
  geom_point(data=summ_2021_mean9099_long,
             aes(x=factor(month), y=cluster_size, shape=factor(value, levels = c("n", unique(summ_2021_mean9099_long$value)))), 
             fill="firebrick2",
             size=2.0,
             show.legend = TRUE) +
  geom_text(data=summ_2021, aes(x=factor(month), y=0.75, label=paste0(num), angle=45), size=2.8) +
  scale_shape_manual(name = NULL,
                     breaks = c("n", "mean_c", "perc90", "perc99"),
                     values = c(utf8ToInt("n"), 21, 23, 24),
                     labels = c("Number of clusters", "Mean cluster size", "90th percentile", "99th percentile"),
                     drop = FALSE) +
  scale_x_discrete(expand=expansion(c(0.10, 0.10)),
                   labels = c("Jan", "Feb", "Mar", "Apr", "May", "Jun", "Jul", "Aug", "Sep", "Oct", "Nov", "Dec")) +
  scale_y_log10(expand=expansion(c(0.05, 0.05)),
                breaks=c(.75, 1, 10, 100, 1000),
                minor_breaks=NULL,
                labels=c("n=","1","10","100","1000")) +
  xlab("Month") +
  ylab("Cluster size") +
  theme_bw() +
  theme(axis.text.x=element_text(angle=45, hjust=1),
        legend.position="bottom") +
  guides(shape = guide_legend(override.aes = list(size = c(5,4,4,4)))) +
  facet_grid(cols = vars(factor(country, levels=c("CH", "DK", "DE"), labels=c("Switzerland", "Denmark", "Germany"))))

ggsave(filename="plots/all_countries/data/plot_violin_clusters_2021.png",
       plot=plot_violin_clusters_2021, width=image_width_in, height=image_width_in/ratio_width_height, units="in") 



# plot grid of overview of cluster data and sequencing probabilities ----
plot_grid_clusters_seq_proba <- plot_grid(plot_violin_clusters_2021,
                                          plot_results_sequencing_probability_variants_all_countries,
                                          labels = c("A", "B"),
                                          nrow = 2,
                                          align = 'v')

ggsave(plot = plot_grid_clusters_seq_proba,
       filename = "plots/paper/figure_cluster_dist.pdf",
       width = 7.3, height = 9, units = c("in"))

