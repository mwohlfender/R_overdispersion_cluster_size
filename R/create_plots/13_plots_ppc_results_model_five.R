
# posterior predictive check of model 5
# create plots


# load data ----
data_parameters_post_pred <- read_csv(path_data_post_pred_model_five_parameters_grid)
results_post_pred <- read_csv(path_results_post_pred_model_five_ch_dk_de_processed)



# create plots ----

# list of months
months <- c("January", "February", "March", "April", "May", "June", "July", "August", "September", "October", "November", "December")

# countries
countries <- unique(data_parameters_post_pred$country)
n_countries <- length(countries)


for (cc in 1:n_countries) {
  
  # single months
  for (mm in 1:12) {
    
    data_plot <- results_post_pred |> filter(country == countries[cc], month == mm)
    
    plot_comparison_clusters_quantile_country_month <- ggplot(data = data_plot) +
      geom_ribbon(aes(x=size, ymin=quantile_2_5,ymax=quantile_97_5),fill="skyblue",alpha=.6) +
      geom_line(aes(x=size, y=frequency_data, colour="col_data"), linewidth=0.25) +
      scale_color_manual(name = NULL,
                         breaks = c("col_data", "col_2_5", "col_97_5"),
                         values = c("dodgerblue2", "firebrick2", "springgreen2"),
                         labels = c("Data", "simulation: 2.5% percentile", "simulation: 97.5% quantile")) +
      scale_fill_manual(name = NULL,
                        breaks = c("col_95_pred_int"),
                        values = c("skyblue"),
                        labels = c("95% Prediction interval")) +
      ggtitle(paste0(toupper(substr(countries[cc], 1, 1)), substr(countries[cc], 2, nchar(countries[cc])), ": ", months[mm])) +
      scale_y_log10() +
      xlab(label="Cluster size") +
      ylab(label="Frequency") +
      theme_bw() +
      theme(legend.position="bottom")
    
    path_plot <- paste0("plots/", countries[cc], "/posterior_predictive_check/model_five/plot_ppc_model_five_", countries[cc], "_", formatC(mm, width=2, flag="0"), ".png")
    
    ggsave(filename=path_plot, plot=plot_comparison_clusters_quantile_country_month, width=7, units="in")
    
  }
  
}



# whole year 2021 ----

# Switzerland ----
plot_comparison_clusters_ch_model_five_quantile_facet <- ggplot(data = results_post_pred |> filter(country=="switzerland")) +
  geom_ribbon(aes(x=size, ymin=quantile_2_5, ymax=quantile_97_5, fill="col_95_pred_int"), alpha=0.6) +
  geom_line(aes(x=size, y=frequency_data, colour="col_data"), linewidth=0.25) +  
  scale_color_manual(name = NULL,
                     breaks = c("col_data", "col_2_5", "col_97_5"),
                     values = c("dodgerblue2", "firebrick2", "springgreen2"),
                     labels = c("Data", "simulation: 2.5% quantile", "simulation: 97.5% quantile")) +
  scale_fill_manual(name = NULL,
                    breaks = c("col_95_pred_int"),
                    values = c("lightskyblue"),
                    labels = c("95% Prediction interval")) +
  # ggtitle("Switzerland") +
  scale_y_log10() +
  xlab(label="Cluster size") +
  ylab(label="Frequency") +
  theme_bw() +
  theme(legend.position="bottom") +
  facet_wrap(facets = vars(factor(month, levels = 1:12)), 
             scales = "free",
             nrow = 4,
             ncol = 3,
             labeller = as_labeller(c("1" = "January", "2" = "February", "3" = "March", "4" = "April",
                                      "5" = "May", "6" = "June", "7" = "July", "8" = "August",
                                      "9" = "September", "10" = "October", "11" = "November", "12" = "December")))

# ggsave(filename="plots/switzerland/posterior_predictive_check/model_five/plot_ppc_model_five_switzerland.png", plot=plot_comparison_clusters_ch_model_five_quantile_facet, width=7, units="in")


plot_comparison_clusters_ch_model_five_quantile_facet_with_title <- plot_comparison_clusters_ch_model_five_quantile_facet +
  ggtitle("Switzerland")

ggsave(filename="plots/switzerland/posterior_predictive_check/model_five/plot_ppc_model_five_switzerland_with_title.png", plot=plot_comparison_clusters_ch_model_five_quantile_facet_with_title, width=7, units="in")



# Denmark ----
plot_comparison_clusters_dk_model_five_quantile_facet <- ggplot(data = results_post_pred |> filter(country=="denmark")) +
  geom_ribbon(aes(x=size, ymin=quantile_2_5, ymax=quantile_97_5, fill="col_95_pred_int"), alpha=0.6) +
  geom_line(aes(x=size, y=frequency_data, colour="col_data"), linewidth=0.25) +  
  scale_color_manual(name = NULL,
                     breaks = c("col_data", "col_2_5", "col_97_5"),
                     values = c("dodgerblue2", "firebrick2", "springgreen2"),
                     labels = c("Data", "simulation: 2.5% quantile", "simulation: 97.5% quantile")) +
  scale_fill_manual(name = NULL,
                    breaks = c("col_95_pred_int"),
                    values = c("lightskyblue"),
                    labels = c("95% Prediction interval")) +
  scale_y_log10() +
  xlab(label="Cluster size") +
  ylab(label="Frequency") +
  theme_bw() +
  theme(legend.position="bottom") +
  facet_wrap(facets = vars(factor(month, levels = 1:12)), 
             scales = "free",
             nrow = 4,
             ncol = 3,
             labeller = as_labeller(c("1" = "January", "2" = "February", "3" = "March", "4" = "April",
                                      "5" = "May", "6" = "June", "7" = "July", "8" = "August",
                                      "9" = "September", "10" = "October", "11" = "November", "12" = "December")))

# ggsave(filename="plots/denmark/posterior_predictive_check/model_five/plot_ppc_model_five_denmark.png", plot=plot_comparison_clusters_dk_model_five_quantile_facet, width=7, units="in")


plot_comparison_clusters_dk_model_five_quantile_facet_with_title <- plot_comparison_clusters_dk_model_five_quantile_facet +
  ggtitle("Denmark")

ggsave(filename="plots/denmark/posterior_predictive_check/model_five/plot_ppc_model_five_denmark_with_title.png", plot=plot_comparison_clusters_dk_model_five_quantile_facet_with_title, width=7, units="in")



# Germany ----
plot_comparison_clusters_de_model_five_quantile_facet <- ggplot(data = results_post_pred |> filter(country=="germany")) +
  geom_ribbon(aes(x=size, ymin=quantile_2_5, ymax=quantile_97_5, fill="col_95_pred_int"), alpha=0.6) +
  geom_line(aes(x=size, y=frequency_data, colour="col_data"), linewidth=0.25) +
  scale_color_manual(name = NULL,
                     breaks = c("col_data", "col_2_5", "col_97_5"),
                     values = c("dodgerblue2", "firebrick2", "springgreen2"),
                     labels = c("Data", "simulation: 2.5% quantile", "simulation: 97.5% quantile")) +
  scale_fill_manual(name = NULL,
                    breaks = c("col_95_pred_int"),
                    values = c("lightskyblue"),
                    labels = c("95% Prediction interval")) +
  scale_y_log10() +
  xlab(label="Cluster size") +
  ylab(label="Frequency") +
  theme_bw() +
  theme(legend.position="bottom") +
  facet_wrap(facets = vars(factor(month, levels = 1:12)), 
             scales = "free",
             nrow = 4,
             ncol = 3,
             labeller = as_labeller(c("1" = "January", "2" = "February", "3" = "March", "4" = "April",
                                      "5" = "May", "6" = "June", "7" = "July", "8" = "August",
                                      "9" = "September", "10" = "October", "11" = "November", "12" = "December")))

# ggsave(filename="plots/germany/posterior_predictive_check/model_five/plot_ppc_model_five_germany.png", plot=plot_comparison_clusters_de_model_five_quantile_facet, width=7, units="in")


plot_comparison_clusters_de_model_five_quantile_facet_with_title <- plot_comparison_clusters_de_model_five_quantile_facet +
  ggtitle("Germany")

ggsave(filename="plots/germany/posterior_predictive_check/model_five/plot_ppc_model_five_germany_with_title.png", plot=plot_comparison_clusters_de_model_five_quantile_facet_with_title, width=7, units="in")


