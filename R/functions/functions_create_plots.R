


# define useful functions ----
create_h_lines <- function(y_major_start, y_major_end, y_major_step, y_minor_start, y_minor_end, y_minor_step) {
  
  list_hlines <- list()
  
  for (ii in seq(from = y_major_start, to = y_major_end, by = y_major_step)) {
    
    list_hlines <- append(list_hlines, geom_hline(yintercept = ii, color = "grey90", linewidth = 0.5))
    
  }
  
  for (ii in seq(from = y_minor_start, to = y_minor_end, by = y_minor_step)) {
    
    list_hlines <- append(list_hlines, geom_hline(yintercept = ii, color = "grey90", linewidth = 0.2))
    
  }
  
  return(list_hlines)
  
}



create_v_lines <- function(x_major_start, x_major_end, x_major_step, x_minor_start, x_minor_end, x_minor_step) {
  
  list_vlines <- list()
  
  for (ii in seq(from = x_major_start, to = x_major_end, by = x_major_step)) {
    
    list_vlines <- append(list_vlines, geom_vline(xintercept = ii, color = "grey90", linewidth = 0.5))
    
  }
  
  for (ii in seq(from = x_minor_start, to = x_minor_end, by = x_minor_step)) {
    
    list_vlines <- append(list_vlines, geom_vline(xintercept = ii, color = "grey90", linewidth = 0.2))
    
  }
  
  return(list_vlines)
  
}



data_h_line_one <- tibble(x = c(-0.25, 12.25),
                          y = c(1, 1))



create_plot_result <- function(data_plot_results, data_plot_other_estimates = NULL, data_estimate, data_lower_cred_int, data_upper_cred_int, plot_color_scale_values, plot_color_scale_labels, scale_y_from, scale_y_to, scale_y_by, add_line_at_one, label_y) {
  
  plot_result <- ggplot() +
    { if (!(is.null(data_plot_other_estimates)))
      geom_line(data = data_plot_other_estimates,
                mapping = aes(x = x, y = value, color = estimate_type, group = estimate_type),
                linewidth = 0.8) } +
    # { if (!(is.null(data_plot_other_estimates)))
    #   geom_ribbon(data=data_plot_other_estimates,
    #               mapping = aes(x = x, ymin = boundary_low, ymax = boundary_high, color = estimate_type, fill = estimate_type), alpha = 0.3)} +
    geom_point(data = data_plot_results,
               mapping = aes(x = month + offset - 0.5, y = data_estimate, color = model),
               shape = 16,
               size = 1.5) +
    geom_line(data = data_plot_results,
              mapping = aes(x = month + offset - 0.5, y = data_estimate, color = model),
              linetype = "dotted",
              linewidth = 0.5) +
    geom_errorbar(data = data_plot_results,
                  mapping = aes(x = month + offset - 0.5, y = data_estimate, ymin = data_lower_cred_int, ymax = data_upper_cred_int, color = model),
                  linewidth = 0.5,
                  width  = 0.02) +
    { if (add_line_at_one)
      geom_line(data = data_h_line_one,
                mapping = aes(x = x, y = y),
                color = "black",
                linetype = "dashed",
                linewidth = 0.5)} +
    { if (is.null(data_plot_other_estimates))
      scale_color_manual(name = "Method:",
                         breaks = sort(unique(data_plot_results$model)),
                         values = plot_color_scale_values,
                         labels = plot_color_scale_labels)} +
    { if (!(is.null(data_plot_other_estimates)))
      scale_color_manual(name = "Method:",
                         breaks = c(sort(unique(data_plot_results$model)), sort(unique(data_plot_other_estimates$estimate_type))),
                         values = plot_color_scale_values,
                         labels = plot_color_scale_labels)} +
    scale_x_continuous(breaks = seq(from = 0.5, to = 11.5, by = 1),
                       expand = c(0, 0),
                       labels = list_months,
                       limits = c(-0.5, 12.5)) +
    scale_y_continuous(breaks = seq(from = scale_y_from, to = scale_y_to, by = scale_y_by),
                       expand = c(0, 0),
                       labels = seq(from = scale_y_from, to = scale_y_to, by = scale_y_by),
                       limits = c(scale_y_from - scale_y_by/2, scale_y_to + scale_y_by/2)) +
    xlab("Time") +
    ylab(label_y) +
    theme_bw() +
    theme(axis.text.x = element_text(angle = 45, hjust = 1),
          legend.position = "bottom",
          legend.box = "horizontal",
          legend.margin = margin(t = 0, r = 0, b = 0, l = 0, unit = "cm"),
          legend.key = element_rect(fill = "white")) +
    { if (length(unique(data_plot_results$model)) == 1 & is.null(data_plot_other_estimates)) guides(color = "none") } +
    { if (length(unique(data_plot_results$model)) >= 2 | !(is.null(data_plot_other_estimates))) guides(color =  guide_legend(order = 1)) }
  
  return(plot_result)
  
}



create_plot_result_variants <- function(data_plot_results, data_plot_variants, data_plot_other_estimates = NULL, data_estimate, data_lower_cred_int, data_upper_cred_int, plot_color_scale_values, plot_color_scale_labels, scale_y_from, scale_y_to, scale_y_by, add_line_at_one, label_y) {
  
  plot_result_variants <- ggplot() +
    geom_area(data = data_plot_variants, mapping = aes(x = months_start_time_float, y = scale_y_to, fill = "col4", group = country)) +
    geom_area(data = data_plot_variants, mapping = aes(x = months_start_time_float, y = scale_y_to*(Alpha + Delta + Omicron), fill = "col3", group = country)) +
    geom_area(data = data_plot_variants, mapping = aes(x = months_start_time_float, y = scale_y_to*(Alpha + Delta), fill = "col2", group = country)) +
    geom_area(data = data_plot_variants, mapping = aes(x = months_start_time_float, y = scale_y_to*Alpha, fill = "col1", group = country)) +
    scale_fill_manual(name = "Variants:",
                      values = c("col1" = lighten(colors_variants[4], amount = 0.6),
                                 "col2" = lighten(colors_variants[3], amount = 0.6),
                                 "col3" = lighten(colors_variants[2], amount = 0.6),
                                 "col4" = lighten(colors_variants[1], amount = 0.6)),
                      labels = c("Alpha", "Delta", "Omicron", "Others")) +
    geom_rect(data = data_plot_variants[1,], mapping = aes(xmin = -0.5, xmax = 0, ymin = scale_y_from, ymax = scale_y_to), fill = "white", color = "white") +
    geom_rect(data = data_plot_variants[1,], mapping = aes(xmin = 12, xmax = 12.5, ymin = scale_y_from, ymax = scale_y_to), fill = "white", color = "white") +
    create_v_lines(x_major_start = 0.5, x_major_end = 11.5, x_major_step = 1.0, x_minor_start = 0.0, x_minor_end = 12.0, x_minor_step = 1.0) +
    create_h_lines(y_major_start = scale_y_from, y_major_end = scale_y_to, y_major_step = scale_y_by, y_minor_start = scale_y_from + scale_y_by/2, y_minor_end = scale_y_to - scale_y_by/2, y_minor_step = scale_y_by) +
    { if (!(is.null(data_plot_other_estimates)))
      geom_line(data = data_plot_other_estimates,
                mapping = aes(x = x, y = value, color = estimate_type, group = estimate_type),
                linewidth = 0.8)} +
    geom_point(data = data_plot_results,
               mapping = aes(x = month + offset - 0.5, y = data_estimate, color = model),
               shape = 16,
               size = 1.5) +
    geom_line(data = data_plot_results,
              mapping = aes(x = month + offset - 0.5, y = data_estimate, color = model),
              linetype = "dotted",
              linewidth = 0.5) +
    geom_errorbar(data = data_plot_results,
                  mapping = aes(x = month + offset - 0.5, y = data_estimate,
                                ymin = data_lower_cred_int, ymax = data_upper_cred_int,
                                color = model),
                  linewidth = 0.5,
                  width  = 0.02) +
    { if (add_line_at_one)
      geom_line(data = data_h_line_one,
                mapping = aes(x = x, y = y),
                color = "black",
                linetype = "dashed",
                linewidth = 0.5)} +
    { if (is.null(data_plot_other_estimates))
      scale_color_manual(name = "Method:",
                         breaks = sort(unique(data_plot_results$model)),
                         values = plot_color_scale_values,
                         labels = plot_color_scale_labels)} +
    { if (!(is.null(data_plot_other_estimates)))
      scale_color_manual(name = "Method:",
                         breaks = c(sort(unique(data_plot_results$model)), sort(unique(data_plot_other_estimates$estimate_type))),
                         values = plot_color_scale_values,
                         labels = plot_color_scale_labels)} +
    scale_x_continuous(breaks = seq(from = 0.5, to = 11.5, by = 1),
                       expand = c(0, 0),
                       labels = list_months,
                       limits = c(-0.5, 12.5)) +
    scale_y_continuous(breaks = seq(from = scale_y_from, to = scale_y_to, by = scale_y_by),
                       expand = c(0, 0),
                       labels = seq(from = scale_y_from, to = scale_y_to, by = scale_y_by),
                       limits = c(scale_y_from - scale_y_by/2, scale_y_to + scale_y_by/2),
                       sec.axis = sec_axis(~ . / scale_y_to, name = "Proportion of variant", 
                                           breaks = seq(from = 0, to = 1, by = 0.2),
                                           labels = seq(from = 0, to = 1, by = 0.2))) +
    xlab("Time") +
    ylab(label_y) +
    theme_bw() +
    theme(axis.text.x = element_text(angle = 45, hjust = 1),
          legend.position = "bottom",
          legend.box = "vertical",
          legend.margin = margin(t = 0, r = 0, b = 0, l = 0, unit = "cm"),
          legend.key = element_rect(fill = "white")) +
    { if (length(unique(data_plot_results$model)) == 1 & is.null(data_plot_other_estimates)) guides(color = "none", fill = guide_legend(order = 1)) } +
    { if (length(unique(data_plot_results$model)) >= 2 | !(is.null(data_plot_other_estimates))) guides(color =  guide_legend(order = 1), fill = guide_legend(order = 2)) }
  
  return(plot_result_variants)
  
}



create_plot_result_all_countries <- function(data_plot_results, data_plot_other_estimates = NULL, data_estimate, data_lower_cred_int, data_upper_cred_int, plot_color_scale_values, plot_color_scale_labels, scale_y_from, scale_y_to, scale_y_by, add_line_at_one, label_y) {
  
  plot_result <- ggplot() +
    { if (!(is.null(data_plot_other_estimates)))
      geom_line(data = data_plot_other_estimates,
                mapping = aes(x = x, y = value, color = estimate_type, group = estimate_type),
                linewidth = 0.8)} +
    geom_point(data = data_plot_results,
               mapping = aes(x = month + offset - 0.5, y = data_estimate, color = model),
               shape = 16,
               size = 1.5) +
    geom_line(data = data_plot_results,
              mapping = aes(x = month + offset - 0.5, y = data_estimate, color = model),
              linetype = "dotted",
              linewidth = 0.5) +
    geom_errorbar(data = data_plot_results,
                  mapping = aes(x = month + offset - 0.5, y = data_estimate, ymin = data_lower_cred_int, ymax = data_upper_cred_int, color = model),
                  linewidth = 0.5,
                  width  = 0.02) +
    { if (add_line_at_one)
      geom_line(data = data_h_line_one,
                mapping = aes(x = x, y = y),
                color = "black",
                linetype = "dashed",
                linewidth = 0.5)} +
    { if (is.null(data_plot_other_estimates))
      scale_color_manual(name = "Method:",
                         breaks = sort(unique(data_plot_results$model)),
                         values = plot_color_scale_values,
                         labels = plot_color_scale_labels)} +
    { if (!(is.null(data_plot_other_estimates)))
      scale_color_manual(name = "Method:",
                         breaks = c(sort(unique(data_plot_results$model)), sort(unique(data_plot_other_estimates$estimate_type))),
                         values = plot_color_scale_values,
                         labels = plot_color_scale_labels)} +
    scale_x_continuous(breaks = seq(from = 0.5, to = 11.5, by = 1),
                       expand = c(0, 0),
                       labels = list_months,
                       limits = c(-0.5, 12.5)) +
    scale_y_continuous(breaks = seq(from = scale_y_from, to = scale_y_to, by = scale_y_by),
                       expand = c(0, 0),
                       labels = seq(from = scale_y_from, to = scale_y_to, by = scale_y_by),
                       limits = c(scale_y_from - scale_y_by/2, scale_y_to + scale_y_by/2)) +
    xlab("Time") +
    ylab(label_y) +
    theme_bw() +
    theme(axis.text.x = element_text(angle = 45, hjust = 1),
          legend.position = "bottom",
          legend.box = "horizontal",
          legend.margin = margin(t = 0, r = 0, b = 0, l = 0, unit = "cm"),
          legend.key = element_rect(fill = "white")) +
    { if (length(unique(data_plot_results$model)) == 1 & is.null(data_plot_other_estimates)) guides(color = "none") } +
    { if (length(unique(data_plot_results$model)) >= 2 | !(is.null(data_plot_other_estimates))) guides(color =  guide_legend(order = 1)) } +
    facet_grid(cols = vars(factor(x = country, levels = c("Switzerland", "Denmark", "Germany"))))
  
  return(plot_result)
  
}



create_plot_result_variants_all_countries <- function(data_plot_results, data_plot_variants, data_plot_other_estimates = NULL, data_estimate, data_lower_cred_int, data_upper_cred_int, plot_color_scale_values, plot_color_scale_labels, scale_y_from, scale_y_to, scale_y_by, add_line_at_one, label_y) {
  
  plot_result_variants <- ggplot() +
    geom_area(data = data_plot_variants, mapping = aes(x = months_start_time_float, y = scale_y_to, fill = "col4", group = country)) +
    geom_area(data = data_plot_variants, mapping = aes(x = months_start_time_float, y = scale_y_to*(Alpha + Delta + Omicron), fill = "col3", group = country)) +
    geom_area(data = data_plot_variants, mapping = aes(x = months_start_time_float, y = scale_y_to*(Alpha + Delta), fill = "col2", group = country)) +
    geom_area(data = data_plot_variants, mapping = aes(x = months_start_time_float, y = scale_y_to*Alpha, fill = "col1", group = country)) +
    scale_fill_manual(name = "Variants:",
                      values = c("col1" = lighten(colors_variants[4], amount = 0.6),
                                 "col2" = lighten(colors_variants[3], amount = 0.6),
                                 "col3" = lighten(colors_variants[2], amount = 0.6),
                                 "col4" = lighten(colors_variants[1], amount = 0.6)),
                      labels = c("Alpha", "Delta", "Omicron", "Others")) +
    geom_rect(data = data_plot_variants[1,], mapping = aes(xmin = -0.5, xmax = 0, ymin = scale_y_from, ymax = scale_y_to), fill = "white", color = "white") +
    geom_rect(data = data_plot_variants[1,], mapping = aes(xmin = 12, xmax = 12.5, ymin = scale_y_from, ymax = scale_y_to), fill = "white", color = "white") +
    create_v_lines(x_major_start = 0.5, x_major_end = 11.5, x_major_step = 1.0, x_minor_start = 0.0, x_minor_end = 12.0, x_minor_step = 1.0) +
    create_h_lines(y_major_start = scale_y_from, y_major_end = scale_y_to, y_major_step = scale_y_by, y_minor_start = scale_y_from + scale_y_by/2, y_minor_end = scale_y_to - scale_y_by/2, y_minor_step = scale_y_by) +
    { if (!(is.null(data_plot_other_estimates)))
      geom_line(data = data_plot_other_estimates,
                mapping = aes(x = x, y = value, color = estimate_type, group = estimate_type),
                linewidth = 0.8)} +
    geom_point(data = data_plot_results,
               mapping = aes(x = month + offset - 0.5, y = data_estimate, color = model),
               shape = 16,
               size = 1.5) +
    geom_line(data = data_plot_results,
              mapping = aes(x = month + offset - 0.5, y = data_estimate, color = model),
              linetype = "dotted",
              linewidth = 0.5) +
    geom_errorbar(data = data_plot_results,
                  mapping = aes(x = month + offset - 0.5, y = data_estimate,
                                ymin = data_lower_cred_int, ymax = data_upper_cred_int,
                                color = model),
                  linewidth = 0.5,
                  width  = 0.02) +
    { if (add_line_at_one)
      geom_line(data = data_h_line_one,
                mapping = aes(x = x, y = y),
                color = "black",
                linetype = "dashed",
                linewidth = 0.5)} +
    { if (is.null(data_plot_other_estimates))
      scale_color_manual(name = "Method:",
                         breaks = sort(unique(data_plot_results$model)),
                         values = plot_color_scale_values,
                         labels = plot_color_scale_labels)} +
    { if (!(is.null(data_plot_other_estimates)))
      scale_color_manual(name = "Method:",
                         breaks = c(sort(unique(data_plot_results$model)), sort(unique(data_plot_other_estimates$estimate_type))),
                         values = plot_color_scale_values,
                         labels = plot_color_scale_labels)} +
    scale_x_continuous(breaks = seq(from = 0.5, to = 11.5, by = 1),
                       expand = c(0, 0),
                       labels = list_months,
                       limits = c(-0.5, 12.5)) +
    scale_y_continuous(breaks = seq(from = scale_y_from, to = scale_y_to, by = scale_y_by),
                       expand = c(0, 0),
                       labels = seq(from = scale_y_from, to = scale_y_to, by = scale_y_by),
                       limits = c(scale_y_from - scale_y_by/2, scale_y_to + scale_y_by/2),
                       sec.axis = sec_axis(~ . / scale_y_to, name = "Proportion of variant", 
                                           breaks = seq(from = 0, to = 1, by = 0.2),
                                           labels = seq(from = 0, to = 1, by = 0.2))) +
    xlab("Time") +
    ylab(label_y) +
    theme_bw() +
    theme(axis.text.x = element_text(angle = 45, hjust = 1),
          legend.position = "bottom",
          legend.box = "vertical",
          legend.margin = margin(t = 0, r = 0, b = 0, l = 0, unit = "cm"),
          legend.key = element_rect(fill = "white")) +
    { if (length(unique(data_plot_results$model)) == 1 & is.null(data_plot_other_estimates)) guides(color = "none", fill = guide_legend(order = 1)) } +
    { if (length(unique(data_plot_results$model)) >= 2 | !(is.null(data_plot_other_estimates))) guides(color =  guide_legend(order = 1), fill = guide_legend(order = 2)) } +
    facet_grid(cols = vars(factor(x = country, levels = c("Switzerland", "Denmark", "Germany"))))
  
  return(plot_result_variants)
  
}


