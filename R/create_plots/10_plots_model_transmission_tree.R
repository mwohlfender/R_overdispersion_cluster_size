
# create figure: model of a transmission tree and illustration of the concept of R_g



# define parameters for plots
ratio_width_height <- 1
image_width_in <- 6


# define graph ----

# define set of nodes
nodes_plot_R_g <- data.frame(node_key = c(1,2,3,4,5,6,7,8,9,10,11,12,13,14),
                             mutation_occured = c(0,0,1,0,1,0,0,1,0,0,0,1,0,0),
                             variant_received = c(0,0,0,0,0,3,5,5,5,5,5,5,3,3),
                             variant_after_mutation = c(1:14),
                             current_variant = c(0,0,3,0,5,3,5,8,5,5,5,12,3,3),
                             detection = c(1,1,0,0,1,1,0,1,0,1,1,0,0,1))


# define set of edges
edges_plot_R_g <- data.frame(from = c(1,1,2,2,3,5,5,5,5,5,5,6,6),
                             to = c(2:14))

edges_plot_R_g$variant_transmitted <- nodes_plot_R_g$variant_received[edges_plot_R_g$to]
edges_plot_R_g$from_variant <- nodes_plot_R_g$current_variant[edges_plot_R_g$from]
edges_plot_R_g$to_variant <- nodes_plot_R_g$current_variant[edges_plot_R_g$to]
edges_plot_R_g$from_detected <- nodes_plot_R_g$detection[edges_plot_R_g$from]
edges_plot_R_g$to_detected <- nodes_plot_R_g$detection[edges_plot_R_g$to]


# define graph
graph_R_g <- tbl_graph(nodes=nodes_plot_R_g, 
                       edges=edges_plot_R_g,
                       directed=TRUE)


# define colors
colors_plot_R_g <- paletteer_c("viridis::plasma", n=(sum(nodes_plot_R_g %>% dplyr::select("mutation_occured"))+1))

# add x and y coordinate of nodes in column "from"
edges_plot_R_g <- edges_plot_R_g %>% mutate(node_key = from) %>% left_join(create_layout(graph_R_g, layout="tree")  %>% dplyr::select(c("node_key", "x", "y"))) %>% rename(x_from = x, y_from = y) %>% dplyr::select(-"node_key")

# add x and y coordinate of nodes in column "to"
edges_plot_R_g <- edges_plot_R_g %>% mutate(node_key = to) %>% left_join(create_layout(graph_R_g, layout="tree")  %>% dplyr::select(c("node_key", "x", "y"))) %>% rename(x_to = x, y_to = y) %>% dplyr::select(-"node_key")

# add column "edge.id" 
edges_plot_R_g <- edges_plot_R_g %>% mutate(edge.id = 1:nrow(edges_plot_R_g))

# add column "in_same_cluster": 1 if both nodes of an edge belong to the same identical sequence cluster, 0 if not 
edges_plot_R_g <- edges_plot_R_g %>% mutate(in_same_cluster = as.numeric(from_variant == to_variant))



# first version of plot: without mutation and without detection ----
plot_R_g <- ggraph(graph=graph_R_g, layout="tree") + 
  geom_edge_link(data=edges_plot_R_g, mapping=aes(x=x_from, y=y_from, xend=x_to, yend=y_to, edge_colour=factor(variant_transmitted), edge_linetype=factor(in_same_cluster), edge_width=factor(in_same_cluster))) +
  scale_edge_color_manual(name="Identical sequence cluster",
                          values=colors_plot_R_g,
                          labels=LETTERS[seq(from = 1, to = length(unique(edges_plot_R_g$variant_transmitted)))],
                          guide="none") +
  scale_discrete_manual(aesthetics=c("edge_width"),
                        name="Transmission",
                        values=c("0"=0.5, "1"=1),
                        labels=c("Not within same cluster", "Within same cluster"),
                        limits=c("1", "0"),
                        guide="none") +
  scale_discrete_manual(aesthetics = c("edge_linetype"),
                        name="Transmission:",
                        values=c("1"="solid", "0"="dashed"),
                        labels=c("Within same cluster", "Not within same cluster"),
                        limits=c("1", "0")) +
  geom_node_point(data=create_layout(graph_R_g, layout="tree"), mapping=aes(x=x, y=y, color=factor(current_variant)), size=2.5, shape=19, fill="white") +
  scale_color_manual(name="Identical sequence cluster:",
                     values=colors_plot_R_g,
                     labels=LETTERS[seq(from=1, to=length(colors_plot_R_g))]) +
  theme(panel.background=element_rect(fill="white", colour="black")) +
  theme(legend.position="bottom",
        legend.box="vertical",
        legend.margin=margin(t=0, r=0, b=0, l=0, unit="cm"),
        legend.key=element_blank()) +
  guides(colour=guide_legend(order=1),
         edge_linetype=guide_legend(order=2))

ggsave(filename=paste0("plots/branching_process/plot_R_g.png"),
       plot=plot_R_g, width=image_width_in, height = image_width_in/ratio_width_height, units="in")



# second version of plot: with mutation and without detection ----
plot_R_g_mutation <- ggraph(graph=graph_R_g, layout="tree") + 
  geom_edge_link(data=edges_plot_R_g, mapping=aes(x=x_from, y=y_from, xend=x_to, yend=y_to, edge_colour=factor(variant_transmitted), edge_linetype=factor(in_same_cluster), edge_width=factor(in_same_cluster))) +
  scale_edge_color_manual(name="Identical sequence cluster",
                          values=colors_plot_R_g,
                          labels=LETTERS[seq(from = 1, to = length(unique(edges_plot_R_g$variant_transmitted)))],
                          guide="none") +
  scale_discrete_manual(aesthetics=c("edge_width"),
                        name="Transmission",
                        values=c("0"=0.5, "1"=1),
                        labels=c("Not within same cluster", "Within same cluster"),
                        limits=c("1", "0"),
                        guide="none") +
  scale_discrete_manual(aesthetics = c("edge_linetype"),
                        name="Transmission:",
                        values=c("1"="solid", "0"="dashed"),
                        labels=c("Within same cluster", "Not within same cluster"),
                        limits=c("1", "0")) +
  geom_node_point(data=create_layout(graph_R_g, layout="tree"), 
                  mapping=aes(x=x, y=y, color=factor(current_variant), shape=factor(mutation_occured), size=factor(mutation_occured))) +
  scale_color_manual(name="Identical sequence cluster:",
                     values=colors_plot_R_g,
                     labels=LETTERS[seq(from=1, to=length(colors_plot_R_g))]) +
  scale_shape_manual(name="Mutation:",
                     breaks=c("1", "0"),
                     values=c(15, 19),
                     labels=c("Mutation occurred", "No mutation occurred")) +
  scale_fill_manual(name="Cluster",
                    breaks=c("0", as.character(unique(nodes_plot_R_g$current_variant+1))),
                    values = c("white", colors_plot_R_g),
                    guide="none") +
  scale_discrete_manual(aesthetics = c("size"),
                        name="Mutation:",
                        breaks=c("1", "0"),
                        values=c(3.5, 2.5),
                        labels=c("Mutation occurred", "No mutation occurred"),
                        guide="none") +
  theme(panel.background=element_rect(fill="white", colour="black")) +
  theme(legend.position="bottom",
        legend.box="vertical",
        legend.margin=margin(t=0, r=0, b=0, l=0, unit="cm"),
        legend.key=element_blank()) +
  guides(colour=guide_legend(order=1),
         edge_linetype=guide_legend(order=2),
         shape=guide_legend(order=3))

ggsave(filename=paste0("plots/branching_process/plot_R_g_mutation.png"),
       plot=plot_R_g_mutation, width=image_width_in, height = image_width_in/ratio_width_height, units="in")



# third version of plot: without mutation and with detection ----
plot_R_g_detection <- ggraph(graph=graph_R_g, layout="tree") + 
  geom_edge_link(data=edges_plot_R_g, mapping=aes(x=x_from, y=y_from, xend=x_to, yend=y_to, edge_colour=factor(variant_transmitted), edge_linetype=factor(in_same_cluster), edge_width=factor(in_same_cluster))) +
  scale_edge_color_manual(name="Identical sequence cluster",
                          values=colors_plot_R_g,
                          labels=LETTERS[seq(from = 1, to = length(unique(edges_plot_R_g$variant_transmitted)))],
                          guide="none") +
  scale_discrete_manual(aesthetics=c("edge_width"),
                        name="Transmission",
                        values=c("0"=0.5, "1"=1),
                        labels=c("Not within same cluster", "Within same cluster"),
                        limits=c("1", "0"),
                        guide="none") +
  scale_discrete_manual(aesthetics = c("edge_linetype"),
                        name="Transmission:",
                        values=c("1"="solid", "0"="dashed"),
                        labels=c("Within same cluster", "Not within same cluster"),
                        limits=c("1", "0")) +
  geom_node_point(data=create_layout(graph_R_g, layout="tree"),
                  mapping=aes(x=x, y=y, color=factor(current_variant), shape=factor(detection)), size=2.5, fill="white") +
  scale_color_manual(name="Identical sequence cluster:",
                     values=colors_plot_R_g,
                     labels=LETTERS[seq(from=1, to=length(colors_plot_R_g))]) +
  scale_shape_manual(name="Detection:",
                     breaks=c("1", "0"),
                     values=c(19, 21),
                     labels=c("Case observed", "Case not observed")) +
  theme(panel.background=element_rect(fill="white", colour="black")) +
  theme(legend.position="bottom",
        legend.box="vertical",
        legend.margin=margin(t=0, r=0, b=0, l=0, unit="cm"),
        legend.key=element_blank()) +
  guides(colour=guide_legend(order=1),
         edge_linetype=guide_legend(order=2))

ggsave(filename=paste0("plots/branching_process/plot_R_g_detection.png"),
       plot=plot_R_g_detection, width=image_width_in, height = image_width_in/ratio_width_height, units="in")



# fourth version of plot: with mutation and with detection ----
plot_R_g_mutation_detection <- ggraph(graph=graph_R_g, layout="tree") + 
  geom_edge_link(data=edges_plot_R_g, mapping=aes(x=x_from, y=y_from, xend=x_to, yend=y_to, edge_colour=factor(variant_transmitted), edge_linetype=factor(in_same_cluster), edge_width=factor(in_same_cluster))) +
  scale_edge_color_manual(name="Identical sequence cluster",
                          values=colors_plot_R_g,
                          labels=LETTERS[seq(from = 1, to = length(unique(edges_plot_R_g$variant_transmitted)))],
                          guide="none") +
  scale_discrete_manual(aesthetics=c("edge_width"),
                        name="Transmission",
                        values=c("0"=0.5, "1"=1),
                        labels=c("Not within same cluster", "Within same cluster"),
                        limits=c("1", "0"),
                        guide="none") +
  scale_discrete_manual(aesthetics = c("edge_linetype"),
                        name="Transmission:",
                        values=c("1"="solid", "0"="dashed"),
                        labels=c("Within same cluster", "Not within same cluster"),
                        limits=c("1", "0")) +
  geom_node_point(data=create_layout(graph_R_g, layout="tree"), 
                  mapping=aes(x=x, y=y, color=factor(current_variant), shape=factor(mutation_occured), fill=factor(detection*(current_variant+1)), size=factor(mutation_occured), alpha=factor(detection))) +
  scale_color_manual(name="Identical sequence cluster:",
                     values=colors_plot_R_g,
                     labels=LETTERS[seq(from=1, to=length(colors_plot_R_g))]) +
  scale_shape_manual(name="Mutation:",
                     breaks=c("1", "0"),
                     values=c(22, 21),
                     labels=c("Mutation occurred", "No mutation occurred")) +
  scale_fill_manual(name="Cluster",
                    breaks=c("0", as.character(unique(nodes_plot_R_g$current_variant+1))),
                    values = c("white", colors_plot_R_g),
                    guide="none") +
  scale_discrete_manual(aesthetics = c("size"),
                        name="Mutation:",
                        breaks=c("1", "0"),
                        values=c(3.5, 2.5),
                        labels=c("Mutation occurred", "No mutation occurred"),
                        guide="none") +
  scale_discrete_manual(aesthetics = c("alpha"),
                        name="Detection:",
                        breaks=c("1", "0"),
                        values=c(1, 1),
                        labels=c("Case observed", "Case not observed")) +
  guides(alpha=guide_legend(override.aes=list(shape=c(19,1)))) +
  theme(panel.background=element_rect(fill="white", colour="black")) +
  theme(legend.position="bottom",
        legend.box="vertical",
        legend.margin=margin(t=0, r=0, b=0, l=0, unit="cm"),
        legend.key=element_blank()) +
  guides(colour=guide_legend(order=1),
         edge_linetype=guide_legend(order=2),
         shape=guide_legend(order=3))

ggsave(filename=paste0("plots/branching_process/plot_R_g_mutation_detection.png"),
       plot=plot_R_g_mutation_detection, width=image_width_in, height = image_width_in/ratio_width_height, units="in")



# fifth version of plot: with mutation, with detection and with cluster size ----
plot_R_g_mutation_detection_cluster_size <- ggraph(graph=graph_R_g, layout="tree") + 
  geom_edge_link(data=edges_plot_R_g, mapping=aes(x=x_from, y=y_from, xend=x_to, yend=y_to, edge_colour=factor(variant_transmitted), edge_linetype=factor(in_same_cluster), edge_width=factor(in_same_cluster))) +
  scale_edge_color_manual(name="Identical sequence cluster",
                          values=colors_plot_R_g,
                          labels=LETTERS[seq(from = 1, to = length(unique(edges_plot_R_g$variant_transmitted)))],
                          guide="none") +
  scale_discrete_manual(aesthetics=c("edge_width"),
                        name="Transmission",
                        values=c("0"=0.5, "1"=1),
                        labels=c("Not within same cluster", "Within same cluster"),
                        limits=c("1", "0"),
                        guide="none") +
  scale_discrete_manual(aesthetics = c("edge_linetype"),
                        name="Transmission:",
                        values=c("1"="solid", "0"="dashed"),
                        labels=c("Within same cluster", "Not within same cluster"),
                        limits=c("1", "0")) +
  geom_node_point(data=create_layout(graph_R_g, layout="tree"), 
                  mapping=aes(x=x, y=y, color=factor(current_variant), shape=factor(mutation_occured), fill=factor(detection*(current_variant+1)), size=factor(mutation_occured), alpha=factor(detection))) +
  scale_color_manual(name="Identical sequence cluster:",
                     values=colors_plot_R_g,
                     labels=LETTERS[seq(from=1, to=length(colors_plot_R_g))]) +
  scale_shape_manual(name="Mutation:",
                     breaks=c("1", "0"),
                     values=c(22, 21),
                     labels=c("Mutation occurred", "No mutation occurred")) +
  scale_fill_manual(name="Cluster",
                    breaks=c("0", as.character(unique(nodes_plot_R_g$current_variant+1))),
                    values = c("white", colors_plot_R_g),
                    guide="none") +
  scale_discrete_manual(aesthetics = c("size"),
                        name="Mutation:",
                        breaks=c("1", "0"),
                        values=c(3.5, 2.5),
                        labels=c("Mutation occurred", "No mutation occurred"),
                        guide="none") +
  scale_discrete_manual(aesthetics = c("alpha"),
                        name="Detection:",
                        breaks=c("1", "0"),
                        values=c(1, 1),
                        labels=c("Case observed", "Case not observed")) +
  annotate("text", x = -2.4, y = 2.125, label = "2", color = colors_plot_R_g[1], size = 5) +
  annotate("text", x = 2.5, y = 1, label = "2", color = colors_plot_R_g[2], size = 5) +
  annotate("text", x = -1.75, y = 0.5, label = "3", color = colors_plot_R_g[3], size = 5) +
  annotate("text", x = -3.25, y = 0.15, label = "1", color = colors_plot_R_g[4], size = 5) +
  annotate("text", x = 0.75, y = 0.15, label = "0", color = colors_plot_R_g[5], size = 5) +
  guides(alpha=guide_legend(override.aes=list(shape=c(19,1)))) +
  theme(panel.background=element_rect(fill="white", colour="black")) +
  theme(legend.position="bottom",
        legend.box="vertical",
        legend.margin=margin(t=0, r=0, b=0, l=0, unit="cm"),
        legend.key=element_blank()) +
  guides(colour=guide_legend(order=1),
         edge_linetype=guide_legend(order=2),
         shape=guide_legend(order=3))


ggsave(filename=paste0("plots/branching_process/plot_R_g_mutation_detection_cluster_size.png"),
       plot=plot_R_g_mutation_detection_cluster_size, width=image_width_in, height = image_width_in/ratio_width_height, units="in")

ggsave(filename=paste0("plots/paper/figure_cluster_simulation.pdf"),
       plot=plot_R_g_mutation_detection_cluster_size, width=image_width_in, height = image_width_in/ratio_width_height, units="in")



# sixth version of plot: without mutation, with detection and with cluster size ----
plot_R_g_detection_cluster_size <- ggraph(graph=graph_R_g, layout="tree") + 
  geom_edge_link(data=edges_plot_R_g, mapping=aes(x=x_from, y=y_from, xend=x_to, yend=y_to, edge_colour=factor(variant_transmitted), edge_linetype=factor(in_same_cluster), edge_width=factor(in_same_cluster))) +
  scale_edge_color_manual(name="Identical sequence cluster",
                          values=colors_plot_R_g,
                          labels=LETTERS[seq(from = 1, to = length(unique(edges_plot_R_g$variant_transmitted)))],
                          guide="none") +
  scale_discrete_manual(aesthetics=c("edge_width"),
                        name="Transmission",
                        values=c("0"=0.5, "1"=1),
                        labels=c("Not within same cluster", "Within same cluster"),
                        limits=c("1", "0"),
                        guide="none") +
  scale_discrete_manual(aesthetics = c("edge_linetype"),
                        name="Transmission:",
                        values=c("1"="solid", "0"="dashed"),
                        labels=c("Within same cluster", "Not within same cluster"),
                        limits=c("1", "0")) +
  geom_node_point(data=create_layout(graph_R_g, layout="tree"),
                  mapping=aes(x=x, y=y, color=factor(current_variant), shape=factor(detection)), size=5.5, fill="white", stroke=1.25) +
  scale_color_manual(name="Identical sequence cluster:",
                     values=colors_plot_R_g,
                     labels=LETTERS[seq(from=1, to=length(colors_plot_R_g))]) +
  scale_shape_manual(name="Detection:",
                     breaks=c("1", "0"),
                     values=c(19, 21),
                     labels=c("Case observed", "Case not observed")) +
  annotate("text", x = -2.4, y = 2.125, label = "2", color = colors_plot_R_g[1], size = 5) +
  annotate("text", x = 2.5, y = 1, label = "2", color = colors_plot_R_g[2], size = 5) +
  annotate("text", x = -1.75, y = 0.5, label = "3", color = colors_plot_R_g[3], size = 5) +
  annotate("text", x = -3.25, y = 0.15, label = "1", color = colors_plot_R_g[4], size = 5) +
  annotate("text", x = 0.75, y = 0.15, label = "0", color = colors_plot_R_g[5], size = 5) +
  theme(panel.background=element_rect(fill="white", colour="black")) +
  theme(legend.position="bottom",
        legend.box="vertical",
        legend.margin=margin(t=0, r=0, b=0, l=0, unit="cm"),
        legend.key=element_blank()) +
  guides(colour=guide_legend(order=1),
         edge_linetype=guide_legend(order=2))

ggsave(filename=paste0("plots/branching_process/plot_R_g_detection_cluster_size.png"),
       plot=plot_R_g_detection_cluster_size, width=image_width_in, height = image_width_in/ratio_width_height, units="in")

ggsave(filename=paste0("plots/paper/figure_cluster_simulation_revised.pdf"),
       plot=plot_R_g_detection_cluster_size, width=image_width_in, height = image_width_in/ratio_width_height, units="in")




