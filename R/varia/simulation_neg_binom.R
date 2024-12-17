

# setup ----

# load packages
library(stats)
library(cowplot)
library(dplyr)
library(ggplot2)
library(tibble)

# set seed
set.seed(314)



# define parameters ----

# effective reproduction number
R <- 1.2

# dispersion parameter
k <- 0.3

# mutation probability
mu <- 0.1

# number of samples
N <- 10000



# get samples ----

# T: number of secondary cases (negative binomial random variable with mean `R` and dispersion parameter `k`)
# V: number of secondary cases within same identical sequence cluster (negative binomial random variable with mean `(1 - mu) R` and dispersion parameter `k`)
# W: number of secondary cases not within same identical sequence cluster (negative binomial random variable with mean `mu R` and dispersion parameter `k`)
samples_T <- rnbinom(n = N, mu = R, size = k)
samples_V <- unlist(lapply(X = samples_T, FUN = function(x) rbinom(n = 1, size = x, prob = 1-mu)))
samples_W <- samples_T - samples_V
samples_V_plus_W <- samples_V + samples_W

table_samples_T <- tibble(x = sort(unique(samples_T)),
                          freq_T = as.numeric(table(samples_T)))
table_samples_V <- tibble(x = sort(unique(samples_V)),
                          freq_V = as.numeric(table(samples_V)))
table_samples_W <- tibble(x = sort(unique(samples_W)),
                          freq_W = as.numeric(table(samples_W)))
table_samples_V_plus_W <- tibble(x = sort(unique(samples_V_plus_W)),
                                 freq_V_plus_W = as.numeric(table(samples_V_plus_W)))

min_T_V_W <- min(samples_T, samples_V, samples_W, samples_V_plus_W)
max_T_V_W <- max(samples_T, samples_V, samples_W, samples_V_plus_W)

table_samples_T_V_W <- tibble(x = min_T_V_W:max_T_V_W) %>%
  left_join(table_samples_T, by = "x") %>%
  left_join(table_samples_V, by = "x") %>%
  left_join(table_samples_W, by = "x") %>%
  left_join(table_samples_V_plus_W, by = "x") %>%
  replace(is.na(.), 0)


# X_1 and Y_1: independent negative binomial random variables 
# X_1: negative binomial random variable with mean `(1 - mu) R` and dispersion parameter `k`
# Y_1: negative binomial random variable with mean `mu R` and dispersion parameter `k`
samples_X_1 <- rnbinom(n = N, mu = (1-mu)*R, size = k)
samples_Y_1 <- rnbinom(n = N, mu = mu*R, size = k)
samples_X_1_plus_Y_1 <- samples_X_1 + samples_Y_1

table_samples_X_1 <- tibble(x = sort(unique(samples_X_1)),
                            freq_X_1 = as.numeric(table(samples_X_1)))
table_samples_Y_1 <- tibble(x = sort(unique(samples_Y_1)),
                            freq_Y_1 = as.numeric(table(samples_Y_1)))
table_samples_X_1_plus_Y_1 <- tibble(x = sort(unique(samples_X_1_plus_Y_1)),
                                     freq_X_1_plus_Y_1 = as.numeric(table(samples_X_1_plus_Y_1)))

min_X_Y_1 <- min(samples_X_1, samples_Y_1, samples_X_1_plus_Y_1)
max_X_Y_1 <- max(samples_X_1, samples_Y_1, samples_X_1_plus_Y_1)

table_samples_X_Y_1 <- tibble(x = min_X_Y_1:max_X_Y_1) %>%
  left_join(table_samples_X_1, by = "x") %>%
  left_join(table_samples_Y_1, by = "x") %>%
  left_join(table_samples_X_1_plus_Y_1, by = "x") %>%
  replace(is.na(.), 0)


# X_2 and Y_2: independent negative binomial random variables 
# X_2: negative binomial random variable with mean `0.5 R` and dispersion parameter `k`
# Y_2: negative binomial random variable with mean `0.5 R` and dispersion parameter `k`
samples_X_2 <- rnbinom(n = N, mu = 0.5*R, size = k)
samples_Y_2 <- rnbinom(n = N, mu = 0.5*R, size = k)
samples_X_2_plus_Y_2 <- samples_X_2 + samples_Y_2

table_samples_X_2 <- tibble(x = sort(unique(samples_X_2)),
                            freq_X_2 = as.numeric(table(samples_X_2)))
table_samples_Y_2 <- tibble(x = sort(unique(samples_Y_2)),
                            freq_Y_2 = as.numeric(table(samples_Y_2)))
table_samples_X_2_plus_Y_2 <- tibble(x = sort(unique(samples_X_2_plus_Y_2)),
                                     freq_X_2_plus_Y_2 = as.numeric(table(samples_X_2_plus_Y_2)))

min_X_Y_2 <- min(samples_X_2, samples_Y_2, samples_X_2_plus_Y_2)
max_X_Y_2 <- max(samples_X_2, samples_Y_2, samples_X_2_plus_Y_2)

table_samples_X_Y_2 <- tibble(x = min_X_Y_2:max_X_Y_2) %>%
  left_join(table_samples_X_2, by = "x") %>%
  left_join(table_samples_Y_2, by = "x") %>%
  left_join(table_samples_X_2_plus_Y_2, by = "x") %>%
  replace(is.na(.), 0)



# define distributions ----

neg_bin_dist <- tibble(x = 0:max(max_T_V_W, max_X_Y_1)) %>%
  mutate(dist_T = dnbinom(x = x, mu = R, size = k),
         dist_V = dnbinom(x = x, mu = (1-mu)*R, size = k),
         dist_W = dnbinom(x = x, mu = mu*R, size = k),
         dist_X_2 = dnbinom(x = x, mu = 0.5*R, size = k),
         dist_X_2_plus_Y_2 = dnbinom(x = x, mu = R, size = 2*k))



# create plots ----

upper_limit_x_axis <- 6

plot_T <- ggplot() + 
  geom_col(data = table_samples_T_V_W, aes(x = x, y = freq_T / N), fill = "navy") +
  geom_point(data = neg_bin_dist, mapping = aes(x = x, y = dist_T), color = "black", fill = "firebrick", shape = 21, size = 3) +
  scale_x_continuous(limits = c(-0.5, upper_limit_x_axis + 0.5),
                     breaks = 0:upper_limit_x_axis) +
  scale_y_continuous(limits = c(0, 1)) +
  xlab(label = "T") +
  ylab(label = NULL) +
  theme_bw() +
  theme(axis.title.x=element_text(color="black"),
        axis.text.x=element_text(color="black"),
        axis.title.y=element_text(color="black"),
        axis.text.y=element_text(color="black"))

plot_V <- ggplot() + 
  geom_col(data = table_samples_T_V_W, aes(x = x, y = freq_V / N), fill = "navy") +
  geom_point(data = neg_bin_dist, mapping = aes(x = x, y = dist_V), color = "black", fill = "firebrick", shape = 21, size = 3) +
  scale_x_continuous(limits = c(-0.5, upper_limit_x_axis + 0.5),
                     breaks = 0:upper_limit_x_axis) +
  scale_y_continuous(limits = c(0, 1)) +
  xlab(label = "V") +
  ylab(label = NULL) +
  theme_bw() +
  theme(axis.title.x=element_text(color="black"),
        axis.text.x=element_text(color="black"),
        axis.title.y=element_text(color="black"),
        axis.text.y=element_text(color="black"))

plot_W <- ggplot() + 
  geom_col(data = table_samples_T_V_W, aes(x = x, y = freq_W / N), fill = "navy") +
  geom_point(data = neg_bin_dist, mapping = aes(x = x, y = dist_W), color = "black", fill = "firebrick", shape = 21, size = 3) +
  scale_x_continuous(limits = c(-0.5, upper_limit_x_axis + 0.5),
                     breaks = 0:upper_limit_x_axis) +
  scale_y_continuous(limits = c(0, 1)) +
  xlab(label = "W") +
  ylab(label = NULL) +
  theme_bw() +
  theme(axis.title.x=element_text(color="black"),
        axis.text.x=element_text(color="black"),
        axis.title.y=element_text(color="black"),
        axis.text.y=element_text(color="black"))


plot_X_1 <- ggplot() + 
  geom_col(data = table_samples_X_Y_1, aes(x = x, y = freq_X_1 / N), fill = "navy") +
  geom_point(data = neg_bin_dist, mapping = aes(x = x, y = dist_V), color = "black", fill = "firebrick", shape = 21, size = 3) +
  scale_x_continuous(limits = c(-0.5, upper_limit_x_axis + 0.5),
                     breaks = 0:upper_limit_x_axis) +
  scale_y_continuous(limits = c(0, 1)) +
  xlab(label = expression(X[1])) +
  ylab(label = NULL) +
  theme_bw() +
  theme(axis.title.x=element_text(color="black"),
        axis.text.x=element_text(color="black"),
        axis.title.y=element_text(color="black"),
        axis.text.y=element_text(color="black"))

plot_Y_1 <- ggplot() + 
  geom_col(data = table_samples_X_Y_1, aes(x = x, y = freq_Y_1 / N), fill = "navy") +
  geom_point(data = neg_bin_dist, mapping = aes(x = x, y = dist_W), color = "black", fill = "firebrick", shape = 21, size = 3) +
  scale_x_continuous(limits = c(-0.5, upper_limit_x_axis + 0.5),
                     breaks = 0:upper_limit_x_axis) +
  scale_y_continuous(limits = c(0, 1)) +
  xlab(label = expression(Y[1])) +
  ylab(label = NULL) +
  theme_bw() +
  theme(axis.title.x=element_text(color="black"),
        axis.text.x=element_text(color="black"),
        axis.title.y=element_text(color="black"),
        axis.text.y=element_text(color="black"))

plot_X_1_plus_Y_1 <- ggplot() + 
  geom_col(data = table_samples_X_Y_1, aes(x = x, y = freq_X_1_plus_Y_1 / N), fill = "navy") +
  geom_point(data = neg_bin_dist, mapping = aes(x = x, y = dist_X_2_plus_Y_2), color = "black", fill = "firebrick", shape = 25, size = 3) +
  geom_point(data = neg_bin_dist, mapping = aes(x = x, y = dist_T), color = "black", fill = "firebrick", shape = 21, size = 3) +
  scale_x_continuous(limits = c(-0.5, upper_limit_x_axis + 0.5),
                     breaks = 0:upper_limit_x_axis) +
  scale_y_continuous(limits = c(0, 1)) +
  xlab(label = expression(X[1] + Y[1])) +
  ylab(label = NULL) +
  theme_bw() +
  theme(axis.title.x=element_text(color="black"),
        axis.text.x=element_text(color="black"),
        axis.title.y=element_text(color="black"),
        axis.text.y=element_text(color="black"))


plot_X_2 <- ggplot() + 
  geom_col(data = table_samples_X_Y_2, aes(x = x, y = freq_X_2 / N), fill = "navy") +
  geom_point(data = neg_bin_dist, mapping = aes(x = x, y = dist_X_2), color = "black", fill = "firebrick", shape = 21, size = 3) +
  scale_x_continuous(limits = c(-0.5, upper_limit_x_axis + 0.5),
                     breaks = 0:upper_limit_x_axis) +
  scale_y_continuous(limits = c(0, 1)) +
  xlab(label = expression(X[2])) +
  ylab(label = NULL) +
  theme_bw() +
  theme(axis.title.x=element_text(color="black"),
        axis.text.x=element_text(color="black"),
        axis.title.y=element_text(color="black"),
        axis.text.y=element_text(color="black"))

plot_Y_2 <- ggplot() + 
  geom_col(data = table_samples_X_Y_2, aes(x = x, y = freq_Y_2 / N), fill = "navy") +
  geom_point(data = neg_bin_dist, mapping = aes(x = x, y = dist_X_2), color = "black", fill = "firebrick", shape = 21, size = 3) +
  scale_x_continuous(limits = c(-0.5, upper_limit_x_axis + 0.5),
                     breaks = 0:upper_limit_x_axis) +
  scale_y_continuous(limits = c(0, 1)) +
  xlab(label = expression(Y[2])) +
  ylab(label = NULL) +
  theme_bw() +
  theme(axis.title.x=element_text(color="black"),
        axis.text.x=element_text(color="black"),
        axis.title.y=element_text(color="black"),
        axis.text.y=element_text(color="black"))

plot_X_2_plus_Y_2 <- ggplot() + 
  geom_col(data = table_samples_X_Y_2, aes(x = x, y = freq_X_2_plus_Y_2 / N), fill = "navy") +
  geom_point(data = neg_bin_dist, mapping = aes(x = x, y = dist_X_2_plus_Y_2), color = "black", fill = "firebrick", shape = 25, size = 3) +
  geom_point(data = neg_bin_dist, mapping = aes(x = x, y = dist_T), color = "black", fill = "firebrick", shape = 21, size = 3) +
  scale_x_continuous(limits = c(-0.5, upper_limit_x_axis + 0.5),
                     breaks = 0:upper_limit_x_axis) +
  scale_y_continuous(limits = c(0, 1)) +
  xlab(label = expression(X[2] + Y[2])) +
  ylab(label = NULL) +
  theme_bw() +
  theme(axis.title.x=element_text(color="black"),
        axis.text.x=element_text(color="black"),
        axis.title.y=element_text(color="black"),
        axis.text.y=element_text(color="black"))


plot_grid_V_W <- plot_grid(plot_V + theme(plot.margin = margin(0, 5, 5, 20)),
                           plot_W + theme(plot.margin = margin(0, 5, 5, 20)),
                           labels = c('B', 'C'), label_size = 12)

plot_grid_1 <- plot_grid(plot_T + theme(plot.margin = margin(5, 5, 5, 20)),
                         plot_grid_V_W + theme(plot.margin = margin(0, 5, 5, 20)),
                         labels = c('A', ''), label_size = 12, nrow = 2)


plot_grid_X_Y_1 <- plot_grid(plot_X_1 + theme(plot.margin = margin(5, 5, 5, 20)),
                             plot_Y_1 + theme(plot.margin = margin(5, 5, 5, 20)),
                             labels = c('D', 'E'), label_size = 12)

plot_grid_2 <- plot_grid(plot_grid_X_Y_1 + theme(plot.margin = margin(0, 5, 5, 20)),
                         plot_X_1_plus_Y_1 + theme(plot.margin = margin(0, 5, 5, 20)),
                         labels = c('', 'F'), label_size = 12, nrow = 2)


plot_grid_X_Y_2 <- plot_grid(plot_X_2 + theme(plot.margin = margin(5, 5, 5, 20)),
                             plot_Y_2 + theme(plot.margin = margin(5, 5, 5, 20)),
                             labels = c('G', 'H'), label_size = 12)

plot_grid_3 <- plot_grid(plot_grid_X_Y_2 + theme(plot.margin = margin(0, 5, 5, 20)),
                         plot_X_2_plus_Y_2 + theme(plot.margin = margin(0, 5, 5, 20)),
                         labels = c('', 'I'), label_size = 12, nrow = 2)

plot_grid <- plot_grid(plot_grid_1,
                       plot_grid_2,
                       plot_grid_3,
                       labels = c('', '', ''), label_size = 12, ncol = 3)


ggsave(filename = "plots/simulation/varia/plot_grid_sim_neg_binom.png",
       plot = plot_grid,
       bg = "white",
       width = 12, height = 7, units = "in")

ggsave(filename = "plots/simulation/varia/plot_grid_sim_neg_binom.pdf",
       plot = plot_grid,
       width = 12, height = 7, units = "in")



