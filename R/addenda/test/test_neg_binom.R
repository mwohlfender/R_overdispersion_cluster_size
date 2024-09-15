
N <- 1e6


m <- 2.5
r <- 0.5

mu <- 0.3

m_one <- (1-mu)*m
m_two <- mu*m


Z <- stats::rnbinom(n = N, size = r, mu = m)

X <- stats::rnbinom(n = N, size = r, mu = m_one)
X_a <- stats::rnbinom(n = N, size = r, prob = (r / (m_one+r)))

Y <- stats::rnbinom(n = N, size = r, mu = m_two)

X_p_Y <- X + Y

table_X <- as_tibble(table(X)) %>% mutate(perc = n / N)
table_X_a <- as_tibble(table(X_a)) %>% mutate(perc = n / N)

table_Y <- as_tibble(table(Y)) %>% mutate(perc = n / N)

table_X_p_Y <- as_tibble(table(X_p_Y)) %>% mutate(perc = n / N)

table_Z <- as_tibble(table(Z)) %>% mutate(perc = n / N)


var_X <- r * (1 - (r / (m_one + r))) / ((r / (m_one + r))^2)
var_Y <- r * (1 - (r / (m_two + r))) / ((r / (m_two + r))^2)

test <- m / (r / (m - 2 * mu * m + 2 * mu^2 * m + r))

((test - m)/(m^2))^-1





samples_T <- stats::rnbinom(n = N, size = r, mu = m)


samples_V <- rep(x = 0, times = N)

samples_W <- rep(x = 0, times = N)

samples_W_a <- rep(x = 0, times = N) 

for (ii in 1:N) {
  
  samples_V[ii] <- sum(stats::rbinom(n = samples_T[ii], size = 1, prob = (1 - mu)))
  
  samples_W[ii] <- samples_T[ii] - samples_V[ii]
  
  samples_W_a[ii] <- sum(stats::rbinom(n = samples_T[ii], size = 1, prob = mu))
  
}


mean(samples_T)
mean(samples_V)
mean(samples_W)

stats::var(samples_T)
m + m^2/r

stats::var(samples_V)
m_one + m_one^2/r

stats::var(samples_W)
m_two + m_two^2/r

stats::cov(samples_V, samples_W)

stats::var(samples_V)+stats::var(samples_W)+2*stats::cov(samples_V, samples_W)



table_samples_T <- as_tibble(table(samples_T)) %>%
  mutate(samples_T = as.numeric(samples_T),
         perc = n / N) %>%
  dplyr::rename(value = samples_T) %>%
  mutate(formula = exp(lgamma(r+value)-lfactorial(value)-lgamma(r)+r*log(r/(r+m))+value*log(m/(r+m))))

table_samples_V <- as_tibble(table(samples_V)) %>%
  mutate(samples_V = as.numeric(samples_V),
         perc = n / N) %>%
  dplyr::rename(value = samples_V) %>%
  mutate(formula = exp(lgamma(r+value)-lfactorial(value)-lgamma(r)+r*log(r/(r+m_one))+value*log(m_one/(r+m_one))))

table_samples_W <- as_tibble(table(samples_W)) %>%
  mutate(samples_W = as.numeric(samples_W),
         perc = n / N) %>%
  dplyr::rename(value = samples_W)  %>%
  mutate(formula = exp(lgamma(r+value)-lfactorial(value)-lgamma(r)+r*log(r/(r+m_two))+value*log(m_two/(r+m_two))))

table_samples_W_a <- as_tibble(table(samples_W_a)) %>%
  mutate(samples_W_a = as.numeric(samples_W_a),
         perc = n / N) %>%
  dplyr::rename(value = samples_W_a)  %>%
  mutate(formula = exp(lgamma(r+value)-lfactorial(value)-lgamma(r)+r*log(r/(r+m_two))+value*log(m_two/(r+m_two))))



p_t <- table_samples_T$formula[1:50]

index <- 0:(length(p_t)-1)

coeffs_W_equal_0 <- choose(index,index) * (1 - mu)^index * mu^0

sum(coeffs_W_equal_0 * p_t)






  
# 
# table_X
# 
# table_samples_V
# 
# 
# table_Y
# 
# table_samples_W
# 
# table_samples_W_a
# 
# 
# Z_reconstructed <- samples_V + samples_W
# Z_reconstructed_a <- samples_V + samples_W_a
# 
# table_Z_reconstructed <- as_tibble(table(Z_reconstructed)) %>% mutate(perc = n / N)
# table_Z_reconstructed_a <- as_tibble(table(Z_reconstructed_a)) %>% mutate(perc = n / N)
# 
# 
