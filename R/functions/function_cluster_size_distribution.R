

distribution_cluster_size_testing <- function(N, Re, k, p_mut, p_detec) {
  
  cluster_sizes <- 1:max((2 / p_detec) * N, N + 500)
  
  Rg <- (1 - p_mut) * Re
  
  # distribution of size of identical sequence clusters with complete case detection
  distribution_0 <- exp(lgamma(k*cluster_sizes + cluster_sizes - 1)
                        - lgamma(k*cluster_sizes)
                        - lgamma(cluster_sizes + 1)
                        + (cluster_sizes-1)*log(Rg/k)
                        - (k*cluster_sizes + cluster_sizes - 1)*log(1 + Rg/k))
  
  if (p_detec == 1) {
    
    distribution <- distribution_0
    
  } else if (p_detec == 0) {
    
    distribution <- rep(x = 0, times = N)
    
  } else {
    
    distribution_1 <- rep(x = 0, times = N+1)
    
    for (ii in 1:max(cluster_sizes)) {
      
      # determine probability that out of an identical sequence cluster of size ii exactly 0 cases are detected
      probability_ii_0 <- exp(ii * log(1-p_detec) + log(distribution_0[ii]))
      
      # update probability that an identical sequence cluster has size 0
      distribution_1[1] <- distribution_1[1] + probability_ii_0
      
    }
    
    for (jj in 1:N) {
      
      for (ii in jj:max(cluster_sizes)) {
        
        # determine probability that out of an identical sequence cluster of size ii exactly jj cases are detected
        probability_ii_jj <- exp(lchoose(ii, jj) + jj * log(p_detec) + (ii-jj) * log(1-p_detec) + log(distribution_0[ii]))
        
        # update probability that an identical sequence cluster has size jj
        distribution_1[jj+1] <- distribution_1[jj+1] + probability_ii_jj
        
      }
      
    }
    
    # rescale `distribution_1` to clusters of size >= 1
    distribution <- rep(x = 0, times = N)
    
    for (ii in 1:N) {
      
      distribution[ii] <- distribution_1[ii+1] / (1 - distribution_1[1])
      
    }

  }

  return(distribution)
  
}


