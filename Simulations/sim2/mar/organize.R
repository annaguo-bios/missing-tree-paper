N <- c(500, 1000, 2000, 4000, 8000)
nsim <- 500
true_beta1 <- 0  # True value of beta1 (X3 ⊥ X1 | X2)
alpha <- 0.05

# Initialize storage matrices for each metric and method
methods <- c("Amelia", "Complete case","MICE", "Missing tree")

# Create a list to store results for each method
results_all <- list()
for (method in methods) {
  results_all[[method]] <- list(
    estimate = matrix(NA, nrow = nsim, ncol = length(N)),
    se = matrix(NA, nrow = nsim, ncol = length(N)),
    ci_lower = matrix(NA, nrow = nsim, ncol = length(N)),
    ci_upper = matrix(NA, nrow = nsim, ncol = length(N)),
    p_value = matrix(NA, nrow = nsim, ncol = length(N))
  )
}

for (n in N) {
  
  cat('n =', n, '\n')
  
  for (i in 1:nsim) {
    
    load(paste0("output/output_", n, "_", i, ".Rdata"))
    
    for (method in methods) {
      results_all[[method]]$estimate[i, which(N == n)] <- results[[method]]$estimate
      results_all[[method]]$se[i, which(N == n)] <- results[[method]]$se
      results_all[[method]]$ci_lower[i, which(N == n)] <- results[[method]]$ci_lower
      results_all[[method]]$ci_upper[i, which(N == n)] <- results[[method]]$ci_upper
      results_all[[method]]$p_value[i, which(N == n)] <- results[[method]]$p_value
    }
    
  }
  
  
}


results <- results_all

save(results, file = "output/results.Rdata")