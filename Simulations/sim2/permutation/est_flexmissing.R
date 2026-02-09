library(boot)

est_flexmissing <- function(data, B = 500, alpha = 0.05){
  vocal <- FALSE
  
  # Define the statistic function for bootstrap
  boot_stat <- function(data, indices) {
    tryCatch({
      # Resample data
      boot_data <- data[indices, ]
      
      # Propensity scores
      propensitys <- f.propensity(graph = G,
                                  data = boot_data,
                                  ID = ID,
                                  law = 'target',
                                  link.R = "logit",
                                  formula.R = NULL,
                                  crossfit = FALSE,
                                  K = 5,
                                  superlearner = FALSE,
                                  lib.SL = c("SL.glm", "SL.earth", "SL.ranger", "SL.mean"),
                                  vocal = vocal)
      
      clique_outcome <- find_clique(graph = G,
                                    data = boot_data,
                                    vocal = vocal,
                                    propensitys = propensitys,
                                    est_R = c('R1', 'R2', 'R3'),
                                    non_ID_R = NULL)
      magic_weight_outcome <- clique_outcome$magic_weight
      
      # Fit weighted regression
      fit <- lm(X3 ~ X1 + X2 + I(X2^2), data = boot_data, weights = magic_weight_outcome)
      
      # Return coefficient for X1
      return(coef(fit)["X1"])
    }, error = function(e) {
      return(NA)
    })
  }
  
  # Run bootstrap until we have B valid estimates
  boot_estimates <- c()
  
  while(sum(!is.na(boot_estimates)) < B) {
    needed <- B - sum(!is.na(boot_estimates))
    cat('needed:',needed,'\n')
    boot_results <- boot(data = data, statistic = boot_stat, R = needed)
    boot_estimates <- c(boot_estimates, boot_results$t[, 1])
  }
  
  # Keep only first B valid estimates
  boot_estimates <- boot_estimates[!is.na(boot_estimates)][1:B]
  
  # Bootstrap standard error
  se_beta1 <- sd(boot_estimates)
  
  # Bootstrap confidence intervals
  ci <- quantile(boot_estimates, probs = c(alpha/2, 1 - alpha/2))
  ci_lower <- ci[1]
  ci_upper <- ci[2]
  
  # Bootstrap p-value
  p_value <- 2 * min(mean(boot_estimates >= 0), mean(boot_estimates <= 0))
  
  return(list(
    estimate = boot_results$t0,
    se = se_beta1,
    ci_lower = ci_lower,
    ci_upper = ci_upper,
    p_value = p_value,
    boot_results = boot_results
  ))
}