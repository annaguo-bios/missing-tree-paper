library(boot)

est_flexmissing <- function(data, B = 500, alpha = 0.05){

  vocal <- FALSE

  # Define the statistic function for bootstrap
  boot_stat <- function(data, indices) {
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
                                  est_R = c('R2', 'R3'),
                                  non_ID_R = NULL)

    magic_weight_outcome <- clique_outcome$magic_weight

    # Fit weighted regression
    fit <- lm(X3 ~ X1 + X2, data = boot_data, weights = magic_weight_outcome)

    # Return coefficient for X1
    return(coef(fit)["X1"])
  }

  # Run bootstrap
  boot_results <- boot(data = data, statistic = boot_stat, R = B)

  # Extract bootstrap estimates
  beta1_hat <- boot_results$t0  # Original estimate
  boot_estimates <- boot_results$t[, 1]  # Bootstrap replicates

  # Bootstrap standard error
  se_beta1 <- sd(boot_estimates)

  # Bootstrap confidence intervals (percentile method)
  ci <- quantile(boot_estimates, probs = c(alpha/2, 1 - alpha/2))
  ci_lower <- ci[1]
  ci_upper <- ci[2]

  # Bootstrap p-value (two-sided test for H0: beta1 = 0)
  p_value <- 2 * min(mean(boot_estimates >= 0), mean(boot_estimates <= 0))

  return(list(
    estimate = beta1_hat,
    se = se_beta1,
    ci_lower = ci_lower,
    ci_upper = ci_upper,
    p_value = p_value,
    boot_results = boot_results
  ))
}