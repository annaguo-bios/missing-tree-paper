library(Amelia)

est_em <- function(data){
  
  data_X_only <- data[, grep("^X", names(data))]
  
  # drop completely missing rows
  data_for_amelia <- data_X_only[rowSums(is.na(data_X_only)) < ncol(data_X_only), ]
  
  amelia_output <- amelia(data_for_amelia, m = 5, noms = NULL, p2s = 0)
  
  fit <- with(amelia_output, lm(X3 ~ X1 + X2))
  
  pooled_sum <- mi.combine(fit, conf.int = TRUE)
  
  idx_x1 <- which(pooled_sum$term == "X1")
  beta1_hat = pooled_sum$estimate[idx_x1]
  se_beta1 = pooled_sum$std.error[idx_x1]
  ci_upper = pooled_sum$conf.low[idx_x1]
  ci_lower = pooled_sum$conf.high[idx_x1]
  p_value = 2 * pt(-abs(pooled_sum$statistic), df = pooled_sum$df)[idx_x1]
  
  return(list(
    estimate = beta1_hat,
    se =  se_beta1,
    ci_lower = ci_lower,
    ci_upper = ci_upper,
    p_value = p_value
  ))
  
  
}
