est_complete <- function(data){
  
  # keep only the X variables, drop the R indicators
  data_complete <- na.omit(data)
  
  fit <- lm(X3 ~ X1 + X2, data = data_complete)
  
  fit_sum <- summary(fit)
  coef_table <- fit_sum$coefficients
  ci <- confint(fit, level = 0.95)
  
  pooled_sum <- data.frame(
    term = rownames(coef_table),
    estimate = coef_table[, "Estimate"],
    std.error = coef_table[, "Std. Error"],
    statistic = coef_table[, "t value"],
    p.value = coef_table[, "Pr(>|t|)"],
    `2.5 %` = ci[, 1],
    `97.5 %` = ci[, 2],
    check.names = FALSE  # keep column names with special characters
  )
  
  idx_x1 <- which(pooled_sum$term == "X1")
  beta1_hat = pooled_sum$estimate[idx_x1]
  se_beta1 = pooled_sum$std.error[idx_x1]
  ci_lower = pooled_sum$`2.5 %`[idx_x1]
  ci_upper = pooled_sum$`97.5 %`[idx_x1]
  p_value = pooled_sum$p.value[idx_x1]
  
  return(list(
    estimate = beta1_hat,
    se =  se_beta1,
    ci_lower = ci_lower,
    ci_upper = ci_upper,
    p_value = p_value
  ))
  
}
