library(mice)
est_mice <- function(data){
  
  # keep only the X variables, drop the R indicators
  data_for_imputation <- data %>% 
    select(X1, X2, X3)
  
  # run mice with m=5
  imp <- mice(data_for_imputation, printFlag=F)
  
  fit <- with(imp, lm(X3 ~ X1 + X2))
  
  pooled <- pool(fit)
  pooled_sum <- summary(pooled, conf.int = TRUE, conf.level = 0.95)
  
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
