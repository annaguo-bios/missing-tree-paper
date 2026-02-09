est_mice <- function(data, vocal=F){
  
  # keep only the X variables, drop the R indicators
  data_for_imputation <- data %>% 
    select(X1, X2, X3)
  
  # run mice with m=5
  imp <- mice(data_for_imputation, printFlag = FALSE)
  
  # get mean and variance for each imputed dataset
  est_stats <- sapply(1:5, function(i) {
    complete_data <- complete(imp, i)
    n <- nrow(complete_data)
    
    bd_output <- bd_est(complete_data, magic_weight_ps = rep(1,n), magic_weight_outcome = rep(1,n))
    
    c(mean = bd_output$estimate, 
      var = bd_output$se^2)
  })
  
  target_means <- est_stats[1, ]
  target_vars <- est_stats[2, ]
  
  # pool using Rubin's rules
  pooled_mean <- mean(target_means)
  
  # within and between variance
  within_var <- mean(target_vars)
  between_var <- var(target_means)
  
  # total variance
  total_var <- within_var + (1 + 1/5) * between_var
  
  if(vocal){
    cat("\nPooled mean:", pooled_mean, "SE:", sqrt(total_var), "\n")
    cat("True mean:", mean(dgp_output$data_no_missing[[variable]]), "\n")
    cat("Naive (complete case):", mean(data[[variable]], na.rm = TRUE), "\n")
  }
  
  return(list(estimate = pooled_mean, se = sqrt(total_var)))
  
}
