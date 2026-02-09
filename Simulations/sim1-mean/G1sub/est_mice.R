est_mice <- function(data, variable, vocal=F){
  
  # keep only the X variables, drop the R indicators
  data_for_imputation <- data %>% 
    select(X1, X2, X3)
  
  # run mice with m=5
  imp <- mice(data_for_imputation, printFlag=F)
  
  # get mean and variance for each imputed dataset
  est_stats <- sapply(1:5, function(i) {
    complete_data <- complete(imp, i)
    c(mean = mean(complete_data[[variable]]), 
      var = var(complete_data[[variable]]) / nrow(complete_data))
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
    cat("Naive (complete case):", mean(data_for_imputation[[variable]], na.rm = TRUE), "\n")
  }
  
  return(list(estimate = pooled_mean, se = sqrt(total_var)))
  
}
