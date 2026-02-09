library(Amelia)

est_em <- function(data,variable,vocal=F){
  
  data_X_only <- data[, grep("^X", names(data))]
  
  # drop completely missing rows
  data_for_amelia <- data_X_only[rowSums(is.na(data_X_only)) < ncol(data_X_only), ]
  
  amelia_output <- amelia(data_for_amelia, m = 5, noms = "X2", p2s = 0)
  
  # get mean and variance for each imputed dataset
  est_stats <- sapply(1:5, function(i) {
    complete_data <- amelia_output$imputations[[i]]
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
