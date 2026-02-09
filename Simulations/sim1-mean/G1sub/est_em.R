library(Amelia)

est_em <- function(data,variable,vocal=F){
  
  data_X_only <- data[, grep("^X", names(data))]
  
  # drop completely missing rows
  data_for_amelia <- data_X_only[rowSums(is.na(data_X_only)) < ncol(data_X_only), ]
  
  amelia_output <- amelia(data_for_amelia, m = 5, noms =NULL, p2s = 0)
  
  # get mean and variance for each imputed dataset
  est_stats <- sapply(1:5, function(i) {
    complete_data <- amelia_output$imputations[[i]]
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
    cat("Naive (complete case):", mean(data[[variable]], na.rm = TRUE), "\n")
  }
  
  return(list(estimate = pooled_mean, se = sqrt(total_var)))
}
