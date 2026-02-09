dgp <- function(n){
  
  vocal <- FALSE
  
  ## -----------------------------
  ## Generate covariates
  ## -----------------------------
  X1 <- rnorm(n, 1, 1); range(X1)
  X2 <- rnorm(n, 3 - 0.6 * abs(X1), 1); range(X2)
  X3 <- rnorm(n, 2 - 1 * X2^2 + 4*X2 + 0 * X1 * X2, 1.5); range(X3)
  
  ## -----------------------------
  ## Missingness probabilities and indicators
  ## -----------------------------
  probR3 <- plogis(3 + 0.5 * X1 - 1 * X2)
  R3 <- rbinom(n, 1, probR3)
  if(vocal) cat("min(probR3) =", round(min(probR3), 3), " | P(R3=0) =", round(mean(R3 == 0), 3), "\n")
  
  probR2 <- plogis(0.0 - 0.5 * X1 + 0.15 * X3)
  R2 <- rbinom(n, 1, probR2)
  if(vocal) cat("min(probR2) =", round(min(probR2), 3), " | P(R2=0) =", round(mean(R2 == 0), 3), "\n")
  
  probR1 <- plogis(1 + 1.0 * R2 + 1.0 * R3)
  R1 <- rbinom(n, 1, probR1)
  if(vocal) cat("min(probR1) =", round(min(probR1), 3), " | P(R1=0) =", round(mean(R1 == 0), 3), "\n")
  
  ## -----------------------------
  ## Observed data
  ## -----------------------------
  data_no_missing <- data.frame(X1 = X1, X2 = X2, X3 = X3, R1 = R1, R2 = R2, R3 = R3)
  
  data <- data_no_missing %>%
    mutate(
      X1 = if_else(R1 == 0, NA_real_, X1),
      X2 = if_else(R2 == 0, NA_real_, X2),
      X3 = if_else(R3 == 0, NA_real_, X3)
    )
  
  missing_prop <- apply(data, 2, function(x) mean(is.na(x)))
  
  return(list(data = data, data_no_missing = data_no_missing, missing_prop = missing_prop))
}