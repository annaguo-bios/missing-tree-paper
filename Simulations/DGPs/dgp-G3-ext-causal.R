dgp <- function(n){
  
  vocal <- FALSE
  
  ## -----------------------------
  ## Generate covariates
  ## -----------------------------
  X1 <- rnorm(n, 1, 1); range(X1)
  X2 <- rbinom(n,  1, plogis(3 - 0.6 * abs(X1))); range(X2)
  X3 <- rnorm(n, 2 - 1 * X2^2 + 4*X2 + 2 * X1 * X2, 1.5); range(X3)
  X5 <- rnorm(n, 2, 1); range(X5)
  X4 <- rnorm(n, 5*X5^3 - 5*abs(X3)*X5, 1); range(X4)
  
  ## -----------------------------
  ## Missingness probabilities and indicators
  ## -----------------------------
  probR5 <- plogis(0.8 + 1.5*X2)
  R5 <- rbinom(n, 1, probR5)
  if(vocal) cat("min(probR5) =", round(min(probR5),3)," | P(R5=0) =", round(mean(R5==0),3), "\n")
  
  probR4 <- plogis(0.3 + 1.5*X1 + 2*R5)
  R4 <- rbinom(n, 1, probR4)
  if(vocal) cat("min(probR4) =", round(min(probR4),3)," | P(R4=0) =", round(mean(R4==0),3), "\n")
  
  probR3 <- plogis(-0.8 + 2*R4 + 1.8*R5)
  R3 <- rbinom(n, 1, probR3)
  if(vocal) cat("min(probR3) =", round(min(probR3),3)," | P(R3=0) =", round(mean(R3==0),3), "\n")
  
  probR2 <- plogis(-4 + 1*X5 + 1*R3)
  R2 <- rbinom(n, 1, probR2)
  if(vocal) cat("min(probR2) =", round(min(probR2),3)," | P(R2=0) =", round(mean(R2==0),3), "\n")
  
  probR1 <- plogis(1.2 + 0.01*X4 + 1.5*R2)
  R1 <- rbinom(n, 1, probR1)
  if(vocal) cat("min(probR1) =", round(min(probR1),3)," | P(R1=0) =", round(mean(R1==0),3), "\n")
  
  ## -----------------------------
  ## Observed data
  ## -----------------------------
  data_no_missing <- data.frame(X1=X1, X2=X2, X3=X3, X4=X4, X5=X5, R1=R1, R2=R2, R3=R3, R4=R4, R5=R5)
  
  data <- data_no_missing %>%
    mutate(
      X1 = if_else(R1 == 0, NA_real_, X1),
      X2 = if_else(R2 == 0, NA_real_, X2),
      X3 = if_else(R3 == 0, NA_real_, X3),
      X4 = if_else(R4 == 0, NA_real_, X4),
      X5 = if_else(R5 == 0, NA_real_, X5)
    )
  
  missing_prop <- apply(data, 2, function(x) mean(is.na(x)))
  
  return(list(data = data, data_no_missing = data_no_missing, missing_prop = missing_prop))
  
}