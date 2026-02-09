dgp <- function(n){
  
  vocal <- F
  
  ## -----------------------------
  ## Generate covariates
  ## -----------------------------
  X1 <- rnorm(n, 1, 1); range(X1)
  X2 <- rbinom(n, 1, plogis(3 - 0.6 * abs(X1))); range(X2)
  X3 <- rnorm(n, 2 - 1 * X2^2 + 4*X2 + 2 * X1 * X2, 1.5); range(X3)
  X4 <- rnorm(n, 1 + X3 - 0.5*X2, 1); range(X4)
  X5 <- rnorm(n, 1 + 0.9*X4 - 0.4*X3, 1); range(X5)
  X6 <- rnorm(n, 1 + 0.8*X5 - 0.3*X4, 1); range(X6)
  X7 <- rnorm(n, 1 + 0.7*X6 - 0.2*X5, 1); range(X7)
  X8 <- rnorm(n, 1 + 0.7*X7 - 0.2*X6, 1); range(X8)
  X9 <- rnorm(n, 1 + 0.7*X8 - 0.2*X7, 1); range(X9)
  X10 <- rnorm(n, 1 + 0.7*X9 - 0.2*X8, 1); range(X10)
  
  ## -----------------------------
  ## Missingness probabilities and indicators
  ## -----------------------------
  probR10 <- plogis(2 - 1*X9 + 1*X8)
  R10 <- rbinom(n, 1, probR10)
  if(vocal) cat("min(probR10) =", round(min(probR10),3)," | P(R10=0) =", round(mean(R10==0),3), "\n")
  
  probR9 <- plogis(2 - 2*X8 + 2*X7 - 0.1*R10)
  R9 <- rbinom(n, 1, probR9)
  if(vocal) cat("min(probR9) =", round(min(probR9),3)," | P(R9=0) =", round(mean(R9==0),3), "\n")
  
  probR8 <- plogis(2 - 2*X7 + 2*X6 - 0.1*R9 + 0.1*R10)
  R8 <- rbinom(n, 1, probR8)
  if(vocal) cat("min(probR8) =", round(min(probR8),3)," | P(R8=0) =", round(mean(R8==0),3), "\n")
  
  probR7 <- plogis(2 - 1*X6 + 1*X5 - 0.1*R8 + 0.1*R9 - 0.1*R10)
  R7 <- rbinom(n, 1, probR7)
  if(vocal) cat("min(probR7) =", round(min(probR7),3)," | P(R7=0) =", round(mean(R7==0),3), "\n")
  
  probR6 <- plogis(2 - 1*X5 + 1*X4 - 0.1*R7 + 0.1*R8 - 0.1*R9 + 0.1*R10)
  R6 <- rbinom(n, 1, probR6)
  if(vocal) cat("min(probR6) =", round(min(probR6),3)," | P(R6=0) =", round(mean(R6==0),3), "\n")
  
  probR5 <- plogis(10 - 1*X4 + 0.2*X3 - 0.1*R6 + 0.1*R7 - 0.1*R8 + 0.1*R9 - 0.1*R10)
  R5 <- rbinom(n, 1, probR5)
  if(vocal) cat("min(probR5) =", round(min(probR5),3)," | P(R5=0) =", round(mean(R5==0),3), "\n")
  
  probR4 <- plogis(10 - 1*X3 + 0.2*X2 - 0.1*R5 + 0.1*R6 - 0.1*R7 + 0.1*R8 - 0.1*R9 + 0.1*R10)
  R4 <- rbinom(n, 1, probR4)
  if(vocal) cat("min(probR4) =", round(min(probR4),3)," | P(R4=0) =", round(mean(R4==0),3), "\n")
  
  probR3 <- plogis(0.1 - 0.3*X2 + 0.3*X1 - 0.1*R4 + 0.1*R5 - 0.1*R6 + 0.1*R7 - 0.1*R8 + 0.1*R9 - 0.1*R10)
  R3 <- rbinom(n, 1, probR3)
  if(vocal) cat("min(probR3) =", round(min(probR3),3)," | P(R3=0) =", round(mean(R3==0),3), "\n")
  
  probR2 <- plogis(0.1 + 0.3*X1 - 0.1*R3 + 0.1*R4 - 0.1*R5 + 0.1*R6 - 0.1*R7 + 0.1*R8 - 0.1*R9 + 0.1*R10)
  R2 <- rbinom(n, 1, probR2)
  if(vocal) cat("min(probR2) =", round(min(probR2),3)," | P(R2=0) =", round(mean(R2==0),3), "\n")
  
  probR1 <- plogis(-0.1 + 0.1*R2 - 0.1*R3 + 0.1*R4 - 0.1*R5 + 0.1*R6 - 0.1*R7 + 0.1*R8 - 0.1*R9 + 0.1*R10)
  R1 <- rbinom(n, 1, probR1)
  if(vocal) cat("min(probR1) =", round(min(probR1),3)," | P(R1=0) =", round(mean(R1==0),3), "\n")
  
  ## -----------------------------
  ## Observed data
  ## -----------------------------
  data_no_missing <- data.frame(X1=X1, X2=X2, X3=X3, X4=X4, X5=X5, X6=X6, X7=X7, X8=X8, X9=X9, X10=X10,
                                R1=R1, R2=R2, R3=R3, R4=R4, R5=R5, R6=R6, R7=R7, R8=R8, R9=R9, R10=R10)
  
  data <- data_no_missing %>%
    mutate(
      X1 = if_else(R1 == 0, NA_real_, X1),
      X2 = if_else(R2 == 0, NA_integer_, X2),
      X3 = if_else(R3 == 0, NA_real_, X3),
      X4 = if_else(R4 == 0, NA_real_, X4),
      X5 = if_else(R5 == 0, NA_real_, X5),
      X6 = if_else(R6 == 0, NA_real_, X6),
      X7 = if_else(R7 == 0, NA_real_, X7),
      X8 = if_else(R8 == 0, NA_real_, X8),
      X9 = if_else(R9 == 0, NA_real_, X9),
      X10 = if_else(R10 == 0, NA_real_, X10)
    )
  
  missing_prop <- apply(data, 2, function(x) mean(is.na(x)))
  
  return(list(data = data, data_no_missing = data_no_missing, missing_prop = missing_prop))
  
}