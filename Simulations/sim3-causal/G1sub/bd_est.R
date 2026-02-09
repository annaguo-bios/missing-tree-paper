bd_est <- function(data, magic_weight_ps, magic_weight_outcome){
  
  # Calculate the causal effect of X2 on X3 via back-door criterion, adjusting for X1
  
  ps_model <- glm(X2~I(X1^2), data = data, family = binomial(), weights=magic_weight_ps)
  
  outcome_model <- glm(X3~I(X1*X2)+X2, data = data, family = gaussian(), weights=magic_weight_outcome)
  
  ps_1 <- predict(ps_model,newdata = data, type='response')
  mu_1 <- predict(outcome_model, newdata=data %>% mutate(X2=1), type='response')
  mu_0 <- predict(outcome_model, newdata=data %>% mutate(X2=0), type='response')
  
  est_a1 <- with(data, mean(product_NA0((X2/ps_1) * (X3 - mu_1) + mu_1, magic_weight_outcome)))
  est_a0 <- with(data, mean(product_NA0(((1 - X2)/(1 - ps_1)) * (X3 - mu_0) + mu_0, magic_weight_outcome)))
  
  IF_a1 <- with(data, (X2/ps_1) * (X3 - mu_1) + mu_1 - est_a1)
  IF_a0 <- with(data, ((1 - X2)/(1 - ps_1)) * (X3 - mu_0) + mu_0 - est_a0)
  
  ATE <- est_a1 -  est_a0
  IF_ATE <- IF_a1 - IF_a0
  var_ATE <- mean(product_NA0(IF_ATE, magic_weight_outcome)^2) / nrow(data)
  
  return(list(estimate=ATE, se=sqrt(var_ATE)))
  
}
