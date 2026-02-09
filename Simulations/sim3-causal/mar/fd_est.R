fd_est <- function(data){
  
  ps_model <- glm("X3~X1", data = data, family = binomial)
  
  mediator_model <- glm("X4~X1+X3", data = data, family = gaussian)
  
  outcome_model <- glm("X5~X1+X4", data = data, family = gaussian)
  
}