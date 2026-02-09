est_flexmissing <- function(data,variable,vocal=F){
  
  ## propensity scores
  propensitys <- f.propensity(graph=G,
                              data=data,
                              ID=ID,
                              law='target', # 'target' or 'full'
                              link.R="logit",
                              formula.R=NULL,
                              crossfit=F,
                              K=5,
                              superlearner=F,
                              lib.SL =c("SL.glm","SL.earth","SL.ranger","SL.mean"),
                              vocal=vocal)
  
  
  clique <- find_clique(G,
                      data,
                      vocal=vocal,
                      propensitys,
                      est_R=sub("X", "R", variable),
                      non_ID_R=NULL)

  data$magic_weight <- clique$magic_weight
  fit_rows <- clique$fit_rows
  clique$clique

  model <- glm(paste0(variable," ~ 1"),
             data=data[fit_rows,],
             family=gaussian(),
             weights=magic_weight)
  
  summary(model)
  
  est <- coef(model)
  est_se <- coef(summary(model))[, "Std. Error"]
  
  if(vocal){
    cat("\nEstimated mean:", est, "SE:", est_se, "\n")
    cat("True mean:", mean(dgp_output$data_no_missing[[variable]]), "\n")
    cat("Naive (complete case):", mean(data[[variable]], na.rm = TRUE), "\n")
  }
  
  return(list(estimate = est, se = est_se))
  
}
