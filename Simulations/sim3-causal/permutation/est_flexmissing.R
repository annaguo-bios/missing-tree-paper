est_flexmissing <- function(data,vocal=F){
  
  ## propensity scores
  propensitys <- f.propensity(graph=G,
                              data=data,
                              ID=ID,
                              law='target', # 'target' or 'full'
                              link.R="logit",
                              formula.R=NULL,
                              vocal=vocal)
  
  clique_ps <- find_clique(graph=G,
                      data=data,
                      vocal=vocal,
                      propensitys=propensitys,
                      est_R=c('R1','R2'),
                      non_ID_R=NULL)
  
  clique_outcome <- find_clique(graph=G,
                            data=data,
                            vocal=vocal,
                            propensitys=propensitys,
                            est_R=c('R1','R2','R3'),
                            non_ID_R=NULL)
  
  
  magic_weight_ps <- clique_ps$magic_weight
  fit_rows_ps <- clique_ps$fit_rows
  clique_ps$clique
  
  
  
  magic_weight_outcome <- clique_outcome$magic_weight
  fit_rows_outcome <- clique_outcome$fit_rows
  clique_outcome$clique
  
  est_output <- bd_est(data, magic_weight_ps, magic_weight_outcome)
  est <- est_output$estimate
  est_se <- est_output$se
  
  
  if(vocal){
    cat("\nEstimated target:", est, "SE:", est_se, "\n")
    cat("True target:", 0.5, "\n")
  }
  
  return(list(estimate = est, se = est_se))
  
}
