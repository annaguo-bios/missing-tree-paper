library(mice)
library(tidyverse)
library(ggplot2)

# Source 
source("Simulations/DGPs/dgp-mar-causal.R") # DGP
source("Simulations/sim3-causal/mar/bd_est.R") # MICE estimation
source("Simulations/sim3-causal/mar/est_mice.R") # MICE estimation
source("Simulations/sim3-causal/mar/est_em.R") # Amelia estimation
source("Simulations/sim3-causal/mar/est_flexmissing.R") # flexMissing estimation
devtools::load_all("../flexMissing") # flexMissing package


## --------------------------useful functions-------------------------
product_NA0 <- function(w,v){ # weight and value
  
  # product_NA0: compute 0*NA as 0 instead of NA
  #
  # Args:
  #   x: numeric vector
  #   w: numeric vector of weights
  #
  # Returns:
  #   weighted product with NA treated as 0
  
  return(ifelse(w == 0 | v == 0, 0, w * v))
  
}

## --------------------------Prepare the graph-------------------------
G <- make.graph(obs_variables = c('X1'),
                missing_variables = c('X2','X3'),
                missing_indicators = c('R2','R3'),
                di_edges = list(c('X1','X2'),c('X2','X3'),c('X1','X3'),
                                c('X1','R2'), 
                                c('X1','R3'),
                                c('R3','R2'))
)

plot(G)
ID <- f.ID_algorithm(G,T)
plot(ID,vertex.size = 20)


## --------------------------Simulation-------------------------

nsim=20

N = c(500,1000,2000,4000,8000)
nsim=500

set.seed(7)
mice_bias = matrix(NA, nrow=nsim, ncol=length(N))
em_bias = matrix(NA, nrow=nsim, ncol=length(N))
flexmissing_bias = matrix(NA, nrow=nsim, ncol=length(N))
complete_case_bias = matrix(NA, nrow=nsim, ncol=length(N))


vocal=F
truth <- -2

for (n in N){
  
  cat('n=',n,'\n')
  
  for (i in 1:nsim){
    
    if(i%%100==0)cat('i =', i, '\n')
    
    dgp_output = dgp(n)
    data = dgp_output$data
    
    mice_output <- est_mice(data, vocal=vocal)
    em_output <- est_em(data, vocal=vocal)
    flexmissing_output <- suppressWarnings(est_flexmissing(data, vocal=vocal))
    complete_output <- bd_est(na.omit(data), 
                             magic_weight_ps = rep(1,nrow(na.omit(data))), 
                             magic_weight_outcome = rep(1,nrow(na.omit(data)))
    )
    
    mice_output$estimate
    em_output$estimate
    flexmissing_output$estimate
    complete_output$estimate
    
    mice_bias[i, which(N==n)] = mice_output$estimate - truth
    em_bias[i, which(N==n)] = em_output$estimate - truth
    flexmissing_bias[i, which(N==n)] = flexmissing_output$estimate - truth
    complete_case_bias[i, which(N==n)] = complete_output$estimate - truth
    
  }
  
}


dat_plot <- bind_rows(
  as.data.frame(mice_bias) %>%
    setNames(N) %>%
    pivot_longer(cols = everything(), names_to = "n", values_to = "bias") %>%
    mutate(n = as.integer(n), Method = "MICE"),
  
  as.data.frame(em_bias) %>%
    setNames(N) %>%
    pivot_longer(cols = everything(), names_to = "n", values_to = "bias") %>%
    mutate(n = as.integer(n), Method = "Amelia"),
  
  as.data.frame(flexmissing_bias) %>%
    setNames(N) %>%
    pivot_longer(cols = everything(), names_to = "n", values_to = "bias") %>%
    mutate(n = as.integer(n), Method = "Missing tree"),
  
  as.data.frame(complete_case_bias) %>%
    setNames(N) %>%
    pivot_longer(cols = everything(), names_to = "n", values_to = "bias") %>%
    mutate(n = as.integer(n), Method = "Complete case")
)


# Plot
p_bias <- ggplot(dat_plot, aes(x = factor(n), y = bias, fill = Method)) +
  geom_boxplot(coef = 3, position = position_dodge(width = 0.75)) +
  ylim(-1, 0.75)+
  geom_hline(yintercept = 0, linetype = "dashed", color = "black") +
  labs(x = "Sample Size", y = "Bias", fill = "Methods") +
  theme(
    axis.title.x = element_text(size = 16),
    axis.title.y = element_text(size = 16),
    axis.text.x = element_text(size = 14),
    axis.text.y = element_text(size = 14)
  )+
  theme_bw()

p_bias

ggsave("Simulations/sim3-causal/mar/plot-sim3-bd.pdf", plot = p_bias, width = 12, height = 8, units = "in")

save(list=c('mice_bias','em_bias','flexmissing_bias','complete_case_bias'),
     file="Simulations/sim3-causal/mar/result.Rdata")
