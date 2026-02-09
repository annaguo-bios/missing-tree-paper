library(mice)
library(tidyverse)
library(ggplot2)

# Source 
source("Simulations/DGPs/dgp-permutation-causal.R") # DGP
source("Simulations/sim3-causal/permutation/est_mice.R") # MICE estimation
source("Simulations/sim3-causal/permutation/est_em.R") # Amelia estimation
source("Simulations/sim3-causal/permutation/est_flexmissing.R") # flexMissing estimation
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
G <- make.graph(obs_variables = c(),
                 missing_variables = paste0('X',1:10),
                 missing_indicators = paste0('R',1:10),
                 di_edges = list(c('X1','X2'),c('X2','X3'),c('X3','X4'),c('X4','X5'),
                                 c('X5','X6'),c('X6','X7'),c('X7','X8'),
                                 c('X8','X9'),c('X9','X10'),
                                 c('X1','X3'), c('X2','X4'), c('X3','X5'), c('X4','X6'),
                                 c('X5','X7'), c('X6','X8'), c('X7','X9'), c('X8','X10'),
                                 c('X1','R2'), c('X1','R3'),
                                 c('X2','R3'), c('X2','R4'),
                                 c('X3','R4'), c('X3','R5'),
                                 c('X4','R5'), c('X4','R6'),
                                 c('X5','R6'), c('X5','R7'),
                                 c('X6','R7'), c('X6','R8'),
                                 c('X7','R8'), c('X7','R9'),
                                 c('X8','R9'), c('X8','R10'),
                                 c('X9','R10'),
                                 c('R2','R1'), c('R3','R1'), c('R4','R1'), c('R5','R1'), c('R6','R1'), c('R7','R1'), c('R8','R1'), c('R9','R1'), c('R10','R1'),
                                 c('R3','R2'), c('R4','R2'), c('R5','R2'), c('R6','R2'), c('R7','R2'), c('R8','R2'), c('R9','R2'), c('R10','R2'),
                                 c('R4','R3'), c('R5','R3'), c('R6','R3'), c('R7','R3'), c('R8','R3'), c('R9','R3'), c('R10','R3'),
                                 c('R5','R4'), c('R6','R4'), c('R7','R4'), c('R8','R4'), c('R9','R4'), c('R10','R4'),
                                 c('R6','R5'), c('R7','R5'), c('R8','R5'), c('R9','R5'), c('R10','R5'),
                                 c('R7','R6'), c('R8','R6'), c('R9','R6'), c('R10','R6'),
                                 c('R8','R7'), c('R9','R7'), c('R10','R7'),
                                 c('R9','R8'), c('R10','R8'),
                                 c('R10','R9')
                                 
                 )
)

plot(G, vertex.size = 5, curvature_factor = 0.1,save=T, file='./Simulations/sim3-causal/permutation/mDAG.png', width = 30, height = 30, units = "in", res = 300)
ID <- f.ID_algorithm(G,F)
plot(ID,vertex.size = 1, save=T, file='./Simulations/sim3-causal/permutation/ID.png', width = 30, height = 30, units = "in", res = 300)
f.summarize_ID(ID)

# # --------------------------Test on one dataset-------------------------
# set.seed(7)
# n <- 1000
# dgp_output <- dgp(n)
# data <- dgp_output$data
# dgp_output$missing_prop
# 
# variable <- "X3" # variable to estimate the mean of
# mice_output <- est_mice(data, variable, vocal=T)
# em_output <- est_em(data, variable, vocal=T)
# flexmissing_output <- suppressMessages(suppressWarnings(est_flexmissing(data, variable, vocal=T)))
# complete_output <- mean(data[[variable]], na.rm = T)
# cat('Estimated mean:', complete_output, '\nTrue mean:', mean(dgp_output$data_no_missing[[variable]]), '\n')


## --------------------------Multiple simulation-------------------------

# N = c(500,1000,2000)
nsim=20

N = c(500,1000,2000,4000,8000)
nsim=500

set.seed(7)
mice_bias = matrix(NA, nrow=nsim, ncol=length(N))
em_bias = matrix(NA, nrow=nsim, ncol=length(N))
flexmissing_bias = matrix(NA, nrow=nsim, ncol=length(N))
complete_case_bias = matrix(NA, nrow=nsim, ncol=length(N))


vocal=F
truth = 5
for (n in N){
  
  cat('n=',n,'\n')
  
  for (i in 1:nsim){
    
    if(i%%100==0)cat(' i =', i, '\n')
    
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
  ylim(-4, 4)+
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

ggsave("Simulations/sim3-causal/permutation/plot-sim3-bd.pdf", plot = p_bias, width = 12, height = 8, units = "in")

save(list=c('mice_bias','em_bias','flexmissing_bias','complete_case_bias'),
     file="Simulations/sim3-causal/permutation/result.Rdata")
