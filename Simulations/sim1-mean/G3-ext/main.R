library(mice)
library(tidyverse)
library(ggplot2)

# Source 
source("Simulations/DGPs/dgp-G3-ext-mean.R") # DGP
source("Simulations/sim1-mean/G3-ext/est_mice.R") # MICE estimation
source("Simulations/sim1-mean/G3-ext/est_em.R") # Amelia estimation
source("Simulations/sim1-mean/G3-ext/est_flexmissing.R") # flexMissing estimation
devtools::load_all("../flexMissing") # flexMissing package

## --------------------------Prepare the graph-------------------------
G <- make.graph(obs_variables = c(),
                 missing_variables = paste0('X',1:5),
                 missing_indicators = paste0('R',1:5),
                 di_edges = list(c('X1','X2'),c('X2','X3'),c('X1','X3'), c('X3','X4'), c('X5','X4'),
                                 c('X1','R4'), c('X2','R5'),
                                 c('X4','R1'), c('X5','R2'),
                                 c('R5','R4'), c('R5','R3'),
                                 c('R4','R3'),
                                 c('R3','R2'),
                                 c('R2','R1')
                                 )
)

plot(G, vertex.size = 15, save=T,file='./Simulations/sim1-mean/G3-ext/mDAG.png', width = 12, height = 9, units = "in", res = 300)
ID <- f.ID_algorithm(G,T)
plot(ID,vertex.size = 15, save=T,file='./Simulations/sim1-mean/G3-ext/ID.png', width = 12, height = 9, units = "in", res = 300)
f.summarize_ID(ID)

## --------------------------Multiple simulation-------------------------

N = c(500,1000,2000,4000,8000)
nsim=500

set.seed(7)
mice_bias = matrix(NA, nrow=nsim, ncol=length(N))
em_bias = matrix(NA, nrow=nsim, ncol=length(N))
flexmissing_bias = matrix(NA, nrow=nsim, ncol=length(N))
complete_case_bias = matrix(NA, nrow=nsim, ncol=length(N))


vocal=F
variable <- "X3" # variable to estimate the mean of

for (n in N){
  
  cat('n=',n,'\n')
  
  for (i in 1:nsim){
    
    if(i%%100==0)cat(' i =', i, '\n')
    
    dgp_output = dgp(n)
    data = dgp_output$data
    
    mice_output = est_mice(data, variable, vocal=vocal)
    em_output = est_em(data, variable, vocal=vocal)
    flexmissing_output <- suppressWarnings(est_flexmissing(data, variable, vocal=vocal))

    mice_bias[i, which(N==n)] = mice_output$estimate - mean(dgp_output$data_no_missing[[variable]])
    em_bias[i, which(N==n)] = em_output$estimate - mean(dgp_output$data_no_missing[[variable]])
    flexmissing_bias[i, which(N==n)] = flexmissing_output$estimate - mean(dgp_output$data_no_missing[[variable]])
    complete_case_bias[i, which(N==n)] = mean(data[[variable]], na.rm = TRUE) - mean(dgp_output$data_no_missing[[variable]])
    
  }
  
}

vocal=T

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
  ylim(-0.25, 0.5)+
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

ggsave("Simulations/sim1-mean/G3-ext/plot-sim1.pdf", plot = p_bias, width = 12, height = 8, units = "in")

save(list=c('mice_bias','em_bias','flexmissing_bias','complete_case_bias'),
     file="Simulations/sim1-mean/G3-ext/result.Rdata")
