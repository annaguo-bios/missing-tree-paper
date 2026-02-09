args = commandArgs(trailingOnly=T)
n=as.integer(args[1]) # sample size for the simulation
seed=as.integer(args[2])  # seeds for replication

library(tidyverse)
library(ggplot2)
library(huxtable)
library(mice)

# Source 
source("../../DGPs/dgp-permutation-test.R") # DGP
source("est_mice.R") # MICE estimation
source("est_em.R") # Amelia estimation
source("est_flexmissing.R") # flexMissing estimation
source("est_complete.R") # flexMissing estimation
devtools::load_all("../../flexMissing") # flexMissing package


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

ID <- f.ID_algorithm(G,F)


plot(G, vertex.size = 10)

## --------------------------Multiple simulation-------------------------

set.seed(seed)

true_beta1 <- 0  # True value of beta1 (X3 ⊥ X1 | X2)
alpha <- 0.05

# Initialize storage matrices for each metric and method
methods <- c("Amelia", "Complete case","MICE", "Missing tree")

# Create a list to store results for each method
results <- list()
for (method in methods) {
  results[[method]] <- list(
    estimate = NA,
    se = NA,
    ci_lower = NA,
    ci_upper = NA,
    p_value = NA
  )
}



# Generate data
dgp_output <- dgp(n)
dgp_output$missing_prop
data <- dgp_output$data

# Get estimates from each method
mice_output <- est_mice(data)
amelia_output <- est_em(data)
flexmissing_output <- suppressWarnings(est_flexmissing(data,B=5000))
cca_output <- est_complete(data)

mice_output$estimate
amelia_output$estimate
flexmissing_output$estimate
cca_output$estimate



# MICE
results[["MICE"]]$estimate <- mice_output$estimate
results[["MICE"]]$se <- mice_output$se
results[["MICE"]]$ci_lower <- mice_output$ci_lower
results[["MICE"]]$ci_upper <- mice_output$ci_upper
results[["MICE"]]$p_value <- mice_output$p_value

# Amelia
results[["Amelia"]]$estimate <- amelia_output$estimate
results[["Amelia"]]$se <- amelia_output$se
results[["Amelia"]]$ci_lower <- amelia_output$ci_lower
results[["Amelia"]]$ci_upper <- amelia_output$ci_upper
results[["Amelia"]]$p_value <- amelia_output$p_value

# FlexMissing
results[["Missing tree"]]$estimate <- flexmissing_output$estimate
results[["Missing tree"]]$se <- flexmissing_output$se
results[["Missing tree"]]$ci_lower <- flexmissing_output$ci_lower
results[["Missing tree"]]$ci_upper <- flexmissing_output$ci_upper
results[["Missing tree"]]$p_value <- flexmissing_output$p_value

# Complete Case
results[["Complete case"]]$estimate <- cca_output$estimate
results[["Complete case"]]$se <- cca_output$se
results[["Complete case"]]$ci_lower <- cca_output$ci_lower
results[["Complete case"]]$ci_upper <- cca_output$ci_upper
results[["Complete case"]]$p_value <- cca_output$p_value


save(results, file = paste0("output/output_", n, "_", seed, ".Rdata"))
