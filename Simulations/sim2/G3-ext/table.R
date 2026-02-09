library(tidyverse)
library(ggplot2)
library(huxtable)

load('Simulations/sim2/G3-ext/results.Rdata')

N <- c(500, 1000, 2000, 4000, 8000)
nsim <- 500
true_beta1 <- 0  # True value of beta1 (X3 ⊥ X1 | X2)
alpha <- 0.05

# Initialize storage matrices for each metric and method
methods <- c("Amelia", "Complete case","MICE", "Missing tree")


# Calculate metrics for each method and sample size
summary_results <- list()

for (method in methods) {
  for (j in 1:length(N)) {
    n <- N[j]
    
    # Extract results for this method and sample size
    estimates <- results[[method]]$estimate[, j]
    ses <- results[[method]]$se[, j]
    ci_lowers <- results[[method]]$ci_lower[, j]
    ci_uppers <- results[[method]]$ci_upper[, j]
    p_values <- results[[method]]$p_value[, j]
    
    # Calculate metrics
    bias <- mean(estimates - true_beta1)
    rmse <- sqrt(mean((estimates - true_beta1)^2))
    type1_error <- mean(p_values < alpha)
    coverage <- mean(ci_lowers <= true_beta1 & ci_uppers >= true_beta1)
    
    # Store
    summary_results[[length(summary_results) + 1]] <- data.frame(
      Method = method,
      n = n,
      Bias = bias,
      RMSE = rmse,
      Type_I_Error = type1_error,
      Coverage = coverage*100,
      stringsAsFactors = FALSE
    )
  }
}

# Combine into one data frame
summary_df <- bind_rows(summary_results)

tab_data <- do.call(rbind, lapply(methods, function(x) {
  t(summary_df[summary_df$Method == x, 3:6])
}))

tab_data <- apply(tab_data, MARGIN=2, FUN=function(x) sprintf("%.3f", x))

tab_data

# make table
library(huxtable)
library(dplyr)
options(scipen = 999)

table1 <- as_hux(tab_data) %>% 
  insert_row("500","1000","2000","4000","8000", after = 0) %>% 
  insert_column(c("n",rep(c('Bias','RMSE','Type I Error','Coverage (%)'),4)), after = 0) %>%
  insert_column(c('','Amelia','','','','Complete case','','','','MICE','','','','Missing tree','','',''), after = 0) %>%
  merge_cells(2:5, 1) %>% 
  merge_cells(6:9, 1) %>%
  merge_cells(10:13, 1) %>%
  merge_cells(14:17, 1) %>%
  set_valign(c(2,6,10,14), col=1,"middle") %>% 
  set_rotation(c(2,6,10,14), 1, 90) %>% 
  set_align(col=1, everywhere, "left") %>%
  set_number_format(everywhere,everywhere, "%s") %>% 
  set_align(col=2:ncol(.),everywhere,"center") %>%
  set_all_padding(2)  %>%
  set_bold(1, everywhere) %>%
  set_bold(everywhere, 1) %>%
  set_top_border(row = c(2,6,10,14), col =2:ncol(.)) %>% 
  set_bottom_border(row = 1, col =2:ncol(.),brdr(0.4, "solid")) %>% 
  set_top_border(row=1,col=everywhere,brdr(1, "solid")) %>% set_bottom_border(row = nrow(.),col = everywhere,brdr(1, "solid")) %>%
  set_font_size(6) %>% 
  set_caption("Comparison on model selection via regression upon applying different missing data methods.")

table1
quick_latex(table1, file = "Simulations/sim2/G3-ext/table.tex")

