library(tidyverse)
library(ggplot2)
library(huxtable)

mDAGs <- c(
  mar         = "G1",
  G1sub       = "G2",
  `G3-ext`    = "G3",
  permutation = "G4"
)

N <- c(500, 1000, 2000, 4000, 8000)
nsim <- 500
true_beta1 <- 0  # True value of beta1 (X3 ⊥ X1 | X2)
alpha <- 0.05
methods <- c("Amelia", "Complete case","MICE", "Missing tree")

# save results for all plots
all_results <- list()

for (i in seq_along(mDAGs)) {
  
  load(paste0("Simulations/sim2/", names(mDAGs)[i], "/results.Rdata"))
  
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
  
  # combine into one data frame
  summary_df <- bind_rows(summary_results)
  
  # transpose long to wide
  tab_data <- do.call(rbind, lapply(methods, function(x) {
    t(summary_df[summary_df$Method == x, 3:6])
  }))
  
  all_results[[i]] <- tab_data
  
}

# data for making table
tab_data <- bind_cols(all_results)

# keep three decimal place
tab_data <- apply(tab_data, MARGIN=2, FUN=function(x) sprintf("%.3f", x))

tab_data

# make table
library(huxtable)
library(dplyr)
options(scipen = 999)

table1 <- as_hux(tab_data) %>% 
  insert_row("G1","","","","","G2","","","","","G3","","","","","G4","","","","", after = 0) %>% 
  insert_row(rep(c("500","1000","2000","4000","8000"),4), after = 1) %>% 
  insert_column(c("","n",rep(c('Bias','RMSE','Type I Error','Coverage (%)'),4)), after = 0) %>%
  insert_column(c('','','Amelia','','','','Complete case','','','','MICE','','','','Missing tree','','',''), after = 0) %>%
  merge_cells(1,3:7) %>% 
  merge_cells(1,8:12) %>%
  merge_cells(1,13:17) %>%
  merge_cells(1,18:22) %>% # merge cells of G1 to G4
  set_align(1, everywhere, "center") %>% # center G1 to G4
  merge_cells(3:6, 1) %>% 
  merge_cells(7:10, 1) %>%
  merge_cells(11:14, 1) %>%
  merge_cells(15:18, 1) %>% # merge cells of methods
  set_valign(c(3,7,11,15), col=1,"middle") %>% 
  set_rotation(c(3,7,11,15), 1, 90) %>% # center and rotate methods
  set_align(col=1, everywhere, "left") %>%
  set_number_format(everywhere,everywhere, "%s") %>% # set number format
  set_align(col=2:ncol(.),everywhere,"center") %>%
  set_all_padding(-1)  %>%
  set_bold(1, everywhere) %>%
  set_bold(everywhere, 1) %>%
  set_top_border(row = c(3,7,11,15), col =2:ncol(.)) %>% 
  set_bottom_border(row = 1, col =2:ncol(.),brdr(0.4, "solid")) %>% 
  set_top_border(row=1,col=everywhere,brdr(1, "solid")) %>% set_bottom_border(row = nrow(.),col = everywhere,brdr(1, "solid")) %>%
  set_right_border(3:nrow(.), 7, brdr(0.4,"double")) %>%
  set_right_border(3:nrow(.), 12, brdr(0.4,"double")) %>%
  set_right_border(3:nrow(.), 17, brdr(0.4,"double")) %>%
  set_font_size(7.5) %>% 
  set_top_padding(c(3,7,11,15), everywhere, 5) %>%
  set_bottom_padding(c(6,10,14,18), everywhere, 5) %>%
  set_caption("Results of tests for conditional independence between $X_3$ and $X_1$ given $X_2$ under different missing data methods.")

table1
quick_latex(table1, file = "Simulations/sim2/table.tex")

