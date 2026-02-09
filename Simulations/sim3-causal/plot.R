library(tidyverse)
library(ggplot2)

mDAGs <- c(
  mar         = "G1",
  G1sub       = "G2",
  `G3-ext`    = "G3",
  permutation = "G4"
)


ymins = c(-0.75, -2.5, -4, -4)
ymaxs = c(0.6, 1, 4, 4)

N = c(500,1000,2000,4000,8000)
nsim=500

for (m in names(mDAGs)) {
  
  load(file.path(
    "Simulations/sim3-causal",
    m,
    "result.Rdata"
  ))
  
  dat_m <- bind_rows(
    as.data.frame(mice_bias) %>%
      setNames(N) %>%
      pivot_longer(everything(), names_to = "n", values_to = "bias") %>%
      mutate(n = as.integer(n), Method = "MICE"),
    
    as.data.frame(em_bias) %>%
      setNames(N) %>%
      pivot_longer(everything(), names_to = "n", values_to = "bias") %>%
      mutate(n = as.integer(n), Method = "Amelia"),
    
    as.data.frame(flexmissing_bias) %>%
      setNames(N) %>%
      pivot_longer(everything(), names_to = "n", values_to = "bias") %>%
      mutate(n = as.integer(n), Method = "Missing tree"),
    
    as.data.frame(complete_case_bias) %>%
      setNames(N) %>%
      pivot_longer(everything(), names_to = "n", values_to = "bias") %>%
      mutate(n = as.integer(n), Method = "Complete case")
  )
  
  panel_label <- mDAGs[m]
  
  # Plot
  p_bias <- ggplot(dat_m, aes(x = factor(n), y = bias, fill = Method)) +
    geom_boxplot(coef = 3, position = position_dodge(width = 0.75)) +
    ylim(ymins[which(m == names(mDAGs))], ymaxs[which(m == names(mDAGs))])+
    geom_hline(yintercept = 0, linetype = "dashed", color = "black") +
    annotate(
      "text",
      x = Inf, y = Inf,
      label = panel_label,
      hjust = 1.1, vjust = 1.3,
      size = 4, fontface = "bold"
    )+
    theme_bw()+
    labs(x = NULL, y = NULL, fill = "Methods") +
    theme(
      axis.title.x = element_blank(),
      axis.title.y = element_blank(),
      axis.text.x = element_text(size = 10),
      axis.text.y = element_text(size = 10),
      legend.text  = element_text(size = 10),
      legend.title = element_text(size = 12)
    )
  
  assign(
    paste0("p", which(m == names(mDAGs))),
    p_bias
  )
  
  
}

library(cowplot)

legend <- get_legend(
  p1 + theme(legend.position = "bottom")
)

prow <- plot_grid(
  p1 + theme(legend.position = "none"),
  p2 + theme(legend.position = "none"),
  p3 + theme(legend.position = "none"),
  p4 + theme(legend.position = "none"),
  ncol = 2
)

final_plot <- plot_grid(
  legend,
  prow,
  ncol = 1,
  rel_heights = c(0.12, 1)
) + theme(
  plot.margin = margin(t = 10, r = 10, b = 20, l = 20)
)

final_plot <- ggdraw(final_plot) +
  draw_label(
    "Sample Size",
    x = 0.5, y = 0.02,
    vjust = 0,
    size = 12
  ) +
  draw_label(
    "Bias",
    x = 0.02, y = 0.5,
    angle = 90,
    vjust = 1,
    size = 12
  )

final_plot

ggsave("Simulations/sim3-causal/plot-sim3.pdf", plot = final_plot, width = 12, height = 8, units = "in")
