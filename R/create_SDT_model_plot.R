# Create SDT plot example

library(tidyverse); theme_set(theme_classic(16))
library(here)
plots_dir <- here::here("plots")

source("utils.R")

x0 <- (-400:400)/100
d <- 2
y_noise   <- dnorm(x0, -d/2, 1)
y_signal  <- dnorm(x0, d/2, 1)

df_plot <- tibble(x = rep(x0,2), y = c(y_noise, y_signal), type = rep(c("noise","signal+noise"), each = length(x0)))

p_SDT_model <- 
  df_plot %>% 
  ggplot(aes(x = x, y = y, col = type)) + 
  geom_path(size = 1.2) + 
  geom_segment(x = 0, xend = 0, y = 0, yend = 0.4, col = "black") +
  geom_segment(x = 1, xend = 1, y = 0, yend = 0.4, col = "black", linetype ="dashed") +
  theme(aspect.ratio = 1) + 
  ylim(0,0.5) +
  annotate("text", x = 0, y = 0.45, label = "d' = 2", size = 6) + 
  annotate("text", x = -3, y = 0.3, label = "c = 0", size = 6) + 
  annotate("text", x = 3, y = 0.3, label = "c = 1", size = 6) + 
  geom_segment(x = -d/2, xend = d/2, y = 0.41, yend = 0.41, col = "black",arrow = arrow(length = unit(0.4,"cm"), ends = "both")) +
  geom_segment(x = -2.8, xend = -0.1, y = 0.27, yend = 0.2, col = "black",arrow = arrow(length = unit(0.4,"cm"))) +
  geom_segment(x = 2.8, xend = 1.1, y = 0.27, yend = 0.2, col = "black",arrow = arrow(length = unit(0.4,"cm"))) +
  xlab("Familiarity")
  
p_SDT_model

ggsave(file.path(plots_dir, "Fig_chp2_sdtmodel.svg"), p_SDT_model, width = 6, height = 6)


## ROC analysis

df1 <- tibble(H = c(0.8,0.45), FA = c(0.6,0.55), label = c("P1", "P2")) 

p_roc <- plot_normal_ROC(df1)

ggsave(file.path(plots_dir, "Fig_chp2_roc.svg"), p_roc, width = 6, height = 6)

