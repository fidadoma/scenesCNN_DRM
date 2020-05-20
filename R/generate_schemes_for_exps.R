set.seed(2005206)
library(tidyverse)
library(here)
theme_set(theme_bw())

plots_dir <- here("plots")

if(!dir.exists(plots_dir)) { dir.create(plots_dir) }


source("R/utils.R")

Sigma <- matrix(c(10,0,0,10),2,2)
m <- MASS::mvrnorm(n = 67, mu=c(0,0), Sigma)
m<- rbind(m,c(0,0))

df <- tibble(x=m[,1], y=m[,2]) %>% 
  mutate(d = sqrt(x^2+y^2))

qs <- compute_quintiles(df$d)

df <- df %>% 
  mutate(quintile = if_else(d < qs[2], 1,
                            if_else(d < qs[3],2,
                                    if_else(d < qs[4],3,
                                            if_else(d < qs[5],4,5)))) %>% as.character()) %>% 
  arrange(d)


df$type <- "remaining images"
df$type[1] <- "selected scene"
df$type[2:9] <- "distractor"
target_locs <- df$type[df$quintile %in% c(2,3,4)]

df$type[df$quintile %in% c(2,3,4)] <- sample(c("target",rep("remaining images",length(target_locs)-1)))

df <- df %>% mutate(type = factor(type, levels = c("selected scene","distractor","target","remaining images")))
p <- ggplot(data = df %>% filter(type=="remaining images"),aes(x = x, y = y, col =quintile, shape = type)) + 
  geom_point(size = 2) + 
  geom_point(data = df %>% filter(type!="remaining images"),size = 4) + 
  scale_shape_manual(values = c(8,16,17,15)) +
  #scale_shape_discrete(solid = T) +
  theme(aspect.ratio = 1) + 
  theme(axis.title.x=element_blank(),
        axis.text.x=element_blank(),
        axis.ticks.x=element_blank(),
        axis.title.y=element_blank(),
        axis.text.y=element_blank(),
        axis.ticks.y=element_blank(),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank())
ggsave(file.path(plots_dir, "Fig_scheme_oddone.svg"), p, width = 6, height = 6)
ggsave(file.path(plots_dir, "Fig_scheme_oddone_noleg.svg"), p + theme(legend.position = "none"), width = 6, height = 6)


# drm scheme --------------------------------------------------------------


set.seed(2005207)
library(tidyverse)

Sigma <- matrix(c(20,0,0,20),2,2)
m <- MASS::mvrnorm(n = 300, mu=c(0,0), Sigma)


df <- tibble(x=m[,1], y=m[,2]) %>% 
  mutate(d = sqrt(x^2+y^2))

med <- median(df$d)

df <- df %>% 
  mutate(median_split = if_else(d < med, "closer", "further")) %>% 
  arrange(d) 

df_targets <- df %>% 
  filter(median_split == "closer") %>% 
  sample_n(15) %>% 
  mutate(type = "target")

targets_center_x <- mean(df_targets$x)
targets_center_y <- mean(df_targets$y)

df_close_dist <- df %>% 
  setdiff(df_targets %>% select(-type)) %>% 
  mutate(targets_center_x = targets_center_x, 
         targets_center_y = targets_center_y) %>% 
  mutate(d_targets = sqrt((x-targets_center_x)^2+(y-targets_center_y)^2)) %>% 
  arrange(d_targets) %>% top_n(-5) %>% 
  mutate(type = "close distractors") %>% 
  select(-targets_center_x,-targets_center_y,-d_targets)

df_far_dist <- df %>% 
  filter(median_split == "further") %>% 
  sample_n(5) %>% 
  mutate(type = "far distractor")
df_trial <- rbind(df_targets, df_close_dist,df_far_dist)

find_hull <- function(df) df[chull(df$x, df$y), ]  
df_target_hull <- df_targets %>% find_hull()
p <- df %>% 
  ggplot(aes(x = x, y = y)) + 
  geom_point(size = 2, alpha = 0.2) +
  geom_point(data = df_trial, aes(col = type), size = 4) + 
  geom_polygon(data = df_target_hull, alpha = 0.2) +
  theme(aspect.ratio = 1) + 
  theme(axis.title.x=element_blank(),
        axis.text.x=element_blank(),
        axis.ticks.x=element_blank(),
        axis.title.y=element_blank(),
        axis.text.y=element_blank(),
        axis.ticks.y=element_blank(),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank())
ggsave(file.path(plots_dir, "Fig_scheme_drm.svg"), p, width = 6, height = 6)
ggsave(file.path(plots_dir, "Fig_scheme_drm_noleg.svg"), p + theme(legend.position = "none"), width = 6, height = 6)


