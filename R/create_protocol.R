rm(list = ls())

set.seed(180620)

# prepare data ------------------------------------------------------------

library(tidyverse)
library(FDhelpers)
library(imager)

source(here::here("R", "utils.R"))

outdir <- here::here("data","protocols")

if(!dir.exists(outdir)) {
  dir.create(outdir)
}


gist <- read_csv(here::here("data", "gist_figrim_all.csv"), col_names = F)
load(here::here("data", "file_info.RData"))

df_files <- df_files %>% dplyr::mutate(id = 1:n())
gist$id <- 1:nrow(gist)

n <- 100

df_files_sample <- df_files %>% sample_n(n)

for (i in 1:n) {
  
  r <- df_files_sample[i,]
  
  this_cat_ix <- df_files$category == r$category
  
  # close to center ----------------------------------------------------------
  
  gist_point <- gist[r$id, ] %>% select(-id) %>% as.matrix()
  
  cl2 <- select_points_close_to_centerL2(gist[this_cat_ix,], gist_point)
  cl3 <- select_points_farthest_from_centerL2(gist[this_cat_ix,], gist_point)
  
  df_for_plot <- rbind(cl2[-9, ], cl3[1, ])
  
  df_for_plot <- df_for_plot %>% left_join(df_files, by = "id")
  
  df_for_plot <- df_for_plot %>%
    rowwise() %>%
    dplyr::mutate(img_data = list(jpeg::readJPEG(pth))) %>%
    ungroup()
  
  # MDS check
  
  mds_onecat <- cmdscale(dist(df_for_plot %>% select(-(id:img_data)))) %>% as.data.frame()
  colnames(mds_onecat) <- c("x","y")
  mds_onecat$close <- T
  mds_onecat$close[df_for_plot$id == cl3$id] <- F
  
  p <- mds_onecat %>%
    ggplot(aes(x, y, col = close)) +
    geom_point() +
    theme(aspect.ratio = 1)
  ggsave(file.path(outdir, sprintf("%s_mds_gist.png", tools::file_path_sans_ext(r$filename))), p)
  
  # we can't use those images for next trials
  keep_ix <- !(df_files$id %in% df_for_plot$id)
  df_files <- df_files[keep_ix,]
  gist <- gist[keep_ix,]
  
 
}

