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
n_points <- 9 # how many images will be there in grid

df_files_sample <- df_files %>% sample_n(n)

p <- create_empty_protocol(1)


for (i in 1:n) {
  
  r <- df_files_sample[i,]
  
  this_cat_ix <- df_files$category == r$category
  
  # close to center ----------------------------------------------------------
  
  gist_point <- gist[r$id, ] %>% select(-id) %>% as.matrix()
  
  gr_close <- select_points_close_to_centerL2(gist[this_cat_ix,], gist_point, n_points = n_points)
  gr_far <- select_points_farthest_from_centerL2(gist[this_cat_ix,], gist_point, n_points = n_points)
  
  # replace the most distant point in close points with the most distant one
  df_for_plot <- rbind(gr_close[-n_points, ], gr_far[1, ]) %>% 
    left_join(df_files, by = "id")

  # we can't use those images for next trials
  keep_ix <- !(df_files$id %in% df_for_plot$id)
  df_files <- df_files[keep_ix,]
  gist <- gist[keep_ix,]
  
 
}

