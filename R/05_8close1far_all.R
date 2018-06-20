# in this variation - select randomly some images from the parts of mds space, its close neigboroughs and one image fruthers away from the center of the group

rm(list = ls())
set.seed(223456)
# prepare data ------------------------------------------------------------

library(tidyverse)
library(FDhelpers)
library(imager)

source(here::here("R", "utils.R"))

main_plotdir <- "plots"
plotdir_name <- "8close_1far_all_gist"
outdir <- here::here(main_plotdir, plotdir_name)

if(!dir.exists(outdir)) {
  dir.create(outdir)
}


gist <- read_csv(here::here("data", "gist_figrim_all.csv"), col_names = F)
load(here::here("data", "file_info.RData"))

df_files <- df_files %>% dplyr::mutate(id = 1:n())
gist$id <- 1:nrow(gist)

n <- 20

df_files_sample <- df_files %>% sample_n(n)
tm <- create.time.measure(n)

for (i in 1:n) {
  
  r <- df_files_sample[i,]
  
  # close to center ----------------------------------------------------------
  
  gist_point <- gist[r$id, ] %>% select(-id) %>% as.matrix()
  
  cl2 <- select_points_close_to_centerL2(gist, gist_point) 
  cl3 <- select_points_farthest_from_centerL2(gist, gist_point)
  
  df_for_plot <- rbind(cl2[-9, ], cl3[1, ])
 
  df_for_plot <- df_for_plot %>% left_join(df_files, by = "id")
  
  df_for_plot <- df_for_plot %>%
    rowwise() %>%
    dplyr::mutate(img_data = list(jpeg::readJPEG(pth))) %>%
    ungroup()
  
  # visualize and store stimuli ---------------------------------------------
  
  png(filename = file.path(outdir, sprintf("%s_gist.png", tools::file_path_sans_ext(r$filename))))
  plot.new()
  par(mar = c(1, 1, 1, 1))
  
  layout(matrix(1:9, 3, 3, byrow = TRUE), respect = TRUE)
  
  for (i in 1:nrow(df_for_plot)) {
    imager::as.cimg(df_for_plot$img_data[[i]]) %>% imrotate(90) %>% plot()
  }
  
  dev.off()
  
  # we can't use those images for next trials
  keep_ix <- !(df_files$id %in% df_for_plot$id)
  df_files <- df_files[keep_ix,]
  gist <- gist[keep_ix,]
  
  
  tm <- update(tm)
  print(tm)
}
