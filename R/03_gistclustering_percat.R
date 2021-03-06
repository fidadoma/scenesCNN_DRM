# this script compute MDS and kmeans for each category separately

rm(list = ls())
set.seed(123456)
# prepare data ------------------------------------------------------------

library(tidyverse)
library(FDhelpers)

source(here::here("R", "utils.R"))

gist <- read_csv(here::here("data", "gist_figrim_all.csv"), col_names = F)
load(here::here("data", "file_info.RData"))

df_files <- df_files %>% dplyr::mutate(id = 1:n())
gist$id <- 1:nrow(gist)


categories <- df_files$category %>% unique()

tm <- create.time.measure(length(categories))

for (i in 1:length(categories)) {
  sel_cat <- categories[i]
  ix <- df_files$category == sel_cat
  gist_onecat <- gist[ix, ]
  df_files_onecat <- df_files[ix, ]


  # kmeans and mds ----------------------------------------------------------

  km1 <- kmeans(gist_onecat %>% select(-id), 2)
  cls <- km1$cluster

  mds_gist_all <- cmdscale(dist(gist_onecat %>% select(-id)), eig = T)
  mds_gist <- mds_gist_all$points
  colnames(mds_gist) <- c("x", "y")

  mds_gist <- as_data_frame(mds_gist) %>%
    dplyr::mutate(cluster = cls, id = gist_onecat$id)

  cl2 <- select_points_close_to_centerL2(gist_onecat, km1$centers[1, ]) %>% left_join(mds_gist, by = "id")
  cl3 <- select_points_close_to_centerL2(gist_onecat, km1$centers[2, ]) %>% left_join(mds_gist, by = "id")

  df_for_plot <- rbind(cl2[-1, ], cl3[1, ]) %>% left_join(df_files_onecat, by = "id")

  df_for_plot <- df_for_plot %>%
    rowwise() %>%
    dplyr::mutate(img_data = list(jpeg::readJPEG(pth))) %>%
    ungroup()

  # plotting ----------------------------------------------------------------

  mds_gist %>%
    ggplot(aes(x, y, col = as.factor(cluster))) +
    geom_point() +
    theme(aspect.ratio = 1)

  p <-
    mds_gist %>%
    ggplot(aes(x, y, col = as.factor(cluster))) +
    geom_point(alpha = 0.1) +
    theme(aspect.ratio = 1) +
    ggtitle(sel_cat) +
    # geom_point(data = km1$centers %>% as_data_frame(), size = 3, col = "black") +
    geom_point(data = cl2, shape = 17, alpha = 1, size = 4) +
    geom_point(data = cl3[1, ], shape = 17, alpha = 1, size = 4)
  ggsave(here::here("plots", "gist", sprintf("%s_mds_gist.png", sel_cat)), p)


  # visualize and store stimuli ---------------------------------------------

  library(imager)

  png(filename = here::here("plots", "gist", sprintf("%s_gist.png", sel_cat)))
  plot.new()
  par(mar = c(1, 1, 1, 1))

  layout(matrix(1:9, 3, 3, byrow = TRUE), respect = TRUE)

  for (i in 1:nrow(df_for_plot)) {
    imager::as.cimg(df_for_plot$img_data[[i]]) %>% imrotate(90) %>% plot()
  }

  dev.off()

  tm <- update(tm)
  print(tm)
}
