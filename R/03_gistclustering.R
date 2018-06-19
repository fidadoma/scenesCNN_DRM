rm(list = ls())

library(tidyverse)

source("utils.R")

sel_cat <- "mountain"

gist <- read_csv(here::here("data", "gist_figrim_all.csv"), col_names = F)
load(here::here("data","file_info.RData"))

gist$id <- 1:nrow(gist)
km1 <- kmeans(gist %>% select(-id) ,2)
cls <- km1$cluster

mds_gist_all <- cmdscale(dist(gist %>% select(-id)), eig = T)
mds_gist <- mds_gist_all$points
colnames(mds_gist) <- c("x","y")

mds_gist <- as_data_frame(mds_gist) %>%
  mutate(cluster = cls, id = 1:n())

mds_gist %>% 
  ggplot(aes(x, y, col = as.factor(cluster))) +
  geom_point() +
  theme(aspect.ratio = 1)




cl2 <- select_points_close_to_centerL2(gist, km1$centers[1,], n_points = 4) %>% left_join(mds_gist, by = "id")
cl3 <- select_points_close_to_centerL2(gist, km1$centers[2,], n_points = 4) %>% left_join(mds_gist, by = "id")


mds_gist %>% 
  ggplot(aes(x, y, col = as.factor(cluster))) + 
  geom_point(alpha = 0.1) +
  theme(aspect.ratio = 1) + 
  ggtitle(sel_cat) + 
  #geom_point(data = km1$centers %>% as_data_frame(), size = 3, col = "black") +
  geom_point(data = cl2, shape = 17, alpha = 1, size = 4) + 
  geom_point(data = cl3[1,], shape = 17, alpha = 1, size = 4)

file_info <- file_info %>% 
  mutate(id = 1:n())

df_for_plot <- rbind(cl2[-1,],cl3[1,]) %>% left_join(file_info, by = "id")

df_for_plot <- df_for_plot %>% 
  rowwise() %>% 
  dplyr::mutate(img_data = list(jpeg::readJPEG(pth))) %>%
  ungroup()


library(imager)

plot.new()
par(mar=c(1,1,1,1))

layout(matrix(1:9, 3, 3, byrow = TRUE), respect = TRUE)

for(i in 1:nrow(df_for_plot)) {
  imager::as.cimg(df_for_plot$img_data[[i]]) %>% imrotate(90) %>% plot()
}
