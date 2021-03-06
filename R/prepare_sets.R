
# load data ---------------------------------------------------------------


rm(list = ls())

library(tidyverse)
library(FDhelpers)

mds_all <- readRDS(here::here("data", "mds_all_180613.Rds"))

set.seed(123)


categories <- mds_all$category %>% unique()

tm <- create.time.measure(length(categories))

for (sel_cat in categories) {
  # sel_cat <- "castle"

  mds_onecat <- mds_all %>% filter(category == sel_cat)

  cl <- mds_onecat %>% select(x, y) %>% as.matrix() %>% kmeans(3)
  mds_onecat$cluster <- cl$cluster


  eucldist <- function(x1, x2, y1, y2) {
    sqrt((x1 - x2)^2 + (y1 - y2)^2)
  }

  select_points_close_to_center <- function(df, cent, n_points = 9) {
    xc <- cent[1]
    yc <- cent[2]
    df %>% mutate(d = eucldist(x, xc, y, yc)) %>% top_n(n_points, -d)
  }


  cl2 <- select_points_close_to_center(mds_onecat, cl$centers[2, ])
  cl3 <- select_points_close_to_center(mds_onecat, cl$centers[3, ])

  p <- mds_onecat %>%
    ggplot(aes(x, y, col = as.factor(cluster))) +
    geom_point(alpha = 0.1) +
    theme(aspect.ratio = 1) +
    ggtitle(sel_cat) +
    geom_point(
      data = cl$centers %>% as_data_frame(),
      size = 3,
      col = "black"
    ) +
    geom_point(
      data = cl2,
      shape = 17,
      alpha = 1,
      size = 4
    ) +
    geom_point(
      data = cl3[1, ],
      shape = 17,
      alpha = 1,
      size = 4
    )
  p
  ggsave(here::here("plots", "composites", sprintf("%s_mds.png", sel_cat)))

  df_for_plot <- rbind(cl2[-1, ], cl3[1, ])

  df_for_plot <- df_for_plot %>%
    rowwise() %>%
    dplyr::mutate(img_data = list(jpeg::readJPEG(pth))) %>%
    ungroup()

# save image --------------------------------------------------------------

  library(imager)

  png(filename = here::here("plots", "composites", sprintf("%s.png", sel_cat)))
  plot.new()
  par(mar = c(1, 1, 1, 1))

  layout(matrix(1:9, 3, 3, byrow = TRUE), respect = TRUE)

  for (i in 1:nrow(df_for_plot)) {
    imager::as.cimg(df_for_plot$img_data[[i]]) %>% imrotate(90) %>% plot()
  }
  dev.off()

  update.tm(tm)
  print(tm)
}
