# use of metrics for pairwise comparison

rm(list = ls())

set.seed(167)

library(tidyverse)


df_metrics <- read_csv(here::here("data", "metrics_pairwise.csv"), col_types = cols(
  group = col_integer(),
  photo1 = col_integer(),
  photo2 = col_integer(),
  fsim = col_double(),
  fsimc = col_double(),
  ssim = col_double(),
  hog = col_double(),
  gist = col_double(),
  `sum(matchmetric_sift)` = col_integer(),
  `n(matches_sift)` = col_integer(),
  `sum(matchmetric_surf)` = col_double(),
  `n(matches_surf)` = col_integer(),
  Clutter = col_double()
))


df_metrics %>% select(fsim, fsimc, ssim, hog, gist, Clutter) %>%  cor()

saveRDS(df_metrics, here::here("data", "pairwise_metrics.rds"))

                                                                      