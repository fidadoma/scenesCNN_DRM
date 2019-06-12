# use of metrics for pairwise comparison

rm(list = ls())

set.seed(167)

library(tidyverse)

df_rgb <- read_csv(here::here("data", "oddoneout","pdist_RGB.csv"), col_types = cols(
  prot_id = col_integer(),
  trial_id = col_integer(),
  im1 = col_character(),
  im2 = col_character(),
  im1_id = col_integer(),
  im2_id = col_integer(),
  pdistRGB = col_double()
))

df_metrics <- read_csv(here::here("data", "oddoneout","pairwise_metrics_allprotocols.csv"), col_types = cols(
  prot_id = col_integer(),
  trial_id = col_integer(),
  im1 = col_character(),
  im2 = col_character(),
  im1_id = col_integer(),
  im2_id = col_integer(),
  ssim = col_double(),
  hog = col_double(),
  gist = col_double(),
  sift_sum = col_integer(),
  sift_n = col_integer(),
  surf_sum = col_double(),
  surf_n = col_integer()
))

df_metrics <- df_metrics %>% left_join(df_rgb, by = c("prot_id", "trial_id", "im1", "im2", "im1_id", "im2_id"))

df_metrics %>% select(ssim, hog, gist, sift_sum,pdistRGB) %>%  cor()

saveRDS(df_metrics, here::here("data", "oddoneout","pairwise_metrics_190611.rds"))

                                                                      