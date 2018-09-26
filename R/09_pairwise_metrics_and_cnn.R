rm(list = ls())
set.seed(167)
load(here::here("data","konkle_180621.RData"))
df_metrics <- readRDS(here::here("data","pairwise_metrics.rds"))
library(tidyverse)
library(lme4)

theme_set(theme_bw())

data_pth <- here::here("data", "results", "categ") 

col_types <- cols(
  .default = col_character(),
  prot_id = col_integer(),
  trial_id = col_integer(),
  target_position = col_integer(),
  quintile = col_integer(),
  trials.thisRepN = col_integer(),
  trials.thisTrialN = col_integer(),
  trials.thisN = col_integer(),
  trials.thisIndex = col_integer(),
  mouse.x = col_double(),
  mouse.y = col_double(),
  mouse.leftButton = col_integer(),
  mouse.midButton = col_integer(),
  mouse.rightButton = col_integer(),
  protocol_id = col_integer(),
  subject_id = col_integer(),
  frameRate = col_double()
)

df <- data_pth %>% 
  dir(pattern = "*.csv", full.names = T) %>% 
  purrr::map(read_csv, col_types = col_types) %>% 
  bind_rows() 

df <- df %>%
  mutate(target_image = paste0("image",target_position),
         correct = as.numeric(target_image == mouse.clicked_name),
         category_type = if_else(category %in% c("desert", "waves", "coast", "underwater", "iceberg","field","garden","cavern","beach","canyon","mountainwhite","woods"),"natural","manmade"))

prot1 <- df %>% filter(prot_id == 1) %>% select(trial_id)
