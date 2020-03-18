rm(list = ls())



set.seed(167)


library(tidyverse)
library(lme4)
library(here)

source(here("R","utils.R"))

load(here("data","konkle_180621.RData"))
df_metrics <- readRDS(here("data","oddoneout","pairwise_metrics_190611.rds"))


data_pth <- here("data", "oddoneout","results_categ") 

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

df_metrics_full <- rbind(df_metrics, (df_metrics %>% mutate(tmp = im1, im1 = im2, tmp_id= im1_id, im1_id = im2_id) %>% mutate(im2 = tmp, im2_id = tmp_id) %>% select(-tmp, -tmp_id)))


wrong_trials <- df_metrics_full %>% group_by(prot_id, trial_id) %>% summarize(n = length(unique(im1))) %>% filter(n<9) %>% select(-n)

df_ordered_outliers <- df_metrics_full %>% 
  group_by(prot_id, trial_id) %>% 
  anti_join(wrong_trials, by = c("prot_id", "trial_id")) %>% 
  do(order_all_outliers(.)) %>% 
  ungroup() 

df_ordered_outliers2 <- 
  df_ordered_outliers %>%
  rowwise() %>% 
  mutate(ssim_top1 = get_top1(ssim_order),
         hog_top1 = get_top1(hog_order),
         gist_top1 = get_top1(gist_order),
         sift_top1 = get_top1(sift_sum),
         pdistRGB_top1 = get_top1(pdistRGB),
         ssim_top3 = list(get_top3(ssim_order)),
         hog_top3 = list(get_top3(hog_order)),
         gist_top3 = list(get_top3(gist_order)),
         sift_top3 = list(get_top3(sift_sum)),
         pdistRGB_top3 = list(get_top3(pdistRGB))) %>% 
  ungroup()
         
df2 <- df %>% left_join(df_ordered_outliers2,  by = c("prot_id", "trial_id")) %>% 
  mutate(ssim_correct = as.numeric(ssim_top1 == target_position),
         hog_correct = as.numeric(hog_top1 == target_position),
         gist_correct = as.numeric(gist_top1 == target_position),
         sift_correct = as.numeric(sift_top1 == target_position),
         pdistRGB_correct = as.numeric(pdistRGB_top1 == target_position),
         ssim_selected = as.numeric(ssim_top1 == as.numeric(str_remove(mouse.clicked_name, "image"))),
         hog_selected = as.numeric(hog_top1 == as.numeric(str_remove(mouse.clicked_name, "image"))),
         gist_selected = as.numeric(gist_top1 == as.numeric(str_remove(mouse.clicked_name, "image"))),
         sift_selected = as.numeric(sift_top1 == as.numeric(str_remove(mouse.clicked_name, "image"))),
         pdistRGB_selected = as.numeric(pdistRGB_top1 == as.numeric(str_remove(mouse.clicked_name, "image")))) %>% 
  rowwise() %>% 
  mutate(ssim3_correct = as.numeric(target_position %in% unlist(ssim_top3)),
         hog3_correct = as.numeric(target_position %in% unlist(hog_top3)),
         gist3_correct = as.numeric(target_position %in% unlist(gist_top3)),
         sift3_correct = as.numeric(target_position %in% unlist(sift_top3)),
         pdistRGB3_correct = as.numeric(target_position %in% unlist(pdistRGB_top3))) %>% 
  mutate(ssim3_selected = as.numeric(as.numeric(str_remove(mouse.clicked_name, "image")) %in% unlist(ssim_top3)),
         hog3_selected = as.numeric(as.numeric(str_remove(mouse.clicked_name, "image")) %in% unlist(hog_top3)),
         gist3_selected = as.numeric(as.numeric(str_remove(mouse.clicked_name, "image")) %in% unlist(gist_top3)),
         sift3_selected = as.numeric(as.numeric(str_remove(mouse.clicked_name, "image")) %in% unlist(sift_top3)),
         pdistRGB3_selected = as.numeric(as.numeric(str_remove(mouse.clicked_name, "image")) %in% unlist(pdistRGB_top3))) %>% 
  mutate(metrics_combined_correct = max(hog_correct,gist_correct,sift_correct,na.rm=T),
         metrics_combined_selected = max(hog_selected,gist_selected,sift_selected,na.rm=T)) %>% 
  ungroup()

df2$metrics_max <- NA
df2$metrics_max_selected <- NA
df2$metrics_max_correct <- NA

for (i in 1:nrow(df2)) {
  df2$metrics_max[i] <- select_max(df2[i,])
  df2$metrics_max_correct[i] <- as.numeric(df2$metrics_max[i] == df2$target_position[i])
  df2$metrics_max_selected[i] <- as.numeric(df2$metrics_max[i] == as.numeric(str_remove(df2$mouse.clicked_name[i], "image")))
}


df3 <- df2 %>% gather(metric, value_fc7, correct, ssim_correct,hog_correct,gist_correct,sift_correct,pdistRGB_correct, metrics_combined_correct,metrics_max_correct)
df3_top3 <- df2 %>% gather(metric, value_fc7_top3, ssim3_correct,hog3_correct,gist3_correct,sift3_correct,pdistRGB3_correct) %>% 
  select(protocol_id, trial_id, subject_id, metric, value_fc7_top3) %>% 
  mutate(metric = str_remove(metric,"3"))

df3 <- df3 %>% left_join(df3_top3, by = c("protocol_id",  "trial_id","subject_id", "metric"))

df3_2 <- df2 %>% gather(metric, value_partic, correct, ssim_selected,hog_selected,gist_selected,sift_selected,pdistRGB_selected,metrics_combined_selected,metrics_max_selected)
df3_2_top3 <- df2 %>% gather(metric, value_partic_top3, ssim3_selected,hog3_selected,gist3_selected,sift3_selected,pdistRGB3_selected) %>% 
  select(protocol_id, trial_id, subject_id, metric, value_partic_top3) %>% 
  mutate(metric = str_remove(metric,"3"))
df3_2 <- df3_2 %>% left_join(df3_2_top3, by = c("protocol_id",  "trial_id","subject_id", "metric"))
df3$value_partic <- df3_2$value_partic
df3$value_partic_top3 <- df3_2$value_partic_top3
saveRDS(df3, here("data", "oddoneout", "metrics_200317.rds"))
saveRDS(df2, here("data", "oddoneout", "metrics_wide_200317.rds"))
