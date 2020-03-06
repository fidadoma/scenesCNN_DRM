set.seed(190425)
library(tidyverse); theme_set(theme_classic(16))
library(FDhelpers)
library(here)

source(here("R","utils.R"))

df_participants <- readxl::read_excel(here::here("data","exp_DRM1","participants_190509.xlsx"))

df2 <- readRDS(here("data/exp_DRM1/results_with_tgtdistances_fc7_200219.rds"))
df2_conv5 <- readRDS(here("data/exp_DRM1/results_with_tgtdistances_conv5_200219.rds"))
df2_conv3 <- readRDS(here("data/exp_DRM1/results_with_tgtdistances_conv3_200219.rds"))

id_subj <- df_participants %>% pull(subject_id) %>% unique()
coord_pth <- here("data/exp_DRM1/img_coords")

df2 <- df2 %>% 
  left_join(df2_conv3 %>% select(subject_id, trial_id, dist_to_proto_conv3), by = c("subject_id", "trial_id")) %>% 
  left_join(df2_conv5 %>% select(subject_id, trial_id, dist_to_proto_conv5), by = c("subject_id", "trial_id")) %>%
  left_join(df2_conv3 %>% select(subject_id, trial_id, dist_to_tgts_conv3), by = c("subject_id", "trial_id")) %>%
  left_join(df2_conv5 %>% select(subject_id, trial_id, dist_to_tgts_conv5), by = c("subject_id", "trial_id"))

# dist_betwen_tgts

# fc7 
df2_fc7_tgt <- df2 %>% 
  group_by(subject_id,trial_grp) %>% 
  filter(type == "target") %>% 
  summarize(avg_dist_between_tgts = mean(unlist(dist_to_tgts)))

df2_conv5_tgt <- df2_conv5 %>% 
  group_by(subject_id,trial_grp) %>% 
  filter(type == "target") %>% 
  summarize(avg_dist_between_tgts5 = mean(unlist(dist_to_tgts_conv5)))

df2_conv3_tgt <- df2_conv3 %>% 
  group_by(subject_id,trial_grp) %>% 
  filter(type == "target") %>% 
  summarize(avg_dist_between_tgts3 = mean(unlist(dist_to_tgts_conv3)))

df2 <- df2 %>% 
  left_join(df2_fc7_tgt, by = c("subject_id", "trial_grp")) %>% 
  left_join(df2_conv5_tgt, by = c("subject_id", "trial_grp")) %>% 
  left_join(df2_conv3_tgt, by = c("subject_id", "trial_grp"))

saveRDS(df2, "data/exp_DRM1/results_after_review_200306.rds")
# conv 5 - dist between tgts ---------------------------------------------------

fs <- list.files(coord_pth,pattern = "*_conv5.rds", full.names = T)  

conv5_dist_btw_tgts <- NULL 

tm <- create.time.measure(length(fs)) 
for (i in 1:length(fs)) {
  fs1 <- fs[i]
  subj <- readRDS(fs1)
  x <- subj %>% 
    rowwise() %>% 
    mutate(mean_dist_between_tgt = dist(tgt_coord) %>% mean()) %>% 
    select(subject_id, trial_id, img_name ,mean_dist_between_tgt)
  if(is.null(conv5_dist_btw_tgts)) {
    conv5_dist_btw_tgts <- x
  } else {
    conv5_dist_btw_tgts <- conv5_dist_btw_tgts %>% rbind(x)
  }
  tm <- update.tm(tm)
  print(tm)
}

# conv 3 - dist between tgts ---------------------------------------------------

fs <- list.files(coord_pth,pattern = "*_conv3.rds", full.names = T)  

conv3_dist_btw_tgts <- NULL 

tm <- create.time.measure(length(fs)) 
for (i in 1:length(fs)) {
  fs1 <- fs[i]
  subj <- readRDS(fs1)
  x <- subj %>% 
    rowwise() %>% 
    mutate(mean_dist_between_tgt = dist(tgt_coord) %>% mean()) %>% 
    select(subject_id, trial_id, img_name ,mean_dist_between_tgt)
  if(is.null(conv5_dist_btw_tgts)) {
    conv3_dist_btw_tgts <- x
  } else {
    conv3_dist_btw_tgts <- conv3_dist_btw_tgts %>% rbind(x)
  }
  tm <- update.tm(tm)
  print(tm)
}


