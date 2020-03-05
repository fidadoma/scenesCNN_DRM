
# data preparation --------------------------------------------------------


set.seed(190216)

library(tidyverse)
library(here)
source(here::here("R","utils.R"))

load(here::here("data","file_info.RData"))
load(here::here("data","figrim_fc7_vectors.RData"))

# we are using cleaned database, we need to clean the figrim data structure as well

df_files <- df_files %>% filter(filename %in% df_figrim_fc7$filename)
rm(df_figrim_fc7)

prot_dir <- here::here("data","exp_DRM1","protocols")

if(!dir.exists(prot_dir)){
  stop("Missing protocols")
}

fs <- list.files(prot_dir, full.names = T, pattern = "*.csv") %>% setdiff(file.path(prot_dir,"P000.csv"))

df_protocols <- fs %>%
  map(~ read_csv(., col_types = cols(
    prot_id = col_double(),
    trial_id = col_double(),
    grp_order = col_double(),
    category = col_character(),
    id_rep = col_double(),
    img_name = col_character(),
    type = col_character(),
    query = col_double(),
    corrKey = col_character()
  ))) %>% 
  reduce(rbind)

save_plots <- T

plots_dir <- here::here("plots","exp_DRM1","image_spaces")

if(!dir.exists(plots_dir)) {
  dir.create(plots_dir, recursive = T)
}


categories <- df_files$category %>% unique()


df <- readRDS(here("data", "exp_DRM1", "results_200219.rds")) %>% 
  group_by(subject_id) %>% 
  mutate(trial_grp = ((trial_id -1)%/%15) -1) %>% 
  ungroup()


df_participants <- readxl::read_excel(here::here("data","exp_DRM1","participants_190509.xlsx"))


# conv3 -------------------------------------------------------------------

# those files are huge, we will be loading coordinates per each category

df_coords_conv3 <- df %>% select(category,subject_id, trial_id,img_name) %>% 
  mutate(img_coord = NA, tgt_coord = NA)


df$dist_to_tgts_conv3 <- NA
df$dist_to_proto_conv3 <- NA
df <- df %>% arrange(subject_id,category, trial_id)

prev_category <- ""
prev_subj  <- -1
prev_grp      <- -1
tm <- FDhelpers::create.time.measure(nrow(df))

for (i in 1:nrow(df)) {
  curr_categ <- df$category[i]
  curr_grp   <- df$trial_grp[i]
  curr_subj  <- df$subject_id[i]
  
  # we changed category, we need to 
  if(prev_category != curr_categ) {
    
    df_figrim_conv3_categ <- readRDS(sprintf("data/figrim_vectors_per_category/conv3/%s.rds",str_replace(curr_categ," ","_")))
    
    prev_category <- curr_categ
    
  }
  
  if(prev_subj!=curr_subj | prev_grp!=curr_grp) { # we need to select the targets only once per group
    ix_check <- (1:15)+15*curr_grp
    p_prot <- df_protocols %>% filter(prot_id == df$prot_id[i])
    p_prot_check <- p_prot %>% filter(trial_id %in% ix_check)
    
    tgt_imgs <- df_figrim_conv3_categ %>% filter(filename %in% p_prot_check$img_name)
    
    categ_img_vals <- tgt_imgs[,-(1:3)] %>% as.matrix()
    colnames(categ_img_vals) <- NULL
    prev_grp <- curr_grp
  }
  
  # we changed subject, we should save data
  if(prev_subj!=curr_subj) { 
    if(prev_subj!=-1) {
    saveRDS(df_coords_conv3 %>% 
                filter(subject_id == prev_subj), 
              sprintf("data/exp_DRM1/img_coords/sub%02d_conv3.rds",prev_subj))
    }
    # and reset coords
    df_coords_conv3 <- df %>% select(category,subject_id, trial_id,img_name) %>% 
        mutate(img_coord = NA, tgt_coord = NA)
    prev_subj <- curr_subj
  }
  
  
  
  
  curr_img <- df_figrim_conv3_categ %>% filter(filename == df$img_name[i])
  
  
  curr_img_vals <- curr_img[,-(1:3)] %>% as.matrix()
  
  
  colnames(curr_img_vals) <- NULL
  
  
  df$dist_to_tgts_conv3[i] <- list(l2norm(categ_img_vals,curr_img_vals))
  df$dist_to_proto_conv3[i] <- l2norm(colMeans(categ_img_vals),curr_img_vals)
  
  df_coords_conv3$img_coord[i] <- list(curr_img_vals)
  df_coords_conv3$tgt_coord[i] <- list(categ_img_vals)
  
  tm <- FDhelpers::update.tm(tm)
  FDhelpers::print.tm(tm)
}

saveRDS(df_coords_conv3 %>% 
          filter(subject_id == prev_subj), 
        sprintf("data/exp_DRM1/img_coords/sub%02d.rds",prev_subj))

saveRDS(df,file = "data/exp_DRM1/results_with_tgtdistances_conv3_200219.rds")


# conv5 -------------------------------------------------------------------

# those files are huge, we will be loading coordinates per each category

df_coords_conv5 <- df %>% select(category,subject_id, trial_id,img_name) %>% 
  mutate(img_coord = NA, tgt_coord = NA)


df$dist_to_tgts_conv5 <- NA
df$dist_to_proto_conv5 <- NA
df <- df %>% arrange(subject_id,category, trial_id)

prev_category <- ""
prev_subj  <- -1
prev_grp      <- -1
tm <- FDhelpers::create.time.measure(nrow(df))

for (i in 1:nrow(df)) {
  curr_categ <- df$category[i]
  curr_grp   <- df$trial_grp[i]
  curr_subj  <- df$subject_id[i]
  
  # we changed category, we need to 
  if(prev_category != curr_categ) {
    
    df_figrim_conv5_categ <- readRDS(sprintf("data/figrim_vectors_per_category/conv5/%s.rds",str_replace(curr_categ," ","_")))
    
    prev_category <- curr_categ
    
  }
  
  if(prev_subj!=curr_subj | prev_grp!=curr_grp) { # we need to select the targets only once per group
    ix_check <- (1:15)+15*curr_grp
    p_prot <- df_protocols %>% filter(prot_id == df$prot_id[i])
    p_prot_check <- p_prot %>% filter(trial_id %in% ix_check)
    
    tgt_imgs <- df_figrim_conv5_categ %>% filter(filename %in% p_prot_check$img_name)
    
    categ_img_vals <- tgt_imgs[,-(1:3)] %>% as.matrix()
    colnames(categ_img_vals) <- NULL
    prev_grp <- curr_grp
  }
  
  # we changed subject, we should save data
  if(prev_subj!=curr_subj) { 
    if(prev_subj!=-1) {
      saveRDS(df_coords_conv5 %>% 
                filter(subject_id == prev_subj), 
              sprintf("data/exp_DRM1/img_coords/sub%02d_conv5.rds",prev_subj))
    }
    # and reset coords
    df_coords_conv5 <- df %>% select(category,subject_id, trial_id,img_name) %>% 
      mutate(img_coord = NA, tgt_coord = NA)
    prev_subj <- curr_subj
  }
  
  
  
  
  curr_img <- df_figrim_conv5_categ %>% filter(filename == df$img_name[i])
  
  
  curr_img_vals <- curr_img[,-(1:3)] %>% as.matrix()
  
  
  colnames(curr_img_vals) <- NULL
  
  
  df$dist_to_tgts_conv5[i] <- list(l2norm(categ_img_vals,curr_img_vals))
  df$dist_to_proto_conv5[i] <- l2norm(colMeans(categ_img_vals),curr_img_vals)
  
  df_coords_conv5$img_coord[i] <- list(curr_img_vals)
  df_coords_conv5$tgt_coord[i] <- list(categ_img_vals)
  
  tm <- FDhelpers::update.tm(tm)
  FDhelpers::print.tm(tm)
}

saveRDS(df_coords_conv5 %>% 
          filter(subject_id == prev_subj), 
        sprintf("data/exp_DRM1/img_coords/sub%02d_conv5.rds",prev_subj))

saveRDS(df,file = "data/exp_DRM1/results_with_tgtdistances_conv5_200219.rds")


