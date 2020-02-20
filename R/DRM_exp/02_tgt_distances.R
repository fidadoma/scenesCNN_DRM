
set.seed(190216)

library(tidyverse)
library(here)
source(here::here("R","utils.R"))

#load("data/m_figrim_l2.RData")
load(here::here("data","file_info.RData"))
load(here::here("data","figrim_vectors.RData"))

# we are using cleaned database, we need to clean the figrim data structure as well

df_files <- df_files %>% filter(filename %in% df_figrim_fc7$filename)

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





tm <- FDhelpers::create.time.measure(nrow(df))

for (i in 1:nrow(df)) {
  ix_check <- (1:15)+15*df$trial_grp[i]
  p_prot <- df_protocols %>% filter(prot_id == df$prot_id[i])
  p_prot_check <- p_prot %>% filter(trial_id %in% ix_check)
  curr_img <- df_figrim_fc7 %>% filter(filename == df$img_name[i])
  tgt_imgs <- df_figrim_fc7 %>% filter(filename %in% p_prot_check$img_name)
  
  curr_img_vals <- curr_img %>% select(starts_with("V")) %>% as.matrix()
  categ_img_vals <- tgt_imgs %>% select(starts_with("V")) %>% as.matrix()
  
  df$dist_to_tgts[i] <- list(l2norm(categ_img_vals,curr_img_vals))
  df$img_coord[i] <- list(curr_img_vals)
  df$tgt_coord[i] <- list(categ_img_vals)
  tm <- FDhelpers::update.tm(tm)
  FDhelpers::print.tm(tm)
}

saveRDS(df,file = "data/exp_DRM1/results_with_tgtdistances_200219.rds")
