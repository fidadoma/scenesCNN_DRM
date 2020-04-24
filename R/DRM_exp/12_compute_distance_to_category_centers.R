
# data preparation --------------------------------------------------------


set.seed(190216)

library(tidyverse)
library(here)
source(here::here("R","utils.R"))

load(here::here("data","file_info.RData"))
load(here::here("data","figrim_fc7_vectors.RData"))

# we are using cleaned database, we need to clean the figrim data structure as well

#df_figrim_fc7 <- df_figrim_fc7 %>% filter(filename %in% df$img_name)

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

cat_centers_fc7 <- readRDS("data/exp_DRM1/categories_centers_fc7.rds")
cat_centers_conv5 <- readRDS("data/exp_DRM1/categories_centers_conv5.rds")
cat_centers_conv3 <- readRDS("data/exp_DRM1/categories_centers_conv3.rds")



categories <- df$category %>% unique()


df <- readRDS(here("data", "exp_DRM1", "results_after_review_200306.rds"))

df_participants <- readxl::read_excel(here::here("data","exp_DRM1","participants_190509.xlsx"))


# those files are huge, we will be loading coordinates per each category

df <- df %>% arrange(category,img_name)

prev_category <- ""

n <- length(categories)
tm <- FDhelpers::create.time.measure(n)
df_dist_center <- NULL
  
for (i in 1:n) {
  
  curr_categ <- str_replace(categories[i]," ","_")
  
  ix <- df$category == categories[i]
  # we changed category, we need to 
  if(prev_category != curr_categ) {
    
    df_figrim_conv3_categ <- readRDS(sprintf("data/figrim_vectors_per_category/conv3/%s.rds",curr_categ)) 
    df_figrim_conv5_categ <- readRDS(sprintf("data/figrim_vectors_per_category/conv5/%s.rds",curr_categ))
    prev_category <- curr_categ
    
    curr_cat_center_conv3 <- cat_centers_conv3 %>% filter(category == curr_categ) %>% select(-category) %>% as.matrix()
    curr_cat_center_conv5 <- cat_centers_conv5 %>% filter(category == curr_categ) %>% select(-category) %>% as.matrix()
    curr_cat_center_fc7   <- cat_centers_fc7 %>% filter(category == curr_categ) %>% select(-category) %>% as.matrix()
    colnames(curr_cat_center_conv3) <- NULL
    colnames(curr_cat_center_conv5) <- NULL
    colnames(curr_cat_center_fc7) <- NULL
  }
  
  curr_img_fc7   <- df_figrim_fc7 %>% filter(category == curr_categ) #df_figrim_fc7 %>% filter(filename == df$img_name[i])
  curr_img_conv3 <- df_figrim_conv3_categ %>% filter(category == curr_categ)#df_figrim_conv3_categ %>% filter(filename == df$img_name[i])
  curr_img_conv5 <- df_figrim_conv5_categ %>% filter(category == curr_categ)#df_figrim_conv5_categ %>% filter(filename == df$img_name[i])
  
  curr_img_vals_fc7 <- curr_img_fc7[,-(1:3)] %>% as.matrix()
  curr_img_vals_conv3 <- curr_img_conv3[,-(1:3)] %>% as.matrix()
  curr_img_vals_conv5 <- curr_img_conv5[,-(1:3)] %>% as.matrix()
  
  colnames(curr_img_vals_fc7) <- NULL
  colnames(curr_img_vals_conv3) <- NULL
  colnames(curr_img_vals_conv5) <- NULL
  
  curr_img_df <- curr_img_fc7 %>% select(category,filename)
  
  curr_img_df$dist_to_cat_center_fc7   <- l2norm(curr_img_vals_fc7,curr_cat_center_fc7)
  curr_img_df$dist_to_cat_center_conv3 <- l2norm(curr_img_vals_conv3,curr_cat_center_conv3)
  curr_img_df$dist_to_cat_center_conv5 <- l2norm(curr_img_vals_conv5,curr_cat_center_conv5)
  
  if(is.null(df_dist_center)) {
    df_dist_center <- curr_img_df
  } else(
    df_dist_center <- rbind(df_dist_center,curr_img_df)
  )
  
  
  tm <- FDhelpers::update.tm(tm)
  FDhelpers::print.tm(tm)
}
df <- df %>% left_join(df_dist_center %>% select(-category), by = c("img_name"="filename"))

saveRDS(df, "data/exp_DRM1/results_after_review_200402.rds")
