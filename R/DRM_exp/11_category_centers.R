set.seed(190216)

library(tidyverse)
library(here)
source(here::here("R","utils.R"))

load(here::here("data","file_info.RData"))
load(here::here("data","figrim_fc7_vectors.RData"))

# we are using cleaned database, we need to clean the figrim data structure as well

df_figrim_fc7 <- df_figrim_fc7 %>% filter(filename %in% df_figrim_fc7$filename)


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

n <- length(categories)
tm <- FDhelpers::create.time.measure(n)
cat_centers_fc7 <- tibble(category = categories)
cat_centers_conv3 <- tibble(category = categories)
cat_centers_conv5 <- tibble(category = categories)

for (i in 1:n) {
  cat <- categories[i]
  
  # fc7
  cat_imnames <- df_figrim_fc7 %>% filter(category == cat) %>% pull(filename)
  
  # select data frame with given category
  df_cat <- df_figrim_fc7 %>% filter(category == cat)
  
  # and extract only the matrix with fc7 vectors
  m <- df_cat %>% select(-category,-filename,-pth) %>% as.matrix()
  rownames(m)  <- cat_imnames
  
  # compute distance matrix using fast implementation for distance matrices
  # this code was used to verify that the distance matrix computed in R is the same as distance matrix computed in python script
  m_dist <- parallelDist::parDist(m) %>% as.matrix()
  cmd_dist_base <- cmdscale(m_dist) %>% as_tibble(rownames = "img_name") %>% mutate(type = "normal")  
  
  # each row is individual image, therefore comuting colmeans is center of the category
  cat_center <- colMeans(m) %>% enframe() %>% pivot_wider(names_from=name, values_from=value) %>% mutate(category = cat) %>% select(category,everything())
  if(ncol(cat_centers_fc7) == 1) {
    cat_centers_fc7 <- cat_centers_fc7 %>% left_join(cat_center)  
  } else {
    cat_centers_fc7[cat_centers_fc7$category == cat, ] <- cat_center
  }
  
  # conv3
  df_figrim_conv5 <- readRDS(sprintf("data/figrim_vectors_per_category/conv5/%s.rds",str_replace(cat," ","_")))
  cat_imnames <- df_figrim_conv5 %>% filter(category == cat) %>% pull(filename)
  
  # select data frame with given category
  df_cat <- df_figrim_conv5 %>% filter(category == cat)
  
  # and extract only the matrix with fc7 vectors
  m <- df_cat %>% select(-category,-filename,-pth) %>% as.matrix()
  rownames(m)  <- cat_imnames
  
  # each row is individual image, therefore comuting colmeans is center of the category
  cat_center <- colMeans(m) %>% enframe() %>% pivot_wider(names_from=name, values_from=value) %>% mutate(category = cat) %>% select(category,everything())
  if(ncol(cat_centers_conv5) == 1) {
    cat_centers_conv5 <- cat_centers_conv5 %>% left_join(cat_center)  
  } else {
    cat_centers_conv5[cat_centers_conv5$category == cat, ] <- cat_center
  }
  
  # conv3
  df_figrim_conv3 <- readRDS(sprintf("data/figrim_vectors_per_category/conv3/%s.rds",str_replace(cat," ","_")))
  cat_imnames <- df_figrim_conv3 %>% filter(category == cat) %>% pull(filename)
  
  # select data frame with given category
  df_cat <- df_figrim_conv3 %>% filter(category == cat)
  
  # and extract only the matrix with fc7 vectors
  m <- df_cat %>% select(-category,-filename,-pth) %>% as.matrix()
  rownames(m)  <- cat_imnames
  
  # each row is individual image, therefore comuting colmeans is center of the category
  cat_center <- colMeans(m) %>% enframe() %>% pivot_wider(names_from=name, values_from=value) %>% mutate(category = cat) %>% select(category,everything())
  if(ncol(cat_centers_conv3) == 1) {
    cat_centers_conv3 <- cat_centers_conv3 %>% left_join(cat_center, by = "category")  
  } else {
    cat_centers_conv3[cat_centers_conv3$category == cat, ] <- cat_center
  }
  
  
  tm <- FDhelpers::update.tm(tm)
  FDhelpers::print.tm(tm)
}

saveRDS(cat_centers_fc7, "data/exp_DRM1/categories_centers_fc7.rds")
saveRDS(cat_centers_conv5, "data/exp_DRM1/categories_centers_conv5.rds")
saveRDS(cat_centers_conv3, "data/exp_DRM1/categories_centers_conv3.rds")



