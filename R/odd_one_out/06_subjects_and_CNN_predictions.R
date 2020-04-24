
set.seed(5167)


library(tidyverse)
library(FDhelpers)
library(here)

source(here("R","utils.R"))

load(here("data","konkle_180621.RData"))
plots_dir <- here("plots", "oddoneout")

if(!dir.exists(plots_dir)) {
  dir.create(plots_dir, recursive = T)
}
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

df_subj <- tibble(prot_id = 1:6, subj1 = c("s1","s100","s3","s4","s5","s6"), subj2 = c("s7","s8","s9","s10","s11","s12"))
df1 <- df %>% 
  select(prot_id, quintile, trial_id, subject_id, selected = mouse.clicked_name) %>% 
  mutate(selected = unlist(selected),
         subject_id = paste0("s",subject_id)) %>% 
  left_join(df_subj, by = "prot_id") %>% 
  group_by(prot_id) %>% 
  pivot_wider(names_from = subject_id, values_from = selected)
for (i in 1:nrow(df1)) {
  df1$subj1[i] <- df1[[i, df1$subj1[i]]]
  df1$subj2[i] <- df1[[i, df1$subj2[i]]]
}
p <- df1 %>% 
  mutate(correct = as.numeric(subj1 == subj2)) %>% 
  mutate(quintile = as.character(quintile)) %>% 
  ggplot(aes(x = quintile, y = correct, group = prot_id)) + 
  stat_summary(fun.y = "mean", geom = "point", alpha = 0.2, position = position_dodge(0.2))+
stat_summary(fun.y = "mean", geom = "line", alpha = 0.2, position = position_dodge(0.2)) + 
  stat_summary(aes(group = 1),fun.data = "mean_cl_boot",size = 1, position = position_dodge(0.2)) +
  stat_summary(aes(group = 1),fun.y = "mean",geom = "line",size = 1) +
  ylim(0,1) + 
  ylab("Perc. correct") + 
  xlab("Quintile") + 
  theme(aspect.ratio = 1)
ggsave(file.path(plots_dir, "Figure1_results_right.svg"), p, width = 6, height = 6)

df2 <- df1 %>% 
  mutate(correct = as.numeric(subj1 == subj2)) %>% 
  mutate(quintile = as.character(quintile))

saveRDS(df2 ,here("data", "oddoneout", "upperbound_subjects.rds"))

# CNN selection -----------------------------------------------------------

n <- nrow(df)
tm <- create.time.measure(n)
df$top1_outl_fc7 <- NA
for (i in 1:n) {
  categ <- df$category[i]
  
  img_names <- df[i, paste0("im",1:9)] %>% as.matrix()
  
  this_ix <- image_info$new %in% img_names
  # close to center ----------------------------------------------------------
  
  
  fc7_point <- fc7[this_ix, this_ix]
  fc7_point[ order(row.names(fc7_point)), ]
  colnames(fc7_point)
  dd <- as.dist(fc7_point)
  o <- DMwR::outliers.ranking(dd, clus=list(dist='euclidean',alg='hclust',meth = "ward.D"))
  outliers1 <- DMwR::outliers.ranking(dd, clus=list(dist='euclidean',alg='hclust',meth = "ward.D"))$rank.outliers %>% paste(collapse=" ")
  cnn_top1 <- get_top1(outliers1)
  cnn_top3 <- get_top3(outliers1)
  x <- colnames(img_names)
  sel_name <- attr(dd, "Labels")[cnn_top1] %>% str_replace("-","_")
  sel_name3 <- attr(dd, "Labels")[cnn_top3] %>% str_replace("-","_")
  df$top1_outl_fc7[i] <- x[img_names == sel_name] %>% str_replace("im","") %>% as.numeric()
  df$top3_outl_fc7_correct[i] <- as.numeric(df$target_position[i] %in% unlist(x[img_names %in% sel_name3] %>% str_replace("im","") %>% as.numeric()))
  tm <- update(tm)
  print(tm)
}

saveRDS(df ,here("data", "oddoneout", "metrics_fc7_only_200407.rds"))

