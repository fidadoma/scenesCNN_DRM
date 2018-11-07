rm(list = ls())

set.seed(167)

load(here::here("data","konkle_180621.RData"))
df_metrics <- readRDS(here::here("data","pairwise_metrics_181106.rds"))

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

df_metrics_full <- rbind(df_metrics, (df_metrics %>% mutate(tmp = im1, im1 = im2, tmp_id= im1_id, im1_id = im2_id) %>% mutate(im2 = tmp, im2_id = tmp_id) %>% select(-tmp, -tmp_id)))

library(dendextend)

df_metrics_full

gr1 <- df_metrics_full %>% filter(prot_id == 1, trial_id == 1) %>% select(im1,im2,ssim) %>% spread(im1,ssim) %>% select(-im2) %>% as.matrix() 
rownames(gr1) <- colnames(gr1)

x <- DMwR::outliers.ranking(as.dist(gr1))
x$rank.outliers
for (i in nrow(gr1)) {
  gr_noi <- gr1[-i,-i]
  as.dist(gr_noi) %>% pam
}

cl1 <- as.dist(cl1) %>% hclust(method = "ward.D")

plot(cl1)

dd <- as.dendrogram(cl1)

h <- get_nodes_attr(dd, "height")
rank(h[h>0])


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
         sift_top1 = get_top1(sift_sum)) %>% 
  ungroup()
         
df2 <- df %>% left_join(df_ordered_outliers2,  by = c("prot_id", "trial_id")) %>% 
  mutate(ssim_correct = as.numeric(ssim_top1 == target_position),
         hog_correct = as.numeric(hog_top1 == target_position),
         gist_correct = as.numeric(gist_top1 == target_position),
         sift_correct = as.numeric(sift_top1 == target_position))

df3 <- df2 %>% gather(metric, value, correct, ssim_correct,hog_correct,gist_correct,sift_correct)

df3 %>% ggplot(aes(x = quintile, y = value, col = metric)) + 
  stat_summary(fun.data = "mean_cl_boot") + 
  ylim(0,1)+
  theme(aspect.ratio = 1) +
  geom_hline(yintercept = 1/9) +
  ylab("Perc. correct") +
  xlab("Quintile") + 
  theme(text = element_text(size=14)) + scale_color_discrete(labels = c("Humans", "GIST", "HOG", "SIFT", "SSIM")) + 
  stat_summary(fun.y=mean, geom="line")
  
