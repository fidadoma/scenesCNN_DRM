rm(list = ls())

library(tidyverse)

# load m and im_names objects
load(here::here("data","m_figrim_l2.RData"))

load(here::here("data","/file_info.RData"))

f_files <-  data_frame(filename = im_names) %>% left_join(df_files, by = c("filename"))

categories <- df_files %>% select(category) %>% distinct(category)

tm <- FDhelpers::create.time.measure(nrow(categories))

# this code takes long time to run
#
m_all <- cmdscale(m)
m_all <- df_files %>% cbind(data_frame(x=m_all[1:9428,1], y=m_all[1:9428,2]) ) %>% as_data_frame()
saveRDS(m_all, file = "data/mds_all_180613.RData")

m_all %>% ggplot(aes(x,y, col = category)) + geom_point()

# run mds for each category

for (i in 1:nrow(categories)) {
  sel_category <- categories$category[i]
  
  ix <- df_files$category == sel_category
  
  df_onecat <- df_files %>% filter(category == sel_category)
  
  m_onecat <- m[ix,ix]
  
  m_onecat %>% rowMeans(na.rm = T)
  
  cv <- cluster::pam(m_onecat %>% as.dist(), 3)
  
  mds_onecat <- cmdscale(m_onecat)
  mds_onecat <- data_frame(V1 = mds_onecat[,1], V2 = mds_onecat[,2], col = as.factor(cv$clustering))
  png(here::here("plots", sprintf("%s_mds.png", sel_category)))
  p <- ggplot(mds_onecat, aes( x = V1, y = V2, col = col)) + 
    geom_point() +
    ggtitle(sprintf("%s (n=%d)", sel_category, nrow(mds_onecat))) +
    theme(aspect.ratio = 1)
  ggsave(here::here("plots", sprintf("%s_mds.png", sel_category)), plot = p)
  
  #colMeans(mds_onecat)
  #df_mds_onecat <- mds_onecat 
  #colnames(df_mds_onecat) <- c("V1", "V2")
  #df_mds_onecat <- cbind(df_onecat, df_mds_onecat) %>% as_tibble()
  
  #p <- plot_ly(df_mds_onecat, x = ~V1, y = ~V2, type = "scatter", mode = "markers")
  #p
  
  tm <- FDhelpers::update.tm(tm)
  FDhelpers::print.tm(tm)
}
