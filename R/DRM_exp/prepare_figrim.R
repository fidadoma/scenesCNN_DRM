library(tidyverse)
library(pbapply)

fs <- list.files("data/figrim_vectors/", full.names = T,pattern = "figrim_fc7_*") 

my_read <-function(x) {
  fname <- str_extract(x,"sun_[a-z]*.jpg")
  xx <- read_delim(x, delim = ",",col_names = F, col_types = cols(X1 = col_double())) 
  names(xx) <- fname
  xx
}

all_files <- bind_cols(pblapply(fs, my_read))

img_names <- colnames(all_files)

m <- t(all_files) %>% as.matrix()

df_figrim_fc7 <- m %>% as.tibble() %>% mutate(fname = img_names) %>% select(fname,everything())

load("data/file_info.RData")
df_figrim_fc7 <- df_files %>% left_join(df_figrim_fc7,by = c("filename" = "fname"))

save(df_figrim_fc7, file = "data/figrim_vectors.RData")
