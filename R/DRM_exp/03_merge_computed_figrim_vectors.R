library(tidyverse)
library(pbapply)

load("data/file_info.RData")

# fc7 layer ---------------------------------------------------------------

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

df_figrim_fc7 <- df_files %>% left_join(df_figrim_fc7,by = c("filename" = "fname"))

# we removed some artefacts, therefore we need to filter those values
df_figrim_fc7 <- df_figrim_fc7 %>% filter(!is.na(V1))
save(df_figrim_fc7, file = "data/figrim_fc7_vectors.RData")


# conv5 layer ---------------------------------------------------------------

fs <- list.files("data/figrim_vectors/", full.names = T,pattern = "figrim_conv5_*") 

my_read <-function(x) {
  fname <- str_extract(x,"sun_[a-z]*.jpg")
  xx <- read_delim(x, delim = ",",col_names = F, col_types = cols(X1 = col_double())) 
  names(xx) <- fname
  xx
}

all_files <- bind_cols(pblapply(fs, my_read))

img_names <- colnames(all_files)

m <- t(all_files) %>% as.matrix()

df_figrim_conv5 <- m %>% as_tibble() %>% mutate(fname = img_names) %>% select(fname,everything())


df_figrim_conv5 <- df_files %>% left_join(df_figrim_conv5,by = c("filename" = "fname"))

# we removed some artifacts, therefore we need to filter those values
df_figrim_conv5 <- df_figrim_conv5 %>% filter(!is.na(V1))
save(df_figrim_conv5, file = "data/figrim_conv5_vectors.RData")

# conv3 layer ---------------------------------------------------------------

fs <- list.files("data/figrim_vectors/", full.names = T,pattern = "figrim_conv3_*") 

my_read <-function(x) {
  fname <- str_extract(x,"sun_[a-z]*.jpg")
  xx <- read_delim(x, delim = ",",col_names = F, col_types = cols(X1 = col_double())) 
  names(xx) <- fname
  xx
}

all_files <- bind_cols(pblapply(fs, my_read))

img_names <- colnames(all_files)

m <- t(all_files) %>% as.matrix()

df_figrim_conv3 <- m %>% as.tibble() %>% mutate(fname = img_names) %>% select(fname,everything())


df_figrim_conv3 <- df_files %>% left_join(df_figrim_conv3,by = c("filename" = "fname"))

# we removed some artifacts, therefore we need to filter those values
df_figrim_conv3 <- df_figrim_conv3 %>% filter(!is.na(V1))
save(df_figrim_conv3, file = "data/figrim_conv3_vectors.RData")


# divide vectors into smaller ones ----------------------------------------

## files might be too big, lets divide them into smaller ones

## conv3

conv3_vectors_dir <- here::here("data/figrim_vectors_per_category/conv3")

if(!dir.exists(conv3_vectors_dir)) {
  dir.create(conv3_vectors_dir, recursive = T)
}

categories <- df_figrim_conv3$category %>% unique()

tm <- FDhelpers::create.time.measure(length(categories))

for (i in 1:length(categories)) {
  file_pth <- file.path(conv3_vectors_dir, sprintf("%s.rds",categories[i]))
  tm <- FDhelpers::update.tm(tm)
  saveRDS(df_figrim_conv3 %>% filter(category == categories[i]),file = file_pth)  
  FDhelpers::print.tm(tm)
}

## conv5

conv5_vectors_dir <- here::here("data/figrim_vectors_per_category/conv5")

if(!dir.exists(conv5_vectors_dir)) {
  dir.create(conv5_vectors_dir, recursive = T)
}

categories <- df_figrim_conv5$category %>% unique()

tm <- FDhelpers::create.time.measure(length(categories))

for (i in 1:length(categories)) {
  file_pth <- file.path(conv5_vectors_dir, sprintf("%s.rds",categories[i]))
  tm <- FDhelpers::update.tm(tm)
  saveRDS(df_figrim_conv5 %>% filter(category == categories[i]),file = file_pth)  
  FDhelpers::print.tm(tm)
}