# small file info structure

rm(list = ls())

library(tidyverse)

sampleDB_pth <- "c:/Users/filip/Downloads/kata_db/natural/mountain/"

fs <- list.files(sampleDB_pth, pattern = "*.jpg",full.names = T, recursive = T)

category <- fs %>% stringr::str_replace(pattern = ".*/(.*)/sun_.*.jpg","\\1")
filename <- fs %>% stringr::str_replace(pattern = ".*(sun_.*.jpg)","\\1")

file_info <- data_frame(category,filename, pth = fs)

save(df_files, file = "data/file_info_sample.RData")
