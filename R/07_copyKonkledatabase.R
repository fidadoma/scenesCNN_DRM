rm(list = ls())

library(tidyverse)
library(FDhelpers)

load(here::here("data","konkle_180621.RData"))


if(Sys.info()[[4]] == "FIDADOMA") {
  out_dir <- "D:/documents/databases/konkle"
  konkle_dir <- "D:/documents/databases/konkle_raw"
} else if (Sys.info()[[4]] == "LUKAVSKY-PSU") {
  out_dir <- "g:/konkle"
  konkle_dir <- "g:/konkle_raw"
  
} else {
  stop("Unknown workstation")
}

tm <- create.time.measure(nrow(image_info))
for(i in 1:nrow(image_info)) {
  r <- image_info[i,]
  oldf <- file.path(konkle_dir, r$dir_orig, r$category_orig, r$old)
  newf <- file.path(out_dir, r$new %>% stringr::str_replace("-","_"))
  file.copy(oldf, newf)
  tm <- update.tm(tm)
  print(tm)
}