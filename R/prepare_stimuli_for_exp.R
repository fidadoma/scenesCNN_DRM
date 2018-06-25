rm(list = ls())

# load data and set paths -------------------------------------------------

library(tidyverse)
theme_set(theme_bw())

col_types <- cols(
  prot_id = col_integer(),
  trial_id = col_integer(),
  category = col_character(),
  target_position = col_integer(),
  selected_image = col_character(),
  quintile = col_integer(),
  im1 = col_character(),
  im2 = col_character(),
  im3 = col_character(),
  im4 = col_character(),
  im5 = col_character(),
  im6 = col_character(),
  im7 = col_character(),
  im8 = col_character(),
  im9 = col_character()
)

if(Sys.info()[[4]] == "FIDADOMA") {
  img_pth <- "D:/documents/databases/konkle"
} else if (Sys.info()[[4]] == "LUKAVSKY-PSU") {
  img_pth <- "g:/konkle"
} else {
  stop("Unknown workstation")
}  

out_dir <- here::here("psychopy","stimuli")

if(!dir.exists(out_dir)) {
  dir.create(out_dir)
}

protocols_dir <- here::here("data","protocols")


# Load all filenames ------------------------------------------------------

prots <- dir(protocols_dir, full.names = T)

filepths <- prots %>% 
  purrr::map(read_csv,col_types = col_types) %>% 
  bind_rows() %>% 
  select(im1:im9) %>% 
  as.matrix() %>% 
  c()


# Copy files --------------------------------------------------------------


ffrom <- file.path(img_pth, filepths)
fto   <- file.path(out_dir, filepths)

copyresult <- file.copy(ffrom, fto)

if(all(copyresult)) {
  print("All files were succesfully copied")
} else {
  print("Some files were not copied!")
}
  

