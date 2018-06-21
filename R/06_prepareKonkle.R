rm(list = ls())

# load data ---------------------------------------------------------------

library(tidyverse)

fc7 <- readRDS(here::here("data","konkle","image_distances.rds"))$L2_fc7

image_names <- readRDS(here::here("data","konkle","original_filenames.rds"))
image_names$old <- as.character(image_names$old)
image_names$new <- as.character(image_names$new)

removed_images <- readRDS(here::here("data","konkle","removed_duplicates.rds"))

category_64_scenes_konkle <- c("concert_hall-68","corridor-68","facadebuilding-68","frontshop-68","gas-station-68","grocerystore-68","homeoffice-68","industrialarea-68","livingroom-68","parking-68","ruins-68","stairscase-68","stream-68","toystore-64","train-68","warehouse-68")
category_64_scenes_ours <- c("concert_hall","corridor","facadebuilding","frontshop","gas-station","grocerystore","homeoffice","industrialarea","livingroom","parking","ruins","stairscase","stream","toystore","train","warehouse")

category_64 <- data_frame(category = category_64_scenes_ours, category_orig = category_64_scenes_konkle, dir_orig = "64-scenes")

category_68_scenes_konkle <- c("airport-68","amusementpark-68","bar-68","barn-68","bathroom-68","beach-68","bedroom-68","bridge-68","buffet-68","campsite-68","canyon-68","castle-68","cavern-68","cemetery-68","classroom-68","closet-68","coast-68","conferenceroom-68","constructionsite-68","countryroad-68","desert-68","diningroom-68","doors-68","empty-68","field-68","garden-68","golfcourse-68","greenhouse-68","gym-68","hairsalon-68","house-68","church-inside-68","iceberg-68","kitchen-68","library-68","lobby-68","mountainwhite-68","playground-68","restaurant-68","seaport-68","skyscraper-68","street-68","swimmingpool-68","temple-68","tenniscourt-68","underwater-68","waves-68","woods-68")
category_68_scenes_ours <- c("airport","amusementpark","bar","barn","bathroom","beach","bedroom","bridge","buffet","campsite","canyon","castle","cavern","cemetery","classroom","closet","coast","conferenceroom","constructionsite","countryroad","desert","diningroom","doors","empty","field","garden","golfcourse","greenhouse","gym","hairsalon","house","church-inside","iceberg","kitchen","library","lobby","mountainwhite","playground","restaurant","seaport","skyscraper","street","swimmingpool","temple","tenniscourt","underwater","waves","woods")

category_68 <- data_frame(category = category_68_scenes_ours, category_orig = category_68_scenes_konkle, dir_orig = "68-scenes")

categories <- rbind(category_64, category_68)



# create image struct -----------------------------------------------------

image_info <- image_names %>% 
  filter(!new %in% removed_images) %>%
  mutate(category = str_replace(new, "^(.*)_[:digit:]*\\.jpg$","\\1")) %>% 
  left_join(categories, by = "category")


# clean fc7 data ----------------------------------------------------------

fc7_names <- colnames(fc7)

fc7 <- fc7[fc7_names != "zzz_1.jpg",fc7_names != "zzz_1.jpg"]

fc7_names <- colnames(fc7)

fc7 <- as.matrix(fc7)


# save data ---------------------------------------------------------------

save(fc7, fc7_names, image_info, file = here::here("data","konkle_180621.RData"))
