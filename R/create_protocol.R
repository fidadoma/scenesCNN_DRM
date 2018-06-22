rm(list = ls())

set.seed(180620)

# prepare data ------------------------------------------------------------

library(tidyverse)
library(FDhelpers)

source(here::here("R", "utils.R"))

outdir <- here::here("data","protocols")

if(!dir.exists(outdir)) {
  dir.create(outdir)
}

load(here::here("data","konkle_180621.RData"))

n <- 64

n_points <- 9 # how many images will be there in grid

# selected_images <- image_info %>% sample_n(n)
categories <- sample(image_info$category %>% unique())



variants <- expand.grid(2:4,2:4,2:4) %>% filter(Var1 != Var2, Var2 != Var3, Var1 != Var3)
nvar <- nrow(variants)
tm <- create.time.measure(n*nvar)

for(j in 1:nvar) {
  v <- variants[j,]
  p <- create_empty_protocol(j) %>%
    alter_protocol(v)
  
  # randomize order of the categories
  categories <- sample(categories)
  
  for (i in 1:n) {
    categ <- categories[i]
    
    this_cat_ix <- image_info$category == categ
    
    r <- image_info[this_cat_ix,] %>% sample_n(1)  
    
    this_ix <- image_info$new == r$new
    # close to center ----------------------------------------------------------
    
    fc7_thiscat <- fc7[this_cat_ix,this_cat_ix]
    fc7_point <- fc7[this_ix, this_cat_ix]
    qs <- compute_quintiles(fc7_point)
    gr_close <- select_closest_points(fc7_point, n_points - 1)
    gr_close_names <- names(gr_close)
    
    gr_far <- select_random_point_from_quintile(fc7_point, p$quintile[i], qs)
    gr_far_name <- names(gr_far)
    
    # replace the most distant point in close points with the most distant one
    p[[paste0("im",p$target_position[i])]][i] <- gr_far_name
    p[paste0("im",setdiff(1:9,p$target_position[i]))][i,] <- gr_close_names
    p$category[i] <- categ
    p$selected_image[i] <- r$new
    # we can't use those images for next trials
    keep_ix <- !(image_info$new %in% c(gr_close_names, gr_far_name))
    image_info <- image_info[keep_ix,]
    fc7 <- fc7[keep_ix,keep_ix]
    tm <- update(tm)
    print(tm)
  }
  write_csv(p, file.path(outdir, sprintf("P%03d.csv",j)))
}