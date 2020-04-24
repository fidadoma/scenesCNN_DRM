rm(list = ls())

set.seed(180620)

# prepare data ------------------------------------------------------------

library(tidyverse)
library(FDhelpers)

source(here::here("R", "utils.R"))

outdir <- here::here("data","figures","oddoneout")

if(!dir.exists(outdir)) {
  dir.create(outdir,recursive = T)
}

load(here::here("data","konkle_180621.RData"))

n <- 64

n_points <- 9 # how many images will be there in grid

# selected_images <- image_info %>% sample_n(n)
categories <- sample(image_info$category %>% unique())



variants <- expand.grid(2:4,2:4,2:4) %>% filter(Var1 != Var2, Var2 != Var3, Var1 != Var3)
nvar <- nrow(variants)
tm <- create.time.measure(n*nvar)


  v <- variants[1,]
  p <- create_empty_protocol(j) %>%
    alter_protocol(v)
  
  # randomize order of the categories
  
    categ <- "bathroom"
    
    this_cat_ix <- image_info$category == categ
    
    r <- image_info[this_cat_ix,] %>% sample_n(1)  
    
    this_ix <- image_info$new == r$new
    # close to center ----------------------------------------------------------
    
    fc7_thiscat <- fc7[this_cat_ix,this_cat_ix]
    fc7_point <- fc7[this_ix, this_cat_ix]
    qs <- compute_quintiles(fc7_point)
    gr_close <- select_closest_points(fc7_point, n_points - 1)
    gr_close_names <- names(gr_close)
    
    gr_far2 <- select_random_point_from_quintile(fc7_point, 2, qs)
    gr_far3 <- select_random_point_from_quintile(fc7_point, 3, qs)
    gr_far4 <- select_random_point_from_quintile(fc7_point, 4, qs)
    gr_far2_name <- names(gr_far2)
    gr_far3_name <- names(gr_far3)
    gr_far4_name <- names(gr_far4)
    
    cat("Close names:", gr_close_names)
    cat("quintile 2: ",gr_far2_name)
    cat("quintile 3: ",gr_far3_name)
    cat("quintile 4: ",gr_far4_name)
