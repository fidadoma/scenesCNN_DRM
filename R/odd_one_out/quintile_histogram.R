# Simple plot for visualizng histograms, I used prettier version in the thesis

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

categ <- "woods"

this_cat_ix <- image_info$category == categ

r <- image_info[this_cat_ix,] %>% sample_n(1)  

this_ix <- image_info$new == r$new
# close to center ----------------------------------------------------------

fc7_thiscat <- fc7[this_cat_ix,this_cat_ix]
fc7_point <- fc7[this_ix, this_cat_ix]
qs <- compute_quintiles(fc7_point)

df <- tibble(imname = names(fc7_point), d = fc7_point)
df <- df %>% mutate(quintile = as.factor(ifelse(d<=qs[2], 1, 
                                      ifelse(d<=qs[3], 2, ifelse(d <= qs[4],3,
                                                                 ifelse(d<=qs[5], 4, 5)))
                                      ))) %>% 
  filter(d>0)
df %>% ggplot(aes(x=d, col = quintile, fill = quintile)) + geom_histogram(bins = 30, position="identity") + theme(aspect.ratio = 2) + theme_bw(18)

