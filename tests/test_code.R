library(tidyverse)
library(testthat)

test_that("test protocol", {
  n <- 64
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
  p_pth <- here::here("data","protocols", "P001.csv")
  
  # load fc7 data
  load(here::here("data/konkle_180621.RData"))
  
  if(Sys.info()[[4]] == "FIDADOMA") {
    img_pth <- "D:/documents/databases/konkle"
  } else if (Sys.info()[[4]] == "LUKAVSKY-PSU") {
    img_pth <- "g:/konkle"
  } else {
    stop("Unknown workstation")
  }  
  
  if (!file.exists(p_pth)) {
    stop("Run create_protocol.R first")
  }
  p <- read_csv(p_pth, col_types = col_types)
  
  expect_length(p$category %>% unique(), n)
  expect_length(p$prot_id %>% unique(), 1)
  expect_length(p$prot_id %>% unique(), 1)
  expect_equal(p$trial_id, 1:n)
  expect_equal(p$quintile %>% unique() %>% sort(), 2:4) # we are using quintiles 2:4
  expect_lte(max(p$quintile %>% table()) - min(p$quintile %>% table()), 1) # distribution of all values should be approximatelly equal
  expect_equal(p$target_position %>% unique() %>% sort(), 1:9)
  
  # test, that files can be loaded
  ix <- sample(1:n,3)
  for(i in ix) {
    expect_true(file.exists(file.path(img_pth, p$im1[i])))
    expect_true(file.exists(file.path(img_pth, p$im2[i])))
    expect_true(file.exists(file.path(img_pth, p$im3[i])))
    expect_true(file.exists(file.path(img_pth, p$im4[i])))
    expect_true(file.exists(file.path(img_pth, p$im5[i])))
    expect_true(file.exists(file.path(img_pth, p$im6[i])))
    expect_true(file.exists(file.path(img_pth, p$im7[i])))
    expect_true(file.exists(file.path(img_pth, p$im8[i])))
    expect_true(file.exists(file.path(img_pth, p$im9[i])))
    
    # test fc7
    just_images <- p %>% select(im1:im9)
    
    fc7_all_ix <- colnames(fc7) %in% just_images[i,] %>% as.matrix() %>% c()
    fc7_row <- fc7[colnames(fc7) == p$selected_image[i],fc7_all_ix]
    
    rest_position   <- names(fc7_row) == p$target_image[i]
    target_position <- names(fc7_row) == (p[[paste0("im",p$target_position[i])]][i])
    expect_equivalent(max(fc7_row), 
                 fc7_row[target_position])
    
    # test selection
    fc7_dist <- fc7[colnames(fc7) == p$selected_image[i],image_info$category %in% p$category[i]]
    expect_equal(select_closest_points(fc7_dist, 8) %>% names() %>% sort(), fc7_row[!target_position]  %>% names() %>% sort())
    
    # test quantiles
    qs <- compute_quintiles(fc7_dist)
    expect_gt(fc7_row[target_position],qs[p$quintile[i]])
    expect_lte(fc7_row[target_position],qs[p$quintile[i]+1])
  }
  
  
  
})
