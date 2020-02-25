library(tidyverse)
library(testthat)


prot_file_core <- here::here("data","exp_DRM1","protocols","P000.csv")

if(!file.exists(prot_file_core)) {
  stop("Core protocol file P000.csv does not exist. Run protocol generation script first!")
}

test_that("basic tests for protocols core", {
  p <- read_csv(prot_file_core)
  expect_length(p$id_rep %>% unique(),3)
  expect_length(p$category %>% unique(),21)
  expect_equal(p$type %>% unique() %>% sort(), c("close distractor", "far distractor", "target"))
  
  p_types <- (table(p$type) %>% sort())/(3*21) %>% as.numeric()
  expect_equal(as.vector(p_types), c(5,5,15))
  
})


test_that("tests of image space for protocols core", {
  p <- read_csv(prot_file_core)
  
  load(here::here("data","figrim_vectors.RData")) # load df_figrim_fc7
  
  # test all categories
  df_category_centers <- df_figrim_fc7 %>% group_by(category) %>% summarize_at(vars(V1:V4096), mean)
  
  categories <- df_figrim_fc7$category %>% unique()
  
  for(i in 1:length(categories)) {
    cat <- categories[i]
    df_cat <- df_figrim_fc7 %>% filter(category == cat)
    m <- df_cat %>% select(V1:V4096) %>% as.matrix()
    rownames(m) <- df_cat %>% pull(filename)
    
    cat_center <- df_category_centers %>% filter(category == cat) %>% select(V1:V4096) %>% as.matrix()
    dist_from_center <- l2norm(m, cat_center) 
    med_dist <- median(dist_from_center)
    
    target_names <- p %>% filter(category == cat) %>% filter(type == "target") %>% pull(img_name)
    distractors_inside_names <- p %>% filter(category == cat) %>% filter(type == "close distractor") %>% pull(img_name)
    distractors_outside_names <- p %>% filter(category == cat) %>% filter(type == "far distractor") %>% pull(img_name)
    
    expect_true(all(dist_from_center[names(dist_from_center) %in% target_names] < med_dist))
    expect_true(all(dist_from_center[names(dist_from_center) %in% distractors_inside_names] < med_dist))
    expect_true(all(dist_from_center[names(dist_from_center) %in% distractors_outside_names] > med_dist))
    expect_length(intersect(target_names, distractors_inside_names), 0)
    expect_length(intersect(target_names, distractors_outside_names), 0)
    expect_length(intersect(distractors_inside_names, distractors_outside_names), 0)
    
    
  }
})

prot_file <- here::here("data","exp_DRM1","protocols","P001.csv")

test_that("tests for individual protocols", {
  p <- read_csv(prot_file)
  expect_true(p$prot_id %>% unique() %>% length() == 1)
  expect_true(p$grp_order %>% unique() %>% length() == 21*3)
  expect_equal(p$query %>% unique(), c(0,1))
  expect_true(all(table(p$category,p$id_rep) == 30))
  expect_true(all(table(p$category,p$id_rep,p$query) == 15))
  expect_true(all(table(p$trial_id)==1))
  expect_equal(p$trial_id,1:nrow(p))
  expect_true(all(p %>% 
                 group_by(grp_order,query) %>% 
                 summarize(min = min(trial_id),max = max(trial_id)) %>%
                 group_by(grp_order) %>% 
                 summarize(tst = max[query == 0] == min[query == 1] - 1) %>% 
                 .$tst))
  tb <- table(p$type, p$query)
  expect_true(all(tb[rownames(tb) != "target",colnames(tb) == "0"] == 0))
  #expect_true(p %>% group_by(grp_order,) %>% .$query)
})