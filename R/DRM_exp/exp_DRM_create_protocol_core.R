# load figrim data and prepare protocols 

set.seed(190216)

library(tidyverse)

source(here::here("R","utils.R"))

#load("data/m_figrim_l2.RData")
load(here::here("data","file_info.RData"))
load(here::here("data","figrim_vectors.RData"))

prot_dir <- here::here("data","exp_DRM1","protocols")

if(!dir.exists(prot_dir)) {
  dir.create(prot_dir, recursive = T)
}

save_plots <- T

plots_dir <- here::here("plots","exp_DRM1","image_spaces")

if(!dir.exists(plots_dir)) {
  dir.create(plots_dir, recursive = T)
}


categories <- df_files$category %>% unique()

n_targets_per_category <- 15 
n_rep_per_category     <- 3
n_close_distractors    <- 5
n_far_distractors      <- 5

# protocol structure
p <- tibble(category = NA, id_rep = NA, img_name = NA, type = NA)

# repeat for all categories
tm <- FDhelpers::create.time.measure(length(categories) * n_rep_per_category)
for (i in 1:length(categories)) {
  
  # for this category
  cat <- categories[i]
  
  # select image names for this category
  cat_imnames <- df_files %>% filter(category == cat) %>% pull(filename)
  
  # select data frame with given category
  df_cat <- df_figrim_fc7 %>% filter(category == categories[i])
 
  # and extract only the matrix with fc7 vectors
  m <- df_cat %>% select(-category,-filename,-pth) %>% as.matrix()
  rownames(m)  <- cat_imnames
  
  # compute distance matrix using fast implementation for distance matrices
  # this code was used to verify that the distance matrix computed in R is the same as distance matrix computed in python script
  m_dist <- parallelDist::parDist(m) %>% as.matrix()
  cmd_dist_base <- cmdscale(m_dist) %>% as_tibble(rownames = "img_name") %>% mutate(type = "normal")  
  
  # each row is individual image, therefore comuting colmeans is center of the category
  cat_center <- colMeans(m)
  
  # compute distance of all images from this category to the center
  l2_dist_to_center <- l2norm(m, cat_center)
  
  # and compute median of all distances
  med_dist_to_center <- median(l2_dist_to_center)
  
  # select images closer to the center than median of distances
  img_closer_to_center_names <- names(l2_dist_to_center)[l2_dist_to_center < med_dist_to_center]
  img_closer_to_center_coord <- m[rownames(m) %in% img_closer_to_center_names, ]
  
  # select images farther from the center than median of distances
  img_farther_from_center_names <- names(l2_dist_to_center)[l2_dist_to_center > med_dist_to_center]
  img_farther_from_center_coord <- m[rownames(m) %in% img_farther_from_center_names, ]
  
  # as we are looping, we need to set temporary structures (both for names and coordinates)
  img_closer_to_center_names_remaining <- img_closer_to_center_names
  img_closer_to_center_coord_remaining <- img_closer_to_center_coord
  
  img_farther_from_center_names_remaining <- img_farther_from_center_names
  img_farther_from_center_coord_remaining <- img_farther_from_center_coord
  
  
  # we have three samplings from each category
  for(j in 1:n_rep_per_category) {
    shouldContinue <- T
    cmd_dist_rep <- cmd_dist_base
    
    # we repeat until we select valid data
    while(shouldContinue) {
      
      # sample n target images
      target_imgs_names <- sample(img_closer_to_center_names_remaining, n_targets_per_category)  
      target_imgs_coord <- m[rownames(m) %in% target_imgs_names,]
      
      cmd_dist_rep$type[cmd_dist_rep$img_name %in% target_imgs_names] <- "target"
      
      stopifnot(nrow(target_imgs_coord) == n_targets_per_category)
      
      # compute center of the targets
      target_center <- colMeans(target_imgs_coord) # compute center of the area created by target images
      
      # possible distractors are all central images without the target images
      possible_central_distractors_names <- setdiff(img_closer_to_center_names_remaining, target_imgs_names)
      possible_central_distractors_coord <- img_closer_to_center_coord_remaining[!(rownames(img_closer_to_center_coord_remaining) %in% target_imgs_names),]
      
      # select distractors (just the rest of distant images)
      possible_distant_distractors_names <- img_farther_from_center_names_remaining
      possible_distant_distractors_coord <- img_farther_from_center_coord_remaining
      
      
      # select 5 images that are closest to the center
      l2_dist_to_target_center <- l2norm(possible_central_distractors_coord, target_center)
      
      top_n_central_distractors_dist <- l2_dist_to_target_center %>% sort() %>% head(n_close_distractors)
      top_n_central_distractors_names <- names(top_n_central_distractors_dist)
      
      cmd_dist_rep$type[cmd_dist_rep$img_name %in% top_n_central_distractors_names] <- "inner distractors"
      # select 5 images outside the median distance
      
      l2_dist_from_target_center_fargroup <- l2norm(possible_distant_distractors_coord, target_center)
      
      top_n_distant_distractors_dist <- l2_dist_from_target_center_fargroup %>% sort() %>% tail(n_far_distractors)
      top_n_distant_distractors_names <- names(top_n_distant_distractors_dist)
      
      cmd_dist_rep$type[cmd_dist_rep$img_name %in% top_n_distant_distractors_names] <- "outer distractors"
      
      # verify, that the selected images are not the target images
      if(length(intersect(target_imgs_names, top_n_central_distractors_names)) == 0) {
        shouldContinue <- F
        img_closer_to_center_names_remaining <- setdiff(img_closer_to_center_names_remaining,
                                                        union(target_imgs_names, top_n_central_distractors_names))
        img_closer_to_center_coord_remaining <- m[rownames(m) %in% img_closer_to_center_names_remaining, ]
        
        img_farther_from_center_names_remaining <- setdiff(img_farther_from_center_names_remaining,
                                                           top_n_distant_distractors_names)
        img_farther_from_center_coord_remaining <- m[rownames(m) %in% img_farther_from_center_names_remaining, ]
        
        # add new data to raw protocol
        p0 <- tibble(category = cat, 
                     id_rep = j, 
                     img_name = c(target_imgs_names, top_n_central_distractors_names, top_n_distant_distractors_names), 
                     type = c(rep("target", length(target_imgs_names)),
                              rep("close distractor", length(top_n_central_distractors_names)),
                              rep("far distractor", length(top_n_distant_distractors_names))))
        
        #print(nrow(p0))
        
        pg <- cmd_dist_rep %>% 
          ggplot(aes(V1,V2, col = type)) + 
          geom_point() + 
          ggtitle(sprintf("Category - %s, rep - %d", cat, j)) + 
          theme(aspect.ratio = 1)
        
        if(save_plots == T) {
          ggsave(file.path(plots_dir, sprintf("%s_rep%d.png",cat,j)), plot = pg, width = 6, height = 6)
        }
        p <- rbind(p,p0)
        tm <- FDhelpers::update.tm(tm)
        FDhelpers::print.tm(tm)
      }
      
      
      
    }
  }
  
  
 
   
}
# remove firstempty row
p <- p[-1,]

stopifnot(nrow(p) == length(categories)*n_rep_per_category*(n_close_distractors+n_far_distractors+n_targets_per_category))

write_csv(p, path = file.path(prot_dir, "P000.csv"))