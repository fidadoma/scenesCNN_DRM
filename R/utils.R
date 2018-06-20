eucldist <- function(x1,x2,y1,y2) {
  sqrt(sum((x1-x2)^2) + sum((y1-y2)^2))
}

l2norm <- function(M, v2) {
  apply(M, 1, function(v1) sqrt(sum((v1-v2)^2)))
}

select_points_close_to_center <- function(df, cent, n_points = 9) {
  xc <- cent[1]
  yc <- cent[2]
  df %>% mutate(d = eucldist(x,xc,y,yc)) %>% top_n(n_points, -d)
}

select_points_close_to_centerL2 <- function(df, cent, n_points = 9) {
  cbind(df, d=l2norm(df %>% select(-id), cent)) %>%  top_n(n_points, -d) %>% arrange(d)
}

select_points_farthest_from_centerL2 <- function(df, cent, n_points = 1) {
  cbind(df, d=l2norm(df %>% select(-id), cent)) %>%  top_n(n_points, d) %>% arrange(d)
}

# we have 9 positions of images, 12 categories, 12 repetitions from each category, 3 inner quintiles
create_empty_protocol <- function(prot_id, n_images = 9, n_categories = 12, n_quintiles = 3, n_rep_per_category = 12) {
  
  n_trials <- n_categories*n_rep_per_category*n_quintiles
  
  new_cols <- data_frame(c = paste0("im", 1:9), v = "") %>% 
    spread(c,v) %>% 
    slice(rep(1:n(), each = n_trials))
  expand.grid(category_type = 1:3, stringsAsFactors = F)
  p <- data_frame(prot_id = rep(prot_id, n_trials),
                  trial_id = 1:n_trials, 
                  target_position = sample(n_images,n_trials,replace=T),
                  ) %>% 
    cbind(new_cols) %>% 
    as_data_frame()
  p$
}
