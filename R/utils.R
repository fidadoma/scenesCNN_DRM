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

# we have 9 positions of images, 64 categories, 3 inner quintiles
create_empty_protocol <- function(prot_id, n_images = 9, n_categories = 64, n_quintiles = 3) {
  n_trials <- n_categories
  
  new_cols <- data_frame(c = paste0("im", 1:9), v = "") %>% 
    spread(c,v) %>% 
    slice(rep(1:n(), each = n_trials))
  
  p <- data_frame(prot_id = rep(prot_id, n_trials),
                  trial_id = 1:n_trials, 
                  category = "",
                  target_position = sample(n_images,n_trials,replace=T),
                  selected_image = "",
                  quintile = rep(2:4, length.out = n_trials)) %>% 
    cbind(new_cols) %>% 
    as_data_frame()
  p
}

alter_protocol <- function(p, v) {
  p$quintile <- recode(p$quintile, `2` = v[[1]], `3` = v[[2]], `4` = v[[3]])
  p
}

compute_quintiles <- function(p) {
  quantile(p, probs = seq(0, 1, 0.2))
}

select_closest_points <- function(p, n_points) {
  sort(p)[1:n_points]
}

select_random_point_from_quintile <- function(p, q, qs) {
   sample(p[p > qs[q] & p <= qs[q+1]],1)
}


order_all_outliers <- function(df1) {
  df2 <- data_frame(prot_id = unique(df1$prot_id), trial_id=unique(df1$trial_id))
  df2$ssim_order <- order_outliers(df1,"ssim")$rank.outliers %>% paste(collapse=" ")
  df2$hog_order <- order_outliers(df1,"hog")$rank.outliers %>% paste(collapse=" ")
  df2$gist_order <- order_outliers(df1,"gist")$rank.outliers %>% paste(collapse=" ")
  df2$sift_sum <- order_outliers(df1,"sift_sum")$rank.outliers %>% paste(collapse=" ")
  df2
}

order_outliers <- function(df1, v) {
  df1$v <- df1[[v]]
  gr1 <- df1 %>% select(im1,im2, v) %>% spread(im1,v) %>% select(-im2) %>% as.matrix() 
  rownames(gr1) <- colnames(gr1)
  DMwR::outliers.ranking(as.dist(gr1), clus=list(dist='euclidean',alg='hclust',meth = "ward.D"))
  
}

get_top1 <- function(x) {
  x %>% strsplit(split = " ") %>% unlist() %>% as.numeric() %>% which.max()
  
}
