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
  cbind(df, d=l2norm(df %>% select(-id), cent)) %>%  top_n(n_points, -d)
}

select_points_farthest_from_centerL2 <- function(df, cent, n_points = 1) {
  cbind(df, d=l2norm(df %>% select(-id), cent)) %>%  top_n(n_points, d)
}
