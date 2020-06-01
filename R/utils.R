select_max <- function(dfx) {
  tb <- table(c(dfx$ssim_top1[1], dfx$gist_top1[1],dfx$sift_top1[1],dfx$pdistRGB_top1[1]))
  tb <- sort(tb,decreasing = T)
  
  if(sum(tb==tb[1])>1) {
    metrics_max <- NA
  } else {
    metrics_max <- (names(tb[1]) %>% as.numeric())
    if(is_empty(metrics_max)) {
      metrics_max <- NA
    }
  }
  metrics_max
}



eucldist <- function(x1,x2,y1,y2) {
  sqrt(sum((x1-x2)^2) + sum((y1-y2)^2))
}

l2norm <- function(M, v2) {
  if(!is.matrix(M)) {
    sqrt(sum((M-v2)^2))
  } else {
    apply(M, 1, function(v1) sqrt(sum((v1-v2)^2)))  
  }
  
}

select_images <- function(df_files, n_per_category) {
  df_files %>% sample_n(n_per_category)
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
  
  new_cols <- tibble(c = paste0("im", 1:9), v = "") %>% 
    spread(c,v) %>% 
    slice(rep(1:n(), each = n_trials))
  
  p <- tibble(prot_id = rep(prot_id, n_trials),
                  trial_id = 1:n_trials, 
                  category = "",
                  target_position = sample(n_images,n_trials,replace=T),
                  selected_image = "",
                  quintile = rep(2:4, length.out = n_trials)) %>% 
    cbind(new_cols) %>% 
    as_tibble()
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
  df2$pdistRGB <- order_outliers(df1,"pdistRGB")$rank.outliers %>% paste(collapse=" ")
  df2
}

order_outliers <- function(df1, v) {
  df1$v <- df1[[v]]
  gr1 <- df1 %>% select(im1,im2, v) %>% spread(im1,v) %>% select(-im2) %>% as.matrix() 
  rownames(gr1) <- colnames(gr1)
  
  DMwR::outliers.ranking(as.dist(gr1), clus=list(dist='euclidean',alg='hclust',meth = "ward.D"))
  
}

get_top1 <- function(x) {
  aa <- x %>% strsplit(split = " ") %>% unlist() %>% as.numeric() 
  aa[1]
  
  
}
get_top3 <- function(x) {
  #x <- x %>% strsplit(split = " ") %>% unlist() %>% as.numeric()
  #top1 <- x %>% which.max()
  #x[top1] <- -1 
  #top2 <- x %>% which.max()
  #x[top2] <- -1
  #top3 <- x %>% which.max()
  #c(top1,top2,top3)
  aa <- x %>% strsplit(split = " ") %>% unlist() %>% as.numeric() 
  aa[1:3]
}

trial_id_with_gaps <- function(from = 1, to = 15, gapsize = 15, ngaps = 10) {
  purrr::map(seq(0,ngaps - 1),function(x) ((to+gapsize)*x)+(from:to)) %>% unlist()
}

get_dur <- function(log_file) {
  list_lines <- str_split(log_file,pattern ="\n") 
  list_lines <- list_lines[[1]] %>% stringi::stri_remove_empty()
  strttime <- list_lines[1] %>% str_split(pattern = " ", simplify = T)
  
  endtime <- tail(list_lines,1) %>% str_split(pattern = " ", simplify = T)
  
  as.numeric(endtime[1])-as.numeric(strttime[1])
}

create_roc_data <- function(df, without_type) {
  df %>% 
    unite(resp, key_resp, confidence) %>% 
    filter(type!=without_type) %>% 
    mutate(resp = factor(resp, levels = c("old_very sure","old_somewhat sure", "old_not at all sure","new_not at all sure", "new_somewhat sure", "new_very sure"))) %>% 
    select(subject_id, resp, corrKey) %>% 
    nest(-subject_id) %>% 
    group_by(subject_id) %>% 
    do(prepare_roc_curve(.$data))
  
}

prepare_roc_curve <- function(df) {
  df1 <- df[[1]]
  df_raw <- table(df1$corrKey, df1$resp) %>% 
    prop.table(margin = 1) %>% 
    apply(.,1,cumsum) %>% 
    .[-6,] 
  
  
  df_out <- tibble(var = paste0("p",1:5),
         Hs = df_raw[,2],
         FAs = df_raw[,1],
         zHs = qnorm(Hs),
         zFAs = qnorm(FAs),
         dprime = df_raw[,2]-df_raw[,1])
         
  rbind(df_out,
    tibble(var = c("p0","p6"), Hs = c(0,1),FAs = c(0,1), zHs = c(NA,NA),zFAs = c(NA,NA), dprime = c(NA,NA))) %>% 
    arrange(var)
  
  
}

get_ROC_slopes <- function(df) {
  zROC_params_oldclose <- df %>% 
    ungroup() %>% 
    filter(!is.na(dprime)) %>% 
    filter(!is.infinite(zHs)) %>%
    filter(!is.infinite(zFAs)) %>% 
    split(.$subject_id) %>% 
    purrr::map(~lm(zHs~zFAs,.)) %>% 
    purrr::map_dfc(coef)
  zROC_params_oldclose[2,] %>% gather() %>% pull(value)
  
}

get_ROC_intercepts <- function(df) {
zROC_params_oldclose <- df %>% 
  ungroup() %>% 
  filter(!is.na(dprime)) %>% 
  filter(!is.infinite(zHs)) %>%
  filter(!is.infinite(zFAs)) %>% 
  split(.$subject_id) %>% 
  purrr::map(~lm(zHs~zFAs,.)) %>% 
  purrr::map_dfc(coef)
zROC_params_oldclose[1,] %>% gather() %>% pull(value)

}

compute_da <- function(H,FA,s) {
  sqrt(2/(1+s^2))*(qnorm(H)-s*qnorm(FA))
}

compute_c2 <- function(H,FA,s) {
  (-s/(1+s))*(qnorm(H)-qnorm(FA))
}

plot_normal_ROC <- function(l) {
  df <- NULL 
  df_HF <- tibble(H = NA,FA = NA,label = l$label)
  Hs  <- seq(0, 1, 0.01)
  
  for (i in 1:length(l$H)) {
    H <- l$H[i]
    FA <- l$FA[i]
    if (H < FA) {
      FA   <- 1-FA
      H <- 1-H
    }
    
    df_HF$H[i] <- H
    df_HF$FA[i] <- FA
    d   <- compute_dprime(H, FA)
    
    FAs <- compute_FA_from_dprime(d,Hs)
    if(is.null(df)){ 
      df <- tibble(Hit=Hs,`False alarm` = FAs, label = l$label[i])
    } else {
      df <- rbind(df, tibble(Hit=Hs,`False alarm` = FAs, label = l$label[i]))
    }
    
    
  }    
  df %>% ggplot(aes(y = Hit, x = `False alarm`, group = label)) + 
    geom_line() + 
    geom_point(data = df_HF, mapping = aes(x = FA, y = H, col = label), size = 2) + 
    geom_text(data = df_HF, mapping = aes(x = FA, y = H+0.05, label = as.character(compute_dprime(H,FA) %>% round(2))), size = 6) + 
    scale_color_discrete("Participant") +
    xlab("False alarm") + 
    theme(aspect.ratio = 1)
  
}

divide_into_groups <- function(dfxx, grp_var, lower_grp, higher_grp) {
  
  dfxx_notgt <- dfxx %>% filter(type != "target") %>% mutate(model_type = NA)
  
  rnk <- rank(dfxx_notgt[[grp_var]])
  
  dfxx_notgt$model_type[rnk <= 5] <- lower_grp
  dfxx_notgt$model_type[rnk > 5]  <- higher_grp
  
  dfxx %>% 
    left_join(dfxx_notgt %>% select(img_name,model_type), by="img_name") %>% 
    mutate(model_type = if_else(type == "target","target",model_type))
}

gt0 <- function(x) {x >0}

print_table1 <- function(){
  df2_modelmem1 <- 
    df2_modelmem %>% 
    ungroup() %>% 
    select(subject_id, trial_id, category_centroid_type = type, memorability_model =model_type, correct, category)
  
  df2_modeldisttoproto31 <- 
    df2_modeldisttoproto3  %>% 
    ungroup() %>% 
    select(subject_id, trial_id, dist_to_proto3 =model_type, correct, category) 
  
  df2_modeldisttoproto51 <- 
    df2_modeldisttoproto5  %>% 
    ungroup() %>% 
    select(subject_id, trial_id, dist_to_proto5 =model_type, correct, category) 
  
  df2_tgt_modeldist1 <- 
    df2_tgt_modeldist  %>% 
    ungroup() %>% 
    select(subject_id, trial_id, min_dist_to_tgt =model_type, correct, category) 
  df_models_res <- df2_modelmem1 %>% 
    left_join(df2_modeldisttoproto31, by = c("subject_id", "trial_id", "correct","category")) %>% 
    left_join(df2_modeldisttoproto51, by = c("subject_id", "trial_id", "correct","category")) %>% 
    left_join(df2_tgt_modeldist1, by = c("subject_id", "trial_id", "correct","category")) %>% 
    filter(category_centroid_type != "target")
  
  df_models_res_longer <- df_models_res %>% 
    pivot_longer(cols = c(category_centroid_type, min_dist_to_tgt, dist_to_proto5, dist_to_proto3, memorability_model)) 
  
  
  glmm_null_model <- df_models_res %>% glmer(correct~1+(1|subject_id) + (1|category), ., family = binomial())
  
  glmm_orig_model <- df_models_res %>% glmer(correct~category_centroid_type+(1+category_centroid_type|subject_id) + (1|category), ., family = binomial())
  glmm_dist_to_tgt <- df_models_res %>% glmer(correct~min_dist_to_tgt + (1+min_dist_to_tgt|subject_id) + (1|category),., family = binomial())
  glmm_dist_to_proto_conv5 <- df_models_res %>% glmer(correct~dist_to_proto5 + (1+dist_to_proto5|subject_id) + (1|category),., family = binomial())
  glmm_dist_to_proto_conv3 <- df_models_res %>% glmer(correct~dist_to_proto3 + (1+dist_to_proto3|subject_id) + (1|category),., family = binomial())
  glmm_memorability <- df_models_res %>% glmer(correct~memorability_model + (1+memorability_model|subject_id) + (1|category),., family = binomial())
  models_list <- list(glmm_null_model,glmm_orig_model,glmm_dist_to_proto_conv3,glmm_dist_to_proto_conv5,glmm_dist_to_tgt,glmm_memorability)
  
  XXtab <- rbind(tibble(name="null_model",value = "-", FA = NA),
        df_models_res_longer %>% 
    group_by(name,value) %>% 
    summarize(FA = sprintf("%.2f", mean(1-correct))) %>% 
    ungroup()) %>% 
    mutate(name = factor(name, levels = c("null_model","category_centroid_type","dist_to_proto3","dist_to_proto5","min_dist_to_tgt","memorability_model"))) %>% 
    arrange(name)
  
 
  R2_cond <- (models_list %>% 
                map(performance::r2_nakagawa) %>%
                map_dbl("R2_conditional") %>% 
                round(2) %>% rep(each = 2))[-1]
  XXtab$R2_conditional <- R2_cond
  
  XXtab$R2_marginal <- (models_list %>% 
    map(performance::r2_nakagawa) %>%
    map_dbl("R2_marginal") %>% 
    round(2)%>% rep(each = 2))[-1]
  
  XXtab$AIC <- (models_list %>% 
    map_dbl(AIC)%>% rep(each = 2))[-1]
  XXtab
}