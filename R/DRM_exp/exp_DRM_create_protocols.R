# load figrim data and prepare protocols 

set.seed(190418)

library(tidyverse)

source(here::here("R","utils.R"))

prot_dir <- here::here("data","exp_DRM1","protocols")

stopifnot(dir.exists(prot_dir)) 

p0 <- read_csv(file.path(prot_dir, "P000.csv"))

p0 <- p0 %>% mutate(category = str_replace(category, "_", " "))

nProt <- 20

for (i in 1:nProt) {
  p0_order <- p0 %>% 
    select(category, id_rep) %>% 
    distinct() %>% 
    sample_frac(1) %>% 
    mutate(grp_order = 1:n())
  
  p_pres <- p0 %>% 
    filter(type == "target") %>% 
    mutate(query = 0) %>% 
    group_by(category, id_rep) %>% 
    sample_frac(1) %>% 
    left_join(p0_order, by = c("category", "id_rep")) %>% 
    arrange(grp_order)
  
  
  ngaps <- nrow(p_pres) / 15
  
  p_pres <- 
    p_pres %>%
    ungroup() %>% 
    mutate(trial_id = trial_id_with_gaps(from = 1, to = 15, gapsize = 15, ngaps = ngaps))
  
  
  p_q <- p0 %>% 
    group_by(category, id_rep, type) %>% 
    sample_n(5) %>% 
    mutate(query = 1) %>% 
    group_by(category, id_rep) %>% 
    sample_frac(1) %>% 
    left_join(p0_order, by = c("category", "id_rep")) %>% 
    arrange(grp_order) 
  
  ngaps_query <- nrow(p_q) / 15
  
  p_q <- p_q %>%
    ungroup() %>% 
    mutate(trial_id = trial_id_with_gaps(from = 16, to = 30, gapsize = 0, ngaps = ngaps_query))
  
  
  p <- rbind(p_pres,p_q) %>% arrange(trial_id)
  p <- p %>% 
    mutate(prot_id = i) %>% 
    mutate(corrKey = if_else(query == 1,
                             if_else(type == "target", "left", "right"),
                             NA_character_)) %>% 
    select(prot_id, trial_id, grp_order, everything())
  write_csv(p, path = file.path(prot_dir, sprintf("P%03d.csv",i)))
  
  
  
  
  
}



