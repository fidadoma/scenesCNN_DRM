# load figrim data and prepare protocols 

set.seed(190416)

library(tidyverse)

source(here::here("R","utils.R"))

prot_dir <- here::here("data","exp_DRM1","protocols")
stopifnot(dir.exists(prot_dir))

p0 <- read_csv(file.path(prot_dir, "P000.csv"))

nProt <- 20

for (i in 1:nProt) {
  p0 %>% 
    group_by(id_rep) %>% 
    filter(type == "target") %>% 
    sample_frac(1)
  p <- p0 %>% 
    group_by(id_rep) %>% 
    sample_frac(1) %>% 
    ungroup() %>% 
    mutate(trial_id = 1:n()) %>% 
    mutate(prot_id = i) %>% 
    mutate(corr_answer = if_else(type == "target", "left", "right")) %>% 
    select(prot_id, trial_id, everything())
  write_csv(p, file.path(prot_dir, sprintf("P%03d.csv", i)))
}
