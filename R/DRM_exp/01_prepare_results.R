# read and merge datafiles

library(tidyverse)
library(here)

data_pth <- here("data/exp_DRM1/results")

fs <- list.files(data_pth, full.names = T, pattern = "*.csv")

df <- fs %>%
  map(~ read_csv(.)) %>% 
  reduce(rbind)

df_resp <- df %>% 
  select(-X31) %>% 
  filter(query == 1) %>% 
  select(subject_id = participant, prot_id:corrKey,key_resp = key_resp_4.keys, correct = key_resp_4.corr, rt_key = key_resp_4.rt,
         confidence = rating.response, confidence.rt = rating.rt, date) %>% 
  mutate(corrKey = recode(corrKey, left = "old", right = "new"),
         key_resp = recode(key_resp, left = "old", right = "new")) %>% 
  filter(!is.na(correct))
  
saveRDS(df_resp, file = here("data/exp_DRM1/results_191017.rds"))
