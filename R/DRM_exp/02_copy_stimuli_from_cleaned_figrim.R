# copy images to stimuli directory
# Figrim can be downloaded from osf directory
# https://osf.io/sqjbn/

library(tidyverse)
theme_set(theme_classic(16))
library(here)

stimuli_outpth <- here("stimuli_exp1")

if(!dir.exists(stimuli_outpth)) {
  dir.create(stimuli_outpth)
}

stimuli_location <- "d:/Documents/figrim2_clean"

prot_dir <- here("data/exp_DRM1/protocols/")

fs <- list.files(prot_dir, pattern = "*.csv", full.names = T) 
fs <- fs[!stringr::str_detect(fs,"P000")]

df <- sapply(fs, read_csv, simplify=FALSE) %>% bind_rows()

img_names <- df$img_name %>% unique()

tm <- FDhelpers::create.time.measure(length(img_names))

for (i in 1:length(img_names)) {
  file.copy(file.path(stimuli_location, img_names[i]),
            file.path(stimuli_outpth, img_names[i]))
  tm <- FDhelpers::update.tm(tm)
  FDhelpers::print.tm(tm)
}