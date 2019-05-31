# This script should be run after the data collection. 

data_pth <- here::here("data", "oddoneout", "results_categ") 
out_pth <- here::here("data","oddoneout")

if(!dir.exists(out_pth)) {
  dir.create(out_pth)
}

coltypes <- cols(
  .default = col_character(),
  prot_id = col_integer(),
  trial_id = col_integer(),
  target_position = col_integer(),
  quintile = col_integer(),
  trials.thisRepN = col_integer(),
  trials.thisTrialN = col_integer(),
  trials.thisN = col_integer(),
  trials.thisIndex = col_integer(),
  mouse.x = col_double(),
  mouse.y = col_double(),
  mouse.leftButton = col_integer(),
  mouse.midButton = col_integer(),
  mouse.rightButton = col_integer(),
  protocol_id = col_integer(),
  subject_id = col_integer(),
  frameRate = col_double(),
  `X32` = col_double()
)

df <- data_pth %>% 
  dir(pattern = "*.csv", full.names = T) %>% 
  purrr::map(read_csv, col_types = coltypes) %>% 
  bind_rows() %>%
  select(-X32,-mouse.leftButton, -mouse.rightButton, -mouse.midButton,-expName) %>% 
  rename(prot_id_version = prot_id) %>% 
  mutate(target_image = paste0("image",target_position),
         correct = as.numeric(target_image == mouse.clicked_name),
         category_type = if_else(category %in% c("desert", "waves", "coast", "underwater", "iceberg","field","garden","cavern","beach","canyon","mountainwhite","woods"),"natural","manmade"))

saveRDS(df, file.path(out_pth,"results_190531.rds"))
