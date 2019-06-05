get_dur <- function(log_file) {
  list_lines <- str_split(log_file,pattern ="\n") 
  list_lines <- list_lines[[1]] %>% stringi::stri_remove_empty()
  strttime <- list_lines[1] %>% str_split(pattern = " ", simplify = T)
  
  endtime <- tail(list_lines,1) %>% str_split(pattern = " ", simplify = T)
  
  as.numeric(endtime[1])-as.numeric(strttime[1])
}
