get_dur <- function(a) {
  aa <- str_split(a,pattern ="\n",simplify = T) 
  strttime <- aa[[1]][1] %>% str_split(pattern = " ", simplify = T)
  
  endtime <- aa[[length(a)]][1] %>% str_split(pattern = " ", simplify = T)
  
  as.numeric(endtime[1])-as.numeric(strttime[1])
}
