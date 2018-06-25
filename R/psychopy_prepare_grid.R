# simple script to compute positions of the images in psychopy
# we have 3x3 grid
# psychopy has 0,0 in center, scales are following
#   +
# - 0 +
#   -
library(dplyr)
imsize <- 256
separsize <- 30

x <- -c(separsize+imsize,
       0,
       -imsize - separsize)
y <- c(imsize+separsize,
       0,
       -imsize - separsize)

expand.grid(x =x ,y = y) %>% mutate(im = 1:9)

