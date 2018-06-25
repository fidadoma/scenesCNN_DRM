# simple script to compute positions of the images in psychopy
# we have 3x3 grid
# psychopy has 0,0 in center, scales are following
#   +
# - 0 +
#   -
imsize <- 256
separsize <- 30

x <- -c(separsize+imsize,
       imsize/2,
       imsize/2 - separsize)
y <- c(imsize/2+separsize+imsize,
       imsize/2,
       imsize/2 + separsize)

expand.grid(x =x ,y = y) %>% mutate(im = 1:9)

