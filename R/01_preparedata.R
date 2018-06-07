rm(list = ls())


#### LOAD MATRIX FRAGMENTS AND CONSTRUCT THE UPPER TRIANGLE ####
# specify paths and input files
this.dir <- "g:/Dropbox/PsU/GACR_visualmemory/false_memory/scenesDRM/py_output/" # directory with matrix fragments
project_name <- "data" # for naming saved files


d.help <- read.table(paste(c(this.dir, "_help.txt"), collapse = "", sep = ""), stringsAsFactors = F)

d.names <- read.table(paste(c(this.dir, "_img.txt"), collapse = "", sep = ""), stringsAsFactors = F)

if (ncol(d.names) != 1) {
  d.names <- apply(d.names, 1, function(x) paste(c(x), sep = "", collapse = ""))
} else {
  d.names <- as.vector(d.names[, 1])
}

# saved layers
d.help[4:nrow(d.help), ]

# were l2 | corr | top5 computed?
measures_i <- c()
for (i in c(1, gregexpr("_", d.help[3, 1])[[1]][1] + 1)) {
  measures_i <- c(measures_i, ifelse(substr(d.help[3, 1], i, i) == "T", T, F))
}
measures <- c("l2", "corr")[measures_i]
measures

# GLUE THE FRAGMENTS

n_loops <- as.numeric(d.help[2, 1])^2 * length(measures) * length(d.help[4:nrow(d.help), ])
p_counter <- 0.05
loop <- 1
m.data <- list()

for (m in measures) {
  #for (layer in d.help[4:nrow(d.help), ]) {
  for (layer in "fc7") {
    this_m <- rep(0, as.numeric(d.help[2, ]) * as.numeric(d.help[1, ]) + 1)
    for (r in 1:as.numeric(d.help[2, ])) {
      this_row <- NULL
      for (c in 1:as.numeric(d.help[2, ])) {
        if (c >= r) {
          this.f <- read.table(paste(c(this.dir, m, "_", layer, "_", r, "_", c, ".txt"), sep = "", collapse = ""), sep = ",", header = F)
          if (c == 1) {
            this_row <- this.f
          } else {
            this_row <- cbind(this_row, this.f)
          }
        } else {
          this_row <- cbind(this_row, array(0, dim = c(as.numeric(d.help[1, ]), as.numeric(d.help[1, ]))))
        }

        if (loop / n_loops >= p_counter) {
          cat(paste(c(p_counter * 100, "% "), sep = "", collapse = ""))
          p_counter <- p_counter + 0.05
        }
        loop <- loop + 1
      }


      if (r == 1) {
        this_m <- this_row
      } else {
        colnames(this_m) <- c(1:ncol(this_m)) -> colnames(this_row)
        this_m <- rbind(this_m, this_row)
      }
    }
    if (length(d.names) != nrow(this_m)) {
      this_m <- this_m[1:length(d.names), 1:length(d.names)]
    }

    colnames(this_m) <- d.names -> row.names(this_m)
    m.data[paste(c(m, "_", layer), sep = "", collapse = "")] <- list(data.frame(this_m))
  }
}

# FILL LOWER TRIANGLE
lower_triangle <- function(m) {
  m[lower.tri(m)] <- t(m)[lower.tri(m)]
  m
}
m.data <- lapply(m.data, lower_triangle)

# fix the diagonal
for (n in names(m.data)) {
  if (substr(n, 1, 4) == "corr") {
    diag(m.data[[n]]) <- 1
  }
  if (substr(n, 1, 2) == "l2") {
    diag(m.data[[n]]) <- 0
  }
}
m <- m.data$l2_fc7
m2 <- as.matrix(m)

remove_lures <- !(d.names$V1 %in% c("sun_xx1.jpg", "sun_xx2.jpg"))

m3 <- m2[remove_lures,remove_lures]


im_names <- colnames(m3)
im_names <- im_names[remove_lures]
colnames(m3) <- NA
rownames(m3) <- NA


# SAVE THE DATA
m <- m3
save(m,im_names, file = "m_figrim_l2.RData", compress = "xz")

