# power analysis

library(tidyverse)

# we have 21 categories
# for each category, we 3 lists
# each list have 15 test items

# simulate data
create_simulated_dataframe <- function(prot_id = 1, subject_id = 1, mean_accuracy, grp_ids = 1:3, category_ids = 1:21, img_per_type = 5) {
  type <- c("old","closer","fahrer")
  
  if(length(mean_accuracy)==1) {
    mean_accuracy <- rep(mean_accuracy,3)
  }
  
  df1 <- expand.grid(grp_id = grp_ids, category_id = category_ids, type = type, img_per_type = 1:img_per_type)
  
  corr_old <- rbinom(n = nrow(df1) / 3, size = 1, prob = mean_accuracy[1])
  corr_closer <- rbinom(n = nrow(df1) / 3, size = 1, prob = mean_accuracy[2])
  corr_fahrer <- rbinom(n = nrow(df1) / 3, size = 1, prob = mean_accuracy[3])
  df1$accuracy[df1$type == "old"] <- corr_old
  df1$accuracy[df1$type == "closer"] <- corr_closer
  df1$accuracy[df1$type == "fahrer"] <- corr_fahrer
  df1
}

df1
for (i in 1:25) {
  df1 <- create_simulated_dataframe(mean_accuracy = c(.8,.6,.4))  
}



library(lme4)

lm1 <- glmer(cbind(accuracy,1-accuracy)~type + (1|category_id), df1, family = binomial)
lmnull <- glmer(cbind(accuracy,1-accuracy)~1 + (1|category_id), df1, family = binomial)

anova(lm1,lmnull)
