
## Average accuracy per image

```{r}
p <- df %>% group_by(type,img_name) %>% 
  summarize(correct = mean(correct, na.rm = T), dist_to_proto = mean(dist_to_proto)) %>% 
  ggplot(aes(x = dist_to_proto, y = correct, col = type)) + 
  geom_point(alpha = 0.2) + stat_smooth(method = "lm") +
  theme(aspect.ratio = 1) + 
  xlab("Distance to visual prototype") + 
  ylab("Accuracy")
p
ggsave(file.path(plots_dir, "FXXScatter.svg"), p, width = 6, height = 6)

```

## Correlation of distance from prototype with accuracy 

As we have for each test scene ditance to prototype and accuracy, we can compute correlation between those variables. I am not sure, whether we can aggregate somehow to get averaged accuracies?
  
  Neverethless, we computed correlations for all scenes, followed by correlation for each type separately. We also show one figure with correlations for each layer.

```{r Correlation of distance from prototype with accuracy }
f <- function(dfx) {
  dfx %>% group_by(img_name) %>% summarize(correct = mean(correct), dist_to_proto = mean(dist_to_proto),dist_to_proto_conv3 = mean(dist_to_proto_conv3),
                                           dist_to_proto_conv5 = mean(dist_to_proto_conv5))  
}
f2 <- function(dfx) {
  dfx %>% group_by(type,img_name) %>% summarize(correct = mean(correct), dist_to_proto = mean(dist_to_proto),dist_to_proto_conv3 = mean(dist_to_proto_conv3),
                                                dist_to_proto_conv5 = mean(dist_to_proto_conv5))  
}

rbind( 
  cor.test(~dist_to_proto+correct, df %>% f()) %>% broom::tidy() %>% mutate(layer="fc7", type = "all"),
  cor.test(~dist_to_proto+correct, df %>% filter(type == "far distractor") %>% f()) %>% broom::tidy() %>% mutate(layer="fc7", type = "far distractor"),
  cor.test(~dist_to_proto+correct, df %>% filter(type == "close distractor") %>% f()) %>% broom::tidy() %>% mutate(layer="fc7", type = "close distractor"),
  cor.test(~dist_to_proto+correct, df %>% filter(type == "target") %>% f()) %>% broom::tidy() %>% mutate(layer="fc7", type = "all", type = "target"),
  cor.test(~dist_to_proto_conv5+correct, df %>% f()) %>% broom::tidy() %>% mutate(layer="conv5", type = "all"),
  cor.test(~dist_to_proto_conv5+correct, df %>% filter(type == "far distractor") %>% f()) %>% broom::tidy() %>% mutate(layer="conv5", type = "far distractor"),
  cor.test(~dist_to_proto_conv5+correct, df %>% filter(type == "close distractor") %>% f()) %>% broom::tidy() %>% mutate(layer="conv5", type = "close distractor"),
  cor.test(~dist_to_proto_conv5+correct, df %>% filter(type == "target") %>% f()) %>% broom::tidy() %>% mutate(layer="conv5", type = "all", type = "target"),
  cor.test(~dist_to_proto_conv3+correct, df %>% f()) %>% broom::tidy() %>% mutate(layer="conv3", type = "all"),
  cor.test(~dist_to_proto_conv3+correct, df %>% filter(type == "far distractor") %>% f()) %>% broom::tidy() %>% mutate(layer="conv3", type = "far distractor"),
  cor.test(~dist_to_proto_conv3+correct, df %>% filter(type == "close distractor") %>% f()) %>% broom::tidy() %>% mutate(layer="conv3", type = "close distractor"),
  cor.test(~dist_to_proto_conv3+correct, df %>% filter(type == "target") %>% f()) %>% broom::tidy() %>% mutate(layer="conv3", type = "all", type = "target")) %>% 
  select(layer, type, estimate, conf.low, conf.high, p.value) %>% 
  knitr::kable(digits = 3, caption ="correlation between accuracy and distance to prototype")

df1 <- df %>% f2() %>% pivot_longer(cols = starts_with("dist_to_proto"),names_to = "layer", values_to = "distance_to_proto")

df1_gr <- df1 %>% 
  group_by(layer, type) %>% 
  do(cor.test(.$distance_to_proto, .$correct) %>% broom::tidy()) %>% 
  ungroup() %>% 
  mutate(layer = recode(layer, dist_to_proto = "fc7", dist_to_proto_conv3 = "conv3", dist_to_proto_conv5 = "conv5")) %>% 
  mutate(layer = factor(layer, levels = c("conv3", "conv5", "fc7"))) 
df1_all <- df1 %>% 
  group_by(layer) %>% 
  do(cor.test(.$distance_to_proto, .$correct) %>% broom::tidy()) %>% 
  ungroup() %>% 
  mutate(layer = recode(layer, dist_to_proto = "fc7", dist_to_proto_conv3 = "conv3", dist_to_proto_conv5 = "conv5")) %>% 
  mutate(layer = factor(layer, levels = c("conv3", "conv5", "fc7"))) %>% 
  mutate(type = "all")
pd <- position_jitter(0.1, seed = 123)
p <- df1_gr %>% 
  arrange(type,layer) %>% 
  ggplot(aes(x = layer, y = estimate, col = type, group = type)) + 
  geom_point(position =  pd)+#position =  position_jitter(width = 0.1, seed = 123)) + 
  geom_line(position =  pd)+#position = position_jitter(width = 0.1, seed = 123)) + 
  geom_linerange(aes(ymin = conf.low, ymax = conf.high),position =  pd)+
  ylab("correlation") +
  ggtitle("Correlation between accuracy and distance to prototype")+
  geom_point(data = df1_all %>% arrange(type,layer), aes(x = layer, y = estimate, group = 1), col = "black") + 
  geom_line(data = df1_all %>% arrange(type,layer), aes(x = layer, y = estimate, group = 1), col = "black") + 
  geom_linerange(data = df1_all %>% arrange(type,layer), aes(ymin = conf.low, ymax = conf.high), col = "black") +
  theme(aspect.ratio = 1)
ggsave(file.path(plots_dir, "FigureXX_layers_correlation.svg"), p, width = 6, height = 6)
```

Figure shows, that correlation is about the same for different layers with exception of far distracators, which decreases with higher layers. Black is for all data

### Distances from tgt images - fc7

## Average distance to target

We can compute distance to targets for each test scene. From these values, we can compute average. Because each layer has different ranges, we normalize the distances by dividint them by distance to the target. Lower values than 1 means, that given scene type is closer to targets than targets themselves (e.g. their are more in center).

```{r}

df2_tgt_agg1 <- df2_tgt %>% 
  mutate(layer = recode(layer, dist_to_tgts = "fc7", dist_to_tgts_conv3 = "conv3", dist_to_tgts_conv5 = "conv5")) %>% 
  mutate(layer = factor(layer, levels = c("conv3", "conv5", "fc7"))) 

df2_tgt_agg1 %>% 
  ungroup() %>% 
  filter(layer == "fc7") %>% 
  group_by(img_name,type) %>% 
  summarize(correct = mean(correct),  min_tgt_dist_no0 = mean(min_tgt_dist_no0)) %>% 
  ggplot(aes(x = min_tgt_dist_no0, y = correct, col = type)) +
  geom_point(alpha = 0.2) + stat_smooth(method = "lm") +
  theme(aspect.ratio = 1) + 
  xlab("Minimum distance to target (0 exlucded)") + 
  ylab("Accuracy") 

df2_tgt_agg1 %>% 
  ungroup() %>% 
  filter(layer == "conv5") %>% 
  group_by(img_name,type) %>% 
  summarize(correct = mean(correct),  min_tgt_dist_no0 = mean(min_tgt_dist_no0), dist_to_proto = mean(dist_to_proto)) %>% 
  ggplot(aes(x = min_tgt_dist_no0, y = correct, col = type)) +
  geom_point(alpha = 0.2) + stat_smooth(method = "lm") +
  theme(aspect.ratio = 1) + 
  xlab("Minimum distance to target (0 exlucded)") + 
  ylab("Accuracy") 

df2_tgt_agg1 %>% 
  ungroup() %>% 
  filter(layer == "fc7") %>% 
  group_by(img_name) %>% 
  summarize(correct = mean(correct),  min_tgt_dist_no0 = mean(min_tgt_dist_no0), dist_to_proto = mean(dist_to_proto)) %>% 
  cor.test(~min_tgt_dist_no0+dist_to_proto,.)

df2_tgt_agg1 %>% 
  ungroup() %>% 
  filter(layer == "fc7") %>% 
  group_by(img_name) %>% 
  summarize(correct = mean(correct),  min_tgt_dist_no0 = mean(min_tgt_dist_no0), dist_to_proto = mean(dist_to_proto)) %>% 
  cor.test(~correct+dist_to_proto,.)
df2_tgt_agg1 %>% 
  ungroup() %>% 
  filter(layer == "fc7") %>% 
  group_by(img_name,type) %>% 
  summarize(correct = mean(correct),  min_tgt_dist_no0 = mean(min_tgt_dist_no0), dist_to_proto = mean(dist_to_proto)) %>% 
  group_by(type) %>% 
  do(cor.test(~correct+dist_to_proto,.) %>% broom::tidy())


p <- df2_tgt_agg1 %>% 
  ungroup() %>% 
  filter(layer == "fc7") %>% 
  group_by(img_name,type) %>% 
  summarize(correct = mean(correct),  min_tgt_dist_no0 = mean(min_tgt_dist_no0), dist_to_proto = mean(dist_to_proto)) %>%
  ggplot(aes(x = dist_to_proto, y = min_tgt_dist_no0, col = type)) + 
  geom_point() + 
  theme(aspect.ratio = 1) +
  xlab("Distance to visual prototype") + 
  ylab("Minimum distance to target (0 exlucded)")
p
ggsave(file.path(plots_dir, "FigureXXDistances_right.svg"), p, width = 6, height = 6)



```

## Minimum distance to target

Alternativelly, we can compute minimum distance of presented scene to one of the targets. We are removing zero distances, as in case of target queries, this minimum would be 0.

```{r Min distance to target}
df2_tgt_agg2_ci <- df2_tgt_agg1 %>% 
  group_by(layer,type) %>% 
  summarize(m=Hmisc::smean.cl.boot(min_tgt_dist_no0)[1],lower =Hmisc::smean.cl.boot(min_tgt_dist_no0)[2], upper = Hmisc::smean.cl.boot(min_tgt_dist_no0)[3])

df2_tgt_agg2_ci %>% 
  group_by(layer) %>% 
  mutate(m = m/m[type=="target"]) %>% 
  ggplot(aes(x=layer, y=m,col = type, group = type)) + 
  geom_point()+geom_line() +
  ggtitle("Minimum distance to targets (identities excluded)")

```

Figure shows that types of images get closer to each other in lower layers.


```{r select examples}

set.seed(19763) 
df_names <- df %>% filter(subject_id == 1, grp_order == 3) %>% group_by(type) %>% sample_n(5)

df %>% filter(img_name %in% df_names$img_name) %>% 
  group_by(type) %>% 
  summarize(correct = mean(correct))
```

## Compare to category center

```{r Compare to category center}
cr <- df %>% rename(dist_to_proto_fc7 = dist_to_proto) %>% select(starts_with("dist_to_proto"),starts_with("dist_to_cat_center"), correct, img_name) %>% 
  group_by(img_name) %>% 
  summarize_each(mean) %>% 
  ungroup() %>% 
  select(-img_name) %>% 
  as.matrix() %>% 
  Hmisc::rcorr()

cr$r

cr_close <- df %>% rename(dist_to_proto_fc7 = dist_to_proto) %>% 
  filter(type == "close distractor") %>% 
  select(starts_with("dist_to_proto"),starts_with("dist_to_cat_center"), correct, img_name) %>% 
  group_by(img_name) %>% 
  summarize_each(mean) %>% 
  ungroup() %>% 
  select(-img_name) %>% 
  as.matrix() %>% 
  Hmisc::rcorr()

cr_close$r %>% knitr::kable(digits = 2)

cr_targets <- df %>% rename(dist_to_proto_fc7 = dist_to_proto) %>% 
  filter(type == "target") %>% 
  select(starts_with("dist_to_proto"),starts_with("dist_to_cat_center"), correct, img_name) %>% 
  group_by(img_name) %>% 
  summarize_each(mean) %>% 
  ungroup() %>% 
  select(-img_name) %>% 
  as.matrix() %>% 
  Hmisc::rcorr()

cr_targets$r %>% knitr::kable(digits = 2)

p <- df %>% group_by(type,img_name) %>% 
  summarize(correct = mean(correct, na.rm = T), dist_to_cat_center_fc7 = mean(dist_to_cat_center_fc7)) %>% 
  ggplot(aes(x = dist_to_cat_center_fc7, y = correct, col = type)) + 
  geom_point(alpha = 0.2) + stat_smooth(method = "lm") +
  theme(aspect.ratio = 1) + 
  xlab("Distance to visual prototype") + 
  ylab("Accuracy")
p
```

## Model Distance to targets 


```{r}
df$dist_to_tgts <- purrr::map2(df$dist_to_tgts, df$avg_dist_between_tgts, ~.x/.y) # normalize distances



dist_vals_targets <- df %>% 
  filter(type == "target") %>% 
  pull(dist_to_tgts) %>% 
  unlist() 

dist_vals_close_dist <- df %>% 
  filter(type == "close distractor") %>% 
  pull(dist_to_tgts) %>% 
  unlist() 

dist_vals_far_dist <- df %>% 
  filter(type == "far distractor") %>% 
  pull(dist_to_tgts) %>% 
  unlist() 

df_dist_vals <- tibble(type = c(rep("target", length(dist_vals_targets)),
                                rep("close distractor", length(dist_vals_close_dist)),
                                rep("far distractor", length(dist_vals_far_dist))),
                       value = c(dist_vals_targets, dist_vals_close_dist, dist_vals_far_dist)) %>% 
  filter(value > 0)

df_dist_vals %>% 
  ggplot(aes(x = value, col = type)) + geom_freqpoly()

df$model_dist_tgt_02 <- purrr::map_dbl(df$dist_to_tgts, ~any(.x<0.2)) # normalize distances
df$model_dist_tgt_03 <- purrr::map_dbl(df$dist_to_tgts, ~any(.x<0.3)) # normalize distances
df$model_dist_tgt_04 <- purrr::map_dbl(df$dist_to_tgts, ~any(.x<0.4)) # normalize distances
df$model_dist_tgt_05 <- purrr::map_dbl(df$dist_to_tgts, ~any(.x<0.5)) # normalize distances
df$model_dist_tgt_06 <- purrr::map_dbl(df$dist_to_tgts, ~any(.x<0.6)) # normalize distances
df$model_dist_tgt_07 <- purrr::map_dbl(df$dist_to_tgts, ~any(.x<0.7)) # normalize distances
df$model_dist_tgt_08 <- purrr::map_dbl(df$dist_to_tgts, ~any(.x<0.8)) # normalize distances
df$model_dist_tgt_09 <- purrr::map_dbl(df$dist_to_tgts, ~any(.x<0.9)) # normalize distances
df$model_dist_tgt_1 <- purrr::map_dbl(df$dist_to_tgts, ~any(.x<1)) # normalize distances
df$model_dist_tgt_12 <- purrr::map_dbl(df$dist_to_tgts, ~any(.x<1.2)) # normalize distances


dfx <- df %>% 
  pivot_longer(cols = starts_with("model_dist_tgt"), names_to = "model", values_to = "accuracy") %>% 
  mutate(accuracy = if_else(type == "target",accuracy, 1-accuracy)) 

dfx %>% 
  ggplot(aes(x = type, y = accuracy, col = model, group = model)) + 
  stat_summary(fun.data = "mean_cl_boot") + 
  stat_summary(fun.y = "mean", geom = "line") 

dfx %>% 
  group_by(img_name, model, type) %>% 
  summarize(correct = mean(correct), accuracy = mean(accuracy)) %>% 
  mutate(accuracy = jitter(accuracy)) %>% 
  group_by(model, type) %>% 
  do(cor.test(~accuracy+correct,.) %>% broom::tidy()) %>% 
  filter(type == "close distractor")

```

## Divide distractors by distance to targets - min no0

```{r}
divide_into_groups_tgt_dist <- function(dfxx) {
  
  dfxx_notgt <- dfxx %>% filter(type != "target") %>% mutate(model_type = NA)
  
  rnk <- rank(dfxx_notgt$min_tgt_dist)
  
  dfxx_notgt$model_type[rnk <= 5] <- "close_tgt"
  dfxx_notgt$model_type[rnk > 5]  <- "far_tgt"
  
  dfxx %>% 
    left_join(dfxx_notgt %>% select(img_name,model_type), by="img_name") %>% 
    mutate(model_type = if_else(type == "target","target",model_type))
}

df2_tgt_modeldist %>% xtabs(~type+model_type,.) %>% prop.table(1)
df2_tgt_modeldist %>% filter(type!="target") %>% xtabs(~type+model_type,.) %>% psych::phi()
df2_tgt_modeldist %>% filter(type != "target") %>% 
  group_by(model_type) %>% 
  summarize(correct = mean(correct))

df2_tgt_modeldist %>% filter(type != "target") %>% 
  group_by(type) %>% 
  summarize(correct = mean(correct))

```

## Divide distractors by memorability

```{r}

df2_modelmem %>% xtabs(~type+model_type,.) %>% prop.table()
df2_modelmem %>% filter(type!="target") %>% xtabs(~type+model_type,.) %>% psych::phi()

df2_modelmem %>% filter(type != "target") %>% 
  group_by(model_type) %>% 
  summarize(correct = mean(correct))


```

## Divide distractors by fc7

```{r}

df2_modeldisttoproto <- df %>% 
  group_by(subject_id,grp_order) %>% 
  do(divide_into_groups(., "dist_to_proto",lower_grp="close_to_prototype",higher_grp="far_to_prototype"))

df2_modeldisttoproto %>% xtabs(~type+model_type,.)
df2_modeldisttoproto %>% filter(type!="target") %>% xtabs(~type+model_type,.) %>% psych::phi()

df2_modeldisttoproto %>% filter(type != "target") %>% 
  group_by(model_type) %>% 
  summarize(correct = mean(correct))


```


```{r}
df2_modeldisttoproto3 %>% filter(type!="target") %>% xtabs(~type+model_type,.) %>% prop.table(1)

df2_modeldisttoproto3 %>% filter(type != "target") %>% 
  group_by(model_type) %>% 
  summarize(correct = mean(correct))

df2_modeldisttoproto3 %>% filter(type != "target") %>% 
  group_by(type) %>% 
  summarize(correct = mean(correct))

```

## Divide distractors by conv5

```{r}

df2_modeldisttoproto5 <- df %>% 
  group_by(subject_id,grp_order) %>% 
  do(divide_into_groups(.,"dist_to_proto_conv5","close_tgt","far_tgt"))

df2_modeldisttoproto5 %>% filter(type!="target") %>% xtabs(~type+model_type,.) %>% prop.table(1)
df2_modeldisttoproto5 %>% filter(type!="target") %>% xtabs(~type+model_type,.) %>% psych::phi()

df2_modeldisttoproto5 %>% filter(type != "target") %>% 
  group_by(model_type) %>% 
  summarize(correct = mean(correct))

df2_tgt_modeldist %>% filter(type != "target") %>% 
  group_by(type) %>% 
  summarize(correct = mean(correct))

```


## Context entropy

```{r}

```

## FAbility



## Hits and FAs



## How many scenes have ground-truth memorability score

```{r}
df_perimage <- df %>% group_by(img_name,type) %>% summarize(m = mean(correct), sd = sd(correct)) %>% arrange(-m)
mem_imgnames <- read_csv(here("data/exp_DRM1/memorability/memorability_imgnames.txt"), col_names = F)
mem_values <- read_csv(here("data/exp_DRM1/memorability/memorability_all.txt"), col_names = F)

df_mem <- tibble(img_name = mem_imgnames$X1, mem_score = mem_values$X1)
dfm <- df %>% left_join(df_mem,by = "img_name") 

df_memorability_ground_truth <- read_csv(here("data/exp_DRM1/memorability/figrim_memorability_within_category.csv"))
dfm <- dfm %>% left_join(df_memorability_ground_truth, by = c("img_name"="filename"))
dfm1 <- dfm %>% 
  group_by(type,img_name) %>% 
  mutate( HR = hits/(hits+misses),
          FR = false_alarms/(false_alarms+correct_rejections)) %>% 
  summarize(hits = mean(hits), fa = mean(false_alarms),HR = mean(HR), FR = mean(FR), correct = mean(correct),mem_score = mean(mem_score)) %>% 
  ungroup() 
dfm1 %>% 
  group_by(type) %>% 
  summarize(n_perc = sum(!is.na(hits))/n(), n=sum(!is.na(hits)))
dfm1 %>% 
  group_by(type) %>% 
  do(cor.test(~HR+correct,.) %>% broom::tidy())
dfm1 %>% 
  group_by(type) %>% 
  mutate(correct = 1-correct) %>% 
  do(cor.test(~FR+correct,.) %>% broom::tidy())

dfm1 %>% 
  group_by(type) %>% 
  do(cor.test(~mem_score+FR,.) %>% broom::tidy())

```

