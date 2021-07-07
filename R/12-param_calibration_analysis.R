library(tidyverse)
theme_set(theme_light())

jobs <- readRDS("out/calib_jobs.rds")
df_b <- map_dfr(jobs, ~ as_tibble(.x$data))

param_proposals <- jobs[[1]]$infos$param_proposals

df <- df_b

df <- df_b %>%
  filter(time > max(time) - 10 * 52) %>%
  group_by(param_batch) %>%
  summarise(
    ir100.gc = median(ir100.gc, na.rm = TRUE),
    ir100.ct = median(ir100.ct, na.rm = TRUE),
    i.prev.dx___B = median(i_dx___B / n___B, na.rm = TRUE),
    cc.dx___B = median(i_dx___B / i___B, na.rm = TRUE),
    cc.linked1m___B = median(linked1m___B / i_dx___B, na.rm = TRUE),
    cc.vsupp___B = median(i_sup___B / i_dx___B, na.rm = TRUE),
    i.prev.dx___H = median(i_dx___H / n___H, na.rm = TRUE),
    cc.dx___H = median(i_dx___H / i___H, na.rm = TRUE),
    cc.linked1m___H = median(linked1m___H / i_dx___H, na.rm = TRUE),
    cc.vsupp___H = median(i_sup___H / i_dx___H, na.rm = TRUE),
    i.prev.dx___W = median(i_dx___W / n___W, na.rm = TRUE),
    cc.dx___W = median(i_dx___W / i___W, na.rm = TRUE),
    cc.linked1m___W = median(linked1m___W / i_dx___W, na.rm = TRUE),
    cc.vsupp___W = median(i_sup___W / i_dx___W, na.rm = TRUE)
  ) %>%
  mutate(
    i.prev.dx___B = i.prev.dx___B - 0.33,
    i.prev.dx___H = i.prev.dx___H - 0.127,
    i.prev.dx___W = i.prev.dx___W - 0.084,
    cc.dx___B = cc.dx___B - 0.804,
    cc.dx___H = cc.dx___H - 0.799,
    cc.dx___W = cc.dx___W - 0.88,
    cc.linked1m___B = cc.linked1m___B - 0.62,
    cc.linked1m___H = cc.linked1m___H - 0.65,
    cc.linked1m___W = cc.linked1m___W - 0.76,
    cc.vsupp___B = cc.vsupp___B - 0.55,
    cc.vsupp___H = cc.vsupp___H - 0.60,
    cc.vsupp___W = cc.vsupp___W - 0.72
  )

df %>%
  group_by(param_batch) %>%
  select(starts_with("i.prev")) %>%
  summarise(across(everything(), median))

param_proposals[c(17, 18)]


df %>%
  ungroup() %>%
  select(starts_with("cc.linked1m")) %>%
  summarise(across(everything(), list(
        q1 = ~ quantile(.x, prob = 0.25, na.rm = TRUE),
        q2 = ~ quantile(.x, prob = 0.50, na.rm = TRUE),
        q3 = ~ quantile(.x, prob = 0.75, na.rm = TRUE)
  ))) %>%
  as.list()

