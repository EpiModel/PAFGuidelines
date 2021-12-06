source("R/utils-targets.R")

library(tidyverse)
theme_set(theme_light())

jobs <- readRDS("out/calib_jobs.rds")
df_b <- map_dfr(jobs, ~ as_tibble(.x$data))

param_proposals <- jobs[[1]]$infos$param_proposals

df <- df_b

df <- df_b %>%
  filter(time > max(time) - 30 * 52) %>%
  group_by(param_batch) %>%
  summarise(
    num = median(num),
    gc_s = median((gc_s___B + gc_s___H + gc_s___W), na.rm = TRUE),
    ct_s = median((ct_s___B + ct_s___H + ct_s___W), na.rm = TRUE),
    gc_i = median(num - (gc_s___B + gc_s___H + gc_s___W), na.rm = TRUE),
    ct_i = median(num - (ct_s___B + ct_s___H + ct_s___W), na.rm = TRUE),
    ir100.gc = median(incid.gc / (gc_s___B + gc_s___H + gc_s___W) * 5200, na.rm = TRUE),
    ir100.ct = median(incid.ct / (ct_s___B + ct_s___H + ct_s___W) * 5200, na.rm = TRUE),
    incid.gc = median(incid.gc, na.rm = TRUE),
    incid.ct = median(incid.ct, na.rm = TRUE),
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
    # prep = median(
    #   (s_prep___B + s_prep___H + s_prep___W) /
    #   (s_prep_elig___B + s_prep_elig___H + s_prep_elig___W),
    #   na.rm = TRUE
    # )
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
    cc.vsupp___W = cc.vsupp___W - 0.72,
    ir100.gc = ir100.gc - 12.81,
    ir100.ct = ir100.ct - 14.59
    # prep = prep - 0.15
  )

df %>%
  select(param_batch, gc_s, ct_s, incid.gc, incid.ct, ir100.gc, ir100.ct)

df %>%
  select(param_batch, starts_with("i.prev.dx"))

good_params <- c(3, 5)

param_proposals[good_params]

d <- df_b %>%
  add_targets() %>%
  group_by(param_batch, time) %>%
  summarise(across(
    c(ir100.ct, ir100.gc),
    list(
      q1 = ~ quantile(.x, probs = 0.25, na.rm = TRUE),
      q2 = ~ quantile(.x, probs = 0.50, na.rm = TRUE),
      q3 = ~ quantile(.x, probs = 0.75, na.rm = TRUE)
    )
  )) %>%
  pivot_longer(-c(param_batch, time)) %>%
  separate(name, c("measure", "pos"), sep = "_") %>%
  pivot_wider(names_from = pos, values_from = value)

d %>%
  filter(
    time > 1000,
    param_batch == 1
  ) %>%
  ggplot(aes(
    x = time / 52, y = q2, ymin = q1, ymax = q3,
    col = measure, fill = measure
  )) +
  geom_line() +
  geom_ribbon(alpha = 0.3, size = 0) +
  geom_hline(yintercept = 12.81) +
  geom_hline(yintercept = 14.59) +
  facet_wrap(~ param_batch)


df_b %>%
  select(time, param_batch, incid.ct, incid.gc) %>%
  filter(param_batch %in% good_params) %>%
  pivot_longer(-c(time, param_batch)) %>%
  group_by(param_batch, name) %>%
  arrange(time) %>%
  mutate(value = RcppRoll::roll_meanr(value, n = 13)) %>%
  ggplot(aes(x = time, y = value, col = as.character(param_batch))) +
  geom_line() +
  facet_wrap(~name)

ggsave("out/sticalib_long.png")

df_b %>%
  filter(
    param_batch == 5
  ) %>%
  ggplot(aes(
    x = time,
    # col = as.factor(sim)
    y = incid.ct / (ct_s___B + ct_s___H + ct_s___W) * 5200
    # y = incid.gc / (gc_s___B + gc_s___H + gc_s___W) * 5200
  )) +
  geom_smooth(alpha = 0.3) +
  geom_hline(yintercept = 12.81) +
  geom_hline(yintercept = 14.59)

df %>% filter(param_batch == 1) %>% as.list()

df %>%
  group_by(param_batch) %>%
  select(starts_with("i.prev")) %>%
  summarise(across(everything(), median)) %>%
  mutate(score = i.prev.dx___B^2 + i.prev.dx___H^2 + i.prev.dx___W^2) %>%
  arrange(i.prev.dx___B) %>%
  print(n = 200)

df %>%
  group_by(param_batch) %>%
  select(starts_with("prep")) %>%
  summarise(across(everything(), median)) %>%
  print(n = 200)

param_proposals[c(66, 41)]

df %>%
  ungroup() %>%
  select(starts_with("cc.vsupp")) %>%
  summarise(across(everything(), median)) %>%
  print(n = 200)

df %>%
  ungroup() %>%
  select(starts_with("ir100")) %>%
  summarise(across(everything(), list(
        q1 = ~ quantile(.x, prob = 0.25, na.rm = TRUE),
        q2 = ~ quantile(.x, prob = 0.50, na.rm = TRUE),
        q3 = ~ quantile(.x, prob = 0.75, na.rm = TRUE)
  ))) %>%
  as.list()

