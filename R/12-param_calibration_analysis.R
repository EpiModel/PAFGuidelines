library(tidyverse)
theme_set(theme_light())

jobs <- readRDS("out/calib_jobs.rds")
df_b <- map_dfr(jobs, ~ as_tibble(.x$data))

param_proposals <- jobs[[1]]$infos$param_proposals

df <- df_b %>%
  mutate(
    gc_batch = vapply(batch, function(x) param_proposals[[x]]$ugc.tprob, 0),
    ct_batch = vapply(batch, function(x) param_proposals[[x]]$uct.tprob, 0)
  )

df %>%
  select(c(time, batch, starts_with("s_prep"))) %>%
  # select(-starts_with("i_sup")) %>%
  group_by(time, batch) %>%
  summarise(across(everything(), ~ mean(.x, na.rm = TRUE))) %>%
  pivot_longer(-c(time, batch)) %>%
  separate(name, sep = "___", into = c("name", "pop")) %>%
  pivot_wider(names_from = name, values_from = value) %>%
  group_by(time, pop) %>%
  summarise(y = median(s_prep / s_prep_elig, na.rm = TRUE)) %>%
  ggplot(aes(x = time / 52, y = y, col = pop)) +
  geom_line()

# gc calib
df %>%
  select(c(time, gc_batch, starts_with("incid.gc"), starts_with("gc_s"))) %>%
  group_by(time, gc_batch) %>%
  summarise(across(everything(), ~ mean(.x, na.rm = TRUE))) %>%
  pivot_longer(-c(time, gc_batch)) %>%
  mutate(name = str_replace(name, ".gc.", ".gc___")) %>%
  separate(name, sep = "___", into = c("name", "pop")) %>%
  pivot_wider(names_from = name, values_from = value) %>%
  group_by(time, gc_batch) %>%
  summarise(y = median(incid.gc / gc_s * 5200, na.rm = TRUE)) %>%
  ggplot(aes(x = time / 52, y = y, col = as.character(gc_batch))) +
  geom_line() +
  labs(x = "Time (Years)", y = "Standardized GC Incidence")

ggsave(
  paste0("out/plots/gc_calib.jpeg"),
  device = "jpeg", dpi = 600,
  height = 5, width = 10,
  units = "in"
)

# ct calib
df %>%
  select(c(time, ct_batch, starts_with("incid.ct"), starts_with("ct_s"))) %>%
  group_by(time, ct_batch) %>%
  summarise(across(everything(), ~ mean(.x, na.rm = TRUE))) %>%
  pivot_longer(-c(time, ct_batch)) %>%
  mutate(name = str_replace(name, ".ct.", ".ct___")) %>%
  separate(name, sep = "___", into = c("name", "pop")) %>%
  pivot_wider(names_from = name, values_from = value) %>%
  group_by(time, ct_batch) %>%
  summarise(y = median(incid.ct / ct_s * 5200, na.rm = TRUE)) %>%
  ggplot(aes(x = time / 52, y = y, col = as.character(ct_batch))) +
  geom_line() +
  labs(x = "Time (Years)", y = "Standardized CT Incidence")

ggsave(
  paste0("out/plots/ct_calib.jpeg"),
  device = "jpeg", dpi = 600,
  height = 5, width = 10,
  units = "in"
)

# ir100 STI
df %>%
  filter(time > max(time) - 10 * 52) %>%
  group_by(param_batch) %>%
  summarise(
    ir100.gc = median(incid.gc / (gc_s___B + gc_s___H + gc_s___W) * 5200),
    ir100.ct = median(incid.ct / (ct_s___B + ct_s___H + ct_s___W) * 5200)
  ) %>%
  mutate(
    ir100.gc = ir100.gc - 12.81,
    ir100.ct = ir100.ct - 14.59
  ) %>%
  print(n = 200)

#GC
lapply(param_proposals[c(11, 12, 19, 20)], \(x) x$ugc.tprob)
#CT
lapply(param_proposals[c(4, 9, 14, 19, 25)], \(x) x$uct.tprob)

