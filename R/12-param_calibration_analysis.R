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
apply(param_proposals[c(14)], \(x) x$ugc.tprob)
#CT
lapply(param_proposals[c(4, 9, 14, 19, 25)], \(x) x$uct.tprob)

