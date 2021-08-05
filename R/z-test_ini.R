source("R/utils-params.R", local = TRUE)

# nsteps <- 52 * 15

# control <- control_msm(
#   nsteps =  nsteps, # one year for prep riskhist then nsteps
#   nsims = 1,
#   ncores = 1,
#   save.nwstats = FALSE,
#   # initialize.FUN = reinit_msm,
#   save.clin.hist = FALSE,
#   verbose = FALSE,
#   raw_output = FALSE
# )

orig <- readRDS("out/est/restart.rds")
nsteps <- 52 * 15
control <- control_msm(
  start = 60 * 52 + 1,
  nsteps = 61 * 52 + 1 + nsteps, # one year for prep riskhist then nsteps
  nsims = 15,
  ncores = 5,
  save.nwstats = FALSE,
  initialize.FUN = reinit_msm,
  save.clin.hist = FALSE,
  verbose = FALSE,
  raw_output = FALSE
)

sim <- netsim(orig, param, init, control)

library(tidyverse)
theme_set(theme_light())

df_b <- as_tibble(sim)

df <- df_b %>%
  filter(time > max(time) - 10 * 52) %>%
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
    cc.vsupp___W = median(i_sup___W / i_dx___W, na.rm = TRUE),
    prep = median(
      (s_prep___B + s_prep___H + s_prep___W) /
      (s_prep_elig___B + s_prep_elig___H + s_prep_elig___W),
      na.rm = TRUE
    )
  )

df_b %>%
  filter(time > 52 * 65) %>%
  mutate(
    prep = (s_prep___B + s_prep___H + s_prep___W) /
           (s_prep_elig___B + s_prep_elig___H + s_prep_elig___W)
  ) %>%
  ggplot(aes(x = time / 52, y = prep)) +
  geom_smooth() +
  scale_y_continuous(breaks = seq(0, 0.2, length.out = 5))
