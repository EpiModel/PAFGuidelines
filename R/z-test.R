library(EpiModelHIV)
library(tidyverse)
theme_set(theme_light())

scenarios <- c("t1_7001", "t1_7009", "t2_7034", "t3_7059")
scenarios_files <- paste0("out/scenarios/", scenarios, ".rds")
dfs <- lapply(scenarios_files, readRDS)
d <- bind_rows(dfs)

right_roll <- function(x) {
  RcppRoll::roll_mean(x, n = 52, align = "right", fill = NA)
}

# STI Prev
d_roll <- d %>%
  arrange(scenario, batch, sim, time) %>%
  mutate(
    y_prev = i___ALL / s___ALL
  ) %>%
  select(time, scenario, starts_with("y_")) %>%
  pivot_longer(-c(time, scenario)) %>%
  group_by(scenario, name, time) %>%
  summarise(
    y = median(value, na.rm = TRUE),
    yl = quantile(value, prob = 0.25, na.rm = TRUE),
    yh = quantile(value, prob = 0.75, na.rm = TRUE)
  ) %>%
  mutate(across(
      c(yl, y, yh),
      ~ right_roll(.x)
  )) %>%
  filter(time > 52 * 130)


ggplot(d_roll, aes(x = time / 52, y = y, ymin = yl, ymax = yh,
                   col = scenario, fill = scenario)) +
  geom_vline(xintercept = 135 + 1/52, col = "gray") +
  geom_ribbon(alpha = 0.1, linetype = "blank") +
  geom_line() +
  facet_grid(cols = vars(name)) +
  scale_y_continuous(labels = scales::label_percent(0.1)) +
  xlab("Time in Years")

