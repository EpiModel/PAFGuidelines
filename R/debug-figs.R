library(EpiModelHIV)
library(tidyverse)
theme_set(theme_light())

scenarios <- c("t1_7001")
scenarios_files <- paste0("out/scenarios/", scenarios, ".rds")

dfs <- lapply(scenarios_files, readRDS)
d <- bind_rows(dfs)

right_roll <- function(x) {
  RcppRoll::roll_mean(x, n = 52, align = "right", fill = NA)
}

d %>%
  group_by(time) %>%
  summarise(
    ir100.gc = mean(incid.gc / (gc_s_hivpos___ALL + gc_s_hivneg___ALL) * 5200, na.rm = TRUE),
    ir100.ct = mean(incid.ct / (ct_s_hivpos___ALL + ct_s_hivneg___ALL) * 5200, na.rm = TRUE)
  ) %>%
  ggplot(aes(x = time / 52, y = ir100.gc)) +
  geom_line()


# STI Prev
d_roll <- d %>%
  arrange(scenario, batch, sim, time) %>%
  mutate(
    sti_prev_hivpos = (ct_i_hivpos___ALL + gc_i_hivpos___ALL) / n___ALL,
    sti_prev_hivpos = right_roll(sti_prev_hivpos),
    sti_prev_hivneg = (ct_i_hivneg___ALL + gc_i_hivneg___ALL) / n___ALL,
    sti_prev_hivneg = right_roll(sti_prev_hivneg)
  ) %>%
  select(time, scenario, sti_prev_hivneg, sti_prev_hivpos) %>%
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
  xlab("Time in Years") +
  ylab("STI prevalence")

ggsave(
  paste0("out/plots/sti_prev.jpeg"),
  device = "jpeg", dpi = 600,
  height = 5, width = 10,
  units = "in"
)

