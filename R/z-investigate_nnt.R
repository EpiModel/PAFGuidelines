#' # HIV STI NNT diagnostic
#'
#' ## Setup
library(tidyverse)
library(ggExtra)
library(knitr)
library(kableExtra)
library(here)

#' ## Caluclation check
#'
#' load raw data from table 3 scenario runs
d <- readRDS(here("out/tables_data/t3.rds"))

#' baseline test STI is from the first scenario in the table, here 7035
med_test_sti <- d %>%
  filter(scenario == "t3_7035") %>%
  pull(test_sti) %>%
  median()

#' get the data we need and re-calculate nnt_hiv_sti as nnt_v2
#' Here we focus on the second line of table 3
d <- d %>%
  filter(scenario == "t3_7036") %>%
  select(nia, test_sti, nnt_hiv_sti) %>%
  mutate(
    additional_tests = test_sti - med_test_sti,
    nnt_v2 = additional_tests / nia
  )

#' check validity
all.equal(d$nnt_hiv_sti, d$nnt_v2)

#' ## Explore the 3 variables:
#'
#' ### NIA
ggplot(d, aes(x = nia)) + geom_density()
quantile(d$nia, c(0, 0.25, 0.5, 0.75, 1))


#' ### additional_tests
ggplot(d, aes(x = additional_tests)) + geom_density()
quantile(d$additional_tests, c(0, 0.25, 0.5, 0.75, 1))

#' ### nnt_hiv_sti
ggplot(d, aes(x = nnt_hiv_sti)) + geom_density()
quantile(d$nnt_hiv_sti, c(0, 0.25, 0.5, 0.75, 1))

#' zoom on the center region
ggplot(d, aes(x = nnt_hiv_sti)) + geom_density() + xlim(-200, 200)


#' additionally, how many positive nnt_hiv_sti
mean(d$nnt_hiv_sti > 0)

#' ## Cross plots
#'
#' plot nia against additional_tests
plot <- ggplot(d, aes(x = nia, y = additional_tests)) +
  geom_point()
ggMarginal(plot, type = "histogram")

#' plot nia against nnt_hiv_sti
plot <- ggplot(d, aes(x = nia, y = nnt_hiv_sti)) +
  geom_point()
ggMarginal(plot, type = "histogram")

#' plot additional_tests against nnt_hiv_sti
plot <- ggplot(d, aes(x = additional_tests, y = nnt_hiv_sti)) +
  geom_point()
ggMarginal(plot, type = "histogram")

#' ## nnt_hiv_sti calculated using the medians
#'
#' one line per scenario with the median value among scenarios
#' nnt_median is additional_tests / nia using the medians


readRDS(here("out/tables_data/t3.rds")) %>%
  select(scenario, nia, test_sti, nnt_hiv_sti) %>%
  mutate(
    additional_tests = test_sti - med_test_sti
  ) %>%
  group_by(scenario) %>%
  summarize(
    nnt_gt0 = mean(nnt_hiv_sti > 0),
    nia_gt0 = mean(nia > 0),
    across(everything(), median)
    ) %>%
  mutate(
    nnt_median = additional_tests / nia,
    nnt_gt0 = scales::label_percent(accuracy = 0.1)(nnt_gt0),
    nia_gt0 = scales::label_percent(accuracy = 0.1)(nia_gt0)
  ) %>%
  select(
    scenario,
    test_sti,
    additional_tests,
    nia,
    nia_gt0,
    nnt_hiv_sti,
    nnt_median,
    nnt_gt0
  ) %>%
  kable() %>%
  kable_styling(bootstrap_options = c("striped", "bordered"))

# rmarkdown::render("R/z-investigate_nnt.R")
