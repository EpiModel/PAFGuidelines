library(data.table)
library(purrr)
library(dplyr)
library(tidyr)
library(EpiModel)

# One or many job_names
job <- "PAF_calib_scale1"

# Read targets
source("R/utils-targets.R")

infos <- readRDS(fs::path("out/remote_jobs/", job, "job_info.rds"))
out_dir <- fs::path(infos$paths$local_job_dir, "out")

sim_files <- fs::dir_ls(out_dir, regexp = "\\d*.rds")
df_ls <- vector(mode = "list", length = length(sim_files))

for (fle in sim_files) {
  btch <- as.numeric(stringr::str_extract(fs::path_file(fle), "\\d+"))
  sim <- readRDS(fle)
  dff <- as_tibble(sim)
  dff <- dff %>%
    filter(time < max(time) - 52) %>%
    mutate(,
      batch = btch,
      param_batch = infos$unique_proposals[btch]
    ) %>%
    group_by(batch, param_batch, sim) %>%
    summarize(
      i.prev.dx___B = median(i_dx___B / n___B, na.rm = TRUE),
      i.prev.dx___W = median(i_dx___W / n___W, na.rm = TRUE),
      i.prev.dx___H = median(i_dx___H / n___H, na.rm = TRUE)
    ) %>%
    ungroup()

  df_ls[[btch]] <- dff
}

df <- bind_rows(df_ls)

tresh <- 1
df %>%
  group_by(param_batch) %>%
  summarize(across(starts_with("i.prev"), median)) %>%
  mutate(
    i.prev.dx___B = i.prev.dx___B / 0.33 - 1,
    i.prev.dx___H = i.prev.dx___H / 0.127 - 1,
    i.prev.dx___W = i.prev.dx___W / 0.084 - 1,
    score = i.prev.dx___B ^ 2 + i.prev.dx___W ^ 2 + i.prev.dx___H ^ 2 ,
    ascore = abs(i.prev.dx___B) + abs(i.prev.dx___W) + abs(i.prev.dx___H)
  ) %>%
  filter(
    abs(i.prev.dx___B) < tresh,
    abs(i.prev.dx___H) < tresh,
    abs(i.prev.dx___W) < tresh
  ) %>%
  arrange(i.prev.dx___W)



saveRDS(jobs, "out/calib_jobs.rds")
