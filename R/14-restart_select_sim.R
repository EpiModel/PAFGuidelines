library(EpiModel)
library(data.table)
library(dplyr)

# One or many job_names
job_names <- c("PAF_restart", "kPAF_restart")
jobs <- list()

# Read targets
source("R/utils-targets.R")

needed_trackers <- c(
  "n", "i", "i_dx", "i_sup", "linked1m", "gc_s", "ct_s"
)

needed_pops <- c("ALL", "B", "H", "W")

needed_trackers <- vapply(
  needed_pops,
  function(pop) paste0(needed_trackers, "___", pop),
  needed_trackers
)

needed_cols <- c(
  "sim", "time", "batch", "param_batch", "num",
  "incid.gc", "incid.ct",
  needed_trackers
)

for (job in job_names) {
  jobs[[job]] <- list()
  infos <- readRDS(fs::path("out/remote_jobs/", job, "job_info.rds"))
  jobs[[job]]$infos <- infos
  out_dir <- fs::path(infos$paths$local_job_dir, "out")

  sim_files <- fs::dir_ls(out_dir, regexp = "\\d*.rds")
  df_ls <- vector(mode = "list", length = length(sim_files))

  for (fle in sim_files) {
    btch <- as.numeric(stringr::str_extract(fs::path_file(fle), "\\d+"))
    # # Multi Jobs
    # btch <- paste0(job, btch)
    sim <- readRDS(fle)
    dff <- as.data.table(sim)
    dff[, `:=`(batch = btch)]

    ## Do caclucation here
    dff <- dff[ time > max(time) - 52 ]

    keep_cols <- intersect(needed_cols, names(dff))
    df_ls[[btch]] <- dff[, ..keep_cols]
  }
  jobs[[job]]$data <- rbindlist(df_ls)
}

df_b <- jobs[[1]]$data
# # multi jobs
# df_b <- bind_rows(jobs[[1]]$data, jobs[[2]]$data)

saveRDS(df_b, "out/restart_chooser.rds")

df <- df_b %>%
  group_by(batch, sim) %>%
  summarise(
    ir100.gc = median(incid.gc / (gc_s___B + gc_s___H + gc_s___W) * 5200, na.rm = TRUE),
    ir100.ct = median(incid.ct / (ct_s___B + ct_s___H + ct_s___W) * 5200, na.rm = TRUE),
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
    cc.vsupp___W = cc.vsupp___W - 0.72,
    ir100.gc = (ir100.gc - 12.81) /12.81,
    ir100.ct = (ir100.ct - 14.59) / 14.59
  )

summarise(ungroup(df), across(everything(), median)) %>% as.list()

df_mat <- as.matrix(df[, 3:ncol(df)])
min_ind <- which.min(rowSums(df_mat^2))
as.list(df[min_ind, ]) # check STI values (not in calculation) gc:12.9, ct:15.1
df[min_ind, 1:2]

# Best == batch37 sim7
## multi jobs
# job <- "PAF_restart"
# batch <- 66
# sim_no <- 12
# rs_path <- paste0("out/remote_jobs/", job, "/out/sim", batch, ".rds")
# sim <- readRDS(rs_path)
# orig <- EpiModel::get_sims(sim, sim_no)

rs_path <- paste0("out/remote_jobs/", job, "/out/sim", df[min_ind, 1], ".rds")
sim <- readRDS(rs_path)
orig <- EpiModel::get_sims(sim, df[min_ind, 2])

orig$epi <- orig$epi["num"] # keep only the "num" epi tracker

saveRDS(orig, "out/est/restart.rds")
