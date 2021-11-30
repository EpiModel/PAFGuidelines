targets <- c(
  # 1st calibration set (all independant)
  cc.dx.B = 0.804,
  cc.dx.H = 0.799,
  cc.dx.W = 0.88,
  cc.linked1m.B = 0.62,
  cc.linked1m.H = 0.65,
  cc.linked1m.W = 0.76,
  # CombPrev appendix 8.2.2
  # 2nd calibration set (all independant)
  cc.vsupp.B = 0.55,
  cc.vsupp.H = 0.60,
  cc.vsupp.W = 0.72,
  # STIs
  ir100.gc = 12.81,
  ir100.ct = 14.59,
  # 3rd calibration set
  i.prev.dx.B = 0.33,
  i.prev.dx.H = 0.127,
  i.prev.dx.W = 0.084,
  prep_prop = 0.15
)

calculate_targets <- function(d, n_steps = 52) {
  d %>%
    filter(time > max(time) - n_steps) %>%
    group_by(sim) %>%
    summarise(
      ir100.gc = median(
        incid.gc / (gc_s___B + gc_s___H + gc_s___W) * 5200,
        na.rm = TRUE
      ),
      ir100.ct = median(
        incid.ct / (ct_s___B + ct_s___H + ct_s___W) * 5200,
        na.rm = TRUE
      ),
      i.prev.dx.B   = median(i_dx___B / n___B, na.rm = TRUE),
      cc.dx.B       = median(i_dx___B / i___B, na.rm = TRUE),
      cc.linked1m.B = median(linked1m___B / i_dx___B, na.rm = TRUE),
      cc.vsupp.B    = median(i_sup___B / i_dx___B, na.rm = TRUE),
      i.prev.dx.H   = median(i_dx___H / n___H, na.rm = TRUE),
      cc.dx.H       = median(i_dx___H / i___H, na.rm = TRUE),
      cc.linked1m.H = median(linked1m___H / i_dx___H, na.rm = TRUE),
      cc.vsupp.H    = median(i_sup___H / i_dx___H, na.rm = TRUE),
      i.prev.dx.W   = median(i_dx___W / n___W, na.rm = TRUE),
      cc.dx.W       = median(i_dx___W / i___W, na.rm = TRUE),
      cc.linked1m.W = median(linked1m___W / i_dx___W, na.rm = TRUE),
      cc.vsupp.W    = median(i_sup___W / i_dx___W, na.rm = TRUE),
      prep_prop = median(
          (s_prep___B + s_prep___H + s_prep___W) /
          (s_prep_elig___B + s_prep_elig___H + s_prep_elig___W),
        na.rm = TRUE
      )
    )
}

add_targets <- function(d) {
    d %>% mutate(
      ir100.gc = incid.gc / (gc_s___B + gc_s___H + gc_s___W) * 5200,
      ir100.ct = incid.ct / (ct_s___B + ct_s___H + ct_s___W) * 5200,
      i.prev.dx.B   = i_dx___B / n___B,
      cc.dx.B       = i_dx___B / i___B,
      cc.linked1m.B = linked1m___B / i_dx___B,
      cc.vsupp.B    = i_sup___B / i_dx___B,
      i.prev.dx.H   = i_dx___H / n___H,
      cc.dx.H       = i_dx___H / i___H,
      cc.linked1m.H = linked1m___H / i_dx___H,
      cc.vsupp.H    = i_sup___H / i_dx___H,
      i.prev.dx.W   = i_dx___W / n___W,
      cc.dx.W       = i_dx___W / i___W,
      cc.linked1m.W = linked1m___W / i_dx___W,
      cc.vsupp.W    = i_sup___W / i_dx___W#,
    #   prep_prop = (s_prep___B + s_prep___H + s_prep___W) /
    #               (s_prep_elig___B + s_prep_elig___H + s_prep_elig___W)
    )
}

source("R/utils-epi_trackers.R")
ls_calib_trackers <- list(
  n           = epi_n,
  i           = epi_i,
  i_dx        = epi_i_dx,
  i_sup       = epi_i_sup,
  linked1m    = epi_linked_time(4),
  gc_s        = epi_gc_s(c(0, 1)),
  ct_s        = epi_ct_s(c(0, 1)),
  s_prep      = epi_s_prep,
  s_prep_elig = epi_s_prep_elig
)

calib_trackers <- epi_tracker_by_race(
  ls_calib_trackers,
  full = FALSE,
  indiv = TRUE
)
