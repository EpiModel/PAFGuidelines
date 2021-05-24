library(EpiModelHIV)
# pkgload::load_all("../EpiModelHIV-p/")

# Epi Trackers
source("R/utils-epi_trackers.R")
ls_trackers <- list(
  n             = epi_n,
  s             = epi_s,
  s_prep_elig   = epi_s_prep_elig,
  s_prep        = epi_s_prep,
  i             = epi_i,
  i_dx          = epi_i_dx,
  i_tx          = epi_i_tx,
  i_sup         = epi_i_sup,
  # i_sup_dur     = epi_i_sup_dur,
  linked1m      = epi_linked_time(4),
  gc_i          = epi_gc_i,
  ct_i          = epi_ct_i,
  gc_s          = epi_gc_s,
  ct_s          = epi_ct_s,
  prep_start    = epi_prep_start,
  prep_time_on  = epi_prep_time_on
)
epi_trackers <- epi_tracker_by_race(ls_trackers, full = F, indiv = TRUE)


# Params and inits
orig <- readRDS("out/est/netest.rds")
netstats <- readRDS("out/est/netstats.rds")
epistats <- readRDS("out/est/epistats.rds")

full_tx_eff <- rep(1, 3)
prep_start_time <- 52 * 65 + 1

param <- param_msm(
  netstats = netstats,
  epistats = epistats,
  # Update on HIV natural history
  vl.acute.rise.int = 3,
  vl.acute.fall = 3,

  hiv.test.rate = c(0.00385, 0.00385, 0.0069),
  hiv.test.late.prob = rep(0, 3),
  tx.init.prob = c(0.1775, 0.19, 0.2521),
  tt.part.supp = 1 - full_tx_eff,
  tt.full.supp = full_tx_eff,
  tt.dur.supp = rep(0, 3),
  tx.halt.partial.prob = c(0.0065, 0.0053, 0.003),
  tx.halt.full.rr = rep(0.45, 3),
  tx.halt.durable.rr = rep(0.45, 3),
  tx.reinit.partial.prob = rep(0.00255, 3),
  tx.reinit.full.rr = rep(1, 3),
  tx.reinit.dur.rr = rep(1, 3),
  max.time.off.tx.full.int = 52 * 15,
  max.time.on.tx.part.int = 52 * 10,
  max.time.off.tx.part.int = 52 * 10,
  aids.mr = 1 / 250,
  trans.scale =  c(3.16, 0.40, 0.30), #c(2.75, 0.4, 0.), #c(2.21, 0.405, 0.255),
  acts.scale = 1.00,
  acts.scale.main = 1.00,
  acts.scale.casl = 1.00,
  acts.aids.vl = 5.75,
  circ.prob = c(0.874, 0.874, 0.918),
  a.rate = 0.00052,
  cond.eff = 0.95, # default
  cond.fail = c(0.25, 0.25, 0.25), # default

  prep.start = prep_start_time,
  riskh.start = prep_start_time - 52,
  prep.adhr.dist = c(0.089, 0.127, 0.784),
  prep.adhr.hr = c(0.69, 0.19, 0.01),
  prep.start.prob =  rep(0.71, 3), # 0.00896,

  # qexp(1 - 0.57, 52) -> 0.0115036 (57% retention at year 1, 52 steps)
  prep.discont.rate = rep(0.02138792, 3), # 1 - (2^(-1/(224.4237/7)))
  ## prep.tst.int = 90/7,         # do I need that?
  prep.risk.int = 26,
  ## prep.sti.screen.int = 182/7,
  ## prep.sti.prob.tx = 1,
  prep.risk.reassess.method = "year",
  prep.require.lnt = TRUE, # FALSE -> start with random PrEP initiation

  ## STI PARAMS
  ## stitestguidelines appendix table s2
  #
  # Calibrate these
  rgc.tprob      = 0.5364,
  ugc.tprob      = 0.4347,
  rct.tprob      = 0.2494,
  uct.tprob      = 0.1944,
  #
  rgc.sympt.prob = 0.16,
  ugc.sympt.prob = 0.80,
  rct.sympt.prob = 0.14,
  uct.sympt.prob = 0.48,
  rgc.ntx.int    = 25,
  ugc.ntx.int    = 25,
  gc.tx.int      = 1, # seen with infectiologist, reasonable assumption
  rct.ntx.int    = 45,
  uct.ntx.int    = 45,
  ct.tx.int      = 1, # same. Time recommended to use condom
  gc.sympt.prob.tx =  rep(0.9, 3),
  ct.sympt.prob.tx =  rep(0.85, 3),
  gc.asympt.prob.tx = rep(1, 3), # prob of starting tx if diagnosed but asympt
  ct.asympt.prob.tx = rep(1, 3), # prob of starting tx if diagnosed but asympt
  sti.cond.eff = 0.90, # default
  sti.cond.fail = c(0.2, 0.2, 0.2), # default
  # Calibrate these **NO**
  hiv.rgc.rr = 2.78,
  hiv.ugc.rr = 1.73,
  hiv.rct.rr = 2.78,
  hiv.uct.rr = 1.73,
 # if both ct + gc -> log(RRgc) + 0.2 * log(RRct) | swap ct and gc if RRct > RRgc
  hiv.dual.rr = 0.2, # not mentionned in appendix

  truncate.plist = 52,
  ## PAFGuidelines specific params
  riskhist.int = 52,
  stitest.start = 1,
  stitest.active.int = 52,
  tst.rect.sti.rr = 1,
  testing.pattern.sti = "interval",
  tst.rect.sti.rr.hivneg = 0.48,
  tst.rect.sti.rr.hivpos = 0.63,
  sti.correlation.time = 12,
  # High Risk testing
  stitest.elig.model = "all",
  stihighrisktest.cov.method = "curr",
  sti.highrisktest.int = 26,
  partnercutoff = 1,
  stihighrisktest.gc.hivneg.coverage = 0.0,
  stihighrisktest.ct.hivneg.coverage = 0.0,
  stihighrisktest.gc.hivpos.coverage = 0.0,
  stihighrisktest.ct.hivpos.coverage = 0.0,
  # Annual testing
  stianntest.cov.method = "curr",
  stianntest.gc.hivneg.coverage = 0.44,
  stianntest.ct.hivneg.coverage = 0.44,
  stianntest.gc.hivpos.coverage = 0.61,
  stianntest.ct.hivpos.coverage = 0.61,

  # Part ident parameters (defaut is ATL Complete, but never starts here)
  part.ident.start = Inf,
  part.index.window = 0, # ALWAYS KEEP AT 0
  part.index.degree = 1,
  part.index.prob = 0.666,
  part.ident.main.window = 52,
  part.ident.casl.window = 52,
  part.ident.ooff.window = 52,
  # see "R/z-indent_prob_calib.R"
  part.ident.main.prob = 0.2057143,
  part.ident.casl.prob = 0.1440000,
  part.ident.ooff.prob = 0.0240000,
  # Part Serv Params
  part.hiv.test.rate = rep(0.394, 3),
  part.prep.start.prob = rep(0, 3),
  part.tx.init.prob = rep(0.387, 3),
  part.tx.reinit.prob = rep(0, 3),

  param_updaters = list(),
  epi_trackers = epi_trackers
)

# Ensure that we do not truncate more than what is needed
# param <- update_params(
#   param, list(
#     truncate.plist = max(
#       param$part.ident.main.window,
#       param$part.ident.casl.window,
#       param$part.ident.ooff.window
#     ) + 1
#   )
# )

## must be set by the calling script
if (lnt == FALSE) {
  param <- update_params(
    param, list(
      prep.require.lnt = FALSE,
      prep.start.prob = 0.00411
    )
  )
}

init <- init_msm(
  prev.ugc = 0.05,
  prev.rct = 0.05,
  prev.rgc = 0.05,
  prev.uct = 0.05
)
