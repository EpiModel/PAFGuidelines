# library(EpiModelHIV)
pkgload::load_all("../EMPAF")

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
  gc_i_hivpos   = epi_gc_i(1),
  ct_i_hivpos   = epi_ct_i(1),
  gc_s_hivpos   = epi_gc_s(1),
  ct_s_hivpos   = epi_ct_s(1),
  gc_i_hivneg   = epi_gc_i(0),
  ct_i_hivneg   = epi_ct_i(0),
  gc_s_hivneg   = epi_gc_s(0),
  ct_s_hivneg   = epi_ct_s(0),
  prep_start    = epi_prep_start,
  prep_time_on  = epi_prep_time_on
)

epi_trackers <- epi_tracker_by_race(ls_trackers, full = TRUE, indiv = FALSE)

ls_restart_trackers <- list(
  n        = epi_n,
  i        = epi_i,
  i_dx     = epi_i_dx,
  i_sup    = epi_i_sup,
  linked1m = epi_linked_time(4),
  gc_s     = epi_gc_s(c(0, 1)),
  ct_s     = epi_ct_s(c(0, 1))
)

restart_trackers <- epi_tracker_by_race(
  ls_restart_trackers,
  full = FALSE,
  indiv = TRUE
)

# Params and inits
orig <- readRDS("out/est/netest.rds")
netstats <- readRDS("out/est/netstats.rds")
epistats <- readRDS("out/est/epistats.rds")

full_tx_eff <- rep(1, 3)
prep_start_time <- 52 * 130 + 1
prep_start_prob <- rep(0.31, 3)

param <- param_msm(
  netstats = netstats,
  epistats = epistats,
  # Update on HIV natural history
  vl.acute.rise.int = 3,
  vl.acute.fall = 3,

  hiv.test.rate = c(0.0036, 0.0036, 0.00685),
  hiv.test.late.prob = rep(0, 3),
  tx.init.prob = c(0.1775, 0.19, 0.2521),
  tt.part.supp = 1 - full_tx_eff,
  tt.full.supp = full_tx_eff,
  tt.dur.supp = rep(0, 3),
  tx.halt.partial.prob =  c(0.00580, 0.00475, 0.00280), # c(0.0065, 0.0053, 0.003),
  tx.halt.full.rr = rep(0.45, 3),
  tx.halt.durable.rr = rep(0.45, 3),
  tx.reinit.partial.prob = rep(0.00255, 3),
  tx.reinit.full.rr = rep(1, 3),
  tx.reinit.dur.rr = rep(1, 3),
  max.time.off.tx.full.int = 52 * 15,
  max.time.on.tx.part.int = 52 * 10,
  max.time.off.tx.part.int = 52 * 10,
  aids.mr = 1 / 250,
  trans.scale =  c(2.53, 0.40, 0.275), # c(3.16, 0.40, 0.30), #c(2.75, 0.4, 0.), #c(2.21, 0.405, 0.255),
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
  prep.start.prob =  prep_start_prob,

  # qexp(1 - 0.57, 52) -> 0.0115036 (57% retention at year 1, 52 steps)
  prep.discont.rate = rep(0.0079, 3),
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
  ugc.tprob      = 0.203, # 0.20525, (post PrEP calib)
  rgc.tprob      = 0.2764412, # plogis(qlogis(param$ugc.tprob) + log(1.5))
  uct.tprob      = 0.13125, # 0.138, (post PrEP calib)
  rct.tprob      = 0.1847507, # plogis(qlogis(param$uct.tprob) + log(1.5))
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
  gc.sympt.prob.tx =  rep(0.7, 3),
  ct.sympt.prob.tx =  rep(0.7, 3),
  gc.asympt.prob.tx = rep(0.8, 3), # prob of starting tx if diagnosed but asympt
  ct.asympt.prob.tx = rep(0.8, 3), # prob of starting tx if diagnosed but asympt
  sti.cond.eff = 0.90, # default
  sti.cond.fail = c(0.2, 0.2, 0.2), # default
  # Calibrate these **NO**
  # Increase ACQUISITION risk if HIV- & STI+
  hiv.rgc.rr = 2.78,
  hiv.ugc.rr = 1.73,
  hiv.rct.rr = 2.78,
  hiv.uct.rr = 1.73,
 # if both ct + gc -> log(RRgc) + 0.2 * log(RRct) | swap ct and gc if RRct > RRgc
  hiv.dual.rr = 0.2, # not mentionned in appendix
  # Increase TRANSMISSION risk from a HIV+ & STI+
  hiv.trans.gc.rr = 1.3,
  hiv.trans.ct.rr = 1.3,

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

  # Part ident parameters - No partner services in this model
  part.ident.start = Inf,

  param_updaters = list(
    # High PrEP intake for the first year
    list(
      at = prep_start_time,
      param = list(
        # prep.start.prob = prep_start_prob * 2
        prep.start.prob = function(x) x * 2
      )
    ),
    # go back to normal to get to 15%
    list(
      at = prep_start_time + 52,
      param = list(
        # prep.start.prob =  prep_start_prob
        prep.start.prob = function(x) x / 2
      )
    )
  ),
  epi_trackers = epi_trackers
)

init <- init_msm(
  prev.ugc = 0.05,
  prev.rct = 0.05,
  prev.rgc = 0.05,
  prev.uct = 0.05
)
