
#
## 03. Epidemic Model Burnin, Stage 1, Parameter Calibration
## PAFGuidelines (https://github.com/EpiModel/PAFGuidelines)
##

rm(list = ls())

## Packages
pkgload::load_all("../EpiModelHIV-p")

## Environmental Arguments
pull_env_vars()

## Parameters
netstats <- readRDS("data/input/netstats.rds")
epistats <- readRDS("data/input/epistats.rds")
est      <- readRDS("data/input/netest.rds")

param <- param_msm(netstats = netstats,
                   epistats = epistats,
                   hiv.test.rate = c(0.00385, 0.00380, 0.00690),
                   tx.init.prob = c(0.1775, 0.190, 0.2521),
                   tx.halt.partial.prob = c(0.0062, 0.0055, 0.0031),
                   tx.reinit.partial.prob = c(0.00255, 0.00255, 0.00255),
                   trans.scale = c(2.44, 0.424, 0.270),
                   riskh.start = 1,
                   prep.start = 26,
                   prep.start.prob = 0.66,

                   truncate.plist = 1,
                   part.ident.start = Inf,

                   riskhist.int = 182,
                   stitest.start = 1,
                   stitest.active.int = 52,
                   tst.rect.sti.rr = 1,
                   sti.highrisktest.int = 26,
                   stitest.elig.model = "all",
                   stianntest.gc.hivneg.coverage = 0.44,
                   stianntest.ct.hivneg.coverage = 0.44,
                   stihighrisktest.gc.hivneg.coverage = 0.0,
                   stihighrisktest.ct.hivneg.coverage = 0.0,
                   stianntest.gc.hivpos.coverage = 0.61,
                   stianntest.ct.hivpos.coverage = 0.61,
                   stihighrisktest.gc.hivpos.coverage = 0.0,
                   stihighrisktest.ct.hivpos.coverage = 0.0,
                   stianntest.cov.method = "curr",
                   stihighrisktest.cov.method = "curr",
                   partnercutoff = 1,
                   testing.pattern.sti = "interval",
                   tst.rect.sti.rr.hivneg = 0.48,
                   tst.rect.sti.rr.hivpos = 0.63
)
init <- init_msm(prev.ugc = 0.01,
                 prev.rgc = 0.01,
                 prev.uct = 0.01,
                 prev.rct = 0.01)

pkgload::load_all("../EpiModelHIV-p")
control <- control_msm(
  simno = fsimno,
  nsteps = 52 * 10,
  nsims = ncores,
  ncores = ncores,
  save.nwstats = TRUE,
  raw.output = FALSE,
  verbose = TRUE,
  initialize.FUN = initialize_msm,
  aging.FUN = aging_msm,
  departure.FUN = departure_msm,
  arrival.FUN = arrival_msm,
  stitest.FUN = sti_test_msm_paf,
  partident.FUN = partident_msm,
  hivtest.FUN = hivtest_msm,
  hivtx.FUN = hivtx_msm,
  hivprogress.FUN = hivprogress_msm,
  hivvl.FUN = hivvl_msm,
  resim_nets.FUN = simnet_msm,
  acts.FUN = acts_msm,
  condoms.FUN = condoms_msm,
  position.FUN = position_msm,
  prep.FUN = prep_msm,
  hivtrans.FUN = hivtrans_msm,
  stitrans.FUN = stitrans_msm,
  stirecov.FUN = stirecov_msm,
  stitx.FUN = stitx_msm,
  prev.FUN = prevalence_msm,
  verbose.FUN = verbose.net
)

# debugonce(sti_test_msm_paf)

## Simulation
sim <- netsim(est, param, init, control)

