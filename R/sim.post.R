library("EpiModelHIV")
#process_simfiles(simno = 2001, nsims = 500)
fn <- list.files("data", pattern = "7009", full.names = TRUE)
njobs <- length(fn)
nsims <- njobs*28
process_simfiles(simno = 7009, min.n = 2, nsims = 50)



#Choose a sim to load for exploratory analysis
load("data/sim.n1001.rda")
df <- as.data.frame(sim, out = "mean")

mean <- c(mean(tail(df$ir100.gc, 52)),
          mean(tail(df$ir100.ct, 52)),
          mean(tail(df$i.prev.dx, 52))
      )


mean

qnt.25 <- c(quantile(tail(df$ir100.gc, 52), 0.25),
          quantile(tail(df$ir100.ct, 52), 0.25),
          quantile(tail(df$i.prev.dx, 52), 0.25)
)


qnt.25

qnt.75 <- c(quantile(tail(df$ir100.gc, 52), 0.75),
            quantile(tail(df$ir100.ct, 52), 0.75),
            quantile(tail(df$i.prev.dx, 52), 0.75)
)


qnt.75

#Follow Up
fn <- list.files("data", pattern = "sim", full.names = TRUE)
njobs <- length(fn)
nsims <- njobs*28
process_simfiles(simno = 7001, min.n = njobs, nsims = nsims)
#Choose a sim to load for exploratory analysis
load("data/sim.n7001.rda")
df <- as.data.frame(sim, out = "mean")
