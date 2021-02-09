## File processing -------------------------------------------------------------

suppressMessages(library("EpiModelHIV"))
suppressMessages(library("tidyverse"))
suppressMessages(library("foreach"))
source("post/fx.R")

fn <- list.files("data/", full.names = TRUE)
fn <- list.files("data/", full.names = TRUE, pattern = "1")


tdf <- data.frame("batch" = 0, "ir100.gc" = 0, "ir100.ct" = 0, "i.prev.dx" = 0)

# doParallel::registerDoParallel(parallel::detectCores())
# tdf <- foreach(i = 1:length(fn)) %dopar% {
#   load(fn[i])
#   f <- function(j) {
#     df <- as.data.frame(x = sim, sim = j)
#     df <- select(df, cc.dx, i.prev.dx, cc.linked1m, cc.vsupp)
#     df <- tail(df, 52)
#     batch <- paste(paste(strsplit(fn[i], "[.]")[[1]][2:3], collapse = "."), j, sep = ".")
#     out <- c(batch, colMeans(df))
#     return(out)
#   }
#   t(sapply(1:sim$control$nsims, f))
# }


for (i in 1:length(fn)) {
  load(fn[i])
  for (j in 1:20) {
    f <- function(j) {
      df <- as.data.frame(x = sim, sim = j)
      df <- select(df, ir100.gc, ir100.ct, i.prev.dx)
      df <- tail(df, 52)
      batch <- paste(paste(strsplit(fn[i], "[.]")[[1]][2:3], collapse = "."), j, sep = ".")
      out <- c(batch, colMeans(df))
      return(out)
    }
    tdf <- rbind(tdf, f(j))
  }
}

tdf <- data.frame(do.call("rbind", tdf), stringsAsFactors = FALSE)
tdf[2:4] <- sapply(tdf[2:4], as.numeric)

## Model Performance
stats <- c(4.2, 6.4, 0.17)

data <- apply(tdf[,c(2:4)], 1, dis_per, y = stats)
temp.per <- which(data >= quantile(data, 0.999))
tdf[temp.per,]

data <- apply(tdf[,c(2:4)], 1, dis_euc, y = stats)
temp.euc <- which(data <= quantile(data, 0.01))
tdf[temp.euc,]

data <- apply(tdf[,c(2:4)], 1, dis_log, y = stats)
temp.log <- which(data <= quantile(data, 0.01))
tdf[temp.log,]

data <- apply(tdf[,c(2:4)], 1, dis_cos, y = stats)
temp.cos <- which(data >= quantile(data, 0.999, na.rm = TRUE))
tdf[temp.cos,]

tdf.select <- list("Percent" = tdf[temp.per,], "Euclidean" = tdf[temp.euc,],
                   "Log-Diff" = tdf[temp.log,], "Cosine Sim." = tdf[temp.cos,])

load("data/sim.n1001.7.20201018.1828.rda")
ls()
s11 <- get_sims(sim, sims = 13)

df <- as.data.frame(s11)
df <- select(df, ir100.gc, ir100.ct, i.prev.dx)
df <- tail(df,52)
df
colMeans(df)

# Save as best-fitting
sim <- s11

saveRDS(sim, file = "est/burnin.ATL.3race.rds", compress = "xz")
saveRDS(tdf, file = "est/tdf.ATL.3race.rds", compress = "xz")
saveRDS(tdf.select, file = "est/tdf.sel.ATL.3race.rds", compress = "xz")
