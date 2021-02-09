plot(sim7001, y = "test.gc.12mo", xlim = c(3000, 3640), ylim = c(0,1), main = "test.gc.12.mo",
     xlab = "Time", "ylab" = "Total Tests")
plot(sim7002, y = "test.gc.12mo", mean.col = "red", qnts.col = "red", add = TRUE)
plot(sim7003, y = "test.gc.12mo", mean.col = "green", qnts.col = "green", add = TRUE)
plot(sim7004, y = "test.gc.12mo", mean.col = "yellow", qnts.col = "yellow", add = TRUE)
plot(sim7005, y = "test.gc.12mo", mean.col = "purple", qnts.col = "purple", add = TRUE)

plot(sim7001, y = "test.ct.12mo", xlim = c(3000, 3640), ylim = c(0,1), main = "test.ct.12.mo",
     xlab = "Time", "ylab" = "Total Tests")
plot(sim7002, y = "test.ct.12mo", mean.col = "red", qnts.col = "red", add = TRUE)
plot(sim7003, y = "test.ct.12mo", mean.col = "green", qnts.col = "green", add = TRUE)
plot(sim7004, y = "test.ct.12mo", mean.col = "yellow", qnts.col = "yellow", add = TRUE)
plot(sim7005, y = "test.ct.12mo", mean.col = "purple", qnts.col = "purple", add = TRUE)

plot(sim7001, y = "test.gc.12mo.hivdiag", xlim = c(3000, 3640), ylim = c(0,1), main = "test.gc.12.mo \n HIV Diag",
     xlab = "Time", "ylab" = "Total Tests")
plot(sim7002, y = "test.gc.12mo.hivdiag", mean.col = "red", qnts.col = "red", add = TRUE)
plot(sim7003, y = "test.gc.12mo.hivdiag", mean.col = "green", qnts.col = "green", add = TRUE)
plot(sim7004, y = "test.gc.12mo.hivdiag", mean.col = "yellow", qnts.col = "yellow", add = TRUE)
plot(sim7005, y = "test.gc.12mo.hivdiag", mean.col = "purple", qnts.col = "purple", add = TRUE)

plot(sim7001, y = "test.ct.12mo.hivdiag", xlim = c(3000, 3640), ylim = c(0,1), main = "test.ct.12.mo \n HIV Diag",
     xlab = "Time", "ylab" = "Total Tests")
plot(sim7002, y = "test.ct.12mo.hivdiag", mean.col = "red", qnts.col = "red", add = TRUE)
plot(sim7003, y = "test.ct.12mo.hivdiag", mean.col = "green", qnts.col = "green", add = TRUE)
plot(sim7004, y = "test.ct.12mo.hivdiag", mean.col = "yellow", qnts.col = "yellow", add = TRUE)
plot(sim7005, y = "test.ct.12mo.hivdiag", mean.col = "purple", qnts.col = "purple", add = TRUE)

plot(sim7001, y = "test.gc.12mo.nonhivdiag", xlim = c(3000, 3640), ylim = c(0,1), main = "test.gc.12.mo \n non-HIV Diag",
     xlab = "Time", "ylab" = "Total Tests")
plot(sim7002, y = "test.gc.12mo.nonhivdiag", mean.col = "red", qnts.col = "red", add = TRUE)
plot(sim7003, y = "test.gc.12mo.nonhivdiag", mean.col = "green", qnts.col = "green", add = TRUE)
plot(sim7004, y = "test.gc.12mo.nonhivdiag", mean.col = "yellow", qnts.col = "yellow", add = TRUE)
plot(sim7005, y = "test.gc.12mo.nonhivdiag", mean.col = "purple", qnts.col = "purple", add = TRUE)

plot(sim7001, y = "test.ct.12mo.nonhivdiag", xlim = c(3000, 3640), ylim = c(0,1), main = "test.ct.12.mo \n non-HIV Diag",
     xlab = "Time", "ylab" = "Total Tests")
plot(sim7002, y = "test.ct.12mo.nonhivdiag", mean.col = "red", qnts.col = "red", add = TRUE)
plot(sim7003, y = "test.ct.12mo.nonhivdiag", mean.col = "green", qnts.col = "green", add = TRUE)
plot(sim7004, y = "test.ct.12mo.nonhivdiag", mean.col = "yellow", qnts.col = "yellow", add = TRUE)
plot(sim7005, y = "test.ct.12mo.nonhivdiag", mean.col = "purple", qnts.col = "purple", add = TRUE)

plot(sim7001, y = "test.ct.12mo", xlim = c(3000, 3640), ylim = c(0,1), main = "test.ct.12.mo",
     xlab = "Time", "ylab" = "Total Tests")
plot(sim7002, y = "test.ct.12mo", mean.col = "red", qnts.col = "red", add = TRUE)
plot(sim7003, y = "test.ct.12mo", mean.col = "green", qnts.col = "green", add = TRUE)
plot(sim7004, y = "test.ct.12mo", mean.col = "yellow", qnts.col = "yellow", add = TRUE)
plot(sim7005, y = "test.ct.12mo", mean.col = "purple", qnts.col = "purple", add = TRUE)

plot(sim7001, y = "ir100.gc", xlim = c(3000, 3640), ylim = c(0,10), main = "ir100.gc",
     xlab = "Time", "ylab" = "Incidence")
plot(sim7002, y = "ir100.gc", mean.col = "red", qnts.col = "red", add = TRUE)
plot(sim7003, y = "ir100.gc", mean.col = "green", qnts.col = "green", add = TRUE)
plot(sim7004, y = "ir100.gc", mean.col = "yellow", qnts.col = "yellow", add = TRUE)
plot(sim7005, y = "ir100.gc", mean.col = "purple", qnts.col = "purple", add = TRUE)

plot(sim7001, y = "ir100.ct", xlim = c(3000, 3640), ylim = c(0,10), main = "ir100.ct",
     xlab = "Time", "ylab" = "Incidence")
plot(sim7002, y = "ir100.ct", mean.col = "red", qnts.col = "red", add = TRUE)
plot(sim7003, y = "ir100.ct", mean.col = "green", qnts.col = "green", add = TRUE)
plot(sim7004, y = "ir100.ct", mean.col = "yellow", qnts.col = "yellow", add = TRUE)
plot(sim7005, y = "ir100.ct", mean.col = "purple", qnts.col = "purple", add = TRUE)

plot(sim7001, y = "i.prev.dx", xlim = c(3000, 3640), ylim = c(0,1), main = "i.prev.dx",
     xlab = "Time", "ylab" = "Incidence")
plot(sim7002, y = "i.prev.dx", mean.col = "red", qnts.col = "red", add = TRUE)
plot(sim7003, y = "i.prev.dx", mean.col = "green", qnts.col = "green", add = TRUE)
plot(sim7004, y = "i.prev.dx", mean.col = "yellow", qnts.col = "yellow", add = TRUE)
plot(sim7005, y = "i.prev.dx", mean.col = "purple", qnts.col = "purple", add = TRUE)


