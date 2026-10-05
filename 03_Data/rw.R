library(RTMB)
dat <- list(y=c(-0.09, 0.09, 4.66, 3.38, -0.19, 0.5, -1.55, -0.34, -1.67,
                2.37, -2.92, 0.8, 3.22, -3.17, 1.08, -1.45, 1, -3.63,
                -3.37, 0.19, 0.67, -0.14, -4.27, 0.45, -5.23, 0.22, 0.26,
                -1.23, 2.18, -1.99, -0.13, -1.11, -2.38, -1.48, -0.25,
                -1.63, -7.54, -7.02, -10.31, -2.73, -8.14, -5.54, -8.23,
                -5.2, -6.94, -9.9, -6.08, -8.54, -2.33, -4.77))

par <- list(logSdRw=0, logSdObs=0, lam0=0, lam=numeric(length(dat$y)))

jnll <- function(par){
  getAll(par, dat)
  sdRw <- exp(logSdRw)
  sdObs <- exp(logSdObs)
  ret <- -dnorm(lam[1], mean=lam0, sd=sdRw, log=TRUE)
  ret <- ret - sum(dnorm(diff(lam), sd=sdRw, log=TRUE))
  ret <- ret - sum(dnorm(y, mean=lam, sd=sdObs, log=TRUE))
  ret
}

obj <- MakeADFun(jnll, par, random="lam", silent=TRUE)
opt <- nlminb(obj$par, obj$fn, obj$gr)

sdr <- sdreport(obj)
pl <- as.list(sdr, "Est")
plsd <- as.list(sdr, "Std")

plot(dat$y, xlab="Time", ylab="Y", las=1)
lines(pl$lam, lwd=3, col="red")
lines(pl$lam-2*plsd$lam, lty="dotted", lwd=3, col="red")
lines(pl$lam+2*plsd$lam, lty="dotted", lwd=3, col="red")

