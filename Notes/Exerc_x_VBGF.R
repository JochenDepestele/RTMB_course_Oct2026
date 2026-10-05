library(RTMB)
library(here)

dat <- read.table(here::here("02_CourseMaterials","MLreminder","length.tab"), head=TRUE) # contains columns 'age', 'sex' and 'length'
par <- list(logLinf=0, logK=0, logSigma=0)

f <- function(par){
  getAll(dat,par)
  k <- exp(logK)
  Linf = exp(logLinf)
  sigma <- exp(logSigma)
  pred <- logLinf + log(1-exp(-k*age))
  ADREPORT(pred)
  REPORT(sigma)
  -sum(dnorm(log(length), pred, sigma, log=TRUE))
}

obj <- MakeADFun(f,par)
fit <- nlminb(obj$par, obj$fn, obj$gr)

sdr <- sdreport(obj)
pl <- as.list(sdr, "Est")
plr <- as.list(sdr, "Est", report=TRUE)



