library(here)
library(RTMB)

####
f <- function(par)1/(1+exp(-par[1]*(par[3]-par[2])))
f(c(.2, 30, 25))

p <- c(k=.2, L50=30, L=25)
F <- MakeTape(f,p)

#### LOGISTIC REGRESSION WITHOUT Automatic Differentiation
sel <- read.table(here::here("02_CourseMaterials","Intro","sel.dat"), header=TRUE)

nll <- function(theta){
  L50 <- exp(theta[1])
  k <- exp(theta[2])
  p <- 1/(1+exp(-k*(dat$L-L50)))
  -sum(dbinom(dat$Y,1,p,log=TRUE))
}
fit <- nlminb(c(logL50=3,logK=-1), nll)
fit


#### Automatic Differentiation
## Derivatives
F$jacobian(p) # Here you're getting the derivatives.

# How to validate this? =>  compare to numerical derivatives:
library(numDeriv)
# Use the actual function f (and not the one that we defined in F)
numDeriv::grad(f,p)

## Derivatives
####
# The Derivative of the original function is called DF:
DF <- F$jacfun() #3 parameters are turned into 3-dimensional gradient
DF(p) 
DF$jacobian(p) # Second order derivative of the original function => this is the same as the Hessian of the original function (numDeriv::hessian).


# How to validate this? =>  compare to numerical derivatives:
library(numDeriv)
# Use the actual function f (and not the one that we defined in F)
numDeriv::hessian(f,p)


#### LOGISTIC REGRESSION WITH Automatic Differentiation
dat <- read.table(here::here("02_CourseMaterials","Intro","sel.dat"), header=TRUE)
nll <- function(theta){ # FUNCTION SHOULD ONLY DEPEND ON PARAMETERS !
  getAll(theta, dat)
  L50 <- exp(logL50)
  k <- exp(logK)
  p <- 1/(1+exp(-k*(L-L50)))
  -sum(dbinom(Y,1,p,log=TRUE))
}

theta <- list(logL50=3,logK=-1)

obj <- MakeADFun(nll, theta)

fit <- nlminb(obj$par, obj$fn, obj$gr)


# sdreport(obj) # => last.par => where the function ended up.
# summary(sdreport(obj))
# obj$env$last.par.best # => summary gives the same result as sdreport(obj)
# sdreport(obj, par.fixed = obj$env$last.par.best)


#### SPEED UP THE COMPUTER if you do not get 0.08 or so... use BLAS or OpenBLAS ???
system.time(A<-solve(diag(1000)+.1)) # Calculate the inverse of a 1000x1000 matrix






