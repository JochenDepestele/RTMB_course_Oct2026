
## NOTE THAT WE ARE IGNORING THE CORRELATION WITHIN THE BUCKETS. WE ARE TREATING EACH BUCKET AS INDEPENDENT.

library(RTMB)
library(here)

dat <- read.table(here::here("02_CourseMaterials","MLreminder","fractions.dat"), head=TRUE)
head(dat)

plot(dat$TF, dat$MF)

# How to set up the likelihood for this exercise?

# Model is based on a beta distribution with 2 params (alpha_1 and alpha_2) and a mean value of mu
# What are the params of the model with beta distribution?
# theta = (alpha, beta, phi>0) # alpha and beta can be positive and negative. phi has to be positive

# MFi ∼ B(μiϕ, (1 − μi)ϕ) # the Measure Fraction is estimated from eDNA samples

# logit(μi) = αlogit(TFi) + β # => this gives the prediction => the True Fraction is the number of mackerel in a bucket (this is observed)
# μi = plogis(logit(μ)) # plogis is the inverse of logit
# logit(p) = log(p/(1-p)) # => qlogis is the logit function

theta <- list(alpha=0, beta=0, logPhi = 0)

nll <- function(theta){
  getAll(dat,theta)
  alpha <- alpha
  beta <- beta
  phi <- exp(logPhi)
  mu <- plogis(alpha*qlogis(TF) + beta)
  -sum(dbeta(MF, mu*phi, (1-mu)*phi, log=TRUE))
}

obj <- MakeADFun(nll, theta)

fit <- nlminb(obj$par, obj$fn, obj$gr)

sdreport(obj)


