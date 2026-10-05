library(RTMB)
library(here)

dat <- read.table(here::here("02_CourseMaterials","MLreminder","length2.tab"), head=TRUE) # contains columns 'age', 'sex' and 'length'
par <- list(logLinf=c(0,0), logK=c(0,0), logSigma=0)


f <- function(par){
  getAll(dat,par)
  idx<-ifelse(sex=="F",1,2)  # YOU CANNOT HAVE AN IF-STATEMENT BASED ON A PARAMETER, BUT YOU CAN 
                             # USE THAT TO INDEX THE PARAMETER VECTOR IF IT's IN THE DATA 
                             # => The index will select between the parameters for males and females
  k <- exp(logK)
  sigma <- exp(logSigma)
  pred <- logLinf[idx] + log(1-exp(-k[idx]*age))
  ADREPORT(pred)
  -sum(dnorm(log(length), pred, sigma, log=TRUE))
}

obj <- MakeADFun(f,par)
fit <- nlminb(obj$par, obj$fn, obj$gr)





obj1a <- MakeADFun(f,par, map=list(logK=factor(c(1,1))))
fit1a <- nlminb(obj1a$par, obj1a$fn, obj1a$gr)

obj1b <- MakeADFun(f,par, map=list(logLinf=factor(c(1,1))))
fit1b <- nlminb(obj1b$par, obj1b$fn, obj1b$gr)

obj2 <- MakeADFun(f,par, map=list(logLinf=factor(c(1,1)), logK=factor(c(1,1))))
fit2 <- nlminb(obj2$par, obj2$fn, obj2$gr)

p1a <- 1-pchisq(2*(fit1a$objective-fit$objective),1) # reject
p1b <- 1-pchisq(2*(fit1b$objective-fit$objective),1) # reject
p2 <- 1-pchisq(2*(fit2$objective-fit$objective),2) # reject
