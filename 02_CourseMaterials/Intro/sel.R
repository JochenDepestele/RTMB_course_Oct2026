

dat <- read.table(here::here("02_CourseMaterials","Intro","sel.dat"), header=TRUE)

# dat <- read.table("sel.dat", head=TRUE)
nll <- function(theta){
  L50 <- exp(theta[1])
  k <- exp(theta[2])
  p <- 1/(1+exp(-k*(dat$L-L50)))
  -sum(dbinom(dat$Y,1,p,log=TRUE))
}

fit <- nlminb(c(logL50=3,logK=-1), nll)

