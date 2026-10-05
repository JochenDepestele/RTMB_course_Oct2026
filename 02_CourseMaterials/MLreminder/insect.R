library(RTMB)

# for data we use the built-in data "InsectSprays"
par <- list(logAlpha=rep(0,nlevels(InsectSprays$spray)))
f<-function(par){
  getAll(InsectSprays, par)
  nll <- 0
  for(i in 1:length(count)){ # InsectSprays$count has 72 observations, so this loop will run 72 times
    lambda <- exp(logAlpha[spray[i]])
    nll <- nll - dpois(count[i],lambda,log=TRUE)
  }
  ADREPORT(exp(logAlpha))
  nll
}
obj <- MakeADFun(f, par)
opt <- nlminb(obj$par, obj$fn, obj$gr)

opt$par

#### Let's test if alpha A, B and F are the same using map (last slide of MLreminder)
# MakeADFun(f, par, map=list(logAlpha=factor(c(1,1,2,3,4,1))))
factor(c(1,1,2,3,4,1))
map1 <- list(logAlpha=factor(c(1,1,2,3,4,1)))
obj1 <- MakeADFun(f, par, map=map1, silent=T)
opt1 <- nlminb(obj1$par, obj1$fn, obj1$gr)

#### Are the models opt and opt1 different? => 
# opt has 6 different alpha values, while opt1 has 4 different alpha values (A, B and F are the same).
# DO THE CHI-SQUARE TEST:
1 - pchisq(2*(opt1$obj-opt$obj),2) # THE MODELS ARE NOT DIFFERENT, SINCE WE GET a P-value of 0.39 which is > 0.05, 
                                   # SO WE CAN SAY THAT ALPHA A, B AND F ARE THE SAME.

sdr <- sdreport(obj1)
plr <- as.list(sdr, "Est", report = TRUE)



### make sure to have alpha's equal to 15

map2 <- list(logAlpha=factor(c(NA,NA,2,3,4,NA)))
par2 <- par 
par2$logAlpha[c(1,2,6)] <- log(15)
obj2 <- MakeADFun(f, par, map=map2, silent=T)
opt2 <- nlminb(obj2$par, obj2$fn, obj2$gr)
pl <- as.list(sdreport(obj2), "Est")
opt2$par











