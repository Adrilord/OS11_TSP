fails = FailureTimes_5$Heures;
# hist(X=FailureTimes_5$Heures,nclass=20);
# est.par <- eWeibull(X=data, method="numerical.MLE");
# library(fitdistrplus);
# mledist(data,"weibull");

# shape        scale 
# 0.9332832 5547.1687746 
# 
# $convergence
# [1] 0
# 
# $value
# [1] 771.7526
# 
# $hessian
# shape         scale
# shape 156.830424231 -5.769351e-03
# scale  -0.005769351  2.302158e-06
# 
# $optim.function
# [1] "optim"
# 
# $optim.method
# [1] "Nelder-Mead"
# 
# $fix.arg
# NULL
# 
# $fix.arg.fun
# NULL
# 
# $weights
# NULL
# 
# $counts
# function gradient 
# 79       NA 
# 
# $optim.message
# NULL
# 
# $loglik
# [1] -771.7526
require(MASS)



cdweibull <- function(x, shape, scale, log = FALSE){
  dd <- dweibull(x, shape= shape, scale = scale, log = log)
  dd <- cumsum(dd) * c(0, diff(x))
  return(dd)
}

fails = FailureTimes_5$Heures;
x <- c(1:max(fails))

weib = fitdistr(na.omit(fails),"weibull")
gamma = fitdistr(na.omit(fails),"gamma")
exp = fitdistr(na.omit(fails),"exponential")
logn = fitdistr(na.omit(fails),"lognormal")
gofstat(logn)

hist(fails, prob=TRUE, main = "", xlab = "x", 
     ylab = "y", xlim = c(0,2*max(fails)), nclass=15,col='green',ylim=c(0,0.0004))

lines(dexp(x,rate=exp$estimate[1]),col='blue')
lines(dlnorm(x,meanlog=logn$estimate[1],sdlog=logn$estimate[2]),col='red')

curve(cdweibull(x, scale=weib$estimate[1], shape=weib$estimate[2]),
      from=0, to=40, add=TRUE)
