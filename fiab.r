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

library(fitdistrplus)

cdweibull <- function(x, shape, scale, log = FALSE){
  dd <- dweibull(x, shape= shape, scale = scale, log = log)
  dd <- cumsum(dd) * c(0, diff(x))
  return(dd)
}

fails <- FailureTimes_5$Heures;
fails <- c(na.omit(fails))
x <- c(1:max(fails))

N=length(fails)
weib = fitdist(fails,"weibull",method = "mle",lower = c(0, 0))
gamma = fitdist(fails,"gamma",method = "mle",lower = c(0, 0))
exp = fitdist(fails/max(fails),"exp",method = "mle",start=list(rate=1))
logn = fitdist(fails,"lnorm",method = "mle")
lambda = N/sum(fails)
gofstat(weib)$chisqpvalue
gofstat(gamma)$chisqpvalue
gofstat(logn)$chisqpvalue
gofstat(exp)$chisqpvalue

hist(fails, prob=TRUE, main = "", xlab = "x", 
     ylab = "y", xlim = c(0,2*max(fails)), nclass=15,col='green',ylim=c(0,0.0002))
hist(fails, prob=TRUE, main = "", xlab = "x", 
     ylab = "y", xlim = c(0,2*max(fails)), nclass=6,col='green',ylim=c(0,0.0002))

lines(dexp(x,rate=exp$estimate[1]),col='blue')
lines(dlnorm(x,meanlog=logn$estimate[1],sdlog=logn$estimate[2]),col='red')

curve(cdweibull(x, scale=weib$estimate[1], shape=weib$estimate[2]),
      from=0, to=40, add=TRUE)

library(stats4)
ll <- function(p,rate1,shape2,rate2) 

cp=800
cc=1200
meanCostExp <- function(x, lambda){
  cost <- (exp(-lambda*x)*(cp-cc)+cc)
  time <- (1/lambda)*(1-exp(-lambda*x))
  return(cost/time)
}
curve(meanCostExp(x,lambda),col='red',from=0,to=max(fails))



evolutionArray <- rep(rep(0,10),10)
evolutionList <- rep(0,100)
dim(evolutionArray) <- c(10,10)
for(j in 3:12) {
  evolutionArray[1,j-2] = DegradLevel_2[1,j]
  evolutionList[j-2] = DegradLevel_2[1,j]
  for(i in 2:10) {
    evolutionArray[i,j-2] = DegradLevel_2[i,j] - DegradLevel_2[i-1,j]
    evolutionList[(i-1)*10+j-2] = DegradLevel_2[i,j] - DegradLevel_2[i-1,j]
  }
}
hist(na.omit(evolutionList), prob=TRUE, main = "", xlab = "x", 
     ylab = "y", xlim = c(0,2*max(na.omit(evolutionList))), nclass=6,col='green')
gammaBis = fitdist(data=c(na.omit(evolutionList)),"gamma")
x=seq(from=0,to=max(c(na.omit(evolutionList))),by=0.1)
curve(dgamma(x,shape=gammaBis$estimate[1],rate=gammaBis$estimate[2]),col='blue',from=0, to=10)
gofstat(gammaBis)$chisqpvalue
