pannes = read.csv(
    file = "FailureTimes_5.csv",
    header = TRUE,
    sep = ",",
    dec = ".",
    colClasses = c("NULL", NA)
);
h = hist(
     pannes$Heures,
     breaks = 20
);
data = pannes$Heures;

library(stats4);
# suppose this is the a mixture of (1-p) * gamma and p * exp
mixden = function(w, # weight of exponential law
                  lambda, # rate of exp
                  k, # shape of gamma
                  theta) { # rate of gamma
    w*dexp(data, lambda) + (1 - w)*dgamma(data, k, theta);
}

nl = function(w,
              lambda,
              k,
              theta) {
    -sum(log(mixden(w, lambda, k, theta)));
}
# intial values
w0 = 0.5;
d = 2500;
d1 = subset(data, data > d);
d2 = subset(data, data > d);
lambda0 = 1 / mean(d1);
s = log(mean(d2)) - mean(log(d2));
k0 = (3 - s + sqrt(s^2 + 18 * s + 9)) / (12 * s);
theta0 = mean(d2) / k0;

# fitting
fit = mle(nl,
          start = list(
              w = w0,
              lambda = lambda0,
              k = k0,
              theta = theta0
          ));
summary(fit);
plot(profile(fit));
