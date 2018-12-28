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
N = length(data);

library(stats4);
nll = function(
    alpha,
    beta,
    lambda
) {
   N * log(gamma(alpha)) - N * log(1 - (beta / (beta + lambda)) ^ alpha) - (alpha - 1) * sum(log(data)) + sum(data);
}
# intial values
alpha0 = 4500; # mid value of second convex
beta0 = sum(data) / (N * alpha0);
lambda0 = 1;
# fitting
fit = mle(nll,
          start = list(
              alpha = alpha0,
              beta = beta0,
              lambda = lambda0
          ));
summary(fit);
