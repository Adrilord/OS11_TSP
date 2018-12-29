pannes = read.csv(
    file = "FailureTimes_5.csv",
    header = TRUE,
    sep = ",",
    dec = ".",
    colClasses = c("NULL", NA)
);
scale = 1000;
data = pannes$Heures / scale;
N = length(data);

library(stats4);
nle = function(
    alpha,
    beta,
    lambda
) {
    # for debug purpose
    # cat("alpha=", alpha,
    #     " beta=", beta,
    #     " lambda=", lambda,
    #     "\n");
    N * lgamma(alpha) - N * log(1 - (beta / (beta + lambda)) ^ alpha) - (alpha - 1) * sum(log(data)) + sum(data);
}
# intial values
alpha0 = 4500; # mid value of second convex
beta0 = sum(data) / (N * alpha0);
lambda0 = N / sum(data);
# fitting
fit = mle(nle,
          start = list(
              alpha = alpha0,
              beta = beta0,
              lambda = lambda0
          ),
          method = "L-BFGS-B",
          lower = 1e-10,
          upper = Inf,
          control = list(
              trace = 6,
              maxit = 1000,
              ndeps = c(
                  1e-4,
                  1e-4,
                  1e-4
              )
          )
          );
summary(fit);

alpha = fit@fullcoef["alpha"];
beta = fit@fullcoef["beta"];
lambda = fit@fullcoef["lambda"];
f_gammaExp = function(x) {
    1 / gamma(alpha) * (1 - (beta / (beta + lambda)) ^ alpha) * x ^ (alpha - 1) * exp(-x)
}
hist(
    data,
    breaks = 40,
    probability = TRUE
);
curve(f_gammaExp,
      add = TRUE,
      col = "red",
      from = min(data),
      to = max(data)
);