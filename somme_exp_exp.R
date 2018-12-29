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
nle_expExp = function(
    lambda
   , muy
) {
    -(N * log(lambda) + N * log(muy)
       + sum(log(
           (exp(-lambda * data) - exp(-muy * data))
           / (muy - lambda)
           )));
}
# fitting
fit_expExp = mle(nle_expExp,
          start = list(
              lambda = 2,
              muy = 1
          ));
summary(fit_expExp);
lambda = fit_expExp@fullcoef["lambda"];
muy = fit_expExp@fullcoef["muy"];
f_expExp = function(x) {
    lambda * muy * (exp(-lambda * x) - exp(-muy * x)) / (muy - lambda);
}
hist(
    data,
    breaks = 40,
    probability = TRUE
);
curve(f_expExp,
      add = TRUE,
      col = "blue",
      from = min(data),
      to = max(data)
      );