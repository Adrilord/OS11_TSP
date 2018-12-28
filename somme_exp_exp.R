pannes = read.csv(
    file = "FailureTimes_5.csv",
    header = TRUE,
    sep = ",",
    dec = ".",
    colClasses = c("NULL", NA)
);
data = pannes$Heures;
h = hist(
    data,
    breaks = 20
);
N = length(data);
library(stats4);
le = function(
    lambda
   , muy
) {
    N * log(lambda) + N * log(muy)
       + sum(log(
           (exp(-lambda * data) - exp(-muy * data))
           / (muy - lambda)
           ));
}
f = function(x) {
    N * log(x[1]) + N * log(x[2])
    + sum(log(
        (exp(-x[1] * data) - exp(-x[2] * data)) / (x[2] - x[1])
        ));
}