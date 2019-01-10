## Estimate parameters for process
lag = 2
delta = 0.8 * lag
d = t(diff(t(process), lag))
# Concatenate into one vector
increments = vector(
    mode = "numeric",
    length = length(d)
)
n = dim(d)[[1]]
l = dim(d)[[2]]
for (i in 1:n) {
    increments[((i - 1) * l + 1):(i * l)] = d[i,]
}
# Filter out NA values
increments = increments[!is.na(increments)]
# Histogram
hiso_degrad = hist(
    increments,
    breaks = 10,
    probability = TRUE,
    main = "Distribution des accroissements",
    xlab = "dx",
    ylab = "Densité"
)
# Estimate gamma distribution
library(MASS)
estim = fitdistr(
    increments,
    dgamma,
    list(
        shape = 1,
        rate = 1
    )
)
# Draw estimated density
a = estim$estimate[[1]] / delta
b = estim$estimate[[2]]
curve(
    dgamma(x, a * delta ,b),
    add = TRUE,
    col = "red"
)
# Kolmogorov-Smirnov test
test = ks.test(increments, "pgamma", a * delta, b)
c(delta, a, b, test$p.value) # Print values
# PDF
f = function(x, t) {
    dgamma(x, shape = a * t, rate = b)
}
# CDF
F = function(x, t) {
    pgamma(x, shape = a * t, rate = b)
}
# Generator
rG = function(n, t) {
    rgamma(n, shape = a *t, rate = b)
}