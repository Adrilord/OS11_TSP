# Fitting mixture of exp and exp
theta_melExpExp = list(
    p = 0.5,
    lambda = 1 / mean(data),
    mu = 2 / mean(data)
)
# PDF
f_melExpExp = function(x, theta = theta_melExpExp) {
    theta$p * dexp(x, rate = theta$lambda) + (1 - theta$p) * dexp(x, rate = theta$mu)
}
nle_melExpExp = function(
    p,
    lambda,
    mu
) {
    ## for debug purpose
    cat("p=", p,
        " lambda=", lambda,
        " mu=", mu,
        "\n")
    theta = list(
        p = p,
        lambda = lambda,
        mu = mu
    )
    - sum(log(f_melExpExp(data, theta)))
}
# Fitting distribution
library(stats4)
fit_melExpExp = mle(
    nle_melExpExp,
    start = theta_melExpExp,
    method = "L-BFGS-B",
    lower = c(
        0,
        1e-4,
        1e-4
    ),
    upper = c(
        1,
        Inf,
        Inf
    ),
    control = list(
       trace = 6,
       maxit = 1000,
       ndeps = c(
           1e-4,
           1e-4,
           1e-4
       )
    )
)
summary(fit_melExpExp)
theta_melExpExp$lambda = fit_melExpExp@coef["lambda"]
theta_melExpExp$mu = fit_melExpExp@coef["mu"]
theta_melExpExp$p = fit_melExpExp@coef["p"]
h_melExpExp = hist(
    data,
    breaks = 40,
    probability = TRUE,
    main = "Fitting mixture of Exp"
)
curve(
    f_melExpExp(x, theta_melExpExp),
    add = TRUE,
    col = "violet",
    from = min(h_melExpExp$mids),
    to = max(h_melExpExp$mids)
)
# CDF
F_melExpExp = function(x, theta = theta_melExpExp) {
    theta$p * pexp(x, rate = theta$lambda) + (1 - theta$p) * pexp(x, rate = theta$mu)
}
# Kolmogorov-Smirnov test
kstest = ks.test(data, F_melExpExp)
