library(stats4)
f_melExp = function(p, lambda, mu, x) {
    p * lambda * exp(-lambda * x) + (1 - p) * mu * exp(-mu * x)
}
nle_melExp = function(
    p,
    lambda,
    mu
) {
    ## for debug purpose
    cat("p=", p,
        " lambda=", lambda,
        " mu=", mu,
        "\n")
    - sum(log(f_melExp(p, lambda, mu, data)))
}
# fitting
fit_melExp = mle(
    nle_melExp,
    start = list(
       p = 0.5,
       lambda = 1,
       mu = 2
    )
    ,
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
summary(fit_melExp)
lambda = fit_melExp@fullcoef["lambda"]
mu = fit_melExp@fullcoef["mu"]
p = fit_melExp@fullcoef["p"]
h_melExp = hist(
    data,
    breaks = 40,
    probability = TRUE,
    main = "Fitting mixture of Exp"
)
curve(
    f_melExp(p, lambda, mu, x),
    add = TRUE,
    col = "violet",
    from = min(h_melExp$mids),
    to = max(h_melExp$mids)
)
F_melExp = function(x) {
    p * (1 - exp(-lambda * x)) + (1 - p) * (1 - exp(-mu * x))
}
n_breaks = length(h_melExp$breaks)
distance_melExp = sum(
    abs((F_melExp(h_melExp$breaks[2:n_breaks]) - F_melExp(h_melExp$breaks[1:n_breaks - 1])) - h_melExp$density)
)