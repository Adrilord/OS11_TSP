# Fitting mixture of Exp and Gamma
s = log(mean(data[data >= 2.5])) - mean(log(data[data >= 2.5]))
theta_melExpGamma = list(
    p1 = 0.5,
    p2 = 0.5,
    lambda = 1 / sum(data[data <= 2.5]),
    alpha = (3 - s + sqrt((s - 3)^2 + 24 * s)) / (12 * s),
    beta = mean(data[data >= 2.5]) / theta_melExpGamma$alpha
)
# PDF
f_melExpGamma = function(x, theta = theta_melExpGamma) {
    theta$p1 * dexp(data, rate = theta$p) + theta$p2 * dgamma(data, shape = theta$alpha, rate = theta$beta)
}
# CDF
F_melExpGamma = function(x, theta = theta_melExpGamma) {
    theta$p1 * pexp(data, rate = theta$p) + theta$p2 * pgamma(data, shape = theta$alpha, rate = theta$beta)
}

# Fitting distribution
library(stats4)
nle_melExpGamma = function(
    p,
    lambda,
    alpha,
    beta
) {
    ## for debug purpose
    theta = list(
        p = p,
        lambda = lambda,
        alpha = alpha,
        beta = beta
    )
    cat("p=", theta$p,
        " lambda=", theta$lambda,
        " alpha=", theta$alpha,
        " beta=", theta$beta,
        "\n")
    - sum(log(f_melExpGamma(data, theta)))
}
fit_melExpGamma = mle(
    nle_melExpGamma,
    start = theta_melExpGamma,
    method = "L-BFGS-B",
    lower = c(
        0,
        1e-6,
        1e-6,
        1e-6
    ),
    upper = c(
        1,
        Inf,
        Inf,
        Inf
    ),
    control = list(
        trace = 6,
        maxit = 10000,
        ndeps = c(
            1e-7,
            1e-7,
            1e-7,
            1e-7
        )
    )
)
summary(fit_melExpGamma)
theta_melExpGamma$p = fit_melExpGamma@coef["p"]
theta_melExpGamma$lambda = fit_melExpGamma@coef["lambda"]
theta_melExpGamma$alpha = fit_melExpGamma@coef["alpha"]
theta_melExpGamma$beta = fit_melExpGamma@coef["beta"]
h_melExpGamma = hist(
    data,
    breaks = 40,
    probability = TRUE,
    main = "Fitting mixture of Exp and Gamma"
)
curve(
    f_melExpGamma(x),
    add = TRUE,
    col = "violet",
    from = min(h_melExpGamma$mids),
    to = max(h_melExpGamma$mids)
)