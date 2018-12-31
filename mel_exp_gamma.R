## Does not work
# Fitting mixture of Exp and Gamma
theta_melExpGamma = list(
    p = 0.5,
    lambda = 1 / sum(data),
    alpha = 5,
    beta = 1
)
# PDF
f_melExpGamma = function(x, theta = theta_melExpGamma) {
    theta$p * dexp(data, rate = theta$p) + (1 - theta$p) * dgamma(data, shape = theta$alpha, rate = theta$beta)
}
# CDF
F_melExpGamma = function(x, theta = theta_melExpGamma) {
    theta$p * pexp(data, rate = theta$p) + (1 - theta$p) * pgamma(data, shape = theta$alpha, rate = theta$beta)
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
        1e-4,
        4.5,
        1e-4
    ),
    upper = c(
        1,
        Inf,
        5.5,
        Inf
    ),
    control = list(
        trace = 6,
        maxit = 1000,
        ndeps = c(
            1e-5,
            1e-5,
            1e-5,
            1e-5
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
n_breaks = length(h_melExpGamma$breaks)
distance_melExpGamma = density_distance(F_melExpGamma, h_melExpGamma)