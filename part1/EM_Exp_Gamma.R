# Fitting mixture of Exp and Gamma
# Algorithm EM
# Initialisation
k = 2 # number of components
p = c(0.5, 0.5)
lambda = 1
alpha = 10
beta = 2
f = list(
    '1' = function(x) {
        dexp(x, rate = lambda)
    },
    '2' = function(x) {
        dgamma(x, shape = alpha, rate = beta)
    }
)
# Illustration initial
f_theta = function(x) {
  p[[1]] * f[[1]](x) + p[[2]] * f[[2]](x)
}
h_theta = hist(
  data,
  breaks = 40,
  probability = TRUE,
  main = "Mixage de la loi Exponentielle et Gamma",
  xlab = "Dates de panne (mille d'heures)",
  ylab = "Densité"
)
curve(
  f_theta(x),
  add = TRUE,
  col = "violet",
  from = min(h_theta$mids),
  to = max(h_theta$mids)
)
epsilon = list(
    alpha = 1e-4,
    theta = 1e-4
)
zeta = matrix(
    0,
    nrow = k,
    ncol = N
)
# Norm
normVec = function(x) sqrt(sum(x^2))
# New value
p_new = p
alpha_new = alpha
beta_new = beta
lambda_new = lambda
repeat {
    ## E Step
    # Calculate each proba
    for (l in 1:k) {
        zeta[l,] = p[[l]] * f[[l]](data)
    }
    # Normalize proba
    zeta = t(t(zeta) / rowSums(t(zeta)))
    ## M step
    # Lambda
    lambda_new = sum(zeta[1,]) / sum(zeta[1,] * data)
    # Alpha
    c = log(sum(zeta[2,] * data) / sum(zeta[2,])) - sum(zeta[2,] * log(data)) / sum(zeta[2,])
    alpha_new = 0.5 / c
    alpha_temp = 0
    repeat {
        alpha_temp = 1 / (1 / alpha_new + (log(alpha_new) - digamma(alpha_new) - c) / (alpha_new^2 * (1 / alpha_new - trigamma(alpha_new))))
        if (abs(alpha_temp - alpha_new) < epsilon$alpha) {
            break
        } else {
            alpha_new = alpha_temp
        }
    }
    alpha_new = alpha_temp
    # Beta
    beta_new = alpha_new * sum(zeta[2,]) / sum(zeta[2,] * data)
    # P
    for (l in 1:k) {
        p_new[[l]] = mean(zeta[l,])
    }
    ## Evaluation
    if (normVec(c(alpha, beta, lambda, p[[1]], p[[2]]) - c(alpha_new, beta_new, lambda_new, p_new[[1]], p_new[[2]])) < epsilon$theta) {
        break
    } else {
        alpha = alpha_new
        beta = beta_new
        lambda = lambda_new
        p = p_new
    }
}
# Final value update
alpha = alpha_new
beta = beta_new
lambda = lambda_new
p = p_new
# Illustration final
f_theta = function(x) {
  p[[1]] * f[[1]](x) + p[[2]] * f[[2]](x)
}
h_theta = hist(
  data,
  breaks = 40,
  probability = TRUE,
  main = "Mixage de la loi Exponentielle et Gamma",
  xlab = "Dates de panne (mille d'heures)",
  ylab = "Densité"
)
curve(
  f_theta(x),
  add = TRUE,
  col = "violet",
  from = min(h_theta$mids),
  to = max(h_theta$mids)
)
# Kolmogorov-Smirnov test
F_theta = function(x) {
    p[[1]] * pexp(x, rate = lambda) + p[[2]] * pgamma(x, shape = alpha, rate = beta)
}
test = ks.test(data, F_theta, exact = TRUE)