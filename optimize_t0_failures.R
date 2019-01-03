# Finding optimal t_0
# Given F_theta
c_c = 1200
c_p = 800
E_C_S = function(x) {
    (c_c - c_p) * F_theta(x) + c_p
}
E_S = function(x) {
    x - integrate(F_theta,0,x)$value
}
E_C = function(x) {
    E_C_S(x) / E_S(x)
}
o = optimize(
    E_C,
    c(min(data),max(data)),
    tol = 1e-5
)
d = seq(
        5,
        10,
        0.01
    )
plot(
    d,
    lapply(
        d,
        E_C
    ),
    main = "Coût moyenne sur une durée de temps",
    xlab = "t_0",
    ylab = "",
    type = "l"
)