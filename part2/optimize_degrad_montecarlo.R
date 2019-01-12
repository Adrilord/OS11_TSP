# Monte carlo to optimize degradation
set.seed(2019)
L = 20
cost = c(10, 800, 1200) # Inspection, Preventive, Corrective
C_S = function(
    p,
    M
) {
    c = 0
    p = p[!is.na(p)]
    len = length(p)
    if (p[[len]] >= M) {
        n_s = length(p[p < M]) # Last index before >= M
        c = c + cost[[1]] * n_s # Inspection
        if (p[[n_s + 1]] >= L) {
            c = c + cost[[3]] # Corrective
        } else {
            c = c + cost[[2]] # Preventive
        }
    }
    return(c)
}
S = function(
    p,
    deltaT,
    M
) {
    p = p[!is.na(p)]
    len = length(p)
    if (p[[len]] < M) return(0)
    else return(length(p[p < M]) + 1)
}
E_C = function(
    deltaT,
    M,
    nSim = 200,
    nStep = floor(13 / deltaT),
    silence = TRUE
) {
    simStep = matrix(
        rG(nSim * nStep, deltaT),
        nrow = nSim,
        ncol = nStep
    )
    simProc = t(apply(
        t(simStep),
        2,
        cumsum
    ))
    # Count cost
    simCost = apply(
        t(simProc),
        2,
        C_S,
        M
    )
    # Count time
    simTime = apply(
        t(simProc),
        2,
        S,
        deltaT,
        M
    )
    meanSimCost = mean(simCost[simCost > 0])
    meanSimTime = mean(simTime[simTime > 0])
    meanCostTime = 0
    if (is.nan(meanSimCost) || is.nan(meanSimTime)) meanCostTime = .Machine$double.xmax
    else meanCostTime = meanSimCost / meanSimTime
    # For debug purpose
    if (!silence)
        cat(
            "dT =", deltaT,
            "M =", M,
            "mCost =", meanSimCost,
            "mTime =", meanSimTime,
            "mCostTime =", meanCostTime,
            "\n"
        )
    return(meanCostTime)
}
# Optimize
optim_degrad = function(
    lower_deltaT,
    upper_deltaT,
    lower_M,
    upper_M,
    tol = 1e-2,
    div_deltaT = 5,
    div_M = 5,
    cut_deltaT = 2,
    cut_M = 2
){
    deltaT = 0
    M = 0
    obj = 0
    while ((((upper_deltaT - lower_deltaT) / div_deltaT)^2 + ((upper_M - lower_M) / div_M)^2) >= tol^2) {
        interval_deltaT = seq(lower_deltaT, upper_deltaT, length.out = div_deltaT)
        interval_M = seq(lower_M, upper_M, length.out = div_M)
        cat("deltaT =", interval_deltaT,
            "\nM =", interval_M,
            "\n")
        Z = outer(
            interval_deltaT,
            interval_M,
            Vectorize(E_C)
        )
        # First minimum position
        posMin = arrayInd(which.min(Z), dim(Z))
        deltaT = interval_deltaT[[posMin[[1]]]]
        M = interval_M[[posMin[[2]]]]
        obj = Z[posMin]
        cat("deltaT = ", deltaT,
            "\nM = ", M,
            "\nobj = ", obj,
            "\n")
        # Adjust zone of optimize
        l_deltaT = upper_deltaT - lower_deltaT
        l_M = upper_M - lower_M
        upper_deltaT = min(deltaT + l_deltaT / cut_deltaT, upper_deltaT)
        lower_deltaT = max(deltaT - l_deltaT / cut_deltaT, lower_deltaT)
        upper_M = min(M + l_M / cut_M, upper_M)
        lower_M = max(M - l_M / cut_M, lower_M)
    }
    return(list(
        deltaT = deltaT,
        M = M,
        obj = obj
    ))
}
val = optim_degrad(0.001,4,0,20)
# Illustration
interval_deltaT = seq(0.1, 4.1, 0.5)
interval_M = seq(1, 20, 1)
Z = outer(
    interval_deltaT,
    interval_M,
    Vectorize(E_C)
)
library(rgl)
persp3d(
    interval_deltaT,
    interval_M,
    Z,
    xlab = "deltaT",
    ylab = "M",
    zlab = "E_C",
    zlim = c(0, 400),
    col = "lightblue"
)
rgl.snapshot(filename = "img/E_C_degrad.png")
