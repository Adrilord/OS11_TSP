# Import degradation
table = read.csv(
    file = "DegradLevel_2.csv",
    header = TRUE,
    sep = ",",
    dec = "."
)
scale = 1000
time = c(0, table$Temps / scale)
nbProcess = length(table) - 2
process = matrix(
    ,
    nrow = nbProcess,
    ncol = 1 + length(table[[3]])
)
for (i in 1:nbProcess) {
    process[i,] = c(0, table[[i + 2]]) # degrad = 0 at t = 0
}