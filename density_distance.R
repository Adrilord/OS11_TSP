density_distance = function(
    F_X, # CDF with one parameter
    H # histogramme of data
) {
    n = length(H$breaks)
    sum(abs((F_X(H$breaks[2:n]) - F_X(H$breaks[1:n - 1])) - H$density))
}