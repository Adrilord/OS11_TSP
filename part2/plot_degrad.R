# Plot process curves
L = 20
# Colormap
library(RColorBrewer)
color = brewer.pal(nbProcess, "Paired")
# First process
plot(
    NULL,
    type = "n",
    main = "Traçe de dégradation du système",
    xlim = c(min(time), max(time)),
    xlab = "Temps (mille heures)",
    ylim = c(0, L),
    ylab = "Niveau de dégradation"
)
for (i in 1:nbProcess) {
    lines(
        x = time,
        y = process[i,],
        type = "s",
        col = color[[i]],
        lwd = 2
    )
}