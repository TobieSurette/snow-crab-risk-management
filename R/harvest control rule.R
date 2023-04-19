library(gulf.data)
library(gulf.graphics)

language <- language("fr")
#language <- "bilingual"

height <- 5
width <- 7

biomass <- 85532

clg()
tiff(file = paste0("results/figures/harvest control rule fab - ", language, ".tiff"), compression = "lzw", units = "in", res = 300, height = height, width = width)

x <- seq(0, 150000, len = 1000)
y <- harvest.control.rule(x, species = "snow crab")

ER <- harvest.control.rule(biomass, species = "snow crab")

plot(range(x) / 1000, 100 * c(0, 0.5),
     type = "n", xlab = "", ylab = "", xaxs = "i", yaxs = "i")
grid()
lines(x / 1000, 100 * y, lwd = 2)
box()

if (language == "english"){
   mtext("Commercial biomass (x1000 t)", 1, 2.5, cex = 1.0)
   mtext("Exploitation rate (%)", 2, 2.5, cex = 1.0)
}
if (language == "french"){
   mtext("Biomasse commerciale (x1000 t)", 1, 2.5, cex = 1.0)
   mtext("Taux d'exploitation (%)", 2, 2.5, cex = 1.0)
}
if (language == "bilingual"){
   mtext("Commercial biomass / Biomasse commerciale (x1000 t)", 1, 2.5, cex = 1.0)
   mtext("Exploitation rate / Taux d'exploitation (%)", 2, 2.5, cex = 1.0)
}

rp <- reference.points("snow crab")

Blim <- rp[["Blim"]]  # Recovery biomass limit reference pont.
Busr <- rp[["Busr"]]  # Upper stock reference biomass.
Flim <- rp[["Flim"]]  # Limit reference point for fishing removal rate.

vline(Blim / 1000, lwd = 2, col = "red")
vline(Busr / 1000, lwd = 2, col = "green")
#hline(100 * Flim, lower = Busr / 1000, col = "blue", lwd = 2)

#if (language == "french")  text(Blim / 1000 + 2.5, 25, expression('B'[lim]*' = 10 000 t'), pos = 1, cex = 1.1, srt = 90)
#if (language == "english") text(Blim / 1000 + 2.5, 25, expression('B'[lim]*' = 10,000 t'), pos = 1, cex = 1.1, srt = 90)
if (language == "french")  text(Blim / 1000 + 2.5, 25, 'PRL = 10 000 t', pos = 1, cex = 1.1, srt = 90)
if (language == "english") text(Blim / 1000 + 2.5, 25, 'LRP = 10 000 t', pos = 1, cex = 1.1, srt = 90)

#text(125, 100 * Flim, expression('F'[lim]*' = 34.6%'), pos = 3, cex = 1.1)
text(125, 100 * 0.45, expression('F'[max]*' = 45%'), pos = 3, cex = 1.1)
#if (language == "french")  text(Busr / 1000 + 2.5, 20, expression('B'[prs]*' = 41 400 t'), pos = 1, cex = 1.1, srt = 90)
#if (language == "english") text(Busr / 1000 + 2.5, 20, expression('B'[usr]*' = 41,400 t'), pos = 1, cex = 1.1, srt = 90)
if (language == "french")  text(Busr / 1000 + 2.5, 20, 'PRS = 41 400 t', pos = 1, cex = 1.1, srt = 90)
if (language == "english") text(Busr / 1000 + 2.5, 20, 'USR = 41 400 t', pos = 1, cex = 1.1, srt = 90)

lines(rep(biomass/1000, 2), c(0, 100 * ER), lty = "dashed", lwd = 2)
lines(c(0, biomass/1000), rep(100 * ER,2), lty = "dashed", lwd = 2)
if (language == "french") text(biomass/1000 + 2.5, 50 * ER, expression('B'[2023]*' = 85 532 t'), pos = 1, srt = 90)
if (language == "english") text(biomass/1000 + 2.5, 50 * ER, expression('B'[2023]*' = 85,532 t'), pos = 1, srt = 90) 
text(0.5*biomass/1000, 100*ER, expression('F'[2023]*' = 41.79%'), pos = 3, srt = 0)

dev.off()

