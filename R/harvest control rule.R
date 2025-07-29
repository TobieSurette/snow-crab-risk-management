library(gulf.data)
library(gulf.graphics)

language <- language("en")
#language <- "bilingual"
FSAR <- TRUE

year <- 2024
biomass <- 51786 
ER  <- harvest.control.rule(biomass, species = 2526)

clg()
png(file = paste0("results/figures/harvest control rule FSAR ", language, ".png"), 
    units = "in", res = 500, height = 5, width = 7)

x <- seq(0, 150000, len = 1000)
y <- harvest.control.rule(x, species = "snow crab")

ER <- harvest.control.rule(biomass, species = "snow crab")

plot(range(x) / 1000, 100 * c(0, 0.5),
     type = "n", xlab = "", ylab = "", xaxs = "i", yaxs = "i")
grid()

r <- reference.points("snow crab")
b <- (1-r[["Flim"]]) * r[["Busr"]]
b[2] <- 35929 # 2010 Commercial biomass.
b[3] <- 1.1 * (0.5 * r[["Bmax"]] * 0.8)
b[4] <- r[["Bmax"]]
vline(b / 1000, upper = 100 * harvest.control.rule(b, species = "snow crab"), lower = 0, 
      lty = "dotted", col = "blue2")
mtext(round(b / 1000, 1), 1, at =  round(b / 1000, 1), font = 2, col = "blue", cex = 0.8)

cex <- 1.1
if (language == "english"){
   mtext("Commercial biomass (x 1000 t)", 1, 2.5, cex = cex, font = 2)
   mtext("Exploitation rate (%)", 2, 2.5, cex = cex, font = 2)
}
if (language == "french"){
   mtext("Biomasse commerciale (x 1000 t)", 1, 2.5, cex = cex, font = 2)
   mtext("Taux d'exploitation (%)", 2, 2.5, cex = cex, font = 2)
}
if (language == "bilingual"){
   mtext("Commercial biomass / Biomasse commerciale (x 1000 t)", 1, 2.5, cex = cex, font = 2)
   mtext("Exploitation rate / Taux d'exploitation (%)", 2, 2.5, cex = cex, font = 2)
}

rp <- reference.points("snow crab")

Blim <- rp[["Blim"]]  # Recovery biomass limit reference pont.
Busr <- rp[["Busr"]]  # Upper stock reference biomass.
Flim <- rp[["Flim"]]  # Limit reference point for fishing removal rate.

if (!FSAR){
   vline(Blim / 1000, lwd = 2, col = "red2")
   vline(Busr / 1000, lwd = 2, col = "green2")
   
   #hline(100 * Flim, lower = Busr / 1000, col = "blue", lwd = 2)
   
   #if (language == "french")  text(Blim / 1000 + 2.5, 25, expression('B'[lim]*' = 10 000 t'), pos = 1, cex = 1.1, srt = 90)
   #if (language == "english") text(Blim / 1000 + 2.5, 25, expression('B'[lim]*' = 10,000 t'), pos = 1, cex = 1.1, srt = 90)
   if (language == "french")  text(Blim / 1000 + 2.5, 25, 'PRL = 10 000 t', pos = 1, cex = 1.1, srt = 90)
   if (language == "english") text(Blim / 1000 + 2.5, 25, 'LRP = 10,000 t', pos = 1, cex = 1.1, srt = 90)
   
   if (language == "french")  text(Busr / 1000 + 2.5, 20, 'PRS = 41 400 t', pos = 1, cex = 1.1, srt = 90)
   if (language == "english") text(Busr / 1000 + 2.5, 20, 'USR = 41,400 t', pos = 1, cex = 1.1, srt = 90)
}

# Represent reference point zones: 
polygon(c(0, Blim / 1000, Blim / 1000, 0), 
        c(par("usr")[3], par("usr")[3], par("usr")[4], par("usr")[4]),
        col = fade("red", 0.2), border = NA) 
polygon(c(Blim / 1000, Busr / 1000, Busr / 1000, Blim / 1000), 
        c(par("usr")[3], par("usr")[3], par("usr")[4], par("usr")[4]),
        col = fade("yellow", 0.2), border = NA) 
polygon(c(Busr / 1000, par("usr")[2], par("usr")[2], Busr / 1000), 
        c(par("usr")[3], par("usr")[3], par("usr")[4], par("usr")[4]),
        col = fade("green", 0.2), border = NA) 

if (language == "english"){
   axis(3, at = Blim / 1000, labels = "LRP", padj = 0.5)
   axis(3, at = Busr / 1000, labels = "USR", padj = 0.5)
   
   mtext("Critical zone", 2, -1.4, at = mean(par("usr")[3:4]), font = 2, col = "red3", cex = 0.80)
   mtext("Cautious zone", 3, -1.0, at = (Blim + Busr) / 2000, font = 2, col = "yellow3", cex = 0.80)
   mtext("Healthy zone", 3, -1.0, at = (Busr / 1000 + par("usr")[2]) / 2, font = 2, col = "green3", cex = 0.80)
}
if (language == "french"){
   axis(3, at = Blim / 1000, labels = "PRL", padj = 0.5)
   axis(3, at = Busr / 1000, labels = "PRS", padj = 0.5) 

   mtext("Zone critique", 2, -1.4, at = mean(par("usr")[3:4]), font = 2, col = "red3", cex = 0.80)
   mtext("Zone précaution", 3, -1.0, at = (Blim + Busr) / 2000, font = 2, col = "yellow3", cex = 0.80)
   mtext("Zone saine", 3, -1.0, at = (Busr / 1000 + par("usr")[2]) / 2, font = 2, col = "green3", cex = 0.80)
}

vline(Blim / 1000, lwd = 1, col = "red2")
vline(Busr / 1000, lwd = 1, col = "green2")

# Harvest control rule:
lines(x / 1000, 100 * y, col = "blue2", lwd = 2)

#text(125, 100 * Flim, expression('F'[lim]*' = 34.6%'), pos = 3, cex = 1.1)
text(125, 100 * 0.45, expression('F'[max]*' = 45%'), pos = 3, cex = 1.1)
#if (language == "french")  text(Busr / 1000 + 2.5, 20, expression('B'[prs]*' = 41 400 t'), pos = 1, cex = 1.1, srt = 90)
#if (language == "english") text(Busr / 1000 + 2.5, 20, expression('B'[usr]*' = 41,400 t'), pos = 1, cex = 1.1, srt = 90)

lines(rep(biomass/1000, 2), c(0, 100 * ER), lty = "dashed", lwd = 2, col = "blue2")
lines(c(0, biomass/1000), rep(100 * ER,2), lty = "dashed", lwd = 2, col = "blue2")
if (language == "french") text(biomass/1000 + 2.5, 50 * ER, expression('B'[2025]*' = 51 786 t'), pos = 1, srt = 90)
if (language == "english") text(biomass/1000 + 2.5, 50 * ER, expression('B'[2025]*' = 51,786 t'), pos = 1, srt = 90) 
text(0.5*biomass/1000, 100*ER, expression('F'[2025]*' = 35.73%'), pos = 3, srt = 0)

box(col = "grey50")

dev.off()

