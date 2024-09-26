library(gulf.data)
library(gulf.graphics)

# Get reference points:
rp <- reference.points(2526)
language <- language("fr")

# Define current year's biomass estimates:
BMMGE95.mu    <- 51786  # Kriged commercial biomass.
BMMGE95.sigma <- 3333.7 # Standard error of kriged commercial biomass
ER  <- harvest.control.rule(BMMGE95.mu, species = 2526)

Blim <- rp[["Blim"]]    # Recovery biomass limit reference pont.
Busr <- rp[["Busr"]]    # Upper stock reference biomass.
Flim <- rp[["Flim"]]    # Limit reference point for fishing removal rate.
if (length(language) == 1) language.str <- language else language.str <- "bilingual"

# Define axis labels:
xlab <- c("Commercial biomass", "Biomasse commerciale")
ylab <- c("Exploitation rate", "Taux d'exploitation")
names(xlab) <- names(ylab) <- c("english", "french")

# Use only specified languages:
xlab <- xlab[language]
ylab <- ylab[language]
xlab <- paste(xlab, collapse = " / ")
ylab <- paste(ylab, collapse = " / ")
xlab <- paste0(xlab, " (x 1000t)")
ylab <- paste0(ylab, " (%)")

if (language == "english") Busr.str <- expression('B'[usr]*' = 41400 t')
if (language == "french")  Busr.str <- expression('B'[nrs]*' = 41400 t')

# Data:
x <- data.frame(year = 1997:2024,
                landings = c(17.66, 13.86, 15.52, 19.18, 18.51, 26.18, 21.16, 31.66, 36.08, 29.12, 26.87, 24.46,
                             23.64, 9.549, 10.71, 21.96, 26.05, 24.44, 25.91, 21.71, 43.656, 24.260, 31.707, 28.156, 24.489,
                             31.661, 35.422, 26.126),  
                com = c(64.5184, 64.5184, 57.8125, 56.7565, 50.621, 60.3283, 79.2275, 84.4475, 103.1457, 82.5652,
                        73.6453, 66.3714, 52.9209, 31.0153, 35.9294, 62.8407, 74.7775, 66.709, 67.9896, 58.9269,
                        98.3942, 65.7376, 80.746, 79.06550, 77.748, 80.950, 85.532, 67.703),
                com.sigma = c(5.6785, 5.6785, 6.6617, 4.9687, 4.8133, 5.7457, 6.0774, 5.8931, 5.699, 4.8234, 4.2417, 3.3922, 3.0653,
                              1.8656, 2.0665, 3.6529, 5.3264, 6.8484, 4.3836, 4.0608, 6.0042, 4.578, 5.302936, 5.364855, 5.397385, 5.5918,
                              5.83920, 4.5777))

# Commercial confidence intervals:
x$com.lci <- x$com - 1.96 * x$com.sigma
x$com.uci <- x$com + 1.96 * x$com.sigma

# Exploitation rate:
x$er.lci <- x$landings / x$com.uci
x$er <- x$landings  / x$com
x$er.uci <- x$landings / x$com.lci

x <- x[-1, ]

png(file = paste0("results/figures/reference point figure alternative ", max(x$year), " - ", language.str, ".png"),
    res = 500, units = "in", height = 5, width = 7)

# Base plot:
plot(c(0, 120), c(0, 50), type ="n", xlab = "", ylab = "", xaxs = "i", yaxs = "i", xaxt = "n", cex.axis = 0.8)
grid()


rp <- reference.points("snow crab")

Blim <- rp[["Blim"]]  # Recovery biomass limit reference pont.
Busr <- rp[["Busr"]]  # Upper stock reference biomass.
Flim <- rp[["Flim"]]  # Limit reference point for fishing removal rate.

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
   #axis(4, at = 34.6, labels = "Flim", padj = -0.5, las = 3)
  
   mtext("Critical zone", 2, -1.4, at = mean(par("usr")[3:4]), font = 2, col = "red3", cex = 0.80)
   mtext("Cautious zone", 3, -1.0, at = (Blim + Busr) / 2000, font = 2, col = fade("yellow4", 0.8), cex = 0.80)
   mtext("Healthy zone", 3, -1.0, at = (Busr / 1000 + par("usr")[2]) / 2, font = 2, col = "green3", cex = 0.80)
}
if (language == "french"){
   axis(3, at = Blim / 1000, labels = "PRL", padj = 0.5)
   axis(3, at = Busr / 1000, labels = "PRS", padj = 0.5) 
   #axis(4, at = 34.6, labels = "Flim", padj = -0.5, las = 3)
   
   mtext("Zone critique", 2, -1.4, at = mean(par("usr")[3:4]), font = 2, col = "red3", cex = 0.80)
   mtext("Zone précaution", 3, -1.0, at = (Blim + Busr) / 2000, font = 2, col = fade("yellow4", 0.8), cex = 0.80)
   mtext("Zone saine", 3, -1.0, at = (Busr / 1000 + par("usr")[2]) / 2, font = 2, col = "green3", cex = 0.80)
}

# Confidence interval lines:
for (i in 1:nrow(x)){
   lines(c(x$com.lci[i], x$com.uci[i]), 100 * c(x$er[i], x$er[i]), col = "grey30")
   lines(c(x$com[i], x$com[i]), 100 * c(x$er.lci[i], x$er.uci[i]), col = "grey30")
}

vline(Blim / 1000, lwd = 1, col = "red2")
vline(Busr / 1000, lwd = 1, col = "green2")

# Reference points:
#vline(Blim / 1000, lwd = 2.5, col = "red")
#vline(Busr / 1000, lwd = 2.5, col = "green3")
#hline(100 * Flim, lower = Busr / 1000, col = "blue2", lwd = 1.5)
#text(Blim / 1000 + 1.25, 10, expression('B'[lim]*' = 10000 t'), pos = 1, cex = 1.00, srt = 90)
#text(Busr / 1000 + 1.25, 10, Busr.str, pos = 1, cex = 1.00, srt = 90)
#text(108, 1 + 100 * Flim, expression('F'[lim]*' = 34.6%'), pos = 3, cex = 1.00)

# Plot points and annotations:
pos <- c(4, 4, 3, 2, 1, 4, 1, 4, 4, 4, 1, 2, 3, 4, 2, 1, 1, 2, 2, 1, 2, 1, 1, 4)
points(x$com, 100 * x$er, pch = 21, cex = 1.25, bg = "grey")
for (i in 1:length(pos)){
   if (pos[i] == 1) delta <- c(1,1)
   if (pos[i] == 2) delta <- c(-1,1)
   if (pos[i] == 3) delta <- c(-1,-1)
   if (pos[i] == 4) delta <- c(1,-1)
   delta[1] <- 4 * delta[1]
   delta[2] <- 2.5 * delta[2]
   text(x$com[i] + delta[1], 100 * x$er[i] + delta[2], x$year[i], cex = 0.75, offset = 0)
}

#arrows(x$com[nrow(x)]+5, 5, x$com[nrow(x)], 0, len = 0.1, lwd = 2)
#text(x$com[nrow(x)]+6.5, 6.5, x$year[nrow(x)] + 1)

axis(1, at = seq(0, 120, by = 20), cex.axis = 0.8)
axis(1, at = seq(10, 110, by = 20), cex.axis = 0.8)

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

lines(c(BMMGE95.mu - 1.96 * BMMGE95.sigma, BMMGE95.mu + 1.96 * BMMGE95.sigma) / 1000, 100 * c(ER, ER), lwd = 2, col = "red2")
points(BMMGE95.mu / 1000, 100 * ER, pch = 22, cex = 1.75, bg = "red2", col = "grey50")


box()

dev.off()

