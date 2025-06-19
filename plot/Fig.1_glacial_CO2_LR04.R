rm(list = ls())
pacman::p_load(tidyverse, readxl, showtext)

# read data
benthic = read.csv("data/marine proxies/benthic_d18O.csv")
paleosol = read_xlsx("data/Dataset S1.xlsx", sheet = 2)
paleosol = paleosol[, c(1, 4, 17:19)] %>% drop_na()
names(paleosol) = c("site", "age", "CO2", "lower", "higher")
ice_core = read.csv("data/co2_compilation/ice_core_co2.csv")
blue_ice = read.csv("data/co2_compilation/blue.ice.csv")

# plot ----
pdf("figures/Fig_1_CO2_time_series.pdf", height = 5.4, width = 5)
par(mar = c(4, 4, 1, 4))
plot(-500, 0, xlim = c(0, 2700), ylim = c(0, 3), axes = FALSE,
     xlab = "", ylab = "")
box()
yext = range(benthic$d18O)
tix = seq(floor(min(yext)), ceiling(max(yext)-1), by = .5)
d18.rs = cbind(benthic$Age,
               3 - (benthic$d18O - min(tix)) / diff(range(tix)))
lines(d18.rs[,1], d18.rs[,2], col = "#F08841")
axis(4, 3 - (tix - min(tix)) / diff(range(tix)), tix, mgp = c(1, .7, 0))
mtext(expression("benthic "*delta^"18"*"O (\u2030)"), 4, line = 2, at = 2.5)

pal = c("#F08841", "#2B6A99")
site = pal[factor(paleosol$site, levels = c("Fuxian", "Zhaojiachuan"))]
tix = seq(150, 350, by = 50)
blue.rs = cbind(blue_ice$Age,
               1 + (blue_ice$CO2 - min(tix)) / diff(range(tix)))
ice.rs = cbind(ice_core$age,
               1 + (ice_core$co2 - min(tix)) / diff(range(tix)))
lines(ice.rs[,1], ice.rs[,2], col = "black")
points(blue.rs[,1], blue.rs[,2], col = "black", pch = 21, cex = 1.2)
axis(2, 1 + (tix - min(tix)) / diff(range(tix)), tix, mgp = c(1, .7, 0))
mtext(expression("CO"[2]*" (ppmv)"), 2, line = 2, at = 1.5)
abline(h = 1 + (300 - min(tix)) / diff(range(tix)), 
       col = "darkorange2", lty = 2, lwd = 2)
lines(c(800, 2700), 
      c(1 + (215 - min(tix)) / diff(range(tix)), 
        1 + (215 - min(tix)) / diff(range(tix))), 
      col = "darkblue", lty = 2, lwd = 2)
lines(c(0, 800), 
      c(1 + (180 - min(tix)) / diff(range(tix)), 
        1 + (180 - min(tix)) / diff(range(tix))), 
      col = "darkblue", lty = 2, lwd = 2)

yext = range(paleosol$lower, paleosol$higher)
tix = seq(100, 500, by = 100)
ice.rs = cbind(ice_core$age,
               0 + (ice_core$co2 - min(tix)) / diff(range(tix)))
paleosol.rs = cbind(paleosol$age,
                    0 + (paleosol$CO2 - min(tix)) / diff(range(tix)),
                    0 + (paleosol$lower - min(tix)) / diff(range(tix)),
                    0 + (paleosol$higher - min(tix)) / diff(range(tix)))
arrows(paleosol.rs[,1], paleosol.rs[,3],
       paleosol.rs[,1], paleosol.rs[,4],
       angle = 90, length = 0, code = 0, col = "grey80", lwd = .2)
points(paleosol.rs[,1], paleosol.rs[,2], bg = "white", col = site, pch = 21, cex = 1)
lines(ice.rs[,1], ice.rs[,2], col = "black")
abline(h = 0 + (300 - min(tix)) / diff(range(tix)), 
       col = "darkorange2", lty = 2, lwd = 2)
lines(c(800, 2700), 
      c(0 + (215 - min(tix)) / diff(range(tix)), 
        0 + (215 - min(tix)) / diff(range(tix))), 
      col = "darkblue", lty = 2, lwd = 2)
lines(c(0, 800), 
      c(0 + (180 - min(tix)) / diff(range(tix)), 
        0 + (180 - min(tix)) / diff(range(tix))), 
      col = "darkblue", lty = 2, lwd = 2)
axis(4, 0 + (tix - min(tix)) / diff(range(tix)), tix, mgp = c(1, .7, 0))
mtext(expression("CO"[2]*" (ppmv)"), 4, line = 2, at = .5)

fit = loess(CO2 ~ age, data = paleosol, span = .5)
x_smooth = seq(810, 2600, length.out = 200)
y_smooth = predict(fit, newdata = data.frame(age = x_smooth))
smooth.rs = cbind(x_smooth,
                  0 + (y_smooth - min(tix)) / diff(range(tix)))
lines(smooth.rs[,1], smooth.rs[,2], col = "royalblue", lwd = 5)

legend(x = 0, y = 1, legend = c("Fuxian", "Zhaojiachuan"),
       col = pal, pch = 21, pt.bg = "white", cex = .8, pt.cex = 1,
       bty = "n")

axis(1, mgp = c(1, .7, 0))
mtext("Age (Ma)", 1, line = 2)
text(x = 2500, y = 2.2, label = "a", font = 2)
text(x = 2500, y = 1.9, label = "b", font = 2)
text(x = 2500, y = 1, label = "c", font = 2)

dev.off()
