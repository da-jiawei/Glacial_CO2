rm(list = ls())
pacman::p_load(tidyverse, readxl)

# read data ----
co2 = read.csv("output/binned_co2.csv")
ice_co2 = co2 |> 
  filter(method == "ice_core", age <= 800)
proxy_co2 = co2 |> 
  filter(age > 800)
gmst_clark = read.csv("data/GMST/clark_2024.csv") |>
  mutate(age = age * 1e3) |>
  filter(age <= 2600)
R_LI = read.csv("data/R_LI_koehler2015.csv") |>
  mutate(age = Age) |> 
  filter(age <= 2600)

# time-series plot ----
pal = c("#A6CEE3", "#1F78B4", "#B2DF8A", "#33A02C", "#FB9A99", "#E31A1C")
method = pal[factor(proxy_co2$method, levels = c("paleosol", "boron", "ice"))]

png("figures/time_series_climate_sensitivity.png", 4, 4.5, units = "in", res = 300)
par(mar = c(4, 4, 1, 4))
graphics::plot(-0.3, 0, xlim = c(0, 2.7), ylim = c(0, 3), axes = FALSE,
               xlab = "", ylab = "")

yext = range(gmst_clark$GMST)
tix = seq(floor(min(yext)), 
          ceiling(max(yext) + 1.2), by = 4)
gmst.rs = cbind(gmst_clark$age / 1e3,
                2 + (gmst_clark$GMST - min(tix)) / diff(range(tix)))
gmst.rs = gmst.rs[order(gmst.rs[, 1]), ]
lines(gmst.rs[, 1], gmst.rs[, 2], col = pal[3], lwd = 1)
axis(2, 2 + (tix - min(tix)) / diff(range(tix)), tix)
mtext(expression(paste(Delta*"GMST (", degree,"C)")), 2, line = 2.5, at = 2.5)

yext = range(R_LI$RLI)
tix = seq(floor(min(yext)), 
          ceiling(max(yext)), by = 2)
R_LI.rs = cbind(R_LI$age / 1e3,
                1 + (R_LI$RLI - min(tix)) / diff(range(tix)))
R_LI.rs = R_LI.rs[order(R_LI.rs[, 1]), ]
lines(R_LI.rs[, 1], R_LI.rs[, 2], col = pal[5], lwd = 1)
axis(4, 1 + (tix - min(tix)) / diff(range(tix)), tix)
mtext(expression(Delta*"R"[LI]*" (W m"^"-2"*")"), 4, line = 2.5, at = 1.5)

yext = range(co2$CO2)
tix = seq(floor(min(yext)-39), 
          ceiling(max(yext)+50), by = 100)
ice.rs = cbind(ice_co2$age / 1e3,
               0 + (ice_co2$CO2 - min(tix)) / diff(range(tix)))
proxy.rs = cbind(proxy_co2$age / 1e3,
                 0 + (proxy_co2$CO2 - min(tix)) / diff(range(tix)))
ice.rs = ice.rs[order(ice.rs[, 1]), ]
lines(ice.rs[, 1], ice.rs[, 2], col = "black", lwd = 1)
points(proxy.rs[, 1], proxy.rs[, 2], col = "black", bg = method, pch = 21, cex = 0.8)
axis(2, 0 + (tix - min(tix)) / diff(range(tix)), tix)
mtext(expression("CO"[2]*" (ppmv)"), 2, line = 2.5, at = 0.5)

axis(1)
mtext("Age (Ma)", 1, line = 2)
dev.off()
