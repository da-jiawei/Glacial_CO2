dat = read.csv("output/glacial_co2_estimates.csv")
dat = dat[, 2:ncol(dat)]
pal = c("#A6CEE3", "#B2DF8A")
site = pal[factor(dat$section, levels = c("Zhaojiachuan", "Fuxian"))]

# input parameters ----
# pdf("figures/time_series_input_parameters.pdf", width = 3.8, height = 5)
png("figures/time_series_derived_parameters.png", 4.4, 6, units = "in", res = 300)
par(mar = c(4, 4, 1, 4))
plot(0, 0, xlim = c(800, 2700), ylim = c(0, 4), axes = FALSE,
     xlab = "", ylab = "")

yext = range(dat$d13c)
tix = seq(floor(min(yext)), 
          ceiling(max(yext)), by = 1)
d13c.rs = cbind(dat$age,
                3 + (dat$d13c - min(tix)) / diff(range(tix)))
points(d13c.rs[, 1], d13c.rs[, 2], col = "black", bg = site, pch = 21, cex = 1)
axis(2, 3 + (tix - min(tix)) / diff(range(tix)), tix)
mtext(expression(delta^"13"*"C"[c]*" (\u2030)"), 2, line = 2.5, at = 3.5)

yext = range(dat$d18c)
tix = seq(floor(min(yext)), 
          ceiling(max(yext)), by = 1)
d18c.rs = cbind(dat$age,
                2 + (dat$d18c - min(tix)) / diff(range(tix)))
points(d18c.rs[, 1], d18c.rs[, 2], col = "black", bg = site, pch = 22, cex = 1)
axis(4, 2 + (tix - min(tix)) / diff(range(tix)), tix)
mtext(expression(delta^"18"*"O"[c]*" (\u2030)"), 4, line = 2.5, at = 2.5)

yext = range(dat$d13o)
tix = seq(floor(min(yext)), 
          ceiling(max(yext)), by = 1)
d13o.rs = cbind(dat$age,
                1 + (dat$d13o - min(tix)) / diff(range(tix)))
points(d13o.rs[, 1], d13o.rs[, 2], col = "black", bg = site, pch = 21, cex = 1)
axis(2, 1 + (tix - min(tix)) / diff(range(tix)), tix)
mtext(expression(delta^"13"*"C"[o]*" (\u2030)"), 2, line = 2.5, at = 1.5)

yext = range(dat$d13a)
tix = seq(floor(min(yext)), 
          ceiling(max(yext)), by = 1)
d13a.rs = cbind(dat$age,
                0 + (dat$d13a - min(tix)) / diff(range(tix)))
points(d13a.rs[, 1], d13a.rs[, 2], col = "black", bg = site, pch = 22, cex = 1)
axis(4, 0 + (tix - min(tix)) / diff(range(tix)), tix)
mtext(expression(delta^"13"*"C"[a]*" (\u2030)"), 4, line = 2.5, at = 0.5)

axis(1, at = seq(800, 2700, 100), labels = FALSE)
text(x = seq(800, 2700, 200), y = -0.28, labels = seq(800, 2700, 200), srt = 40, xpd = TRUE, adj = 1)
mtext("Age (ka)", 1, line = 2)
legend(x = 800, y = 4.1, legend = c("Zhaojiachuan", "Fuxian"),
       col = pal, pch = 16, cex = 0.6, pt.cex = 1)
dev.off()

# derived parameters ----
png("figures/time_series_derived_parameters.png", 4.4, 6, units = "in", res = 300)
par(mar = c(4, 4, 1, 4))
plot(0, 0, xlim = c(800, 2700), ylim = c(0, 4), axes = FALSE,
     xlab = "", ylab = "")

dat = dat %>%
  mutate(d13s.low = d13s - d13s.sd,
         d13s.high = d13s + d13s.sd)
yext = range(dat$d13s.low, dat$d13s.high)
tix = seq(floor(min(yext)), 
          ceiling(max(yext)), by = 1)
d13s.rs = cbind(dat$age,
                3 + (dat$d13s - min(tix)) / diff(range(tix)),
                3 + (dat$d13s.low - min(tix)) / diff(range(tix)),
                3 + (dat$d13s.high - min(tix)) / diff(range(tix)))
arrows(d13s.rs[, 1], d13s.rs[, 3], d13s.rs[, 1], d13s.rs[, 4], col = "black",
       angle=90, length=0, code = 0, lwd = 0.5)
points(d13s.rs[, 1], d13s.rs[, 2], col = "black", bg = site, pch = 21, cex = 1)
axis(2, 3 + (tix - min(tix)) / diff(range(tix)), tix)
mtext(expression(delta^"13"*"C"[s]*" (\u2030)"), 2, line = 2.5, at = 3.5)

dat = dat %>%
  mutate(Sz.low = Sz - Sz.sd,
         Sz.high = Sz + Sz.sd)
yext = range(dat$Sz.low, dat$Sz.high)
tix = seq(floor(min(yext)-2), 
          ceiling(max(yext)), by = 200)
Sz.rs = cbind(dat$age,
              2 + (dat$Sz - min(tix)) / diff(range(tix)),
              2 + (dat$Sz.low - min(tix)) / diff(range(tix)),
              2 + (dat$Sz.high - min(tix)) / diff(range(tix)))
arrows(Sz.rs[, 1], Sz.rs[, 3], Sz.rs[, 1], Sz.rs[, 4], col = "gray",
       angle=90, length=0, code = 0, lwd = 0.5)
points(Sz.rs[, 1], Sz.rs[, 2], col = "black", bg = site, pch = 22, cex = 1)
axis(4, 2 + (tix - min(tix)) / diff(range(tix)), tix)
mtext(expression("S"[(z)]*" (ppmv)"), 4, line = 2.5, at = 2.5)

dat = dat %>%
  mutate(R.low = R - R.sd,
         R.high = R + R.sd)
yext = range(dat$R.low, dat$R.high)
tix = seq(floor(min(yext)*10), 
          ceiling(max(yext)*10), by = 2) / 10
R.rs = cbind(dat$age,
             1 + (dat$R - min(tix)) / diff(range(tix)),
             1 + (dat$R.low - min(tix)) / diff(range(tix)),
             1 + (dat$R.high - min(tix)) / diff(range(tix)))
arrows(R.rs[, 1], R.rs[, 3], R.rs[, 1], R.rs[, 4], col = "black",
       angle=90, length=0, code = 0, lwd = 0.5)
points(R.rs[, 1], R.rs[, 2], col = "black", bg = site, pch = 21, cex = 1)
axis(2, 1 + (tix - min(tix)) / diff(range(tix)), tix)
mtext(expression("R (CO"[2]*"/S"[(z)]*")"), 2, line = 2.5, at = 1.5)

dat = dat %>%
  mutate(co2.low = co2 - co2.sd,
         co2.high = co2 + co2.sd)
yext = range(dat$co2.low, dat$co2.high)
tix = seq(floor(min(yext)-33), 
          ceiling(max(yext)), by = 100)
co2.rs = cbind(dat$age,
               0 + (dat$co2 - min(tix)) / diff(range(tix)),
               0 + (dat$co2.low - min(tix)) / diff(range(tix)),
               0 + (dat$co2.high - min(tix)) / diff(range(tix)))
arrows(co2.rs[, 1], co2.rs[, 3], co2.rs[, 1], co2.rs[, 4], col = "gray",
       angle=90, length=0, code = 0, lwd = 0.5)
points(co2.rs[, 1], co2.rs[, 2], col = "black", bg = site, pch = 22, cex = 1)
axis(4, 0 + (tix - min(tix)) / diff(range(tix)), tix)
mtext(expression("CO"[2]*" (ppmv)"), 4, line = 2.5, at = 0.5)

axis(1, at = seq(800, 2700, 100), labels = FALSE)
text(x = seq(800, 2700, 200), y = -0.28, labels = seq(800, 2700, 200), srt = 40, xpd = TRUE, adj = 1)
mtext("Age (ka)", 1, line = 2)
legend(x = 800, y = 4.1, legend = c("Zhaojiachuan", "Fuxian"),
       col = pal, pch = 16, cex = 0.6, pt.cex = 1)
dev.off()
