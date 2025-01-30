library(tidyverse)
library(ggpubr)
source('plot/functions.R')
set.seed(42)
nsyth = 10000
theme = theme(axis.text.x = element_text(margin = margin(t = 0.1, unit = "cm")),
              axis.text.y = element_text(margin = margin(r = 0.1, unit = "cm")),
              axis.ticks.length=unit(0.15, "cm"),
              axis.ticks = element_line(colour = "black"),
              text = element_text(color = "black", size = 10),
              axis.title = element_text(size = 10), 
              axis.text = element_text(size = 10),
              plot.title = element_text(hjust = 0.1, vjust = -10),
              legend.text = element_text(size = 10),
              legend.title = element_text(size = 10),
              panel.grid.minor = element_blank(),
              panel.grid.major = element_blank())

# read data
co2 = read.csv("output/binned_co2.csv")
ice_co2 = co2 %>% filter(method == "ice", age <= 0.8)
proxy_co2 = co2 %>% filter(age > 0.8)
gmst = read.csv("output/binned_gmst.csv")
benthic = read.csv("output/binned_benthic.csv")
bwt = read.csv("output/binned_bwt.csv")
wpwp = read.csv("output/binned_wpwp.csv")
R_LI = read.csv("data/R_LI_koehler2015.csv") %>%
  mutate(age = Age / 1000) %>%
  filter(age < 2.7)
dat_ig = read.csv("output/climate_sensitivity_interglacial.csv")
dat_g = read.csv("output/climate_sensitivity_glacial.csv")

# time-series plot ----
pal = c("#A6CEE3", "#1F78B4", "#B2DF8A", "#33A02C", "#FB9A99", "#E31A1C")
method = pal[factor(proxy_co2$method, levels = c("paleosol", "boron", "ice"))]

png("figures/time_series_climate_sensitivity.png", 4, 5.5, units = "in", res = 300)
par(mar = c(4, 4, 1, 4))
plot(-0.3, 0, xlim = c(0, 2.7), ylim = c(0, 6), axes = FALSE,
     xlab = "", ylab = "")

legend(x = 0, y = 4.1, legend = c("paleosol", "boron", "blue ice"),
       col = pal, pch = 16, cex = 0.4, pt.cex = 1)

yext = range(gmst$GMST)
tix = seq(floor(min(yext)), 
          ceiling(max(yext)), by = 4)
gmst.rs = cbind(gmst$age,
                5 + (gmst$GMST - min(tix)) / diff(range(tix)))
gmst.rs = gmst.rs[order(gmst.rs[, 1]), ]
lines(gmst.rs[, 1], gmst.rs[, 2], col = pal[1], lwd = 1)
axis(2, 5 + (tix - min(tix)) / diff(range(tix)), tix)
mtext(expression(paste(Delta*"GMST (", degree,"C)")), 2, line = 2.5, at = 5.5)

yext = range(R_LI$RLI)
tix = seq(floor(min(yext)), 
          ceiling(max(yext)), by = 2)
R_LI.rs = cbind(R_LI$age,
               4 + (R_LI$RLI - min(tix)) / diff(range(tix)))
R_LI.rs = R_LI.rs[order(R_LI.rs[, 1]), ]
lines(R_LI.rs[, 1], R_LI.rs[, 2], col = pal[5], lwd = 1)
axis(4, 4 + (tix - min(tix)) / diff(range(tix)), tix)
mtext(expression(Delta*"R"[LI]*" (W m"^"-2"*")"), 4, line = 2.5, at = 4.5)

yext = range(co2$CO2)
tix = seq(floor(min(yext)-39), 
          ceiling(max(yext)+50), by = 100)
ice.rs = cbind(ice_co2$age,
                3 + (ice_co2$CO2 - min(tix)) / diff(range(tix)))
proxy.rs = cbind(proxy_co2$age,
                 3 + (proxy_co2$CO2 - min(tix)) / diff(range(tix)))
ice.rs = ice.rs[order(ice.rs[, 1]), ]
lines(ice.rs[, 1], ice.rs[, 2], col = "black", lwd = 1)
points(proxy.rs[, 1], proxy.rs[, 2], col = "black", bg = method, pch = 21, cex = 1)
axis(2, 3 + (tix - min(tix)) / diff(range(tix)), tix)
mtext(expression("CO"[2]*" (ppmv)"), 2, line = 2.5, at = 3.5)

yext = range(benthic$d18O)
tix = seq(ceiling(max(yext)),
          floor(min(yext)), by = -1)
benthic.rs = cbind(benthic$age,
               3 - (benthic$d18O - min(tix)) / diff(range(tix)))
benthic.rs = benthic.rs[order(benthic.rs[, 1]), ]
lines(benthic.rs[, 1], benthic.rs[, 2], col = pal[1], lwd = 1)
axis(4, 3 - (tix - min(tix)) / diff(range(tix)), tix)
mtext(expression("benthic "*delta^"18"*"O (\u2030)"), 4, line = 2.5, at = 2.5)

yext = range(bwt$BWT)
tix = seq(floor(min(yext)), 
          ceiling(max(yext)), by = 2)
bwt.rs = cbind(bwt$age,
                   1 + (bwt$BWT - min(tix)) / diff(range(tix)))
bwt.rs = bwt.rs[order(bwt.rs[, 1]), ]
lines(bwt.rs[, 1], bwt.rs[, 2], col = pal[2], lwd = 1)
axis(2, 1 + (tix - min(tix)) / diff(range(tix)), tix)
mtext(expression(paste("BWT (", degree, "T)")), 2, line = 2.5, at = 1.5)

yext = range(wpwp$SST)
tix = seq(floor(min(yext)), 
          ceiling(max(yext)), by = 2)
wpwp.rs = cbind(wpwp$age,
               0 + (wpwp$SST - min(tix)) / diff(range(tix)))
wpwp.rs = wpwp.rs[order(wpwp.rs[, 1]), ]
lines(wpwp.rs[, 1], wpwp.rs[, 2], col = pal[3], lwd = 1)
axis(4, 0 + (tix - min(tix)) / diff(range(tix)), tix)
mtext(expression(paste("SST (", degree, "T)")), 4, line = 2.5, at = 0.5)

axis(1)
mtext("Age (Ma)", 1, line = 2)
dev.off()

# time window mean ----
## glacials ----
# calculating radiative forcing
co2_g = co2 %>% filter(period == "glacial")
r_li_g = filter_g(R_LI, "Age") %>% mutate(time = assign_time_group(age, 0.3, 2.7))
# land ice changes contribute comparatively less per unit radiative forcing to the global temperature anomalies than the CO2 changes
r_li_g = r_li_g %>% mutate(RLI = RLI * 0.45) # using a efficacy factor of 0.45 - Stap (2019)
forcing_g = data.frame("time" = unique(co2_g$time))
for (i in 1:nrow(forcing_g)) {
  co2_sub = co2_g %>% filter(time == forcing_g$time[i])
  co2_s = sample(co2_sub$CO2, nsyth, replace = TRUE)
  rli_sub = r_li_g %>% filter(time == forcing_g$time[i])
  rli_s = sample(rli_sub$RLI, nsyth, replace = TRUE)
  lnco2 = log(co2_s / 278)
  # conversion factor of 0.67 to account for other the influence of other long-term processes
  # namely vegetation, aerosol, and non-CO2 greenhouse gas changes
  r_total = (5.35 * lnco2 + rli_s) * 0.67 
  forcing_g$lnco2[i] = mean(lnco2)
  forcing_g$lnco2.sd[i] = sd(lnco2)
  forcing_g$r[i] = mean(r_total)
  forcing_g$r.sd[i] = sd(r_total)
}
gmst_g = gmst %>%
  filter(period == "glacial") %>% group_by(time) %>%
  summarise(gmst = mean(GMST), gmst.sd = sd(GMST))
d18_g = benthic %>%
  filter(period == "glacial") %>% group_by(time) %>%
  summarise(d18 = mean(d18O), d18.sd = sd(d18O))
bwt_g = bwt %>% 
  filter(period == "glacial") %>% group_by(time) %>%
  summarise(bwt = mean(BWT), bwt.sd = sd(BWT))
wpwp_g = wpwp %>% 
  filter(period == "glacial") %>% group_by(time) %>%
  summarise(sst = mean(SST), sst.sd = sd(SST))

dat_list = list(forcing_g, gmst_g, d18_g, bwt_g, wpwp_g)
dat_g = reduce(dat_list, full_join, by = "time")
write.csv(dat_g, "output/climate_sensitivity_glacial.csv")

## interglacials ----
co2_ig = co2 %>% filter(period == "interglacial")
r_li_ig = filter_ig(R_LI, "Age") %>% mutate(time = assign_time_group(age, 0.3, 2.7))
r_li_ig = r_li_ig %>% mutate(RLI = RLI * 0.45) # using a efficacy factor of 0.45 - Stap (2019)
forcing_ig = data.frame("time" = unique(co2_ig$time))
for (i in 1:nrow(forcing_ig)) {
  co2_sub = co2_ig %>% filter(time == forcing_ig$time[i])
  co2_s = sample(co2_sub$CO2, nsyth, replace = TRUE)
  rli_sub = r_li_ig %>% filter(time == forcing_ig$time[i])
  rli_s = sample(rli_sub$RLI, nsyth, replace = TRUE)
  lnco2 = log(co2_s / 278)
  r_total = (5.35 * lnco2 + rli_s) * 0.67
  forcing_ig$lnco2[i] = mean(lnco2)
  forcing_ig$lnco2.sd[i] = sd(lnco2)
  forcing_ig$r[i] = mean(r_total)
  forcing_ig$r.sd[i] = sd(r_total)
}
gmst_ig = gmst %>%
  filter(period == "interglacial") %>% group_by(time) %>%
  summarise(gmst = mean(GMST), gmst.sd = sd(GMST)) 
d18_ig = benthic %>%
  filter(period == "interglacial") %>% group_by(time) %>%
  summarise(d18 = mean(d18O), d18.sd = sd(d18O))
bwt_ig = bwt %>% 
  filter(period == "interglacial") %>% group_by(time) %>%
  summarise(bwt = mean(BWT), bwt.sd = sd(BWT))
wpwp_ig = wpwp %>% 
  filter(period == "interglacial") %>% group_by(time) %>%
  summarise(sst = mean(SST), sst.sd = sd(SST))

dat_list = list(forcing_ig, gmst_ig, d18_ig, bwt_ig, wpwp_ig)
dat_ig = reduce(dat_list, full_join, by = "time")
write.csv(dat_ig, "output/climate_sensitivity_interglacial.csv")

## plot ----
# ranges
x.min = round(min(dat_g$lnco2 - dat_g$lnco2.sd)-0.1,1)
x.max = round(max(dat_ig$lnco2 + dat_ig$lnco2.sd),1)

p1 = ggplot(dat_g, aes(x = lnco2, y = gmst)) +
  geom_smooth(method = "lm", formula = y ~ x, show.legend = F, span = 1, alpha = 0.1, linetype = "dashed", color = "#2171B5", fill = "#2171B5") +
  geom_errorbar(aes(xmin = lnco2 - lnco2.sd, xmax = lnco2 + lnco2.sd), width = 0, linewidth = 0.2) +
  geom_errorbar(aes(ymin = gmst - gmst.sd, ymax = gmst + gmst.sd), width = 0, linewidth = 0.2) +
  geom_point(aes(fill = time), shape = 21, size = 3) +
  scale_fill_brewer(palette = 1) +
  theme_bw() + theme +
  scale_x_continuous(limits = c(x.min, x.max)) +
  scale_y_continuous(limits = c(-7, 5)) +
  labs(x = expression("ln(CO"[2]*"/C"[o]*")"),
       y = expression(paste(Delta*"GMST (", degree, "C)")))

p2 = ggplot(dat_g, aes(x = lnco2, y = d18)) +
  geom_smooth(method = "lm", formula = y ~ x, show.legend = F, span = 1, alpha = 0.1, linetype = "dashed", color = "#2171B5", fill = "#2171B5") +
  geom_errorbar(aes(xmin = lnco2 - lnco2.sd, xmax = lnco2 + lnco2.sd), width = 0, linewidth = 0.2) +
  geom_errorbar(aes(ymin = d18 - d18.sd, ymax = d18 + d18.sd), width = 0, linewidth = 0.2) +
  geom_point(aes(fill = time), shape = 21, size = 3) +
  scale_fill_brewer(palette = 1) +
  theme_bw() + theme +
  scale_x_continuous(limits = c(x.min, x.max)) +
  scale_y_reverse(limits = c(4.7, 3.1)) +
  labs(x = expression("ln(CO"[2]*"/C"[o]*")"),
       y = expression(delta^"18"*"O (\u2030)"))

p3 = ggplot(dat_g, aes(x = lnco2, y = bwt)) +
  geom_smooth(method = "lm", formula = y ~ x, show.legend = F, span = 1, alpha = 0.1, linetype = "dashed", color = "#2171B5", fill = "#2171B5") +
  geom_errorbar(aes(xmin = lnco2 - lnco2.sd, xmax = lnco2 + lnco2.sd), width = 0, linewidth = 0.2) +
  geom_errorbar(aes(ymin = bwt - bwt.sd, ymax = bwt + bwt.sd), width = 0, linewidth = 0.2) +
  geom_point(aes(fill = time), shape = 21, size = 3) +
  scale_fill_brewer(palette = 1) +
  theme_bw() + theme +
  scale_x_continuous(limits = c(x.min, x.max)) +
  scale_y_continuous(limits = c(-1, 5.5)) +
  labs(x = expression("ln(CO"[2]*"/C"[o]*")"),
       y = expression(paste("BWT (", degree, "C)")))

p4 = ggplot(dat_g, aes(x = lnco2, y = sst)) +
  geom_smooth(method = "lm", formula = y ~ x, show.legend = F, span = 1, alpha = 0.1, linetype = "dashed", color = "#2171B5", fill = "#2171B5") +
  geom_errorbar(aes(xmin = lnco2 - lnco2.sd, xmax = lnco2 + lnco2.sd), width = 0, linewidth = 0.2) +
  geom_errorbar(aes(ymin = sst - sst.sd, ymax = sst + sst.sd), width = 0, linewidth = 0.2) +
  geom_point(aes(fill = time), shape = 21, size = 3) +
  scale_fill_brewer(palette = 1) +
  theme_bw() + theme +
  scale_x_continuous(limits = c(x.min, x.max)) +
  scale_y_continuous(limits = c(25, 31)) +
  labs(x = expression("ln(CO"[2]*"/C"[o]*")"),
       y = expression(paste("SST (", degree, "C)")))

x.min = round(min(dat_g$r - dat_g$r.sd)-0.1,1)
x.max = round(max(dat_ig$r + dat_ig$r.sd),1)

p5 = ggplot(dat_g, aes(x = r, y = gmst)) +
  geom_smooth(method = "lm", formula = y ~ x, show.legend = F, span = 1, alpha = 0.1, linetype = "dashed", color = "#2171B5", fill = "#2171B5") +
  geom_errorbar(aes(xmin = r - r.sd, xmax = r + r.sd), width = 0, linewidth = 0.2) +
  geom_errorbar(aes(ymin = gmst - gmst.sd, ymax = gmst + gmst.sd), width = 0, linewidth = 0.2) +
  geom_point(aes(fill = time), shape = 21, size = 3) +
  scale_fill_brewer(palette = 1) +
  theme_bw() + theme +
  scale_x_continuous(limits = c(x.min, x.max)) +
  scale_y_continuous(limits = c(-7, 5)) +
  labs(x = expression(Delta*"F"[CO2+LI]*" (W m"^"-2"*")"),
       y = expression(paste(Delta*"GMST (", degree, "C)")))

p6 = ggplot(dat_g, aes(x = r, y = d18)) +
  geom_smooth(method = "lm", formula = y ~ x, show.legend = F, span = 1, alpha = 0.1, linetype = "dashed", color = "#2171B5", fill = "#2171B5") +
  geom_errorbar(aes(xmin = r - r.sd, xmax = r + r.sd), width = 0, linewidth = 0.2) +
  geom_errorbar(aes(ymin = d18 - d18.sd, ymax = d18 + d18.sd), width = 0, linewidth = 0.2) +
  geom_point(aes(fill = time), shape = 21, size = 3) +
  scale_fill_brewer(palette = 1) +
  theme_bw() + theme +
  scale_x_continuous(limits = c(x.min, x.max)) +
  scale_y_reverse(limits = c(4.7, 3.1)) +
  labs(x = expression(Delta*"F"[CO2+LI]*" (W m"^"-2"*")"),
       y = expression(delta^"18"*"O (\u2030)"))

p7 = ggplot(dat_g, aes(x = r, y = bwt)) +
  geom_smooth(method = "lm", formula = y ~ x, show.legend = F, span = 1, alpha = 0.1, linetype = "dashed", color = "#2171B5", fill = "#2171B5") +
  geom_errorbar(aes(xmin = r - r.sd, xmax = r + r.sd), width = 0, linewidth = 0.2) +
  geom_errorbar(aes(ymin = bwt - bwt.sd, ymax = bwt + bwt.sd), width = 0, linewidth = 0.2) +
  geom_point(aes(fill = time), shape = 21, size = 3) +
  scale_fill_brewer(palette = 1) +
  theme_bw() + theme +
  scale_x_continuous(limits = c(x.min, x.max)) +
  scale_y_continuous(limits = c(-1, 5.5)) +
  labs(x = expression(Delta*"F"[CO2+LI]*" (W m"^"-2"*")"),
       y = expression(paste("BWT (", degree, "C)")))

p8 = ggplot(dat_g, aes(x = r, y = sst)) +
  geom_smooth(method = "lm", formula = y ~ x, show.legend = F, span = 1, alpha = 0.1, linetype = "dashed", color = "#2171B5", fill = "#2171B5") +
  geom_errorbar(aes(xmin = r - r.sd, xmax = r + r.sd), width = 0, linewidth = 0.2) +
  geom_errorbar(aes(ymin = sst - sst.sd, ymax = sst + sst.sd), width = 0, linewidth = 0.2) +
  geom_point(aes(fill = time), shape = 21, size = 3) +
  scale_fill_brewer(palette = 1) +
  theme_bw() + theme +
  scale_x_continuous(limits = c(x.min, x.max)) +
  scale_y_continuous(limits = c(25, 31)) +
  labs(x = expression(Delta*"F"[CO2+LI]*" (W m"^"-2"*")"),
       y = expression(paste("SST (", degree, "C)")))

ggarrange(p1,p2,p3,p4,p5,p6,p7,p8, nrow = 2, ncol = 4, align = "hv", common.legend = TRUE)
ggsave("figures/Fig_3_climate_sensitivity_glacials.pdf", width = 9.9, height = 5.8)

# ranges
x.min = round(min(dat_g$lnco2 - dat_g$lnco2.sd)-0.1,1)
x.max = round(max(dat_ig$lnco2 + dat_ig$lnco2.sd),1)

p1 = ggplot(dat_ig, aes(x = lnco2, y = gmst)) +
  geom_smooth(method = "lm", formula = y ~ x, show.legend = F, span = 1, alpha = 0.1, linetype = "dashed", color = "#D94801", fill = "#D94801") +
  geom_errorbar(aes(xmin = lnco2 - lnco2.sd, xmax = lnco2 + lnco2.sd), width = 0, linewidth = 0.2) +
  geom_errorbar(aes(ymin = gmst - gmst.sd, ymax = gmst + gmst.sd), width = 0, linewidth = 0.2) +
  geom_point(aes(fill = time), shape = 21, size = 3) +
  scale_fill_brewer(palette = 7) +
  theme_bw() + theme +
  scale_x_continuous(limits = c(x.min, x.max)) +
  scale_y_continuous(limits = c(-7, 5)) +
  labs(x = expression("ln(CO"[2]*"/C"[o]*")"),
       y = expression(paste(Delta*"GMST (", degree, "C)")))

p2 = ggplot(dat_ig, aes(x = lnco2, y = d18)) +
  geom_smooth(method = "lm", formula = y ~ x, show.legend = F, span = 1, alpha = 0.1, linetype = "dashed", color = "#D94801", fill = "#D94801") +
  geom_errorbar(aes(xmin = lnco2 - lnco2.sd, xmax = lnco2 + lnco2.sd), width = 0, linewidth = 0.2) +
  geom_errorbar(aes(ymin = d18 - d18.sd, ymax = d18 + d18.sd), width = 0, linewidth = 0.2) +
  geom_point(aes(fill = time), shape = 21, size = 3) +
  scale_fill_brewer(palette = 7) +
  theme_bw() + theme +
  scale_x_continuous(limits = c(x.min, x.max)) +
  scale_y_reverse(limits = c(4.7, 3.1)) +
  labs(x = expression("ln(CO"[2]*"/C"[o]*")"),
       y = expression(delta^"18"*"O (\u2030)"))

p3 = ggplot(dat_ig, aes(x = lnco2, y = bwt)) +
  geom_smooth(method = "lm", formula = y ~ x, show.legend = F, span = 1, alpha = 0.1, linetype = "dashed", color = "#D94801", fill = "#D94801") +
  geom_errorbar(aes(xmin = lnco2 - lnco2.sd, xmax = lnco2 + lnco2.sd), width = 0, linewidth = 0.2) +
  geom_errorbar(aes(ymin = bwt - bwt.sd, ymax = bwt + bwt.sd), width = 0, linewidth = 0.2) +
  geom_point(aes(fill = time), shape = 21, size = 3) +
  scale_fill_brewer(palette = 7) +
  theme_bw() + theme +
  scale_x_continuous(limits = c(x.min, x.max)) +
  scale_y_continuous(limits = c(-1, 5.5)) +
  labs(x = expression("ln(CO"[2]*"/C"[o]*")"),
       y = expression(paste("BWT (", degree, "C)")))

p4 = ggplot(dat_ig, aes(x = lnco2, y = sst)) +
  geom_smooth(method = "lm", formula = y ~ x, show.legend = F, span = 1, alpha = 0.1, linetype = "dashed", color = "#D94801", fill = "#D94801") +
  geom_errorbar(aes(xmin = lnco2 - lnco2.sd, xmax = lnco2 + lnco2.sd), width = 0, linewidth = 0.2) +
  geom_errorbar(aes(ymin = sst - sst.sd, ymax = sst + sst.sd), width = 0, linewidth = 0.2) +
  geom_point(aes(fill = time), shape = 21, size = 3) +
  scale_fill_brewer(palette = 7) +
  theme_bw() + theme +
  scale_x_continuous(limits = c(x.min, x.max)) +
  scale_y_continuous(limits = c(25, 31)) +
  labs(x = expression("ln(CO"[2]*"/C"[o]*")"),
       y = expression(paste("SST (", degree, "C)")))

x.min = round(min(dat_g$r - dat_g$r.sd)-0.1,1)
x.max = round(max(dat_ig$r + dat_ig$r.sd),1)

p5 = ggplot(dat_ig, aes(x = r, y = gmst)) +
  geom_smooth(method = "lm", formula = y ~ x, show.legend = F, span = 1, alpha = 0.1, linetype = "dashed", color = "#D94801", fill = "#D94801") +
  geom_errorbar(aes(xmin = r - r.sd, xmax = r + r.sd), width = 0, linewidth = 0.2) +
  geom_errorbar(aes(ymin = gmst - gmst.sd, ymax = gmst + gmst.sd), width = 0, linewidth = 0.2) +
  geom_point(aes(fill = time), shape = 21, size = 3) +
  scale_fill_brewer(palette = 7) +
  theme_bw() + theme +
  scale_x_continuous(limits = c(x.min, x.max)) +
  scale_y_continuous(limits = c(-7, 5)) +
  labs(x = expression(Delta*"F"[CO2+LI]*" (W m"^"-2"*")"),
       y = expression(paste(Delta*"GMST (", degree, "C)")))

p6 = ggplot(dat_ig, aes(x = r, y = d18)) +
  geom_smooth(method = "lm", formula = y ~ x, show.legend = F, span = 1, alpha = 0.1, linetype = "dashed", color = "#D94801", fill = "#D94801") +
  geom_errorbar(aes(xmin = r - r.sd, xmax = r + r.sd), width = 0, linewidth = 0.2) +
  geom_errorbar(aes(ymin = d18 - d18.sd, ymax = d18 + d18.sd), width = 0, linewidth = 0.2) +
  geom_point(aes(fill = time), shape = 21, size = 3) +
  scale_fill_brewer(palette = 7) +
  theme_bw() + theme +
  scale_x_continuous(limits = c(x.min, x.max)) +
  scale_y_reverse(limits = c(4.7, 3.1)) +
  labs(x = expression(Delta*"F"[CO2+LI]*" (W m"^"-2"*")"),
       y = expression(delta^"18"*"O (\u2030)"))

p7 = ggplot(dat_ig, aes(x = r, y = bwt)) +
  geom_smooth(method = "lm", formula = y ~ x, show.legend = F, span = 1, alpha = 0.1, linetype = "dashed", color = "#D94801", fill = "#D94801") +
  geom_errorbar(aes(xmin = r - r.sd, xmax = r + r.sd), width = 0, linewidth = 0.2) +
  geom_errorbar(aes(ymin = bwt - bwt.sd, ymax = bwt + bwt.sd), width = 0, linewidth = 0.2) +
  geom_point(aes(fill = time), shape = 21, size = 3) +
  scale_fill_brewer(palette = 7) +
  theme_bw() + theme +
  scale_x_continuous(limits = c(x.min, x.max)) +
  scale_y_continuous(limits = c(-1, 5.5)) +
  labs(x = expression(Delta*"F"[CO2+LI]*" (W m"^"-2"*")"),
       y = expression(paste("BWT (", degree, "C)")))

p8 = ggplot(dat_ig, aes(x = r, y = sst)) +
  geom_smooth(method = "lm", formula = y ~ x, show.legend = F, span = 1, alpha = 0.1, linetype = "dashed", color = "#D94801", fill = "#D94801") +
  geom_errorbar(aes(xmin = r - r.sd, xmax = r + r.sd), width = 0, linewidth = 0.2) +
  geom_errorbar(aes(ymin = sst - sst.sd, ymax = sst + sst.sd), width = 0, linewidth = 0.2) +
  geom_point(aes(fill = time), shape = 21, size = 3) +
  scale_fill_brewer(palette = 7) +
  theme_bw() + theme +
  scale_x_continuous(limits = c(x.min, x.max)) +
  scale_y_continuous(limits = c(25, 31)) +
  labs(x = expression(Delta*"F"[CO2+LI]*" (W m"^"-2"*")"),
       y = expression(paste("SST (", degree, "C)")))

ggarrange(p1,p2,p3,p4,p5,p6,p7,p8, nrow = 2, ncol = 4, align = "hv", common.legend = TRUE)
ggsave("figures/Fig_3_climate_sensitivity_interglacials.pdf", width = 9.9, height = 5.8)

## statistics ----
# the p-value of the regression slopes
sens.co2 = data.frame(matrix(nrow = 2, ncol = 5))
names(sens.co2) = c("gmst", "d18", "bwt", "wpwp", "period")
sens.co2$period = c("glacial", "interglacial")
m_gmst_g = lm(gmst ~ lnco2, data = dat_g)
sens.co2$gmst[1] = summary(m_gmst_g)$coefficients[2,4]
m_gmst_ig = lm(gmst ~ lnco2, data = dat_ig)
sens.co2$gmst[2] = summary(m_gmst_ig)$coefficients[2,4]
m_d18_g = lm(d18 ~ lnco2, data = dat_g)
sens.co2$d18[1] = summary(m_d18_g)$coefficients[2,4]
m_d18_ig = lm(d18 ~ lnco2, data = dat_ig)
sens.co2$d18[2] = summary(m_d18_ig)$coefficients[2,4]
m_bwt_g = lm(bwt ~ lnco2, data = dat_g)
sens.co2$bwt[1] = summary(m_bwt_g)$coefficients[2,4]
m_bwt_ig = lm(bwt ~ lnco2, data = dat_ig)
sens.co2$bwt[2] = summary(m_bwt_ig)$coefficients[2,4]
m_wpwp_g = lm(sst ~ lnco2, data = dat_g)
sens.co2$wpwp[1] = summary(m_wpwp_g)$coefficients[2,4]
m_wpwp_ig = lm(sst ~ lnco2, data = dat_ig)
sens.co2$wpwp[2] = summary(m_wpwp_ig)$coefficients[2,4]

sens = data.frame(matrix(nrow = 2, ncol = 5))
names(sens) = c("gmst", "d18", "bwt", "wpwp", "period")
sens$period = c("glacial", "interglacial")
m_gmst_g = lm(gmst ~ r, data = dat_g)
sens$gmst[1] = summary(m_gmst_g)$coefficients[2,4]
m_gmst_ig = lm(gmst ~ r, data = dat_ig)
sens$gmst[2] = summary(m_gmst_ig)$coefficients[2,4]
m_d18_g = lm(d18 ~ r, data = dat_g)
sens$d18[1] = summary(m_d18_g)$coefficients[2,4]
m_d18_ig = lm(d18 ~ r, data = dat_ig)
sens$d18[2] = summary(m_d18_ig)$coefficients[2,4]
m_bwt_g = lm(bwt ~ r, data = dat_g)
sens$bwt[1] = summary(m_bwt_g)$coefficients[2,4]
m_bwt_ig = lm(bwt ~ r, data = dat_ig)
sens$bwt[2] = summary(m_bwt_ig)$coefficients[2,4]
m_wpwp_g = lm(sst ~ r, data = dat_g)
sens$wpwp[1] = summary(m_wpwp_g)$coefficients[2,4]
m_wpwp_ig = lm(sst ~ r, data = dat_ig)
sens$wpwp[2] = summary(m_wpwp_ig)$coefficients[2,4]

write.csv(sens.co2, "output/sens_co2_pvalue.csv")
write.csv(sens, "output/sens_pvalue.csv")



# PDFs ----
slope = data.frame(matrix(nrow = nsyth, ncol = 8))
names(slope) = c("gmst_g", "d18_g", "bwt_g", "wpwp_g", "gmst_ig", "d18_ig", "bwt_ig", "wpwp_ig")
time = unique(co2_g$time)
# based on raw data ----
gmst_g = gmst %>% filter(period == "glacial")
d18_g = benthic %>% filter(period == "glacial")
bwt_g = bwt %>% filter(period == "glacial")
wpwp_g = wpwp %>% filter(period == "glacial")
gmst_ig = gmst %>% filter(period == "interglacial")
d18_ig = benthic %>% filter(period == "interglacial")
bwt_ig = bwt %>% filter(period == "interglacial")
wpwp_ig = wpwp %>% filter(period == "interglacial")
for (i in 1:nsyth) {
  subgroup = data.frame(matrix(nrow = length(time), ncol = 10))
  names(subgroup) = c("df.g", "gmst.g", "d18.g", "bwt.g", "sst.g", "df.ig", "gmst.ig", "d18.ig", "bwt.ig", "sst.ig")
  for (p in 1:length(time)) {
    gmst_s = gmst_g %>% filter(time == time[p])
    d18_s = d18_g %>% filter(time == time[p])
    bwt_s = bwt_g %>% filter(time == time[p])
    wpwp_s = wpwp_g %>% filter(time == time[p])
    # co2_s = co2_g %>% filter(time == time[p])
    # rli_s = r_li_g %>% filter(time == time[p])
    # r_co2 = 5.35 * log(sample(co2_s$CO2, 1)/278)
    # r_li = sample(rli_s$RLI, 1)
    # subgroup$df.g[p] = (r_co2 + r_li) * 0.67
    forcing_s = forcing_g %>% filter(time == time[p])
    subgroup$df.g[p] = rnorm(1, forcing_s$r, forcing_s$r.sd)
    subgroup$gmst.g[p] = sample(gmst_s$GMST, 1)
    subgroup$d18.g[p] = sample(d18_s$d18O, 1)
    subgroup$bwt.g[p] = sample(bwt$BWT, 1)
    subgroup$sst.g[p] = sample(wpwp$SST, 1)
    gmst_s = gmst_ig %>% filter(time == time[p])
    d18_s = d18_ig %>% filter(time == time[p])
    bwt_s = bwt_ig %>% filter(time == time[p])
    wpwp_s = wpwp_ig %>% filter(time == time[p])
    # co2_s = co2_ig %>% filter(time == time[p])
    # rli_s = r_li_ig %>% filter(time == time[p])
    # r_co2 = 5.35 * log(sample(co2_s$CO2, 1)/278)
    # r_li = sample(rli_s$RLI, 1)
    # subgroup$df.ig[p] = (r_co2 + r_li) * 0.67
    forcing_s = forcing_ig %>% filter(time == time[p])
    subgroup$df.ig[p] = rnorm(1, forcing_s$r, forcing_s$r.sd)
    subgroup$gmst.ig[p] = sample(gmst_s$GMST, 1)
    subgroup$d18.ig[p] = sample(d18_s$d18O, 1)
    subgroup$bwt.ig[p] = sample(bwt$BWT, 1)
    subgroup$sst.ig[p] = sample(wpwp$SST, 1)
  }
  m_gmst = lm(gmst.g ~ df.g, data = subgroup)
  m_d18 = lm(d18.g ~ df.g, data = subgroup)
  m_bwt = lm(bwt.g ~ df.g, data = subgroup)
  m_sst = lm(sst.g ~ df.g, data = subgroup)
  slope$gmst_g[i] = ifelse(summary(m_gmst)$coefficients[2, "Pr(>|t|)"]<0.05, m_gmst$coefficients[2], NA)
  slope$d18_g[i] = ifelse(summary(m_d18)$coefficients[2, "Pr(>|t|)"]<0.05, m_d18$coefficients[2], NA)
  slope$bwt_g[i] = ifelse(summary(m_bwt)$coefficients[2, "Pr(>|t|)"]<0.05, m_bwt$coefficients[2], NA)
  slope$wpwp_g[i] = ifelse(summary(m_sst)$coefficients[2, "Pr(>|t|)"]<0.05, m_sst$coefficients[2], NA)
  # slope$d18_g[i] = m_d18$coefficients[2]
  # slope$bwt_g[i] = m_bwt$coefficients[2]
  # slope$wpwp_g[i] = m_sst$coefficients[2]
  m_gmst = lm(gmst.ig ~ df.g, data = subgroup)
  m_d18 = lm(d18.ig ~ df.ig, data = subgroup)
  m_bwt = lm(bwt.ig ~ df.ig, data = subgroup)
  m_sst = lm(sst.ig ~ df.ig, data = subgroup)
  slope$gmst_ig[i] = ifelse(summary(m_gmst)$coefficients[2, "Pr(>|t|)"]<0.05, m_gmst$coefficients[2], NA)
  slope$d18_ig[i] = ifelse(summary(m_d18)$coefficients[2, "Pr(>|t|)"]<0.05, m_d18$coefficients[2], NA)
  slope$bwt_ig[i] = ifelse(summary(m_bwt)$coefficients[2, "Pr(>|t|)"]<0.05, m_bwt$coefficients[2], NA)
  slope$wpwp_ig[i] = ifelse(summary(m_sst)$coefficients[2, "Pr(>|t|)"]<0.05, m_sst$coefficients[2], NA)
  # slope$d18_ig[i] = m_d18$coefficients[2]
  # slope$bwt_ig[i] = m_bwt$coefficients[2]
  # slope$wpwp_ig[i] = m_sst$coefficients[2]
}

# based on mean and sd ----
for (i in 1:nsyth) {
  subgroup = data.frame(matrix(nrow = length(time), ncol = 10))
  names(subgroup) = c("df.g", "gmst.g", "d18.g", "bwt.g", "sst.g", "df.ig", "gmst.ig", "d18.ig", "bwt.ig", "sst.ig")
  for (p in 1:length(time)) {
    dat_s = dat_g %>% filter(time == time[p])
    subgroup$df.g[p] = rnorm(1, dat_s$r, dat_s$r.sd)
    subgroup$gmst.g[p] = rnorm(1, dat_s$gmst, dat_s$gmst.sd)
    subgroup$d18.g[p] = rnorm(1, dat_s$d18, dat_s$d18.sd)
    subgroup$bwt.g[p] = rnorm(1, dat_s$bwt, dat_s$bwt.sd)
    subgroup$sst.g[p] = rnorm(1, dat_s$sst, dat_s$sst.sd)
    dat_s = dat_ig %>% filter(time == time[p])
    subgroup$df.ig[p] = rnorm(1, dat_s$r, dat_s$r.sd)
    subgroup$gmst.ig[p] = rnorm(1, dat_s$gmst, dat_s$gmst.sd)
    subgroup$d18.ig[p] = rnorm(1, dat_s$d18, dat_s$d18.sd)
    subgroup$bwt.ig[p] = rnorm(1, dat_s$bwt, dat_s$bwt.sd)
    subgroup$sst.ig[p] = rnorm(1, dat_s$sst, dat_s$sst.sd)
  }
  m_gmst = lm(gmst.g ~ df.g, data = subgroup)
  slope$gmst_g[i] = ifelse(summary(m_gmst)$coefficients[2, "Pr(>|t|)"]<0.05, m_gmst$coefficients[2], NA)
  m_d18 = lm(d18.g ~ df.g, data = subgroup)
  slope$d18_g[i] = ifelse(summary(m_d18)$coefficients[2, "Pr(>|t|)"]<0.05, m_d18$coefficients[2], NA)
  m_bwt = lm(bwt.g ~ df.g, data = subgroup)
  slope$bwt_g[i] = ifelse(summary(m_bwt)$coefficients[2, "Pr(>|t|)"]<0.05, m_bwt$coefficients[2], NA)
  m_sst = lm(sst.g ~ df.g, data = subgroup)
  slope$wpwp_g[i] = ifelse(summary(m_sst)$coefficients[2, "Pr(>|t|)"]<0.05, m_sst$coefficients[2], NA)
  m_gmst = lm(gmst.ig ~ df.ig, data = subgroup)
  slope$gmst_ig[i] = ifelse(summary(m_gmst)$coefficients[2, "Pr(>|t|)"]<0.05, m_gmst$coefficients[2], NA)
  m_d18 = lm(d18.ig ~ df.ig, data = subgroup)
  slope$d18_ig[i] = ifelse(summary(m_d18)$coefficients[2, "Pr(>|t|)"]<0.05, m_d18$coefficients[2], NA)
  m_bwt = lm(bwt.ig ~ df.ig, data = subgroup)
  slope$bwt_ig[i] = ifelse(summary(m_bwt)$coefficients[2, "Pr(>|t|)"]<0.05, m_bwt$coefficients[2], NA)
  m_sst = lm(sst.ig ~ df.ig, data = subgroup)
  slope$wpwp_ig[i] = ifelse(summary(m_sst)$coefficients[2, "Pr(>|t|)"]<0.05, m_sst$coefficients[2], NA)
}

# plot ----
t.test(slope$gmst_g, slope$gmst_ig)
p1 = ggplot(slope) +
  geom_density(aes(x = gmst_g), color = "#2171B5", fill = "#2171B5", size = 1, alpha = 0.3) +
  geom_density(aes(x = gmst_ig), color = "#D94801", fill = "#D94801", size = 1, alpha = 0.2) +
  geom_vline(xintercept = median(slope$gmst_g, na.rm = TRUE), color = "#2171B5", linewidth = 1, linetype = "dashed") +
  geom_vline(xintercept = median(slope$gmst_ig, na.rm = TRUE), color = "#D94801", linewidth = 1, linetype = "dashed") +
  theme_bw() + theme +
  annotate("text", x = 0.6, y = 0.7, label = expression(Delta*"GMST")) +
  scale_x_continuous(limits = c(0, 5)) +
  labs(x = "slope", y = "density")
p1
t.test(slope$d18_g, slope$d18_ig)
p2 = ggplot(slope) +
  geom_density(aes(x = d18_g), color = "#2171B5", fill = "#2171B5", size = 1, alpha = 0.3) +
  geom_density(aes(x = d18_ig), color = "#D94801", fill = "#D94801", size = 1, alpha = 0.2) +
  geom_vline(xintercept = median(slope$d18_g, na.rm = TRUE), color = "#2171B5", linewidth = 1, linetype = "dashed") +
  geom_vline(xintercept = median(slope$d18_ig, na.rm = TRUE), color = "#D94801", linewidth = 1, linetype = "dashed") +
  theme_bw() + theme +
  annotate("text", x = -0.75, y = 4, label = expression("Benthic "*delta^"18"*"O")) +
  scale_x_continuous(limits = c(-1, 0)) +
  labs(x = "slope", y = "density")
p2
t.test(slope$bwt_g, slope$bwt_ig)
p3 = ggplot(slope) +
  geom_density(aes(x = bwt_g), color = "#2171B5", fill = "#2171B5", size = 1, alpha = 0.3) +
  geom_density(aes(x = bwt_ig), color = "#D94801", fill = "#D94801", size = 1, alpha = 0.2) +
  geom_vline(xintercept = median(slope$bwt_g, na.rm = TRUE), color = "#2171B5", linewidth = 1, linetype = "dashed") +
  geom_vline(xintercept = median(slope$bwt_ig, na.rm = TRUE), color = "#D94801", linewidth = 1, linetype = "dashed") +
  theme_bw() + theme +
  annotate("text", x = 3.5, y = 1.2, label = "BWT") +
  scale_x_continuous(limits = c(0, 4)) +
  labs(x = "slope", y = "density")
p3
t.test(slope$wpwp_g, slope$wpwp_ig)
p4 = ggplot(slope) +
  geom_density(aes(x = wpwp_g), color = "#2171B5", fill = "#2171B5", size = 1, alpha = 0.3) +
  geom_density(aes(x = wpwp_ig), color = "#D94801", fill = "#D94801", size = 1, alpha = 0.2) +
  geom_vline(xintercept = median(slope$wpwp_g, na.rm = TRUE), color = "#2171B5", size = 1, linetype = "dashed") +
  geom_vline(xintercept = median(slope$wpwp_ig, na.rm = TRUE), color = "#D94801", size = 1, linetype = "dashed") +
  theme_bw() + theme +
  annotate("text", x = 2.5, y = 1, label = "WPWP") +
  scale_x_continuous(limits = c(-0.5, 3)) +
  labs(x = "slope", y = "density")
p4
ggarrange(p1, p2, p3, p4, nrow = 1, ncol = 4, align = "hv")
ggsave("figures/Fig_3_climate_sensitivity_pdfs.pdf", width = 9.9, height = 2.5)
