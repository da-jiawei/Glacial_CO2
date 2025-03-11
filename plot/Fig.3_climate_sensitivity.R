library(tidyverse)
library(ggpubr)
library(effsize)
source('plot/functions.R')
set.seed(42)
nsyth = 1e5
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
ice_co2 = co2 %>% filter(method == "ice_core", age <= 800)
proxy_co2 = co2 %>% filter(age > 800)
gmst = read.csv("output/binned_gmst.csv")
benthic = read.csv("output/binned_benthic.csv")
bwt = read.csv("output/binned_bwt.csv")
wpwp = read.csv("output/binned_wpwp.csv")
R_LI = read.csv("data/R_LI_koehler2015.csv") %>%
  rename(age = Age) %>% filter(age < 2700)
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
r_li_g = filter_g(R_LI, "age") %>% 
  mutate(time = assign_time_group(age, 300, 2700))
# land ice changes contribute comparatively less per unit radiative forcing to the global temperature anomalies than the CO2 changes
r_li_g = r_li_g %>% mutate(RLI = RLI * 0.45) # using a efficacy factor of 0.45 - Stap (2019)
forcing_g = data.frame("time" = unique(co2_g$time))
subsample = data.frame(matrix(ncol = 2, nrow = nsyth))
names(subsample) = c("CO2", "R_li")
for (i in 1:nrow(forcing_g)) {
  co2_s = co2_g %>% filter(time == forcing_g$time[i])
  rli_s = r_li_g %>% filter(time == forcing_g$time[i])
  subsample$CO2 = sample(co2_s$CO2, nsyth, replace = TRUE)
  subsample$R_li = sample(rli_s$RLI, nsyth, replace = TRUE)
  lnco2 = log(subsample$CO2 / 278)
  R_total = 0.67 * (5.35 * lnco2 + subsample$R_li)
  # conversion factor of 0.67 to account for other the influence of other long-term processes
  # namely vegetation, aerosol, and non-CO2 greenhouse gas changes
  forcing_g$lnco2[i] = mean(lnco2)
  forcing_g$lnco2.sd[i] = sd(lnco2)
  forcing_g$r[i] = mean(R_total)
  forcing_g$r.sd[i] = sd(R_total)
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
r_li_ig = filter_ig(R_LI, "age") %>% 
  mutate(time = assign_time_group(age, 300, 2700))
# land ice changes contribute comparatively less per unit radiative forcing to the global temperature anomalies than the CO2 changes
r_li_ig = r_li_ig %>% mutate(RLI = RLI * 0.45) # using a efficacy factor of 0.45 - Stap (2019)
forcing_ig = data.frame("time" = unique(co2_ig$time))
subsample = data.frame(matrix(ncol = 2, nrow = nsyth))
names(subsample) = c("CO2", "R_li")
for (i in 1:nrow(forcing_ig)) {
  co2_s = co2_ig %>% filter(time == forcing_ig$time[i])
  rli_s = r_li_ig %>% filter(time == forcing_ig$time[i])
  subsample$CO2 = sample(co2_s$CO2, nsyth, replace = TRUE)
  subsample$R_li = sample(rli_s$RLI, nsyth, replace = TRUE)
  lnco2 = log(subsample$CO2 / 278)
  R_total = 0.67 * (5.35 * lnco2 + subsample$R_li)
  # conversion factor of 0.67 to account for other the influence of other long-term processes
  # namely vegetation, aerosol, and non-CO2 greenhouse gas changes
  forcing_ig$lnco2[i] = mean(lnco2)
  forcing_ig$lnco2.sd[i] = sd(lnco2)
  forcing_ig$r[i] = mean(R_total)
  forcing_ig$r.sd[i] = sd(R_total)
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
dat_g = read.csv("output/climate_sensitivity_glacial.csv")
dat_ig = read.csv("output/climate_sensitivity_interglacial.csv")
pal = c("#2171B5", "#D94801")
# plot lnCO2
plot_lnco2 = function(dat, param, param.sd){
  if(deparse(substitute(dat)) == "dat_g"){
    pal_s = pal[1]
    num = 1
  } else {
    pal_s = pal[2]
    num = 7
  }
  xmin = round(min(dat_g$lnco2 - dat_g$lnco2.sd)-0.1,1)
  xmax = round(max(dat_ig$lnco2 + dat_ig$lnco2.sd),1)
  if(param == "d18"){
    ymin = min(dat_ig[[param]] - dat_ig[[param.sd]])
    ymax = max(dat_g[[param]] + dat_g[[param.sd]])
    ggplot(dat, aes(x = lnco2, y = .data[[param]])) +
      geom_smooth(method = "lm", formula = y ~ x, show.legend = F, span = 1, alpha = 0.1, linetype = "dashed", color = pal_s, fill = pal_s) +
      geom_errorbar(aes(xmin = lnco2 - lnco2.sd, xmax = lnco2 + lnco2.sd), width = 0, linewidth = 0.2) +
      geom_errorbar(aes(ymin = .data[[param]] - .data[[param.sd]], ymax = .data[[param]] + .data[[param.sd]]), width = 0, linewidth = 0.2) +
      geom_point(aes(fill = time), shape = 21, size = 3) +
      scale_fill_brewer(palette = num) +
      theme_bw() + theme +
      scale_x_continuous(limits = c(xmin, xmax)) +
      scale_y_reverse(limits = c(ymax, ymin)) +
      labs(x = expression("ln(CO"[2]*"/C"[o]*")"))
  } else {
    ymin = min(dat_g[[param]] - dat_g[[param.sd]])
    ymax = max(dat_ig[[param]] + dat_ig[[param.sd]])
    ggplot(dat, aes(x = lnco2, y = .data[[param]])) +
      geom_smooth(method = "lm", formula = y ~ x, show.legend = F, span = 1, alpha = 0.1, linetype = "dashed", color = pal_s, fill = pal_s) +
      geom_errorbar(aes(xmin = lnco2 - lnco2.sd, xmax = lnco2 + lnco2.sd), width = 0, linewidth = 0.2) +
      geom_errorbar(aes(ymin = .data[[param]] - .data[[param.sd]], ymax = .data[[param]] + .data[[param.sd]]), width = 0, linewidth = 0.2) +
      geom_point(aes(fill = time), shape = 21, size = 3) +
      scale_fill_brewer(palette = num) +
      theme_bw() + theme +
      scale_x_continuous(limits = c(xmin, xmax)) +
      scale_y_continuous(limits = c(ymin, ymax)) +
      labs(x = expression("ln(CO"[2]*"/C"[o]*")"))
  }
}
plot_r = function(dat, param, param.sd){
  if(deparse(substitute(dat)) == "dat_g"){
    pal_s = pal[1]
    num = 1
  } else {
    pal_s = pal[2]
    num = 7
  }
  xmin = round(min(dat_g$r - dat_g$r.sd)-0.1,1)
  xmax = round(max(dat_ig$r + dat_ig$r.sd),1)
  if(param == "d18"){
    ymin = min(dat_ig[[param]] - dat_ig[[param.sd]])
    ymax = max(dat_g[[param]] + dat_g[[param.sd]])
    ggplot(dat, aes(x = r, y = .data[[param]])) +
      geom_smooth(method = "lm", formula = y ~ x, show.legend = F, span = 1, alpha = 0.1, linetype = "dashed", color = pal_s, fill = pal_s) +
      geom_errorbar(aes(xmin = r - r.sd, xmax = r + r.sd), width = 0, linewidth = 0.2) +
      geom_errorbar(aes(ymin = .data[[param]] - .data[[param.sd]], ymax = .data[[param]] + .data[[param.sd]]), width = 0, linewidth = 0.2) +
      geom_point(aes(fill = time), shape = 21, size = 3) +
      scale_fill_brewer(palette = num) +
      theme_bw() + theme +
      scale_x_continuous(limits = c(xmin, xmax)) +
      scale_y_reverse(limits = c(ymax, ymin)) +
      labs(x = expression(Delta*"F"[CO2+LI]*" (W m"^"-2"*")"))
  } else {
    ymin = min(dat_g[[param]] - dat_g[[param.sd]])
    ymax = max(dat_ig[[param]] + dat_ig[[param.sd]])
    ggplot(dat, aes(x = r, y = .data[[param]])) +
      geom_smooth(method = "lm", formula = y ~ x, show.legend = F, span = 1, alpha = 0.1, linetype = "dashed", color = pal_s, fill = pal_s) +
      geom_errorbar(aes(xmin = r - r.sd, xmax = r + r.sd), width = 0, linewidth = 0.2) +
      geom_errorbar(aes(ymin = .data[[param]] - .data[[param.sd]], ymax = .data[[param]] + .data[[param.sd]]), width = 0, linewidth = 0.2) +
      geom_point(aes(fill = time), shape = 21, size = 3) +
      scale_fill_brewer(palette = num) +
      theme_bw() + theme +
      scale_x_continuous(limits = c(xmin, xmax)) +
      scale_y_continuous(limits = c(ymin, ymax)) +
      labs(x = expression(Delta*"F"[CO2+LI]*" (W m"^"-2"*")"))
  }
}
summary(lm(data = dat_g, gmst ~ r))
summary(lm(data = dat_ig, gmst ~ r))
p1 = plot_lnco2(dat_g, "gmst", "gmst.sd")
p2 = plot_lnco2(dat_g, "d18", "d18.sd")
p3 = plot_lnco2(dat_g, "bwt", "bwt.sd")
p4 = plot_lnco2(dat_g, "sst", "sst.sd")
p5 = plot_r(dat_g, "gmst", "gmst.sd")
p6 = plot_r(dat_g, "d18", "d18.sd")
p7 = plot_r(dat_g, "bwt", "bwt.sd")
p8 = plot_r(dat_g, "sst", "sst.sd")

ggarrange(p1,p2,p3,p4,p5,p6,p7,p8, nrow = 2, ncol = 4, align = "hv", common.legend = TRUE)
ggsave("figures/Fig_3_climate_sensitivity_glacials.pdf", width = 9.9, height = 5.8)

p1 = plot_lnco2(dat_ig, "gmst", "gmst.sd")
p2 = plot_lnco2(dat_ig, "d18", "d18.sd")
p3 = plot_lnco2(dat_ig, "bwt", "bwt.sd")
p4 = plot_lnco2(dat_ig, "sst", "sst.sd")
p5 = plot_r(dat_ig, "gmst", "gmst.sd")
p6 = plot_r(dat_ig, "d18", "d18.sd")
p7 = plot_r(dat_ig, "bwt", "bwt.sd")
p8 = plot_r(dat_ig, "sst", "sst.sd")

ggarrange(p1,p2,p3,p4,p5,p6,p7,p8, nrow = 2, ncol = 4, align = "hv", common.legend = TRUE)
ggsave("figures/Fig_3_climate_sensitivity_interglacials.pdf", width = 9.9, height = 5.8)

# PDFs ----
slope_pdf = function(param, param.sd) {
  slope = data.frame(matrix(nrow = nsyth, ncol = 2))
  names(slope) = c("glacial", "interglacial")
  for (i in 1:nsyth) {
    subsample = data.frame(matrix(nrow = nrow(dat_g), ncol = 5))
    names(subsample) = c("time", "r.g", "sst.g", "r.ig", "sst.ig")
    subsample$time = dat_g$time
    for (p in 1:nrow(subsample)) {
      subsample$r.g[p] = rnorm(1, mean = dat_g$r[p], sd = dat_g$r.sd[p])
      subsample$sst.g[p] = rnorm(1, mean = dat_g[[param]][p], sd = dat_g[[param.sd]][p])
      subsample$r.ig[p] = rnorm(1, mean = dat_ig$r[p], sd = dat_ig$r.sd[p])
      subsample$sst.ig[p] = rnorm(1, mean = dat_ig[[param]][p], sd = dat_ig[[param.sd]][p])
    }
    m1 = lm(sst.g ~ r.g, subsample)
    slope$glacial[i] = ifelse(summary(m1)$coefficients[2, "Pr(>|t|)"] < 0.05,
                              m1$coefficients[2], NA)
    # slope$glacial[i] = m1$coefficients[2]
    m2 = lm(sst.ig ~ r.ig, subsample)
    slope$interglacial[i] = ifelse(summary(m2)$coefficients[2, "Pr(>|t|)"] < 0.05,
                                   m2$coefficients[2], NA)
    # slope$interglacial[i] = m2$coefficients[2]
  }
  return(slope)
}
slope_gmst = slope_pdf("gmst", "gmst.sd")
slope_d18 = slope_pdf("d18", "d18.sd")
slope_bwt = slope_pdf("bwt", "bwt.sd")
slope_sst = slope_pdf("sst", "sst.sd")

# plot ----
slope_gmst_clean = slope_gmst %>% 
  filter(!is.na(glacial) & !is.na(interglacial)) %>%
  filter(interglacial >= 0 & glacial >= 0)
cohen.d(slope_gmst_clean$glacial,slope_gmst_clean$interglacial)
p1 = ggplot(slope_gmst_clean) +
  geom_density(aes(x = glacial), color = pal[1], fill = pal[1], size = 1, alpha = 0.3) +
  geom_density(aes(x = interglacial), color = pal[2], fill = pal[2], size = 1, alpha = 0.2) +
  geom_vline(xintercept = median(slope_gmst_clean$glacial), color = pal[1], linewidth = 1, linetype = "dashed") +
  geom_vline(xintercept = median(slope_gmst_clean$interglacial), color = pal[2], linewidth = 1, linetype = "dashed") +
  theme_bw() + theme +
  scale_x_continuous(limits = c(0, 6)) +
  labs(x = "slope", y = "density") +
  annotate("text", x = 5, y = 0.7, label = expression(Delta*"GMST")) +
  annotate("text", label = round(median(slope_gmst_clean$glacial), 2), x = 5, y = 0.6, color = pal[1]) +
  annotate("text", label = round(median(slope_gmst_clean$interglacial), 2), x = 5, y = 0.5, color = pal[2]) +
  annotate("text", label = paste("d =", round(cohen.d(slope_gmst_clean$glacial, slope_gmst_clean$interglacial)$estimate, 2)), x = 5, y = 0.4)

slope_d18_clean = slope_d18 %>% 
  filter(!is.na(glacial) & !is.na(interglacial)) %>%
  filter(interglacial <= 0 & glacial <= 0)
cohen.d(slope_d18_clean$interglacial, slope_d18_clean$glacial)
p2 = ggplot(slope_d18_clean) +
  geom_density(aes(x = glacial), color = pal[1], fill = pal[1], size = 1, alpha = 0.3) +
  geom_density(aes(x = interglacial), color = pal[2], fill = pal[2], size = 1, alpha = 0.2) +
  geom_vline(xintercept = median(slope_d18_clean$glacial), color = pal[1], linewidth = 1, linetype = "dashed") +
  geom_vline(xintercept = median(slope_d18_clean$interglacial), color = pal[2], linewidth = 1, linetype = "dashed") +
  theme_bw() + theme +
  scale_x_continuous(limits = c(-1, 0)) +
  labs(x = "slope", y = "density") +
  annotate("text", x = -0.7, y = 3.5, label = expression(delta^"18"*"O")) +
  annotate("text", label = round(median(slope_d18_clean$glacial), 2), x = -0.7, y = 3, color = pal[1]) +
  annotate("text", label = round(median(slope_d18_clean$interglacial), 2), x = -0.7, y = 2.5, color = pal[2]) +
  annotate("text", label = paste("d =", round(cohen.d(slope_d18_clean$interglacial, slope_d18_clean$glacial)$estimate, 2)), 
           x = -0.7, y = 2)

slope_bwt_clean = slope_bwt %>% 
  filter(!is.na(glacial) & !is.na(interglacial)) %>%
  filter(interglacial >= 0 & glacial >= 0)
cohen.d(slope_bwt_clean$glacial, slope_bwt_clean$interglacial)
p3 = ggplot(slope_bwt_clean) +
  geom_density(aes(x = glacial), color = pal[1], fill = pal[1], size = 1, alpha = 0.3) +
  geom_density(aes(x = interglacial), color = pal[2], fill = pal[2], size = 1, alpha = 0.2) +
  geom_vline(xintercept = median(slope_bwt_clean$glacial), color = pal[1], linewidth = 1, linetype = "dashed") +
  geom_vline(xintercept = median(slope_bwt_clean$interglacial), color = pal[2], linewidth = 1, linetype = "dashed") +
  theme_bw() + theme +
  scale_x_continuous(limits = c(0, 4)) +
  labs(x = "slope", y = "density") +
  annotate("text", x = 3, y = 1, label = "BWT") +
  annotate("text", label = round(median(slope_bwt_clean$glacial), 2), x = 3, y = 0.7, color = pal[1]) +
  annotate("text", label = round(median(slope_bwt_clean$interglacial), 2), x = 3, y = 0.6, color = pal[2]) +
  annotate("text", label = paste("d =", abs(round(cohen.d(slope_bwt_clean$glacial, slope_bwt_clean$interglacial)$estimate, 2))), 
           x = 3, y = 0.5)

slope_sst_clean = slope_sst %>% 
  filter(!is.na(glacial) & !is.na(interglacial)) %>%
  filter(interglacial >= 0 & glacial >= 0)
cohen.d(slope_sst_clean$glacial, slope_sst_clean$interglacial)
p4 = ggplot(slope_sst_clean) +
  geom_density(aes(x = glacial), color = pal[1], fill = pal[1], size = 1, alpha = 0.3) +
  geom_density(aes(x = interglacial), color = pal[2], fill = pal[2], size = 1, alpha = 0.2) +
  geom_vline(xintercept = median(slope_sst_clean$glacial), color = pal[1], linewidth = 1, linetype = "dashed") +
  geom_vline(xintercept = median(slope_sst_clean$interglacial), color = pal[2], linewidth = 1, linetype = "dashed") +
  theme_bw() + theme +
  scale_x_continuous(limits = c(0, 4)) +
  labs(x = "slope", y = "density") +
  annotate("text", x = 3, y = 1, label = "SST") +
  annotate("text", label = round(median(slope_sst_clean$glacial), 2), x = 3, y = 0.7, color = pal[1]) +
  annotate("text", label = round(median(slope_sst_clean$interglacial), 2), x = 3, y = 0.6, color = pal[2]) +
  annotate("text", label = paste("d =", round(cohen.d(slope_sst_clean$glacial, slope_sst_clean$interglacial)$estimate, 2)), 
           x = 3, y = 0.5)

ggarrange(p1, p2, p3, p4, nrow = 1, ncol = 4, align = "hv")
ggsave("figures/Fig_3_climate_sensitivity_pdfs.pdf", width = 9.9, height = 2.5)
