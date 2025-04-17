rm(list = ls())
pacman::p_load(tidyverse, ggpubr, effsize, sn, readxl)
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
ice_co2 = co2 |> 
  filter(method == "ice_core", age <= 800)
proxy_co2 = co2 |> 
  filter(age > 800)
gmst_clark = read.csv("data/GMST/clark_2024.csv") |>
  mutate(age = age * 1e3) |>
  filter(age <= 2600)
gmst_stap = read_xlsx("data/GMST/stap_2018.xlsx") |>
  mutate(age = age_ka) |>
  filter(age <= 2600)
R_LI = read.csv("data/R_LI_koehler2015.csv") |>
  mutate(age = Age) |> 
  filter(age <= 2600)

# time-series plot ----
pal = c("#A6CEE3", "#1F78B4", "#B2DF8A", "#33A02C", "#FB9A99", "#E31A1C")
method = pal[factor(proxy_co2$method, levels = c("paleosol", "boron", "ice"))]

png("figures/time_series_climate_sensitivity.png", 4, 5.5, units = "in", res = 300)
par(mar = c(4, 4, 1, 4))
graphics::plot(-0.3, 0, xlim = c(0, 2.7), ylim = c(0, 4), axes = FALSE,
     xlab = "", ylab = "")

# legend(x = 0, y = 4.1, legend = c("paleosol", "boron", "blue ice"),
#        col = pal, pch = 16, cex = 0.4, pt.cex = 1)

yext = range(gmst_stap$DGMST)
tix = seq(floor(min(yext)), 
          ceiling(max(yext)), by = 1)
gmst.rs = cbind(gmst_stap$age / 1e3,
                3 + (gmst_stap$DGMST - min(tix)) / diff(range(tix)))
gmst.rs = gmst.rs[order(gmst.rs[, 1]), ]
lines(gmst.rs[, 1], gmst.rs[, 2], col = pal[3], lwd = 1)
axis(4, 3 + (tix - min(tix)) / diff(range(tix)), tix)
mtext(expression(paste(Delta*"GMST (", degree,"C)")), 4, line = 2.5, at = 3.5)

yext = range(gmst_clark$GMST)
tix = seq(floor(min(yext)), 
          ceiling(max(yext) + 1.2), by = 4)
gmst.rs = cbind(gmst_clark$age / 1e3,
                2 + (gmst_clark$GMST - min(tix)) / diff(range(tix)))
gmst.rs = gmst.rs[order(gmst.rs[, 1]), ]
lines(gmst.rs[, 1], gmst.rs[, 2], col = pal[1], lwd = 1)
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
points(proxy.rs[, 1], proxy.rs[, 2], col = "black", bg = method, pch = 21, cex = 1)
axis(2, 0 + (tix - min(tix)) / diff(range(tix)), tix)
mtext(expression("CO"[2]*" (ppmv)"), 2, line = 2.5, at = 0.5)

axis(1)
mtext("Age (Ma)", 1, line = 2)
dev.off()

# time window mean ----
## glacials ----
# calculating radiative forcing
co2_g = co2 |> 
  filter(period == "glacial")
r_li_g = filter_g(R_LI, "age") |> 
  mutate(time = assign_time_group(age, 300, 2700))
forcing_g = data.frame("time" = unique(co2_g$time))

for (i in 1:nrow(forcing_g)) {
  co2_s = co2_g |> filter(time == forcing_g$time[i])
  rli_s = r_li_g |> filter(time == forcing_g$time[i])
  co2_iter = sample(co2_s$CO2, nsyth, replace = TRUE)
  r_co2 = 5.35 * log(co2_iter / 278)
  
  rli_iter = sample(rli_s$RLI, nsyth, replace = TRUE)
  # a efficacy factor of 0.45 +0.34/-0.2 to correct for ice sheet forcing - Stap (2019)
  median_val = .45
  upper_sigma = .34
  lower_sigma = .20
  location = median_val
  scale = (upper_sigma + lower_sigma) / 4 
  shape = (upper_sigma - lower_sigma) / scale
  eps_li = rsn(nsyth, xi = location, omega = scale, alpha = shape)
  rli_cal = rli_iter * eps_li

  # conversion factor of 0.64 +/-0.07 to account for other the influence of other long-term processes
  theta = rnorm(nsyth, .64, .07)
  r_total = theta * (r_co2 + rli_iter)
  r_total_cal = theta * (r_co2 + rli_cal)
  
  forcing_g$r_co2[i] = mean(r_co2)
  forcing_g$r_co2.sd[i] = sd(r_co2)
  forcing_g$r[i] = mean(r_total)
  forcing_g$r.sd[i] = sd(r_total)
  forcing_g$r_cal[i] = mean(r_total_cal)
  forcing_g$r_cal.sd[i] = sd(r_total_cal)
}
gmst_clark_g = gmst_clark |>
  filter(period == "glacial") |> 
  group_by(time) |>
  summarise(gmst_c = mean(GMST), 
            gmst_c.sd = sd(GMST))
gmst_stap_g = filter_g(gmst_stap, "age") |>
  mutate(time = assign_time_group(age, 300, 2700)) |>
  group_by(time) |>
  summarise(gmst_s = mean(DGMST), 
            gmst_s.sd = sd(DGMST))

dat_list = list(forcing_g, gmst_clark_g, gmst_stap_g)
dat_g = reduce(dat_list, full_join, by = "time")
write.csv(dat_g, "output/climate_sensitivity_glacial.csv")

## interglacials ----
co2_ig = co2 |> filter(period == "interglacial")
r_li_ig = filter_ig(R_LI, "age") |> 
  mutate(time = assign_time_group(age, 300, 2700))
forcing_ig = data.frame("time" = unique(co2_ig$time))

for (i in 1:nrow(forcing_ig)) {
  co2_s = co2_ig |> filter(time == forcing_ig$time[i])
  rli_s = r_li_ig |> filter(time == forcing_ig$time[i])
  co2_iter = sample(co2_s$CO2, nsyth, replace = TRUE)
  r_co2 = 5.35 * log(co2_iter / 278)
  
  rli_iter = sample(rli_s$RLI, nsyth, replace = TRUE)
  # a efficacy factor of 0.45 +0.34/-0.2 to correct for ice sheet forcing - Stap (2019)
  median_val = .45
  upper_sigma = .34
  lower_sigma = .20
  location = median_val
  scale = (upper_sigma + lower_sigma) / 4 
  shape = (upper_sigma - lower_sigma) / scale
  eps_li = rsn(nsyth, xi = location, omega = scale, alpha = shape)
  rli_cal = rli_iter * eps_li

  # conversion factor of 0.64 +/-0.07 to account for other the influence of other long-term processes
  theta = rnorm(nsyth, .64, .07)
  r_total = theta * (r_co2 + rli_iter)
  r_total_cal = theta * (r_co2 + rli_cal)
  
  forcing_ig$r_co2[i] = mean(r_co2)
  forcing_ig$r_co2.sd[i] = sd(r_co2)
  forcing_ig$r[i] = mean(r_total)
  forcing_ig$r.sd[i] = sd(r_total)
  forcing_ig$r_cal[i] = mean(r_total_cal)
  forcing_ig$r_cal.sd[i] = sd(r_total_cal)
}
gmst_clark_ig = gmst_clark |>
  filter(period == "interglacial") |> 
  group_by(time) |>
  summarise(gmst_c = mean(GMST), 
            gmst_c.sd = sd(GMST))
gmst_stap_ig = filter_ig(gmst_stap, "age") |>
  mutate(time = assign_time_group(age, 300, 2700)) |>
  group_by(time) |>
  summarise(gmst_s = mean(DGMST), 
            gmst_s.sd = sd(DGMST))


dat_list = list(forcing_ig, gmst_clark_ig, gmst_stap_ig)
dat_ig = reduce(dat_list, full_join, by = "time")
write.csv(dat_ig, "output/climate_sensitivity_interglacial.csv")

## plot ----
dat_g = read.csv("output/climate_sensitivity_glacial.csv")
dat_ig = read.csv("output/climate_sensitivity_interglacial.csv")
pal = c("#2171B5", "#D94801")
# plot lnCO2
plot = function(dat, forcing, forcing.sd, param, param.sd){
  if(deparse(substitute(dat)) == "dat_g"){
    pal_s = pal[1]
    num = 1
  } else {
    pal_s = pal[2]
    num = 7
  }
  xmin = round(min(dat_g[[forcing]] - dat_g[[forcing.sd]])-0.1, 1)
  xmax = round(max(dat_ig[[forcing]] + dat_ig[[forcing.sd]])+0.4, 1)
  ymin = floor(min(dat_g[[param]] - dat_g[[param.sd]]))
  ymax = ceiling(max(dat_ig[[param]] + dat_ig[[param.sd]]))
  ggplot(dat, aes(x = .data[[forcing]], y = .data[[param]])) +
    geom_smooth(method = "lm", formula = y ~ x, show.legend = F, span = 1, alpha = 0.1, linetype = "dashed", color = pal_s, fill = pal_s) +
    geom_errorbar(aes(xmin = .data[[forcing]] - .data[[forcing.sd]], xmax = .data[[forcing]] + .data[[forcing.sd]]), width = 0, linewidth = 0.2) +
    geom_errorbar(aes(ymin = .data[[param]] - .data[[param.sd]], ymax = .data[[param]] + .data[[param.sd]]), width = 0, linewidth = 0.2) +
    geom_point(aes(fill = time), shape = 21, size = 3) +
    scale_fill_brewer(palette = num) +
    theme_bw() + theme +
    scale_x_continuous(limits = c(xmin, xmax)) +
    scale_y_continuous(limits = c(ymin, ymax))
}

summary(lm(data = dat_g, gmst_c ~ r_co2))
summary(lm(data = dat_g, gmst_c ~ r))
summary(lm(data = dat_g, gmst_c ~ r_cal))

p1 = plot(dat_g, "r_co2", "r_co2.sd", "gmst_c", "gmst_c.sd")
p2 = plot(dat_g, "r", "r.sd", "gmst_c", "gmst_c.sd")
p3 = plot(dat_g, "r_cal", "r_cal.sd", "gmst_c", "gmst_c.sd")

p4 = plot(dat_g, "r_co2", "r_co2.sd", "gmst_s", "gmst_s.sd")
p5 = plot(dat_g, "r", "r.sd", "gmst_s", "gmst_s.sd")
p6 = plot(dat_g, "r_cal", "r_cal.sd", "gmst_s", "gmst_s.sd")

ggarrange(p1,p2,p3,p4,p5,p6, nrow = 2, ncol = 3, align = "hv", common.legend = TRUE)
ggsave("figures/Fig_3_climate_sensitivity_glacials.pdf", width = 7.5, height = 5.8)

summary(lm(data = dat_ig, gmst_c ~ r_co2))
summary(lm(data = dat_ig, gmst_c ~ r))
summary(lm(data = dat_ig, gmst_c ~ r_cal))

p1 = plot(dat_ig, "r_co2", "r_co2.sd", "gmst_c", "gmst_c.sd")
p2 = plot(dat_ig, "r", "r.sd", "gmst_c", "gmst_c.sd")
p3 = plot(dat_ig, "r_cal", "r_cal.sd", "gmst_c", "gmst_c.sd")

p4 = plot(dat_ig, "r_co2", "r_co2.sd", "gmst_s", "gmst_s.sd")
p5 = plot(dat_ig, "r", "r.sd", "gmst_s", "gmst_s.sd")
p6 = plot(dat_ig, "r_cal", "r_cal.sd", "gmst_s", "gmst_s.sd")

ggarrange(p1,p2,p3,p4,p5,p6, nrow = 2, ncol = 3, align = "hv", common.legend = TRUE)
ggsave("figures/Fig_3_climate_sensitivity_interglacials.pdf", width = 7.5, height = 5.8)

# PDFs ----
slope_pdf = function(param, param.sd) {
  g_co2 = rep(NA, nsyth)
  g_r = rep(NA, nsyth)
  g_r_cal = rep(NA, nsyth)
  ig_co2 = rep(NA, nsyth)
  ig_r = rep(NA, nsyth)
  ig_r_cal = rep(NA, nsyth)
  
  for (i in 1:nsyth) {
    subsample = data.frame(matrix(nrow = nrow(dat_g), ncol = 9))
    names(subsample) = c("time", "r_co2.g", "r.g", "r_cal.g", "sst.g", "r_co2.ig", "r.ig", "r_cal.ig", "sst.ig")
    subsample$time = dat_g$time
    for (p in 1:nrow(subsample)) {
      subsample$r_co2.g[p] = rnorm(1, mean = dat_g$r_co2[p], sd = dat_g$r_co2.sd[p])
      subsample$r.g[p] = rnorm(1, mean = dat_g$r[p], sd = dat_g$r.sd[p])
      subsample$r_cal.g[p] = rnorm(1, mean = dat_g$r_cal[p], sd = dat_g$r_cal.sd[p])
      subsample$sst.g[p] = rnorm(1, mean = dat_g[[param]][p], sd = dat_g[[param.sd]][p])
      subsample$r_co2.ig[p] = rnorm(1, mean = dat_ig$r_co2[p], sd = dat_ig$r_co2.sd[p])
      subsample$r.ig[p] = rnorm(1, mean = dat_ig$r[p], sd = dat_ig$r.sd[p])
      subsample$r_cal.ig[p] = rnorm(1, mean = dat_ig$r_cal[p], sd = dat_ig$r_cal.sd[p])
      subsample$sst.ig[p] = rnorm(1, mean = dat_ig[[param]][p], sd = dat_ig[[param.sd]][p])
    }
    # glacials
    m1 = lm(sst.g ~ r_co2.g, subsample)
    g_co2[i] = ifelse(summary(m1)$coefficients[2, "Pr(>|t|)"] < 0.05,
                              m1$coefficients[2], NA)
    m1 = lm(sst.g ~ r.g, subsample)
    g_r[i] = ifelse(summary(m1)$coefficients[2, "Pr(>|t|)"] < 0.05,
                              m1$coefficients[2], NA)
    m1 = lm(sst.g ~ r_cal.g, subsample)
    g_r_cal[i] = ifelse(summary(m1)$coefficients[2, "Pr(>|t|)"] < 0.05,
                                  m1$coefficients[2], NA)
    # interglacials
    m1 = lm(sst.ig ~ r_co2.ig, subsample)
    ig_co2[i] = ifelse(summary(m1)$coefficients[2, "Pr(>|t|)"] < 0.05,
                                   m1$coefficients[2], NA)
    m1 = lm(sst.ig ~ r.ig, subsample)
    ig_r[i] = ifelse(summary(m1)$coefficients[2, "Pr(>|t|)"] < 0.05,
                                   m1$coefficients[2], NA)
    m1 = lm(sst.ig ~ r_cal.ig, subsample)
    ig_r_cal[i] = ifelse(summary(m1)$coefficients[2, "Pr(>|t|)"] < 0.05,
                                   m1$coefficients[2], NA)
  }
  g_co2 = g_co2[g_co2 > 0 & !is.na(g_co2)]
  g_r = g_r[g_r > 0 & !is.na(g_r)]
  g_r_cal = g_r_cal[g_r_cal > 0 & !is.na(g_r_cal)]
  ig_co2 = ig_co2[ig_co2 > 0 & !is.na(ig_co2)]
  ig_r = ig_r[ig_r > 0 & !is.na(ig_r)]
  ig_r_cal = ig_r_cal[ig_r_cal > 0 & !is.na(ig_r_cal)]
  dat_list = list(g_co2, ig_co2, g_r, ig_r, g_r_cal, ig_r_cal)
  return(dat_list)
}
slope_gmst_c = slope_pdf("gmst_c", "gmst_c.sd")
slope_gmst_s = slope_pdf("gmst_s", "gmst_s.sd")

# plot ----
plot_pdf = function(dat1, dat2, name, xmax) {
  target = data.frame(
    value = c(dat1, dat2),
    group = factor(c(
      rep("glacial", length(dat1)),
      rep("interglacial", length(dat2))
    ))
  )
  ymax = max(density(dat1)$y, density(dat2)$y)
  p1 = ggplot(target) +
    geom_density(aes(x = value, fill = group, color = group), alpha = 0.3) +
    scale_fill_manual(values = c(pal[1], pal[2])) +
    scale_color_manual(values = c(pal[1], pal[2])) +
    geom_vline(xintercept = median(dat1), color = pal[1], linewidth = 1, linetype = "dashed") +
    geom_vline(xintercept = median(dat2), color = pal[2], linewidth = 1, linetype = "dashed") +
    theme_bw() + theme +
    scale_x_continuous(limits = c(0, xmax)) +
    scale_y_continuous(limits = c(0, ymax)) +
    labs(title = name,
         x = "slope", y = "density") +
    guides(fill = "none", color = "none") +
    annotate("text", label = round(median(dat1), 2), x = xmax*.8, y = ymax*.8, color = pal[1]) +
    annotate("text", label = round(median(dat2), 2), x = xmax*.8, y = ymax*.6, color = pal[2]) +
    annotate("text", label = paste("d =", round(cohen.d(dat1, dat2)$estimate, 2)), x = xmax*.8, y = ymax*.4) +
    annotate("text", label = paste("p =", round(t.test(dat1, dat2)$p.value, 2)), x = xmax*.8, y = ymax*.2)
  return(p1)
}

p1 = plot_pdf(slope_gmst_c[[1]], slope_gmst_c[[2]], "CO2",6)
p2 = plot_pdf(slope_gmst_c[[3]], slope_gmst_c[[4]], "R",6)
p3 = plot_pdf(slope_gmst_c[[5]], slope_gmst_c[[6]], "R_cal",6)
p4 = plot_pdf(slope_gmst_s[[1]], slope_gmst_s[[2]], "CO2",2.5)
p5 = plot_pdf(slope_gmst_s[[3]], slope_gmst_s[[4]], "R",2.5)
p6 = plot_pdf(slope_gmst_s[[5]], slope_gmst_s[[6]], "R_cal",2.5)

ggarrange(p1,p2,p3,p4,p5,p6, nrow = 2, ncol = 3, align = "hv")
ggsave("figures/Fig_3_climate_sensitivity_pdfs.pdf", width = 7.8, height = 5.7)
