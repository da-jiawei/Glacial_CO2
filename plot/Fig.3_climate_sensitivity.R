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

# read data ----
co2 = read.csv("output/binned_co2.csv")
proxy_co2 = co2 |> 
  filter(age > 800)
gmst_clark = read.csv("data/GMST/clark_2024.csv") |>
  mutate(age = age * 1e3) |>
  filter(age <= 2600)
R_LI = read.csv("data/R_LI_koehler2015.csv") |>
  mutate(age = Age) |> 
  filter(age <= 2600)

# time window mean
# calculating radiative forcing ----
RF = function(time_period){
  if (time_period == "glacial") {
    co2_sub = filter_g(co2, "age")
    Rli_sub = filter_g(R_LI, "age") |>
      mutate(time = assign_time_group(age, 300, 2700))
  } else {
    co2_sub = filter_ig(co2, "age")
    Rli_sub = filter_ig(R_LI, "age") |>
      mutate(time = assign_time_group(age, 300, 2700))
  }
  
  forcing_sub = data.frame("time" = unique(co2_sub$time))
  for (i in 1:nrow(forcing_sub)) {
    # calculate CO2 forcing
    co2_s = co2_sub |> filter(time == forcing_sub$time[i])
    co2_iter = sample(co2_s$CO2, nsyth, replace = TRUE)
    r_co2 = 5.35 * log(co2_iter / 278)
    
    # calculate CO2 + ice sheet
    rli_s = Rli_sub |> filter(time == forcing_sub$time[i])
    rli_iter = sample(rli_s$RLI, nsyth, replace = TRUE)
    r_co2_li = r_co2 + rli_iter
    
    # a efficacy factor of 0.45 +0.34/-0.2 to correct for ice sheet forcing - Stap (2019)
    median_val = .45
    upper_sigma = .34
    lower_sigma = .20
    location = median_val
    scale = (upper_sigma + lower_sigma) / 4 
    shape = (upper_sigma - lower_sigma) / scale
    eps_li = rsn(nsyth, xi = location, omega = scale, alpha = shape)
    rli_cal = rli_iter * eps_li
    r_co2_li_cal = r_co2 + rli_cal
    
    forcing_sub$r_co2[i] = mean(r_co2)
    forcing_sub$r_co2.sd[i] = sd(r_co2)
    forcing_sub$r_co2_li[i] = mean(r_co2_li)
    forcing_sub$r_co2_li.sd[i] = sd(r_co2_li)
    forcing_sub$r_co2_li_cal[i] = mean(r_co2_li_cal)
    forcing_sub$r_co2_li_cal.sd[i] = sd(r_co2_li_cal)
  }
  return(forcing_sub)
}
# glacial 
forcing_g = RF("glacial")
gmst_clark_g = filter_g(gmst_clark, "age") |>
  mutate(time = assign_time_group(age, 300, 2700)) |>
  group_by(time) |>
  summarise(gmst_c = mean(GMST), 
            gmst_c.sd = sd(GMST))

dat_list = list(forcing_g, gmst_clark_g)
dat_g = reduce(dat_list, full_join, by = "time")
write.csv(dat_g, "output/climate_sensitivity_glacial.csv")
# interglacial
forcing_ig = RF("interglacial")
gmst_clark_ig = filter_ig(gmst_clark, "age") |>
  mutate(time = assign_time_group(age, 300, 2700)) |>
  group_by(time) |>
  summarise(gmst_c = mean(GMST), 
            gmst_c.sd = sd(GMST))
dat_list = list(forcing_ig, gmst_clark_ig)
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
summary(lm(data = dat_g, gmst_c ~ r_co2_li))
summary(lm(data = dat_g, gmst_c ~ r_co2_li_cal))

p1 = plot(dat_g, "r_co2", "r_co2.sd", "gmst_c", "gmst_c.sd")
p2 = plot(dat_g, "r_co2_li", "r_co2_li.sd", "gmst_c", "gmst_c.sd")
p3 = plot(dat_g, "r_co2_li_cal", "r_co2_li_cal.sd", "gmst_c", "gmst_c.sd")

summary(lm(data = dat_ig, gmst_c ~ r_co2))
summary(lm(data = dat_ig, gmst_c ~ r_co2_li))
summary(lm(data = dat_ig, gmst_c ~ r_co2_li_cal))

p4 = plot(dat_ig, "r_co2", "r_co2.sd", "gmst_c", "gmst_c.sd")
p5 = plot(dat_ig, "r_co2_li", "r_co2_li.sd", "gmst_c", "gmst_c.sd")
p6 = plot(dat_ig, "r_co2_li_cal", "r_co2_li_cal.sd", "gmst_c", "gmst_c.sd")

ggarrange(p1,p2,p3,p4,p5,p6, nrow = 2, ncol = 3, align = "hv", common.legend = TRUE)
ggsave("figures/Fig_3_climate_sensitivity.pdf", width = 7.5, height = 5.8)

# PDFs ----
nsyth = 1e5
slope_pdf = function(param, param.sd) {
  g_co2 = rep(NA, nsyth)
  g_co2_li = rep(NA, nsyth)
  g_r = rep(NA, nsyth)
  ig_co2 = rep(NA, nsyth)
  ig_co2_li = rep(NA, nsyth)
  ig_r = rep(NA, nsyth)

  for (i in 1:nsyth) {
    subsample = data.frame(matrix(nrow = nrow(dat_g), ncol = 9))
    names(subsample) = c("time", "r_co2.g", "r_co2_li.g", "r_co2_li_cal.g", "gmst.g", 
                         "r_co2.ig", "r_co2_li.ig", "r_co2_li_cal.ig", "gmst.ig")
    subsample$time = dat_g$time
    for (p in 1:nrow(subsample)) {
      subsample$r_co2.g[p] = rnorm(1, mean = dat_g$r_co2[p], sd = dat_g$r_co2.sd[p])
      subsample$r_co2_li.g[p] = rnorm(1, mean = dat_g$r_co2_li[p], sd = dat_g$r_co2_li.sd[p])
      subsample$r_co2_li_cal.g[p] = rnorm(1, mean = dat_g$r_co2_li_cal[p], sd = dat_g$r_co2_li_cal.sd[p])
      subsample$gmst.g[p] = rnorm(1, mean = dat_g[[param]][p], sd = dat_g[[param.sd]][p])
      
      subsample$r_co2.ig[p] = rnorm(1, mean = dat_ig$r_co2[p], sd = dat_ig$r_co2.sd[p])
      subsample$r_co2_li.ig[p] = rnorm(1, mean = dat_ig$r_co2_li[p], sd = dat_ig$r_co2_li.sd[p])
      subsample$r_co2_li_cal.ig[p] = rnorm(1, mean = dat_ig$r_co2_li_cal[p], sd = dat_ig$r_co2_li_cal.sd[p])
      subsample$gmst.ig[p] = rnorm(1, mean = dat_ig[[param]][p], sd = dat_ig[[param.sd]][p])
    }
    
    # conversion factor of 0.64 +/-0.07 to account for other the influence of other long-term processes
    phi = rnorm(1, .64, .07)
    
    # glacials
    m1 = lm(gmst.g ~ r_co2.g, subsample)
    g_co2[i] = ifelse(summary(m1)$coefficients[2, "Pr(>|t|)"] < 0.05,
                              m1$coefficients[2], NA)
    m1 = lm(gmst.g ~ r_co2_li.g, subsample)
    g_co2_li[i] = ifelse(summary(m1)$coefficients[2, "Pr(>|t|)"] < 0.05,
                      m1$coefficients[2], NA)
    m1 = lm(gmst.g ~ r_co2_li_cal.g, subsample)
    g_r[i] = ifelse(summary(m1)$coefficients[2, "Pr(>|t|)"] < 0.05,
                              m1$coefficients[2] * phi, NA)
    # interglacials
    m1 = lm(gmst.ig ~ r_co2.ig, subsample)
    ig_co2[i] = ifelse(summary(m1)$coefficients[2, "Pr(>|t|)"] < 0.05,
                                   m1$coefficients[2], NA)
    m1 = lm(gmst.ig ~ r_co2_li.g, subsample)
    ig_co2_li[i] = ifelse(summary(m1)$coefficients[2, "Pr(>|t|)"] < 0.05,
                       m1$coefficients[2], NA)
    m1 = lm(gmst.ig ~ r_co2_li_cal.ig, subsample)
    ig_r[i] = ifelse(summary(m1)$coefficients[2, "Pr(>|t|)"] < 0.05,
                                   m1$coefficients[2] * phi, NA)
  }
  g_co2 = g_co2[g_co2 > 0 & !is.na(g_co2)]
  g_co2_li = g_co2_li[g_co2_li > 0 & !is.na(g_co2_li)]
  g_r = g_r[g_r > 0 & !is.na(g_r)]
  ig_co2 = ig_co2[ig_co2 > 0 & !is.na(ig_co2)]
  ig_co2_li = ig_co2_li[ig_co2_li > 0 & !is.na(ig_co2_li)]
  ig_r = ig_r[ig_r > 0 & !is.na(ig_r)]
  dat_list = list(g_co2, ig_co2, g_co2_li, ig_co2_li, g_r, ig_r)
  return(dat_list)
}
slope_gmst_c = slope_pdf("gmst_c", "gmst_c.sd")

# plot ----
plot_pdf = function(dat1, dat2, name, xmax, ymax) {
  target = data.frame(
    value = c(dat1, dat2),
    group = factor(c(
      rep("glacial", length(dat1)),
      rep("interglacial", length(dat2))
    ))
  )
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

t.test(slope_gmst_c[[5]], slope_gmst_c[[6]])
cohen.d(slope_gmst_c[[1]], slope_gmst_c[[2]])

p1 = plot_pdf(slope_gmst_c[[1]], slope_gmst_c[[2]], "R_CO2",6,0.84)
p2 = plot_pdf(slope_gmst_c[[3]], slope_gmst_c[[4]], "R_CO2_LI",6,1.9)
p3 = plot_pdf(slope_gmst_c[[5]], slope_gmst_c[[6]], "R_total",6,1.7)

ggarrange(p1,p2,p3,nrow = 1, ncol = 3, align = "hv")
ggsave("figures/Fig_3_climate_sensitivity_pdfs.pdf", width = 7.8, height = 3) 

# fing the 95% confidence intervals
cf_95 = function(slopes){
  dat = density(slopes)
  dx = diff(dat$x[1:2])
  cdf = cumsum(dat$y) * dx
  lower_idx = which.min(abs(cdf - 0.025))
  upper_idx = which.min(abs(cdf - 0.975))
  ci_lower = dat$x[lower_idx]
  ci_upper = dat$x[upper_idx] 
  return(c("lower" = ci_lower,
           "upper" = ci_upper,
           "ECS_lower" = ci_lower * 3.7,
           "ECS_upper"= ci_upper* 3.7))
}
cf_95(slope_gmst_c[[5]]) # glacial
cf_95(slope_gmst_c[[6]]) # interglacial



