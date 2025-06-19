rm(list = ls())
pacman::p_load(tidyverse, effsize, sn, readxl, patchwork, ggnewscale, showtext)
showtext_auto()
source('plot/functions.R')
set.seed(42)
nsyth = 1e5
theme = theme(axis.text.x = element_text(margin = margin(t = 0.1, unit = "cm")),
              axis.text.y = element_text(margin = margin(r = 0.1, unit = "cm")),
              axis.ticks.length=unit(0.15, "cm"),
              axis.ticks = element_line(colour = "black"),
              text = element_text(color = "black", size = 10, family = "Arial"),
              axis.title = element_text(size = 12),
              axis.title.y = element_text(margin = margin(r = 0)),
              axis.text = element_text(size = 10, color = "black"),
              plot.title = element_text(hjust = 0.1, vjust = -10),
              legend.text = element_text(size = 10),
              legend.title = element_text(size = 10),
              panel.grid = element_blank())

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
summary(lm(data = dat_g, gmst_c ~ r_co2))
summary(lm(data = dat_g, gmst_c ~ r_co2_li))
summary(lm(data = dat_g, gmst_c ~ r_co2_li_cal))
summary(lm(data = dat_ig, gmst_c ~ r_co2))
summary(lm(data = dat_ig, gmst_c ~ r_co2_li))
summary(lm(data = dat_ig, gmst_c ~ r_co2_li_cal))

# S_CO2 
p1 = ggplot(dat_g, aes(x = r_co2, y = gmst_c)) +
  geom_errorbar(aes(xmin = r_co2 - r_co2.sd, xmax = r_co2 + r_co2.sd),
                linewidth = .2, width = 0) +
  geom_errorbar(aes(ymin = gmst_c - gmst_c.sd, ymax = gmst_c + gmst_c.sd),
                linewidth = .2, width = 0) +
  geom_errorbar(data = dat_ig, aes(x = r_co2, y = gmst_c,
                                   xmin = r_co2 - r_co2.sd, xmax = r_co2 + r_co2.sd),
                linewidth = .2, width = 0) +
  geom_errorbar(data = dat_ig, aes(x = r_co2, y = gmst_c,
                                   ymin = gmst_c - gmst_c.sd, ymax = gmst_c + gmst_c.sd),
                linewidth = .2, width = 0) +
  geom_point(aes(fill = time), size = 3, shape = 21) +
  geom_smooth(method = "lm", fill = pal[1], alpha = .1,
              linetype = "dashed", size = 1, col = pal[1]) +
  scale_fill_brewer(palette = 1) +
  ggnewscale::new_scale_fill() +
  geom_point(data = dat_ig, 
             aes(x = r_co2, y = gmst_c, fill = time), 
             size = 3, shape = 21) +
  scale_fill_brewer(palette = 7) +
  geom_smooth(data = dat_ig,
              aes(x = r_co2, y = gmst_c),
              method = "lm", fill = pal[2], alpha = .1,
              linetype = "dashed", size = 1, col = pal[2]) +
  annotate("text", x = 1, y = -2, label = expression("R"^"2"*" = 0.27"), col = pal[2]) +
  annotate("text", x = 1, y = -3, label = expression(italic(p)*" = 0.089"), col = pal[2]) +
  annotate("text", x = 0, y = -5, label = expression("R"^"2"*" = 0.65"), col = pal[1]) +
  annotate("text", x = 0, y = -6, label = expression(italic(p)*" = 0.005"), col = pal[1]) +
  annotate("text", x = -2, y = 4, label = "a", size = 5, fontface = "bold") +
  theme_bw() + theme +
  theme(legend.position = "none") +
  labs(x = expression(Delta*"F"[CO2]*" (W/m"^"2"*")"),
       y = expression(paste(Delta*"GMST (", degree, "C)"))) +
  theme(axis.title.x = element_text(margin = margin(t = -2)),
        axis.title.y = element_text(margin = margin(r = -2)))

# S_CO2_LI 
p2 = ggplot(dat_g, aes(x = r_co2_li, y = gmst_c)) +
  geom_errorbar(aes(xmin = r_co2_li - r_co2_li.sd, xmax = r_co2_li + r_co2_li.sd),
                linewidth = .2, width = 0) +
  geom_errorbar(aes(ymin = gmst_c - gmst_c.sd, ymax = gmst_c + gmst_c.sd),
                linewidth = .2, width = 0) +
  geom_errorbar(data = dat_ig, aes(x = r_co2_li, y = gmst_c,
                                   xmin = r_co2_li - r_co2_li.sd, xmax = r_co2_li + r_co2_li.sd),
                linewidth = .2, width = 0) +
  geom_errorbar(data = dat_ig, aes(x = r_co2_li, y = gmst_c,
                                   ymin = gmst_c - gmst_c.sd, ymax = gmst_c + gmst_c.sd),
                linewidth = .2, width = 0) +
  geom_point(aes(fill = time), size = 3, shape = 21) +
  geom_smooth(method = "lm", fill = pal[1], alpha = .1,
              linetype = "dashed", size = 1, col = pal[1]) +
  scale_fill_brewer(palette = 1) +
  ggnewscale::new_scale_fill() +
  geom_point(data = dat_ig, 
             aes(x = r_co2_li, y = gmst_c, fill = time), 
             size = 3, shape = 21) +
  scale_fill_brewer(palette = 7) +
  geom_smooth(data = dat_ig,
              aes(x = r_co2_li, y = gmst_c),
              method = "lm", fill = pal[2], alpha = .1,
              linetype = "dashed", size = 1, col = pal[2]) +
  annotate("text", x = 0.3, y = -2, label = expression("R"^"2"*" = 0.62"), col = pal[2]) +
  annotate("text", x = 0.3, y = -3, label = expression(italic(p)*" = 0.007"), col = pal[2]) +
  annotate("text", x = -1, y = -4.5, label = expression("R"^"2"*" = 0.87"), col = pal[1]) +
  annotate("text", x = -1, y = -5.5, label = expression(italic(p)*" < 0.001"), col = pal[1]) +
  annotate("text", x = -5, y = 4, label = "b", size = 5, fontface = "bold") +
  theme_bw() + theme +
  theme(legend.position = "none") +
  labs(x = expression(Delta*"F"["CO2,LI"]*" (W/m"^"2"*")"),
       y = expression(paste(Delta*"GMST (", degree, "C)"))) +
  theme(axis.title.x = element_text(margin = margin(t = -2)),
        axis.title.y = element_text(margin = margin(r = -2)))

# S_CO2_LI_cal
p3 = ggplot(dat_g, aes(x = r_co2_li_cal, y = gmst_c)) +
  geom_errorbar(aes(xmin = r_co2_li_cal - r_co2_li_cal.sd, xmax = r_co2_li_cal + r_co2_li_cal.sd),
                linewidth = .2, width = 0) +
  geom_errorbar(aes(ymin = gmst_c - gmst_c.sd, ymax = gmst_c + gmst_c.sd),
                linewidth = .2, width = 0) +
  geom_errorbar(data = dat_ig, aes(x = r_co2_li_cal, y = gmst_c,
                                   xmin = r_co2_li_cal - r_co2_li_cal.sd, xmax = r_co2_li_cal + r_co2_li_cal.sd),
                linewidth = .2, width = 0) +
  geom_errorbar(data = dat_ig, aes(x = r_co2_li_cal, y = gmst_c,
                                   ymin = gmst_c - gmst_c.sd, ymax = gmst_c + gmst_c.sd),
                linewidth = .2, width = 0) +
  geom_point(aes(fill = time), size = 3, shape = 21) +
  geom_smooth(method = "lm", fill = pal[1], alpha = .1,
              linetype = "dashed", size = 1, col = pal[1]) +
  scale_fill_brewer(palette = 1) +
  ggnewscale::new_scale_fill() +
  geom_point(data = dat_ig, 
             aes(x = r_co2_li_cal, y = gmst_c, fill = time), 
             size = 3, shape = 21) +
  scale_fill_brewer(palette = 7) +
  geom_smooth(data = dat_ig,
              aes(x = r_co2_li_cal, y = gmst_c),
              method = "lm", fill = pal[2], alpha = .1,
              linetype = "dashed", size = 1, col = pal[2]) +
  annotate("text", x = 0.5, y = -2, label = expression("R"^"2"*" = 0.50"), col = pal[2]) +
  annotate("text", x = 0.5, y = -3, label = expression(italic(p)*" = 0.021"), col = pal[2]) +
  annotate("text", x = -1, y = -4.5, label = expression("R"^"2"*" = 0.82"), col = pal[1]) +
  annotate("text", x = -1, y = -5.5, label = expression(italic(p)*" < 0.001"), col = pal[1]) +
  annotate("text", x = -3.8, y = 4, label = "c", size = 5, fontface = "bold") +
  theme_bw() + theme +
  theme(legend.position = "none") +
  labs(x = expression(Delta*"F"["CO2,LI-cal"]*" (W/m"^"2"*")"),
       y = expression(paste(Delta*"GMST (", degree, "C)"))) +
  theme(axis.title.x = element_text(margin = margin(t = -2)),
        axis.title.y = element_text(margin = margin(r = -2)))

p1 + p2 + p3

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
t.test(slope_gmst_c[[5]], slope_gmst_c[[6]])
cohen.d(slope_gmst_c[[1]], slope_gmst_c[[2]])
# S_CO2
target = data.frame(
  value = c(slope_gmst_c[[1]], slope_gmst_c[[2]]),
  group = factor(c(
    rep("glacial", length(slope_gmst_c[[1]])),
    rep("interglacial", length(slope_gmst_c[[2]]))
  ))
)
p4 = ggplot(target) +
  geom_density(aes(x = value, fill = group, color = group), alpha = 0.3) +
  scale_fill_manual(values = c(pal[1], pal[2])) +
  scale_color_manual(values = c(pal[1], pal[2])) +
  geom_vline(xintercept = median(slope_gmst_c[[1]]), color = pal[1], linewidth = 1) +
  geom_vline(xintercept = (median(slope_gmst_c[[1]]) - sd(slope_gmst_c[[1]])), 
             color = pal[1], linetype = "dashed") +
  geom_vline(xintercept = (median(slope_gmst_c[[1]]) + sd(slope_gmst_c[[1]])), 
             color = pal[1], linetype = "dashed") +
  geom_vline(xintercept = median(slope_gmst_c[[1]]), color = pal[1], linewidth = 1) +
  geom_vline(xintercept = median(slope_gmst_c[[2]]), color = pal[2], linewidth = 1) +
  geom_vline(xintercept = (median(slope_gmst_c[[2]]) - sd(slope_gmst_c[[2]])), 
             color = pal[2], linetype = "dashed") +
  geom_vline(xintercept = (median(slope_gmst_c[[2]]) + sd(slope_gmst_c[[2]])), 
             color = pal[2], linetype = "dashed") +
  theme_bw() + theme +
  scale_x_continuous(limits = c(0, 6)) +
  labs(x = "slope", 
       y = "density") +
  guides(fill = "none", color = "none") +
  annotate("text", label = expression("S"[CO2]), x = 5, y = .7, size = 6) +
  annotate("text", x = 5, y = .4, label = expression(italic(d)*" = 0.50")) +
  annotate("text", x = 5, y = .3, label = expression(italic(p)*" < 0.001")) +
  annotate("text", x = .3, y = .75, size = 5, label = "d", fontface = "bold") +
  theme(axis.title.x = element_text(margin = margin(t = -2)),
        axis.title.y = element_text(margin = margin(r = -2)))

# S_CO2_LI
target = data.frame(
  value = c(slope_gmst_c[[3]], slope_gmst_c[[4]]),
  group = factor(c(
    rep("glacial", length(slope_gmst_c[[3]])),
    rep("interglacial", length(slope_gmst_c[[4]]))
  ))
)
p5 = ggplot(target) +
  geom_density(aes(x = value, fill = group, color = group), alpha = 0.3) +
  scale_fill_manual(values = c(pal[1], pal[2])) +
  scale_color_manual(values = c(pal[1], pal[2])) +
  geom_vline(xintercept = median(slope_gmst_c[[3]]), color = pal[1], linewidth = 1) +
  geom_vline(xintercept = (median(slope_gmst_c[[3]]) - sd(slope_gmst_c[[3]])), 
             color = pal[1], linetype = "dashed") +
  geom_vline(xintercept = (median(slope_gmst_c[[3]]) + sd(slope_gmst_c[[3]])), 
             color = pal[1], linetype = "dashed") +
  geom_vline(xintercept = median(slope_gmst_c[[3]]), color = pal[1], linewidth = 1) +
  geom_vline(xintercept = median(slope_gmst_c[[4]]), color = pal[2], linewidth = 1) +
  geom_vline(xintercept = (median(slope_gmst_c[[4]]) - sd(slope_gmst_c[[4]])), 
             color = pal[2], linetype = "dashed") +
  geom_vline(xintercept = (median(slope_gmst_c[[4]]) + sd(slope_gmst_c[[4]])), 
             color = pal[2], linetype = "dashed") +
  theme_bw() + theme +
  scale_x_continuous(limits = c(0, 3)) +
  labs(x = "slope", 
       y = "density") +
  guides(fill = "none", color = "none") +
  annotate("text", label = expression("S"["CO2,LI"]), x = 2.5, y = 1.5, size = 6) +
  annotate("text", x = 2.5, y = .7, label = expression(italic(d)*" = 1.34")) +
  annotate("text", x = 2.5, y = .5, label = expression(italic(p)*" < 0.001")) +
  annotate("text", x = .2, y = 1.7, size = 5, label = "e", fontface = "bold") +
  theme(axis.title.x = element_text(margin = margin(t = -2)),
        axis.title.y = element_text(margin = margin(r = -2)))

# S_CO2_LI_cal
target = data.frame(
  value = c(slope_gmst_c[[5]], slope_gmst_c[[6]]),
  group = factor(c(
    rep("glacial", length(slope_gmst_c[[5]])),
    rep("interglacial", length(slope_gmst_c[[6]]))
  ))
)
p6 = ggplot(target) +
  geom_density(aes(x = value, fill = group, color = group), alpha = 0.3) +
  scale_fill_manual(values = c(pal[1], pal[2])) +
  scale_color_manual(values = c(pal[1], pal[2])) +
  geom_vline(xintercept = median(slope_gmst_c[[5]]), color = pal[1], linewidth = 1) +
  geom_vline(xintercept = (median(slope_gmst_c[[5]]) - sd(slope_gmst_c[[5]])), 
             color = pal[1], linetype = "dashed") +
  geom_vline(xintercept = (median(slope_gmst_c[[5]]) + sd(slope_gmst_c[[5]])), 
             color = pal[1], linetype = "dashed") +
  geom_vline(xintercept = median(slope_gmst_c[[5]]), color = pal[1], linewidth = 1) +
  geom_vline(xintercept = median(slope_gmst_c[[6]]), color = pal[2], linewidth = 1) +
  geom_vline(xintercept = (median(slope_gmst_c[[6]]) - sd(slope_gmst_c[[6]])), 
             color = pal[2], linetype = "dashed") +
  geom_vline(xintercept = (median(slope_gmst_c[[6]]) + sd(slope_gmst_c[[6]])), 
             color = pal[2], linetype = "dashed") +
  theme_bw() + theme +
  scale_x_continuous(limits = c(0, 3)) +
  labs(x = "slope", 
       y = "density") +
  guides(fill = "none", color = "none") +
  annotate("text", label = expression("S"[total]), x = 2.5, y = 1.4, size = 6) +
  annotate("text", x = 2.5, y = .7, label = expression(italic(d)*" = 0.29")) +
  annotate("text", x = 2.5, y = .5, label = expression(italic(p)*" < 0.001")) +
  annotate("text", x = .2, y = 1.5, size = 5, label = "f", fontface = "bold") +
  theme(axis.title.x = element_text(margin = margin(t = -2)),
        axis.title.y = element_text(margin = margin(r = -2)))

p1 + p2 + p3 + p4 + p5 + p6 +
  plot_layout(ncol = 3)

ggsave("figures/Fig_3_climate_sensitivity_pdfs.pdf", width = 8.7, height = 6.2) 

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



