rm(list = ls())
pacman::p_load(tidyverse, ggpubr, effsize, sn, readxl)
source('plot/functions.R')
set.seed(42)
nsyth = 1e3
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
pal = c("#2171B5", "#D94801")

# read data ----
co2 = read.csv("output/binned_co2.csv")
gmst_clark = read.csv("data/GMST/clark_2024.csv") |>
  mutate(age = age * 1e3) |>
  filter(age <= 2600)
R_LI = read.csv("data/R_LI_koehler2015.csv") |>
  mutate(age = Age) |> 
  filter(age <= 2600)

# filter data
co2_g = filter_g(co2, "age")
Rli_g = filter_g(R_LI, "age") |>
  mutate(time = assign_time_group(age, 300, 2700))
gmst_g = filter_g(gmst_clark, "age") |>
  mutate(time = assign_time_group(age, 300, 2700))
co2_g$Rli = approx(x = Rli_g$age, y = Rli_g$RLI, xout = co2_g$age)$y
co2_g$gmst = approx(x = gmst_g$age, y = gmst_g$GMST, xout = co2_g$age)$y 
co2_ig = filter_ig(co2, "age")
Rli_ig = filter_ig(R_LI, "age") |>
  mutate(time = assign_time_group(age, 300, 2700))
gmst_ig = filter_ig(gmst_clark, "age") |>
  mutate(time = assign_time_group(age, 300, 2700))
co2_ig$Rli = approx(x = Rli_ig$age, y = Rli_ig$RLI, xout = co2_ig$age)$y
co2_ig$gmst = approx(x = gmst_ig$age, y = gmst_ig$GMST, xout = co2_ig$age)$y 

# ECS analyses ----
forcing = data.frame("time" = unique(co2_g$time))
g_co2 = rep(NA, nsyth)
g_co2_li = rep(NA, nsyth)
g_r = rep(NA, nsyth)
g_r_cal = rep(NA, nsyth)
ig_co2 = rep(NA, nsyth)
ig_co2_li = rep(NA, nsyth)
ig_r = rep(NA, nsyth)
ig_r_cal = rep(NA, nsyth)

for (p in 1:nsyth) {
  for (i in 1:nrow(forcing)) {
    # calculate CO2 forcing
    co2_s = co2_g |> filter(time == forcing$time[i])
    co2_iter = sample(co2_s$CO2, 1, replace = TRUE)
    rli_iter = sample(co2_s$Rli, 1, replace = TRUE)
    gmst = sample(co2_s$gmst, 1, replace = TRUE)
    co2_ig_s = co2_ig |> filter(time == forcing$time[i])
    co2_ig_iter = sample(co2_ig_s$CO2, 1, replace = TRUE)
    rli_ig_iter = sample(co2_ig_s$Rli, 1, replace = TRUE)
    gmst_ig = sample(co2_ig_s$gmst, 1, replace = TRUE)
    
    r_co2 = 5.35 * log(co2_iter / 278)
    r_co2_ig = 5.35 * log(co2_ig_iter / 278)
    
    # calculate CO2 + ice sheet
    r_co2_li = (r_co2 + rli_iter)
    r_co2_li_ig = (r_co2_ig + rli_ig_iter)
    
    # a efficacy factor of 0.45 +0.34/-0.2 to correct for ice sheet forcing - Stap (2019)
    median_val = .45
    upper_sigma = .34
    lower_sigma = .20
    location = median_val
    scale = (upper_sigma + lower_sigma) / 4 
    shape = (upper_sigma - lower_sigma) / scale
    eps_li = rsn(nsyth, xi = location, omega = scale, alpha = shape)
    rli_cal = rli_iter * eps_li
    rli_ig_cal = rli_ig_iter * eps_li
    
    # conversion factor of 0.64 +/-0.07 to account for other the influence of other long-term processes
    theta = rnorm(nsyth, .64, .07)
    r_total = theta * r_co2_li
    r_total_cal = theta * (r_co2 + rli_cal)
    r_ig_total = theta * r_co2_li_ig
    r_ig_total_cal = theta * (r_co2_ig + rli_ig_cal)
    
    forcing$r_co2[i] = r_co2
    forcing$r_co2_li[i] = r_co2_li
    forcing$r[i] = r_total
    forcing$r_cal[i] = r_total_cal
    forcing$gmst[i] = gmst
    
    forcing$r_co2_ig[i] = r_co2_ig
    forcing$r_co2_li_ig[i] = r_co2_li_ig
    forcing$r_ig[i] = r_ig_total
    forcing$r_ig_cal[i] = rli_ig_cal
    forcing$gmst_ig[i] = gmst_ig
  }
  m1 = lm(gmst ~ r_co2, forcing)
  g_co2[p] = ifelse(summary(m1)$coefficients[2, "Pr(>|t|)"] < 0.05,
                    m1$coefficients[2], NA)
  m1 = lm(gmst ~ r_co2_li, forcing)
  g_co2_li[p] = ifelse(summary(m1)$coefficients[2, "Pr(>|t|)"] < 0.05,
                       m1$coefficients[2], NA)
  m1 = lm(gmst ~ r, forcing)
  g_r[p] = ifelse(summary(m1)$coefficients[2, "Pr(>|t|)"] < 0.05,
                  m1$coefficients[2], NA)
  m1 = lm(gmst ~ r_cal, forcing)
  g_r_cal[p] = ifelse(summary(m1)$coefficients[2, "Pr(>|t|)"] < 0.05,
                  m1$coefficients[2], NA)
  
  m1 = lm(gmst_ig ~ r_co2_ig, forcing)
  ig_co2[p] = ifelse(summary(m1)$coefficients[2, "Pr(>|t|)"] < 0.05,
                    m1$coefficients[2], NA)
  m1 = lm(gmst_ig ~ r_co2_li_ig, forcing)
  ig_co2_li[p] = ifelse(summary(m1)$coefficients[2, "Pr(>|t|)"] < 0.05,
                       m1$coefficients[2], NA)
  m1 = lm(gmst_ig ~ r_ig, forcing)
  ig_r[p] = ifelse(summary(m1)$coefficients[2, "Pr(>|t|)"] < 0.05,
                  m1$coefficients[2], NA)
  m1 = lm(gmst_ig ~ r_ig_cal, forcing)
  ig_r_cal[p] = ifelse(summary(m1)$coefficients[2, "Pr(>|t|)"] < 0.05,
                      m1$coefficients[2], NA)
  
}
g_co2 = g_co2[g_co2 > 0 & !is.na(g_co2)]
g_co2_li = g_co2_li[g_co2_li > 0 & !is.na(g_co2_li)]
g_r = g_r[g_r > 0 & !is.na(g_r)]
g_r_cal = g_r_cal[g_r_cal > 0 & !is.na(g_r_cal)]
ig_co2 = ig_co2[ig_co2 > 0 & !is.na(ig_co2)]
ig_co2_li = ig_co2_li[ig_co2_li > 0 & !is.na(ig_co2_li)]
ig_r = ig_r[ig_r > 0 & !is.na(ig_r)]
ig_r_cal = ig_r_cal[ig_r_cal > 0 & !is.na(ig_r_cal)]
dat_list = list(g_co2, g_co2_li, g_r, g_r_cal, ig_co2, ig_co2_li, ig_r, ig_r_cal)

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


plot_pdf(dat_list[[1]], dat_list[[5]], "R_CO2",6,0.84)
plot_pdf(dat_list[[2]], dat_list[[6]], "R_CO2_LI",4,1.6)
plot_pdf(dat_list[[3]], dat_list[[7]], "R_total",4,1)
plot_pdf(dat_list[[4]], dat_list[[8]], "R_total_cal",6,1)

