library(tidyverse)
library(ggpubr)
library(readxl)
library(effsize)
source('plot/functions.R')
set.seed(42)
nsyth = 50000
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
dat_g = read.csv("output/climate_sensitivity_glacial.csv")
r_g = dat_g[, c("time", "r", "r.sd")]
dat_ig = read.csv("output/climate_sensitivity_interglacial.csv")
r_ig = dat_ig[, c("time", "r", "r.sd")]
sst_882 = read_xlsx("data/marine proxies/MidHighLatitudes.xlsx", sheet = "882")
sst_1208 = read_xlsx("data/marine proxies/MidHighLatitudes.xlsx", sheet = "1208")
sst_1090 = read_xlsx("data/marine proxies/MidHighLatitudes.xlsx", sheet = "1090")
sst_722 = read_xlsx("data/marine proxies/UpwellingZones.xlsx", sheet = "722")
sst_1012 = read_xlsx("data/marine proxies/UpwellingZones.xlsx", sheet = "1012")
sst_846 = read_xlsx("data/marine proxies/UpwellingZones.xlsx", sheet = "846")
sst_1148 = read_xlsx("data/marine proxies/WP.xlsx", sheet = "1148")

# calculating the statistics of each time period ----
bin_stat = function(num){
  df_g = filter_g(get(paste0("sst_", num)), "age") %>% 
    mutate(time = assign_time_group(age, 300, 2700)) %>%
    group_by(time) %>% 
    summarise(sst = mean(SST), sst.sd = sd(SST), count = n())
  df_g$period = "glacial"
  df_g = left_join(r_g, df_g, by = "time")
  df_ig = filter_ig(get(paste0("sst_", num)), "age") %>% 
    mutate(time = assign_time_group(age, 300, 2700)) %>%
    group_by(time) %>% 
    summarise(sst = mean(SST), sst.sd = sd(SST), count = n())
  df_ig$period = "interglacial"
  df_ig = left_join(r_ig, df_ig, by = "time")
  dat = rbind(df_g, df_ig)
  return(dat)
}

slope_882 = bin_stat(882)
slope_1208 = bin_stat(1208)
slope_1090 = bin_stat(1090)
slope_722 = bin_stat(722)
slope_1012 = bin_stat(1012)
slope_846 = bin_stat(846)
slope_1148 = bin_stat(1148)
slope_1143_g = dat_g[, c("time", "r", "r.sd", "sst", "sst.sd")] %>% 
  mutate(period = "glacial")
slope_1143_ig = dat_ig[, c("time", "r", "r.sd", "sst", "sst.sd")] %>% 
  mutate(period = "interglacial")
slope_1143 = rbind(slope_1143_g, slope_1143_ig)
slope_bwt_g = dat_g[, c("time", "r", "r.sd", "bwt", "bwt.sd")] %>% 
  mutate(period = "glacial")
slope_bwt_ig = dat_ig[, c("time", "r", "r.sd", "bwt", "bwt.sd")] %>% 
  mutate(period = "interglacial")
slope_bwt = rbind(slope_bwt_g, slope_bwt_ig) %>%
  rename(sst = bwt, sst.sd = bwt.sd)

# Forcing vs Temperature plot ----
pal = c("#2171B5", "#D94801")
xmin = round(min(dat_g$r - dat_g$r.sd)-0.1,1)
xmax = round(max(dat_ig$r + dat_ig$r.sd),1)
plot_g = function(dat){
  ymin = min(dat$sst - dat$sst.sd)
  ymax = max(dat$sst + dat$sst.sd)
  dat = dat %>% filter(period == "glacial")
  ggplot(dat, aes(x = r, y = sst)) +
    geom_smooth(method = "lm", formula = y ~ x, show.legend = F, span = 1, alpha = 0.1, linetype = "dashed", color = pal[1], fill = pal[1]) +
    geom_errorbar(aes(xmin = r - r.sd, xmax = r + r.sd), width = 0, linewidth = 0.2) +
    geom_errorbar(aes(ymin = sst - sst.sd, ymax = sst + sst.sd), width = 0, linewidth = 0.2) +
    geom_point(aes(fill = time), shape = 21, size = 3) +
    scale_fill_brewer(palette = 1) +
    theme_bw() + theme +
    scale_x_continuous(limits = c(xmin, xmax)) +
    scale_y_continuous(limits = c(ymin, ymax)) +
    labs(x = expression(Delta*"F"*" (W m"^"-2"*")"),
         y = expression(paste("SST (", degree, "C)")))
}
p1 = plot_g(slope_882)
p2 = plot_g(slope_1208)
p3 = plot_g(slope_1090)
p4 = plot_g(slope_722)
p5 = plot_g(slope_1012)
p6 = plot_g(slope_846)
p7 = plot_g(slope_1148)
p8 = plot_g(slope_1143)
p9 = plot_g(slope_bwt)
ggarrange(p1, p2, p3, p4, p5, p6, p7, p8, p9, nrow = 3, ncol = 3, align = "hv",
          labels = c("a", "b", "c", "d", "e", "f", "g", "h", "i"), common.legend = TRUE)

# generating pdfs of the regression slopes ----
slope_pdf = function(dat) {
  slope = data.frame(matrix(nrow = nsyth, ncol = 2))
  names(slope) = c("glacial", "interglacial")
  for (i in 1:nsyth) {
    df_g = dat %>% filter(period == "glacial")
    df_ig = dat %>% filter(period == "interglacial")
    subsample = data.frame(matrix(nrow = nrow(df_g), ncol = 5))
    names(subsample) = c("time", "r.g", "sst.g", "r.ig", "sst.ig")
    subsample$time = df_g$time
    for (p in 1:nrow(df_g)) {
      subsample$r.g[p] = rnorm(1, mean = df_g$r[p], sd = df_g$r.sd[p])
      subsample$sst.g[p] = rnorm(1, mean = df_g$sst[p], sd = df_g$sst.sd[p])
      subsample$r.ig[p] = rnorm(1, mean = df_ig$r[p], sd = df_ig$r.sd[p])
      subsample$sst.ig[p] = rnorm(1, mean = df_ig$sst[p], sd = df_ig$sst.sd[p])
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

pdf_882 = slope_pdf(slope_882)
pdf_1208 = slope_pdf(slope_1208)
pdf_1090 = slope_pdf(slope_1090)
pdf_722 = slope_pdf(slope_722)
pdf_1012 = slope_pdf(slope_1012)
pdf_846 = slope_pdf(slope_846)
pdf_1148 = slope_pdf(slope_1148)
pdf_1143 = slope_pdf(slope_1143)
pdf_bwt = slope_pdf(slope_bwt)


# plot ---
pal = c("#2171B5", "#D94801")
pdf_882_clean = pdf_882 %>% 
  filter(!is.na(glacial) & !is.na(interglacial)) %>%
  filter(interglacial >= 0 & glacial >= 0)
cohen.d(pdf_882_clean$glacial, pdf_882_clean$interglacial)
p1 = ggplot(pdf_882_clean) +
  geom_density(aes(x = glacial), color = pal[1], fill = pal[1], size = 1, alpha = 0.3) +
  geom_density(aes(x = interglacial), color = pal[2], fill = pal[2], size = 1, alpha = 0.2) +
  geom_vline(xintercept = median(pdf_882_clean$glacial), color = pal[1], size = 1, linetype = "dashed") +
  geom_vline(xintercept = median(pdf_882_clean$interglacial), color = pal[2], size = 1, linetype = "dashed") +
  theme_bw() + theme +
  labs(x = "slope") +
  labs(y = "density") +
  scale_x_continuous(limits = c(0, 10)) +
  annotate("text", label = "882", x = 7.5, y = 0.4) +
  annotate("text", label = round(median(pdf_882_clean$interglacial), 2), x = 7.5, y = 0.3, color = pal[2]) +
  annotate("text", label = round(median(pdf_882_clean$glacial), 2), x = 7.5, y = 0.25, color = pal[1]) +
  annotate("text", label = paste("d =", round(cohen.d(pdf_882_clean$glacial, pdf_882_clean$interglacial)$estimate, 2)), 
           x = 7.5, y = 0.2)

pdf_1208_clean = pdf_1208 %>% 
  filter(!is.na(glacial) & !is.na(interglacial)) %>%
  filter(interglacial >= 0 & glacial >= 0)
cohen.d(pdf_1208_clean$glacial, pdf_1208_clean$interglacial)
p2 = ggplot(pdf_1208_clean) +
  geom_density(aes(x = glacial), color = pal[1], fill = pal[1], size = 1, alpha = 0.3) +
  geom_density(aes(x = interglacial), color = pal[2], fill = pal[2], size = 1, alpha = 0.2) +
  geom_vline(xintercept = median(pdf_1208_clean$glacial), color = pal[1], size = 1, linetype = "dashed") +
  geom_vline(xintercept = median(pdf_1208_clean$interglacial), color = pal[2], size = 1, linetype = "dashed") +
  theme_bw() + theme +
  labs(x = "slope") +
  labs(y = "density") +
  scale_x_continuous(limits = c(0, 6)) +
  annotate("text", label = "1208", x = 5, y = 0.8) +
  annotate("text", label = round(median(pdf_1208_clean$interglacial), 2), x = 5, y = 0.5, color = pal[2]) +
  annotate("text", label = round(median(pdf_1208_clean$glacial), 2), x = 5, y = 0.4, color = pal[1]) +
  annotate("text", label = paste("d =", abs(round(cohen.d(pdf_1208_clean$glacial, pdf_1208_clean$interglacial)$estimate, 2))), 
           x = 5, y = 0.3)

pdf_1090_clean = pdf_1090 %>% 
  filter(!is.na(glacial) & !is.na(interglacial)) %>%
  filter(interglacial >= 0 & glacial >= 0)
cohen.d(pdf_1090_clean$glacial, pdf_1090_clean$interglacial)
p3 = ggplot(pdf_1090_clean) +
  geom_density(aes(x = glacial), color = pal[1], fill = pal[1], size = 1, alpha = 0.3) +
  geom_density(aes(x = interglacial), color = pal[2], fill = pal[2], size = 1, alpha = 0.2) +
  geom_vline(xintercept = median(pdf_1090_clean$glacial), color = pal[1], size = 1, linetype = "dashed") +
  geom_vline(xintercept = median(pdf_1090_clean$interglacial), color = pal[2], size = 1, linetype = "dashed") +
  theme_bw() + theme +
  labs(x = "slope") +
  labs(y = "density") +
  scale_x_continuous(limits = c(0, 6)) +
  annotate("text", label = "1090", x = 5, y = 0.8) +
  annotate("text", label = round(median(pdf_1090_clean$interglacial), 2), x = 5, y = 0.5, color = pal[2]) +
  annotate("text", label = round(median(pdf_1090_clean$glacial), 2), x = 5, y = 0.4, color = pal[1]) +
  annotate("text", label = paste("d =", abs(round(cohen.d(pdf_1090_clean$glacial, pdf_1090_clean$interglacial)$estimate, 2))), 
           x = 5, y = 0.3)

pdf_722_clean = pdf_722 %>% 
  filter(!is.na(glacial) & !is.na(interglacial)) %>%
  filter(interglacial >= 0 & glacial >= 0)
cohen.d(pdf_722_clean$glacial, pdf_722_clean$interglacial)
p4 = ggplot(pdf_722_clean) +
  geom_density(aes(x = glacial), color = pal[1], fill = pal[1], size = 1, alpha = 0.3) +
  geom_density(aes(x = interglacial), color = pal[2], fill = pal[2], size = 1, alpha = 0.2) +
  geom_vline(xintercept = median(pdf_722_clean$glacial), color = pal[1], size = 1, linetype = "dashed") +
  geom_vline(xintercept = median(pdf_722_clean$interglacial), color = pal[2], size = 1, linetype = "dashed") +
  theme_bw() + theme +
  labs(x = "slope") +
  labs(y = "density") +
  scale_x_continuous(limits = c(0, 4)) +
  annotate("text", label = "722", x = 3, y = 1) +
  annotate("text", label = round(median(pdf_722_clean$interglacial), 2), x = 3, y = 0.7, color = pal[2]) +
  annotate("text", label = round(median(pdf_722_clean$glacial), 2), x = 3, y = 0.6, color = pal[1]) +
  annotate("text", label = paste("d =", abs(round(cohen.d(pdf_722_clean$glacial, pdf_722_clean$interglacial)$estimate, 2))), 
           x = 3, y = 0.5)

pdf_1012_clean = pdf_1012 %>% 
  filter(!is.na(glacial) & !is.na(interglacial)) %>%
  filter(interglacial >= 0 & glacial >= 0)
cohen.d(pdf_1012_clean$glacial, pdf_1012_clean$interglacial)
p5 = ggplot(pdf_1012_clean) +
  geom_density(aes(x = glacial), color = pal[1], fill = pal[1], size = 1, alpha = 0.3) +
  geom_density(aes(x = interglacial), color = pal[2], fill = pal[2], size = 1, alpha = 0.2) +
  geom_vline(xintercept = median(pdf_1012_clean$glacial), color = pal[1], size = 1, linetype = "dashed") +
  geom_vline(xintercept = median(pdf_1012_clean$interglacial), color = pal[2], size = 1, linetype = "dashed") +
  theme_bw() + theme +
  labs(x = "slope") +
  labs(y = "density") +
  scale_x_continuous(limits = c(0, 6)) +
  annotate("text", label = "1012", x = 5, y = 0.7) +
  annotate("text", label = round(median(pdf_1012_clean$interglacial), 2), x = 5, y = 0.5, color = pal[2]) +
  annotate("text", label = round(median(pdf_1012_clean$glacial), 2), x = 5, y = 0.4, color = pal[1]) +
  annotate("text", label = paste("d =", abs(round(cohen.d(pdf_1012_clean$glacial, pdf_1012_clean$interglacial)$estimate, 2))), 
           x = 5, y = 0.3)

pdf_846_clean = pdf_846 %>% 
  filter(!is.na(glacial) & !is.na(interglacial)) %>%
  filter(interglacial >= 0 & glacial >= 0)
cohen.d(pdf_846_clean$glacial, pdf_846_clean$interglacial)
p6 = ggplot(pdf_846_clean) +
  geom_density(aes(x = glacial), color = pal[1], fill = pal[1], size = 1, alpha = 0.3) +
  geom_density(aes(x = interglacial), color = pal[2], fill = pal[2], size = 1, alpha = 0.2) +
  geom_vline(xintercept = median(pdf_846_clean$glacial), color = pal[1], size = 1, linetype = "dashed") +
  geom_vline(xintercept = median(pdf_846_clean$interglacial), color = pal[2], size = 1, linetype = "dashed") +
  theme_bw() + theme +
  labs(x = "slope") +
  labs(y = "density") +
  scale_x_continuous(limits = c(0, 5)) +
  annotate("text", label = "846", x = 4, y = 1.2) +
  annotate("text", label = round(median(pdf_846_clean$interglacial), 2), x = 4, y = 0.8, color = pal[2]) +
  annotate("text", label = round(median(pdf_846_clean$glacial), 2), x = 4, y = 0.7, color = pal[1]) +
  annotate("text", label = paste("d =", abs(round(cohen.d(pdf_846_clean$glacial, pdf_846_clean$interglacial)$estimate, 2))), 
           x = 4, y = 0.6)

pdf_1143_clean = pdf_1143 %>% 
  filter(!is.na(glacial) & !is.na(interglacial)) %>%
  filter(interglacial >= 0 & glacial >= 0)
cohen.d(pdf_1143_clean$glacial, pdf_1143_clean$interglacial)
p7 = ggplot(pdf_1143_clean) +
  geom_density(aes(x = glacial), color = pal[1], fill = pal[1], size = 1, alpha = 0.3) +
  geom_density(aes(x = interglacial), color = pal[2], fill = pal[2], size = 1, alpha = 0.2) +
  geom_vline(xintercept = median(pdf_1143_clean$glacial), color = pal[1], size = 1, linetype = "dashed") +
  geom_vline(xintercept = median(pdf_1143_clean$interglacial), color = pal[2], size = 1, linetype = "dashed") +
  theme_bw() + theme +
  labs(x = "slope") +
  labs(y = "density") +
  scale_x_continuous(limits = c(0, 4)) +
  annotate("text", label = "1143", x = 3, y = 1) +
  annotate("text", label = round(median(pdf_1143_clean$interglacial), 2), x = 3, y = 0.8, color = pal[2]) +
  annotate("text", label = round(median(pdf_1143_clean$glacial), 2), x = 3, y = 0.7, color = pal[1]) +
  annotate("text", label = paste("d =", abs(round(cohen.d(pdf_1143_clean$glacial, pdf_1143_clean$interglacial)$estimate, 2))), 
           x = 3, y = 0.6)

pdf_1148_clean = pdf_1148 %>% 
  filter(!is.na(glacial) & !is.na(interglacial)) %>%
  filter(interglacial >= 0 & glacial >= 0)
cohen.d(pdf_1148_clean$glacial, pdf_1148_clean$interglacial)
p8 = ggplot(pdf_1148_clean) +
  geom_density(aes(x = glacial), color = pal[1], fill = pal[1], size = 1, alpha = 0.3) +
  geom_density(aes(x = interglacial), color = pal[2], fill = pal[2], size = 1, alpha = 0.2) +
  geom_vline(xintercept = median(pdf_1148_clean$glacial), color = pal[1], size = 1, linetype = "dashed") +
  geom_vline(xintercept = median(pdf_1148_clean$interglacial), color = pal[2], size = 1, linetype = "dashed") +
  theme_bw() + theme +
  labs(x = "slope") +
  labs(y = "density") +
  scale_x_continuous(limits = c(0, 5)) +
  annotate("text", label = "1148", x = 4, y = 0.7) +
  annotate("text", label = round(median(pdf_1148_clean$interglacial), 2), x = 4, y = 0.6, color = pal[2]) +
  annotate("text", label = round(median(pdf_1148_clean$glacial), 2), x = 4, y = 0.5, color = pal[1]) +
  annotate("text", label = paste("d =", abs(round(cohen.d(pdf_1148_clean$glacial, pdf_1148_clean$interglacial)$estimate, 2))), 
           x = 4, y = 0.4)

pdf_bwt_clean = pdf_bwt %>% 
  filter(!is.na(glacial) & !is.na(interglacial)) %>%
  filter(interglacial >= 0 & glacial >= 0)
cohen.d(pdf_bwt_clean$glacial, pdf_bwt_clean$interglacial)
p9 = ggplot(pdf_bwt_clean) +
  geom_density(aes(x = glacial), color = pal[1], fill = pal[1], size = 1, alpha = 0.3) +
  geom_density(aes(x = interglacial), color = pal[2], fill = pal[2], size = 1, alpha = 0.2) +
  geom_vline(xintercept = median(pdf_bwt_clean$glacial), color = pal[1], size = 1, linetype = "dashed") +
  geom_vline(xintercept = median(pdf_bwt_clean$interglacial), color = pal[2], size = 1, linetype = "dashed") +
  theme_bw() + theme +
  labs(x = "slope") +
  labs(y = "density") +
  scale_x_continuous(limits = c(0, 4)) +
  annotate("text", label = "BWT", x = 3, y = 1) +
  annotate("text", label = round(median(pdf_bwt_clean$interglacial), 2), x = 3, y = 0.9, color = pal[2]) +
  annotate("text", label = round(median(pdf_bwt_clean$glacial), 2), x = 3, y = 0.8, color = pal[1]) +
  annotate("text", label = paste("d =", abs(round(cohen.d(pdf_bwt_clean$glacial, pdf_bwt_clean$interglacial)$estimate, 2))), 
           x = 3, y = 0.7)

ggarrange(p1, p2, p3, p4, p5, p6, p7, p8, p9, nrow = 3, ncol = 3, align = "hv",
        labels = c("a", "b", "c", "d", "e", "f", "g", "h", "i"))
ggsave("figures/Fig_4_SST_sensitivities.pdf", width = 6.8, height = 6.8)
