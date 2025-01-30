library(tidyverse)
library(ggpubr)
library(readxl)
source('plot/functions.R')
set.seed(42)
nsyth = 20000
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
g_882 = filter_g(sst_882, "age") %>% mutate(assign_time_group(age, 0.3, 2.7))
ig_882 = filter_ig(sst_882, "age") %>% mutate(assign_time_group(age, 0.3, 2.7))
sst_1208 = read_xlsx("data/marine proxies/MidHighLatitudes.xlsx", sheet = "1208")
g_1208 = filter_g(sst_1208, "age") %>% mutate(assign_time_group(age, 0.3, 2.7))
ig_1208 = filter_ig(sst_1208, "age") %>% mutate(assign_time_group(age, 0.3, 2.7))
sst_1090 = read_xlsx("data/marine proxies/MidHighLatitudes.xlsx", sheet = "1090")
g_1090 = filter_g(sst_1090, "age") %>% mutate(assign_time_group(age, 0.3, 2.7))
ig_1090 = filter_ig(sst_1090, "age") %>% mutate(assign_time_group(age, 0.3, 2.7))
sst_722 = read_xlsx("data/marine proxies/UpwellingZones.xlsx", sheet = "722")
g_722 = filter_g(sst_722, "age") %>% mutate(assign_time_group(age, 0.3, 2.7))
ig_722 = filter_ig(sst_722, "age") %>% mutate(assign_time_group(age, 0.3, 2.7))
sst_1012 = read_xlsx("data/marine proxies/UpwellingZones.xlsx", sheet = "1012")
g_1012 = filter_g(sst_1012, "age") %>% mutate(assign_time_group(age, 0.3, 2.7))
ig_1012 = filter_ig(sst_1012, "age") %>% mutate(assign_time_group(age, 0.3, 2.7))
sst_846 = read_xlsx("data/marine proxies/UpwellingZones.xlsx", sheet = "846")
g_846 = filter_g(sst_846, "age") %>% mutate(assign_time_group(age, 0.3, 2.7))
ig_846 = filter_ig(sst_846, "age") %>% mutate(assign_time_group(age, 0.3, 2.7))
sst_1148 = read_xlsx("data/marine proxies/WP.xlsx", sheet = "1148")
g_1148 = filter_g(sst_1148, "age") %>% mutate(assign_time_group(age, 0.3, 2.7))
ig_1148 = filter_ig(sst_1148, "age") %>% mutate(assign_time_group(age, 0.3, 2.7))
sst_1143 = read.csv("output/binned_wpwp.csv")
g_1143 = sst_1143 %>% filter(period == "glacial")
g_1143 = g_1143[, 2:4]
ig_1143 = sst_1143 %>% filter(period == "interglacial")
ig_1143 = ig_1143[, 2:4]
bwt = read.csv("output/binned_bwt.csv")
g_bwt = bwt %>% filter(period == "glacial")
g_bwt = g_bwt[, 2:4]
colnames(g_bwt)[2] = "SST"
ig_bwt = bwt %>% filter(period == "interglacial")
ig_bwt = ig_bwt[, 2:4]
colnames(ig_bwt)[2] = "SST"

# PDFs of regression slopes ----
slope_pdf = function(forcing, param){
  pdf_s = rep(0, nsyth)
  for (i in 1:nsyth) {
    subsample = data.frame(matrix(nrow = nrow(forcing), ncol = 2))
    for (p in 1:nrow(forcing)) {
      r_s = forcing[p,]
      subsample[p, 1] = rnorm(1, mean = r_s$r, sd = r_s$r.sd)
      sst_s = param %>% filter(param[[3]] == forcing$time[p])
      sst_mean = mean(sst_s$SST)
      sst_sd = sd(sst_s$SST)
      subsample[p, 2] = rnorm(1, mean = sst_mean, sd = sst_sd)
      # subsample[p, 2] = sample(sst_s$SST, 1)
    }
    m1 = lm(X2 ~ X1, subsample)
    pdf_s[i] = ifelse(summary(m1)$coefficients[2, "Pr(>|t|)"] < 0.05,
                      m1$coefficients[2], NA)
  }
  return(pdf_s)
}

slope_882 = data.frame(matrix(nrow = nsyth, ncol = 2))
names(slope_882) = c("glacial", "interglacial")
slope_882$glacial = slope_pdf(r_g, g_882)
slope_882$interglacial = slope_pdf(r_ig, ig_882) 

slope_1208 = data.frame(matrix(nrow = nsyth, ncol = 2))
names(slope_1208) = c("glacial", "interglacial")
slope_1208$glacial = slope_pdf(r_g, g_1208)
slope_1208$interglacial = slope_pdf(r_ig, ig_1208) 

slope_1090 = data.frame(matrix(nrow = nsyth, ncol = 2))
names(slope_1090) = c("glacial", "interglacial")
slope_1090$glacial = slope_pdf(r_g, g_1090)
slope_1090$interglacial = slope_pdf(r_ig, ig_1090) 

slope_722 = data.frame(matrix(nrow = nsyth, ncol = 2))
names(slope_722) = c("glacial", "interglacial")
slope_722$glacial = slope_pdf(r_g, g_722)
slope_722$interglacial = slope_pdf(r_ig, ig_722) 

slope_1012 = data.frame(matrix(nrow = nsyth, ncol = 2))
names(slope_1012) = c("glacial", "interglacial")
slope_1012$glacial = slope_pdf(r_g, g_1012)
slope_1012$interglacial = slope_pdf(r_ig, ig_1012) 

slope_846 = data.frame(matrix(nrow = nsyth, ncol = 2))
names(slope_846) = c("glacial", "interglacial")
slope_846$glacial = slope_pdf(r_g, g_846)
slope_846$interglacial = slope_pdf(r_ig, ig_846) 

slope_1143 = data.frame(matrix(nrow = nsyth, ncol = 2))
names(slope_1143) = c("glacial", "interglacial")
slope_1143$glacial = slope_pdf(r_g, g_1143)
slope_1143$interglacial = slope_pdf(r_ig, ig_1143) 

slope_1148 = data.frame(matrix(nrow = nsyth, ncol = 2))
names(slope_1148) = c("glacial", "interglacial")
slope_1148$glacial = slope_pdf(r_g, g_1148)
slope_1148$interglacial = slope_pdf(r_ig, ig_1148) 

slope_bwt = data.frame(matrix(nrow = nsyth, ncol = 2))
names(slope_bwt) = c("glacial", "interglacial")
slope_bwt$glacial = slope_pdf(r_g, g_bwt)
slope_bwt$interglacial = slope_pdf(r_ig, ig_bwt) 

# plot ---
slope_882 = slope_882 %>%
  mutate(interglacial = ifelse(interglacial < 0, NA, interglacial))
t1 = t.test(slope_882$glacial, slope_882$interglacial)
t1$p.value
p1 = ggplot(slope_882) +
  geom_density(aes(x = glacial), color = "#2171B5", fill = "#2171B5", size = 1, alpha = 0.3) +
  geom_density(aes(x = interglacial), color = "#D94801", fill = "#D94801", size = 1, alpha = 0.2) +
  geom_vline(xintercept = median(slope_882$glacial, na.rm = TRUE), color = "#2171B5", size = 1, linetype = "dashed") +
  geom_vline(xintercept = median(slope_882$interglacial, na.rm = TRUE), color = "#D94801", size = 1, linetype = "dashed") +
  theme_bw() + theme +
  labs(x = "slope") +
  labs(y = "density") +
  scale_x_continuous(limits = c(0, 10)) +
  annotate("text", label = "882", x = 1, y = 0.3) +
  annotate("text", label = expression(italic(p)*" < 0.001"), x = 7.5, y = 0.3)

t1 = t.test(slope_1208$glacial, slope_1208$interglacial)
t1$p.value
p2 = ggplot(slope_1208) +
  geom_density(aes(x = glacial), color = "#2171B5", fill = "#2171B5", size = 1, alpha = 0.3) +
  geom_density(aes(x = interglacial), color = "#D94801", fill = "#D94801", size = 1, alpha = 0.2) +
  geom_vline(xintercept = median(slope_1208$glacial, na.rm = TRUE), color = "#2171B5", size = 1, linetype = "dashed") +
  geom_vline(xintercept = median(slope_1208$interglacial, na.rm = TRUE), color = "#D94801", size = 1, linetype = "dashed") +
  theme_bw() + theme +
  labs(x = "slope") +
  labs(y = "density") +
  scale_x_continuous(limits = c(0, 6)) +
  annotate("text", label = "1208", x = 0.5, y = 0.8) +
  annotate("text", label = expression(italic(p)*" < 0.001"), x = 4, y = 0.8)

t1 = t.test(slope_1090$glacial, slope_1090$interglacial)
t1$p.value
p3 = ggplot(slope_1090) +
  geom_density(aes(x = glacial), color = "#2171B5", fill = "#2171B5", size = 1, alpha = 0.3) +
  geom_density(aes(x = interglacial), color = "#D94801", fill = "#D94801", size = 1, alpha = 0.2) +
  geom_vline(xintercept = median(slope_1090$glacial, na.rm = TRUE), color = "#2171B5", size = 1, linetype = "dashed") +
  geom_vline(xintercept = median(slope_1090$interglacial, na.rm = TRUE), color = "#D94801", size = 1, linetype = "dashed") +
  theme_bw() + theme +
  labs(x = "slope") +
  labs(y = "density") +
  scale_x_continuous(limits = c(0, 6)) +
  annotate("text", label = "1090", x = 0.5, y = 0.7) +
  annotate("text", label = expression(italic(p)*" < 0.001"), x = 4, y = 0.7)

t1 = t.test(slope_722$glacial, slope_722$interglacial)
t1$p.value
p4 = ggplot(slope_722) +
  geom_density(aes(x = glacial), color = "#2171B5", fill = "#2171B5", size = 1, alpha = 0.3) +
  geom_density(aes(x = interglacial), color = "#D94801", fill = "#D94801", size = 1, alpha = 0.2) +
  geom_vline(xintercept = median(slope_722$glacial, na.rm = TRUE), color = "#2171B5", size = 1, linetype = "dashed") +
  geom_vline(xintercept = median(slope_722$interglacial, na.rm = TRUE), color = "#D94801", size = 1, linetype = "dashed") +
  theme_bw() + theme +
  labs(x = "slope") +
  labs(y = "density") +
  scale_x_continuous(limits = c(0, 4)) +
  annotate("text", label = "722", x = 0.5, y = 1) +
  annotate("text", label = expression(italic(p)*" = 0.206"), x = 3, y = 1)

t1 = t.test(slope_1012$glacial, slope_1012$interglacial)
slope_1012 = slope_1012 %>%
  mutate(interglacial = ifelse(interglacial < 0, NA, interglacial))
t1$p.value
p5 = ggplot(slope_1012) +
  geom_density(aes(x = glacial), color = "#2171B5", fill = "#2171B5", size = 1, alpha = 0.3) +
  geom_density(aes(x = interglacial), color = "#D94801", fill = "#D94801", size = 1, alpha = 0.2) +
  geom_vline(xintercept = median(slope_1012$glacial, na.rm = TRUE), color = "#2171B5", size = 1, linetype = "dashed") +
  geom_vline(xintercept = median(slope_1012$interglacial, na.rm = TRUE), color = "#D94801", size = 1, linetype = "dashed") +
  theme_bw() + theme +
  labs(x = "slope") +
  labs(y = "density") +
  scale_x_continuous(limits = c(0, 5)) +
  annotate("text", label = "1012", x = 0.5, y = 0.7) +
  annotate("text", label = expression(italic(p)*" = 0.097"), x = 3.5, y = 0.7)

t1 = t.test(slope_846$glacial, slope_846$interglacial)
t1$p.value
p6 = ggplot(slope_846) +
  geom_density(aes(x = glacial), color = "#2171B5", fill = "#2171B5", size = 1, alpha = 0.3) +
  geom_density(aes(x = interglacial), color = "#D94801", fill = "#D94801", size = 1, alpha = 0.2) +
  geom_vline(xintercept = median(slope_846$glacial, na.rm = TRUE), color = "#2171B5", size = 1, linetype = "dashed") +
  geom_vline(xintercept = median(slope_846$interglacial, na.rm = TRUE), color = "#D94801", size = 1, linetype = "dashed") +
  theme_bw() + theme +
  labs(x = "slope") +
  labs(y = "density") +
  scale_x_continuous(limits = c(0, 4)) +
  annotate("text", label = "846", x = 0.5, y = 1) +
  annotate("text", label = expression(italic(p)*" < 0.001"), x = 3.5, y = 1)

t1 = t.test(slope_1143$glacial, slope_1143$interglacial)
t1$p.value
p7 = ggplot(slope_1143) +
  geom_density(aes(x = glacial), color = "#2171B5", fill = "#2171B5", size = 1, alpha = 0.3) +
  geom_density(aes(x = interglacial), color = "#D94801", fill = "#D94801", size = 1, alpha = 0.2) +
  geom_vline(xintercept = median(slope_1143$glacial, na.rm = TRUE), color = "#2171B5", size = 1, linetype = "dashed") +
  geom_vline(xintercept = median(slope_1143$interglacial, na.rm = TRUE), color = "#D94801", size = 1, linetype = "dashed") +
  theme_bw() + theme +
  labs(x = "slope") +
  labs(y = "density") +
  # scale_x_continuous(limits = c(0, 3)) +
  annotate("text", label = "1143", x = 0.4, y = 1) +
  annotate("text", label = expression(italic(p)*" < 0.001"), x = 2.5, y = 1)

slope_1148 = slope_1148 %>%
  mutate(interglacial = ifelse(interglacial < 0, NA, interglacial),
         glacial = ifelse(glaical < 0, NA, glacial))
t1 = t.test(slope_1148$glacial, slope_1148$interglacial)
t1$p.value
p8 = ggplot(slope_1148) +
  geom_density(aes(x = glacial), color = "#2171B5", fill = "#2171B5", size = 1, alpha = 0.3) +
  geom_density(aes(x = interglacial), color = "#D94801", fill = "#D94801", size = 1, alpha = 0.2) +
  geom_vline(xintercept = mean(slope_1148$glacial, na.rm = TRUE), color = "#2171B5", size = 1, linetype = "dashed") +
  geom_vline(xintercept = mean(slope_1148$interglacial, na.rm = TRUE), color = "#D94801", size = 1, linetype = "dashed") +
  theme_bw() + theme +
  labs(x = "slope") +
  labs(y = "density") +
  scale_x_continuous(limits = c(0, 5)) +
  annotate("text", label = "1148", x = 0.5, y = 0.7) +
  annotate("text", label = expression(italic(p)*" < 0.001"), x = 4, y = 0.7)

t1 = t.test(slope_bwt$glacial, slope_bwt$interglacial)
t1$p.value
ggplot(slope_bwt) +
  geom_density(aes(x = glacial), color = "#2171B5", fill = "#2171B5", size = 1, alpha = 0.3) +
  geom_density(aes(x = interglacial), color = "#D94801", fill = "#D94801", size = 1, alpha = 0.2) +
  geom_vline(xintercept = median(slope_bwt$glacial, na.rm = TRUE), color = "#2171B5", size = 1, linetype = "dashed") +
  geom_vline(xintercept = median(slope_bwt$interglacial, na.rm = TRUE), color = "#D94801", size = 1, linetype = "dashed") +
  theme_bw() + theme +
  labs(x = "slope") +
  labs(y = "density") +
  scale_x_continuous(limits = c(0, 3)) +
  annotate("text", label = "BWT", x = 0.4, y = 1) +
  annotate("text", label = expression(italic(p)*" < 0.001"), x = 2.5, y = 1)

ggarrange(p1, p2, p3, p4, p5, p6, p7, p8, p9, nrow = 3, ncol = 3, align = "hv",
        labels = c("a", "b", "c", "d", "e", "f", "g", "h", "i"))
ggsave("figures/Fig_4_SST_sensitivities.pdf", width = 6.8, height = 6.8)
