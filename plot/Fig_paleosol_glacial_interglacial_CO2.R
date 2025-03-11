library(tidyverse)
library(readxl)
library(ggpubr)
source('plot/functions.R')
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
ice = read_xlsx("data/ice_core_co2.xlsx")
# paleosol data ---
dat = read_xlsx("data/Dataset S1.xlsx", sheet = 2)
dat_g = dat[, c(1, 4, 17:19)] %>% drop_na()
names(dat_g) = c("site", "age", "CO2", "lower", "higher")
dat_g$period = "glacial"
dat = read_xlsx("data/co2_compilation/paleosol_interglacial_CO2_da2019.xlsx", sheet = 2)
dat_ig = dat[4:nrow(dat), c(4, 15:17)] %>% drop_na()
names(dat_ig) = c("age", "CO2", "lower", "higher")
dat_ig[] = lapply(dat_ig, as.numeric)
dat_ig$age = dat_ig$age * 1000
dat_ig$site = "Luochuan"
dat_ig$period = "interglacial"
dat = rbind(dat_g, dat_ig)
# plot
p1 = ggplot(dat, aes(x = age/1000, y = CO2)) +
  geom_line(data = ice, aes(x = age/1000, y = co2)) +
  geom_errorbar(aes(ymin = lower, ymax = higher), linewidth = 0.2, width = 0, color = "gray") +
  geom_point(aes(color = site), shape = 21, size = 3, fill = "white") +
  geom_hline(yintercept = 180, linetype = "dashed") +
  geom_hline(yintercept = 300, linetype = "dashed") +
  theme_bw() + theme +
  scale_color_brewer(palette = "Paired") +
  labs(x = "Age (Ma)", y = expression("CO"[2]*" (ppmv)"), fill = "") +
  scale_x_continuous(breaks = seq(0, 2.5, 0.5), limits = c(0, 2.6)) +
  scale_y_continuous(breaks = seq(0, 1000, 100), limits = c(0, 1000))

p1 = ggplot(dat, aes(x = age/1000, y = CO2)) +
  geom_errorbar(aes(ymin = lower, ymax = higher), linewidth = 0.2, width = 0, color = "gray") +
  geom_line(data = ice, aes(x = age/1000, y = co2)) +
  geom_point(aes(color = period), shape = 21, size = 2, fill = "white") +
  geom_smooth(aes(color = period), size = 1, se = FALSE, span = 0.5) +
  geom_hline(yintercept = 180, linetype = "dashed") +
  geom_hline(yintercept = 300, linetype = "dashed") +
  theme_bw() + theme +
  scale_color_manual(values = c("dodgerblue", "firebrick1")) +
  labs(x = "Age (Ma)", y = expression("CO"[2]*" (ppmv)"), fill = "") +
  scale_x_continuous(breaks = seq(0, 2.5, 0.5), limits = c(0, 2.6)) +
  scale_y_continuous(breaks = seq(0, 700, 100), limits = c(0, 700))

# other proxies ----
GMST = read_csv("data/GMST.csv") %>% filter(age <= 2.6)
proxy = read_xlsx("data/co2_compilation/marine_co2_data.xlsx")
proxy = proxy[, c(3,4,7,8,9,11)]
names(proxy) = c("proxy", "age", "co2", "higher", "lower", "doi")
proxy_ig = filter_ig(proxy, "age") 
proxy_g = filter_g(proxy, "age")
ggplot(proxy, aes(x = age/1000, y = co2)) +
  geom_errorbar(aes(ymin = lower, ymax = higher), width = 0, color = "gray", linewidth = 0.2) +
  geom_point(aes(fill = proxy), size = 2, shape = 21) +
  scale_fill_brewer(palette = "Paired") +
  geom_hline(yintercept = 180, linetype = "dashed") +
  geom_hline(yintercept = 300, linetype = "dashed") +
  theme_bw() + theme +
  labs(x = "Age (Ma)", y = expression("CO"[2]*" (ppmv)")) +
  scale_x_continuous(breaks = seq(0, 2.5, 0.5),limits = c(0, 2.6)) +
  scale_y_continuous(breaks = seq(0, 1000, 100))

proxy_g$time = "glacial"
proxy_ig$time = "interglacial"
proxy = rbind(proxy_g, proxy_ig)
p2 = ggplot(proxy, aes(x = age/1000, y = co2, color = time)) +
  geom_errorbar(aes(ymin = lower, ymax = higher), width = 0, color = "gray", linewidth = 0.2) +
  geom_point(size = 2, shape = 21, fill = "white") +
  geom_smooth(size = 1, se = FALSE, span = 0.4) +
  scale_color_manual(values = c("dodgerblue", "firebrick1")) +
  geom_hline(yintercept = 180, linetype = "dashed") +
  geom_hline(yintercept = 300, linetype = "dashed") +
  theme_bw() + theme +
  labs(x = "Age (Ma)", y = expression("CO"[2]*" (ppmv)")) +
  scale_x_continuous(breaks = seq(0, 2.5, 0.5)) +
  scale_y_continuous(breaks = seq(0, 1000, 200), limits = c(0, 1000))

p2 = ggplot(proxy, aes(x = age/1000, y = co2, color = doi)) +
  geom_errorbar(aes(ymin = lower, ymax = higher), width = 0, color = "gray", linewidth = 0.2) +
  geom_point(size = 2, shape = 21, fill = "white") +
  scale_color_brewer(palette = "Paired") +
  geom_hline(yintercept = 180, linetype = "dashed") +
  geom_hline(yintercept = 300, linetype = "dashed") +
  theme_bw() + theme +
  labs(x = "Age (Ma)", y = expression("CO"[2]*" (ppmv)")) +
  scale_x_continuous(breaks = seq(0, 2.5, 0.5),limits = c(0, 2.6)) +
  scale_y_continuous(breaks = seq(0, 1000, 200)) +
  guides(fill = "none", color = "none")

p3 = ggplot(GMST, aes(x = age, y = GMST)) +
  geom_line(color = "deepskyblue2") +
  theme_bw() + theme +
  scale_x_continuous(breaks = seq(0, 2.5, 0.5)) +
  labs(x = "Age (Ma)", y = expression(paste("GMST (", degree, "C)")))

ggarrange(p1, p3, nrow = 2, ncol = 1, align = "hv", heights = c(2,1), common.legend = TRUE)
ggarrange(p2, p3, nrow = 2, ncol = 1, align = "hv", heights = c(2,1), common.legend = TRUE)
ggarrange(p2, p1, nrow = 2, ncol = 1, align = "hv", common.legend = TRUE)
ggsave("figures/interglacial_glacial_CO2.pdf", width = 4.2, height = 5.7)
