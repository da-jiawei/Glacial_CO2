rm(list = ls())
library(tidyverse)
library(ggpubr)
library(readxl)
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
cat("\014")

# read data ----
# groom paleosol data
paleosol_ig = read_xlsx("data/co2_compilation/paleosol_interglacial_CO2_da2019.xlsx", sheet = 2)
paleosol_ig = paleosol_ig[4:nrow(paleosol_ig), c(4, 15)] %>% drop_na()
names(paleosol_ig) = c("age", "CO2")
paleosol_ig[] = lapply(paleosol_ig, as.numeric)
paleosol_ig$age = paleosol_ig$age * 1000
paleosol_g = read_xlsx("data/Dataset S1.xlsx", sheet = 2)
paleosol_g = paleosol_g[, c(4, 17)] %>% drop_na()
names(paleosol_g) = c("age", "CO2")
# other proxy data
gmst = read.csv("data/GMST/clark_2024.csv")
boron = read.csv("data/co2_compilation/boron_CO2.csv") %>%
  rename(age = Age)
ice_core = read.csv("data/co2_compilation/ice_core_co2.csv") %>%
  rename(CO2 = co2)
blue_ice = read.csv("data/co2_compilation/blue.ice.csv") %>%
  rename(age = Age)
benthic = read.csv("data/marine proxies/benthic_d18O.csv") %>%
  rename(age = Age)
bwt = read.csv("data/marine proxies/BWT607.csv")
wpwp.sst = read.csv("data/marine proxies/1143Li.csv")

# divide glacials and interglacials ----
## age unit: kyr
boron_g = filter_g(boron, "age")
ice_core_g = filter_g(ice_core, "age")
blue_ice_g = blue_ice %>% filter(period == "G")
# denote methods
paleosol_g$method = "paleosol"
boron_g$method = "boron"
ice_core_g$method = "ice_core"
blue_ice_g$method = "blue_ice"
co2_g = bind_rows(paleosol_g, boron_g, ice_core_g, blue_ice_g) %>% 
  select(age, CO2, method)
gmst$age = gmst$age*1000
gmst_g = filter_g(gmst, "age")
benthic_g = filter_g(benthic, "age")
bwt_g = filter_g(bwt, "age")
wpwp.sst_g = filter_g(wpwp.sst, "age")

boron_ig = filter_ig(boron, "age")
ice_core_ig = filter_ig(ice_core, "age")
blue_ice_ig = blue_ice %>% filter(period == "IG")
# denote methods
paleosol_ig$method = "paleosol"
boron_ig$method = "boron"
ice_core_ig$method = "ice_core"
blue_ice_ig$method = "blue_ice"
co2_ig = bind_rows(paleosol_ig, boron_ig, ice_core_ig, blue_ice_ig) %>% 
  select(age, CO2, method)
gmst_ig = filter_ig(gmst, "age")
benthic_ig = filter_ig(benthic, "age")
bwt_ig = filter_ig(bwt, "age")
wpwp.sst_ig = filter_ig(wpwp.sst, "age")

# binning ----
interval = 300
age_max = 2700
co2_g = co2_g %>%
  mutate(time = assign_time_group(age, interval, age_max))
gmst_g = gmst_g %>%
  mutate(time = assign_time_group(age, interval, age_max))
benthic_g = benthic_g %>%
  mutate(time = assign_time_group(age, interval, age_max))
bwt_g = bwt_g %>%
  mutate(time = assign_time_group(age, interval, age_max))
wpwp.sst_g = wpwp.sst_g %>%
  mutate(time = assign_time_group(age, interval, age_max))
co2_ig = co2_ig %>%
  mutate(time = assign_time_group(age, interval, age_max))
gmst_ig = gmst_ig %>%
  mutate(time = assign_time_group(age, interval, age_max))
benthic_ig = benthic_ig %>%
  mutate(time = assign_time_group(age, interval, age_max))
bwt_ig = bwt_ig %>%
  mutate(time = assign_time_group(age, interval, age_max))
wpwp.sst_ig = wpwp.sst_ig %>%
  mutate(time = assign_time_group(age, interval, age_max))

# # plot glacial vs interglacial records
# ggplot() +
#   geom_line(data = wpwp.sst_g, aes(x = age, y = SST), color = "blue") +
#   geom_line(data = wpwp.sst_ig, aes(x = age, y = SST), color = "red")

# output data
co2_g$period = "glacial"
co2_ig$period = "interglacial"
binned_CO2 = rbind(co2_g, co2_ig)
write.csv(binned_CO2, "output/binned_co2.csv")
gmst_g$period = "glacial"
gmst_ig$period = "interglacial"
binned_gmst = rbind(gmst_g, gmst_ig)
write.csv(binned_gmst, "output/binned_gmst.csv")
benthic_g$period = "glacial"
benthic_ig$period = "interglacial"
binned_benthic = rbind(benthic_g, benthic_ig)
write.csv(binned_benthic, "output/binned_benthic.csv")
bwt_g$period = "glacial"
bwt_ig$period = "interglacial"
binned_bwt = rbind(bwt_g, bwt_ig)
write.csv(binned_bwt, "output/binned_bwt.csv")
wpwp.sst_g$period = "glacial"
wpwp.sst_ig$period = "interglacial"
binned_wpwp.sst = rbind(wpwp.sst_g, wpwp.sst_ig)
write.csv(binned_wpwp.sst, "output/binned_wpwp.csv")

# plot ----
df_filtered = benthic_g %>%
  filter(d18O > quantile(benthic_g$d18O, 0.25) - 1.5*IQR(benthic_g$d18O) & 
         d18O < quantile(benthic_g$d18O, 0.75) + 1.5*IQR(benthic_g$d18O))
min_values = aggregate(d18O ~ time, df_filtered, min)
max_values = aggregate(d18O ~ time, df_filtered, max)
p1 = ggplot(benthic_g, aes(x = time, y = d18O)) +
  geom_violin(color = "lightblue") +
  geom_jitter(width = 0.1, color = "lightblue") +
  stat_summary(fun = median, geom = "crossbar", width = 0.5, color = "navy") +
  geom_line(data = min_values, aes(group = 1, x = time, y = d18O), linetype = "dashed", color = "gray") +
  geom_line(data = max_values, aes(group = 1, x = time, y = d18O), linetype = "dashed", color = "gray") +
  scale_fill_viridis_d() +
  theme_bw() + theme + theme(legend.position = "none") +
  theme(axis.text.x = element_text(angle = 30, hjust = 1)) +
  scale_y_reverse(limits = c(5.1, 2.9)) +
  xlab("") + ylab(expression(delta^"18"*"O (\u2030)"))

min_values = aggregate(BWT ~ time, bwt_g, min)
max_values = aggregate(BWT ~ time, bwt_g, max)
p2 = ggplot(bwt_g, aes(x = time, y = BWT)) +
  geom_violin(color = "lightblue") +
  geom_jitter(width = 0.1, color = "lightblue") +
  stat_summary(fun = median, geom = "crossbar", width = 0.5, color = "navy") +
  geom_line(data = min_values, aes(group = 1, x = time, y = BWT, color = "gray"), linetype = "dashed", color = "gray") +
  geom_line(data = max_values, aes(group = 1, x = time, y = BWT, color = "gray"), linetype = "dashed", color = "gray") +
  scale_fill_viridis_d() +
  theme_bw() + theme + theme(legend.position = "none") +
  theme(axis.text.x = element_text(angle = 30, hjust = 1)) +
  scale_y_continuous(limits = c(-2, 6.5), breaks = seq(0, 6, 2)) +
  xlab("") + ylab("BWT (°C)")

min_values = aggregate(SST ~ time, wpwp.sst_g, min)
max_values = aggregate(SST ~ time, wpwp.sst_g, max)
p3 = ggplot(wpwp.sst_g, aes(x = time, y = SST)) +
  geom_violin(color = "lightblue", width = 1) +
  geom_jitter(width = 0.1, color = "lightblue") +
  stat_summary(fun = median, geom = "crossbar", width = 0.5, color = "navy") +
  geom_line(data = min_values, aes(group = 1, x = time, y = SST), linetype = "dashed", color = "gray") +
  geom_line(data = max_values, aes(group = 1, x = time, y = SST), linetype = "dashed", color = "gray") +
  scale_fill_viridis_d() +
  theme_bw() + theme + theme(legend.position = "none") +
  theme(axis.text.x = element_text(angle = 30, hjust = 1)) +
  scale_y_continuous(limits = c(24, 31)) +
  xlab("") + ylab("WPWP SST (°C)")

min_values = aggregate(GMST ~ time, gmst_g, min)
max_values = aggregate(GMST ~ time, gmst_g, max)
p4 = ggplot(gmst_g, aes(x = time, y = GMST)) +
  geom_violin(color = "lightblue") +
  geom_jitter(width = 0.1, color = "lightblue") +
  stat_summary(fun = median, geom = "crossbar", width = 0.5, color = "navy") +
  geom_line(data = min_values, aes(group = 1, x = time, y = GMST), linetype = "dashed", color = "gray") +
  geom_line(data = max_values, aes(group = 1, x = time, y = GMST), linetype = "dashed", color = "gray") +
  theme_bw() + theme + theme(legend.position = "none") +
  theme(axis.text.x = element_text(angle = 30, hjust = 1)) +
  scale_y_continuous(limits = c(-8, 3)) +
  labs(x = "", y = expression(paste("GMST (", degree, "C)")))

min_values = aggregate(CO2 ~ time, co2_g, min)
max_values = aggregate(CO2 ~ time, co2_g, max)
p5 = ggplot(co2_g, aes(x = time, y = CO2)) +
  geom_violin(color = "lightblue") +
  geom_jitter(width = 0.1, color = "lightblue") +
  stat_summary(fun = median, geom = "crossbar", width = 0.5, color = "navy") +
  geom_line(data = min_values, aes(group = 1, x = time, y = CO2), linetype = "dashed", color = "gray") +
  geom_line(data = max_values, aes(group = 1, x = time, y = CO2), linetype = "dashed", color = "gray") +
  theme_bw() + theme + theme(legend.position = "none") +
  theme(axis.text.x = element_text(angle = 30, hjust = 1)) +
  scale_y_continuous(limits = c(120, 450)) +
  xlab("") + ylab(expression("CO"[2]*" (ppm)"))

# interglacials
df_filtered = benthic_ig %>%
  filter(d18O > quantile(benthic_ig$d18O, 0.25) - 1.5*IQR(benthic_ig$d18O) & 
           d18O < quantile(benthic_ig$d18O, 0.75) + 1.5*IQR(benthic_ig$d18O))
min_values = aggregate(d18O ~ time, df_filtered, min)
max_values = aggregate(d18O ~ time, df_filtered, max)
p6 = ggplot(benthic_ig, aes(x = time, y = d18O)) +
  geom_violin(color = "lightsalmon") +
  geom_jitter(width = 0.1, color = "lightsalmon") +
  stat_summary(fun = median, geom = "crossbar", width = 0.5, color = "firebrick") +
  geom_line(data = min_values, aes(group = 1, x = time, y = d18O), linetype = "dashed", color = "gray") +
  geom_line(data = max_values, aes(group = 1, x = time, y = d18O), linetype = "dashed", color = "gray") +
  scale_fill_viridis_d() +
  theme_bw() + theme + theme(legend.position = "none") +
  theme(axis.text.x = element_text(angle = 30, hjust = 1)) +
  scale_y_reverse(limits = c(5.1, 2.9)) +
  xlab("") + ylab(expression(delta^"18"*"O (\u2030)"))

min_values = aggregate(BWT ~ time, bwt_ig, min)
max_values = aggregate(BWT ~ time, bwt_ig, max)
p7 = ggplot(bwt_ig, aes(x = time, y = BWT)) +
  geom_violin(color = "lightsalmon") +
  geom_jitter(width = 0.1, color = "lightsalmon") +
  stat_summary(fun = median, geom = "crossbar", width = 0.5, color = "firebrick") +
  geom_line(data = min_values, aes(group = 1, x = time, y = BWT, color = "gray"), linetype = "dashed", color = "gray") +
  geom_line(data = max_values, aes(group = 1, x = time, y = BWT, color = "gray"), linetype = "dashed", color = "gray") +
  scale_fill_viridis_d() +
  theme_bw() + theme + theme(legend.position = "none") +
  theme(axis.text.x = element_text(angle = 30, hjust = 1)) +
  scale_y_continuous(limits = c(-2, 6.5), breaks = seq(0, 6, 2)) +
  xlab("") + ylab("BWT (°C)")

min_values = aggregate(SST ~ time, wpwp.sst_ig, min)
max_values = aggregate(SST ~ time, wpwp.sst_ig, max)
p8 = ggplot(wpwp.sst_ig, aes(x = time, y = SST)) +
  geom_violin(color = "lightsalmon", width = 1) +
  geom_jitter(width = 0.1, color = "lightsalmon") +
  stat_summary(fun = median, geom = "crossbar", width = 0.5, color = "firebrick") +
  geom_line(data = min_values, aes(group = 1, x = time, y = SST), linetype = "dashed", color = "gray") +
  geom_line(data = max_values, aes(group = 1, x = time, y = SST), linetype = "dashed", color = "gray") +
  scale_fill_viridis_d() +
  theme_bw() + theme + theme(legend.position = "none") +
  theme(axis.text.x = element_text(angle = 30, hjust = 1)) +
  scale_y_continuous(limits = c(24, 31)) +
  xlab("") + ylab("WPWP SST (°C)")

min_values = aggregate(GMST ~ time, gmst_ig, min)
max_values = aggregate(GMST ~ time, gmst_ig, max)
p9 = ggplot(gmst_ig, aes(x = time, y = GMST)) +
  geom_violin(color = "lightsalmon") +
  geom_jitter(width = 0.1, color = "lightsalmon") +
  stat_summary(fun = median, geom = "crossbar", width = 0.5, color = "firebrick") +
  geom_line(data = min_values, aes(group = 1, x = time, y = GMST), linetype = "dashed", color = "gray") +
  geom_line(data = max_values, aes(group = 1, x = time, y = GMST), linetype = "dashed", color = "gray") +
  theme_bw() + theme + theme(legend.position = "none") +
  theme(axis.text.x = element_text(angle = 30, hjust = 1)) +
  scale_y_continuous(limits = c(-8, 3)) +
  labs(x = "", y = expression(paste("GMST (", degree, "C)")))

min_values = aggregate(CO2 ~ time, co2_ig, min)
max_values = aggregate(CO2 ~ time, co2_ig, max)
p10 = ggplot(co2_ig, aes(x = time, y = CO2)) +
  geom_violin(color = "lightsalmon") +
  geom_jitter(width = 0.1, color = "lightsalmon") +
  stat_summary(fun = median, geom = "crossbar", width = 0.5, color = "firebrick") +
  geom_line(data = min_values, aes(group = 1, x = time, y = CO2), linetype = "dashed", color = "gray") +
  geom_line(data = max_values, aes(group = 1, x = time, y = CO2), linetype = "dashed", color = "gray") +
  theme_bw() + theme + theme(legend.position = "none") +
  theme(axis.text.x = element_text(angle = 30, hjust = 1)) +
  scale_y_continuous(limits = c(120, 450)) +
  xlab("") + ylab(expression("CO"[2]*" (ppm)"))

ggarrange(p1,p6,p2,p7,p3,p8,p4,p9,p5,p10,
          nrow = 5, ncol = 2, align = "hv",
          labels = c("a", "b", "c", "d", "e", "f", "g", "h", "i", "j"))
# ggarrange(p1,p5,p4,p8, nrow = 2, ncol = 2, align = "hv")
ggsave("figures/Fig_2_time_series_proxy_data_boxplot_500ky.pdf", width = 6.7, height = 11)

