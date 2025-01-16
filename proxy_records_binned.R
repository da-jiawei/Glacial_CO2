library(tidyverse)
library(ggpubr)
library(readxl)
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
boron = read.csv("data/co2_compilation/boron_CO2.csv")
ice_core = read.csv("data/co2_compilation/ice_core_co2.csv")
blue_ice = read.csv("data/co2_compilation/blue.ice.csv")
benthic = read.csv("data/marine proxies/benthic_d18O.csv")
bwt = read.csv("data/marine proxies/BWT607.csv")
wpwp.sst = read.csv("data/marine proxies/1143Li.csv")

# divide glacials and interglacials ----
filter_g = function(data, age_column) {
  glacial_age_ranges = tibble::tibble(
    start = c(0.014, 0.057, 0.13, 0.243, 0.337, 0.424, 0.533, 0.621, 0.712, 
              0.79, 0.866, 0.917, 0.959, 0.982, 1.031, 1.081, 1.114, 1.19, 
              1.244, 1.286, 1.32, 1.362, 1.405, 1.452, 1.492, 1.53, 1.57, 
              1.608, 1.6425, 1.6975, 1.743, 1.782, 1.816, 1.8325, 1.8595, 
              1.898, 1.941, 1.99, 2.043, 2.103, 2.146, 2.192, 2.236, 2.273, 
              2.309, 2.35, 2.387, 2.427, 2.477, 2.51, 2.554),
    end   = c(0.029, 0.071, 0.191, 0.3, 0.374, 0.478, 0.563, 0.676, 0.761, 
              0.814, 0.9, 0.936, 0.97, 1.014, 1.062, 1.104, 1.141, 1.215, 
              1.264, 1.304, 1.344, 1.383, 1.424, 1.469, 1.51, 1.5475, 1.585, 
              1.6285, 1.67, 1.715, 1.758, 1.8025, 1.826, 1.849, 1.875, 1.915, 
              1.965, 2.017, 2.088, 2.125, 2.168, 2.2075, 2.25, 2.291, 2.333, 
              2.373, 2.407, 2.452, 2.494, 2.54, 2.575)
  )
  data %>%
    mutate(age = !!sym(age_column) / 1000) %>%
    filter(
      purrr::map_lgl(age, ~ any(. >= glacial_age_ranges$start & . <= glacial_age_ranges$end))
    )
}

paleosol_g = paleosol_g %>% mutate(age = age/1000)
boron_g = filter_g(boron, "Age")
ice_core_g = filter_g(ice_core, "age")
names(ice_core_g) = c("age", "CO2")
blue_ice_g = blue_ice %>% filter(period == "G") %>% mutate(age = Age/1000)
co2_g = rbind(paleosol_g[,c("age", "CO2")], boron_g[, c("age", "CO2")], ice_core_g, blue_ice_g[, c("age", "CO2")])
benthic_g = filter_g(benthic, "Age")
bwt_g = filter_g(bwt, "age")
wpwp.sst_g = filter_g(wpwp.sst, "age")

filter_ig = function(data, age_column) {
  interglacial_age_ranges = tibble::tibble(
    start = c(0, 0.029, 0.071, 0.191, 0.3, 0.374, 0.478, 0.563, 0.676, 
              0.761, 0.814, 0.9, 0.936, 0.97, 1.014, 1.062, 1.104, 1.141, 
              1.215, 1.264, 1.304, 1.344, 1.383, 1.424, 1.469, 1.51, 1.5475, 
              1.585, 1.6285, 1.67, 1.715, 1.758, 1.8025, 1.826, 1.849, 1.875, 
              1.915, 1.965, 2.017, 2.088, 2.125, 2.168, 2.2075, 2.25, 2.291, 
              2.333, 2.373, 2.407, 2.452, 2.494, 2.54, 2.575),
    end   = c(0.014, 0.057, 0.13, 0.243, 0.337, 0.424, 0.533, 0.621, 0.712, 
              0.79, 0.866, 0.917, 0.959, 0.982, 1.031, 1.081, 1.114, 1.19, 
              1.244, 1.286, 1.32, 1.362, 1.405, 1.452, 1.492, 1.53, 1.57, 
              1.608, 1.6425, 1.6975, 1.743, 1.782, 1.816, 1.8325, 1.8595, 
              1.898, 1.941, 1.99, 2.043, 2.103, 2.146, 2.192, 2.236, 2.273, 
              2.309, 2.35, 2.387, 2.427, 2.477, 2.51, 2.554, 2.595)
  )
  data %>%
    mutate(age = !!sym(age_column) / 1000) %>%
    filter(
      purrr::map_lgl(age, ~ any(. >= interglacial_age_ranges$start & . <= interglacial_age_ranges$end))
    )
}
paleosol_ig = paleosol_ig %>% mutate(age = age/1000)
boron_ig = filter_ig(boron, "Age")
ice_core_ig = filter_ig(ice_core, "age")
names(ice_core_ig) = c("age", "CO2")
blue_ice_ig = blue_ice %>% filter(period == "IG") %>% mutate(age = Age/1000)
co2_ig = rbind(paleosol_ig[,c("age", "CO2")], boron_ig[, c("age", "CO2")], ice_core_ig, blue_ice_ig[, c("age", "CO2")])
benthic_ig = filter_ig(benthic, "Age")
bwt_ig = filter_ig(bwt, "age")
wpwp.sst_ig = filter_ig(wpwp.sst, "age")

# binning ----
interval = 0.3
age_max = 2.7
assign_time_group = function(age, interval, age_max) {
  breaks = seq(0, age_max, by = interval)
  labels = paste0(
    sprintf("%.1f", breaks[-length(breaks)] + interval), "-", 
    sprintf("%.1f", breaks[-length(breaks)]), " Ma"
  )
  
  cut(age, breaks, labels = labels, right = FALSE)
}
co2_g = co2_g %>%
  mutate(time = assign_time_group(age, interval, age_max))
benthic_g = benthic_g %>%
  mutate(time = assign_time_group(age, interval, age_max))
bwt_g = bwt_g %>%
  mutate(time = assign_time_group(age, interval, age_max))
wpwp.sst_g = wpwp.sst_g %>%
  mutate(time = assign_time_group(age, interval, age_max))
co2_ig = co2_ig %>%
  mutate(time = assign_time_group(age, interval, age_max))
benthic_ig = benthic_ig %>%
  mutate(time = assign_time_group(age, interval, age_max))
bwt_ig = bwt_ig %>%
  mutate(time = assign_time_group(age, interval, age_max))
wpwp.sst_ig = wpwp.sst_ig %>%
  mutate(time = assign_time_group(age, interval, age_max))

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

min_values = aggregate(CO2 ~ time, co2_g, min)
max_values = aggregate(CO2 ~ time, co2_g, max)
p4 = ggplot(co2_g, aes(x = time, y = CO2)) +
  geom_violin(color = "lightblue") +
  geom_jitter(width = 0.1, color = "lightblue") +
  stat_summary(fun = median, geom = "crossbar", width = 0.5, color = "navy") +
  geom_line(data = min_values, aes(group = 1, x = time, y = CO2), linetype = "dashed", color = "gray") +
  geom_line(data = max_values, aes(group = 1, x = time, y = CO2), linetype = "dashed", color = "gray") +
  theme_bw() + theme + theme(legend.position = "none") +
  theme(axis.text.x = element_text(angle = 30, hjust = 1)) +
  scale_y_continuous(limits = c(120, 450)) +
  xlab("") + ylab(expression("CO"[2]*" (ppm)"))

df_filtered = benthic_ig %>%
  filter(d18O > quantile(benthic_ig$d18O, 0.25) - 1.5*IQR(benthic_ig$d18O) & 
           d18O < quantile(benthic_ig$d18O, 0.75) + 1.5*IQR(benthic_ig$d18O))
min_values = aggregate(d18O ~ time, df_filtered, min)
max_values = aggregate(d18O ~ time, df_filtered, max)
p5 = ggplot(benthic_ig, aes(x = time, y = d18O)) +
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
p6 = ggplot(bwt_ig, aes(x = time, y = BWT)) +
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
p7 = ggplot(wpwp.sst_ig, aes(x = time, y = SST)) +
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

min_values = aggregate(CO2 ~ time, co2_ig, min)
max_values = aggregate(CO2 ~ time, co2_ig, max)
p8 = ggplot(co2_ig, aes(x = time, y = CO2)) +
  geom_violin(color = "lightsalmon") +
  geom_jitter(width = 0.1, color = "lightsalmon") +
  stat_summary(fun = median, geom = "crossbar", width = 0.5, color = "firebrick") +
  geom_line(data = min_values, aes(group = 1, x = time, y = CO2), linetype = "dashed", color = "gray") +
  geom_line(data = max_values, aes(group = 1, x = time, y = CO2), linetype = "dashed", color = "gray") +
  theme_bw() + theme + theme(legend.position = "none") +
  theme(axis.text.x = element_text(angle = 30, hjust = 1)) +
  scale_y_continuous(limits = c(120, 450)) +
  xlab("") + ylab(expression("CO"[2]*" (ppm)"))

ggarrange(p1,p5,p2,p6,p3,p7,p4,p8,
          nrow = 4, ncol = 2, align = "hv",
          labels = c("a", "b", "c", "d", "e", "f", "g", "h"))
ggsave("figures/Fig_2.time_series_proxy_data_boxplot.jpg", width = 6.7, height = 9.7)
