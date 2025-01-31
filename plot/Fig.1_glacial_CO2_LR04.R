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

# read data
benthic = read.csv("data/marine proxies/benthic_d18O.csv")
paleosol = read_xlsx("data/Dataset S1.xlsx", sheet = 2)
paleosol = paleosol[, c(1, 4, 17:19)] %>% drop_na()
names(paleosol) = c("site", "age", "CO2", "lower", "higher")
ice_core = read.csv("data/co2_compilation/ice_core_co2.csv")
blue_ice = read.csv("data/co2_compilation/blue.ice.csv")

# plot ----
p1 = ggplot(benthic, aes(x = Age, y = d18O)) +
  geom_line(color = "#F08841") +
  theme_bw() + theme +
  scale_y_reverse() +
  labs(x = "Age (ka)", y = expression("benthic "*delta^"18"*"O (\u2030)"))

p2 = ggplot() +
  geom_point(data = blue_ice, aes(x = Age, y = CO2), shape = 1, size = 3) +
  geom_line(data = ice_core, aes(x = age, y = co2)) +
  theme_bw() + theme +
  labs(x = "Age (ka)", y = expression("CO"[2]*" (ppmv)")) +
  scale_y_continuous(limits = c(150, 300),breaks = seq(150, 300, 50))

p3 = ggplot(paleosol, aes(x = age, y = CO2)) +
  geom_line(data = ice_core, aes(x = age, y = co2)) +
  geom_errorbar(aes(ymin = lower, ymax = higher, color = site), linewidth = 0.1, width = 0) +
  geom_point(aes(color = site), shape = 21, size = 3, fill = "white") +
  geom_smooth(span = 0.5, linewidth = 2, se = FALSE) +
  scale_color_manual(values = c("#F08841", "#2B6A99")) +
  theme_bw() + theme +
  labs(x = "Age (ka)", y = expression("CO"[2]*" (ppmv)")) +
  scale_y_continuous(limits = c(0, 600),breaks = seq(100, 600, 100))

ggarrange(p1, p2, p3, nrow = 3, ncol = 1, align = "hv", heights = c(0.7,0.6,1))
ggsave("figures/Fig_1_glacial_CO2.pdf", height = 5.3, width = 5.7)
