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

# read data ----
## model simulations
longterm = read_xlsx("data/Model.xlsx", sheet = 1) %>%
  filter(Model != "FAMOUS" & Model != "modern") %>%
  filter(scenario != 16) %>%
  mutate(forcing = 5.35*forcing)
modern = read_xlsx("data/Model.xlsx", sheet = 1) %>%
  filter(Model == "modern") %>%
  mutate(forcing = 5.35*forcing)
cesm = read_xlsx("data/Model.xlsx", sheet = 2) %>%
  mutate(forcing = 5.35*log(CO2/278))
hadgem = read_xlsx("data/Model.xlsx", sheet = 3) %>%
  mutate(forcing = 5.35*log(CO2/278))

## records
dat_g = read.csv("output/climate_sensitivity_glacial.csv")
r_g = dat_g[, c("time", "r", "r.sd")]
dat_ig = read.csv("output/climate_sensitivity_interglacial.csv")
r_ig = dat_ig[, c("time", "r", "r.sd")]

binned_mean = function(dat, site) {
  dat_g = filter_g(dat, "age") %>% mutate(assign_time_group(age, 0.3, 2.7))
  colnames(dat_g)[3] = "time"
  dat_g = dat_g %>%
    group_by(time) %>%
    summarise(mean = mean(SST), sd = sd(SST))
  dat_ig = filter_ig(dat, "age") %>% mutate(assign_time_group(age, 0.3, 2.7))
  colnames(dat_ig)[3] = "time"
  dat_ig = dat_ig %>%
    group_by(time) %>%
    summarise(mean = mean(SST), sd = sd(SST))
  dat_list = list(r_g, dat_g, r_ig, dat_ig)
  dat = reduce(dat_list, full_join, by = "time")
  names(dat) = c("time", "r_g", "r.sd_g", "sst_g", "sst.sd_g", "r_ig", "r.sd_ig", "sst_ig", "sst.sd_ig")
  return(dat)
}

dat = read_xlsx("data/marine proxies/MidHighLatitudes.xlsx", sheet = "882")
sst_882 = binned_mean(dat, "882")
dat = read_xlsx("data/marine proxies/MidHighLatitudes.xlsx", sheet = "1208")
sst_1208 = binned_mean(dat, "1208")
dat = read_xlsx("data/marine proxies/MidHighLatitudes.xlsx", sheet = "1090")
sst_1090 = binned_mean(dat, "1090")
dat = read_xlsx("data/marine proxies/UpwellingZones.xlsx", sheet = "722")
sst_722 = binned_mean(dat, "722")
dat = read_xlsx("data/marine proxies/UpwellingZones.xlsx", sheet = "1012")
sst_1012 = binned_mean(dat, "1012")
dat = read_xlsx("data/marine proxies/UpwellingZones.xlsx", sheet = "846")
sst_846 = binned_mean(dat, "846")
dat = read_xlsx("data/marine proxies/WP.xlsx", sheet = "1148")
sst_1148 = binned_mean(dat, "1148")

# plot ----
m1 = lm(sst_ig~r_ig, sst_882)
summary(m1)
m2 = lm(sst_g~r_g, sst_882)
summary(m2)
p1 = ggplot() +
  geom_point(data = cesm, aes(x = forcing, y = `882`), size = 1.5, color = "bisque") +
  geom_point(data = hadgem, aes(x = forcing, y = `882`), size = 1.5, color = "cornflowerblue") +
  geom_smooth(data = sst_882, aes(x = r_ig, y = sst_ig),
              method = "lm", formula = y ~ x, show.legend = F, span = 1, color = "#D94801", fill = "#D94801") +
  geom_smooth(data = sst_882, aes(x = r_g, y = sst_g),
              method = "lm", formula = y ~ x, show.legend = F, span = 1, color = "#2171B5", fill = "#2171B5") +
  geom_point(data = longterm, aes(x = forcing, y = `882`, fill = Model), size = 3, shape = 21) +
  geom_point(data = modern, aes(x = forcing, y = `882`), size = 4, shape = 22, fill = "white", stroke = 1) +
  scale_fill_viridis_d() +
  theme_bw() + theme +
  xlab(expression(Delta*italic(F)*" (W m"^"-2"*")")) + ylab("SST (°C)") +
  scale_y_continuous(limits = c(5, 20)) +
  annotate("text", label = "Site 882", x = -2.5, y = 20)
  
m1 = lm(sst_ig~r_ig, sst_1208)
summary(m1)
m2 = lm(sst_g~r_g, sst_1208)
summary(m2)
p2 = ggplot() +
  geom_point(data = cesm, aes(x = forcing, y = `1208`), size = 1.5, color = "bisque") +
  geom_point(data = hadgem, aes(x = forcing, y = `1208`), size = 1.5, color = "cornflowerblue") +
  geom_smooth(data = sst_1208, aes(x = r_ig, y = sst_ig),
              method = "lm", formula = y ~ x, show.legend = F, span = 1, color = "#D94801", fill = "#D94801") +
  geom_smooth(data = sst_1208, aes(x = r_g, y = sst_g),
              method = "lm", formula = y ~ x, show.legend = F, span = 1, color = "#2171B5", fill = "#2171B5") +
  geom_point(data = longterm, aes(x = forcing, y = `1208`, fill = Model), size = 3, shape = 21) +
  geom_point(data = modern, aes(x = forcing, y = `1208`), size = 4, shape = 22, fill = "white", stroke = 1) +
  scale_fill_viridis_d() +
  theme_bw() + theme +
  xlab(expression(Delta*italic(F)*" (W m"^"-2"*")")) + ylab("SST (°C)") +
  scale_y_continuous(limits = c(15, 30)) +
  annotate("text", label = "Site 1208", x = -2.5, y = 30)

m1 = lm(sst_ig~r_ig, sst_1090)
summary(m1)
m2 = lm(sst_g~r_g, sst_1090)
summary(m2)
p3 = ggplot() +
  geom_point(data = cesm, aes(x = forcing, y = `1090`), size = 1.5, color = "bisque") +
  geom_point(data = hadgem, aes(x = forcing, y = `1090`), size = 1.5, color = "cornflowerblue") +
  geom_smooth(data = sst_1090, aes(x = r_ig, y = sst_ig),
              method = "lm", formula = y ~ x, show.legend = F, span = 1, color = "#D94801", fill = "#D94801") +
  geom_smooth(data = sst_1090, aes(x = r_g, y = sst_g),
              method = "lm", formula = y ~ x, show.legend = F, span = 1, color = "#2171B5", fill = "#2171B5") +
  geom_point(data = longterm, aes(x = forcing, y = `1090`, fill = Model), size = 3, shape = 21) +
  geom_point(data = modern, aes(x = forcing, y = `1090`), size = 4, shape = 22, fill = "white", stroke = 1) +
  scale_fill_viridis_d() +
  theme_bw() + theme +
  xlab(expression(Delta*italic(F)*" (W m"^"-2"*")")) + ylab("SST (°C)") +
  scale_y_continuous(limits = c(8, 25)) +
  annotate("text", label = "Site 1090", x = -2.5, y = 25)

m1 = lm(sst_ig~r_ig, sst_722)
summary(m1)
m2 = lm(sst_g~r_g, sst_722)
summary(m2)
p4 = ggplot() +
  geom_point(data = cesm, aes(x = forcing, y = `722`), size = 1.5, color = "bisque") +
  geom_point(data = hadgem, aes(x = forcing, y = `722`), size = 1.5, color = "cornflowerblue") +
  geom_smooth(data = sst_722, aes(x = r_ig, y = sst_ig),
              method = "lm", formula = y ~ x, show.legend = F, span = 1, color = "#D94801", fill = "#D94801") +
  geom_smooth(data = sst_722, aes(x = r_g, y = sst_g),
              method = "lm", formula = y ~ x, show.legend = F, span = 1, color = "#2171B5", fill = "#2171B5") +
  geom_point(data = longterm, aes(x = forcing, y = `722`, fill = Model), size = 3, shape = 21) +
  geom_point(data = modern, aes(x = forcing, y = `722`), size = 4, shape = 22, fill = "white", stroke = 1) +
  scale_fill_viridis_d() +
  theme_bw() + theme +
  xlab(expression(Delta*italic(F)*" (W m"^"-2"*")")) + ylab("SST (°C)") +
  scale_y_continuous(limits = c(23, 35), breaks = seq(25, 35, 5)) +
  annotate("text", label = "Site 722", x = -2.5, y = 35)

m1 = lm(sst_ig~r_ig, sst_1012)
summary(m1)
m2 = lm(sst_g~r_g, sst_1012)
summary(m2)
p5 = ggplot() +
  geom_point(data = cesm, aes(x = forcing, y = `1012`), size = 1.5, color = "bisque") +
  geom_point(data = hadgem, aes(x = forcing, y = `1012`), size = 1.5, color = "cornflowerblue") +
  geom_smooth(data = sst_1012, aes(x = r_ig, y = sst_ig),
              method = "lm", formula = y ~ x, show.legend = F, span = 1, color = "#D94801", fill = "#D94801") +
  geom_smooth(data = sst_1012, aes(x = r_g, y = sst_g),
              method = "lm", formula = y ~ x, show.legend = F, span = 1, color = "#2171B5", fill = "#2171B5") +
  geom_point(data = longterm, aes(x = forcing, y = `1012`, fill = Model), size = 3, shape = 21) +
  geom_point(data = modern, aes(x = forcing, y = `1012`), size = 4, shape = 22, fill = "white", stroke = 1) +
  scale_fill_viridis_d() +
  theme_bw() + theme +
  xlab(expression(Delta*italic(F)*" (W m"^"-2"*")")) + ylab("SST (°C)") +
  scale_y_continuous(limits = c(14, 30)) +
  annotate("text", label = "Site 1012", x = -2.5, y = 30)

m1 = lm(sst_ig~r_ig, sst_846)
summary(m1)
m2 = lm(sst_g~r_g, sst_846)
summary(m2)
p6 = ggplot() +
  geom_point(data = cesm, aes(x = forcing, y = `846`), size = 1.5, color = "bisque") +
  geom_point(data = hadgem, aes(x = forcing, y = `846`), size = 1.5, color = "cornflowerblue") +
  geom_smooth(data = sst_846, aes(x = r_ig, y = sst_ig),
              method = "lm", formula = y ~ x, show.legend = F, span = 1, color = "#D94801", fill = "#D94801") +
  geom_smooth(data = sst_846, aes(x = r_g, y = sst_g),
              method = "lm", formula = y ~ x, show.legend = F, span = 1, color = "#2171B5", fill = "#2171B5") +
  geom_point(data = longterm, aes(x = forcing, y = `846`, fill = Model), size = 3, shape = 21) +
  geom_point(data = modern, aes(x = forcing, y = `846`), size = 4, shape = 22, fill = "white", stroke = 1) +
  scale_fill_viridis_d() +
  theme_bw() + theme +
  xlab(expression(Delta*italic(F)*" (W m"^"-2"*")")) + ylab("SST (°C)") +
  scale_y_continuous(limits = c(20, 40)) +
  annotate("text", label = "Site 846", x = -2.5, y = 40)

m1 = lm(sst~r, dat_ig)
summary(m1)
m2 = lm(sst~r, dat_g)
summary(m2)
p7 = ggplot() +
  geom_point(data = cesm, aes(x = forcing, y = `1143`), size = 1.5, color = "bisque") +
  geom_point(data = hadgem, aes(x = forcing, y = `1143`), size = 1.5, color = "cornflowerblue") +
  geom_smooth(data = dat_ig, aes(x = r, y = sst),
              method = "lm", formula = y ~ x, show.legend = F, span = 1, color = "#D94801", fill = "#D94801") +
  geom_smooth(data = dat_g, aes(x = r, y = sst),
              method = "lm", formula = y ~ x, show.legend = F, span = 1, color = "#2171B5", fill = "#2171B5") +
  geom_point(data = longterm, aes(x = forcing, y = `1143`, fill = Model), size = 3, shape = 21) +
  geom_point(data = modern, aes(x = forcing, y = `1143`), size = 4, shape = 22, fill = "white", stroke = 1) +
  scale_fill_viridis_d() +
  theme_bw() + theme +
  xlab(expression(Delta*italic(F)*" (W m"^"-2"*")")) + ylab("SST (°C)") +
  scale_y_continuous(limits = c(26, 40)) +
  annotate("text", label = "Site 1143", x = -2.5, y = 40)

m1 = lm(sst_ig~r_ig, sst_1148)
summary(m1)
m2 = lm(sst_g~r_g, sst_1148)
summary(m2)
p8 = ggplot() +
  geom_point(data = cesm, aes(x = forcing, y = `1148`), size = 1.5, color = "bisque") +
  geom_point(data = hadgem, aes(x = forcing, y = `1148`), size = 1.5, color = "cornflowerblue") +
  geom_smooth(data = sst_1148, aes(x = r_ig, y = sst_ig),
              method = "lm", formula = y ~ x, show.legend = F, span = 1, color = "#D94801", fill = "#D94801") +
  geom_smooth(data = sst_1148, aes(x = r_g, y = sst_g),
              method = "lm", formula = y ~ x, show.legend = F, span = 1, color = "#2171B5", fill = "#2171B5") +
  geom_point(data = longterm, aes(x = forcing, y = `1148`, fill = Model), size = 3, shape = 21) +
  geom_point(data = modern, aes(x = forcing, y = `1148`), size = 4, shape = 22, fill = "white", stroke = 1) +
  scale_fill_viridis_d() +
  theme_bw() + theme +
  xlab(expression(Delta*italic(F)*" (W m"^"-2"*")")) + ylab("SST (°C)") +
  scale_y_continuous(limits = c(23, 38)) +
  annotate("text", label = "Site 1148", x = -2.5, y = 38)

m1 = lm(bwt~r, dat_ig)
summary(m1)
m2 = lm(bwt~r, dat_g)
summary(m2)
p9 = ggplot() +
  geom_point(data = cesm, aes(x = forcing, y = `607`), size = 1.5, color = "bisque") +
  geom_point(data = hadgem, aes(x = forcing, y = `607`), size = 1.5, color = "cornflowerblue") +
  geom_smooth(data = dat_ig, aes(x = r, y = bwt),
              method = "lm", formula = y ~ x, show.legend = F, span = 1, color = "#D94801", fill = "#D94801") +
  geom_smooth(data = dat_g, aes(x = r, y = bwt),
              method = "lm", formula = y ~ x, show.legend = F, span = 1, color = "#2171B5", fill = "#2171B5") +
  geom_point(data = longterm, aes(x = forcing, y = BWT, fill = Model), size = 3, shape = 21) +
  geom_point(data = modern, aes(x = forcing, y = BWT), size = 4, shape = 22, fill = "white", stroke = 1) +
  scale_fill_viridis_d() +
  theme_bw() + theme +
  xlab(expression(Delta*italic(F)*" (W m"^"-2"*")")) + ylab("SST (°C)") +
  scale_y_continuous(limits = c(0, 10)) +
  annotate("text", label = "Site 607", x = -2.5, y = 10)

ggarrange(p1, p2, p3, p4, p5, p6, p7, p8, p9, nrow = 3, ncol = 3, align = "hv",
          labels = c("a", "b", "c", "d", "e", "f", "g", "h", "i"),
          common.legend = TRUE)
ggsave("figures/Fig_5_model_data_comparison.pdf", width = 7.8, height = 8.5)
