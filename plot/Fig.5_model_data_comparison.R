library(tidyverse)
library(ggpubr)
library(readxl)
library(effsize)
source('plot/functions.R')
set.seed(42)
nsyth = 10000
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

# plot ----
plot_dm = function(num){
  df_name = paste0("slope_", num)
  dat = get(df_name)
  glacial = dat %>% filter(period == "glacial")
  interglacial = dat %>% filter(period == "interglacial")
  col_name = as.character(num)
  ggplot() +
    geom_point(data = cesm, aes(x = forcing, y = .data[[col_name]]), size = 1.5, color = "bisque") +
    geom_point(data = hadgem, aes(x = forcing, y = .data[[col_name]]), size = 1.5, color = "cornflowerblue") +
    geom_smooth(data = interglacial, aes(x = r, y = sst),
                method = "lm", formula = y ~ x, show.legend = F, span = 1, color = "#D94801", fill = "#D94801") +
    geom_smooth(data = glacial, aes(x = r, y = sst),
                method = "lm", formula = y ~ x, show.legend = F, span = 1, color = "#2171B5", fill = "#2171B5") +
    geom_point(data = longterm, aes(x = forcing, y = .data[[col_name]], fill = Model), size = 3, shape = 21) +
    geom_point(data = modern, aes(x = forcing, y = .data[[col_name]]), size = 4, shape = 22, fill = "white", stroke = 1) +
    scale_fill_viridis_d() +
    theme_bw() + theme +
    xlab(expression(Delta*italic(F)*" (W m"^"-2"*")")) + ylab("SST (°C)") +
    scale_x_continuous(limits = c(-3, 12)) +
    annotate("text", x = 0, y = max(longterm[[col_name]])-1, label = paste0("Site ", col_name))
}
p1 = plot_dm(882) 
p2 = plot_dm(1208) 
p3 = plot_dm(1090)
p4 = plot_dm(722)
p5 = plot_dm(1012)
p6 = plot_dm(846)
p7 = plot_dm(1143)
p8 = plot_dm(1148)

glacial = slope_bwt %>% filter(period == "glacial")
interglacial = slope_bwt %>% filter(period == "interglacial")
p9 = ggplot() +
  # geom_point(data = cesm, aes(x = forcing, y = `607`), size = 1.5, color = "bisque") +
  # geom_point(data = hadgem, aes(x = forcing, y = `607`), size = 1.5, color = "cornflowerblue") +
  geom_smooth(data = interglacial, aes(x = r, y = sst),
              method = "lm", formula = y ~ x, show.legend = F, span = 1, color = "#D94801", fill = "#D94801") +
  geom_smooth(data = glacial, aes(x = r, y = sst),
              method = "lm", formula = y ~ x, show.legend = F, span = 1, color = "#2171B5", fill = "#2171B5") +
  geom_point(data = longterm, aes(x = forcing, y = BWT, fill = Model), size = 3, shape = 21) +
  geom_point(data = modern, aes(x = forcing, y = BWT), size = 4, shape = 22, fill = "white", stroke = 1) +
  scale_fill_viridis_d() +
  theme_bw() + theme +
  xlab(expression(Delta*italic(F)*" (W m"^"-2"*")")) + ylab("SST (°C)") +
  scale_x_continuous(limits = c(-3, 12)) +
  annotate("text", label = "Site 607", x = 0, y = 10)

ggarrange(p1, p2, p3, p4, p5, p6, p7, p8, p9, nrow = 3, ncol = 3, align = "hv",
          labels = c("a", "b", "c", "d", "e", "f", "g", "h", "i"),
          common.legend = TRUE)
ggsave("figures/Fig_5_model_data_comparison.pdf", width = 7.8, height = 8.5)
