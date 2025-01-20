library(tidyverse)
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
ggplot(dat, aes(x = age/1000, y = CO2, fill = period)) +
  geom_errorbar(aes(ymin = lower, ymax = higher), linewidth = 0.2) +
  geom_point(shape = 21, size = 3) +
  theme_bw() + theme +
  labs(x = "Age (Ma)", y = expression("CO"[2]*" (ppmv)"), fill = "")
