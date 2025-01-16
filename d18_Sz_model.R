library(tidyverse)
library(readxl)

# read input data ----
input = read_xlsx("data/Dataset S1.xlsx", sheet = 1)
dat = input[, c(1,3,6,13,14)] %>% drop_na()
names(dat) = c("section", "age", "d18c", "R", "R.sd")

# obtain ice-core atmospheric CO2 record
ice_co2 = read_xlsx("data/ice_core_co2.xlsx")
ice_co2 = as.data.frame(ice_co2)
dat$co2 = approx(x = ice_co2$age, y = ice_co2$co2, xout = dat$age)$y
dat$co2.sd = sapply(dat$age, function(age){
  idx = which(ice_co2[,1] > age)[1]
  sd(ice_co2[(idx - 2):(idx + 1), 2], na.rm = TRUE)
})

# d18c - S(z) model ----
# S(z) calculation
nsyth = 10000
for (i in 1:nrow(input)) {
  R = rnorm(nsyth, dat$R[i], dat$R.sd[i])
  co2 = rnorm(nsyth, dat$co2[i], dat$co2.sd[i])
  Sz = co2 / R
  dat$Sz[i] = median(Sz)
  dat$Sz.sd[i] = sd(Sz)
}

write.csv(dat, "output/800_ky_data.csv")

# d18Oc - S(z) transfer function
m1 = lm(Sz ~ d18c, data = dat)
summary(m1)
m2 = nls(Sz ~ a * exp(b * d18c), data = dat, start = list(a = 26, b = -0.3))
summary(m2)
rss = sum(residuals(m2)^2)
tss = sum((dat$Sz - mean(dat$Sz))^2)
r2 = 1 - (rss / tss)

sim = data.frame("d18c" = seq(min(dat$d18c), max(dat$d18c), length.out = 100))
sim$fit = predict(m2, newdata = sim)

# d18Oc - S(z) plot
theme = theme(axis.text.x = element_text(margin = margin(t = 0.1, unit = "cm")),
               axis.text.y = element_text(margin = margin(r = 0.1, unit = "cm")),
               axis.ticks.length=unit(0.15, "cm"),
               axis.ticks = element_line(colour = "black"),
               text = element_text(color = "black", size = 10),
               axis.title = element_text(size = 13), 
               axis.text = element_text(size = 12),
               plot.title = element_text(hjust = 0.1, vjust = -10),
               legend.text = element_text(size = 10),
               legend.title = element_text(size = 10),
               panel.grid.minor = element_blank(),
               panel.grid.major = element_blank())

ggplot(dat, aes(x = d18c, y = Sz)) +
  geom_errorbar(aes(ymin = Sz - Sz.sd, ymax = Sz + Sz.sd),
                linewidth = 0.2, color = "gray") +
  geom_point(aes(fill = section), shape = 21, size = 3) +
  geom_line(data = sim, aes(x = d18c, y = fit), linetype = "dashed") +
  theme_bw() + theme +
  labs(x = expression(delta^"18"*"O (\u2030, SMOW)"),
       y = expression("S"[(z)]*" (ppmv)"))

ggsave("figures/d18c_Sz_model.jpg", width = 4.7, height = 3.8)

# calculating S(z) for early Pleistocene data ---- 

input = read_xlsx("data/Dataset S1.xlsx", sheet = 2)
dat = input[, c(1, 4, 6)] %>% drop_na()
names(dat) = c("section", "age", "d18c")
for (i in 1:nrow(dat)) {
  d18c = rnorm(nsyth, dat$d18c[i], 0.03)
  a = rnorm(nsyth, 48.19298, 19.63901)
  b = rnorm(nsyth, -0.21362, 0.04034)
  sz = a * exp(b * d18c)
  dat$sz[i] = median(sz)
  dat$sz.sd[i] = sd(sz)
}

write.csv(dat, "output/glacial_Sz_estimates.csv")




