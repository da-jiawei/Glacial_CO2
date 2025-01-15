library(tidyverse)
library(readxl)

# read input data
input = read_xlsx("data/800_ky_data.xlsx")

# obtain d13C of atmospheric CO2 from tipple et al. 2010
tipple = read.table("data/tipple_et_al_2010.txt", header = FALSE)
names(tipple) = c("age", "d13a")
input$d13a = approx(x = tipple$age, y = tipple$d13a, xout = input$age_ka/1000)$y
input$d13a.sd = sapply(input$age_ka/1000, function(age){
  idx = which(tipple[, 1] > age)[1]
  sd(tipple[(idx - 2):(idx + 1), 2], na.rm = TRUE)
})

# obtain ice-core atmospheric CO2 record 
ice_co2 = read_xlsx("data/ice_core_co2.xlsx")
ice_co2 = as.data.frame(ice_co2)
input$co2 = approx(x = ice_co2$age, y = ice_co2$co2, xout = input$age_ka)$y
input$co2.sd = sapply(input$age_ka, function(age){
  idx = which(ice_co2[,1] > age)[1]
  sd(ice_co2[(idx - 2):(idx + 1), 2], na.rm = TRUE)
})

# S(z) calculation
nsyth = 10000
output = data.frame("R" = numeric(nrow(input)), "R.sd" = numeric(nrow(input)),
                    "Sz" = numeric(nrow(input)), "Sz.sd" = numeric(nrow(input)))
for (i in 1:nrow(input)) {
  temp = rnorm(nsyth, input$temp[i], 3)
  d13c = rnorm(nsyth, input$d13c[i], 0.03)
  d13s = (d13c + 1000) / ((11.98 - 0.12 * temp) / 1000 + 1) - 1000
  d13r = rnorm(nsyth, input$d13o[i]-2, 0.5)
  d13a = rnorm(nsyth, input$d13a[i], input$d13a.sd[i])
  ratio = (d13s - 1.0044 * d13r - 4.4) / (d13a - d13s)
  co2 = rnorm(nsyth, input$co2[i], input$co2.sd[i])
  Sz = co2 / ratio
  output$R[i] = median(ratio)
  output$R.sd[i] = sd(ratio)
  output$Sz[i] = median(Sz)
  output$Sz.sd[i] = sd(Sz)
}

dat = cbind(input, output)
write.csv(dat, "output/800_ky_data.csv")

# d18Oc - S(z) transfer function
# m1 = lm(Sz ~ d18c, data = dat)
# summary(m1)
m2 = nls(Sz ~ a * exp(b * d18c), data = dat, start = list(a = 26, b = -0.3))
summary(m2)
rss = sum(residuals(m2)^2)
tss = sum((dat$Sz - mean(dat$Sz))^2)
r2 = 1 - (rss / tss)


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

a = summary_model$parameters[1,1]
b = summary_model$parameters[2,1]
d18c_model = data.frame(d18c = seq(-11,-8.5,0.1))
d18c_model$Sz = a * exp(b * d18c_model$d18c)
ggplot(dat, aes(x = d18c, y = Sz)) +
  geom_errorbar(aes(ymin = Sz - Sz.sd, ymax = Sz + Sz.sd),
                linewidth = 0.2, color = "gray") +
  geom_point(aes(fill = section), shape = 21, size = 3) +
  geom_line(data = d18c_model, aes(x = d18c, y = Sz), linetype = "dashed") +
  theme_bw() + theme +
  labs(x = expression(delta^"18"*"O (\u2030, SMOW)"),
       y = expression("S"[(z)]*" (ppmv)"))












