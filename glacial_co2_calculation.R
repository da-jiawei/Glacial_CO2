library(tidyverse)
library(readxl)
# read data
dat = read_xlsx("data/paleosol_da_2024_unpublished.xlsx", sheet = "paleosols")
input = dat[3:nrow(dat), c(5,33,40,48,93)]
names(input) = c("age", "d13c", "d13o", "d18c", "temp")
input[] <- lapply(input, function(x) as.numeric(as.character(x)))
section = dat[3:nrow(dat), 17]
names(section) = c("section")
input = cbind(section, input)
# obtain d13C of atmospheric CO2
tipple = read.table("data/tipple_et_al_2010.txt", header = FALSE)
names(tipple) = c("age", "d13a")
input$d13a = approx(x = tipple$age, y = tipple$d13a, xout = input$age/1000)$y
input$d13a.sd = sapply(input$age/1000, function(age){
  idx = which(tipple[, 1] > age)[1]
  sd(tipple[(idx - 2):(idx + 1), 2], na.rm = TRUE)
})

# CO2 calculation
nsyth = 10000
output = data.frame("d13s" = numeric(nrow(input)), "d13s.sd" = numeric(nrow(input)),
                    "R" = numeric(nrow(input)), "R.sd" = numeric(nrow(input)),
                    "Sz" = numeric(nrow(input)), "Sz.sd" = numeric(nrow(input)),
                    "co2" = numeric(nrow(input)), "co2.sd" = numeric(nrow(input)))
for (i in 1:nrow(input)) {
  temp = rnorm(nsyth, input$temp[i], 3)
  d13c = rnorm(nsyth, input$d13c[i], 0.03)
  d13s = (d13c + 1000) / ((11.98 - 0.12 * temp) / 1000 + 1) - 1000
  d13r = rnorm(nsyth, input$d13o[i]-2, 0.5)
  d13a = rnorm(nsyth, input$d13a[i], 0.1)
  ratio = (d13s - 1.0044 * d13r - 4.4) / (d13a - d13s)
  d18c = rnorm(nsyth, input$d18c[i], 0.03)
  a = rnorm(nsyth, 52.1862306, 21.43311390)
  b = rnorm(nsyth, -0.2065816, 0.04067195)
  Sz = a * exp(b * d18c)
  co2 = Sz * ratio
  output$d13s[i] = median(d13s)
  output$d13s.sd[i] = sd(d13s)
  output$R[i] = median(ratio)
  output$R.sd[i] = sd(ratio)
  output$Sz[i] = median(Sz)
  output$Sz.sd[i] = sd(Sz)
  output$co2[i] = median(co2)
  output$co2.sd[i] = sd(co2)
}

dat = cbind(input, output)
write.csv(dat, "output/glacial_co2_estimates.csv")

dat2 = read_xlsx("data/Dataset S1.xlsx", sheet = 2)
PBUQ = dat2[, c(4,8:14)] %>% drop_na()
names(PBUQ) = c("age", "R", "R.sd", "Sz", "Sz.sd", "co2", "co2.low", "co2.high")

plot(dat$R, PBUQ$R)
abline(a = 0, b = 1)
plot(dat$Sz, PBUQ$Sz)
abline(a = 0, b = 1)
plot(dat$co2, PBUQ$co2)
abline(a = 0, b = 1)
