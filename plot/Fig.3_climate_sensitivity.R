library(tidyverse)
library(ggpubr)
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

# read data
co2 = read.csv("output/binned_co2.csv")
ice_co2 = co2 %>% filter(method == "ice", age <= 0.8)
proxy_co2 = co2 %>% filter(age > 0.8)
benthic = read.csv("output/binned_benthic.csv")
bwt = read.csv("output/binned_bwt.csv")
wpwp = read.csv("output/binned_wpwp.csv")
R_LI = read.csv("data/R_LI_koehler2015.csv") %>%
  mutate(age = Age / 1000) %>%
  filter(age < 2.7)

# time-series plot ----
pal = c("#A6CEE3", "#1F78B4", "#B2DF8A", "#33A02C", "#FB9A99", "#E31A1C")
method = pal[factor(proxy_co2$method, levels = c("paleosol", "boron", "ice"))]

png("figures/time_series_climate_sensitivity.png", 4, 4.5, units = "in", res = 300)
par(mar = c(4, 4, 1, 4))
plot(-0.3, 0, xlim = c(0, 2.7), ylim = c(0, 5), axes = FALSE,
     xlab = "", ylab = "")

legend(x = 0, y = 4.1, legend = c("paleosol", "boron", "blue ice"),
       col = pal, pch = 16, cex = 0.4, pt.cex = 1)

yext = range(R_LI$RLI)
tix = seq(floor(min(yext)), 
          ceiling(max(yext)), by = 2)
R_LI.rs = cbind(R_LI$age,
               4 + (R_LI$RLI - min(tix)) / diff(range(tix)))
R_LI.rs = R_LI.rs[order(R_LI.rs[, 1]), ]
lines(R_LI.rs[, 1], R_LI.rs[, 2], col = pal[5], lwd = 1)
axis(4, 4 + (tix - min(tix)) / diff(range(tix)), tix)
mtext(expression("R"[LI]*" (W m"^"-2"*")"), 4, line = 2.5, at = 4.5)

yext = range(co2$CO2)
tix = seq(floor(min(yext)-39), 
          ceiling(max(yext)+50), by = 100)
ice.rs = cbind(ice_co2$age,
                3 + (ice_co2$CO2 - min(tix)) / diff(range(tix)))
proxy.rs = cbind(proxy_co2$age,
                 3 + (proxy_co2$CO2 - min(tix)) / diff(range(tix)))
ice.rs = ice.rs[order(ice.rs[, 1]), ]
lines(ice.rs[, 1], ice.rs[, 2], col = "black", lwd = 1)
points(proxy.rs[, 1], proxy.rs[, 2], col = "black", bg = method, pch = 21, cex = 1)
axis(2, 3 + (tix - min(tix)) / diff(range(tix)), tix)
mtext(expression("CO"[2]*" (ppmv)"), 2, line = 2.5, at = 3.5)

yext = range(benthic$d18O)
tix = seq(ceiling(max(yext)),
          floor(min(yext)), by = -1)
benthic.rs = cbind(benthic$age,
               3 - (benthic$d18O - min(tix)) / diff(range(tix)))
benthic.rs = benthic.rs[order(benthic.rs[, 1]), ]
lines(benthic.rs[, 1], benthic.rs[, 2], col = pal[1], lwd = 1)
axis(4, 3 - (tix - min(tix)) / diff(range(tix)), tix)
mtext(expression("benthic "*delta^"18"*"O (\u2030)"), 4, line = 2.5, at = 2.5)

yext = range(bwt$BWT)
tix = seq(floor(min(yext)), 
          ceiling(max(yext)), by = 2)
bwt.rs = cbind(bwt$age,
                   1 + (bwt$BWT - min(tix)) / diff(range(tix)))
bwt.rs = bwt.rs[order(bwt.rs[, 1]), ]
lines(bwt.rs[, 1], bwt.rs[, 2], col = pal[2], lwd = 1)
axis(2, 1 + (tix - min(tix)) / diff(range(tix)), tix)
mtext(expression(paste("BWT (", degree, "T)")), 2, line = 2.5, at = 1.5)

yext = range(wpwp$SST)
tix = seq(floor(min(yext)), 
          ceiling(max(yext)), by = 2)
wpwp.rs = cbind(wpwp$age,
               0 + (wpwp$SST - min(tix)) / diff(range(tix)))
wpwp.rs = wpwp.rs[order(wpwp.rs[, 1]), ]
lines(wpwp.rs[, 1], wpwp.rs[, 2], col = pal[3], lwd = 1)
axis(4, 0 + (tix - min(tix)) / diff(range(tix)), tix)
mtext(expression(paste("SST (", degree, "T)")), 4, line = 2.5, at = 0.5)

axis(1)
mtext("Age (Ma)", 1, line = 2)
dev.off()

# time window mean ----
## glacials ----
co2_g = co2 %>% filter(period == "glacial")
d18_g = benthic %>% filter(period == "glacial")
bwt_g = bwt %>% filter(period == "glacial")
wpwp_g = wpwp %>% filter(period == "glacial")
r_li_g = filter_g(R_LI, "Age")
r_li_g = r_li_g %>% mutate(time = assign_time_group(age, 0.3, 2.7)) 

dat_g = data.frame("time" = unique(co2_g$time))
for (i in 1:nrow(dat_g)) {
  co2_s = co2_g %>% filter(time == dat_g$time[i])
  rli_s = r_li_g %>% filter(time == dat_g$time[i])
  d18_s = d18_g %>% filter(time == dat_g$time[i])
  bwt_s = bwt_g %>% filter(time == dat_g$time[i])
  wpwp_s = wpwp_g %>% filter(time == dat_g$time[i])
  co2_i = sample(co2_s$CO2, nsyth, replace = TRUE)
  R_LI_i = sample(rli_s$RLI, nsyth, replace = TRUE)
  ln_co2 = log(co2_i / 278)
  R_total = 5.35 * ln_co2 + R_LI_i
  d18_i = sample(d18_s$d18O, nsyth, replace = TRUE)
  bwt_i = sample(bwt_s$BWT, nsyth, replace = TRUE)
  sst_i = sample(wpwp_s$SST, nsyth, replace = TRUE)
  dat_g$lnco2[i] = mean(ln_co2)
  dat_g$lnco2.sd[i] = sd(ln_co2)
  dat_g$r[i] = mean(R_total)
  dat_g$r.sd[i] = sd(R_total)
  dat_g$d18[i] = mean(d18_i)
  dat_g$d18.sd[i] = sd(d18_i)
  dat_g$bwt[i] = mean(bwt_i)
  dat_g$bwt.sd[i] = sd(bwt_i)
  dat_g$sst[i] = mean(sst_i)
  dat_g$sst.sd[i] = sd(sst_i)
}

p1 = ggplot(dat_g, aes(x = lnco2, y = d18)) +
  geom_smooth(method = "lm", formula = y ~ x, show.legend = F, span = 1, alpha = 0.1, linetype = "dashed", color = "#2171B5", fill = "#2171B5") +
  geom_errorbar(aes(xmin = lnco2 - lnco2.sd, xmax = lnco2 + lnco2.sd), width = 0, linewidth = 0.2) +
  geom_errorbar(aes(ymin = d18 - d18.sd, ymax = d18 + d18.sd), width = 0, linewidth = 0.2) +
  geom_point(aes(fill = time), shape = 21, size = 3) +
  scale_fill_brewer(palette = 1) +
  theme_bw() + theme +
  scale_x_continuous(limits = c(-0.5, 0.5)) +
  scale_y_reverse(limits = c(4.7, 3.1)) +
  labs(x = expression("ln(CO"[2]*"/C"[o]*")"),
       y = expression(delta^"18"*"O (\u2030)"))

p2 = ggplot(dat_g, aes(x = lnco2, y = bwt)) +
  geom_smooth(method = "lm", formula = y ~ x, show.legend = F, span = 1, alpha = 0.1, linetype = "dashed", color = "#2171B5", fill = "#2171B5") +
  geom_errorbar(aes(xmin = lnco2 - lnco2.sd, xmax = lnco2 + lnco2.sd), width = 0, linewidth = 0.2) +
  geom_errorbar(aes(ymin = bwt - bwt.sd, ymax = bwt + bwt.sd), width = 0, linewidth = 0.2) +
  geom_point(aes(fill = time), shape = 21, size = 3) +
  scale_fill_brewer(palette = 1) +
  theme_bw() + theme +
  scale_x_continuous(limits = c(-0.5, 0.5)) +
  scale_y_continuous(limits = c(-1, 5.5)) +
  labs(x = expression("ln(CO"[2]*"/C"[o]*")"),
       y = expression(paste("BWT (", degree, "C)")))

p3 = ggplot(dat_g, aes(x = lnco2, y = sst)) +
  geom_smooth(method = "lm", formula = y ~ x, show.legend = F, span = 1, alpha = 0.1, linetype = "dashed", color = "#2171B5", fill = "#2171B5") +
  geom_errorbar(aes(xmin = lnco2 - lnco2.sd, xmax = lnco2 + lnco2.sd), width = 0, linewidth = 0.2) +
  geom_errorbar(aes(ymin = sst - sst.sd, ymax = sst + sst.sd), width = 0, linewidth = 0.2) +
  geom_point(aes(fill = time), shape = 21, size = 3) +
  scale_fill_brewer(palette = 1) +
  theme_bw() + theme +
  scale_x_continuous(limits = c(-0.5, 0.5)) +
  scale_y_continuous(limits = c(25, 31)) +
  labs(x = expression("ln(CO"[2]*"/C"[o]*")"),
       y = expression(paste("SST (", degree, "C)")))

p4 = ggplot(dat_g, aes(x = r, y = d18)) +
  geom_smooth(method = "lm", formula = y ~ x, show.legend = F, span = 1, alpha = 0.1, linetype = "dashed", color = "#2171B5", fill = "#2171B5") +
  geom_errorbar(aes(xmin = r - r.sd, xmax = r + r.sd), width = 0, linewidth = 0.2) +
  geom_errorbar(aes(ymin = d18 - d18.sd, ymax = d18 + d18.sd), width = 0, linewidth = 0.2) +
  geom_point(aes(fill = time), shape = 21, size = 3) +
  scale_fill_brewer(palette = 1) +
  theme_bw() + theme +
  scale_x_continuous(limits = c(-6, 1.5)) +
  scale_y_reverse(limits = c(4.7, 3.1)) +
  labs(x = expression(Delta*"F"[CO2+LI]*" (W m"^"-2"*")"),
       y = expression(delta^"18"*"O (\u2030)"))

p5 = ggplot(dat_g, aes(x = r, y = bwt)) +
  geom_smooth(method = "lm", formula = y ~ x, show.legend = F, span = 1, alpha = 0.1, linetype = "dashed", color = "#2171B5", fill = "#2171B5") +
  geom_errorbar(aes(xmin = r - r.sd, xmax = r + r.sd), width = 0, linewidth = 0.2) +
  geom_errorbar(aes(ymin = bwt - bwt.sd, ymax = bwt + bwt.sd), width = 0, linewidth = 0.2) +
  geom_point(aes(fill = time), shape = 21, size = 3) +
  scale_fill_brewer(palette = 1) +
  theme_bw() + theme +
  scale_x_continuous(limits = c(-6, 1.5)) +
  scale_y_continuous(limits = c(-1, 5.5)) +
  labs(x = expression(Delta*"F"[CO2+LI]*" (W m"^"-2"*")"),
       y = expression(paste("BWT (", degree, "C)")))

p6 = ggplot(dat_g, aes(x = r, y = sst)) +
  geom_smooth(method = "lm", formula = y ~ x, show.legend = F, span = 1, alpha = 0.1, linetype = "dashed", color = "#2171B5", fill = "#2171B5") +
  geom_errorbar(aes(xmin = r - r.sd, xmax = r + r.sd), width = 0, linewidth = 0.2) +
  geom_errorbar(aes(ymin = sst - sst.sd, ymax = sst + sst.sd), width = 0, linewidth = 0.2) +
  geom_point(aes(fill = time), shape = 21, size = 3) +
  scale_fill_brewer(palette = 1) +
  theme_bw() + theme +
  scale_x_continuous(limits = c(-6, 1.5)) +
  scale_y_continuous(limits = c(25, 31)) +
  labs(x = expression(Delta*"F"[CO2+LI]*" (W m"^"-2"*")"),
       y = expression(paste("SST (", degree, "C)")))

ggarrange(p1,p2,p3,p4,p5,p6, nrow = 2, ncol = 3, align = "hv", common.legend = TRUE)
ggsave("figures/Fig_3_climate_sensitivity_glacials.pdf", width = 7.2, height = 5.8)

## interglacials ----
co2_ig = co2 %>% filter(period == "interglacial")
d18_ig = benthic %>% filter(period == "interglacial")
bwt_ig = bwt %>% filter(period == "interglacial")
wpwp_ig = wpwp %>% filter(period == "interglacial")
r_li_ig = filter_ig(R_LI, "Age") # function filter_ig() in Fig.2 file
r_li_ig = r_li_ig %>% mutate(time = assign_time_group(age, 0.3, 2.7)) # function assign_time_group in Fig.2 file

dat_ig = data.frame("time" = unique(co2_ig$time))
for (i in 1:nrow(dat_ig)) {
  co2_s = co2_ig %>% filter(time == dat_ig$time[i])
  rli_s = r_li_ig %>% filter(time == dat_ig$time[i])
  d18_s = d18_ig %>% filter(time == dat_ig$time[i])
  bwt_s = bwt_ig %>% filter(time == dat_ig$time[i])
  wpwp_s = wpwp_ig %>% filter(time == dat_ig$time[i])
  co2_i = sample(co2_s$CO2, nsyth, replace = TRUE)
  R_LI_i = sample(rli_s$RLI, nsyth, replace = TRUE)
  ln_co2 = log(co2_i / 278)
  R_total = 5.35 * ln_co2 + R_LI_i
  d18_i = sample(d18_s$d18O, nsyth, replace = TRUE)
  bwt_i = sample(bwt_s$BWT, nsyth, replace = TRUE)
  sst_i = sample(wpwp_s$SST, nsyth, replace = TRUE)
  dat_ig$lnco2[i] = mean(ln_co2)
  dat_ig$lnco2.sd[i] = sd(ln_co2)
  dat_ig$r[i] = mean(R_total)
  dat_ig$r.sd[i] = sd(R_total)
  dat_ig$d18[i] = mean(d18_i)
  dat_ig$d18.sd[i] = sd(d18_i)
  dat_ig$bwt[i] = mean(bwt_i)
  dat_ig$bwt.sd[i] = sd(bwt_i)
  dat_ig$sst[i] = mean(sst_i)
  dat_ig$sst.sd[i] = sd(sst_i)
}

p1 = ggplot(dat_ig, aes(x = lnco2, y = d18)) +
  geom_smooth(method = "lm", formula = y ~ x, show.legend = F, span = 1, alpha = 0.1, linetype = "dashed", color = "#D94801", fill = "#D94801") +
  geom_errorbar(aes(xmin = lnco2 - lnco2.sd, xmax = lnco2 + lnco2.sd), width = 0, linewidth = 0.2) +
  geom_errorbar(aes(ymin = d18 - d18.sd, ymax = d18 + d18.sd), width = 0, linewidth = 0.2) +
  geom_point(aes(fill = time), shape = 21, size = 3) +
  scale_fill_brewer(palette = 7) +
  theme_bw() + theme +
  scale_x_continuous(limits = c(-0.5, 0.5)) +
  scale_y_reverse(limits = c(4.7, 3.1)) +
  labs(x = expression("ln(CO"[2]*"/C"[o]*")"),
       y = expression(delta^"18"*"O (\u2030)"))

p2 = ggplot(dat_ig, aes(x = lnco2, y = bwt)) +
  geom_smooth(method = "lm", formula = y ~ x, show.legend = F, span = 1, alpha = 0.1, linetype = "dashed", color = "#D94801", fill = "#D94801") +
  geom_errorbar(aes(xmin = lnco2 - lnco2.sd, xmax = lnco2 + lnco2.sd), width = 0, linewidth = 0.2) +
  geom_errorbar(aes(ymin = bwt - bwt.sd, ymax = bwt + bwt.sd), width = 0, linewidth = 0.2) +
  geom_point(aes(fill = time), shape = 21, size = 3) +
  scale_fill_brewer(palette = 7) +
  theme_bw() + theme +
  scale_x_continuous(limits = c(-0.5, 0.5)) +
  scale_y_continuous(limits = c(-1, 5.5)) +
  labs(x = expression("ln(CO"[2]*"/C"[o]*")"),
       y = expression(paste("BWT (", degree, "C)")))

p3 = ggplot(dat_ig, aes(x = lnco2, y = sst)) +
  geom_smooth(method = "lm", formula = y ~ x, show.legend = F, span = 1, alpha = 0.1, linetype = "dashed", color = "#D94801", fill = "#D94801") +
  geom_errorbar(aes(xmin = lnco2 - lnco2.sd, xmax = lnco2 + lnco2.sd), width = 0, linewidth = 0.2) +
  geom_errorbar(aes(ymin = sst - sst.sd, ymax = sst + sst.sd), width = 0, linewidth = 0.2) +
  geom_point(aes(fill = time), shape = 21, size = 3) +
  scale_fill_brewer(palette = 7) +
  theme_bw() + theme +
  scale_x_continuous(limits = c(-0.5, 0.5)) +
  scale_y_continuous(limits = c(25, 31)) +
  labs(x = expression("ln(CO"[2]*"/C"[o]*")"),
       y = expression(paste("SST (", degree, "C)")))

p4 = ggplot(dat_ig, aes(x = r, y = d18)) +
  geom_smooth(method = "lm", formula = y ~ x, show.legend = F, span = 1, alpha = 0.1, linetype = "dashed", color = "#D94801", fill = "#D94801") +
  geom_errorbar(aes(xmin = r - r.sd, xmax = r + r.sd), width = 0, linewidth = 0.2) +
  geom_errorbar(aes(ymin = d18 - d18.sd, ymax = d18 + d18.sd), width = 0, linewidth = 0.2) +
  geom_point(aes(fill = time), shape = 21, size = 3) +
  scale_fill_brewer(palette = 7) +
  theme_bw() + theme +
  scale_x_continuous(limits = c(-6, 1.5)) +
  scale_y_reverse(limits = c(4.7, 3.1)) +
  labs(x = expression(Delta*"F"[CO2+LI]*" (W m"^"-2"*")"),
       y = expression(delta^"18"*"O (\u2030)"))

p5 = ggplot(dat_ig, aes(x = r, y = bwt)) +
  geom_smooth(method = "lm", formula = y ~ x, show.legend = F, span = 1, alpha = 0.1, linetype = "dashed", color = "#D94801", fill = "#D94801") +
  geom_errorbar(aes(xmin = r - r.sd, xmax = r + r.sd), width = 0, linewidth = 0.2) +
  geom_errorbar(aes(ymin = bwt - bwt.sd, ymax = bwt + bwt.sd), width = 0, linewidth = 0.2) +
  geom_point(aes(fill = time), shape = 21, size = 3) +
  scale_fill_brewer(palette = 7) +
  theme_bw() + theme +
  scale_x_continuous(limits = c(-6, 1.5)) +
  scale_y_continuous(limits = c(-1, 5.5)) +
  labs(x = expression(Delta*"F"[CO2+LI]*" (W m"^"-2"*")"),
       y = expression(paste("BWT (", degree, "C)")))

p6 = ggplot(dat_ig, aes(x = r, y = sst)) +
  geom_smooth(method = "lm", formula = y ~ x, show.legend = F, span = 1, alpha = 0.1, linetype = "dashed", color = "#D94801", fill = "#D94801") +
  geom_errorbar(aes(xmin = r - r.sd, xmax = r + r.sd), width = 0, linewidth = 0.2) +
  geom_errorbar(aes(ymin = sst - sst.sd, ymax = sst + sst.sd), width = 0, linewidth = 0.2) +
  geom_point(aes(fill = time), shape = 21, size = 3) +
  scale_fill_brewer(palette = 7) +
  theme_bw() + theme +
  scale_x_continuous(limits = c(-6, 1.5)) +
  scale_y_continuous(limits = c(25, 31)) +
  labs(x = expression(Delta*"F"[CO2+LI]*" (W m"^"-2"*")"),
       y = expression(paste("SST (", degree, "C)")))

ggarrange(p1,p2,p3,p4,p5,p6, nrow = 2, ncol = 3, align = "hv", common.legend = TRUE)
ggsave("figures/Fig_3_climate_sensitivity_interglacials.pdf", width = 7.2, height = 5.8)



## statistics ----
sens.co2 = data.frame(matrix(nrow = 2, ncol = 4))
names(sens.co2) = c("d18", "bwt", "wpwp", "period")
sens.co2$period = c("glacial", "interglacial")
m_d18_g = lm(d18 ~ lnco2, data = dat_g)
sens.co2$d18[1] = summary(m_d18_g)$coefficients[2,4]
m_d18_ig = lm(d18 ~ lnco2, data = dat_ig)
sens.co2$d18[2] = summary(m_d18_ig)$coefficients[2,4]
m_bwt_g = lm(bwt ~ lnco2, data = dat_g)
sens.co2$bwt[1] = summary(m_bwt_g)$coefficients[2,4]
m_bwt_ig = lm(bwt ~ lnco2, data = dat_ig)
sens.co2$bwt[2] = summary(m_bwt_ig)$coefficients[2,4]
m_wpwp_g = lm(sst ~ lnco2, data = dat_g)
sens.co2$wpwp[1] = summary(m_wpwp_g)$coefficients[2,4]
m_wpwp_ig = lm(sst ~ lnco2, data = dat_ig)
sens.co2$wpwp[2] = summary(m_wpwp_ig)$coefficients[2,4]

sens = data.frame(matrix(nrow = 2, ncol = 4))
names(sens) = c("d18", "bwt", "wpwp", "period")
sens$period = c("glacial", "interglacial")
m_d18_g = lm(d18 ~ r, data = dat_g)
sens$d18[1] = summary(m_d18_g)$coefficients[2,1]
m_d18_ig = lm(d18 ~ r, data = dat_ig)
sens$d18[2] = summary(m_d18_ig)$coefficients[2,1]
m_bwt_g = lm(bwt ~ r, data = dat_g)
sens$bwt[1] = summary(m_bwt_g)$coefficients[2,1]
m_bwt_ig = lm(bwt ~ r, data = dat_ig)
sens$bwt[2] = summary(m_bwt_ig)$coefficients[2,1]
m_wpwp_g = lm(sst ~ r, data = dat_g)
sens$wpwp[1] = summary(m_wpwp_g)$coefficients[2,1]
m_wpwp_ig = lm(sst ~ r, data = dat_ig)
sens$wpwp[2] = summary(m_wpwp_ig)$coefficients[2,1]

write.csv(sens.co2, "output/sens_co2_pvalue.csv")
write.csv(sens, "output/sens_pvalue.csv")


# pdfs ----
slope = data.frame(matrix(nrow = nsyth, ncol = 6))
names(slope) = c("d18_g", "bwt_g", "wpwp_g", "d18_ig", "bwt_ig", "wpwp_ig")
time = unique(co2_g$time)
for (i in 1:nsyth) {
  subgroup = data.frame(matrix(nrow = length(time), ncol = 8))
  names(subgroup) = c("df.g", "d18.g", "bwt.g", "sst.g", "df.ig", "d18.ig", "bwt.ig", "sst.ig")
  for (p in 1:length(time)) {
    co2_s = co2_g %>% filter(time == time[1])
    rli_s = r_li_g %>% filter(time == time[p])
    d18_s = d18_g %>% filter(time == time[p])
    bwt_s = bwt_g %>% filter(time == time[p])
    wpwp_s = wpwp_g %>% filter(time == time[p])
    subgroup$df.g[p] = 5.35 * log(sample(co2_s$CO2, 1)/278) + sample(rli_s$RLI, 1)
    subgroup$d18.g[p] = sample(d18_s$d18O, 1)
    subgroup$bwt.g[p] = sample(bwt$BWT, 1)
    subgroup$sst.g[p] = sample(wpwp$SST, 1)
    co2_s = co2_ig %>% filter(time == time[p])
    rli_s = r_li_ig %>% filter(time == time[p])
    d18_s = d18_ig %>% filter(time == time[p])
    bwt_s = bwt_ig %>% filter(time == time[p])
    wpwp_s = wpwp_ig %>% filter(time == time[p])
    subgroup$df.ig[p] = 5.35 * log(sample(co2_s$CO2, 1)/278) + sample(rli_s$RLI, 1)
    subgroup$d18.ig[p] = sample(d18_s$d18O, 1)
    subgroup$bwt.ig[p] = sample(bwt$BWT, 1)
    subgroup$sst.ig[p] = sample(wpwp$SST, 1)
  }
  m_d18 = lm(d18.g ~ df.g, data = subgroup)
  # slope$d18_g[i] = ifelse(summary(m_d18)$coefficients[2, "Pr(>|t|)"]<0.05, m_d18$coefficients[2], NA)
  slope$d18_g[i] = m_d18$coefficients[2]
  m_bwt = lm(bwt.g ~ df.g, data = subgroup)
  # slope$bwt_g[i] = ifelse(summary(m_bwt)$coefficients[2, "Pr(>|t|)"]<0.05, m_bwt$coefficients[2], NA)
  slope$bwt_g[i] = m_bwt$coefficients[2]
  m_sst = lm(sst.g ~ df.g, data = subgroup)
  # slope$wpwp_g[i] = ifelse(summary(m_sst)$coefficients[2, "Pr(>|t|)"]<0.05, m_sst$coefficients[2], NA)
  slope$wpwp_g[i] = m_sst$coefficients[2]
  m_d18 = lm(d18.ig ~ df.ig, data = subgroup)
  # slope$d18_ig[i] = ifelse(summary(m_d18)$coefficients[2, "Pr(>|t|)"]<0.05, m_d18$coefficients[2], NA)
  slope$d18_ig[i] = m_d18$coefficients[2]
  m_bwt = lm(bwt.ig ~ df.ig, data = subgroup)
  # slope$bwt_ig[i] = ifelse(summary(m_bwt)$coefficients[2, "Pr(>|t|)"]<0.05, m_bwt$coefficients[2], NA)
  slope$bwt_ig[i] = m_bwt$coefficients[2]
  m_sst = lm(sst.ig ~ df.ig, data = subgroup)
  # slope$wpwp_ig[i] = ifelse(summary(m_sst)$coefficients[2, "Pr(>|t|)"]<0.05, m_sst$coefficients[2], NA)
  slope$wpwp_ig[i] = m_sst$coefficients[2]
}

ggplot(slope) +
  geom_density(aes(x = d18_g), color = "#2171B5", fill = "#2171B5", size = 1, alpha = 0.3) +
  geom_density(aes(x = d18_ig), color = "#D94801", fill = "#D94801", size = 1, alpha = 0.2) +
  # geom_vline(xintercept = median(slope_d18_g$d18_g), color = "#2171B5", size = 1, linetype = "dashed") +
  # geom_vline(xintercept = median(slope_d18_ig$d18_ig), color = "#D94801", size = 1, linetype = "dashed") +
  theme_bw() + theme +
  # annotate("text", x = -0.75, y = 2, label = expression("Benthic "*delta^"18"*"O")) +
  labs(x = "slope", y = "density") +
  scale_x_continuous(limits = c(-1, 1))

ggplot(slope) +
  geom_density(aes(x = wpwp_g), color = "#2171B5", fill = "#2171B5", size = 1, alpha = 0.3) +
  geom_density(aes(x = wpwp_ig), color = "#D94801", fill = "#D94801", size = 1, alpha = 0.2) +
  # geom_vline(xintercept = median(slope_bwt_g$bwt_g), color = "#2171B5", size = 1, linetype = "dashed") +
  # geom_vline(xintercept = median(slope_bwt_ig$bwt_ig), color = "#D94801", size = 1, linetype = "dashed") +
  theme_bw() + theme +
  # annotate("text", x = 4, y = 1, label = "BWT") +
  labs(x = "slope", y = "density") +
  scale_x_continuous(limits = c(-5, 5))

ggplot(slope) +
  geom_density(aes(x = sst_g), color = "#2171B5", fill = "#2171B5", size = 1, alpha = 0.3) +
  geom_density(aes(x = sst_ig), color = "#D94801", fill = "#D94801", size = 1, alpha = 0.2) +
  # geom_vline(xintercept = median(slope_bwt_g$bwt_g), color = "#2171B5", size = 1, linetype = "dashed") +
  # geom_vline(xintercept = median(slope_bwt_ig$bwt_ig), color = "#D94801", size = 1, linetype = "dashed") +
  theme_bw() + theme +
  # annotate("text", x = 4, y = 1, label = "BWT") +
  labs(x = "slope", y = "density") +
  scale_x_continuous(limits = c(-5, 5))
