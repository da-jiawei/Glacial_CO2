library(tidyverse)
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
source("soil_carbonate_isotope_forward_model.R") 

# rainfall
vars = ctrl()
MAP = seq(100, 600, 100)
vars$MAP = MAP
sens.p = fm(vars)
sens.p = cbind(MAP, sens.p)
p1 = ggplot(sens.p, aes(x = MAP, y = z)) +
  geom_line() +
  geom_point(size = 3, shape = 21, fill = "white") +
  theme_bw() + theme +
  labs(x = "MAP (mm)",
       y = "z (m)")
p2 = ggplot(sens.p, aes(x = MAP, y = Sz)) +
  geom_line() +
  geom_point(size = 3, shape = 21, fill = "white") +
  theme_bw() + theme +
  labs(x = "MAP (mm)",
       y = expression("S"[(z)]*" (ppmv)"))
p3 = ggplot(sens.p, aes(x = MAP, y = d18Os)) +
  geom_line() +
  geom_point(size = 3, shape = 21, fill = "white") +
  theme_bw() + theme +
  labs(x = "MAP (mm)",
       y = expression(delta^"18"*"O"[s]*" (\u2030, SMOW)"))
p4 = ggplot(sens.p, aes(x = MAP, y = d18Oc)) +
  geom_line() +
  geom_point(size = 3, shape = 21, fill = "white") +
  theme_bw() + theme +
  labs(x = "MAP (mm)",
       y = expression(delta^"18"*"O"[c]*" (\u2030, VPDB)"))

ggarrange(p1, p2, p3, p4, nrow = 2, ncol = 2, align = "hv")
ggsave("figures/sensitivity_analyses.jpg", width = 4, height = 3.7)
