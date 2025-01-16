ctrl = function(){
  vars = list(
    pCO2 = 300,
    MAT = 15,
    MAP = 500,
    PCQ.pf = 0.25,
    PCQ_to = 10,
    tsc = 0.3,
    lat = 35,
    pore = 0.35,
    tort = 0.7,
    f_R = 0.11,
    spre = 0.55,
    ETR = 0.06,
    d13Ca = -8,
    theta_bar = 0.065 # mean water content [0.05, 0.5]
    # ,d13Cr = -25
  )
}

fm = function(vars){
  
  ## Unpack variables
  list2env(vars, environment())
  
  # Constants ----
  R13.VPDB = 0.011237
  R18.VPDB = 0.0020672
  R18.VSMOW = 0.0020052
  alpha18.diff = 1.028489
  a.theta = 0.05 # rate of increase of water content with depth (m-1) (Barnes and Allison, 1983)
  Rgas = 8.314462 # gas constant
  rho = 1000 # liquid water density (kg/m3)
  Dv.air = 2.44E-05 # water vapor diffusivity in air (m2/s) (Merlivat, 1978)
  
  # Derived values ----
  PPCQ = MAP * PCQ.pf 
  TmPCQ = MAT + PCQ_to # air temperature of pedogenic carbonate quarter
  Ra = 42.608 - 0.3538 * abs(lat) # total radiation at the top of the atmosphere
  Rs = Ra * 0.16 * sqrt(12) # daily temperature range assumed to be 12
  
  # soil parameters ----
  ## Depth to carbonate formation based on  Retallack (2005) data
  z = 0.0925 * MAP + 13.4
  z_m = z / 100 # in meter unit 
  
  ## Soil temperatures at depth z
  d = sqrt((2 * 0.0007) / ((2 * 3.1415 / 3.154e7) * 0.3))
  Tsoil = MAT + PCQ_to * sin(2 * 3.1415 * tsc - z / d) / exp(z / d) 
  Tsoil.K = Tsoil + 273.15
  
  ## Potential Evapotranspiration - Hargreaves and Samani (1982) and Turc (1961)
  ha = pmin(0.95, 0.25 + 0.7 * (PPCQ / 900))
  PET_PCQ_D = ifelse(ha < 0.5, 
                     0.013 * (TmPCQ / (TmPCQ + 15)) * (23.885 * Rs + 50) * (1 + ((0.5 - ha) / 0.7)),
                     0.013 * (TmPCQ / (TmPCQ + 15)) * (23.885 * Rs + 50))
  PET_PCQ_D = pmax(PET_PCQ_D, 0.01)
  PET_PCQ = PET_PCQ_D * 90
  PET_D = ifelse(ha < 0.5, 
                 0.013 * (MAT / (MAT + 15)) * (23.885 * Rs + 50) * (1 + ((0.5 - ha) / 0.7)),
                 0.013 * (MAT / (MAT + 15)) * (23.885 * Rs + 50))
  PET_D = pmax(PET_D, 0.01)
  PET = PET_D * 365
  
  ## AET in mm/quarter from Budyko curve - Pike (1964)
  AET_PCQ = PPCQ * (1 / (sqrt(1 + (1 / ((PET_PCQ / (PPCQ)))) ^ 2)))

  # Carbon isotope ----
  ## Free air porosity
  AI = PET / MAP
  L = ifelse(AI < 1.4, (-2 * AI^2 + 2.5 * AI + 1) * 100, 60) # mean rooting depth
  FAP = pmin((pore - ((PPCQ - AET_PCQ) / (L * 10 * pore))), pore - 0.05)
  FAP = pmax(FAP, 0.01)
  
  ## Soil respiration rate 
  R_PCQ_D = 1.25 * exp(0.05452 * TmPCQ) * PPCQ / (127.77 + PPCQ)  # Raich (2002)
  # R_PCQ_D_m1 = 1.25 * exp(0.07987 * MAT) * MAP / (29.86 + MAP) # CLP model
  R_PCQ_D = R_PCQ_D * f_R # (gC/m2/d)
  R_PCQ_D = R_PCQ_D / (12.01 * 100^2)  # from gC/m2/d to molC/cm2/d
  R_PCQ_S = R_PCQ_D / (24 * 3600)  # molC/ cm2 / s
  R_PCQ_S_0 = R_PCQ_S / (L * pore) # Quade et al. (2007)
  
  ## CO2 diffusion
  Dair = 0.1369 * (Tsoil.K / 273.15) ^ 1.958
  DIFC = FAP * tort * Dair
  
  ## S(z) 
  k = L / 2 / log(2) # Respiration characteristic production depth (cm) - Quade (2007)
  S_z_mol = k ^ 2 * R_PCQ_S_0 / DIFC * (1 - exp(-z / k)) # (mol/cm3)
  S_z = S_z_mol * (0.08206 * Tsoil.K * 10^9) # ppmv
  
  ## estimate the d13Cr of soil-respired CO2
  DD13_water = 25.09 - 1.2 * (MAP + 975) / (27.2 + 0.04 * (MAP + 975))
  D13C_plant = (28.26 * 0.22 * (pCO2 + 23.9)) / (28.26 + 0.22 * (pCO2 + 23.9)) - DD13_water # schubert & Jahren (2015)
  d13Cr = d13Ca - D13C_plant
  SOM.frac = 0
  d13Co = d13Cr + 1 + SOM.frac 
  
  ## d13C of pedogenic carbonate
  d13Cs = (pCO2 * d13Ca + S_z * (1.0044 * d13Cr + 4.4))/(S_z + pCO2)
  d13Cc = ((1 + (11.98 - 0.12 * Tsoil) / 1000) * (d13Cs + 1000)) - 1000
  
  # Oxygen isotope ----
  ## Rainfall isotopes
  TmOOS = (4 * MAT - TmPCQ) / 3 # out-of-season air temperature
  d18p_PCQ = -15 + 0.58 * TmPCQ
  d18p_OOS = -15 + 0.58 * TmOOS
  d18p = (d18p_PCQ * PCQ.pf + d18p_OOS * (1 - spre) * (1 - PCQ.pf)) / (PCQ.pf + (1 - spre) * (1 - PCQ.pf))
  R18.p = (d18p / 1000 + 1) * R18.VSMOW
  
  ## Equilibrium fractionation (Horita and Wesolowski 1994)
  alpha18.eq = 1 / exp(((1.137e6 / (Tsoil.K ^ 2) - 0.4156e3/Tsoil.K - 2.0667) /1000))
  
  ### Atmospheric water vapor isotopes
  R18.a = R18.p * alpha18.eq
  
  ### Soil evaporation from AET
  E = ETR * AET_PCQ
  E = pmax(E, 1) # minimum of 1 mm
  E_s = E / (1000 * 90 * 24 * 3600) # soil evaporation rate in m/sec
  
  ### Water vapor diffusivity
  Dv.soil = Dv.air * tort * (pore - 0.05) # effective diffusivity of water vapor in soil (m2/s)
  es = (0.611 * exp(17.502 * Tsoil / (Tsoil + 240.97))) * 1000 # saturated water vapor pressure from Tetens formula
  N.sat = 0.01802 * es / (Rgas * Tsoil.K) # saturated water vapor concentration at a given temperature
  z.bar = N.sat * Dv.soil / (E_s * rho) # mean penetration depth (m)
  z.ef = (1 - ha) * z.bar # the thickness of the water vapor phase region (m)
  z.ef = pmax(z.ef, 1e-10)
  
  ### Liquid water diffusivity (m2/s) (Easteal 1984)
  Dl = exp(1.6766 + 1.6817 * (1000 / Tsoil.K) - 0.5773 * (1000 / Tsoil.K)^2) * 10^-9 
  Dl.soil = Dl * pore * tort # effective diffusivity of liquid water (m2/s)
  z.hat = Dl.soil / E_s # the decay length (mean penetration depth)
  
  ### The evaporation front
  h.ef = ha + z.ef / z.bar # humidity at the evaporation front
  R18.ef = (alpha18.diff * R18.p * (z.ef / z.bar) + ha * R18.a) / (h.ef * alpha18.eq) # isotopic composition at the evaporation front
  
  ### Isotope composition of soil water at depth z
  hs = pmin(ha + z_m / z.bar, 1)
  z.f = (theta_bar / a.theta) * log(z_m / z.ef) # the modified depth function
  R18.s = ifelse(z_m <= z.ef, (alpha18.diff * R18.p * z_m / z.bar + ha * R18.a) / (hs * alpha18.eq),
                 (R18.ef - R18.p) * exp(-z.f / z.hat) + R18.p)
  d18O.s = ((R18.s / R18.VSMOW) - 1) * 1000
  
  ### Isotope composition of soil carbonate
  alpha18_c_w_eq = exp((1.61e4 / Tsoil.K - 24.6) / 1000) # Tremaine (2011)
  R18.c = R18.s * alpha18_c_w_eq
  d18Oc = (R18.c / R18.VPDB - 1) * 1000
  D47c = 0.0391e6 / Tsoil.K ^ 2 + 0.154 # Andersen (2021)
  
  results = data.frame("d13Cc" = rep(d13Cc), "d18Oc" = rep(d18Oc), 
                       "D47" = rep(D47c),
                       "d18Os" = rep(d18O.s),
                       "d13Co" = rep(d13Co),
                       "d13Cs" = rep(d13Cs),
                       "Sz" = rep(S_z),
                       "d18Op" = rep(d18p),
                       "z" = rep(z_m),
                       "ha" = rep(ha),
                       "z.f" = rep(z.f),
                       "E_s" = rep(E_s))
}

sens.plot = function(sens.t, var) {
  png(paste("figure/sens_test/sens_", var, ".png", sep = ""), 9.87, 2.57, units = "in", res = 300)
  par(mfrow = c(1, 4))
  par(mar = c(5,5,1,1))
  plot(sens.t[[var]], sens.t$d13Cc, type = "l", cex.lab = 1.4, cex.axis = 1.2,
       xlab = var, ylab = expression(delta^"13"*"C"[c]*" (\u2030)"))
  plot(sens.t[[var]], sens.t$d18Oc, type = "l", xlab = var, cex.lab = 1.4, cex.axis = 1.2,
       ylab = expression(delta^"18"*"O"[c]*" (\u2030)"))
  plot(sens.t[[var]], sens.t$D47, type = "l", xlab = var, cex.lab = 1.4, cex.axis = 1.2,
       ylab = expression(Delta[47]*" (\u2030)"))
  plot(sens.t[[var]], sens.t$d13Co, type = "l", xlab = var, cex.lab = 1.4, cex.axis = 1.2,
       ylab = expression(delta^"13"*"C"[o]*" (\u2030)"))
  dev.off()
}
