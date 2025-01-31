filter_g = function(data, age_column) {
  glacial_age_ranges = tibble::tibble(
    start = c(14, 57, 130, 243, 337, 424, 533, 621, 712, 790, 866, 917, 959, 982, 1031, 1081, 1114,
              1190, 1244, 1286, 1320, 1362, 1405, 1452, 1492, 1530, 1570, 1608, 1642, 1698, 1743, 
              1782, 1816, 1832, 1860, 1898, 1941, 1990, 2043, 2103, 2146, 2192, 2236, 2273, 2309, 
              2350, 2387, 2427, 2477, 2510, 2554),
    end = c(29, 71, 191, 300, 374, 478, 563, 676, 761, 814, 900, 936, 970, 1014, 1062, 1104, 1141,
            1215, 1264, 1304, 1344, 1383, 1424, 1469, 1510, 1548, 1585, 1628, 1670, 1715, 1758, 
            1802, 1826, 1849, 1875, 1915, 1965, 2017, 2088, 2125, 2168, 2208, 2250, 2291, 2333, 
            2373, 2407, 2452, 2494, 2540, 2575)
  )
  data %>%
    mutate(age = !!sym(age_column)) %>%
    filter(
      purrr::map_lgl(age, ~ any(. >= glacial_age_ranges$start & . <= glacial_age_ranges$end))
    )
}

filter_ig = function(data, age_column) {
  interglacial_age_ranges = tibble::tibble(
    start = c(0, 29, 71, 191, 300, 374, 478, 563, 676, 761, 814, 900, 936, 970, 1014, 1062, 1104, 1141,
              1215, 1264, 1304, 1344, 1383, 1424, 1469, 1510, 1548, 1585, 1628, 1670, 1715, 1758, 
              1802, 1826, 1849, 1875, 1915, 1965, 2017, 2088, 2125, 2168, 2208, 2250, 2291, 2333, 
              2373, 2407, 2452, 2494, 2540, 2575), 
    end = c(14, 57, 130, 243, 337, 424, 533, 621, 712, 790, 866, 917, 959, 982, 1031, 1081, 1114,
            1190, 1244, 1286, 1320, 1362, 1405, 1452, 1492, 1530, 1570, 1608, 1642, 1698, 1743, 
            1782, 1816, 1832, 1860, 1898, 1941, 1990, 2043, 2103, 2146, 2192, 2236, 2273, 2309, 
            2350, 2387, 2427, 2477, 2510, 2554, 2595)
  )
  data %>%
    mutate(age = !!sym(age_column)) %>%
    filter(
      purrr::map_lgl(age, ~ any(. >= interglacial_age_ranges$start & . <= interglacial_age_ranges$end))
    )
}

# assign binned time groups
assign_time_group = function(age, interval, age_max) {
  breaks = seq(0, age_max, by = interval)
  labels = paste0(
    sprintf("%.1f", breaks[-length(breaks)]/1000 + interval/1000), "-", 
    sprintf("%.1f", breaks[-length(breaks)]/1000), " Ma"
  )
  
  cut(age, breaks, labels = labels, right = FALSE)
}
