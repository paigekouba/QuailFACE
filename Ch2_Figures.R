# Chapter 2 Figures
# run after 4.9.24_testing 

pdf(file = "Ch2_figures_6.6.24.pdf", width=6, height=5)

# Daytime CO2 Elevation 4/9-4/19
daytimeCO2_4.9to4.19

# CO2 elevation between plots
between_pointrange

# Interpolated ∆CO2 for horizontal control
interpolated_CO2

# CO2 elevation above plots, with a shaded area showing height within the screen
above_pointrange

# CO2 elevation with varying vegetation height
veg_pointrange

# CO2 elevation over 10d test: eCO2, aCO2 1 min averages (hourly?) and wind

# Quail Ridge average daily temperature and cumulative rainfall
quail_weather

dev.off()