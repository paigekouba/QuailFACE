# Chapter 2 Figures
# run after 4.9.24_testing 

#pdf(file = "Ch2_figures_6.6.24.pdf", width=6, height=5)

# Daytime CO2 Elevation 4/9-4/19
png("Ch2_Fig3.png", width=14, height=5, units="in", res=300)
daytimeCO2_4.9to4.19 
dev.off()

# CO2 elevation between plots
png("Ch2_Fig4.png", width=14, height=5, units="in", res=300)
between_pointrange
dev.off()

# Interpolated ∆CO2 for horizontal control
png("Ch2_Fig5.png", width=6.5, height=5.5, units="in", res=300)
interpolated_CO2 + ylim(c(-70,70)) + xlim(c(-70,70))
dev.off()

# CO2 elevation above plots, with a shaded area showing height within the screen
png("Ch2_Fig6.png", width=6.5, height=5.5, units="in", res=300)
above_pointrange
dev.off()

# CO2 elevation with varying vegetation height
png("Ch2_Fig7.png", width=6.5, height=5.5, units="in", res=300)
veg_pointrange
dev.off()

# CO2 elevation over 10d test: eCO2, aCO2 1 min averages (hourly?) and wind

# Quail Ridge average daily temperature and cumulative rainfall
quail_weather

dev.off()