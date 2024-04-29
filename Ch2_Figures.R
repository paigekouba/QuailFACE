# Chapter 2 Figures
# run after 4.9.24_testing 

pdf(file = "Ch2_figures_4.28.24.pdf")

# Daytime CO2 Elevation 4/9-4/19
daytimeCO2_4.9to4.19

# CO2 elevation between plots
between_boxplots

# Interpolated ∆CO2 for horizontal control
interpolated_CO2

# CO2 elevation above plots, with a shaded area showing height within the screen
above_boxplots

# CO2 elevation with varying vegetation height
veg_boxplot

# CO2 elevation over 10d test: eCO2, aCO2 1 min averages (hourly?) and wind

dev.off()