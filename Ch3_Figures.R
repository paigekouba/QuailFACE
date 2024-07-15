# Ch 3 Figures
# Friday 7/12/24

pdf(file = "Ch3_figures_7.12.24.pdf", width=6, height=5)
# Figures
grid.arrange(fig2V_nh, fig2L_nh, nrow=2) # mean % change with eCO2 for watered and water-stressed plants

grid.arrange(
  ggpredict(lm(Anet~rescale(CO2)*rescale(SWC)+time_scaled, data=plotmeans.L),
            terms=c("CO2","SWC [4,42]"))%>% plot(rawdata=T,ci=T,colors=c("red","blue")) + labs(title="Anet (µmol CO2/m2/s), Live Oak"), 
  ggpredict(lm(Ht.mm..8~rescale(CO2)*rescale(SWC), data=plotmeans.L), 
            terms=c("CO2","SWC [4,42]"))%>% plot(rawdata=T,ci=T,colors=c("red","blue")) + labs(title="Final Ht (mm), Live Oak"),
  ggpredict(lm(SRL~rescale(CO2)*rescale(SWC), data=plotmeans.L), 
            terms=c("CO2","SWC [4,42]"))%>% plot(rawdata=T,ci=T,colors=c("red","blue")) + labs(title="Specifc Root Length (mm/g), Live Oak"),
  ggpredict(lm(tot_area~rescale(CO2)*rescale(SWC), data=plotmeans.V), 
            terms=c("CO2","SWC [4,42]"))%>% plot(rawdata=T,ci=T,colors=c("red","blue")) + labs(title="Total Leaf Area (mm2), Valley Oak"))

# Supplement:
quail_weather # average T and rainfall during experiment

CO2_Oaks # monthly mean and sd for e and aCO2, all time

plot_CO2 # per-plot mean and sd of CO2 tmt

dev.off()