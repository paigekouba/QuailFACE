# Ch 3 Figures
# Friday 7/12/24

#pdf(file = "Ch3_figures_7.12.24.pdf", width=6, height=5)
# Figures
grid.arrange(z_bootV, z_bootL, nrow=2) # mean change (in z scores) with watering for aCO2 and eCO2 conditions

grid.arrange(
  ggpredict(lm(Anet~rescale(CO2)*rescale(SWC)+time_scaled, data=plotmeans.L),
            terms=c("CO2","SWC [4,42]"))%>% plot(rawdata=T,ci=T,colors=c("red","blue")) + labs(title="Anet (µmol CO2/m2/s), Live Oak"), 
  ggpredict(lm(Ht.mm..8~rescale(CO2)*rescale(SWC), data=plotmeans.L), 
            terms=c("CO2","SWC [4,42]"))%>% plot(rawdata=T,ci=T,colors=c("red","blue")) + labs(title="Final Ht (mm), Live Oak"),
  ggpredict(lm(SRL~rescale(CO2)*rescale(SWC), data=plotmeans.L), 
            terms=c("CO2","SWC [4,42]"))%>% plot(rawdata=T,ci=T,colors=c("red","blue")) + labs(title="Specifc Root Length (mm/g), Live Oak"),
  ggpredict(glm(as.numeric(resprout) ~ rescale(CO2)*rescale(meanSWC), family = "binomial", data = filter(herb_list, Spp=="V")), 
            terms=c("CO2","meanSWC [4,42]"))%>% plot(rawdata=T,ci=T,colors=c("red","blue"), jitter=0.07) + labs(title="Resprouting Rates, Valley Oak"))

inv_all_nfh %>% # for seedlings without *full* herbivory, how did height change over the course of the study?
  group_by(Spp, Tmt, value) %>% 
  summarise(mean_ht = mean(ht_mm, na.rm = TRUE),se_ht = sd(ht_mm, na.rm = TRUE)/sqrt(n())) %>% 
  ggplot(aes(x=value, y = mean_ht, group = Tmt)) + 
  geom_line(aes(color = Tmt), position = position_dodge(20, preserve = "total"), linewidth = 1.2) +
  geom_pointrange(aes(ymin = mean_ht - se_ht, ymax = mean_ht + se_ht, color = Tmt, shape = Tmt), fill = "white", position = position_dodge(20, preserve = "total"), size = 1.5, linewidth=1) + 
  scale_color_manual(values = c("#850a01", "#010c85", "#850a01", "#010c85")) +
  scale_shape_manual(values = c(21,21,16,16)) + 
  geom_text(data = inv_nequals, aes(x = value, y = as.numeric(as.factor(Tmt))*12-50, color = Tmt, label = paste0("n=",n))) +
  ylab(label = "Mean Height (mm)") +
  xlab(label = "Date") +
  facet_grid(~ Spp, labeller = as_labeller(c("L" = "Q. wislizeni", "V"= "Q. lobata"))) + theme_classic(base_size = 19)

# Supplement:
quail_weather # average T and rainfall during experiment

CO2_Oaks + theme(panel.background = element_blank(), axis.line = element_line(colour = "black")) # monthly mean and sd for e and aCO2, all time

plot_CO2 # per-plot mean and sd of CO2 tmt

#dev.off()