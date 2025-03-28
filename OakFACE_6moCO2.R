CO2_total_corr %>% 
  filter(TIMESTAMP > "2023-06-01") %>% 
  group_by(day=cut(as.Date(TIMESTAMP), breaks = "1 day")) %>% 
  mutate(day=ymd(day)) %>% 
  summarise(meanCO2ref_corr= mean(CO2ref_corr, na.rm = T), yminCO2ref = mean(CO2ref_corr, na.rm = T)-sd(CO2ref_corr, na.rm = T), ymaxCO2ref = mean(CO2ref_corr, na.rm = T)+sd(CO2ref_corr, na.rm = T), meanCO2elev_corr=mean(CO2elev_corr, na.rm = T), yminCO2elev = mean(CO2elev_corr, na.rm = T)-sd(CO2elev_corr, na.rm = T), ymaxCO2elev = mean(CO2elev_corr, na.rm = T)+sd(CO2elev_corr, na.rm = T)) %>% 
  ungroup() %>%
  ggplot() +
  geom_pointrange(aes(x=day, y=meanCO2ref_corr, ymin=yminCO2ref, ymax=ymaxCO2ref), color="darkgray", position=position_nudge(-2.5,0)) +
  geom_pointrange(aes(x=day, y=meanCO2elev_corr, ymin=yminCO2elev, ymax=ymaxCO2elev), color="black", position=position_nudge(2.5,0)) +
  geom_line(aes(x=day, y=meanCO2ref_corr), color="darkgray", linewidth=1, position=position_nudge(-2.5,0)) +
  geom_line(aes(x=day, y=meanCO2elev_corr), color="black", linewidth=1, position=position_nudge(2.5,0)) +
  scale_x_date(date_breaks="1 month") +
  theme(axis.text.x = element_text(size = 12, angle = 45, hjust = 1), axis.text.y = element_text(size = 12)) +
  labs(title = "eCO2 and aCO2: Daily Mean Values (sd) for June-Dec 2023") +
  ylab("CO2 Concentration (ppm)")


CO2_total_corr %>% 
  filter(TIMESTAMP > "2023-06-01") %>% 
  summarise(meanCO2ref_corr= mean(CO2ref_corr, na.rm = T), sdCO2ref = sd(CO2ref_corr, na.rm = T), meanCO2elev_corr=mean(CO2elev_corr, na.rm = T), sdCO2elev = sd(CO2elev_corr, na.rm = T), meandeltaObs = mean(CO2elev_corr - CO2ref_corr, na.rm =T), sddeltaObs = sd(CO2elev_corr - CO2ref_corr, na.rm =T)) %>% 
  ungroup() 
#     meanCO2ref_corr sdCO2ref meanCO2elev_corr sdCO2elev
#               <dbl>    <dbl>            <dbl>     <dbl>
#   1            400.     27.7             567.      53.0