# CO2 Performance Data for Sarah's ESA Poster
# Thu 8/3/23

library(tidyverse)
library(lubridate)
setwd("/Users/paigekouba/Documents/UC_Davis/2021_Winter/Quals/Proposal/Chapter 1/TinyFACE/CO2_control")
#CO2_early <- read.csv("CR3000_Data20s_till5.26.23.csv")
CO2_early <- read.csv("CR3000_Data20s_early.csv", skip = 1)
CO2_early <- CO2_early[3:1395329,]

CO2_late <- read.csv("CR3000_Data20s_5.26to8.2.csv")
# had to create new datafile as it was getting too large; both stored on Box
CO2_20s_full <- rbind(CO2_early,CO2_late)

CO2_20s <- CO2_20s_full %>%
  filter(TurnCO2On == 1) %>%
  filter(PARuE > 50) 
# only need CO2 elevation during photosynthesis, so the system turns off at night by design
#CO2_20s$TIMESTAMP <- mdy_hm(CO2_20s$TIMESTAMP, tz="America/Los_Angeles")
CO2_20s$TIMESTAMP <- parse_date_time(CO2_20s$TIMESTAMP, orders=c("Ymd HMS", "mdy HM"), tz = "America/Los_Angeles")
# "168 failed to parse"; contains NAs. will figure out later
max(CO2_20s$TIMESTAMP, na.rm = TRUE)

mean(CO2_daytime$DeltaObs)
sd(CO2_daytime$DeltaObs)

spring <- CO2_20s %>% 
  filter(TIMESTAMP > "2023-03-01") 
  filter(TIMESTAMP < "2023-06-01")
# bad performance? pump issues :\ need to sort out when it was 0 data bc pump, and when it was 0 CO2 bc tanks
  
summer <- CO2_20s %>% 
  filter(TurnCO2On == 1) %>%
  filter(PARuE > 50) %>% 
  filter(TIMESTAMP > "2023-06-01") %>% 
  filter(TIMESTAMP < "2023-07-01"| TIMESTAMP > "2023-07-19")
# performance from the summer, minus the times the pump was broken
mean(summer$DeltaObs) # 185.7026

ggplot(summer, aes(x=DeltaObs))+
  geom_density(size=2)+
  geom_vline(aes(xintercept = mean(DeltaObs)),color = "red", linetype="dashed", size=1)+
  labs(title="CO2 Elevation in Summer 2023", 
       x= "∆ CO2 (ppm)",
       y= "Density")+
  theme_classic()+
  theme(axis.text = element_text(size=15), axis.title=element_text(size=20), 
        plot.title=element_text(size=27))

functional <- CO2_20s %>% 
  filter(!is.na(TIMESTAMP)) %>% 
  filter(TIMESTAMP > "2022-10-17") %>% 
  filter(TIMESTAMP < "2023-02-01"| TIMESTAMP > "2023-03-23") %>% 
  filter(TIMESTAMP < "2023-07-01"| TIMESTAMP > "2023-07-19")

plot(functional$TIMESTAMP, functional$DeltaObs)

ggplot(functional, aes(x=DeltaObs))+
  geom_density(size=2)+
  geom_vline(aes(xintercept = mean(DeltaObs, na.rm=TRUE)),color = "red", linetype="dashed", size=1)+
  labs(title="CO2 Elevation since 10/17/22", 
       x= "∆ CO2 (ppm)",
       y= "Density")+
  theme_classic()+
  theme(axis.text = element_text(size=15), axis.title=element_text(size=20), 
        plot.title=element_text(size=27))

ggplot(functional, aes(x=DeltaObs))+
  geom_histogram()

mean(as.numeric(functional$DeltaObs))

functional$DeltaObs <- as.numeric(functional$DeltaObs)
mean(functional$DeltaObs) # 111.3
