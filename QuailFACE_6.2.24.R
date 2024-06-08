# Wed 6/5/24

library(tidyverse)
library(lubridate)
library(dygraphs)
library(xts)

dat <- read_csv("/Users/paigekouba/Documents/UC_Davis/2021_Winter/Quals/Proposal/Chapter 1/TinyFACE/GitHub/QuailFACE/RawData/CR3000_Data20s_6.3.24.csv", skip = 4, col_names = c("TIMESTAMP",	"RECORD",	"BattV_Avg",	"CO2signal",	"CO2ref",	"CO2elev",	"CO2test",	"DeltaObs",	"DeltaSet",	"LicorSig2",	"FlowMFC",	"AirTC",	"RH",	"WS_ms",	"PARuE",	"TurnCO2On",	"SpanObs",	"SpanAct",	"ZeroObs",	"ZeroAct"))
dat$TIMESTAMP <- force_tz(dat$TIMESTAMP, "America/Los_Angeles")

data_6.2.24 <- dat %>% 
  filter(TIMESTAMP > "2024-06-02 06:00:00") %>%
  mutate(DeltaTest = CO2test - CO2ref) %>%
  distinct()

data_6.2.24 %>% 
  select(TIMESTAMP, CO2elev, CO2ref, FlowMFC, PARuE) %>% 
  dygraph() %>% 
  dyRangeSelector()

write.csv(data_6.2.24, "QuailFACE_6.2.24.csv")
