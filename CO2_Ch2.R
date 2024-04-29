# CO2 Performance for Ch 2
# Thu 3/28/24

library(tidyverse)
library(lubridate)
library(dygraphs)
library(xts)
######################### Warning from 'xts' package ##########################
#                                                                             #
# The dplyr lag() function breaks how base R's lag() function is supposed to  #
# work, which breaks lag(my_xts). Calls to lag(my_xts) that you type or       #
# source() into this session won't work correctly.                            #
#                                                                             #
# Use stats::lag() to make sure you're not using dplyr::lag(), or you can add #
# conflictRules('dplyr', exclude = 'lag') to your .Rprofile to stop           #
# dplyr from breaking base R's lag() function.                                #
#                                                                             #
# Code in packages is not affected. It's protected by R's namespace mechanism #
# Set `options(xts.warn_dplyr_breaks_lag = FALSE)` to suppress this warning.  #
#                                                                             #
###############################################################################

# Hourly data to start out

# CO2_6.8.22_5.24.23_Hourly <- read_csv("/Users/paigekouba/Documents/UC_Davis/2021_Winter/Quals/Proposal/Chapter 1/TinyFACE/CO2_control/CR3000_3.28.24/6.8.22_5.24.23_Hourly.csv", skip = 4, col_names = c("TIMESTAMP",	"RECORD",	"BattV",	"CO2ref",	"CO2elev",	"CO2test",	"AirTC",	"RH",	"WS_ms",	"PARuE",	"DeltaSet",	"FlowMFC",	"MFCbase",	"Pgain",	"Igain",	"TurnCO2On",	"TurnIOn",	"SpanObs",	"SpanAct",	"ZeroObs",	"ZeroAct"))
# 
# CO2_5.25.23_3.27.24_Hourly <- read_csv("/Users/paigekouba/Documents/UC_Davis/2021_Winter/Quals/Proposal/Chapter 1/TinyFACE/CO2_control/CR3000_3.28.24/5.25.23_3.27.24_Hourly.csv", skip = 4, col_names = c("TIMESTAMP",	"RECORD",	"BattV",	"CO2ref",	"CO2elev",	"CO2test",	"AirTC",	"RH",	"WS_ms",	"PARuE",	"DeltaSet",	"FlowMFC",	"MFCbase",	"Pgain",	"Igain",	"TurnCO2On",	"TurnIOn",	"SpanObs",	"SpanAct",	"ZeroObs",	"ZeroAct"))
# 
# CO2_Hourly <- rbind(CO2_6.8.22_5.24.23_Hourly, CO2_5.25.23_3.27.24_Hourly)
# 
# xts_Hourly <- xts(CO2_Hourly, order.by = CO2_Hourly$TIMESTAMP)
# 
# 
# CO2_Hourly %>% 
#   select(TIMESTAMP, CO2ref, CO2elev, CO2test) %>% 
#   #mutate(DeltaObs = CO2elev - CO2ref) %>% 
#   dygraph() %>% 
#   dyRangeSelector()


# 20s data is more complete

CO2_6.8.22_5.24.23_20s <- read_csv("/Users/paigekouba/Documents/UC_Davis/2021_Winter/Quals/Proposal/Chapter 1/TinyFACE/CO2_control/CR3000_3.28.24/6.8.22_5.24.23_20s.csv", skip = 4, col_names = c("TIMESTAMP",	"RECORD",	"BattV_Avg",	"CO2signal",	"CO2ref",	"CO2elev",	"CO2test",	"DeltaObs",	"DeltaSet",	"LicorSig2",	"FlowMFC",	"AirTC",	"RH",	"WS_ms",	"PARuE",	"TurnCO2On",	"SpanObs",	"SpanAct",	"ZeroObs",	"ZeroAct"))

CO2_5.25.23_3.27.24_20s <- read_csv("/Users/paigekouba/Documents/UC_Davis/2021_Winter/Quals/Proposal/Chapter 1/TinyFACE/CO2_control/CR3000_3.28.24/5.25.23_3.27.24_20s.csv", skip = 4, col_names = c("TIMESTAMP",	"RECORD",	"BattV_Avg",	"CO2signal",	"CO2ref",	"CO2elev",	"CO2test",	"DeltaObs",	"DeltaSet",	"LicorSig2",	"FlowMFC",	"AirTC",	"RH",	"WS_ms",	"PARuE",	"TurnCO2On",	"SpanObs",	"SpanAct",	"ZeroObs",	"ZeroAct"))

CO2_one <- CO2_6.8.22_5.24.23_20s
CO2_two <- CO2_5.25.23_3.27.24_20s
CO2_20s <- rbind(CO2_one, CO2_two)

attr(CO2_20s$TIMESTAMP, "tzone") # starts out as UTC, though clock time is correct
# force_tz keeps the clock time but reassigns the tz to be accurate
CO2_20s$TIMESTAMP <- force_tz(CO2_20s$TIMESTAMP, "America/Los_Angeles")
CO2_20s %>% # check that it worked
  filter(TIMESTAMP < "2022-06-09") %>% 
  View() # starts at correct clock time ()

# break into chunks: 4 months (except for grasses = 2 and last chunk of oaks = 2)
oaks_i <- CO2_20s %>% # 6/8/22 - 9/30/22
  filter(TIMESTAMP < "2022-10-01")

oaks_ii <- CO2_20s %>% # 10/1/22 - 1/31/23
  filter(TIMESTAMP > "2022-09-30") %>% 
  filter(TIMESTAMP < "2023-02-01")

oaks_iii <- CO2_20s %>% # 2/1/23 - 5/31/23
  filter(TIMESTAMP > "2023-01-31") %>% 
  filter(TIMESTAMP < "2023-06-01")

oaks_iv <- CO2_20s %>% # 6/1/23 - 9/30/23
  filter(TIMESTAMP > "2023-05-31") %>% 
  filter(TIMESTAMP < "2023-10-01")

oaks_v <- CO2_20s %>% # 10/1/23 - 11/21/23
  filter(TIMESTAMP > "2023-09-30") %>% 
  filter(TIMESTAMP < "2023-11-22")

grass <- CO2_20s %>% # 11/22/23 - 3/27/24
  filter(TIMESTAMP > "2023-11-22")

tail(CO2_20s) # shows that when I was there on Weds, the last time recorded was 18:16:04, which is the local time when I disconnected the DataLogger
# However, if I ask it to filter by values < 2022-06-09, it gives back values up to 022-06-09 06:59:53, which is presumably because UTC is 7h ahead (most of the year...)
# so I went back and used force_tz to change UTC to America/Los_Angeles
  
# now graph
oaks_iv %>% # 6/1/23 - 9/30/23; useful for showing broken pump
  select(TIMESTAMP, CO2ref, CO2elev, CO2test, DeltaObs, PARuE, FlowMFC) %>% 
  dygraph() %>% 
  dyRangeSelector()

oaks_iv %>% # 6/1/23 - 9/30/23
  select(TIMESTAMP, AirTC, RH, WS_ms, PARuE) %>% 
  dygraph() %>% 
  dySeries("WS_ms", axis = "y2") %>% # add second axis for wind
  dyRoller(rollPeriod = 180) %>% 
  dyRangeSelector()

oaks_iv %>% # 6/1/23 - 9/30/23
  select(TIMESTAMP, LicorSig2, PARuE) %>% 
  dygraph() %>% 
  dyRangeSelector()


# The types of problems that can occur are
# 1) no CO2 (tank empty) - skip these days for Ch 2, include for Ch 3
# 2) yes CO2, but no deltaObs (pump broken) - skip for Ch 2, use FlowMFC to infer for Ch 3
# 3) daytime leak in DL box (aCO2 higher than overnight) - One run raw, one with corrected
# 4) LiCOR drift (all is well, but aCO2 is creeping) - One raw, one using nearby NOAA corxn


# I want a few things to prove it works

# all-time ∆CO2 distribution (during daytime, when CO2 tank is full and pump is on)
CO2_operational <- CO2_20s %>% 
 # filter(DeltaObs > 25) %>% # should knock out empty-tank days and broken-pump days ....
  filter(PARuE >= 50) %>% # restricts to just daytime (on) values
  filter(TIMESTAMP < "2023-11-22") %>%  # leaves out GrassFACE era
  filter(TIMESTAMP > "2023-08-01") # reliable CO2 period
# mean of DeltaObs all time until 11/22/23 = 155 ppm
mean(CO2_operational$DeltaObs) # 157.7379
sd(CO2_operational$DeltaObs)# 35.0
# what if you pretend the set point is the mean
sum(CO2_operational$DeltaObs < 157.7*1.1 & CO2_operational$DeltaObs > 157.7*0.9)/nrow(CO2_operational) # 33.5% of the time within 10%
sum(CO2_operational$DeltaObs < 157.7*1.2 & CO2_operational$DeltaObs > 157.7*0.8)/nrow(CO2_operational) # 59.9% of the time within 20%
sum(CO2_operational$DeltaObs < 157.7*1.25 & CO2_operational$DeltaObs > 157.7*0.75)/nrow(CO2_operational) # 69.3% of the time within 25%

sum(CO2_operational$CO2elev < (CO2_operational$CO2ref+157.7)*1.2 & CO2_operational$CO2elev > (CO2_operational$CO2ref+157.7)*0.8)/nrow(CO2_operational)
#[1] 0.9330214 within ∆157.7


plot(density(CO2_operational$DeltaObs))

mean(CO2_operational$CO2elev) # 549
sd(CO2_operational$CO2elev) # 84

mean(CO2_operational$CO2ref) # 391
sd(CO2_operational$CO2ref) # 61

# check on all-time DeltaObs
CO2_20s %>% 
  #filter(DeltaObs > 25) %>% # should knock out empty-tank days and broken-pump days
  filter(PARuE >= 50) %>%
  select(TIMESTAMP, CO2ref, DeltaObs) %>% 
  dygraph() %>% 
  dyRoller(rollPeriod = 180) %>% 
  dyRangeSelector()
# there's an error (Unsupported type passed to argument 'data'.) if you comment out the filter for DeltaObs > 25; why?

# there are two periods when TIMESTAMP = NA
View(CO2_20s[which(is.na(CO2_20s$TIMESTAMP)),])
# starting with RECORD = 756962 and then RECORD = 1971828
View(CO2_20s[which(CO2_20s$RECORD == 756962-50):which(CO2_20s$RECORD == 757141+50),])
# first one starts after 2023-03-12 01:59:45, ends before 2023-03-12 03:00:05
View(CO2_20s[which(CO2_20s$RECORD == 1971828-50):which(CO2_20s$RECORD == 1972007+50),])
# second one starts after 2024-03-10 01:59:53, ends before 2024-03-10 03:00:13
# weird timing, exactly 1hr between 2-3pm on a day in the second week of March??

# check on all-time DeltaObs, try again
CO2_20s %>% 
  filter(!is.na(TIMESTAMP)) %>% # should knock out NAs
  filter(PARuE >= 50) %>%
  select(TIMESTAMP, CO2ref, DeltaObs, BattV_Avg, FlowMFC, SpanObs, ZeroObs) %>% 
  dygraph() %>% 
  dyRoller(rollPeriod = 180) %>% 
  dyRangeSelector()

ggplot(CO2_operational, aes(x=DeltaObs))+
  geom_density()+
  geom_vline(aes(xintercept = mean(DeltaObs)),color = "red", linetype="dashed", size=1)+
  labs(title="CO2 Elevation: Daytime, with Full Tank and Functioning Pump", 
       x= "∆ CO2 (ppm)",
       y= "Density")+
  theme_classic()

# preliminary between-plot testing: 10/9/23
between_plot <- CO2_20s %>% 
  filter(DeltaObs > 25) %>% # should knock out empty-tank days and broken-pump days
  filter(PARuE >= 50) %>% # restricts to just daytime (on) values
  filter(TIMESTAMP > "2023-10-09 10:00:00" & TIMESTAMP < "2023-10-09 18:00:00") %>% 
  select(TIMESTAMP, CO2ref, CO2elev, CO2test, DeltaObs) %>% 
  mutate(DeltaTest = CO2test - CO2ref) %>% 
  dygraph() %>% 
  dyRoller(rollPeriod = 10) %>% 
  dyRangeSelector()

# avg_between <- between_plot %>% 
#   group_by(TIMESTAMP = cut(TIMESTAMP, breaks = "30 min")) %>% 
#   summarize(mDeltaTest = mean(DeltaTest), se = sd(DeltaTest)/sqrt(length(DeltaTest)))

avg_between <- between_plot %>% 
  group_by(TIMESTAMP = cut(TIMESTAMP, breaks = "30 min")) %>% 
  summarize(mDeltaTest = mean(DeltaTest), sd = sd(DeltaTest))

avg_between$Plot <- as.character(c(4:16,1:3))
# ggplot(avg_between, aes(Plot, mDeltaTest)) +
#   geom_point() +
#   geom_pointrange(data = avg_between, aes(ymin=(mDeltaTest - se), ymax = (mDeltaTest + se)))
ggplot(avg_between, aes(Plot, mDeltaTest)) +
  geom_point() +
  geom_pointrange(data = avg_between, aes(ymin=(mDeltaTest - sd), ymax = (mDeltaTest + sd)))

# within-plot testing: 3/27/24

within_plot <- CO2_20s %>% 
  filter(DeltaObs > 25) %>% # should knock out empty-tank days and broken-pump days
  filter(PARuE >= 50) %>% # restricts to just daytime (on) values
  filter(TIMESTAMP > "2024-03-27 12:40:00" & TIMESTAMP < "2024-03-27 18:10:00") %>% 
  select(TIMESTAMP, CO2ref, CO2elev, CO2test, DeltaObs) %>% 
  mutate(DeltaTest = CO2test - CO2ref) %>% 
  dygraph() %>% 
  dyRoller(rollPeriod = 10) %>% 
  dyRangeSelector()

# avg_within <- within_plot %>% 
#   group_by(TIMESTAMP = cut(TIMESTAMP, breaks = "15 min")) %>% 
#   summarize(mDeltaTest = mean(DeltaTest), se = sd(DeltaTest)/sqrt(length(DeltaTest)))
avg_within <- within_plot %>% 
  group_by(TIMESTAMP = cut(TIMESTAMP, breaks = "15 min")) %>% 
  summarize(mDeltaTest = mean(DeltaTest), sd = sd(DeltaTest))

avg_within$Position <- c("center", LETTERS[1:12], paste0(seq(20,100,by=10),"cm"))
avg_within$Position <- factor(avg_within$Position, levels = unique(avg_within$Position))
ggplot(avg_within, aes(Position, mDeltaTest)) +
  geom_point() +
  geom_pointrange(data = avg_within, aes(ymin=(mDeltaTest - sd), ymax = (mDeltaTest + sd)))

avg_within2 <- left_join(avg_within, points_within, join_by(Position == position))
avg_within2 <- avg_within2[1:13,]

interp10.9 <- interp(x = avg_within2$x, y = avg_within2$y, z = avg_within2$mDeltaTest, linear=FALSE, extrap = FALSE)
#convert this to a long form dataframe
interp_df10.9 <- expand_grid(i = seq_along(interp10.9$x), 
                         j = seq_along(interp10.9$y)) %>% 
  mutate(x = interp10.9$x[i],
         y = interp10.9$y[j],
         DeltaTest = map2_dbl(i, j, ~interp10.9$z[.x,.y])) %>% 
  select(-i, -j)

# visualize all interpolated points
ggplot(data = interp_df10.9, aes(x = x, y = y, color = DeltaTest)) + geom_point(size=6) + scale_color_continuous(type = "viridis") + theme_classic()

# visualize all interpolated points, with measured values overlaid
ggplot() + 
  geom_point(data = interp_df10.9, aes(x = x, y = y, color = DeltaTest), size=4.3, shape=15) +
  scale_color_continuous(type = "viridis") + theme_classic() +
  geom_text(data = avg_within2, aes(x=x, y=y, label = round(mDeltaTest, 1)), size = 5)

CO2_20s %>% 
 # filter(DeltaObs > 25) %>% # should knock out empty-tank days and broken-pump days
#  filter(PARuE >= 50) %>% # restricts to just daytime (on) values
  filter( TIMESTAMP < "2023-11-23" &  TIMESTAMP > "2023-08-01") %>% 
  select(TIMESTAMP, RH, LicorSig2) %>% 
  dygraph() %>% 
  dyRangeSelector()

CO2_20s %>% # check windspeed
    # filter(DeltaObs > 25) %>% # should knock out empty-tank days and broken-pump days
   #  filter(PARuE >= 50) %>% # restricts to just daytime (on) values
     filter( TIMESTAMP < "2023-11-23" &  TIMESTAMP > "2023-08-01") %>% 
     select(TIMESTAMP, WS_ms) %>% 
     dygraph() %>% 
     dyRangeSelector() %>% dyRoller(rollPeriod = 180)

# check aCO2 against Walnut Grove
# https://gml.noaa.gov/dv/data/index.php?type=Surface%252BPFP&frequency=Discrete&site=WGC&parameter_name=Carbon%2BDioxide
WGC_CO2 <- read.csv("/Users/paigekouba/Documents/UC_Davis/2021_Winter/Quals/Proposal/Chapter 1/TinyFACE/CO2_control/WGC_CO2.csv")

WGC_CO2 <- WGC_CO2 %>% 
  mutate(timestamp = gsub("[^0-9.-:]", "", datetime)) %>% 
  mutate(timestamp = ymd_hms(timestamp))

WGC_CO2 %>% 
  select (timestamp, value) %>% 
  dygraph() %>% 
  #  dyRoller(rollPeriod = 10) %>% 
  dyRangeSelector()

CO2_20s %>% # graph of CO2ref
  #  filter(TIMESTAMP < "2023-01-01") %>% 
  filter(CO2elev - CO2ref > 25) %>% 
  filter(PARuE >= 50) %>% 
  select(TIMESTAMP, CO2ref) %>% 
  dygraph() %>% 
  dyRoller(rollPeriod = 180) %>% # hourly avg (of 20s data)
  dyRangeSelector()