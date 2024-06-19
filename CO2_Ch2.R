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

CO2_total <- CO2_20s %>% 
  filter(PARuE >= 50) %>% # restricts to just daytime (on) values
  filter(TIMESTAMP < "2023-11-22")

oaks_i %>% 
  filter(PARuE >= 50) %>% # restricts to just daytime (on) values
 # filter(TIMESTAMP < "2023-11-22") %>%  # leaves out GrassFACE era
  select(TIMESTAMP, DeltaObs, FlowMFC, PARuE) %>% 
  dygraph() %>% 
  dyRangeSelector() 

  mean(CO2_total$DeltaObs) # [1] 109.9424
  median(CO2_total$DeltaObs) # 130.8
  mean(CO2_total$CO2ref) # 485.1931
  median(CO2_total$CO2ref) # 436.1647
  mean(CO2_total$CO2elev) # 593.9587 (+108.8)
  median(CO2_total$CO2elev) # 577.118 (+ 141.0)
  
# raw plot of eCO2 and aCO2 distributions per month
  CO2_total %>% 
    group_by(month=cut(TIMESTAMP, breaks = "1 month"), drop=FALSE) %>% 
    mutate(month=ymd(month)) %>% 
    filter(month < "2023-02-15" | month > "2023-04-07") %>% 
    summarise(medianCO2ref= median(CO2ref), medianCO2elev=median(CO2elev)) %>% 
    ungroup() %>% 
  ggplot() +
    geom_point(aes(x=month, y=medianCO2ref), color="darkgray") +
    geom_point(aes(x=month, y=medianCO2elev), color="black") +
    geom_line(aes(x=month, y=medianCO2ref), color="darkgray") +
    geom_line(aes(x=month, y=medianCO2elev), color="black") +
    scale_x_date(date_breaks="1 month")
  
# next I should add back the predicted eCO2 values based on the corrected df below (and interpolate aCO2)
  # or actually, just do the above plotting on the corrected dataset!?
  # not quite; only modeled DeltaObs. Can do the same modeling (FlowMFC-based) for eCO2; and then use average of months on either side as aCO2
  
# here is where I will connect ∆Obs with FlowMFC and then back out the overall average ∆CO2
CO2_total %>%
  select(TIMESTAMP, CO2ref, CO2elev, FlowMFC) %>%
  dygraph() %>%
  dyRangeSelector() %>%
  dyRoller(rollPeriod=180)
  
# the periods I need to fix are 2/15/23--3/2/23, and 7/1/23-7/22/23; a total of 15 + 21 = 36 days
# I should make a relationship between DeltaObs and FlowMFC for all the days NOT counting those days
  # then use that to predict DeltaObs from FlowMFC

CO2_op_dailymeans <- CO2_operational %>% 
  filter(DeltaObs < 1000 & DeltaObs > -200) %>% 
  filter(TurnCO2On == 1) %>% 
  group_by(TIMESTAMP = cut(TIMESTAMP, breaks = "1 day")) %>% 
  summarise(DeltaObs = mean(DeltaObs), FlowMFC = mean(FlowMFC), CO2elev=mean(CO2elev)) 

CO2_op_dailymeans %>% 
ggplot() +
  geom_point(aes(x=FlowMFC, y=DeltaObs), alpha=0.4) +
  geom_smooth(aes(x=FlowMFC, y=DeltaObs), method="lm")

CO2_op_dailymeans %>% 
  ggplot() +
  geom_point(aes(x=FlowMFC, y=CO2elev), alpha=0.4) +
  geom_smooth(aes(x=FlowMFC, y=CO2elev), method="lm")

summary(lm(DeltaObs ~ FlowMFC, data= CO2_op_dailymeans))
# Coefficients:
#   Estimate Std. Error t value Pr(>|t|)    
#   (Intercept) -8.63487   18.68306  -0.462    0.645    
#   meanFlowMFC  0.10611    0.01181   8.986 1.43e-14 ***

summary(lm(CO2elev ~ FlowMFC, data= CO2_op_dailymeans))
# Coefficients:
#   Estimate Std. Error t value Pr(>|t|)    
#   (Intercept) 389.61233   45.38373   8.585 1.08e-13 ***
#   FlowMFC       0.10249    0.02869   3.573 0.000541 ***

CO2_operational %>% 
  filter(DeltaObs < 1000 & DeltaObs > -200) %>% 
  filter(TurnCO2On == 1) %>% 
  ggplot() +
  geom_point(aes(x=FlowMFC, y=DeltaObs), alpha=0.4) +
  geom_smooth(aes(x=FlowMFC, y=DeltaObs), method="lm")

summary(lm(DeltaObs ~ FlowMFC, data= CO2_operational))

# I think, use the daily means model to establish the relationship, then use instantaneous FlowMFC for the broken pump days. Try that

MFCmod <- lm(DeltaObs ~ FlowMFC, data= CO2_op_dailymeans)
CO2elevmod <- lm(CO2elev ~ FlowMFC, data=CO2_op_dailymeans)

# CO2_total_corr <- CO2_total
# CO2_total_corr$DeltaObs_pred <- predic(MFCmod, )
  
 # biomass_nh2$predVW <- predict(lm_VW, newdata = biomass_nh2)

CO2_total %>% 
  filter((TIMESTAMP > "2023-02-15" & TIMESTAMP < "2023-04-07") | (TIMESTAMP > "2023-07-01" & TIMESTAMP < "2023-07-23")) %>% 
  select(CO2ref) %>% 
  colMeans() # aCO2 mean = 757.6 for broken pump periods

CO2_total %>% 
  filter(TIMESTAMP < "2023-02-15" |  (TIMESTAMP > "2023-04-07" & TIMESTAMP < "2023-07-01") | TIMESTAMP > "2023-07-23")  %>% 
  select(CO2ref) %>% 
  colMeans() # 432.6682 
CO2ref_mean <- 432.6682 

CO2_total_corr <- CO2_total %>% 
  filter(CO2ref>0, CO2elev>0, FlowMFC!=0) %>% 
  mutate(hms = lubridate::hms(format(TIMESTAMP, "%H:%M:%S"))) %>% 
  filter(TIMESTAMP > "2022-10-11" | hms > lubridate::hms("10:00:00")) %>% 
  mutate(brokenpump = case_when( (TIMESTAMP > "2023-02-15" & TIMESTAMP < "2023-04-07") ~ 1,  
                                (TIMESTAMP > "2023-04-07" & TIMESTAMP < "2023-05-11") ~ 2,
                                (TIMESTAMP > "2023-07-01" & TIMESTAMP < "2023-07-23") ~ 3,
                                .default = 0)) %>% 
  mutate(DeltaObs_pred = predict(MFCmod, newdata=CO2_total%>% 
                                 filter(CO2ref>0, CO2elev>0, FlowMFC!=0) %>% 
                                 mutate(hms = lubridate::hms(format(TIMESTAMP, "%H:%M:%S"))) %>% 
                                 filter(TIMESTAMP>"2022-10-11" |hms>lubridate::hms("10:00:00"))))%>% 
  #  ggplot() + geom_point(aes(x=DeltaObs, y=DeltaObs_pred))
  mutate(DeltaObs_corr = case_when(brokenpump==0 ~ DeltaObs,
                                   brokenpump!=0 ~ DeltaObs_pred)) %>% 
  mutate(CO2elev_pred = predict(CO2elevmod, newdata=CO2_total%>% 
                                  filter(CO2ref>0, CO2elev>0, FlowMFC!=0) %>% 
                                  mutate(hms = lubridate::hms(format(TIMESTAMP, "%H:%M:%S"))) %>% 
                                  filter(TIMESTAMP>"2022-10-11" |hms>lubridate::hms("10:00:00"))))%>% 
  mutate(CO2elev_corr = case_when(brokenpump==0 ~ CO2elev,
                                  brokenpump!=0 ~ CO2elev_pred)) %>% 
  mutate(CO2ref_corr = case_when(brokenpump==0 ~ CO2ref,
                                 brokenpump==1 ~ mean(c(419,413)),
                                 brokenpump==2 ~ mean(c(419,413)),
                                 brokenpump==3 ~ mean(c(405,393))))

CO2_total_corr0 <- CO2_total %>% 
  filter(CO2ref>0, CO2elev>0, FlowMFC!=0) %>% 
  mutate(brokenpump = case_when( (TIMESTAMP > "2023-02-15" & TIMESTAMP < "2023-04-07") ~ 1,  
                                 (TIMESTAMP > "2023-04-07" & TIMESTAMP < "2023-05-11") ~ 2,
                                 (TIMESTAMP > "2023-07-01" & TIMESTAMP < "2023-07-23") ~ 3,
                                 .default = 0)) %>% 
  mutate(DeltaObs_pred = predict(MFCmod, newdata=CO2_total%>% 
                                   filter(CO2ref>0, CO2elev>0, FlowMFC!=0))) %>% 
  #  ggplot() + geom_point(aes(x=DeltaObs, y=DeltaObs_pred))
  mutate(DeltaObs_corr = case_when(brokenpump==0 ~ DeltaObs,
                                   brokenpump!=0 ~ DeltaObs_pred)) %>% 
  mutate(CO2elev_pred = predict(CO2elevmod, newdata=CO2_total%>% 
                                  filter(CO2ref>0, CO2elev>0, FlowMFC!=0))) %>% 
  mutate(CO2elev_corr = case_when(brokenpump==0 ~ CO2elev,
                                  brokenpump!=0 ~ CO2elev_pred)) %>% 
  mutate(CO2ref_corr = case_when(brokenpump==0 ~ CO2ref,
                                 brokenpump==1 ~ mean(c(419,413)),
                                 brokenpump==2 ~ mean(c(419,413)),
                                 brokenpump==3 ~ mean(c(405,393))))

# pausing here; I attempted to filter out the early-morning spikes from dates before 10/11/22, but it didn't seem to change to output of the plot at all so I'm wondering if I missed something.

library(hms)

CO2_total_corr %>% 
  select(TIMESTAMP, CO2ref, CO2ref_corr, CO2elev, CO2elev_corr, DeltaObs_corr, brokenpump) %>% 
  summarise_if(is.numeric, median, na.rm = T)
  # mutate_if(negate(is.numeric), ~ -999) %>%
  # colMeans(na.rm = TRUE)

# median
#       CO2ref   CO2ref_corr       CO2elev  CO2elev_corr DeltaObs_corr    brokenpump 
#         433.           416          578.          566.           155.            0

# mean
#       CO2ref   CO2ref_corr       CO2elev  CO2elev_corr DeltaObs_corr    brokenpump 
#         489.          422.          602.          544.           127.        0.460


mean(CO2_total_corr$DeltaObs_corr) #  127.2421
median(CO2_total_corr$DeltaObs_corr) # 155.1963
# also need a mean (corrected) for aCO2 and eCO2
# should get these from all days EXCEPT broken pump (2/15/23--4/7/23, and 7/1/23-7/22/23)
mean(CO2_total_corr$CO2ref_corr) # 421.5844
median(CO2_total_corr$CO2ref_corr) # 416
mean(CO2_total_corr$CO2elev_corr) # 544.2796
median(CO2_total_corr$CO2elev_corr) # 565.7973

CO2_total_corr %>% 
  ggplot() +
  geom_point(aes(x=TIMESTAMP, y=CO2elev_corr), color="black") +
  geom_point(aes(x=TIMESTAMP, y=CO2ref_corr), color="darkgray")

# also need aCO2 interpolated for broken-pump days
# then, scale 10/9 plotwise eCO2 values to overall average

CO2_total_corr %>% 
  group_by(month=cut(TIMESTAMP, breaks = "1 month")) %>% 
  mutate(month=ymd(month)) %>% 
  # ggplot() +
  # geom_boxplot(aes(x=month, y=CO2elev_corr, group = month))
  summarise(meanCO2ref_corr= mean(CO2ref_corr), yminCO2ref = mean(CO2ref_corr)-sd(CO2ref_corr), ymaxCO2ref = mean(CO2ref_corr)+sd(CO2ref_corr), meanCO2elev_corr=mean(CO2elev_corr), yminCO2elev = mean(CO2elev_corr)-sd(CO2elev_corr), ymaxCO2elev = mean(CO2elev_corr)+sd(CO2elev_corr)) %>% 
  ungroup() %>% 
  ggplot() +
  geom_pointrange(aes(x=month, y=meanCO2ref_corr, ymin=yminCO2ref, ymax=ymaxCO2ref), color="darkgray", size=1.2, linewidth=1.2, position=position_nudge(-2.5,0)) +
  geom_pointrange(aes(x=month, y=meanCO2elev_corr, ymin=yminCO2elev, ymax=ymaxCO2elev), color="black", size=1.2, linewidth=1.2, position=position_nudge(2.5,0)) +
  # geom_point(aes(x=month, y=meanCO2ref_corr), color="darkgray") +
  # geom_point(aes(x=month, y=meanCO2elev_corr), color="black") +
  geom_line(aes(x=month, y=meanCO2ref_corr), color="darkgray", linewidth=1, position=position_nudge(-2.5,0)) +
  geom_line(aes(x=month, y=meanCO2elev_corr), color="black", linewidth=1, position=position_nudge(2.5,0)) +
  scale_x_date(date_breaks="1 month") +
  theme(axis.text.x = element_text(size = 12, angle = 45, hjust = 1), axis.text.y = element_text(size = 12)) +
  labs(title = "eCO2 and aCO2: Monthly Mean Values (sd)") +
  ylab("CO2 Concentration (ppm)")

CO2_total_corr %>% 
  select(TIMESTAMP, CO2ref_corr, CO2elev_corr) %>% 
  dygraph() %>% 
  dyRoller(rollPeriod = 180) %>% 
  dyRangeSelector()


# mean of DeltaObs all time until 11/22/23 = 155 ppm
mean(CO2_operational$DeltaObs) # 157.7379
sd(CO2_operational$DeltaObs)# 35.0
# what if you pretend the set point is the mean
sum(CO2_operational$DeltaObs < 157.7*1.1 & CO2_operational$DeltaObs > 157.7*0.9)/nrow(CO2_operational) # 33.5% of the time within 10%
sum(CO2_operational$DeltaObs < 157.7*1.2 & CO2_operational$DeltaObs > 157.7*0.8)/nrow(CO2_operational) # 59.9% of the time within 20%
sum(CO2_operational$DeltaObs < 157.7*1.25 & CO2_operational$DeltaObs > 157.7*0.75)/nrow(CO2_operational) # 69.3% of the time within 25%

sum(CO2_operational$CO2elev < (CO2_operational$CO2ref+157.7)*1.2 & CO2_operational$CO2elev > (CO2_operational$CO2ref+157.7)*0.8)/nrow(CO2_operational)
#[1] 0.9330214 within ∆157.7

# redo this for CO2_total_corr
sum(CO2_total_corr$CO2elev_corr < (CO2_total_corr$CO2ref_corr+127.2421)*1.2 & CO2_total_corr$CO2elev_corr > (CO2_total_corr$CO2ref_corr+127.2421)*0.8)/nrow(CO2_total_corr) # 76.3% within 20%
sum(CO2_total_corr$CO2elev_corr < (CO2_total_corr$CO2ref_corr+127.2421)*1.25 & CO2_total_corr$CO2elev_corr > (CO2_total_corr$CO2ref_corr+127.2421)*0.75)/nrow(CO2_total_corr) # 88.1% within 25%

plot(density(CO2_operational$DeltaObs))
plot(density(CO2_total_corr$DeltaObs))

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
  labs(title="CO2 Elevation", #: Daytime, with Full Tank and Functioning Pump", 
       x= "∆ CO2 (ppm)",
       y= "Density")+
  theme_classic()

# preliminary between-plot testing: 10/9/23
between_plot <- CO2_20s %>% 
#  filter(DeltaObs > 25) %>% # should knock out empty-tank days and broken-pump days
#  filter(PARuE >= 50) %>% # restricts to just daytime (on) values
  filter(TIMESTAMP > "2023-10-09 10:00:00" & TIMESTAMP < "2023-10-09 18:00:00") %>% 
  dplyr::select(TIMESTAMP, CO2ref, CO2elev, CO2test, DeltaObs) %>% 
  mutate(DeltaTest = CO2test - CO2ref) #%>% 
  # dygraph() %>% 
  # dyRoller(rollPeriod = 10) %>% 
  # dyRangeSelector()

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
ggplot(avg_between, aes(factor(Plot, levels = c("1","2","3","4","5","6","7","8","9","10","11","12","13","14","15","16")), mDeltaTest)) +
  geom_point() +
  geom_pointrange(data = avg_between, aes(ymin=(mDeltaTest - sd), ymax = (mDeltaTest + sd))) +
  theme_classic(base_size = 20)

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
  geom_pointrange(data = avg_within, aes(ymin=(mDeltaTest - sd), ymax = (mDeltaTest + sd))) +
  theme_classic(base_size = 20)

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

weather <- read.csv("/Users/paigekouba/Documents/UC_Davis/2021_Winter/Quals/Proposal/Chapter 1/TinyFACE/GitHub/QuailFACE/RawData/QuailWeather2.csv")
head(weather)
typeof(weather$Time)
mdy_hm(weather[1:10,1])
weather$Time <- mdy_hm(weather$Time)
weather$Time <- force_tz(weather$Time, "America/Los_Angeles")

quail_weather <- weather %>% 
  select(Time,Quail.Ridge.Rainfall.Cumulative.mm, Quail.Ridge.Air.Temp.Avg.degC) %>% 
  filter(Quail.Ridge.Air.Temp.Avg.degC > -5 & Quail.Ridge.Air.Temp.Avg.degC < 37) %>% 
  mutate(day=substr(Time,1,10)) %>% 
  group_by(day) %>% 
  summarise(meanRain = mean(Quail.Ridge.Rainfall.Cumulative.mm), meanTemp = mean(Quail.Ridge.Air.Temp.Avg.degC)) %>% ungroup() %>% 
  ggplot(aes(x=ymd(day))) +
  geom_line(aes(y=meanRain),color="darkblue") +
  geom_line(aes(y=meanTemp*19.5),color="darkorange") +
  scale_y_continuous(name="Cumulative Rainfall (mm)",
                     sec.axis=sec_axis(~.*0.05128205, name="Average Temperature (ºC)")) +
  xlab(label="Date") +
  theme_classic(base_size = 18)

