# Thu 5/29/25
# Script to process and analyze data for CO2 x watering expt, SEMs

# Step 1: get reference ∆CO2 value per plot, using all-time CO2 data and 10/9/23 testing
library(tidyverse)
library(lubridate)
library(dplyr)

# load 20s CO2 data from June 2022 to March 2024
CO2_6.8.22_5.24.23_20s <- read_csv("/Users/paigekouba/Documents/UC_Davis/2021_Winter/Quals/Proposal/Chapter 1/TinyFACE/CO2_control/CR3000_3.28.24/6.8.22_5.24.23_20s.csv", skip = 4, col_names = c("TIMESTAMP",	"RECORD",	"BattV_Avg",	"CO2signal",	"CO2ref",	"CO2elev",	"CO2test",	"DeltaObs",	"DeltaSet",	"LicorSig2",	"FlowMFC",	"AirTC",	"RH",	"WS_ms",	"PARuE",	"TurnCO2On",	"SpanObs",	"SpanAct",	"ZeroObs",	"ZeroAct"))
CO2_5.25.23_3.27.24_20s <- read_csv("/Users/paigekouba/Documents/UC_Davis/2021_Winter/Quals/Proposal/Chapter 1/TinyFACE/CO2_control/CR3000_3.28.24/5.25.23_3.27.24_20s.csv", skip = 4, col_names = c("TIMESTAMP",	"RECORD",	"BattV_Avg",	"CO2signal",	"CO2ref",	"CO2elev",	"CO2test",	"DeltaObs",	"DeltaSet",	"LicorSig2",	"FlowMFC",	"AirTC",	"RH",	"WS_ms",	"PARuE",	"TurnCO2On",	"SpanObs",	"SpanAct",	"ZeroObs",	"ZeroAct"))
CO2_20s <- rbind(CO2_6.8.22_5.24.23_20s, CO2_5.25.23_3.27.24_20s)
# starts out as UTC, though clock time is correct
# force_tz keeps the clock time but reassigns the tz to be accurate
# BUT also causes issues with DST
# CO2_20s$TIMESTAMP <- force_tz(CO2_20s$TIMESTAMP, "America/Los_Angeles", roll_dst = c("boundary", "post"))

# the CO2 treatment is imperfectly represented so far
# The types of problems that can occur are
# 1) no CO2 (tank empty) - must include for accurate characterization of oak treatment
# 2) yes CO2, but no deltaObs (pump broken) - use FlowMFC to infer deltaObs for these days
# 3) [leaks in DL box] (not currently addressed)
# 4) [LiCOR drift] (not currently addressed)

# the periods I need to fix are 2/15/23--3/2/23, and 7/1/23-7/22/23; a total of 15 + 21 = 36 days
# I should make a relationship between DeltaObs and FlowMFC for all the days NOT counting those days
# then use that to predict DeltaObs from FlowMFC

# all-time ∆CO2 distribution (during daytime)
CO2_oaks <- CO2_20s %>% 
  filter(PARuE >= 50) %>% # restricts to just daytime (on) values
  filter(TIMESTAMP < "2023-11-22") %>%  # ends when seedlings are harvested
  filter(TIMESTAMP > "2022-08-01") # leaving out early calibration period

CO2_pumpOn <- CO2_oaks %>% 
  filter(PARuE >= 50) %>% # restricts to just daytime (on) values
  filter(!(TIMESTAMP >= "2023-02-15" & TIMESTAMP <= "2023-03-02")) %>% # drops power outage period
  filter(!(TIMESTAMP >= "2023-07-01" & TIMESTAMP <= "2023-07-22")) # drops broken pump period

CO2_alltime <- CO2_20s %>% 
  filter(PARuE >= 50) %>% # restricts to just daytime (on) values
  filter(TIMESTAMP > "2022-08-01") # leaving out early calibration period

CO2_dailymeans <- CO2_pumpOn %>% 
  filter(DeltaObs < 1000 & DeltaObs > -200) %>% # remove errors and CO2 leaks
  filter(TurnCO2On == 1) %>% # only when CO2 is turned on
  group_by(TIMESTAMP = cut(TIMESTAMP, breaks = "1 day")) %>% 
  summarise(DeltaObs = mean(DeltaObs), FlowMFC = mean(FlowMFC), CO2elev=mean(CO2elev)) 
# taking daily means to reduce noise

# connect ∆Obs with FlowMFC and use that to interpolate missing days
summary(lm(DeltaObs ~ FlowMFC, data=CO2_dailymeans))
# Coefficients:
#   Estimate Std. Error t value Pr(>|t|)    
# (Intercept) -2.176405   5.151845  -0.422    0.673    
# FlowMFC      0.089456   0.003426  26.110   <2e-16 ***
# Adjusted R-squared:  0.6235  

# how well does FlowMFC predict DeltaObs? Visualize:
ggplot(CO2_dailymeans, aes(x=FlowMFC, y = DeltaObs, alpha = 0.2)) + geom_smooth(method = "lm") + geom_point() 
# most FlowMFC values are between 1500-2000; makes sense, MFCBase set to 1000
#  use the daily means model to establish the relationship, then use instantaneous FlowMFC for the broken pump days
MFCmod <- lm(DeltaObs ~ FlowMFC, data= CO2_dailymeans)
CO2elevmod <- lm(CO2elev ~ FlowMFC, data=CO2_dailymeans)

CO2_alltime_corr <- CO2_alltime %>% 
  filter(CO2ref>0, CO2elev>0, FlowMFC!=0) %>% 
  mutate(brokenpump = case_when( (TIMESTAMP > "2023-02-15" & TIMESTAMP < "2023-04-07") ~ 1,  
                                 (TIMESTAMP > "2023-04-07" & TIMESTAMP < "2023-05-11") ~ 2,
                                 (TIMESTAMP > "2023-07-01" & TIMESTAMP < "2023-07-23") ~ 3,
                                 .default = 0)) %>% 
  mutate(DeltaObs_pred = predict(MFCmod, newdata=CO2_alltime%>% 
                                   filter(CO2ref>0, CO2elev>0, FlowMFC!=0))) %>% 
  mutate(DeltaObs_corr = case_when(brokenpump==0 ~ DeltaObs,
                                   brokenpump!=0 ~ DeltaObs_pred)) %>% 
  mutate(CO2elev_pred = predict(CO2elevmod, newdata=CO2_alltime%>% 
                                  filter(CO2ref>0, CO2elev>0, FlowMFC!=0))) %>% 
  mutate(CO2elev_corr = case_when(brokenpump==0 ~ CO2elev,
                                  brokenpump!=0 ~ CO2elev_pred)) %>% 
  mutate(CO2ref_corr = case_when(brokenpump==0 ~ CO2ref,
                                 brokenpump==1 ~ mean(c(419,413)),
                                 brokenpump==2 ~ mean(c(419,413)),
                                 brokenpump==3 ~ mean(c(405,393)))) 

# mean(CO2_alltime_corr$DeltaObs_corr) # 124.3926
# median(CO2_alltime_corr$DeltaObs_corr) # 148.4
# mean(CO2_alltime_corr$CO2ref_corr) # 424.3407
# median(CO2_alltime_corr$CO2ref_corr) # 416 (because mean of 413, 419...)
# mean(CO2_alltime_corr$CO2elev_corr) # 568.6407
# median(CO2_alltime_corr$CO2elev_corr) # 581.1878

# big gross daily means graph
# CO2_alltime_corr %>% 
#   group_by(day=cut(TIMESTAMP, breaks = "1 day")) %>% 
#   mutate(day=ymd(day)) %>% 
#   summarise(meanCO2ref_corr= mean(CO2ref_corr, na.rm=T), yminCO2ref = mean(CO2ref_corr, na.rm=T)-sd(CO2ref_corr, na.rm=T), ymaxCO2ref = mean(CO2ref_corr, na.rm=T)+sd(CO2ref_corr, na.rm=T), meanCO2elev_corr=mean(CO2elev_corr, na.rm=T), yminCO2elev = mean(CO2elev_corr, na.rm=T)-sd(CO2elev_corr, na.rm=T), ymaxCO2elev = mean(CO2elev_corr, na.rm=T)+sd(CO2elev_corr, na.rm=T)) %>% 
#   ungroup() %>% 
#   ggplot() +
#   geom_pointrange(aes(x=day, y=meanCO2ref_corr, ymin=yminCO2ref, ymax=ymaxCO2ref), color="darkgray", position=position_nudge(-2.5,0)) +
#   geom_pointrange(aes(x=day, y=meanCO2elev_corr, ymin=yminCO2elev, ymax=ymaxCO2elev), color="black", position=position_nudge(2.5,0)) +
#   # geom_point(aes(x=day, y=meanCO2ref_corr), color="darkgray") +
#   # geom_point(aes(x=day, y=meanCO2elev_corr), color="black") +
#   geom_line(aes(x=day, y=meanCO2ref_corr), color="darkgray", position=position_nudge(-2.5,0)) +
#   geom_line(aes(x=day, y=meanCO2elev_corr), color="black", position=position_nudge(2.5,0)) +
#   scale_x_date(date_breaks="1 month") +
#   theme(axis.text.x = element_text(size = 12, angle = 45, hjust = 1), axis.text.y = element_text(size = 12)) +
#   labs(title = "eCO2 and aCO2: Daily Mean Values (sd)") +
#   ylab("CO2 Concentration (ppm)")

# Now, per-plot means from testing on October 9
between_plot <- CO2_20s %>% 
  mutate(TIMESTAMP = force_tz(TIMESTAMP, "America/Los_Angeles")) %>% # updates TZ from UTC
  filter(TIMESTAMP > "2023-10-09 10:00:00" & TIMESTAMP < "2023-10-09 18:00:00") %>% 
  dplyr::select(TIMESTAMP, CO2ref, CO2elev, CO2test, DeltaObs) %>% 
  mutate(DeltaTest = CO2test - CO2ref) 

avg_between <- between_plot %>% 
  group_by(TIMESTAMP = cut(TIMESTAMP, breaks = "30 min")) %>% 
  summarize(mDeltaTest = mean(DeltaTest), sd = sd(DeltaTest)) %>% 
  mutate(Plot = as.character(c(4:16,1:3)))
# ggplot(avg_between, aes(factor(Plot, levels = c("1","2","3","4","5","6","7","8","9","10","11","12","13","14","15","16")), mDeltaTest)) +
#   geom_point() +
#   geom_pointrange(data = avg_between, aes(ymin=(mDeltaTest - sd), ymax = (mDeltaTest + sd)))

# now set up per-plot CO2 treatment levels, based on October 9 testing
min(avg_between$mDeltaTest) # -6.114156
max(avg_between$mDeltaTest) # 256.731
mean(avg_between$mDeltaTest[avg_between$mDeltaTest>100]) # 210.8419
mean(avg_between$mDeltaTest[avg_between$mDeltaTest<100]) # 8.442497
# this day, the mean elevation was 210; but I will need per-plot CO2 levels, scaled to the requisite fraction of the *all-time* mean elevation (124.3926 ppm) added to the mean ambient value (424.3407)

plot_CO2 <- avg_between %>% 
  select(Plot, mDeltaTest) %>% 
  mutate(CO2 = case_when( 
    mDeltaTest > 100 ~ (424.3407 + mDeltaTest)*((424.3407+124.3926)/(424.3407+210.8419)),
    mDeltaTest < 100 ~ 424.3407 + mDeltaTest) )

# Step 2: read in and process LiCOR data
library(stringr)

setwd("~/Documents/UC_Davis/2021_Winter/Quals/Proposal/Chapter 1/TinyFACE/GitHub/QuailFACE")
LiCOR_3 <- read.csv("RawData/LiCOR_8.26.23.csv") # LiCOR datafile
crosswalk3 <- read.csv("RawData/Survey3.csv") # data sheet with observation and plant codes

crosswalk3 <- crosswalk3[-c(1:2),c(1:6)]
lookup <- data.frame(as.character(c(1:16)), c("AW","ED","AD","EW",
                                              "AW","ED","EW","AD",
                                              "ED","AW","EW","AD",
                                              "ED","AW","EW","AD")) # tmt codes per plot
names(lookup) <- c("Plot","Tmt") 
# data entry error; 2-digit plots getting assigned as "1"
crosswalk3 <- crosswalk3 %>% 
  mutate(Plot = if_else(nchar(ID) == 4, substr(ID,1,1), substr(ID,1,2))) %>% # add Plot 
  left_join(lookup, by = "Plot")
# data entry error; log for 12V6b is listed as 12 four times
#which(crosswalk3$Log == "12")
# 40 166 207 208 209 210
crosswalk3[208,3] <- 13
crosswalk3[209,3] <- 14
crosswalk3[210,3] <- 15
# data entry error; log for 2V4b taken 8/26/23, misentered as 8/29
crosswalk3[crosswalk3$ID == "2V4b",2] <- "8/26/23"

crosswalk3$SurveyDay <- # associate sampling dates with survey days
  case_match( crosswalk3$Date,
              c("8/26","8/26/23") ~ 1,
              c("8/27") ~ 2,
              c("8/29","8/29/23") ~ 3)

crosswalk3$LiCOR_ID <- paste0(crosswalk3$SurveyDay, "_", crosswalk3$Log) # get a unique ID for each measurement: survey day x log #
LiCOR_3$LiCOR_ID <- paste0(LiCOR_3$SurveyDay, "_", LiCOR_3$Obs) # same thing in LiCOR datafile
#length(unique(LiCOR_3$LiCOR_ID)) # 233 but some duds/extras
#length(unique(crosswalk3$LiCOR_ID)) # 224 recorded measurements in datasheet
#sum(unique(crosswalk3$LiCOR_ID) %in% unique(LiCOR_3$LiCOR_ID)) # 224 overlapping 

LiCOR_3 <- LiCOR_3 %>% 
  filter(LiCOR_ID != "NA_NA") # takes it from 983 to 232; still 3-4 measurements per leaf

# get plant ID names, treatment codes, into LiCOR measurement df
df3 <- left_join(LiCOR_3, crosswalk3, by = join_by(LiCOR_ID)) 
df3 <- df3 %>% # eliminate negative values, add Spp
  filter(Photo > 0 & Ci > 0) %>% 
  filter(ID != "NA") %>% 
  mutate(Spp = str_sub(ID, - 3, - 3)) 

df3$HHMMSS <- parse_date_time(df3$HHMMSS, orders = c("HMS"), tz = "America/Los_Angeles") # get timestamp as datetime
# now has each LiCOR measurement associated with plant ID. sets of 3 and 4. CO2 setting goes 400-600-800(-400)

# do the same steps for survey #4
LiCOR_4 <- read.csv("RawData/Quail_LiCOR4_data.csv")
crosswalk4 <- read.csv("RawData/Survey4.csv")

# Fix data entry error: 15V1a has 63, 63, 65
#crosswalk4[241:243,]
crosswalk4[242,3] <- 64
crosswalk4 <- crosswalk4[-c(1:2),c(1:6)]
crosswalk4 <- crosswalk4 %>% 
  mutate(Plot = if_else(nchar(ID) == 4, substr(ID,1,1), substr(ID,1,2))) %>% # add Plot 
  left_join(lookup, by = "Plot")

crosswalk4$SurveyDay <- 
  case_match( crosswalk4$Date,
              ("30-Sep") ~ 1,
              c("10/1/23", "1-Oct") ~ 2,
              c("10/2/23", "2-Oct") ~ 3)

crosswalk4$LiCOR_ID <- paste0(crosswalk4$SurveyDay, "_", crosswalk4$Log)
LiCOR_4$LiCOR_ID <- paste0(LiCOR_4$SurveyDay, "_", LiCOR_4$Obs)
#length(unique(LiCOR_4$LiCOR_ID)) # 187
#length(unique(crosswalk4$LiCOR_ID)) # 188
#sum(crosswalk4$LiCOR_ID %in% LiCOR_4$LiCOR_ID) # 187 overlapping

# get plant ID names, treatment codes, into LiCOR measurement df
df4 <- left_join(LiCOR_4, crosswalk4, by = join_by(LiCOR_ID))
# now has each LiCOR measurement associated with plant ID. sets of 3 and 4. CO2 setting goes 400-600-800(-400)
df4 <- df4 %>% # eliminate negative values, add Spp
  filter(Photo > 0 & Ci > 0) %>% 
  filter(ID != "NA") %>% 
  mutate(Spp = str_sub(ID, - 3, - 3)) 

df4$HHMMSS <- parse_date_time(df4$HHMMSS, orders = c("HMS"), tz = "America/Los_Angeles") # get timestamp as datetime
# ready!

# combine dataframes, explore relationships between variables
df3_nonoverlapping <- df3 %>% 
  filter(!ID %in% df4$ID)

df_all <- rbind(df4, df3_nonoverlapping)


# read in Survey 1 and 2
LiCOR_1 <- read.csv("RawData/LiCOR_6.15.23.csv")
LiCOR_2 <- read.csv("RawData/LiCOR_7.28.23.csv")
# survey 1 needs SWC; get from plot averages in survey 2

survey2_SWC <- LiCOR_2 %>% 
  group_by(Plot) %>% 
  summarise(SWC = mean(SWC)) %>% 
  filter(Plot %in% c(1:16))

LiCOR_1 <- LiCOR_1 %>% # add SWC values from survey 2
  mutate(Plot = as.character(Plot)) %>% 
  left_join(survey2_SWC, by = "Plot") %>% 
  mutate(Spp = str_sub(ID, - 3, - 3)) %>% 
  mutate(CO2R = signif(CO2S, 1))

LiCOR_2 <- LiCOR_2 %>% # add Spp
  mutate(Spp = str_sub(ID, - 3, - 3)) %>% 
  mutate(CO2R = signif(CO2S, 1))

LiCOR_1$HHMMSS<- parse_date_time(LiCOR_1$HHMMSS, orders = c("HMS"), tz = "America/Los_Angeles")
LiCOR_2$HHMMSS <- parse_date_time(LiCOR_2$HHMMSS, orders = c("HMS"), tz = "America/Los_Angeles")


LiCOR_all <- df_all %>% 
  select(ID, Plot, Date, HHMMSS, Ci, Photo, Cond, Tleaf, PARi, VpdL, CO2R, RH_R, RH_S, SWC, Spp) %>%
  left_join(lookup, by = "Plot") %>% 
  mutate(Tmt = as.factor(Tmt)) %>% 
  filter(!is.na(Tmt)) %>% 
  filter(Ci > 0, Photo > 0)

LiCOR_IDs <- unique(LiCOR_all$ID) # get plant codes in LiCOR sample
LiCOR_Ci <- vector(length = length(LiCOR_IDs)) # initialize results vector

# make a function where you get from ID to CO2S based on plot_CO2.
seedling_CO2 <- function(ID){
  plot <- if_else(nchar(ID) == 4, substr(ID,1,1), substr(ID,1,2))
  plotCO2 <- select(filter(plot_CO2, Plot==plot),CO2)
  names(plotCO2) <- "CO2R"
  return(plotCO2)
}

# how well does CO2S predict Ci for existing data?

for(i in 1:length(LiCOR_IDs)){ # calculate linear interpolation for each ID's datapoints, at plot-level CO2S
  LiCOR_Ci[i] <- predict(lm(Ci ~ CO2R, data = LiCOR_all %>%
                              filter(ID == LiCOR_IDs[i])), newdata=data.frame(CO2S = seedling_CO2(ID = LiCOR_IDs[i]))) }

# see how well this does; extract R^2 values
r.sq <- vector(length = length(LiCOR_IDs))
nobs <- vector(length = length(LiCOR_IDs))
for(i in 1:length(LiCOR_IDs)){ # calculate linear interpolation for each ID's datapoints, at plot-level CO2S
  r.sq[i] <- summary(lm(Ci ~ CO2R, data = LiCOR_all %>%
                          filter(ID == LiCOR_IDs[i])))$r.squared
  nobs[i] <- nobs(lm(Ci ~ CO2R, data = LiCOR_all %>%
                       filter(ID == LiCOR_IDs[i])))
}
r.sq_df <- data.frame(cbind(r.sq, nobs)) %>% 
  filter(nobs > 2)
mean(r.sq_df$r.sq) # 0.9215826
r.sq_df$r.sq

LiCOR_Ci_df <- data.frame(cbind(LiCOR_IDs, LiCOR_Ci)) # pairs ID/code with predicted Ci at Ca
colnames(LiCOR_Ci_df) <- c("ID","Ci")
LiCOR_Ci_df$Ci <- as.numeric(LiCOR_Ci_df$Ci)
LiCOR_Ci_df <- LiCOR_Ci_df %>% 
  filter(Ci > 150 & Ci < 800)

# now use those Ci values to get the point along the ACi curve
LiCOR_IDs2 <- unique(LiCOR_Ci_df$ID) # get plant codes in usable subset
LiCOR_Anet <- vector(length = length(LiCOR_IDs2)) # initialize results vector
for(i in 1:length(LiCOR_IDs2)){ # calculate linear interpolation for each ID's datapoints
  LiCOR_Anet[i] <- predict(lm(Photo ~ Ci, data = LiCOR_all %>%
                                filter(ID == LiCOR_IDs2[i])), newdata=data.frame(Ci = LiCOR_Ci_df[i,2])) }
LiCOR_Anet_df <- data.frame(cbind(LiCOR_IDs2, LiCOR_Anet)) # pairs ID/code with predicted Ci at Ca
colnames(LiCOR_Anet_df) <- c("ID","Anet")
LiCOR_Anet_df$Anet <- as.numeric(LiCOR_Anet_df$Anet)

Anet_r.sq <- vector(length = length(LiCOR_IDs))
Anet_nobs <- vector(length = length(LiCOR_IDs))
for(i in 1:length(LiCOR_IDs)){ # calculate linear interpolation for each ID's datapoints, at plot-level CO2S
  Anet_r.sq[i] <- summary(lm(Photo ~ Ci, data = LiCOR_all %>%
                               filter(ID == LiCOR_IDs[i])))$r.squared
  Anet_nobs[i] <- nobs(lm(Photo ~ Ci, data = LiCOR_all %>%
                            filter(ID == LiCOR_IDs[i])))
}
Anet_r.sq_df <- data.frame(cbind(Anet_r.sq, Anet_nobs)) %>% 
  filter(Anet_nobs > 2)
mean(Anet_r.sq_df$Anet_r.sq) # 0.8093054

# do the same for gs
LiCOR_gs <- vector(length = length(LiCOR_IDs2))# initialize results vector

for(i in 1:length(LiCOR_IDs2)){ # calculate linear interpolation *of Cond* for each ID's datapoints, at Ca
  LiCOR_gs[i] <- predict(lm(Cond ~ Ci, data = LiCOR_all %>%
                              filter(ID == LiCOR_IDs2[i])), newdata=data.frame(Ci = LiCOR_Ci_df[i,2])) }

LiCOR_gs_df <- data.frame(cbind(LiCOR_IDs2, LiCOR_gs)) # pairs ID/code with predicted gs 
colnames(LiCOR_gs_df) <- c("ID","gs")
LiCOR_gs_df$gs <- as.numeric(LiCOR_gs_df$gs)

gs_r.sq <- vector(length = length(LiCOR_IDs))
gs_nobs <- vector(length = length(LiCOR_IDs))
for(i in 1:length(LiCOR_IDs)){ # calculate linear interpolation for each ID's datapoints, at plot-level CO2S
  gs_r.sq[i] <- summary(lm(Cond ~ Ci, data = LiCOR_all %>%
                             filter(ID == LiCOR_IDs[i])))$r.squared
  gs_nobs[i] <- nobs(lm(Cond ~ Ci, data = LiCOR_all %>%
                          filter(ID == LiCOR_IDs[i])))
}
gs_r.sq_df <- data.frame(cbind(gs_r.sq, gs_nobs)) %>% 
  filter(gs_nobs > 2)
mean(gs_r.sq_df$gs_r.sq) # 0.6110546

newLiCOR <- left_join(LiCOR_Ci_df, LiCOR_gs_df, by="ID") %>% left_join(LiCOR_Anet_df, by="ID")
LiCOR_new <- left_join(LiCOR_all, newLiCOR, by = "ID") %>% 
  group_by(ID) %>% 
  mutate(n=n()) %>% 
  filter(n>2) %>% 
  summarise(across(c(HHMMSS:RH_S,Ci.y,gs,Anet), ~ mean(.x, na.rm = TRUE))) %>% 
  ungroup() %>% 
  mutate(Plot = if_else(nchar(ID) == 4, substr(ID,1,1), substr(ID,1,2)), Spp = str_sub(ID, - 3, - 3))  %>%
  left_join(lookup, by = "Plot") # add Tmt codes

# Step 3: Combine LiCOR, biomass data, herbivory data
library(scales)

# per-plot meanSWC over all sampling dates
plot_SWC <- rbind(df3, df4) %>% 
  select(ID, Plot, Date, HHMMSS, Ci, Photo, Cond, Tleaf, PARi, VpdL, CO2R, RH_R, RH_S, SWC, Spp) %>% 
  rbind(select(LiCOR_1, ID, Plot, Date, HHMMSS, Ci, Photo, Cond, Tleaf, PARi, VpdL, CO2R, RH_R, RH_S, SWC, Spp)) %>% 
  rbind(select(LiCOR_2, ID, Plot, Date, HHMMSS, Ci, Photo, Cond, Tleaf, PARi, VpdL, CO2R, RH_R, RH_S, SWC, Spp)) %>% 
  left_join(lookup, by = "Plot") %>% 
  mutate(Tmt = as.factor(Tmt)) %>% 
  filter(!is.na(Tmt)) %>% 
  filter(Ci > 0, Photo > 0) %>% 
  select(Plot, SWC, Date) %>% 
  group_by(Plot, Date) %>% 
  summarise(meanSWC=mean(SWC)) %>% 
  group_by(Plot) %>% 
  summarise(meanSWC=mean(meanSWC)) %>% 
  as.data.frame()

# update LiCOR data with derived variable WUE, CO2Tmt, per-plot CO2
LiCOR_df <- LiCOR_new %>% 
  mutate(WUE = Anet/gs) %>% 
  mutate(CO2Tmt = substring(Tmt,1,1), H2OTmt = substring(Tmt,2,2)) %>% 
  mutate(Code = if_else(nchar(ID) == 4,substr(ID,1,3),substr(ID,1,4))) %>% 
  left_join(plot_CO2, by = "Plot")

# herbivory data:
herbivory <- read.csv("/Users/paigekouba/Documents/UC_Davis/2021_Winter/Quals/Proposal/Chapter 1/TinyFACE/GitHub/QuailFACE/RawData/OakFACE Herbivory - Sheet1.csv")
herbivory$Date <- mdy(herbivory$Date)
firstherb <- herbivory %>% # most conservative: first date any herbivory was observed
  group_by(Code) %>% 
  summarise(firstherb = min(Date)) # next, removing some that look undamaged per growth curves

# add in these:
new_ones <- data.frame(c("11V5","9V3", "9V5","7L6","8L6"),c("8/25/23","8/25/23","8/25/23", "6/13/23", "8/25/23"))
colnames(new_ones) <- c("Code", "firstherb")
new_ones <- new_ones %>% 
  mutate(firstherb = mdy(firstherb))
firstherb <- rbind(firstherb, new_ones) # 55 seedlings had any herbivory

firstfullherb <- herbivory %>% # first date full stem herbivory was observed
  group_by(Code) %>% 
  filter(full_herb == "x") %>% 
  summarise(firstherb = min(Date))  
firstfullherb <- rbind(firstfullherb, new_ones) # 46 seedlings had stem herbivory

# biomass data:
biomass_raw <- read.csv("/Users/paigekouba/Documents/UC_Davis/2021_Winter/Quals/Proposal/Chapter 1/TinyFACE/GitHub/QuailFACE/RawData/Biomass_final.csv")
rootmass_raw <- read.csv("/Users/paigekouba/Documents/UC_Davis/2021_Winter/Quals/Proposal/Chapter 1/TinyFACE/GitHub/QuailFACE/RawData/Root_mass.csv")
rootmass <- rootmass_raw %>% 
 # filter(!is.na(rootmass_g)) %>% 
  filter(Code!="4V3c") %>% # remove "4V3c" to avoid duplicate code
  filter(Code != "16V1a") %>% # thinned but grew back
  mutate(Code = if_else(nchar(Code)==4,substr(Code,1,3),substr(Code,1,4))) # shortcode


# add inventory data for heights
inventory_raw <- read.csv("/Users/paigekouba/Documents/UC_Davis/2021_Winter/Quals/Proposal/Chapter 1/TinyFACE/GitHub/QuailFACE/RawData/Inventory8.csv")

# fix all the dates to the *earliest* day if multi-day inventory; also, fill in blanks
inventory_raw$Inv..1.Date <- "8/2/22"
inventory_raw$Inv..2.Date <- "10/21/22"
inventory_raw$Inv..3.Date <- "2/24/23"
inventory_raw$Inv..4.Date <- "4/21/23"
inventory_raw$Inv..5.Date <- "6/14/23"
inventory_raw$Inv..6.Date <- "8/26/23"
inventory_raw$Inv..7.Date <- "9/30/23"
inventory_raw$Inv..8.Date <- "11/20/23"

# fixing data entry errors
inventory_raw[inventory_raw$Code == "10V5b",25] <- 46 # 26; assume 46 based on previous and subsequent entries
inventory_raw[inventory_raw$Code == "13V5b",25] <- mean(c(43,39))# NA; assign average of adjacent hts
inventory_raw[inventory_raw$Code == "12V4b",25] <- 61.5 # NA; assign average of adjacent hts
inventory_raw[inventory_raw$Code == "4V1b",28] <- 50 # 15; assume "50" misheard
inventory_raw[inventory_raw$Code == "4V1b",49] <- mean(65,73) # NA; assign average of adjacent hts
inventory_raw[inventory_raw$Code == "9V2b",43] <- 116 # 65; assume 116, same as adjacent hts
inventory_raw[inventory_raw$Code == "9V5a",43] <- mean(c(101,84)) # NA; assign mean of adjacent hts
inventory_raw[inventory_raw$Code == "14L5a", 55] <- 450 # 45, assume entry error --> 450
inventory_raw[which(inventory_raw$Leaf.Ct..5 == 74),45] <- 7 # 74, assume entry error --> 7

# remove "4V3c" to avoid duplicate code
inventory_raw <- inventory_raw %>% 
  filter(Code!="4V3c") # now 384 rows
# remove rows for seedlings that were missing/never grew in
inventory_thinned <- inventory_raw %>% # group_by shortcode and get max as summary
  mutate(shortcode = if_else(nchar(Code)==4,substr(Code,1,3),substr(Code,1,4))) %>% 
  group_by(shortcode) %>% 
  summarise_if(is.numeric, ~ max(.x, na.rm = TRUE)) # %>% # this results in 192 rows with the max of each shortcode
# update column header for consistency
colnames(inventory_thinned)[colnames(inventory_thinned)=="shortcode"] <- "Code"

# now I need to reassociate the date values from inventory_thinned
inventory_thinned$Inv..1.Date <- c(rep("8/2/22", nrow(inventory_thinned)))
inventory_thinned$Inv..2.Date <- c(rep("10/21/22", nrow(inventory_thinned)))
inventory_thinned$Inv..3.Date <- c(rep("2/24/23", nrow(inventory_thinned)))
inventory_thinned$Inv..4.Date <- c(rep("4/21/23", nrow(inventory_thinned)))
inventory_thinned$Inv..5.Date <- c(rep("6/14/23", nrow(inventory_thinned)))
inventory_thinned$Inv..6.Date <- c(rep("8/26/23", nrow(inventory_thinned)))
inventory_thinned$Inv..7.Date <- c(rep("9/30/23", nrow(inventory_thinned)))
inventory_thinned$Inv..8.Date <- c(rep("11/20/23", nrow(inventory_thinned)))

# next, making inventory data long (many rows, fewer columns, instead of each date a new col)
# # start with inventory date
inv_long1 <- inventory_thinned %>% 
  dplyr::select(Code, Inv..1.Date, Inv..2.Date, Inv..3.Date, Inv..4.Date, Inv..5.Date, Inv..6.Date, Inv..7.Date, Inv..8.Date)

inv_long <- inv_long1 %>%
  pivot_longer(!Code, names_to = "Date") %>%
  mutate(value = mdy(value))

# add in the reorganized ht, condition, and dia values
inv_ht <- inventory_thinned %>% 
  dplyr::select(Code, paste0("Ht.mm..",c(1:8))) %>% 
  pivot_longer(!Code, names_to = "key", values_to = "ht_mm")

inv_cond <- inventory_thinned %>% 
  dplyr::select(Code, paste0("Cond..",c(1:8))) %>% 
  pivot_longer(!Code, names_to = "key", values_to = "cond")

inv_dia <- inventory_thinned %>% 
  dplyr::select(Code, paste0("Dia.mm..",c(1:8))) %>% 
  pivot_longer(!Code, names_to = "key", values_to = "dia")

inv_all <- cbind(inv_long, inv_ht[,3], inv_dia[,3], inv_cond[,3]) # combine code, date, ht, dia, cond

inv_all[which.max(inv_all$dia),] # some outliers need adjusting
#    Code        Date      value ht_mm dia cond
# 776  2L1 Inv..8.Date 2023-11-20   156 389    5
inv_all[which(inv_all$dia == 389),5] <- 3.89 # it's a decimal error
inv_all[which.max(inv_all$dia),]
#      Code        Date      value ht_mm  dia cond
# 934  3V3 Inv..6.Date 2023-08-26    96 54.9    5
inv_all[which(inv_all$dia == 54.9),5] <- 2.69 # mean of Dia.mm..5 and Dia.mm..7
inv_all[which.max(inv_all$dia),]
#     Code        Date      value ht_mm  dia cond
# 1470  9L4 Inv..6.Date 2023-08-26  4.24 37.8    5
inv_all[which(inv_all$dia == 37.8),4] <- 100.5 # mean of Ht.mm..5 and Ht.mm..7
inv_all[which(inv_all$dia == 37.8),5] <- 4.24

# now add Plot and Spp, replace -Inf with NAs
inv_all <- inv_all %>% 
  mutate(Plot = if_else(nchar(Code)==3,substr(Code,1,1),substr(Code,1,2))) %>% 
  mutate(Spp = substr(Code, nchar(Code)-1,nchar(Code)-1)) %>% 
  left_join(lookup, by = "Plot") %>% 
  mutate(across(c(ht_mm, dia, cond), na_if, -Inf))

# find Codes for seedlings with any herbivory and drop after the date of first herbivory
inv_all_nh0 <- right_join(firstherb, inv_all, by = "Code") 
inv_all_nfh0 <- right_join(firstfullherb, inv_all, by = "Code") 

inv_all_nh0[is.na(inv_all_nh0$firstherb),2] <- ymd("9999-09-09") # if not listed in firstherb (ie NA), assign a dummy date >> any in the surveys
inv_all_nfh0[is.na(inv_all_nfh0$firstherb),2] <- ymd("9999-09-09")

inv_all_nh <- inv_all_nh0 %>% 
  filter(firstherb > value)

inv_all_nfh <- inv_all_nfh0 %>% 
  filter(firstherb > value)

# get counts per sampling date, species and tmt 
inv_nequals <- inv_all_nfh %>%
  filter(!is.na(dia)) %>% 
  group_by(Spp, Tmt, value) %>%
   tally()

inv_all_nfh %>% # for seedlings without *full* herbivory, how did height change over the course of the study?
  group_by(Spp, Tmt, value) %>% 
  # summarise(mean_ht = mean(ht_mm, na.rm = TRUE),sd_ht = sd(ht_mm, na.rm = TRUE)) %>% 
  summarise(mean_ht = mean(ht_mm, na.rm = TRUE),se_ht = sd(ht_mm, na.rm = TRUE)/sqrt(n())) %>% 
  # summarise(mean_dia = mean(dia, na.rm = TRUE),sd_dia = sd(dia, na.rm = TRUE)) %>% 
  # summarise(mean_cond = mean(cond, na.rm = TRUE),sd_cond = sd(cond, na.rm = TRUE)) %>% 
  ggplot(aes(x=value, y = mean_ht, group = Tmt)) + 
  geom_line(aes(color = Tmt), position = position_dodge(20, preserve = "total"), linewidth = 1.2) +
  geom_pointrange(aes(ymin = mean_ht - se_ht, ymax = mean_ht + se_ht, color = Tmt, shape = Tmt), position = position_dodge(20, preserve = "total"), size = 1.5, linewidth=1, alpha = 0.75) + 
  # geom_pointrange(aes(ymin = mean_dia - sd_dia, ymax = mean_dia + sd_dia, color = Tmt, shape = Tmt), position = position_dodge(20, preserve = "total"), size = 1.5, alpha = 0.75) + 
  # geom_pointrange(aes(ymin = mean_cond - sd_cond, ymax = mean_cond + sd_cond, color = Tmt, shape = Tmt), position = position_dodge(20, preserve = "total"), size = 1.5, alpha = 0.75) + 
  scale_color_manual(values = c("#e8665d", "#828cfa", "#850a01", "#010c85")) +
  scale_shape_manual(values = c(16,16,17,17)) +
  geom_text(data = inv_nequals, aes(x = value, y = as.numeric(as.factor(Tmt))*12-50, color = Tmt, label = paste0("N=",n))) +
  facet_grid(~ Spp) + theme_classic(base_size = 19)

# view the growth curves for all seedlings with STEM herbivory
inv_all[which(inv_all$Code %in% firstfullherb$Code),] %>% # (add ! after which( to get all without herb)
  na.omit() %>% 
  group_by(Spp, Tmt, value, Code) %>% 
  ggplot(aes(x=value, y=ht_mm,  group = Code, color = Tmt, shape = Tmt, size = 1.5, alpha = 0.75)) +
  geom_line(aes(x=value, y=ht_mm,  group = Code), position = position_dodge(20, preserve = "total"), linewidth = 1.2, na.rm = TRUE) +
  geom_point(position = position_dodge(20, preserve = "total")) +
  scale_color_manual(values = c("#e8665d", "#828cfa", "#850a01", "#010c85")) +
  scale_shape_manual(values = c(16,16,17,17)) +
  facet_grid(rows=vars(Spp), scales = "free")

#inv_all %>% # how did dia change over the course of the study?
inv_all[which(!(inv_all$Code %in% firstfullherb$Code)),] %>% # how did dia change over the course of the study for non-chomped seedlings??
  group_by(Spp, Tmt, value) %>% 
  summarise(mean_dia = mean(dia, na.rm = TRUE),se_dia = sd(dia, na.rm = TRUE)/sqrt(n())) %>% 
  ggplot(aes(x=value, y = mean_dia, group = Tmt)) + 
  geom_line(aes(color = Tmt), position = position_dodge(20, preserve = "total"), linewidth = 1.2) +
  geom_pointrange(aes(ymin = mean_dia - se_dia, ymax = mean_dia + se_dia, color = Tmt, shape = Tmt), position = position_dodge(20, preserve = "total"), size = 1.5, alpha = 0.75) + 
  scale_color_manual(values = c("#e8665d", "#828cfa", "#850a01", "#010c85")) +
  scale_shape_manual(values = c(16,16,17,17)) +
  facet_grid(~ Spp) + theme_classic(base_size = 19)

# # mortality data
# mort_df <- inventory_raw %>% 
#   filter(Code!="4V3c") %>%
#   filter(Code != "16V1a") %>%
#   filter(G. == "Y" & Thinned. != "8/26") %>%  # want ones that grew in but were not thinned
#   select(Code, Spp, Plot, c(paste0("Cond..",1:8))) %>% 
#   mutate(shortcode = if_else(nchar(Code)==4,substr(Code,1,3),substr(Code,1,4)))  %>% 
#   mutate(Plot = as.character(Plot)) %>% 
#   rowwise() %>% 
#   mutate(condmin = min(Cond..1, Cond..2, Cond..3, Cond..4, Cond..5, Cond..6, Cond..7, Cond..8, na.rm = T)) %>% 
#   mutate(mortality = as.numeric(condmin == 1 & Cond..8 %in% c(1, NA))) %>%
#   left_join(lookup, by = "Plot") %>% 
#   left_join(plot_CO2, by = "Plot") %>% 
#   left_join(plot_SWC, by = "Plot")

mort_df <- inventory_raw %>% 
  filter(Code!="4V3c") %>%
  filter(Code != "16V1a") %>%
  filter(G. == "Y" & Thinned. != "8/26") %>%  # want ones that grew in but were not thinned
  select(Code, Spp, Plot, c(paste0("Cond..",1:8)),c(paste0("Ht.mm..",1:8)), c(paste0("Dia.mm..",1:8))) %>% 
  mutate(max_ht = 
           pmax(Ht.mm..1, Ht.mm..2, Ht.mm..3, Ht.mm..4, Ht.mm..5, Ht.mm..6, Ht.mm..7, Ht.mm..8, na.rm=T)) %>%   
  mutate(max_dia = pmax(Dia.mm..1, Dia.mm..2, Dia.mm..3, Dia.mm..4, Dia.mm..5, Dia.mm..6, Dia.mm..7, Dia.mm..8, na.rm=T)) %>% 
  mutate(condmin = pmin(Cond..1, Cond..2, Cond..3, Cond..4, Cond..5, Cond..6, Cond..7, Cond..8, na.rm = T)) %>% 
  mutate(mortality = as.numeric(condmin == 1 & Cond..8 %in% c(1, NA))) %>%
  select(Code, Spp, Plot, max_ht, max_dia, mortality) %>% 
  left_join(select(rootmass_raw, Code, rootmass_g)) %>% 
  mutate(shortcode = if_else(nchar(Code)==4,substr(Code,1,3),substr(Code,1,4)))  %>% 
  mutate(Plot = as.character(Plot)) %>% 
  left_join(lookup, by = "Plot") %>% 
  left_join(plot_CO2, by = "Plot") %>% 
  left_join(plot_SWC, by = "Plot")

chisq.test(matrix(
  c(sum(mort_df$mortality == 1 & (mort_df$Tmt == "AD" | mort_df$Tmt == "AW")),  # died, aCO2
    sum(mort_df$mortality == 1 & (mort_df$Tmt == "ED" | mort_df$Tmt == "EW")),  # died, eCO2
    sum(mort_df$mortality == 0 & (mort_df$Tmt == "AD" | mort_df$Tmt == "AW")),  # live, aCO2
    sum(mort_df$mortality == 0 & (mort_df$Tmt == "ED" | mort_df$Tmt == "EW"))), # live, eCO2 
  byrow = TRUE, nrow = 2))
# X-squared = 3.6779, df = 1, p-value = 0.05514
# elevated CO2 plants had significantly lower mortality
chisq.test(matrix(
  c(sum(mort_df$mortality == 1 & (mort_df$Tmt == "AD" | mort_df$Tmt == "ED")),  # died, dry
    sum(mort_df$mortality == 1 & (mort_df$Tmt == "AW" | mort_df$Tmt == "EW")),  # died, wet
    sum(mort_df$mortality == 0 & (mort_df$Tmt == "AD" | mort_df$Tmt == "ED")),  # live, dry
    sum(mort_df$mortality == 0 & (mort_df$Tmt == "AW" | mort_df$Tmt == "EW"))), # live, wet  
  byrow = TRUE, nrow = 2))
# watering treatment had no effect on mortality

# # all combined
# final_df <- biomass2 %>% 
#   dplyr::select(Plot, Spp, Code, StemWet_g, totmass, Ht.mm..5, Ht.mm..8, CO2, meanSWC) %>% 
#   left_join(LiCOR_df[,c("Code","Anet","gs", "WUE", "HHMMSS")], by = "Code") %>% 
#   group_by(Plot, Spp) %>% 
#   mutate(n = n()) %>% 
#   left_join(lookup, by = "Plot") %>% 
#   mutate(H2OTmt = substr(Tmt,2,2), CO2Tmt = substr(Tmt,1,1)) %>% 
#   ungroup() %>% 
#   mutate(time_scaled = rescale(HHMMSS)) 
# 
# final_df_mort <- biomass_mort2 %>% 
#   dplyr::select(Plot, Spp, Code, StemWet_g, totmass, Ht.mm..5, Ht.mm..8, CO2, meanSWC, mortality) %>% 
#   # mutate(Code = if_else(nchar(Code)==4,substr(Code,1,3),substr(Code,1,4))) %>% 
#   left_join(LiCOR_df[,c("Code","Anet","gs", "WUE", "HHMMSS")], by = "Code") %>% 
#   group_by(Plot, Spp) %>% 
#   mutate(n = n()) %>% 
#   left_join(lookup, by = "Plot") %>% 
#   mutate(H2OTmt = substr(Tmt,2,2), CO2Tmt = substr(Tmt,1,1)) %>% 
#   ungroup() 

# final df with biomass(+rootmass), final ht, final dia, mort, resprout, Anet gs WUE
# to "biomass", add inventory (Ht, Dia) and get max; derive totmass; add mortality (0/1) and resprout (0/1)

# biomass2 <- biomass %>% # biomass data with full dataset and extra inventory columns
#   left_join(inventory_thinned[,c("Code","Ht.mm..1", "Ht.mm..2", "Ht.mm..3", "Ht.mm..4", "Ht.mm..5", "Ht.mm..6", "Ht.mm..7", "Ht.mm..8")]) %>% 
#   mutate(across(c(StemWet_g, LeafWet_g, LeafDry_g, rootmass_g, Ht.mm..1, Ht.mm..2, Ht.mm..3, Ht.mm..4, Ht.mm..5, Ht.mm..6, Ht.mm..7, Ht.mm..8), na_if, -Inf))  %>% 
#   mutate(H2OTmt = substr(Tmt,2,2)) %>% 
#   mutate(totmass = rootmass_g + StemWet_g + LeafWet_g) %>% 
#   left_join(plot_CO2, by = "Plot") %>% 
#   left_join(plot_SWC, by = "Plot") 

biomass_raw %>% 
  filter(Code %in% unlist(c(mort_df[mort_df$mortality==1,"Code"]))) %>% nrow()

df_final <- biomass_raw %>% 
  dplyr::select(Code, StemWet_g, LeafWet_g, LeafDry_g) %>% 
#  filter(Code %in% unlist(c(mort_df$Code))) %>% 
  mutate(longcode = Code) %>% 
  mutate(Code = if_else(nchar(Code)==4,substr(Code,1,3),substr(Code,1,4))) %>%  # removes the a or b from the end of the seedling ID ("Code")
  # group_by(Code, longcode) %>% 
  # summarise_if(is.numeric, ~ max(.x, na.rm = TRUE)) %>% # group by shortened Code and collapse values in case of a/b confusion
  # ungroup() %>% head() # need to summarize by code but keep longcode!
  filter((!(is.na(StemWet_g)) | longcode %in% mort_df$Code)) %>% 
  left_join(dplyr::select(rootmass_raw, Code, Plot, Spp, rootmass_g), join_by(longcode == Code))  %>% # SeedMass.g., Cond..7, 
 # mutate(across(c(StemWet_g, LeafWet_g, LeafDry_g), na_if, -Inf)) %>% 
  left_join(inventory_thinned[,c("Code","Ht.mm..1", "Ht.mm..2", "Ht.mm..3", "Ht.mm..4", "Ht.mm..5", "Ht.mm..6", "Ht.mm..7", "Ht.mm..8", 
                                 "Dia.mm..1", "Dia.mm..2", "Dia.mm..3", "Dia.mm..4", "Dia.mm..5", "Dia.mm..6", "Dia.mm..7", "Dia.mm..8")]) %>% 
  mutate(across(c(StemWet_g, LeafWet_g, LeafDry_g, rootmass_g, Ht.mm..1, Ht.mm..2, Ht.mm..3, Ht.mm..4, Ht.mm..5, Ht.mm..6, Ht.mm..7, Ht.mm..8, 
                  Dia.mm..1, Dia.mm..2, Dia.mm..3, Dia.mm..4, Dia.mm..5, Dia.mm..6, Dia.mm..7, Dia.mm..8), na_if, -Inf))  %>% 
  mutate(max_ht = 
           pmax(Ht.mm..1, Ht.mm..2, Ht.mm..3, Ht.mm..4, Ht.mm..5, Ht.mm..6, Ht.mm..7, Ht.mm..8, na.rm=T)) %>%   
  mutate(max_dia = pmax(Dia.mm..1, Dia.mm..2, Dia.mm..3, Dia.mm..4, Dia.mm..5, Dia.mm..6, Dia.mm..7, Dia.mm..8, na.rm=T)) %>%
  select(Plot, Code, Spp, StemWet_g, LeafWet_g, LeafDry_g, longcode, max_ht, max_dia, rootmass_g) %>% 
  left_join(select(mort_df, Code, max_ht, max_dia, mortality, rootmass_g, shortcode), join_by(longcode == Code, max_ht, max_dia, rootmass_g)) %>%
  mutate(resprout = as.numeric(Code %in% resprout))

lookup <- data.frame(as.character(c(1:16)), c("AW","ED","AD","EW",
                                              "AW","ED","EW","AD",
                                              "ED","AW","EW","AD",
                                              "ED","AW","EW","AD"))
names(lookup) <- c("Plot","Tmt")

# biomass <- merge(lookup, biomass, by = 'Plot')
df_final <- merge(lookup, df_final, by = 'Plot')

df_final <- df_final %>% 
  mutate(H2OTmt = substr(Tmt,2,2)) %>% 
  mutate(totmass = rootmass_g + StemWet_g + LeafWet_g) %>% 
  left_join(plot_CO2, by = "Plot") %>% 
  left_join(plot_SWC, by = "Plot") 
  
df_final[which.max(df_final$max_dia),] # some outliers need adjusting
#    Code        Date      value ht_mm dia cond
# 776  2L1 Inv..8.Date 2023-11-20   156 389    5
df_final[which(df_final$max_dia == 389),"max_dia"] <- 3.89 # it's a decimal error
df_final[which.max(df_final$max_dia),]
#      Code        Date      value ht_mm  dia cond
# 934  3V3 Inv..6.Date 2023-08-26    96 54.9    5
df_final[which(df_final$max_dia == 54.9),"max_dia"] <- 2.69 # mean of Dia.mm..5 and Dia.mm..7
df_final[which.max(df_final$max_dia),]
#     Code        Date      value ht_mm  dia cond
# 1470  9L4 Inv..6.Date 2023-08-26  4.24 37.8    5
df_final[which(df_final$max_dia == 37.8),"max_dia"] <- 4.24
  
df_final <- df_final %>% 
  left_join(LiCOR_df[,c("Code","Anet","gs", "WUE", "HHMMSS")], by = "Code") %>% 
  mutate(time_scaled = rescale(HHMMSS)) 

names(df_final)
# [1] "Plot"        "Tmt"         "Code"        "Spp"         "StemWet_g"   "LeafWet_g"   "LeafDry_g"   "longcode"   
# [9] "max_ht"      "max_dia"     "rootmass_g"  "mortality"   "shortcode"   "resprout"    "H2OTmt"      "totmass"    
# [17] "mDeltaTest"  "CO2"         "meanSWC"     "Anet"        "gs"          "WUE"         "HHMMSS"      "time_scaled"




# df_final <- biomass %>% # biomass data with full dataset and extra inventory columns
#   left_join(inventory_thinned[,c("Code","Ht.mm..1", "Ht.mm..2", "Ht.mm..3", "Ht.mm..4", "Ht.mm..5", "Ht.mm..6", "Ht.mm..7", "Ht.mm..8", 
#                                  "Dia.mm..1", "Dia.mm..2", "Dia.mm..3", "Dia.mm..4", "Dia.mm..5", "Dia.mm..6", "Dia.mm..7", "Dia.mm..8")]) %>% 
#   mutate(across(c(StemWet_g, LeafWet_g, LeafDry_g, rootmass_g, Ht.mm..1, Ht.mm..2, Ht.mm..3, Ht.mm..4, Ht.mm..5, Ht.mm..6, Ht.mm..7, Ht.mm..8, 
#                   Dia.mm..1, Dia.mm..2, Dia.mm..3, Dia.mm..4, Dia.mm..5, Dia.mm..6, Dia.mm..7, Dia.mm..8), na_if, -Inf))  %>% 
#   mutate(max_ht = 
#    pmax(Ht.mm..1, Ht.mm..2, Ht.mm..3, Ht.mm..4, Ht.mm..5, Ht.mm..6, Ht.mm..7, Ht.mm..8, na.rm=T)) %>%   
#   mutate(max_dia = pmax(Dia.mm..1, Dia.mm..2, Dia.mm..3, Dia.mm..4, Dia.mm..5, Dia.mm..6, Dia.mm..7, Dia.mm..8, na.rm=T)) %>%
#   
#   mutate(H2OTmt = substr(Tmt,2,2)) %>% 
#   mutate(totmass = rootmass_g + StemWet_g + LeafWet_g) %>% 
#   left_join(plot_CO2, by = "Plot") %>% 
#   left_join(plot_SWC, by = "Plot") 

# resprouting behavior
# get full list of resprouters
resprout <- c("2V5", "8V2", "11V1", "11V6", "2V5", "2V6", "3V1", "3V5", "4V4", "7V6", "8V3", "10L3", "11V1","11V2","11V3","11V4","11V6", "13V2", "14V3", "14V5", "15V2", "16V3", "2V2", "2V5", "2V6", "3V1", "3V5", "4V3", "4V4", "7V5", "8V3", "10L3", "10V4", "11V1","11V2","11V3","11V4","11V6", "13V2", "14V1","14V3","14V4","14V5","14V6", "14L4", "15V2", "16V3", "2V2", "2V5", "2V6", "3V1", "3V5", "4V3", "4V4", "4V3c", "7V1","7V2","7V6", "8V2", "8V3", "10L3", "10V4", "11V1","11V2","11V3","11V4","11V6", "13V2", "14V1c","14V3","14V4","14V5","14V6", "14L4", "15V2", "16V3","1V3", "1V5", "2V5", "2V6", "3V1", "3V5", "7V1", "7V6", "7L3", "8V2", "8V3", "13V2", "14V1c", "14V3", "14V5", "14V6", "16V3")
resprout <- unique(resprout)
herb_list <- firstherb %>% 
  mutate(Spp = substr(Code, nchar(Code)-1,nchar(Code)-1)) %>% 
  # left_join(biomass[,1:3], by = "Code") %>% 
  left_join(select(df_final,Plot, Tmt, Code, CO2, meanSWC, rootmass_g), by = "Code") %>% 
  mutate(resprout = Code %in% resprout) %>% 
  mutate(H2OTmt = substr(Tmt,2,2)) %>% 
  mutate(CO2Tmt = substr(Tmt,1,1))
