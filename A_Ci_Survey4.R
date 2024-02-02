# 11/9/23
# Three-Point A/Ci Curves 
# Quail Ridge FACE

setwd("~/Documents/UC_Davis/2021_Winter/Quals/Proposal/Chapter 1/TinyFACE/GitHub/QuailFACE")
LiCOR_4 <- read.csv("RawData/Quail_LiCOR4_data.csv")
crosswalk4 <- read.csv("RawData/Survey4.csv")
library("stringr")  
library(tidyverse)
head(LiCOR_4)
head(crosswalk4)
# Fix data entry error: 15V1a has 63, 63, 65
crosswalk4[241:243,]
crosswalk4[242,3] <- 64
crosswalk4 <- crosswalk4[,1:15]

typeof(crosswalk4$Date)

crosswalk4$SurveyDay <- 
case_match( crosswalk4$Date,
   ("30-Sep") ~ 1,
    c("10/1/23", "1-Oct") ~ 2,
     c("10/2/23", "2-Oct") ~ 3)
    
crosswalk4$LiCOR_ID <- paste0(crosswalk4$SurveyDay, "_", crosswalk4$Log)
LiCOR_4$LiCOR_ID <- paste0(LiCOR_4$SurveyDay, "_", LiCOR_4$Obs)
names(LiCOR_4)
length(unique(LiCOR_4$LiCOR_ID))
length(unique(crosswalk4$LiCOR_ID))
sum(crosswalk4$LiCOR_ID %in% LiCOR_4$LiCOR_ID) # all match! 187 unique measurements

# get plant ID names, treatment codes, into LiCOR measurement df
df4 <- left_join(LiCOR_4, crosswalk4, by = join_by(LiCOR_ID))
# now has each LiCOR measurement associated with plant ID. sets of 3 and 4. CO2 setting goes 400-600-800(-400)

# test_df <- df[1:23,c(3, 6:8, 41, 86:93)]
# library(ggplot2)
# ggplot(test_df, aes(x = Ci.x, y = Photo.x, colour = Treatment)) +
# geom_point() +
#   facet_wrap( ~ ID) +
# geom_vline(xintercept = 400, colour="blue")

ggplot(df4, aes(x = Ci.x, y = Photo.x, colour = Treatment)) +
  geom_point() +
  facet_wrap( ~ ID) +
  geom_vline(xintercept = 330, colour="blue")
# average of median line for survey 3 (328) and 4 (333)

df4 %>% 
  select(HHMMSS, Photo.x, Cond, Ci.x, CO2R, SWC, Date, Log, X., Time, ID, Plot, Treatment) %>% 
  group_by(ID, Treatment) %>% 
  summarize(ambient = if_else(max(Ci.x)>400, TRUE, FALSE)) %>% 
  group_by(Treatment) %>% 
  summarize(keepers = sum(ambient)) %>% 
  View()
# AD = 8   AW = 5    ED = 9   EW = 10 

# MG says to take the average midpoint of all curves and make that the cutoff. Also a simple two-way ANOVA of the interpolated values.
midpoints4 <- df4 %>% 
  select(HHMMSS, Photo.x, Cond, Ci.x, CO2R, SWC, Date, Log, X., Time, ID, Plot, Treatment) %>% 
  group_by(ID) %>% 
  summarize(Ci.midpoint = (min(Ci.x)+max(Ci.x))/2) 
mean(midpoints4$Ci.midpoint) # 339.23
median(midpoints4$Ci.midpoint) # 333.2088

# AML: can you get a bigger sample size by lowering the threshold (yes) or extrapolating?
# using 333.21 Ci:
df4 %>% 
  select(HHMMSS, Photo.x, Cond, Ci.x, CO2R, SWC, Date, Log, X., Time, ID, Plot, Treatment) %>% 
  group_by(ID, Treatment) %>% 
  summarize(threshold = if_else(max(Ci.x)>333.21, TRUE, FALSE)) %>% 
  group_by(Treatment) %>% 
  summarize(keepers = sum(threshold)) 
# AD = 8   AW = 8    ED = 12   EW = 14 ; 42 total

# Andrew asks how many samples measured per species per treatment? So, of those that passed the threshold test, what is the breakdown?
# first, add species code column
df4 <- df4 %>% 
  mutate(Spp = str_sub(ID, - 3, - 3)) 

# tally "acceptable" curves per treatment.spp combo
df4 %>%   
  select(HHMMSS, Photo.x, Cond, Ci.x, CO2R, SWC, Date, Log, X., Time, ID, Plot, Treatment, Spp) %>% 
  group_by(ID, Treatment, Spp) %>% 
  summarize(threshold = if_else(max(Ci.x)>333.21, TRUE, FALSE)) %>% 
  group_by(Treatment, Spp) %>% 
  summarize(keepers = sum(threshold)) %>% 
  View()
# between 3-7 suitable curves for each Treatment.Spp combination (threshold=333.21) *note, one has all 3 pts too *high*

# how variable are the results by species.treatment? Determines how big a sample size is enough.
# I want to interpolate the Photo.x value at 333 Ci for each curve
# linear interpolation:
# approx(x,y,xout)
df4_small <- df4 %>%   
  select(HHMMSS, Photo.x, Cond, Ci.x, CO2R, SWC, Date, Log, X., Time, ID, Plot, Treatment, Spp) %>% 
  group_by(ID) %>% 
  mutate(interpol = approx(Ci.x,Photo.x, xout=333.21)$y)

df4_smaller <- df4_small[!duplicated(df4_small$interpol),]

ggplot(df4_smaller, aes(x= factor(Treatment, levels= c("AD","ED","AW","EW")), y= interpol)) +
  geom_boxplot(aes(colour=Spp)) +
  #geom_jitter(aes(colour=Spp)) +
  ggtitle("Live and Valley Oak Response to eCO2 x Water Stress") +
  scale_x_discrete(labels=c("Ambient \nDry", "Elevated \nDry", "Ambient \nWet", "Elevated \nWet")) +
  xlab("Treatment") + ylab("A(net) at 333 ppm [CO2]")

ggplot(df4_smaller, aes(x= Treatment, y= interpol)) +
  geom_jitter(aes(colour=Spp))
