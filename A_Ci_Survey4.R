# 11/9/23
# Three-Point A/Ci Curves 
# Quail Ridge FACE

setwd("~/Documents/UC_Davis/2021_Winter/Quals/Proposal/Chapter 1/TinyFACE/GitHub/QuailFACE")
LiCOR_4 <- read.csv("RawData/Quail_LiCOR4_data.csv")
crosswalk <- read.csv("RawData/Survey4.csv")
library("stringr")  
library(tidyverse)
head(LiCOR_4)
head(crosswalk)
# Fix data entry error: 15V1a has 63, 63, 65
crosswalk[241:243,]
crosswalk[242,3] <- 64
crosswalk <- crosswalk[,1:15]

typeof(crosswalk$Date)

crosswalk$SurveyDay <- 
case_match( crosswalk$Date,
   ("30-Sep") ~ 1,
    c("10/1/23", "1-Oct") ~ 2,
     c("10/2/23", "2-Oct") ~ 3)
    
crosswalk$LiCOR_ID <- paste0(crosswalk$SurveyDay, "_", crosswalk$Log)
LiCOR_4$LiCOR_ID <- paste0(LiCOR_4$SurveyDay, "_", LiCOR_4$Obs)
names(LiCOR_4)
length(unique(LiCOR_4$LiCOR_ID))
length(unique(crosswalk$LiCOR_ID))
sum(crosswalk$LiCOR_ID %in% LiCOR_4$LiCOR_ID) # all match! 187 unique measurements

# get plant ID names, treatment codes, into LiCOR measurement df
df <- left_join(LiCOR_4, crosswalk, by = join_by(LiCOR_ID))
# now has each LiCOR measurement associated with plant ID. sets of 3 and 4. CO2 setting goes 400-600-800(-400)

test_df <- df[1:23,c(3, 6:8, 41, 86:93)]
library(ggplot2)
ggplot(test_df, aes(x = Ci.x, y = Photo.x, colour = Treatment)) +
geom_point() +
  facet_wrap( ~ ID) +
geom_vline(xintercept = 400, colour="blue")

ggplot(df, aes(x = Ci.x, y = Photo.x, colour = Treatment)) +
  geom_point() +
  facet_wrap( ~ ID) +
  geom_vline(xintercept = 333, colour="blue")

df %>% 
  select(HHMMSS, Photo.x, Cond, Ci.x, CO2R, SWC, Date, Log, X., Time, ID, Plot, Treatment) %>% 
  group_by(ID, Treatment) %>% 
  summarize(ambient = if_else(max(Ci.x)>400, TRUE, FALSE)) %>% 
  group_by(Treatment) %>% 
  summarize(keepers = sum(ambient)) %>% 
  View()
# AD = 8   AW = 5    ED = 9   EW = 10 

# MG says to take the average midpoint of all curves and make that the cutoff. Also a simple two-way ANOVA of the interpolated values.
midpoints <- df %>% 
  select(HHMMSS, Photo.x, Cond, Ci.x, CO2R, SWC, Date, Log, X., Time, ID, Plot, Treatment) %>% 
  group_by(ID) %>% 
  summarize(Ci.midpoint = (min(Ci.x)+max(Ci.x))/2) 
mean(midpoints$Ci.midpoint) # 339.23
median(midpoints$Ci.midpoint) # 333.2088

# AML: can you get a bigger sample size by lowering the threshold (yes) or extrapolating?
# using 333.21 Ci:
df %>% 
  select(HHMMSS, Photo.x, Cond, Ci.x, CO2R, SWC, Date, Log, X., Time, ID, Plot, Treatment) %>% 
  group_by(ID, Treatment) %>% 
  summarize(threshold = if_else(max(Ci.x)>333.21, TRUE, FALSE)) %>% 
  group_by(Treatment) %>% 
  summarize(keepers = sum(threshold)) %>% 
  View()
# AD = 8   AW = 8    ED = 12   EW = 14 ; 42 total

# Andrew asks how many samples measured per species per treatment? So, of those that passed the threshold test, what is the breakdown?
# first, add species code column
df <- df %>% 
  mutate(Spp = str_sub(ID, - 3, - 3)) 

# tally "acceptable" curves per treatment.spp combo
df %>%   
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
# approx(x,y,xout=5.019802)
df_small <- df %>%   
  select(HHMMSS, Photo.x, Cond, Ci.x, CO2R, SWC, Date, Log, X., Time, ID, Plot, Treatment, Spp) %>% 
  group_by(ID) %>% 
  mutate(interpol = approx(Ci.x,Photo.x, xout=333.21)$y)

ggplot(df_small, aes(x= Treatment, y= interpol)) +
  geom_boxplot(aes(colour=Spp))

ggplot(df_small, aes(x= Treatment, y= interpol)) +
  geom_jitter(aes(colour=Spp))
