# 1/31/24
# Three-Point A/Ci Curves 
# Quail Ridge FACE

setwd("~/Documents/UC_Davis/2021_Winter/Quals/Proposal/Chapter 1/TinyFACE/GitHub/QuailFACE")
LiCOR_3 <- read.csv("RawData/LiCOR_8.26.23.csv")
crosswalk3 <- read.csv("RawData/Survey3.csv")
library("stringr")  
library(tidyverse)
head(LiCOR_3)
head(crosswalk3)

crosswalk3 <- crosswalk3[,1:15]
# data entry error; log for 12V6b is listed as 12 four times
which(crosswalk3$Log == "12")
# 42 168 209 210 211 212
crosswalk3[210,3] <- 13
crosswalk3[211,3] <- 14
crosswalk3[212,3] <- 15
# data entry error; log for 2V4b taken 8/26/23, misentered as 8/29
crosswalk3[crosswalk3$ID == "2V4b",2] <- "8/26/23"

typeof(crosswalk3$Date)

crosswalk3$SurveyDay <- 
  case_match( crosswalk3$Date,
              c("8/26","8/26/23") ~ 1,
              c("8/27") ~ 2,
              c("8/29","8/29/23") ~ 3)

crosswalk3$LiCOR_ID <- paste0(crosswalk3$SurveyDay, "_", crosswalk3$Log)
LiCOR_3$LiCOR_ID <- paste0(LiCOR_3$SurveyDay, "_", LiCOR_3$Obs)
names(LiCOR_3)
length(unique(LiCOR_3$LiCOR_ID))
length(unique(crosswalk3$LiCOR_ID))
sum(unique(crosswalk3$LiCOR_ID) %in% unique(LiCOR_3$LiCOR_ID)) # all match! 224 unique measurements

LiCOR_3 <- LiCOR_3 %>% 
  filter(LiCOR_ID != "NA_NA")

# get plant ID names, treatment codes, into LiCOR measurement df
df3 <- left_join(LiCOR_3, crosswalk3, by = join_by(LiCOR_ID))
df3 <- df3 %>% 
  filter(Photo.x > 0 & Ci.x > 0) %>% 
  filter(ID != "NA")
# now has each LiCOR measurement associated with plant ID. sets of 3 and 4. CO2 setting goes 400-600-800(-400)

# test_df <- df[1:23,c(3, 6:8, 41, 86:93)]
# library(ggplot2)
# ggplot(test_df, aes(x = Ci.x, y = Photo.x, colour = Treatment)) +
#   geom_point() +
#   facet_wrap( ~ ID) +
#   geom_vline(xintercept = 400, colour="blue")
# 
# test_df2 <- df %>% 
#   select(Ci.x, Photo.x, Treatment, SWC, Plot, ID) %>% 
#   filter(Photo.x > 0 & Ci.x > 0) %>% 
#   filter(ID != "NA")

ggplot(df3, aes(x = Ci.x, y = Photo.x, colour = Treatment)) +
  geom_point() +
  facet_wrap( ~ ID) +
  geom_vline(xintercept = 330, colour="blue")
 
# df %>% 
#   select(HHMMSS, Photo.x, Cond, Ci.x, CO2R, SWC, Date, Log, X., Time, ID, Plot, Treatment) %>% 
#   group_by(ID, Treatment) %>% 
#   summarize(ambient = if_else(max(Ci.x)>400, TRUE, FALSE)) %>% 
#   group_by(Treatment) %>% 
#   summarize(keepers = sum(ambient)) %>% 
#   View()
# # AD = 8   AW = 5    ED = 9   EW = 10 

# MG says to take the average midpoint of all curves and make that the cutoff. Also a simple two-way ANOVA of the interpolated values.
midpoints3 <- df3 %>% 
  select(HHMMSS, Photo.x, Cond, Ci.x, CO2R, SWC, Date, Log, X., Time, ID, Plot, Treatment) %>% 
  group_by(ID) %>% 
  summarize(Ci.midpoint = (min(Ci.x)+max(Ci.x))/2) 
mean(midpoints3$Ci.midpoint) # 340.2411
median(midpoints3$Ci.midpoint) # 328.6591

# AML: can you get a bigger sample size by lowering the threshold (yes) or extrapolating?
# using 333.21 Ci:
df3 %>% 
  select(HHMMSS, Photo.x, Cond, Ci.x, CO2R, SWC, Date, Log, X., Time, ID, Plot, Treatment) %>% 
  group_by(ID, Treatment) %>% 
  summarize(threshold = if_else(max(Ci.x)>330, TRUE, FALSE)) %>% 
  group_by(Treatment) %>% 
  summarize(keepers = sum(threshold)) 
# AD = 9   AW = 16    ED = 15   EW = 14 ; 54 total

# Andrew asks how many samples measured per species per treatment? So, of those that passed the threshold test, what is the breakdown?
# first, add species code column
df3 <- df3 %>% 
  mutate(Spp = str_sub(ID, - 3, - 3)) 

# tally "acceptable" curves per treatment.spp combo
df3 %>%   
  select(HHMMSS, Photo.x, Cond, Ci.x, CO2R, SWC, Date, Log, X., Time, ID, Plot, Treatment, Spp) %>% 
  group_by(ID, Treatment, Spp) %>% 
  summarize(threshold = if_else(max(Ci.x)>330, TRUE, FALSE)) %>% 
  group_by(Treatment, Spp) %>% 
  summarize(keepers = sum(threshold)) %>% 
  View()
# between 3-8 suitable curves for each Treatment.Spp combination (threshold=328.67) 

# how variable are the results by species.treatment? Determines how big a sample size is enough.
# I want to interpolate the Photo.x value at 333 Ci for each curve
# linear interpolation:
# approx(x,y,xout)
df3_small <- df3 %>%   
  select(HHMMSS, Photo.x, Cond, Ci.x, CO2R, SWC, Date, Log, X., Time, ID, Plot, Treatment, Spp) %>% 
  group_by(ID) %>% 
  mutate(interpol = approx(Ci.x,Photo.x, xout=328.67)$y)

df3_smaller <- df3_small[!duplicated(df3_small$interpol),]

# combine data for survey 3 and 4
df_all <- rbind(df3_smaller, df4_smaller)

ggplot(df3_smaller, aes(x= factor(Treatment, levels= c("AD","ED","AW","EW")), y= interpol)) +
  geom_boxplot(aes(colour=Spp)) +
  #geom_jitter(aes(colour=Spp)) +
  ggtitle("Live and Valley Oak Response to eCO2 x Water Stress") +
  scale_x_discrete(labels=c("Ambient \nDry", "Elevated \nDry", "Ambient \nWet", "Elevated \nWet")) +
  xlab("Treatment") + ylab("A(net) at 328 ppm [CO2]")

ggplot(df_all, aes(x= factor(Treatment, levels= c("AD","ED","AW","EW")), y= interpol)) +
  geom_boxplot(aes(colour=Spp)) +
  geom_point(aes(colour=Spp), position=position_jitterdodge(dodge.width=0.75, jitter.width=0.05), alpha=0.5, size=2) +
  facet_wrap( ~ Spp) +
  ggtitle("Live and Valley Oak Response to eCO2 x Water Stress") +
  scale_x_discrete(labels=c("Ambient \nDry", "Elevated \nDry", "Ambient \nWet", "Elevated \nWet")) +
  xlab("Treatment") + ylab("A(net) at 330 ppm [CO2]") +
  theme_bw(base_size=22)

# combine all species? NO it was gross
# facet wrap instead of colour=Spp — nice

# make it look like the hypothesis plot
summarySE <- function(data=NULL, measurevar, groupvars=NULL, na.rm=FALSE,
                      conf.interval=.95, .drop=TRUE) {
  library(plyr)
  # New version of length which can handle NA's: if na.rm==T, don't count them
  length2 <- function (x, na.rm=FALSE) {
    if (na.rm) sum(!is.na(x))
    else       length(x)
  }
  # This does the summary. For each group's data frame, return a vector with
  # N, mean, and sd
  datac <- ddply(data, groupvars, .drop=.drop,
                 .fun = function(xx, col) {
                   c(N    = length2(xx[[col]], na.rm=na.rm),
                     mean = mean   (xx[[col]], na.rm=na.rm),
                     sd   = sd     (xx[[col]], na.rm=na.rm)
                   )
                 },
                 measurevar)
  # Rename the "mean" column    
  datac <- rename(datac, c("mean" = measurevar))
  datac$se <- datac$sd / sqrt(datac$N)  # Calculate standard error of the mean
  # Confidence interval multiplier for standard error
  # Calculate t-statistic for confidence interval: 
  # e.g., if conf.interval is .95, use .975 (above/below), and use df=N-1
  ciMult <- qt(conf.interval/2 + .5, datac$N-1)
  datac$ci <- datac$se * ciMult
  return(datac)
}
df_all_se <- summarySE(df_all, measurevar = "interpol", groupvars=c("Treatment","Spp"), na.rm = TRUE)

L_se_D <- df_all_se %>% 
  filter(Spp == "L") %>% 
  filter(Treatment == "AD" | Treatment == "ED") %>% 
  mutate(Water = "Dry") %>% 
  mutate(CO2 = case_when(Treatment =="AD" ~ "Ambient",
                         TRUE ~ "Elevated"))

L_se_W <- df_all_se %>% 
  filter(Spp == "L") %>% 
  filter(Treatment == "AW" | Treatment == "EW") %>% 
  mutate(Water = "Wet") %>% 
  mutate(CO2 = case_when(Treatment =="AW" ~ "Ambient",
                         TRUE ~ "Elevated"))

V_se_D <- df_all_se %>% 
  filter(Spp == "V") %>% 
  filter(Treatment == "AD" | Treatment == "ED") %>% 
  mutate(Water = "Dry") %>% 
  mutate(CO2 = case_when(Treatment =="AD" ~ "Ambient",
                         TRUE ~ "Elevated"))

V_se_W <- df_all_se %>% 
  filter(Spp == "V") %>% 
  filter(Treatment == "AW" | Treatment == "EW") %>% 
  mutate(Water = "Wet") %>% 
  mutate(CO2 = case_when(Treatment =="AW" ~ "Ambient",
                         TRUE ~ "Elevated"))


all_se_D <- df_all_se %>% 
  filter(Treatment == "AD" | Treatment == "ED") %>% 
  mutate(Water = "Dry") %>% 
  mutate(CO2 = case_when(Treatment =="AD" ~ "Ambient",
                         TRUE ~ "Elevated"))

all_se_W <- df_all_se %>% 
  filter(Treatment == "AW" | Treatment == "EW") %>% 
  mutate(Water = "Wet") %>% 
  mutate(CO2 = case_when(Treatment =="AW" ~ "Ambient",
                         TRUE ~ "Elevated"))


# Standard error of the mean
ggplot(df_all_se, aes(x=factor(Treatment, levels= c("AD","ED","AW","EW")), y=interpol, colour=Spp)) + 
  geom_errorbar(aes(ymin=interpol-se, ymax=interpol+se), width=.1) +
  geom_line() +
  geom_point() 

ggplot() + 
  geom_point(data = L_se_D, aes(x=factor(CO2), y=interpol), 
             colour = "red", size = 5) +
  geom_errorbar(data = L_se_D, aes(x=factor(CO2), 
     ymin=interpol-se, ymax=interpol+se), width=.1, colour = "red") +
  geom_point(data = L_se_W, aes(x=factor(CO2), y=interpol), 
             colour = "blue", size = 5)+
  geom_errorbar(data = L_se_W, aes(x=factor(CO2), y=interpol, 
    ymin=interpol-se, ymax=interpol+se), width=.1, colour = "blue")

ggplot() + 
  geom_point(data = V_se_D, aes(x=factor(CO2), y=interpol), 
             colour = "red", size = 5) +
  geom_errorbar(data = V_se_D, aes(x=factor(CO2), 
                                   ymin=interpol-se, ymax=interpol+se), width=.1, colour = "red") +
  geom_point(data = V_se_W, aes(x=factor(CO2), y=interpol), 
             colour = "blue", size = 5)+
  geom_errorbar(data = V_se_W, aes(x=factor(CO2), y=interpol, 
                                   ymin=interpol-se, ymax=interpol+se), width=.1, colour = "blue")

ggplot() + 
  geom_point(data = all_se_D, aes(x=factor(CO2), y=interpol), colour = "red", size = 7) +
  geom_errorbar(data = all_se_D, aes(x=factor(CO2), 
    ymin=interpol-se, ymax=interpol+se), width=.1, colour = "red", linewidth = 1) +
  geom_point(data = all_se_W, aes(x=factor(CO2), y=interpol), colour = "blue", size = 7)+
  geom_errorbar(data = all_se_W, aes(x=factor(CO2), y=interpol, 
    ymin=interpol-se, ymax=interpol+se), width=.1, colour = "blue", linewidth = 1) +
facet_wrap( ~ Spp) +
  ggtitle("Live and Valley Oak Response to eCO2 x Water Stress") +
  scale_x_discrete(labels=c("Ambient \nCO2", "Elevated \nCO2")) +
  xlab("Treatment") + ylab("Net CO2 Uptake") +
  theme_classic(base_size=22)

