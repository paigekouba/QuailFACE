# Sat 5/11/24
# LiCOR Data Processing for Ch 3

setwd("~/Documents/UC_Davis/2021_Winter/Quals/Proposal/Chapter 1/TinyFACE/GitHub/QuailFACE")
LiCOR_3 <- read.csv("RawData/LiCOR_8.26.23.csv") # LiCOR datafile
crosswalk3 <- read.csv("RawData/Survey3.csv") # data sheet with observation and plant codes
library("stringr")  
library(tidyverse)

crosswalk3 <- crosswalk3[-c(1:2),c(1:6)]
lookup <- data.frame(as.character(c(1:16)), c("AW","ED","AD","EW",
                                              "AW","ED","EW","AD",
                                              "ED","AW","EW","AD",
                                              "ED","AW","EW","AD"))
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
df_all <- rbind(df3, df4)

# df_all %>% 
#   group_by(ID) %>% 
#    summarise(across(HHMMSS:SWC, ~ mean(.x, na.rm = TRUE))) %>% 
#    ungroup() %>% 
#    mutate(Plot = if_else(nchar(ID) == 4, substr(ID,1,1), substr(ID,1,2)), Spp = str_sub(ID, - 3, - 3))  %>%
#    left_join(lookup, by = "Plot") %>% 
# ggplot() + # is there a relationship with leaf temperature?
#   geom_point(aes(x=Tleaf, y=Photo, group=Tmt, color=Tmt)) +
#   geom_smooth(aes(x=Tleaf, y=Photo, group=Tmt, color=Tmt), se=F) + 
#   scale_color_manual(values = c("pink", "lightblue", "red", "blue")) + facet_grid(~Tmt)

# df_all %>% 
#   group_by(ID) %>% 
#   summarise(across(HHMMSS:SWC, ~ mean(.x, na.rm = TRUE))) %>% 
#   ungroup() %>% 
#   mutate(Plot = if_else(nchar(ID) == 4, substr(ID,1,1), substr(ID,1,2)), Spp = str_sub(ID, - 3, - 3))  %>%
#   left_join(lookup, by = "Plot") %>% 
#   ggplot() + # is there a relationship with SWC?
#   geom_point(aes(x=SWC, y=Photo)) +
#   geom_smooth(aes(x=SWC, y=Photo), se=F) + 
#   scale_color_manual(values = c("pink", "lightblue", "red", "blue"))

# df_all %>% 
#   group_by(ID) %>% 
#   summarise(across(HHMMSS:SWC, ~ mean(.x, na.rm = TRUE))) %>% 
#   ungroup() %>% 
#   mutate(Plot = if_else(nchar(ID) == 4, substr(ID,1,1), substr(ID,1,2)), Spp = str_sub(ID, - 3, - 3))  %>%
#   left_join(lookup, by = "Plot") %>% 
#   ggplot() + # is there a relationship with h2o.i?
#   geom_point(aes(x=h2o_i, y=Photo, group=Tmt, color=Tmt)) +
#   geom_smooth(aes(x=h2o_i, y=Photo, group=Tmt, color=Tmt), se=F) + 
#   scale_color_manual(values = c("pink", "lightblue", "red", "blue")) + facet_grid(~Tmt)

# df_all %>% 
#   group_by(ID) %>% 
#   summarise(across(HHMMSS:SWC, ~ mean(.x, na.rm = TRUE))) %>% 
#   ungroup() %>% 
#   mutate(Plot = if_else(nchar(ID) == 4, substr(ID,1,1), substr(ID,1,2)), Spp = str_sub(ID, - 3, - 3))  %>%
#   left_join(lookup, by = "Plot") %>% 
#   ggplot() + # is there a relationship with h20diff?
#   geom_point(aes(x=h20diff, y=Photo, group=Tmt, color=Tmt)) +
#   geom_smooth(aes(x=h20diff, y=Photo, group=Tmt, color=Tmt), se=F) + 
#   scale_color_manual(values = c("pink", "lightblue", "red", "blue")) + facet_grid(~Tmt)

# df_all %>% 
#   group_by(ID) %>% 
#   summarise(across(HHMMSS:SWC, ~ mean(.x, na.omit = TRUE))) %>% 
#   ungroup() %>% 
#   mutate(Plot = if_else(nchar(ID) == 4, substr(ID,1,1), substr(ID,1,2)), Spp = str_sub(ID, - 3, - 3))  %>%
#   left_join(lookup, by = "Plot") %>% 
#   ggplot() + # is there a relationship with Ci?
#   geom_point(aes(x=Ci, y=Photo, group=Tmt, color=Tmt)) +
#   geom_smooth(aes(x=Ci, y=Photo, group=Tmt, color = Tmt), method = "lm", se=F) + 
#   scale_color_manual(values = c("pink", "lightblue", "red", "blue")) + facet_wrap(~Spp)

# Tleaf, PARo, h2o.i, h20diff, Ci.Ca   vs. Photo

# interpolation: want to compare treatment groups on apples-to-apples basis. Find the midpoint of all the curves and extrapolate to that value, for Photo and Cond (and other variables deemed necessary)
midpoints <- df_all %>%
  dplyr::select(HHMMSS, Photo, Cond, Ci, CO2R, SWC, Date, Log, X., Time, ID, Plot, Tmt) %>%
  group_by(ID) %>%
  summarize(Ci.midpoint = (min(Ci)+max(Ci))/2) 
#mean(midpoints$Ci.midpoint) # 351.6937
#median(midpoints$Ci.midpoint) # 348.1874
# go with 350

# ggplot(df_all, aes(x = Ci, y = Photo, colour = Tmt)) + # this one will show the A/Ci mini curves + midpoint
#   geom_point() +
#   facet_wrap( ~ ID) +
#   geom_vline(xintercept = 350, colour="blue")

## get linear model predictions for each curve
LiCOR_IDs <- unique(df_all$ID) # get plant codes in LiCOR sample
LiCOR_extp <- vector(length = length(LiCOR_IDs)) # initialize results vector

for(i in 1:length(LiCOR_IDs)){ # calculate linear interpolation for each ID's datapoints, at 350 ppm Ci
  LiCOR_extp[i] <- predict(lm(Photo ~ Ci, data = df_all %>%
                              dplyr::select(HHMMSS, Photo, Cond, Ci, CO2R, SWC, Date, Log, X., Time, ID, Plot, Tmt, Spp) %>%
                              filter(ID == LiCOR_IDs[i])), newdata=data.frame(Ci = 350)) }

LiCOR_extp_df <- data.frame(cbind(LiCOR_IDs, LiCOR_extp)) # pairs ID/code with predicted Anet at 350
colnames(LiCOR_extp_df) <- c("ID","LiCOR_extp")
LiCOR_extp_df$LiCOR_extp <- as.numeric(LiCOR_extp_df$LiCOR_extp)

# do the same for WUE
LiCOR_gs <- vector(length = length(LiCOR_IDs)) # initialize results vector

for(i in 1:length(LiCOR_IDs)){ # calculate linear interpolation *of Cond* for each ID's datapoints, at 350 ppm Ci
  LiCOR_gs[i] <- predict(lm(Cond ~ Ci, data = df_all %>%
                                dplyr::select(HHMMSS, Photo, Cond, Ci, CO2R, SWC, Date, Log, X., Time, ID, Plot, Tmt, Spp) %>%
                                filter(ID == LiCOR_IDs[i])), newdata=data.frame(Ci = 350)) }

LiCOR_gs_df <- data.frame(cbind(LiCOR_IDs, LiCOR_gs)) # pairs ID/code with predicted gs at 350
colnames(LiCOR_gs_df) <- c("ID","LiCOR_gs")
LiCOR_gs_df$LiCOR_gs <- as.numeric(LiCOR_gs_df$LiCOR_gs)

plotting_df <- data.frame(ID = c(df_all$ID, LiCOR_extp_df$ID), Ci = c(df_all$Ci, rep(350,length(unique(df_all$ID)))), Photo = c(df_all$Photo, LiCOR_extp_df$LiCOR_extp), Cond = c(df_all$Cond, LiCOR_gs_df$LiCOR_gs))

# plotting_df %>% 
# ggplot(aes(x = Ci, y = Photo)) + # this one will show how well predicted Anet value tracks the A/Ci mini curve
#   geom_point() +
#   geom_vline(xintercept = 350, colour="blue", linetype="dashed") +
#   geom_smooth(data=plotting_df, aes(x=Ci, y=Photo), color="forestgreen",method="lm", se=F) +
#   facet_wrap( ~ ID)

# plotting_df %>% 
#   ggplot(aes(x = Ci, y = Cond)) + # this one will show how well predicted Cond value tracks the Cond/Ci mini curve
#   geom_point() +
#   geom_vline(xintercept = 350, colour="blue", linetype="dashed") +
#   geom_smooth(data=plotting_df, aes(x=Ci, y=Cond),method="lm", se=F) +
#   facet_wrap( ~ ID)

LiCOR_df <- df_all %>% 
  group_by(ID) %>% 
  summarise(across(HHMMSS:SWC, ~ mean(.x, na.rm = TRUE))) %>% 
  ungroup() %>% 
  mutate(Plot = if_else(nchar(ID) == 4, substr(ID,1,1), substr(ID,1,2)), Spp = str_sub(ID, - 3, - 3))  %>%
  left_join(lookup, by = "Plot") # add Tmt codes

LiCOR_df <- LiCOR_df %>% 
  left_join(plotting_df[plotting_df$Ci == 350,], by="ID") %>% 
  mutate(WUE.350 = Photo.y/Cond.y)
# ready!

# trying package plantecophys
install.packages("plantecophys")
library(plantecophys)
acidata1
View(manyacidat)

# get df_all into this format

duursma <- df_all %>% 
  select(CO2S, Ci, Tleaf, Photo, PARi, ID) %>% 
  fitacis(group="ID")

duursma2 <- df_all %>% 
  mutate(Spp = as.factor(Spp)) %>% 
  select(CO2S, Ci, Tleaf, Photo, PARi, ID, Tmt, Spp) %>% 
  filter(ID %in% names(Filter(function(a) any(!is.na(a)), duursma))) %>% 
  fitacis(group="ID", id="Spp")

plot(duursma2, how="oneplot", colour_by_id = TRUE)

Filter(function(a) any(!is.na(a)), duursma) %>% plantecophys::plot(how="oneplot")

plot(duursma[[1:11]], how="oneplot")
plot(duursma, how="oneplot", what="data")
plot(duursma)
plot(duursma, how="")
