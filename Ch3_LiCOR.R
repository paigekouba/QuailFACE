# Sat 5/11/24
# LiCOR Data Processing for Ch 3

# will need per-plot CO2 levels:
# get the per-plot ∆CO2 from the 10/9 test (from CO2_Ch2), scale it to the requisite fraction of the mean elevation (127 ppm) added to the mean ambient value (422)
min(avg_between$mDeltaTest) # -6.114156
max(avg_between$mDeltaTest) # 256.731
mean(avg_between$mDeltaTest[avg_between$mDeltaTest>100]) # 210.8419
mean(avg_between$mDeltaTest[avg_between$mDeltaTest<100]) # 8.442497

# for eCO2 plots, the mean is (210 - 127) = 83.8 lower
plot_CO2. <- avg_between %>% 
  select(Plot, mDeltaTest) %>% 
  mutate(CO2 = case_when(mDeltaTest > 100 ~ 422+(mDeltaTest-83.8),
                         mDeltaTest < 100 ~ 422+(mDeltaTest)) ) %>% 
  as.data.frame()

plot_CO2. <- plot_CO2. %>% arrange(as.numeric(Plot))

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
# df_all <- rbind(df3, df4)

df3_nonoverlapping <- df3 %>% 
  filter(!ID %in% df4$ID)

df_all <- rbind(df4, df3_nonoverlapping)

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
# midpoints <- df_all %>%
#   dplyr::select(HHMMSS, Photo, Cond, Ci, CO2R, SWC, Date, Log, X., Time, ID, Plot, Tmt) %>%
#   group_by(ID) %>%
#   summarize(Ci.midpoint = (min(Ci)+max(Ci))/2) 
#mean(midpoints$Ci.midpoint) # 351.6937
#median(midpoints$Ci.midpoint) # 348.1874
# go with 350

# ggplot(df_all, aes(x = Ci, y = Photo, colour = Tmt)) + # this one will show the A/Ci mini curves + midpoint
#   geom_point() +
#   facet_wrap( ~ ID) +
#   geom_vline(xintercept = 350, colour="blue")

# ggplot(LiCOR_L, aes(x = Ci, y = Photo, colour = Tmt)) + # this one will show the A/Ci mini curves + midpoint
#   geom_point() +
#   facet_wrap( ~ ID) +
#   geom_vline(xintercept = 322, colour="blue")

## get linear model predictions for each curve
# LiCOR_IDs <- unique(df_all$ID) # get plant codes in LiCOR sample
# LiCOR_extp <- vector(length = length(LiCOR_IDs)) # initialize results vector
# 
# for(i in 1:length(LiCOR_IDs)){ # calculate linear interpolation for each ID's datapoints, at 350 ppm Ci
#   LiCOR_extp[i] <- predict(lm(Photo ~ Ci, data = df_all %>%
#                               dplyr::select(HHMMSS, Photo, Cond, Ci, CO2R, SWC, Date, Log, X., Time, ID, Plot, Tmt, Spp) %>%
#                               filter(ID == LiCOR_IDs[i])), newdata=data.frame(Ci = 350)) }

# LiCOR_extp_df <- data.frame(cbind(LiCOR_IDs, LiCOR_extp)) # pairs ID/code with predicted Anet at 350
# colnames(LiCOR_extp_df) <- c("ID","LiCOR_extp")
# LiCOR_extp_df$LiCOR_extp <- as.numeric(LiCOR_extp_df$LiCOR_extp)

# do the same for WUE
#LiCOR_gs <- vector(length = length(LiCOR_IDs)) # initialize results vector

# for(i in 1:length(LiCOR_IDs)){ # calculate linear interpolation *of Cond* for each ID's datapoints, at 350 ppm Ci
#   LiCOR_gs[i] <- predict(lm(Cond ~ Ci, data = df_all %>%
#                                 dplyr::select(HHMMSS, Photo, Cond, Ci, CO2R, SWC, Date, Log, X., Time, ID, Plot, Tmt, Spp) %>%
#                                 filter(ID == LiCOR_IDs[i])), newdata=data.frame(Ci = 350)) }
# 
# LiCOR_gs_df <- data.frame(cbind(LiCOR_IDs, LiCOR_gs)) # pairs ID/code with predicted gs at 350
# colnames(LiCOR_gs_df) <- c("ID","LiCOR_gs")
# LiCOR_gs_df$LiCOR_gs <- as.numeric(LiCOR_gs_df$LiCOR_gs)
# 
# plotting_df <- data.frame(ID = c(df_all$ID, LiCOR_extp_df$ID), Ci = c(df_all$Ci, rep(350,length(unique(df_all$ID)))), Photo = c(df_all$Photo, LiCOR_extp_df$LiCOR_extp), Cond = c(df_all$Cond, LiCOR_gs_df$LiCOR_gs))

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

# LiCOR_df <- df_all %>% 
#   group_by(ID) %>% 
#   summarise(across(HHMMSS:SWC, ~ mean(.x, na.rm = TRUE))) %>% 
#   ungroup() %>% 
#   mutate(Plot = if_else(nchar(ID) == 4, substr(ID,1,1), substr(ID,1,2)), Spp = str_sub(ID, - 3, - 3))  %>%
#   left_join(lookup, by = "Plot") # add Tmt codes
# 
# LiCOR_df <- LiCOR_df %>% 
#   left_join(plotting_df[plotting_df$Ci == 350,], by="ID") %>% 
#   mutate(WUE.350 = Photo.y/Cond.y)
# ready!

## try the old way, but with all the data and doing interpolations for Ca = 422, 544 (= Ci 322, 416)






# add in Survey 1 and 2

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
  rbind(filter(select(LiCOR_1, ID, Plot, Date, HHMMSS, Ci, Photo, Cond, Tleaf, PARi, VpdL, CO2R, RH_R, RH_S, SWC, Spp), ID %in% c("12L6a","16L1b", "7V1b"))) %>% 
  rbind(filter(select(LiCOR_2, ID, Plot, Date, HHMMSS, Ci, Photo, Cond, Tleaf, PARi, VpdL, CO2R, RH_R, RH_S, SWC, Spp), ID %in% c("12L6a","16L1b", "7V1b"))) %>% 
  left_join(lookup, by = "Plot") %>% 
  mutate(Tmt = as.factor(Tmt)) %>% 
  filter(!is.na(Tmt)) %>% 
  filter(Ci > 0, Photo > 0)


## get linear model predictions for each curve
# start by extracting CO2S and Ci from LiCOR_all
ggplot(LiCOR_all) +
  geom_point(aes(x=CO2R, y=Ci, color=Tmt)) + facet_grid(~Spp)

# need to group by ID and then find the Ci predicted at CO2S = 422 and 544 (OR CO2S = plotwise CO2 means from plot_CO2.)
# so like... a function that works on the data (x,y points for CO2S, Ci) 

LiCOR_IDs <- unique(LiCOR_all$ID) # get plant codes in LiCOR sample
LiCOR_Ci <- vector(length = length(LiCOR_IDs)) # initialize results vector

# make a function where you get from ID to CO2S based on plot_CO2.
seedling_CO2 <- function(ID){
  plot <- if_else(nchar(ID) == 4, substr(ID,1,1), substr(ID,1,2))
  plotCO2 <- select(filter(plot_CO2., Plot==plot),CO2)
  names(plotCO2) <- "CO2R"
  return(plotCO2)
}
seedling_CO2(ID = "10L3b")

# # need to know how good the following function is; how well does CO2S predict Ci for existing data?
# summary(lm(Ci ~ CO2R, data=LiCOR_all))
# # it anyway should be different for different treatment groups
# summary(lm(Ci ~ CO2R + Tmt, data=LiCOR_all))$adj.r.squared
# # don't panic, you are fitting the model to each plant specifically each time

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
mean(r.sq_df$r.sq) # 0.9212305
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
mean(Anet_r.sq_df$Anet_r.sq) # 0.7963953


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
mean(gs_r.sq_df$gs_r.sq) # 0.6098981


newLiCOR <- left_join(LiCOR_Ci_df, LiCOR_gs_df, by="ID") %>% left_join(LiCOR_Anet_df, by="ID")

# plotting_df <- data.frame(ID = c(df_all$ID, LiCOR_extp_df$ID), Ci = c(df_all$Ci, rep(350,length(unique(df_all$ID)))), Photo = c(df_all$Photo, LiCOR_extp_df$LiCOR_extp), Cond = c(df_all$Cond, LiCOR_gs_df$LiCOR_gs))

#left_join(LiCOR_all, newLiCOR, by = "ID")

LiCOR_new <- left_join(LiCOR_all, newLiCOR, by = "ID") %>% 
  group_by(ID) %>% 
  mutate(n=n()) %>% 
  filter(n>2) %>% 
  summarise(across(c(HHMMSS:SWC,Ci.y,gs,Anet), ~ mean(.x, na.rm = TRUE))) %>% 
  ungroup() %>% 
  mutate(Plot = if_else(nchar(ID) == 4, substr(ID,1,1), substr(ID,1,2)), Spp = str_sub(ID, - 3, - 3))  %>%
  left_join(lookup, by = "Plot") # add Tmt codes


ggplot() + # this one will show how well predicted Anet value tracks the A/Ci mini curve
  geom_point(data=LiCOR_all, mapping=aes(x = Ci, y = Photo, color=Date)) +
  geom_smooth(data=LiCOR_all, aes(x=Ci, y=Photo), color="forestgreen",method="lm", se=F) +
  geom_point(LiCOR_new, mapping=aes(x = Ci.y, y = Anet), color="red") +
  facet_wrap( ~ ID)

ggplot() + # this one will show how well predicted gs value tracks the gs vs Ci for each plant
  geom_point(data=LiCOR_all, mapping=aes(x = Ci, y = Cond, color=Date)) +
  geom_smooth(data=LiCOR_all, aes(x=Ci, y=Cond), color="goldenrod",method="lm", se=F) +
  geom_point(LiCOR_new, mapping=aes(x = Ci.y, y = gs), color="blue") +
  facet_wrap( ~ ID)

ggplot(LiCOR_new) + # this shows WUE (as the slope of Anet ~ gs, per Spp*Tmt)
  geom_point(aes(x=gs, y=Anet, group=Tmt, color=Tmt)) + geom_smooth(aes(x=gs, y=Anet, group=Tmt, color=Tmt), method="lm") + facet_grid(~Spp)

LiCOR_new %>% 
  ggplot() +
  geom_boxplot(aes(x=Tmt, y=log(Anet), color=Tmt)) + facet_grid(~Spp) + scale_color_manual(values = c("pink", "lightblue", "red", "blue"))

LiCOR_new %>% 
  ggplot() +
  geom_boxplot(aes(x=Tmt, y=gs, color=Tmt)) + facet_grid(~Spp) + scale_color_manual(values = c("pink", "lightblue", "red", "blue"))

LiCOR_new %>% 
  ggplot() +
  geom_boxplot(aes(x=Tmt, y=SWC)) + ylim(c(0,11))


t.test(plot_SWC.[plot_SWC.$Plot %in% c(3,8,12,16),"meanSWC"],plot_SWC.[plot_SWC.$Plot %in% c(2,6,9,13),"meanSWC"]) # p = 0.1872 for AD v. ED

t.test(plot_SWC.[plot_SWC.$Plot %in% c(1,5,10,14),"meanSWC"],plot_SWC.[plot_SWC.$Plot %in% c(4,7,11,15),"meanSWC"]) # p = 0.522 for AW v. EW
# redo this for SWC_plot
