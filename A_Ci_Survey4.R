# 11/9/23
# Three-Point A/Ci Curves 
# Quail Ridge FACE

setwd("~/Documents/UC_Davis/2021_Winter/Quals/Proposal/Chapter 1/TinyFACE/GitHub/QuailFACE")
LiCOR_4 <- read.csv("RawData/Quail_LiCOR4_data.csv")
crosswalk <- read.csv("RawData/Survey4.csv")
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
