# Data Explore for Chapter 3

# for now, treating CO2 as a categorical variable; later, could get average ∆CO2 for each treatment, or even scale by position in plot (?)
# Response variables: LiCOR data, Above and below-ground biomass data, Root stx, SIF d13C, Growth/mortality


## LiCOR data (Photosynthesis)
# from Ch3_LiCOR
head(LiCOR_df)
# check whether HHMMSS is a time
str(LiCOR_df$HHMMSS)
length(unique(LiCOR_df$ID)) # 75
length(LiCOR_df$ID) # 75
# NB some of the seedlings are resampled (92 curves, 63 plants)

nequals_licor <- LiCOR_df %>% # here I want a count per Spp per Tmt
  group_by(Tmt, Spp) %>% 
  tally()

# LiCOR_df is a combination of the mini ACi curves from two sampling efforts, collapsed to include one predicted Anet and one predicted gs per plant, with averages of other numeric variables. Conductance and the interpolated value of Anet are used to calculate WUE. SWC was measured with a TDR probe immediately before LiCOR samples were taken for each plot. HHMMSS is probably necessary because photosynthetic rates may change throughout the day. Plot as a blocking variable; separate models for Spp?

# how does time affect Anet? 
ggplot(LiCOR_df.) +
  geom_point(aes(x=rescale(HHMMSS), y=Anet, color=Tmt)) + geom_smooth(aes(x=rescale(HHMMSS), y=Anet, color=Tmt), method="lm") + scale_color_manual(values = c("pink", "lightblue", "red", "blue")) + facet_grid(~Spp)
ggplot(LiCOR_df.) +
  geom_point(aes(x=rescale(HHMMSS), y=Anet)) + geom_smooth(aes(x=rescale(HHMMSS), y=Anet), method="lm") + facet_grid(~Spp)
summary(lm(Anet ~ rescale(HHMMSS)+Tmt, filter(LiCOR_df., Spp =="V"))) # -10.852, p = 0.000730 ***
summary(lm(Anet ~ rescale(HHMMSS)+Tmt, filter(LiCOR_df., Spp=="L"))) # -4.6311, p = 0.043238 *

summary(lm(Anet ~ rescale(CO2)+rescale(meanSWC)+time_scaled, plotmeans.V)) #
summary(lmer(Anet ~ rescale(CO2)+rescale(meanSWC)+time_scaled +(1|Plot), filter(final_df, Spp=="V"))) 
summary(lm(Anet ~ rescale(CO2)*rescale(meanSWC)+time_scaled, plotmeans.L)) 
summary(lmer(Anet ~ rescale(CO2)*rescale(meanSWC)+time_scaled +(1|Plot), filter(final_df, Spp=="L"))) 

ggplot(LiCOR_df.) + # what is the effect of time on gs?
  geom_point(aes(x=rescale(HHMMSS), y=gs, color=Tmt)) + geom_smooth(aes(x=rescale(HHMMSS), y=gs, color=Tmt), method="lm") + scale_color_manual(values = c("pink", "lightblue", "red", "blue")) + facet_grid(~Spp)
ggplot(LiCOR_df.) +
  geom_point(aes(x=rescale(HHMMSS), y=gs)) + geom_smooth(aes(x=rescale(HHMMSS), y=gs), method="lm") + facet_grid(~Spp)
summary(lm(gs ~ rescale(HHMMSS), LiCOR_df.)) # -0.10057, p=0.00483
summary(lm(gs ~ rescale(HHMMSS)+Tmt, LiCOR_df.))

summary(lm(Anet ~ rescale(HHMMSS)+Tmt+Tleaf, filter(LiCOR_df., Spp =="V"))) 
summary(lm(Anet ~ rescale(HHMMSS)+Tmt+Tleaf, filter(LiCOR_df., Spp=="L"))) 

# see how they associate by plot (with per-plot eCO2 ?)
plot_order <- c("3","8","12","16","1","5","10","14","2","6","9","13","4","7","11","15")
ggplot(LiCOR_df) +
  geom_boxplot(aes(x=factor(Plot, level=plot_order), y=Photo.y, color=Tmt)) + facet_grid(rows = vars(Spp), scales = "free") + scale_color_manual(values = c("pink", "lightblue", "red", "blue")) 

# how does SWC look across different plots/treatments? first, average SWC measurements by Plot
LiCOR_df. %>% 
  group_by(Plot, Tmt) %>% mutate(meanSWC = mean(SWC)) %>% 
  distinct(Plot, .keep_all = TRUE) %>% 
ggplot() +
  geom_boxplot(aes(x=Tmt, y=meanSWC, color = Tmt)) + geom_point(aes(x=Tmt, y=meanSWC, color = Tmt)) + scale_color_manual(values = c("pink", "lightblue", "red", "blue")) 

ggplot(LiCOR_df) +
  geom_boxplot(aes(x=Plot, y=SWC, color = Tmt)) # plot 10 and 15 stink but still >2x SWC of highest dry plot

library(gridExtra)
grid.arrange( # breaks down Anet, gs, and WUE
  LiCOR_df %>% ggplot() + geom_boxplot(aes(x=Tmt, y= Photo.y, color = Tmt)) + facet_grid(rows = vars(Spp), scales = "free") + labs(y = "Standardized A(net)") + 
    geom_text(data = nequals_licor, aes(x = Tmt, y = 1, label = paste0("N = ",n))) +  
    scale_color_manual(values = c("pink", "lightblue", "red", "blue")) ,
  LiCOR_df %>% ggplot() + geom_boxplot(aes(x=Tmt, y= log(Cond.y), color = Tmt)) + facet_grid(rows = vars(Spp), scales = "free") + labs(y = "log(Stomatal Conductance)") + geom_text(data = nequals_licor, aes(x = Tmt, y = 0.01, label = paste0("N = ",n)))
  +  scale_color_manual(values = c("pink", "lightblue", "red", "blue")) ,
  LiCOR_df %>% 
    filter(WUE.350 < 1500) %>% # removing one outlier from AW, L
    ggplot() + geom_boxplot(aes(x=Tmt, y= WUE.350, color = Tmt)) + facet_grid(rows = vars(Spp), scales = "free") +
  scale_color_manual(values = c("pink", "lightblue", "red", "blue")) + geom_text(data = nequals_licor, aes(x = Tmt, y = 4, label = paste0("N = ",n))), nrow =1  )

# to accurately compare all the following data types, need to account for herbivory, mostly starting 7/19/23, between Inv 5 and 6
# that also happens to be when the growth curves start to diverge ?
# (drip irrigation installed Sunday Aug 28, 2022, but got the same watering for the first summer. Would like to add layer of ppt + supplementary water)

# need to read in the long-form table of herbivory data
herbivory <- read.csv("/Users/paigekouba/Documents/UC_Davis/2021_Winter/Quals/Proposal/Chapter 1/TinyFACE/GitHub/QuailFACE/RawData/OakFACE Herbivory - Sheet1.csv")
herbivory$Date <- mdy(herbivory$Date)
# group by code, find min(date) for rows with no NA in [full / any]_herbivory
# Find all Code x date combos where date > date of first herbivory, exclude those
# Redo plots below
# will need to drop as and bs from all codes 

firstherb <- herbivory %>% # most conservative: first date any herbivory was observed
  group_by(Code) %>% 
  summarise(firstherb = min(Date)) # next, removing some that look undamaged per growth curves
  ## come back and check this!! 
 # filter(!(Code %in% c("10L6","10V4","11L7","11L5","11V1","11V2","14L4","14V3","15V1", "1V5", "4V3c", "14V1c"))) 
# add in these:
new_ones <- data.frame(c("11V5","9V3", "9V5","7L6","8L6"),c("8/25/23","8/25/23","8/25/23", "6/13/23", "8/25/23"))
colnames(new_ones) <- c("Code", "firstherb")
new_ones <- new_ones %>% 
  mutate(firstherb = mdy(firstherb))
firstherb <- rbind(firstherb, new_ones)

firstfullherb <- herbivory %>% # first date full stem herbivory was observed
  group_by(Code) %>% 
  filter(full_herb == "x") %>% 
  summarise(firstherb = min(Date))  
#  filter(!(Code %in% c("10L6","10V4","11L7","11L5","11V1","11V2","14L4","14V3","15V1","1V5"))) 
firstfullherb <- rbind(firstfullherb, new_ones)

## Above and below-ground biomass data
library(ggplot2)
library(dplyr)
# biomass_final
biomass_raw <- read.csv("/Users/paigekouba/Documents/UC_Davis/2021_Winter/Quals/Proposal/Chapter 1/TinyFACE/GitHub/QuailFACE/RawData/Biomass_final.csv")
# root_mass
rootmass_raw <- read.csv("/Users/paigekouba/Documents/UC_Davis/2021_Winter/Quals/Proposal/Chapter 1/TinyFACE/GitHub/QuailFACE/RawData/Root_mass.csv")
# remove "4V3c" to avoid duplicate code
lai_raw <- read.csv("/Users/paigekouba/Documents/UC_Davis/2021_Winter/Quals/Proposal/Chapter 1/TinyFACE/GitHub/QuailFACE/RawData/Quail_Leaf_Data.csv")
biomass_raw <- biomass_raw %>% 
  filter(Code!="4V3c") %>%  # now 384 rows
  filter(Code != "16V1a") # thinned but grew back; now 383

rootmass <- rootmass_raw %>% 
  filter(!is.na(rootmass_g)) %>% 
  filter(Code!="4V3c") %>% # remove "4V3c" to avoid duplicate code
  filter(Code != "16V1a") %>% # thinned but grew back
  mutate(Code = if_else(nchar(Code)==4,substr(Code,1,3),substr(Code,1,4))) # shortcode
# remove outlier, root 16V1 (likely thinned but roots remained)

biomass <- biomass_raw %>% 
  dplyr::select(Code, StemWet_g, LeafWet_g, LeafDry_g) %>% 
  mutate(Code = if_else(nchar(Code)==4,substr(Code,1,3),substr(Code,1,4))) %>%  # removes the a or b from the end of the seedling ID ("Code")
  group_by(Code) %>% 
  summarise_if(is.numeric, ~ max(.x, na.rm = TRUE)) %>% # group by shortened Code and collapse values in case of a/b confusion
  ungroup() %>% 
  merge(dplyr::select(rootmass, Code, Plot, Spp, SeedMass.g., Cond..7, rootmass_g), all.x=F, all.y=T)  %>% 
  mutate(across(c(StemWet_g, LeafWet_g, LeafDry_g), na_if, -Inf))

lookup <- data.frame(as.character(c(1:16)), c("AW","ED","AD","EW",
                                              "AW","ED","EW","AD",
                                              "ED","AW","EW","AD",
                                              "ED","AW","EW","AD"))
names(lookup) <- c("Plot","Tmt")

biomass <- merge(lookup, biomass, by = 'Plot')

biomass2 <- biomass %>% # prediction data with full dataset and extra inventory columns
  left_join(inventory_thinned[,c(1,7,10,13,17,22,27,32,37)]) %>% 
  mutate(across(c(StemWet_g, LeafWet_g, LeafDry_g, rootmass_g, Ht.mm..1, Ht.mm..2, Ht.mm..3, Ht.mm..4, Ht.mm..5, Ht.mm..6, Ht.mm..7, Ht.mm..8), na_if, -Inf))  %>% 
  mutate(H2OTmt = substr(Tmt,2,2))

lai <- lai_raw %>% 
  group_by(Code) %>% 
  summarise(avg_area = mean(Area), perim_per_A = mean(Perim/Area), tot_area = sum(Area), count=n()) %>% 
  mutate(Spp = substr(Code, nchar(Code)-2,nchar(Code)-2)) %>% 
  mutate(Plot = if_else(nchar(Code) == 4, substr(Code,1,1), substr(Code,1,2))) %>% 
  left_join(lookup, by = "Plot") %>% 
  mutate(Code = if_else(nchar(Code)==4,substr(Code,1,3),substr(Code,1,4))) %>% 
  left_join(biomass[,c("Code","LeafDry_g")], by = "Code") %>% 
  mutate(SLA = tot_area/LeafDry_g)

lai %>% 
 # filter(!Code%in%firstherb$Code) %>% 
 # filter(SLA != 12458.28) %>% 
ggplot() +
  geom_boxplot(aes(x=Tmt, y=SLA, group = Tmt, color= Tmt)) + # outlier.shape = NA
  scale_color_manual(values = c("pink", "lightblue", "red", "blue")) + facet_grid(rows=vars(Spp), scales="free") + geom_text(data = lai %>% filter(!Code%in%firstherb$Code) %>% group_by(Tmt, Spp)%>%tally(), aes(x = Tmt, y = 10000, label = paste0("N = ",n)))


# remove seedlings with any herbivory
biomass_nh <- biomass %>% 
  filter(!Code %in% firstherb$Code)

# remove seedlings with full herbivory
biomass_nfh <- biomass %>% 
  filter(!Code %in% firstfullherb$Code)

# will later use these to compare backfilled data (with modeled biomass for herbivory list) vs filtered data (with no herbivory list seedlings)


## Root structure
rootimage <- read.csv("/Users/paigekouba/Documents/UC_Davis/2021_Winter/Quals/Proposal/Chapter 1/TinyFACE/GitHub/QuailFACE/RawData/features_4.26.24_final.csv")
rootimage <- merge(lookup, rootimage, by = 'Plot') %>% 
  filter(Code != "16V1a") # thinned but grew back

rootimage <- rootimage %>% 
  mutate(Spp = substr(Code, nchar(Code)-2,nchar(Code)-2))
library(ggpubr)
rootimage %>% 
ggplot(aes(x=Tmt, y=Average.Diameter.mm, group=Tmt)) + geom_boxplot(outlier.shape = NA, aes(color=Tmt)) +
  scale_color_manual(values = c("pink", "lightblue", "red", "blue")) + facet_grid(rows = vars(Spp)) +
  geom_signif(test="wilcox.test", exact=FALSE, comparisons = combn(c("AD", "AW", "ED", "EW"),2,simplify=F),step_increase=0.2)

## SIF d13C data
SIF_raw <- read.csv("/Users/paigekouba/Documents/UC_Davis/2021_Winter/Quals/Proposal/Chapter 1/TinyFACE/GitHub/QuailFACE/RawData/PVK_SIF.csv")
SIF_raw <- SIF_raw %>% 
  filter(!is.na(Internal.ID))

SIF <- SIF_raw %>% 
  mutate(Spp = substr(Sample.ID, nchar(Sample.ID)-2,nchar(Sample.ID)-2)) %>% 
  mutate(Plot = if_else(nchar(Sample.ID) == 4, substr(Sample.ID,1,1), substr(Sample.ID,1,2))) %>% 
  dplyr::select(Sample.ID, δ13CVPDB...., Spp, Plot) %>% 
  filter(Sample.ID != "4V3c")
  

colnames(SIF) <- c("Code","d13C","Spp","Plot")

SIF <- left_join(SIF, lookup, by = "Plot")

nequals_SIF <- SIF %>% 
  group_by(Tmt, Spp) %>% 
  tally()

SIF %>% # d13C
  ggplot(aes(x=Tmt, y=d13C)) + geom_boxplot(aes(color=Tmt)) + facet_grid(rows = vars(Spp), scales = "free") + 
  scale_color_manual(values = c("pink", "lightblue", "red", "blue")) +
  geom_text(data = nequals_SIF, aes(x = Tmt, y = -26, label = paste0("N = ",n))) 

ggplot(SIF, aes(x=Tmt, y=d13C)) + geom_boxplot()

## Growth and mortality (Inventory)
library(lubridate)
library(RColorBrewer)
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

# adding to herbivory 
## 11V5 first herb is 8/25/23
## 9V3 first herb is 8/25/23
## 9V5 first herb is 8/25/23
## 7L6 first herb is 6/13/23
## 8L6 first herb is 8/25/23

# taking off herbivory list because their growth curves didn't look bad
# 10L6, 10V4, 11L7, 11L5, 11V1, 11V2, 14L4, 14V3, 15V1, 1V3, 1V5

# how many seedlings germinated per spp per tmt? how many were thinned? how many remained?
# need to associate "Tmt" with "Plot"
inventory_raw %>% 
  mutate(Plot = as.character(Plot)) %>% 
  left_join(lookup, by = "Plot") %>% 
  group_by(Spp, Tmt) %>% 
  summarise(germts = sum(G. == "Y"), germrate = sum(G. == "Y")*100/48, thinned = sum(Thinned. == "8/26")) %>% 
  mutate(remain = germts - thinned) 
#   Spp   Tmt   germts germrate thinned remain
# 1 "L"   AD        31    64.6       10     21
# 2 "L"   AW        29    60.4       12     17
# 3 "L"   ED        33    68.8       12     21
# 4 "L"   EW        29    60.4       10     19
# 5 "V"   AD        13    27.1        0     13
# 6 "V"   AW        20    41.7        1     19
# 7 "V"   ED        16    33.3        1     15
# 8 "V"   EW        17    35.4        0     17
# Total:           189               46    143

# I want to find the max value entered for each XYZ[a/b], since some data was mis-entered
inventory_raw %>% 
  mutate(shortcode = if_else(nchar(Code)==4,substr(Code,1,3),substr(Code,1,4))) %>% 
  group_by(shortcode) %>% 
  filter(n()>1) %>% 
  nrow() # 384 duplicated seed codes

inventory_thinned <- inventory_raw %>% # group_by shortcode and get max as summary
  mutate(shortcode = if_else(nchar(Code)==4,substr(Code,1,3),substr(Code,1,4))) %>% 
  group_by(shortcode) %>% 
  summarise_if(is.numeric, ~ max(.x, na.rm = TRUE)) # %>% # this results in 192 rows with the max of each shortcode
 # mutate(across(c(ht_mm, dia, cond), na_if, -Inf))

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


# # start with inventory date
inv_long1 <- inventory_thinned %>% 
   dplyr::select(Code, Inv..1.Date, Inv..2.Date, Inv..3.Date, Inv..4.Date, Inv..5.Date, Inv..6.Date, Inv..7.Date, Inv..8.Date) #%>% 
 #  mutate(Code = if_else(nchar(Code)==4,substr(Code,1,3),substr(Code,1,4))) # remove as and bs

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

inv_leafct <- inventory_thinned %>% 
  dplyr::select(Code, c("Leaf.Ct.",paste0("Leaf.Ct..",c(1,5,6,7)))) %>% 
  pivot_longer(!Code, names_to = "key", values_to = "leafct")

inv_maxleaf <- inventory_thinned %>% 
  dplyr::select(Code, c("Max.Leaf.Length.1",paste0("Max.Leaf.",c(5,6,7,8)))) %>% 
  pivot_longer(!Code, names_to = "key", values_to = "maxleaf")

## COME BACK TO THIS SPOT AND START FROM INV_LONG, ADD DIA AND COND

inv_leafct <- inv_leafct %>% 
  mutate(Plot = if_else(nchar(Code)==3,substr(Code,1,1),substr(Code,1,2))) %>% 
  mutate(Spp = substr(Code, nchar(Code)-1,nchar(Code)-1)) %>% 
  left_join(lookup, by = "Plot") %>% 
  mutate(across(c(leafct), na_if, -Inf))

inv_leafct %>% # this was ugly don't do it again
  ggplot() +  geom_point(aes(x=factor(key, levels = unique(key)), y=leafct, color= Tmt, )) + 
  geom_line(aes(x=factor(key, levels = unique(key)), y=leafct, color= Tmt)) +
  facet_wrap(~ Code, scales = "free") 

inv_maxleaf <- inv_maxleaf %>% 
  mutate(Plot = if_else(nchar(Code)==3,substr(Code,1,1),substr(Code,1,2))) %>% 
  mutate(Spp = substr(Code, nchar(Code)-1,nchar(Code)-1)) %>% 
  left_join(lookup, by = "Plot") %>% 
  mutate(across(c(maxleaf), na_if, -Inf))

inv_maxleaf %>% # this was ugly don't do it again
  ggplot() +  geom_point(aes(x=factor(key, levels = unique(key)), y=,maxleaf, color= Tmt, )) + 
  facet_wrap(~ Code, scales = "free") 

inv_all <- cbind(inv_long, inv_ht[,3], inv_dia[,3], inv_cond[,3]) # combine code, date, ht, dia, cond

inv_all[which.max(inv_all$dia),] # some outliers need adjusting
#    Code        Date      value ht_mm dia cond
# 776  2L1 Inv..8.Date 2023-11-20   156 389    5
inv_all[776,5] <- 3.89 # it's a decimal error
inv_all[which.max(inv_all$dia),]
#      Code        Date      value ht_mm  dia cond
# 934  3V3 Inv..6.Date 2023-08-26    96 54.9    5
inv_all[934,5] <- 2.69 # mean of Dia.mm..5 and Dia.mm..7
inv_all[which.max(inv_all$dia),]
#     Code        Date      value ht_mm  dia cond
# 1470  9L4 Inv..6.Date 2023-08-26  4.24 37.8    5
inv_all[1470,5] <- 4.24
inv_all[1470,4] <- 100.5 # mean of Ht.mm..5 and Ht.mm..7

# now add Plot and Spp, replace -Inf with NAs
inv_all <- inv_all %>% 
  mutate(Plot = if_else(nchar(Code)==3,substr(Code,1,1),substr(Code,1,2))) %>% 
  mutate(Spp = substr(Code, nchar(Code)-1,nchar(Code)-1)) %>% 
  left_join(lookup, by = "Plot") %>% 
  mutate(across(c(ht_mm, dia, cond), na_if, -Inf))

# examine conditions and see how many get to 1
inv_all %>% 
  ggplot() +  geom_point(aes(x=value, y=cond, color= Tmt)) + facet_wrap(~ Code, scales = "fixed")

# find Codes for seedlings with any herbivory and drop after the date of first herbivory
inv_all_nh0 <- right_join(firstherb, inv_all, by = "Code") 
inv_all_nfh0 <- right_join(firstfullherb, inv_all, by = "Code") 

inv_all_nh0[is.na(inv_all_nh0$firstherb),2] <- ymd("9999-09-09") # if not listed in firstherb (ie NA), assign a dummy date >> any in the surveys
inv_all_nfh0[is.na(inv_all_nfh0$firstherb),2] <- ymd("9999-09-09")

inv_all_nh <- inv_all_nh0 %>% 
  filter(firstherb > value)

inv_all_nfh <- inv_all_nfh0 %>% 
  filter(firstherb > value)

# mortality rate; needs to be a tally of the seedlings that go to 1 and do not recover
select(inventory_thinned, c(Code, paste0("Cond..",c(1:8)))) %>% View()

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

# let's look at the growth curves for this subset of seedlings

# view the growth curves (actual) for all seedlings with herbivory
inv_all[which(inv_all$Code %in% firstherb$Code),] %>% # (add ! after which( to get all without herb)
  na.omit() %>% 
  group_by(Spp, Tmt, value, Code) %>% 
  # summarise(mean_ht = mean(ht_mm, na.rm = TRUE),sd_ht = sd(ht_mm, na.rm = TRUE)) %>% 
  # ggplot(aes(x=value, y = mean_ht, group = Tmt)) + 
  # geom_pointrange(aes(ymin = mean_ht - sd_ht, ymax = mean_ht + sd_ht, color = Tmt, shape = Tmt), position = position_dodge(20, preserve = "total"), size = 1.5, alpha = 0.75) + 
  ggplot(aes(x=value, y=ht_mm,  group = Code, color = Tmt, shape = Tmt, size = 1.5, alpha = 0.75)) +
  geom_line(aes(x=value, y=ht_mm,  group = Code), position = position_dodge(20, preserve = "total"), linewidth = 1.2, na.rm = TRUE) +
  geom_point(position = position_dodge(20, preserve = "total")) +
  scale_color_manual(values = c("#e8665d", "#828cfa", "#850a01", "#010c85")) +
  scale_shape_manual(values = c(16,16,17,17)) +
   facet_grid(rows=vars(Spp), scales = "free")
# okay we have checked it out; they for sure get shorter

# after reviewing the initial plots, I found 12 instances of data entry errors and 12 seedlings in the "herbivory" subset that didn't lose enough height to matter. Made those edits above (after loading inventory data and adding dates). **I reversed this bc better safe than sorry
# I checked this against leaf herbivory: compare ht.8 vs leaf mass and look for outliers. 1V6 was funky, added to herbivory list to be safe


# Test to see if herbivory is significantly associated with Spp
# summary(glm(as.numeric(resprout) ~ rescale(CO2)*rescale(meanSWC), family = "binomial", data = filter(herb_list, Spp=="V"))) rewrite glms/glmer to test herbivory v Spp

# need a 2x2 table where the rows are (herb / no herb), cols are L / V, and cells are counts
chisq.test(matrix(c(sum(biomass$Spp == "L") - sum(biomass_nh$Spp == "L"), sum(biomass$Spp == "V") - sum(biomass_nh$Spp == "V"), 
       sum(biomass_nh$Spp == "L"), sum(biomass_nh$Spp == "V")), byrow = TRUE, nrow = 2))
# X-squared = 37.699, df = 1, p-value = 8.254e-10

# Test to see if herbivory is significantly associated with Water
chisq.test(matrix(c(sum(biomass$Tmt == "AD" | biomass$Tmt == "ED") - sum(biomass_nh$Tmt == "AD" | biomass_nh$Tmt == "ED"), sum(biomass$Tmt == "AW" | biomass$Tmt == "EW") - sum(biomass_nh$Tmt == "AW" | biomass_nh$Tmt == "EW"), 
                    sum(biomass_nh$Tmt == "AD" | biomass_nh$Tmt == "ED"), sum(biomass_nh$Tmt == "AW" | biomass_nh$Tmt == "EW")), byrow = TRUE, nrow = 2))
# X-squared = 10.025, df = 1, p-value = 0.001544

# Test to see if herbivory is significantly associated with CO2
chisq.test(matrix(c(sum(biomass$Tmt == "AD" | biomass$Tmt == "AW") - sum(biomass_nh$Tmt == "AD" | biomass_nh$Tmt == "AW"), sum(biomass$Tmt == "ED" | biomass$Tmt == "EW") - sum(biomass_nh$Tmt == "ED" | biomass_nh$Tmt == "EW"), 
                    sum(biomass_nh$Tmt == "AD" | biomass_nh$Tmt == "AW"), sum(biomass_nh$Tmt == "ED" | biomass_nh$Tmt == "EW")), byrow = TRUE, nrow = 2))
# X-squared = 0.044273, df = 1, p-value = 0.8333

# get full list of resprouters

resprout <- c("2V5", "8V2", "11V1", "11V6", "2V5", "2V6", "3V1", "3V5", "4V4", "7V6", "8V3", "10L3", "11V1","11V2","11V3","11V4","11V6", "13V2", "14V3", "14V5", "15V2", "16V3", "2V2", "2V5", "2V6", "3V1", "3V5", "4V3", "4V4", "7V5", "8V3", "10L3", "10V4", "11V1","11V2","11V3","11V4","11V6", "13V2", "14V1","14V3","14V4","14V5","14V6", "14L4", "15V2", "16V3", "2V2", "2V5", "2V6", "3V1", "3V5", "4V3", "4V4", "4V3c", "7V1","7V2","7V6", "8V2", "8V3", "10L3", "10V4", "11V1","11V2","11V3","11V4","11V6", "13V2", "14V1c","14V3","14V4","14V5","14V6", "14L4", "15V2", "16V3","1V3", "1V5", "2V5", "2V6", "3V1", "3V5", "7V1", "7V6", "7L3", "8V2", "8V3", "13V2", "14V1c", "14V3", "14V5", "14V6", "16V3")
resprout <- unique(resprout)
herb_list <- firstherb %>% 
  mutate(Spp = substr(Code, nchar(Code)-1,nchar(Code)-1)) %>% 
  left_join(biomass[,1:3], by = "Code") %>% 
  mutate(resprout = Code %in% resprout) %>% 
  mutate(H2OTmt = substr(Tmt,2,2)) %>% 
  mutate(CO2Tmt = substr(Tmt,1,1))
# out of the [herbivory] dataset (firstherb), how are resprouters distributed?
# need a 2x2 table where the rows are resprout/no resprout, columns are variables of interest

# L vs V
fisher.test(matrix(c(sum(herb_list[herb_list$Spp == "L",]$resprout), sum(herb_list[herb_list$Spp == "V",]$resprout),
       sum(abs(herb_list[herb_list$Spp == "L",]$resprout - 1)), sum(abs(herb_list[herb_list$Spp == "V",]$resprout - 1))), byrow = TRUE, nrow = 2))
# p-value = 0.09387

# Dry vs Wet
fisher.test(matrix(c(sum(herb_list[herb_list$H2OTmt == "D",]$resprout, na.rm = TRUE), sum(herb_list[herb_list$H2OTmt == "W",]$resprout, na.rm = TRUE),
               sum(abs(herb_list[herb_list$H2OTmt == "D",]$resprout - 1), na.rm = TRUE), sum(abs(herb_list[herb_list$H2OTmt == "W",]$resprout - 1), na.rm = TRUE)), byrow = TRUE, nrow = 2))
# p-value = 0.7422

# aCO2 vs eCO2
fisher.test(matrix(c(sum(herb_list[herb_list$CO2Tmt == "A",]$resprout, na.rm = TRUE), sum(herb_list[herb_list$CO2Tmt == "E",]$resprout, na.rm = TRUE),
                     sum(abs(herb_list[herb_list$CO2Tmt == "A",]$resprout - 1), na.rm = TRUE), sum(abs(herb_list[herb_list$CO2Tmt == "E",]$resprout - 1), na.rm = TRUE)), byrow = TRUE, nrow = 2))
# p-value = 0.7546

# percent resprouters
select(herb_list %>% group_by(Tmt) %>% tally(resprout), n)/table(herb_list$Tmt)
# AD 0.7142857
# AW 0.6000000
# ED 0.5714286
# EW 0.7777778

# is resprouting associated with treatments? is it associated with root mass?
herb_list <- herb_list %>% 
  left_join(plot_CO2., by = "Plot") %>% 
  left_join(plot_SWC., by = "Plot") %>% 
  left_join(select(biomass2., Code, rootmass_g), by = "Code")
  
summary(glm(as.numeric(resprout) ~ rescale(CO2)*rescale(meanSWC), family = "binomial", data = filter(herb_list, Spp=="V"))) # for Vs, marginally significant intx effect on resprouting!! p = 0.08

ggpredict(glm(as.numeric(resprout) ~ rescale(CO2)*rescale(meanSWC), family = "binomial", data = filter(herb_list, Spp=="V")), 
          terms=c("CO2","meanSWC [4,42]"))%>% plot(rawdata=T,ci=T,colors=c("red","blue"), jitter=0.07) + labs(title="resprouting, V") # what does this mean

ggpredict(glmer(as.numeric(resprout) ~ rescale(CO2)*rescale(meanSWC) +(1|Plot), family = "binomial", data = filter(herb_list, Spp=="V")), 
          terms=c("CO2","meanSWC [4,42]"))%>% plot(rawdata=T,ci=T,colors=c("red","blue"), jitter=0.07) + labs(title="resprouting, V")

ggplot(filter(herb_list, Spp=="V")) +
  geom_point(aes(x=rootmass_g, y=as.numeric(resprout), color=meanSWC)) +
  geom_smooth(mapping = aes(x=rootmass_g, y=as.numeric(resprout)), method = "glm", formula = "y ~ x")

summary(glm(as.numeric(resprout) ~ rescale(CO2)*rescale(meanSWC), family = "binomial", data = filter(herb_list, Spp=="L"))) # very few datapoints


ggpredict(glm(as.numeric(resprout) ~ rootmass_g*CO2, family = "binomial", data = filter(herb_list, Spp=="V")),
          terms=c("rootmass_g [all]","CO2 [400, 600]"))%>% plot(rawdata=T,ci=T,colors=c("red","blue"), jitter=0.07) + labs(title="resprouting, V") 
# eck

# can you get mortality rate, or longitudinal data?

mort_df <- inventory_raw %>% 
  filter(Code!="4V3c") %>%
  filter(Code != "16V1a") %>%
  filter(G. == "Y" & Thinned. != "8/26") %>%  # want ones that grew in but were not thinned
  select(Code, Spp, Plot, c(paste0("Cond..",1:8))) %>% 
  mutate(Plot = as.character(Plot)) %>% 
  rowwise() %>% 
  mutate(condmin = min(Cond..1, Cond..2, Cond..3, Cond..4, Cond..5, Cond..6, Cond..7, Cond..8, na.rm = T)) %>% 
  mutate(mortality = as.numeric(condmin == 1 & Cond..8 %in% c(1, NA))) %>%
  left_join(lookup, by = "Plot") %>% 
  left_join(plot_CO2., by = "Plot") %>% 
  left_join(plot_SWC., by = "Plot")

summary(glm(mortality ~ rescale(CO2), family = "binomial", data = mort_df)) 
summary(glmer(mortality ~ rescale(CO2) +(1|Plot), family = "binomial", data = mort_df)) 

ggpredict(glm(mortality ~ rescale(CO2), family = "binomial", data = mort_df), 
          terms=c("CO2"))%>% plot(rawdata=T,ci=T,colors=c("red","blue"), jitter=0.05) + labs(title="mortality")

ggplot(mort_df, aes(x=CO2, y=mortality)) +
  geom_point() + geom_smooth(mapping = aes(x=CO2, y=mortality), method = "glm", formula = "y ~ x")

# need a 2x2 table where the rows are (mort / no mort), cols are eCO2 / aCO2, and cells are counts
chisq.test(matrix(
  c(sum(mort_df$mortality == 1 & (mort_df$Tmt == "AD" | mort_df$Tmt == "AW")), 
    sum(mort_df$mortality == 1 & (mort_df$Tmt == "ED" | mort_df$Tmt == "EW")),
    sum(mort_df$mortality == 0 & (mort_df$Tmt == "AD" | mort_df$Tmt == "AW")), 
    sum(mort_df$mortality == 0 & (mort_df$Tmt == "ED" | mort_df$Tmt == "EW"))),   byrow = TRUE, nrow = 2))
# X-squared = 3.6779, df = 1, p-value = 0.05514

ggpredict(lmer(Root.Length.Diameter.Range.2.mm~rescale(CO2)+rescale(meanSWC)+(1|Plot), data=filter(rootimage., Spp=="L")), 
          terms=c("CO2","meanSWC [4,42]"))%>% plot(rawdata=T,ci=T,colors=c("red","blue")) + labs(title="root depth, L *")
summary(lmer(Root.Length.Diameter.Range.2.mm~rescale(CO2)+rescale(meanSWC)+(1|Plot), data=filter(rootimage., Spp=="L")))
