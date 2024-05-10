# Data Explore for Chapter 3

# for now, treating CO2 as a categorical variable; later, could get average ∆CO2 for each treatment, or even scale by position in plot (?)
# Response variables: LiCOR data, Above and below-ground biomass data, Root stx, SIF d13C, Growth/mortality


## LiCOR data (Photosynthesis)
# from A_Ci_Survey3 and A_Ci_Survey4:
View(df_all)

# need to make HHMMSS a time
df_all$HHMMSS <- parse_date_time(df_all$HHMMSS, orders = c("HMS"), tz = "America/Los_Angeles")

length(unique(df_all$ID)) # 63
length(df_all$ID) # 92
# NB some of the seedlings are resampled for df_all

# in this part I want to get the mean of the numerical variables  (Cond, SWC, interpol) for cases where there are two observations for a given ID. 
df_all <- df_all %>% 
  filter(!is.na(interpol)) %>% 
  group_by(ID) %>% 
  mutate(HHMMSS = mean(HHMMSS), Cond = mean(Cond), SWC = mean(SWC), interpol = mean(interpol)) %>% 
  ungroup() %>% 
  distinct(ID, .keep_all = TRUE) %>% # removes the duplicate rows per ID
  mutate(WUE = interpol/Cond) %>% 
  mutate(CO2Tmt = substring(Tmt,1,1), H2OTmt = substring(Tmt,2,2))

nequals_licor <- df_all %>% # here I want a count per Spp per Tmt
  filter(!is.na(interpol)) %>% 
  group_by(ID) %>% 
  mutate(HHMMSS = mean(HHMMSS), Cond = mean(Cond), SWC = mean(SWC), interpol = mean(interpol)) %>% 
  ungroup() %>% 
  distinct(ID, .keep_all = TRUE) %>% 
  mutate(WUE = interpol/Cond) %>% 
  mutate(CO2Tmt = substring(Tmt,1,1), H2OTmt = substring(Tmt,2,2)) %>% 
  group_by(Tmt, Spp) %>% 
  tally()

colnames(df_all)
# [1] "HHMMSS"    "Photo.x"   "Cond"      "Ci.x"      "CO2R"      "SWC"       "Date"      "Log"       "X."        "Time"      "ID"
# [12] "Plot"      "Treatment" "Spp"       "interpol"  "WUE"       "CO2Tmt"    "H2OTmt"  


# df_all is a combination of the mini ACi curves from two sampling efforts. Measurements of photosynthetic rate (Photo.x) are used to interpolate the photosynthetic rate at a standard Ci (328 for survey 3, 333 for survey 4—should standardize these later). Conductance and this interpolated value of Anet are used to calculate WUE. SWC was measured with a TDR probe immediately before LiCOR samples were taken for each plot. HHMMSS is probably necessary because photosynthetic rates change throughout the day. Plot as a blocking variable; separate models for Spp?

# how does time affect Anet? 
ggplot(df_all) +
  geom_point(aes(x=HHMMSS, y=interpol, color=Tmt)) + geom_line(aes(x=HHMMSS, y=interpol, group=Tmt, color = Tmt)) + scale_color_manual(values = c("pink", "lightblue", "red", "blue")) + facet_grid(rows = vars(H2OTmt))
# no evident effect of time of day

# see how they associate by plot (with per-plot eCO2 ?)
plot_order <- c("3","8","12","16","1","5","10","14","2","6","9","13","4","7","11","15")
ggplot(df_all) +
  geom_boxplot(aes(x=factor(Plot, level=plot_order), y=interpol, color=Tmt)) + facet_grid(rows = vars(Spp), scales = "free") + scale_color_manual(values = c("pink", "lightblue", "red", "blue")) 

# how does SWC look across different plots/treatments? first, average SWC measurements by Plot
df_all %>% 
  group_by(Plot, Tmt) %>% mutate(meanSWC = mean(SWC)) %>% 
  distinct(Plot, .keep_all = TRUE) %>% 
ggplot() +
  geom_boxplot(aes(x=Tmt, y=meanSWC, color = Tmt)) + geom_point(aes(x=Tmt, y=meanSWC, color = Tmt)) + scale_color_manual(values = c("pink", "lightblue", "red", "blue")) 

ggplot(df_all) +
  geom_boxplot(aes(x=Plot, y=SWC, color = Tmt)) # plot fifteen stinks but it's still >>2x SWC of highest dry plot

library(gridExtra)
grid.arrange( # breaks down Anet, gs, and WUE
  df_all %>% ggplot() + geom_boxplot(aes(x=Tmt, y= interpol, color = Tmt)) + facet_grid(rows = vars(Spp), scales = "free") + labs(y = "Standardized A(net)") +
    scale_color_manual(values = c("pink", "lightblue", "red", "blue")) ,
df_all %>% ggplot() + geom_boxplot(aes(x=Tmt, y= Cond, color = Tmt)) + facet_grid(rows = vars(Spp), scales = "free") + labs(y = "Stomatal Conductance") +
  scale_color_manual(values = c("pink", "lightblue", "red", "blue")) ,
df_all %>% ggplot() + geom_boxplot(aes(x=Tmt, y= WUE, color = Tmt)) + facet_grid(rows = vars(Spp), scales = "free") +
  scale_color_manual(values = c("pink", "lightblue", "red", "blue")) + geom_text(data = nequals_licor, aes(x = Tmt, y = 4, label = paste0("N = ",n))), nrow =1  )

# to accurately compare all the following data types, need to account for herbivory starting 7/19/23, between Inv 5 and 6
# that also happens to be when the growth curves start to diverge ?
# (drip irrigation installed Sunday Aug 28, 2022. Would like to add layer of ppt + supplementary water)

# need to read in the long-form table of herbivory data
herbivory <- read.csv("/Users/paigekouba/Documents/UC_Davis/2021_Winter/Quals/Proposal/Chapter 1/TinyFACE/GitHub/QuailFACE/RawData/OakFACE Herbivory - Sheet1.csv")
herbivory$Date <- mdy(herbivory$Date)
# group by code, find min(date) for rows with no NA in [full / any]_herbivory
# Find all Code x date combos where dat > date of first herbivory, exclude those
# Redo plots below
# will need to drop as and bs from all codes ?

firstherb <- herbivory %>% # most conservative: first date any herbivory was observed
  group_by(Code) %>% 
  summarise(firstherb = min(Date)) %>% 
  filter(!(Code %in% c("10L6","10V4","11L7","11L5","11V1","11V2","14L4","14V3","15V1","1V3","1V5","1V6"))) 
# add in these:
new_ones <- data.frame(c("11V5","9V3", "9V5","7L6","8L6"),c("8/25/23","8/25/23","8/25/23", "6/13/23", "8/25/23"))
colnames(new_ones) <- c("Code", "firstherb")
new_ones <- new_ones %>% 
  mutate(firstherb = mdy(firstherb))
firstherb <- rbind(firstherb, new_ones)

firstfullherb <- herbivory %>% # first date full stem herbivory was observed
  group_by(Code) %>% 
  filter(full_herb == "x") %>% 
  summarise(firstherb = min(Date)) %>% 
  filter(!(Code %in% c("10L6","10V4","11L7","11L5","11V1","11V2","14L4","14V3","15V1","1V3","1V5","1V6"))) 
firstfullherb <- rbind(firstfullherb, new_ones)

## Above and below-ground biomass data
library(ggplot2)
library(dplyr)
# biomass_final
biomass_raw <- read.csv("/Users/paigekouba/Documents/UC_Davis/2021_Winter/Quals/Proposal/Chapter 1/TinyFACE/GitHub/QuailFACE/RawData/Biomass_final.csv")
# root_mass
rootmass_raw <- read.csv("/Users/paigekouba/Documents/UC_Davis/2021_Winter/Quals/Proposal/Chapter 1/TinyFACE/GitHub/QuailFACE/RawData/Root_mass.csv")

rootmass <- rootmass_raw %>% 
  filter(!is.na(rootmass_g)) %>% 
  mutate(Code = if_else(nchar(biomass$Code)==4,substr(biomass$Code,1,3),substr(biomass$Code,1,4)))

biomass <- biomass_raw %>% 
  select(Code, StemWet_g, LeafWet_g, LeafDry_g) %>% 
  mutate(Code = if_else(nchar(Code)==4,substr(Code,1,3),substr(Code,1,4))) %>% # removes the a or b from the end of the seedling ID ("Code")
  group_by(Code) %>% 
  summarise_if(is.numeric, ~ max(.x, na.rm = TRUE)) %>% # group by shortened Code and collapse values in case of a/b confusion
  ungroup() %>% 
  left_join(select(rootmass, Code, Plot, Spp, SeedMass.g., Cond..7, rootmass_g), by = "Code") %>% 
  filter(!is.na(rootmass_g)) %>% 
  mutate(across(c(StemWet_g, LeafWet_g, LeafDry_g), na_if, -Inf))

lookup <- data.frame(as.character(c(1:16)), c("AW","ED","AD","EW",
                                "AW","ED","EW","AD",
                                "ED","AW","EW","AD",
                                "ED","AW","EW","AD"))
names(lookup) <- c("Plot","Tmt")

biomass <- merge(lookup, biomass, by = 'Plot')


# remove seedlings with any herbivory
biomass_nh <- biomass %>% 
  filter(!Code %in% firstherb$Code)

# remove seedlings with full herbivory
biomass_nfh <- biomass %>% 
  filter(!Code %in% firstfullherb$Code)

# I want number of observations for each box
nequals <- biomass_nfh %>% 
  group_by(Tmt, Spp) %>% 
  tally()

# total biomass grouped by Tmt, for all seedlings without FULL herbivory
biomass_nfh %>% ggplot() + geom_boxplot( aes(x=Tmt, y= (StemWet_g+LeafDry_g+rootmass_g), color = Tmt)) + facet_grid(rows = vars(Spp), scales = "free") +
  geom_text(data = nequals, aes(x = Tmt, y = 0.2, label = paste0("N = ",n))) + labs(y = "Total Biomass (g)") +
  scale_color_manual(values = c("pink", "lightblue", "red", "blue")) 

nequals_nh <- biomass_nh %>% 
  group_by(Tmt, Spp) %>% 
  tally()
# total biomass grouped by Tmt, for all seedlings without ANY herbivory
biomass_nh %>% ggplot() + geom_boxplot(aes(x = Tmt, y =(StemWet_g+LeafDry_g+rootmass_g), color = Tmt)) + facet_grid(rows = vars(Spp), scales = "free") + geom_text(data = nequals_nh, aes(x = Tmt, y = 0.2, label = paste0("N = ",n))) + labs(y = "Total Biomass (g)") + scale_color_manual(values = c("pink", "lightblue", "red", "blue")) 

# Leaf Water Content grouped by Tmt, for all seedlings without ANY herbivory
biomass_nh %>% ggplot() + geom_boxplot(aes(x = Tmt, y = (LeafWet_g - LeafDry_g)/LeafWet_g, color=Tmt)) + facet_grid(rows = vars(Spp), scales = "free") + geom_text(data = nequals_nh, aes(x = Tmt, y = 0.5, label = paste0("N = ",n))) + labs(y = "Leaf Water Content") +
  scale_color_manual(values = c("pink", "lightblue", "red", "blue")) 

nequals_bm <- biomass %>% 
  group_by(Tmt, Spp) %>% 
  tally()
biomass %>% # root mass for all seedlings (no need to filter on herbivory)
  ggplot() + geom_boxplot(aes(x = Tmt, y = rootmass_g, color=Tmt)) + facet_grid(rows = vars(Spp), scales = "free") + geom_text(data = nequals_bm, aes(x = Tmt, y = 0.5, label = paste0("N = ",n))) + labs(y = "Root mass (g)") +
  scale_color_manual(values = c("pink", "lightblue", "red", "blue")) 

biomass_nh %>% # root:shoot ratio for seedlings without any herbivory
  ggplot() + geom_boxplot(aes(x=Tmt, y= (rootmass_g/(StemWet_g+LeafDry_g)), color = Tmt)) + facet_grid(rows =  vars(Spp), scales = "free") +
  geom_text(data = nequals_nh, aes(x = Tmt, y = 2.5, label = paste0("N = ",n))) + labs(y = "Root:Shoot") +
  scale_color_manual(values = c("pink", "lightblue", "red", "blue")) 
# seems like: drought stressed plants invest more in roots, and (V?) eCO2 lets them double down

# using inventory, biomass and herbivory dataframes, calculate the projected final biomass for herbivorized seedlings, based on growth curve and/or size at Inventory 6. (See below)

# use this to re-run biomass plots and see how much they changed

# decide which dataset to use for analysis


## Root structure
rootimage <- read.csv("/Users/paigekouba/Documents/UC_Davis/2021_Winter/Quals/Proposal/Chapter 1/TinyFACE/GitHub/QuailFACE/RawData/features_4.26.24_final.csv")
rootimage <- merge(lookup, rootimage, by = 'Plot')

rootimage <- rootimage %>% 
  mutate(Spp = substr(Code, nchar(Code)-2,nchar(Code)-2))

rootimage %>% 
ggplot(aes(x=Tmt, y=Root.Length.Diameter.Range.1.mm/Root.Length.Diameter.Range.2.mm)) + geom_boxplot(outlier.shape = NA) + scale_y_continuous(limits = c(0,6)) + facet_grid(rows = vars(Spp))

## SIF d13C data

SIF_raw <- read.csv("/Users/paigekouba/Documents/UC_Davis/2021_Winter/Quals/Proposal/Chapter 1/TinyFACE/GitHub/QuailFACE/RawData/PVK_SIF.csv")
SIF_raw <- SIF_raw %>% 
  filter(!is.na(Internal.ID))

SIF <- SIF_raw %>% 
  mutate(Spp = substr(Sample.ID, nchar(Sample.ID)-2,nchar(Sample.ID)-2)) %>% 
  mutate(Plot = if_else(nchar(Sample.ID) == 4, substr(Sample.ID,1,1), substr(Sample.ID,1,2))) %>% 
  select(Sample.ID, δ13CVPDB...., Spp, Plot)

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

# adding to herbivory 
## 11V5 first herb is 8/25/23
## 9V3 first herb is 8/25/23
## 9V5 first herb is 8/25/23
## 7L6 first herb is 6/13/23
## 8L6 first herb is 8/25/23

# taking off herbivory list because their growth curves didn't look bad
# 10L6, 10V4, 11L7, 11L5, 11V1, 11V2, 14L4, 14V3, 15V1, 1V3, 1V5, 1V6

# how many seedlings germinated per spp per tmt? how many were thinned? how many remained?
# need to associate "Tmt" with "Plot"
inventory_raw %>% 
  mutate(Plot = as.character(Plot)) %>% 
  left_join(lookup, by = "Plot") %>% 
  group_by(Spp, Tmt) %>% 
  summarise(germts = sum(G. == "Y"), germrate = sum(G. == "Y")*100/48, thinned = sum(Thinned. == "8/26")) %>% 
  mutate(remain = germts - thinned) 
#   Spp   Tmt   germts germrate thinned remain
# 1 ""    NA         1    2.08        0      1    <-- this one was the "blue oak"
# 2 "L"   AD        31    64.6       10     21
# 3 "L"   AW        29    60.4       12     17
# 4 "L"   ED        33    68.8       12     21
# 5 "L"   EW        29    60.4       10     19
# 6 "V"   AD        13    27.1        0     13
# 7 "V"   AW        20    41.7        1     19
# 8 "V"   ED        16    33.3        1     15
# 9 "V"   EW        17    35.4        0     17
# Total:           189               46    143


# first drop out all the thinned ones
# actually try it without this; just filter on Spp
inventory_thinned <- inventory_raw %>% 
 # filter(Thinned. != "8/26") %>% 
  filter(Spp != "") 

# 385 to 143 if you take out thinned and non-germinants; but want to have those rows open in case of mis-entered data

# I want to find the max value entered for each XYZ[a/b], since some data was mis-entered
inventory_thinned %>% 
  mutate(shortcode = if_else(nchar(Code)==4,substr(Code,1,3),substr(Code,1,4))) %>% 
  group_by(shortcode) %>% 
  filter(n()>1) %>% 
  nrow() # 292 duplicated seed codes (384 if you don't filter thinned ones or non-germs ?)

inventory_thinned <- inventory_thinned %>% # try to group_by shortcode and get max as summary
  mutate(shortcode = if_else(nchar(Code)==4,substr(Code,1,3),substr(Code,1,4))) %>% 
  group_by(shortcode) %>% 
  summarise_if(is.numeric, ~ max(.x, na.rm = TRUE)) %>% # this results in 192 rows with the max of each shortcode
  mutate(across(c(ht_mm, dia, cond), na_if, -Inf))

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
   select(Code, Inv..1.Date, Inv..2.Date, Inv..3.Date, Inv..4.Date, Inv..5.Date, Inv..6.Date, Inv..7.Date, Inv..8.Date) #%>% 
 #  mutate(Code = if_else(nchar(Code)==4,substr(Code,1,3),substr(Code,1,4))) # remove as and bs

inv_long <- inv_long1 %>%
  pivot_longer(!Code, names_to = "Date") %>%
  mutate(value = mdy(value))

# add in the reorganized ht, condition, and dia values
inv_ht <- inventory_thinned %>% 
  select(Code, paste0("Ht.mm..",c(1:8))) %>% 
  pivot_longer(!Code, names_to = "key", values_to = "ht_mm")

inv_cond <- inventory_thinned %>% 
  select(Code, paste0("Cond..",c(1:8))) %>% 
  pivot_longer(!Code, names_to = "key", values_to = "cond")

inv_dia <- inventory_thinned %>% 
  select(Code, paste0("Dia.mm..",c(1:8))) %>% 
  pivot_longer(!Code, names_to = "key", values_to = "dia")

## COME BACK TO THIS SPOT AND START FROM INV_LONG, ADD DIA AND COND
inv_all <- cbind(inv_long, inv_ht[,3], inv_dia[,3], inv_cond[,3])

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

# now add Plot and Spp
inv_all <- inv_all %>% 
  mutate(Plot = if_else(nchar(Code)==3,substr(Code,1,1),substr(Code,1,2))) %>% 
  mutate(Spp = substr(Code, nchar(Code)-1,nchar(Code)-1)) %>% 
  left_join(lookup, by = "Plot") %>% 
  mutate(across(c(ht_mm, dia, cond), na_if, -Inf))

# examine conditions and see how many get to 1
inv_all %>% 
 # filter(Code == "16V4") %>% 
  ggplot() +  geom_point(aes(x=value, y=cond, color= Tmt)) + facet_wrap(~ Code, scales = "fixed")

# filter for herbivory
inv_all_nh0 <- right_join(firstherb, inv_all, by = "Code") 
inv_all_nfh0 <- right_join(firstfullherb, inv_all, by = "Code") 

inv_all_nh0[is.na(inv_all_nh0$firstherb),2] <- ymd("9999-09-09")
inv_all_nfh0[is.na(inv_all_nfh0$firstherb),2] <- ymd("9999-09-09")

inv_all_nh <- inv_all_nh0 %>% 
  filter(firstherb > value)

inv_all_nfh <- inv_all_nfh0 %>% 
  filter(firstherb > value)

# mortality rate; needs to be a tally of the seedlings that go to 1 and do not recover
select(inventory_thinned, c(Code, paste0("Cond..",c(1:8)))) %>% View()

# find Codes for seedlings with any herbivory and drop after the date of first  herbivory
 
inv_nequals <- inv_all_nfh %>%
  filter(!is.na(dia)) %>% 
  group_by(Spp, Tmt, value) %>%
  tally()

inv_all_nfh %>% 
  group_by(Spp, Tmt, value) %>% 
  summarise(mean_ht = mean(ht_mm, na.rm = TRUE),sd_ht = sd(ht_mm, na.rm = TRUE)) %>% 
 # summarise(mean_dia = mean(dia, na.rm = TRUE),sd_dia = sd(dia, na.rm = TRUE)) %>% 
 # summarise(mean_cond = mean(cond, na.rm = TRUE),sd_cond = sd(cond, na.rm = TRUE)) %>% 
  ggplot(aes(x=value, y = mean_ht, group = Tmt)) + 
  geom_line(aes(color = Tmt), position = position_dodge(20, preserve = "total"), linewidth = 1.2) +
  geom_pointrange(aes(ymin = mean_ht - sd_ht, ymax = mean_ht + sd_ht, color = Tmt, shape = Tmt), position = position_dodge(20, preserve = "total"), size = 1.5, alpha = 0.75) + 
  # geom_pointrange(aes(ymin = mean_dia - sd_dia, ymax = mean_dia + sd_dia, color = Tmt, shape = Tmt), position = position_dodge(20, preserve = "total"), size = 1.5, alpha = 0.75) + 
  # geom_pointrange(aes(ymin = mean_cond - sd_cond, ymax = mean_cond + sd_cond, color = Tmt, shape = Tmt), position = position_dodge(20, preserve = "total"), size = 1.5, alpha = 0.75) + 
  scale_color_manual(values = c("#e8665d", "#828cfa", "#850a01", "#010c85")) +
  scale_shape_manual(values = c(16,16,17,17)) +
  geom_text(data = inv_nequals, aes(x = value, y = as.numeric(as.factor(Tmt))*12-50, color = Tmt, label = paste0("N=",n))) +
   facet_grid(~ Spp)

## Back-filling biomass data
# will make a new df called biomass_pred, which has observed values for all the non-herbivory seedlings, but predicted values for the 47 eaten
sum(biomass$Code %in% herb_list$Code) # 47
View(biomass[which(biomass$Code %in% herb_list$Code),])
# let's look at the growth curves for this subset of seedlings

# view the growth curves (actual) for all seedlings with herbivory
inv_all[which(!inv_all$Code %in% firstherb$Code),] %>% # (add ! after which( to get all without herb)
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

# there is a rogue point in the AW group, survey 7 height; around 20mm but then ~400 on either side. Find it
inv_all[which(inv_all$Code %in% firstherb$Code),] %>% 
#inv_all_nh[which(inv_all_nh$Code %in% firstherb$Code),] %>% 
  na.omit() %>% 
  group_by(Spp, Tmt, value) %>% 
  ggplot(aes(x=value, y=ht_mm,  group = Code, color = Tmt, shape = Tmt, size = 1.5, alpha = 0.75)) +
  geom_line(aes(x=value, y=ht_mm,  group = Code), position = position_dodge(20, preserve = "total"), linewidth = 1.2, na.rm = TRUE) +
  geom_point(position = position_dodge(20, preserve = "total")) +
  scale_color_manual(values = c("#e8665d", "#828cfa", "#850a01", "#010c85")) +
  scale_shape_manual(values = c(16,16,17,17)) +
  facet_wrap(~ Code, scales = "free")

# okay we have checked it out; they for sure get shorter
# I found 12 instances of data entry errors and 12 seedlings in the "herbivory" subset that didn't lose enough height to matter
# I will go up to the inventory_raw step and make those adjustments. Done.

# START HERE TOMORROW: redo stuff below to get best guess at herbivory seedlings final resting place 

# I will write a model to see whether height and dia in inventory 6 predict biomass for the non-herbivory plants
# need to join inventory hts and biomass columns with biomass data, keeping all/only rows in the biomass dataset

biomass_nh2 <- biomass_nh %>% 
  left_join(inventory_thinned[,c(1,7,10,13,17,22,27,32,37)]) %>% 
  mutate(across(c(StemWet_g, LeafWet_g, LeafDry_g, rootmass_g, Ht.mm..1, Ht.mm..2, Ht.mm..3, Ht.mm..4, Ht.mm..5, Ht.mm..6, Ht.mm..7, Ht.mm..8), na_if, -Inf)) 

# models to see how well hts predict final biomass
summary(lm(StemWet_g ~ Ht.mm..1+Ht.mm..2+Ht.mm..3+Ht.mm..4+Ht.mm..5+Ht.mm..6+Ht.mm..7+Ht.mm..8, data = filter(biomass_nh2, Spp == "L" & Tmt == "AW" | Tmt == "EW")))
summary(lm(StemWet_g ~ Ht.mm..1+Ht.mm..2+Ht.mm..3+Ht.mm..4+Ht.mm..5, data = filter(biomass_nh2, Spp == "L" & Tmt == "AD" | Tmt == "ED")))
summary(lm(LeafWet_g ~ polym(Ht.mm..1,Ht.mm..4,Ht.mm..5, degree = 2, raw = TRUE), data = filter(biomass_nh2, Spp == "L" & Tmt == "AD" | Tmt == "ED")))

lm_VW <- lm(StemWet_g ~ polym(Ht.mm..1,Ht.mm..4,Ht.mm..5, degree = 2, raw = TRUE), data = filter(biomass_nh2, Spp == "V" & Tmt == "AW" | Tmt == "EW"))
lm_VD <- lm(StemWet_g ~ polym(Ht.mm..1,Ht.mm..4,Ht.mm..5, degree = 2, raw = TRUE), data = filter(biomass_nh2, Spp == "V" & Tmt == "AD" | Tmt == "ED"))
lm_LW <- lm(StemWet_g ~ polym(Ht.mm..1,Ht.mm..4,Ht.mm..5, degree = 2, raw = TRUE), data = filter(biomass_nh2, Spp == "L" & Tmt == "AW" | Tmt == "EW"))
lm_LD <- lm(StemWet_g ~ polym(Ht.mm..1,Ht.mm..4,Ht.mm..5, degree = 2, raw = TRUE), data = filter(biomass_nh2, Spp == "L" & Tmt == "AD" | Tmt == "ED"))
# now I need to use these to predict the final StemWet_g 
lm_all <- lm(StemWet_g ~ polym(Ht.mm..1,Ht.mm..4,Ht.mm..5, degree = 2, raw = TRUE), data = biomass_nh2) # test

biomass_nh2$pred1 <- predict(lm_all, newdata = biomass_nh2)
biomass_nh2$pred2 <- predict(lm_LD, newdata = biomass_nh2)
biomass_nh2$pred3 <- predict(lm_VD, newdata = biomass_nh2)

ggplot(biomass_nh2, aes(x = StemWet_g, y=pred1)) + # plot
  geom_point() 

ggplot(biomass_nh2, aes(x=Ht.mm..8, y = StemWet_g)) + # plot
  geom_point(biomass_nh2, mapping= aes(x = Ht.mm..8, y=pred1)) +
  #  geom_point(filter(biomass_nh2, Spp == "L" & Tmt == "AD" | Tmt == "ED"), mapping= aes(x = Ht.mm..8, y=pred2)) +
  #geom_point(filter(biomass_nh2, Spp == "V" & Tmt == "AD" | Tmt == "ED"), mapping= aes(x = Ht.mm..8, y=pred3)) +
  geom_point(biomass_nh2, mapping= aes(x= Ht.mm..8, y = StemWet_g, group = Tmt, color = Tmt)) + facet_grid( ~ Spp)



# now add a column to biomass_nh2 that is the slope (increase in mm per days its been) since ht 4 (54 days between 4/21 and 6/14)
# use slope[4-5] and ht[5] to model [6,7,8] and [final biomass]
biomass_nh2 <- biomass_nh2 %>% 
  mutate(slope4_5 = (Ht.mm..5 - Ht.mm..4)/54)

summary(lm(Ht.mm..6 ~ Ht.mm..5, data = biomass_nh2)) # R2 = 0.5126
summary(lm(Ht.mm..7 ~ Ht.mm..5, data = biomass_nh2)) # R2 = 0.4555
summary(lm(Ht.mm..8 ~ Ht.mm..5, data = biomass_nh2)) # R2 = 0.4381
summary(lm((StemWet_g) ~ Ht.mm..5, data = biomass_nh2)) # R2 = 0.3913

# Hmm. Let us try a linear extrapolation from the time series data in the style of stack overflow:
# first with one that has all 8 values and no herb
extpTest_df <- data.frame(unique(inv_all_nh$value), inv_all[which(inv_all$Code == "14L1"),]$ht_mm) # prep df
colnames(extpTest_df) <- c("Date","ht_mm")
extpTest_df$pred1 <- predict(lm(ht_mm ~ poly(Date,3), data=extpTest_df)) # add predicted values

pred <- data.frame(Date = c(extpTest_df$Date, ymd("2023-12-20"),ymd("2024-01-20"),ymd("2024-02-20")))
pred$ht_mm <- predict(lm(ht_mm ~ poly(Date,3), data = extpTest_df), newdata=pred)

ggplot(extpTest_df, aes(x = Date, y=ht_mm)) + # plot
  geom_line() +
  geom_point() +
  geom_hline(aes(yintercept=0)) +
  geom_line(aes(y = pred1), color = "red") +
  geom_point(color="blue", data=pred)

# 3rd degree is best for prediction past November
# now for one that has herb, so only 5 values; or you could fit the average curve for the whole nh set?
extpTest_df2 <- data.frame(unique(inv_all_nh[which(inv_all_nh$Code == "11V3"),]$value), inv_all_nh[which(inv_all_nh$Code == "11V3"),]$ht_mm) # prep df
colnames(extpTest_df2) <- c("Date","ht_mm")
extpTest_df2$pred1 <- predict(lm(ht_mm ~ poly(Date,3), data=extpTest_df2)) # add predicted values

pred2 <- data.frame(Date = c(unique(inv_all_nh$value)))
pred2$ht_mm <- predict(lm(ht_mm ~ poly(Date,3), data = extpTest_df2), newdata=pred2)

ggplot(extpTest_df2, aes(x = Date, y=ht_mm)) + # plot
  geom_line() +
  geom_point() +
  geom_hline(aes(yintercept=0)) +
  geom_point(color="blue", shape=15, data=pred2) +
  geom_line(color="blue", data=pred2)

# Test to see if herbivory is significantly associated with Spp
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

# can you get mortality rate, or longitudinal data?

# for fun now go redo the ht_mm graph with dia_mm
