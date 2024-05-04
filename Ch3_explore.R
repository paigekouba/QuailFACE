# Data Explore for Chapter 3

# for now, treating CO2 as a categorical variable; later, could get average ∆CO2 for each treatment, or even scale by position in plot (?)
# Response variables: LiCOR data, Above and below-ground biomass data, Root stx, SIF d13C, Growth/mortality


## LiCOR data (Photosynthesis)
# from A_Ci_Survey3 and A_Ci_Survey4:
View(df_all)
# need to separate A v E and W v D
df_all$CO2Tmt <- substring(df_all$Treatment, 1,1)
df_all$H2OTmt <- substring(df_all$Treatment, 2,2)

# need to make HHMMSS a time
df_all$HHMMSS <- parse_date_time(df_all$HHMMSS, orders = c("HMS"), tz = "America/Los_Angeles")

df_all <- df_all %>% 
  filter(!is.na(interpol)) %>% 
  mutate(WUE = interpol/Cond)
colnames(df_all)
# [1] "HHMMSS"    "Photo.x"   "Cond"      "Ci.x"      "CO2R"      "SWC"       "Date"      "Log"       "X."    
# [10] "Time"      "ID"        "Plot"      "Treatment" "Spp"       "interpol"  "CO2Tmt"    "H2OTmt"    "WUE"  

# df_all is a combination of the mini ACi curves from two sampling efforts. Measurements of photosynthetic rate (Photo.x) are used to interpolate the photosynthetic rate at a standard Ci (328 for survey 3, 333 for survey 4—should standardize these later). Conductance and this interpolated value of Anet are used to calculate WUE. SWC was measured with a TDR probe immediately before LiCOR samples were taken for each plot. HHMMSS is probably necessary because photosynthetic rates change throughout the day. Plot as a blocking variable; separate models for Spp?
library(gridExtra)
grid.arrange(
  df_all %>% ggplot() + geom_boxplot(aes(x=Treatment, y= interpol, color = Treatment)) + facet_grid(~ Spp) + labs(y = "Standardized A(net)") +
    scale_color_manual(values = c("pink", "lightblue", "red", "blue")) ,
df_all %>% ggplot() + geom_boxplot(aes(x=Treatment, y= Cond, color = Treatment)) + facet_grid(~ Spp) + labs(y = "Stomatal Conductance") +
  scale_color_manual(values = c("pink", "lightblue", "red", "blue")) ,
df_all %>% ggplot() + geom_boxplot(aes(x=Treatment, y= WUE, color = Treatment)) + facet_grid(~ Spp) +
  scale_color_manual(values = c("pink", "lightblue", "red", "blue")) )

# to accurately compare all the following data types, need to account for herbivory starting 7/19/23, between Inv 5 and 6
# that also happens to be when the growth curves start to diverge ?
# (drip irrigation installed Sunday Aug 28, 2022)

# need to read in the long-form table of herbivory data
herbivory <- read.csv("/Users/paigekouba/Documents/UC_Davis/2021_Winter/Quals/Proposal/Chapter 1/TinyFACE/GitHub/QuailFACE/RawData/OakFACE Herbivory - Sheet1.csv")
herbivory$Date <- mdy(herbivory$Date)
# group by code, find min(date) for rows with no NA in [full / any]_herbivory
# Find all Code x date combos where dat > date of first herbivory, exclude those
# Redo plots below
# will need to drop as and bs from all codes ?

firstherb <- herbivory %>% # most conservative: first date any herbivory was observed
  group_by(Code) %>% 
  summarise(firstherb = min(Date))

firstfullherb <- herbivory %>% # first date full stem herbivory was observed
  group_by(Code) %>% 
  filter(full_herb == "x") %>% 
  summarise(firstherb = min(Date))

## Above and below-ground biomass data
library(ggplot2)
library(dplyr)
# biomass_final
biomass_raw <- read.csv("/Users/paigekouba/Documents/UC_Davis/2021_Winter/Quals/Proposal/Chapter 1/TinyFACE/GitHub/QuailFACE/RawData/Biomass_final.csv")
# root_mass
rootmass_raw <- read.csv("/Users/paigekouba/Documents/UC_Davis/2021_Winter/Quals/Proposal/Chapter 1/TinyFACE/GitHub/QuailFACE/RawData/Root_mass.csv")

rootmass <- rootmass_raw %>% 
  filter(!is.na(rootmass_g))

biomass <- biomass_raw %>% 
  select(Code, StemWet_g, LeafWet_g, LeafDry_g) %>% 
  left_join(select(rootmass, Code, Plot, Spp, SeedMass.g., Cond..7, rootmass_g), by = "Code") %>% 
  filter(!is.na(rootmass_g))

lookup <- data.frame(as.character(c(1:16)), c("AW","ED","AD","EW",
                                "AW","ED","EW","AD",
                                "ED","AW","EW","AD",
                                "ED","AW","EW","AD"))
names(lookup) <- c("Plot","Tmt")

biomass <- merge(lookup, biomass, by = 'Plot')
biomass$Code <- if_else(nchar(biomass$Code)==4,substr(biomass$Code,1,3),substr(biomass$Code,1,4))
# removes the a or b from the end of the seedling ID ("Code")

# remove seedlings with any herbivory
biomass_nh <- biomass %>% 
  filter(!Code %in% firstherb$Code)

# remove seedlings with full herbivory
biomass_nfh <- biomass %>% 
  filter(!Code %in% firstfullherb$Code)

# total biomass grouped by Tmt, for all seedlings without FULL herbivory
biomass_nfh %>% ggplot() + geom_boxplot( aes(x=Tmt, y= log(StemWet_g+LeafDry_g+rootmass_g), color = Tmt)) + facet_grid(~ Spp) +
  geom_text(data = nequals, aes(x = Tmt, y = 4, label = paste0("N = ",n))) + labs(y = "Total Biomass (g)") +
  scale_color_manual(values = c("pink", "lightblue", "red", "blue")) 
# I want number of observations for each box
nequals <- biomass_nfh %>% 
  group_by(Tmt, Spp) %>% 
  tally()

# biomass %>% ggplot() + geom_boxplot( aes(x=Tmt, y= log(StemWet_g+LeafDry_g+rootmass_g), color = Tmt)) + facet_grid(~ Spp) +
#   geom_text(data = nequals, aes(x = Tmt, y = 4, label = paste0("N =",n)))
# # I want number of observations for each box
# nequals <- biomass %>% 
#   group_by(Tmt, Spp) %>% 
#   tally()

ggplot() + geom_boxplot(data = biomass, aes(x=as.factor(Plot), y= LeafWet_g, color = Tmt)) + facet_grid(~ Spp)

biomass_nfh %>% ggplot() + geom_boxplot(aes(x=Tmt, y= log(rootmass_g/(StemWet_g+LeafDry_g)), color = Tmt)) + facet_grid(~ Spp) +
  geom_text(data = nequals, aes(x = Tmt, y = 2.5, label = paste0("N = ",n))) + labs(y = "Log(Root:Shoot)") +
  scale_color_manual(values = c("pink", "lightblue", "red", "blue")) 


ggplot() + geom_boxplot(data = biomass_nfh, aes(x=Tmt, y= StemWet_g))
ggplot() + geom_boxplot(data = biomass_nfh, aes(x=Tmt, y= LeafWet_g))
ggplot() + geom_boxplot(data = biomass_nfh, aes(x=Tmt, y= rootmass_g))


## Root structure

rootimage <- read.csv("/Users/paigekouba/Documents/UC_Davis/2021_Winter/Quals/Proposal/Chapter 1/TinyFACE/GitHub/QuailFACE/RawData/features_4.26.24_final.csv")
rootimage <- merge(lookup, rootimage, by = 'Plot')

rootimage <- rootimage %>% 
  mutate(Spp = substr(Code, nchar(Code)-2,nchar(Code)-2))

rootimage %>% 
ggplot(aes(x=Tmt, y=Number.of.Root.Tips)) + geom_boxplot() + facet_grid(~Spp)


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

SIF %>% 
  ggplot(aes(x=Tmt, y=d13C)) + geom_boxplot(aes(color=Tmt)) + facet_grid(~Spp) + 
  scale_color_manual(values = c("pink", "lightblue", "red", "blue")) 

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

# want a long format df with columns =  [ Code, Plot, Tmt, Date, Ht ] x 129 
# then, plot x = date y = ht, group_by Code, color = Tmt

# try it just with inventory date
inv_long1 <- inventory_raw %>% 
  select(Code, Inv..1.Date, Inv..2.Date, Inv..3.Date, Inv..4.Date, Inv..5.Date, Inv..6.Date, Inv..7.Date, Inv..8.Date) %>% 
  mutate(Code = if_else(nchar(Code)==4,substr(Code,1,3),substr(Code,1,4)))

inv_long <- inv_long1 %>% 
  pivot_longer(!Code, names_to = "Date") %>% 
  mutate(value = mdy(value)) 

# now do it with ht data
inv_hts <- inventory_raw %>% 
  select(Code, paste0("Ht.mm..",c(1:8))) %>% 
  pivot_longer(!Code, names_to = "key", values_to = "ht_mm")

inv_long <- cbind(inv_long, inv_hts) %>%
  select(!4) %>% 
  mutate(Plot = if_else(nchar(Code)==3,substr(Code,1,1),substr(Code,1,2)))

inv_long <- left_join(inv_long, lookup, by = "Plot")

inv_long <- inv_long %>% 
  mutate(Spp = substr(Code, nchar(Code)-1,nchar(Code)-1))

ggplot(inv_long) +
  geom_point(aes(x = value, y = ht_mm, group = Code, color = Spp)) +
  geom_line(aes(x = value, y = ht_mm, group = Code, color = Spp)) + facet_grid(Spp ~ Tmt, scales = "free")

inv_cond <- inventory_raw %>% 
  select(Code, paste0("Cond..",c(1:8))) %>% 
  pivot_longer(!Code, names_to = "key", values_to = "cond")

cond_long <- cbind(inv_long, inv_cond) 

cond_long <- cond_long[,c(1:3,5:8,11)]

cond_long %>% 
  filter(Spp == "V") %>% 
ggplot() +
  geom_point(aes(x = value, y = cond, color = Tmt)) +
  geom_line(aes(x = value, y = cond, color = Tmt)) + facet_wrap( ~ Code)  


inv_long %>% 
  group_by(Spp, Tmt, value) %>% 
  summarise(mean_ht = mean(ht_mm, na.rm = TRUE),sd_ht = sd(ht_mm, na.rm = TRUE)) %>% 
  ggplot(aes(x=value, y = mean_ht, group = Tmt)) + 
  geom_pointrange(aes(ymin = mean_ht - sd_ht/7, ymax = mean_ht + sd_ht/7, color = Tmt, shape = Tmt), position = position_dodge(0.2), size = 1.5, alpha = 0.75) + 
  scale_color_manual(values = c("red", "blue", "red", "blue")) +
  scale_shape_manual(values = c(16,16,17,17)) +
  geom_line(aes(color = Tmt), position = position_dodge(0.2), linewidth = 1.2) + facet_grid(~Spp)

inv_long %>% 
  group_by(Spp, Tmt, Date) %>%
  tally() %>% View()

# for inv_long, (get Codes w/o a and b), find Codes for seedlings with full herbivory and drop after the date of first full herbivory


inv_long_nh0 <- right_join(firstherb, inv_long, by = "Code") 
inv_long_nfh0 <- right_join(firstfullherb, inv_long, by = "Code") 

sum(is.na(inv_long_nh0$firstherb)) # 2304
nrow(inv_long_nh0[is.na(inv_long_nh0$firstherb),2]) # 2304
inv_long_nh0[is.na(inv_long_nh0$firstherb),2] <- ymd("9999-09-09")
inv_long_nfh0[is.na(inv_long_nfh0$firstherb),2] <- ymd("9999-09-09")

inv_long_nh <- inv_long_nh0 %>% 
  filter(firstherb > value)

inv_long_nfh <- inv_long_nfh0 %>% 
  filter(firstherb > value)

# find Codes for seedlings with any herbivory and drop after the date of first  herbivory
 
inv_long_nfh %>% 
  group_by(Spp, Tmt, value) %>% 
  summarise(mean_ht = mean(ht_mm, na.rm = TRUE),sd_ht = sd(ht_mm, na.rm = TRUE)) %>% 
  ggplot(aes(x=value, y = mean_ht, group = Tmt)) + 
  geom_pointrange(aes(ymin = mean_ht - sd_ht/7, ymax = mean_ht + sd_ht/7, color = Tmt, shape = Tmt), position = position_dodge(0.2), size = 1.5, alpha = 0.75) + 
  scale_color_manual(values = c("pink", "lightblue", "red", "blue")) +
  scale_shape_manual(values = c(16,16,17,17)) +
  geom_line(aes(color = Tmt), position = position_dodge(0.2), linewidth = 1.2) + facet_grid(~Spp)


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
# need a 2x2 tabble where the rows are resprout/no resprout, columns are variables of interest

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

