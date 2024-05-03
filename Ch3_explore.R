# Data Explore for Chapter 3

# for now, treating CO2 as a categorical variable; later, could get average ∆CO2 for each treatment, or even scale by position in plot (?)

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

# to accurately compare all the following data types, need to account for herbivory starting 7/19/23, between Inv 5 and 6
# that also happens to be when the growth curves start to diverge ?
# (drip irrigation installed Sunday Aug 28, 2022)

# need to read in the long-form table of herbivory data
herbivory <- read.csv("/Users/paigekouba/Documents/UC_Davis/2021_Winter/Quals/Proposal/Chapter 1/TinyFACE/GitHub/QuailFACE/RawData/OakFACE Herbivory - Sheet1.csv")
# group by code, find min(date) for rows with no NA in [full / any]_herbivory
# Find all Code x date combos where dat > date of first herbivory, exclude those
# Redo plots below
# will need to drop as and bs from all codes ?

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



ggplot() + geom_boxplot(data = biomass, aes(x=as.factor(Plot), y= log(StemWet_g+LeafDry_g+rootmass_g), color = Tmt)) + facet_grid(~ Spp)
biomass %>% ggplot() + geom_boxplot( aes(x=Tmt, y= log(StemWet_g+LeafDry_g+rootmass_g), color = Tmt)) + facet_grid(~ Spp)

ggplot() + geom_boxplot(data = biomass, aes(x=as.factor(Plot), y= LeafWet_g, color = Tmt)) + facet_grid(~ Spp)

ggplot() + geom_boxplot(data = rootmass, aes(x=as.factor(Plot), y= rootmass_g)) + facet_grid(~ Spp)
biomass %>% ggplot() + geom_boxplot(aes(x=Tmt, y= log(rootmass_g/(StemWet_g+LeafDry_g)), color = Tmt)) + facet_grid(~ Spp)


ggplot() + geom_boxplot(data = biomass, aes(x=Tmt, y= StemWet_g))
ggplot() + geom_boxplot(data = biomass, aes(x=Tmt, y= LeafWet_g))
ggplot() + geom_boxplot(data = biomass, aes(x=Tmt, y= rootmass_g))

biomass  %>% ggplot() + 
  geom_point(aes(x=log(LeafWet_g+StemWet_g), y=log(rootmass_g), group = Tmt, shape = Spp, color=Tmt, size = 2)) + facet_grid(~ Spp)
 # geom_smooth(aes(x=log(LeafWet_g+StemWet_g), y=log(rootmass_g), group = Tmt))

ggplot(biomass, aes(x=LeafWet_g+StemWet_g, y=(rootmass_g), group = Tmt, shape = Spp)) + geom_point(aes(color=Tmt, size = 2))
ggplot(biomass, aes(x=Tmt, y=log(rootmass_g/(LeafWet_g+StemWet_g)))) + geom_boxplot() + facet_grid(~ Spp)


## Root structure

rootimage <- read.csv("/Users/paigekouba/Documents/UC_Davis/2021_Winter/Quals/Proposal/Chapter 1/TinyFACE/GitHub/QuailFACE/RawData/features_4.26.24_final.csv")
rootimage <- merge(lookup, rootimage, by = 'Plot')

rootimage <- rootimage %>% 
  mutate(Spp = substr(Code, nchar(Code)-2,nchar(Code)-2))

biomass %>% 
  filter(Spp == "V") %>% 
  ggplot(aes(x=Tmt, y=log(rootmass_g/LeafWet_g))) + geom_jitter(aes(color=Tmt, size = 2))

rootimage %>% 
 # filter(Spp == "L") %>% 
ggplot(aes(x=Tmt, y=Number.of.Root.Tips)) + geom_boxplot()

ggplot(rootimage, aes(x=Tmt, y=Total.Root.Length.mm)) + geom_boxplot() + facet_grid(~Spp)

ggplot(rootimage, aes(x=Tmt, y=Maximum.Diameter.mm)) + geom_boxplot() + facet_grid(~Spp)

rootimage %>% 
  filter(Spp == "L") %>% 
  ggplot(aes(x=Tmt, y=Root.Length.Diameter.Range.1.mm)) + geom_boxplot()

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
  #filter(Spp == "V") %>% 
  ggplot(aes(x=Tmt, y=d13C)) + geom_boxplot(aes(color=Tmt))

ggplot(SIF, aes(x=Tmt, y=d13C)) + geom_boxplot()

## Growth and mortality (Inventory)
library(lubridate)
library(RColorBrewer)
inventory_raw <- read.csv("/Users/paigekouba/Documents/UC_Davis/2021_Winter/Quals/Proposal/Chapter 1/TinyFACE/GitHub/QuailFACE/RawData/Inventory8.csv")

inventory_raw$Inv..8.Date <- "11/20/23"

# want a long format df with columns =  [ Code, Plot, Tmt, Date, Ht ] x 129 
# then, plot x = date y = ht, group_by Code, color = Tmt

# try it just with inventory date
inv_long <- inventory_raw %>% 
  select(Code, Inv..1.Date, Inv..2.Date, Inv..3.Date, Inv..4.Date, Inv..5.Date, Inv..6.Date, Inv..7.Date, Inv..8.Date) %>% 
  pivot_longer(!Code, names_to = "Date") %>% 
  mutate(value = mdy(value)) 

# now do it with ht data
inv_hts <- inventory_raw %>% 
  select(Code, paste0("Ht.mm..",c(1:8))) %>% 
  pivot_longer(!Code, names_to = "key", values_to = "ht_mm")

inv_long <- cbind(inv_long, inv_hts) %>%
  select(!4) %>% 
  mutate(Plot = if_else(nchar(Code) == 4, substr(Code, 1,1), substr(Code,1,2)))

inv_long <- left_join(inv_long, lookup, by = "Plot")

inv_long <- inv_long %>% 
  mutate(Spp = substr(Code, nchar(Code)-2,nchar(Code)-2))

ggplot(inv_long) +
  geom_jitter(aes(x = value, y = ht_mm, group = Code, color = Spp)) +
  geom_line(aes(x = value, y = ht_mm, group = Code, color = Spp)) + facet_grid(Spp ~ Tmt, scales = "free")

inv_cond <- inventory_raw %>% 
  select(Code, paste0("Cond..",c(1:8))) %>% 
  pivot_longer(!Code, names_to = "key", values_to = "cond")

cond_long <- cbind(inv_long, inv_cond) 

cond_long <- cond_long[,c(1:3,5:8,11)]

ggplot(cond_long) +
  geom_jitter(aes(x = value, y = cond, group = Code, color = Spp)) +
  geom_line(aes(x = value, y = cond, group = Code, color = Spp)) + facet_grid(Spp ~ Tmt, scales = "free")  

inv_dia <- inventory_raw %>% 
  select(Code, paste0("Dia.mm..",c(1:8))) %>% 
  pivot_longer(!Code, names_to = "key", values_to = "dia")

dia_long <- cbind(inv_long, inv_dia) 

dia_long <- dia_long[,c(1:3,5:8,11)]

dia_long[which.max(dia_long$dia),] # two outliers need adjusting
#     Code        Date      value ht_mm Plot Tmt Spp dia
# 104 2L1a Inv..8.Date 2023-11-20   156    2  ED   L 389
dia_long[104,8] <- 3.89 # assume it's a decimal error
dia_long[which.max(dia_long$dia),]
#      Code        Date      value ht_mm Plot Tmt Spp dia
# 1774 3V3b Inv..6.Date 2023-08-26    96    3  AD   V 54.9
dia_long[1774,8] <- 5.49 # assume a decimal error again, even though this is quite possiblya leaf length
#      Code        Date      value ht_mm Plot Tmt Spp dia
# 830 9L4b Inv..6.Date 2023-08-26  4.24    9  ED   L 37.8
dia_long[830,8] <- 3.78

ggplot(dia_long) +
  geom_jitter(aes(x = value, y = dia, group = Code, color = Spp)) +
  geom_line(aes(x = value, y = dia, group = Code, color = Spp)) + facet_grid(Spp ~ Tmt, scales = "free")  

dia_long %>% 
  group_by(Spp, Tmt, Date) %>% 
  summarise(mean_dia = mean(dia, na.rm = TRUE), mean_ht = mean(ht_mm, na.rm = TRUE),
            sd_dia = sd(dia, na.rm = TRUE), sd_ht = sd(ht_mm, na.rm = TRUE)) %>% View()
  ggplot(aes(x=Date, y = mean_ht)) + geom_point(aes(x=Date, y = mean_ht)) + geom_line(aes(x=Date, y = mean_ht)) + facet_grid(Spp ~ Tmt, scales = "free")


dia_long %>% 
  group_by(Spp, Tmt, Date) %>% 
  summarise(mean_dia = mean(dia, na.rm = TRUE), mean_ht = mean(ht_mm, na.rm = TRUE),
            sd_dia = sd(dia, na.rm = TRUE), sd_ht = sd(ht_mm, na.rm = TRUE)) %>% 
  ggplot(aes(x=Date, y = mean_ht, group = Tmt)) + 
  geom_pointrange(aes(ymin = mean_ht - sd_ht/7, ymax = mean_ht + sd_ht/7, color = Tmt, shape = Tmt), position = position_dodge(0.2), size = 1.5, alpha = 0.75) + 
  scale_color_manual(values = c("red", "blue", "red", "blue")) +
  scale_shape_manual(values = c(16,16,17,17)) +
  geom_line(aes(color = Tmt), position = position_dodge(0.2), linewidth = 1.2) + facet_grid(~Spp)


dia_long %>% 
  group_by(Spp, Tmt, Date) %>%
  tally() %>% View()
