# 5/20/24
# Ch 3 Analysis

# load all data types
## load the data fresh with "." suffix; do steps from Ch3_explore and Ch3_LiCOR. Examine colnames

# LiCOR_df
# actually just load this one after running Ch3_LiCOR
LiCOR_df. <- LiCOR_df
LiCOR_df.[,80:92]
#LiCOR_df.[,80:94] # has SWC, Plot, Spp, Tmt, Photo.y, Cond.y, WUE.350, CO2Tmt and H2OTmt
# needs shortcode, CO2
LiCOR_df. <- LiCOR_df. %>% 
  mutate(Code = if_else(nchar(ID) == 4,substr(ID,1,3),substr(ID,1,4))) %>% 
  left_join(plot_CO2., by = "Plot")
LiCOR_df.[which(LiCOR_df$WUE.350 > 550),92] <- NA # filter outliers for WUE.350

# plot_CO2, plot_SWC
plot_SWC. <- LiCOR_df.[,c("Plot", "SWC")] %>% 
  group_by(Plot) %>% 
  summarise(meanSWC=mean(SWC)) %>% 
  as.data.frame()

## This needs to be changed ! plotwise means from 10/9 were at or near 200, but overall means from the whole experiment were 118 ppm
# plot_CO2. <- avg_between %>% # from CO2_Ch2.R
#   dplyr::select(Plot, mDeltaTest) %>% 
#   mutate(CO2 = 420+mDeltaTest) %>% 
#   as.data.frame()
# change it here and downstream stuff will work out

# get the per-plot ∆CO2 from the 10/9 test, scale it to the requisite fraction of the mean elevation (127 ppm) added to the mean ambient value (422)
min(avg_between$mDeltaTest) # -6.114156
max(avg_between$mDeltaTest) # 256.731
scale((avg_between$mDeltaTest + 422), center = F, scale = T)*544
mean(avg_between$mDeltaTest[avg_between$mDeltaTest>100]) # 210.8419
mean(avg_between$mDeltaTest[avg_between$mDeltaTest<100]) # 8.442497

# for eCO2 plots, the mean is (210 - 127) = 83.8 lower
plot_CO2. <- avg_between %>% 
  select(Plot, mDeltaTest) %>% 
  mutate(CO2 = case_when(mDeltaTest > 100 ~ 422+(mDeltaTest-83.8),
                         mDeltaTest < 100 ~ 422+(mDeltaTest)) ) %>% 
  as.data.frame()

# biomass2
# jeez too many steps. get this one after Ch3_explore, it is messy but w/e
## add leaf area columns to biomass2 in explore script ##
biomass2. <- biomass2
biomass2.[1,]
biomass2. <- biomass2. %>% 
  mutate(rootshoot = rootmass_g/(StemWet_expanded + LeafWet_expanded), totmass = rootmass_g + StemWet_expanded + LeafWet_expanded, lwc = (LeafWet_g-LeafDry_g)*100/LeafWet_g) %>% 
  left_join(plot_CO2., by = "Plot") %>% 
  left_join(plot_SWC., by = "Plot") 

biomass2.[which(biomass2.$totmass==47.05),"totmass"] <- NA # filter outlier for totmass

# exclude rootshoot for 6V2, an outlier: was herbivory list but predicted values failed to adjust appropriately
biomass2.[which(biomass2.$Code=="6V2"),"rootshoot"] <- NA

# now remove lwc for seedlings from herbivory list, likely to have no leaves 
# biomass2.[which(biomass2.$Code %in% firstherb$Code),]$lwc <- NA 
# actually I don't think this is necessary! water content won't be affected by leaf herbivory, it's proportional

# leaf area
lai. <- lai
lai.[which(lai.$tot_area < 1000),]$tot_area <- NA
lai.[which(lai.$tot_area < 1000),]$avg_area <- NA
lai.[which(lai.$tot_area < 1000),]$count <- NA
lai.[which(lai.$perim_per_A > 1),]$perim_per_A <- NA
lai.[which(lai.$SLA<4000),]$SLA <- NA
lai.[which(lai.$Spp=="L" & lai.$SLA > 12000),]$SLA <- NA

# rootimage
rootimage. <- read.csv("/Users/paigekouba/Documents/UC_Davis/2021_Winter/Quals/Proposal/Chapter 1/TinyFACE/GitHub/QuailFACE/RawData/features_4.26.24_final.csv") %>% 
  mutate(Plot = as.character(Plot))
rootimage. <- left_join(lookup, rootimage., by = 'Plot')

rootimage. <- rootimage. %>% 
  mutate(Spp = substr(Code, nchar(Code)-2,nchar(Code)-2)) %>%
  filter(Code != "4V3c") %>% # prevent duplicate codes; 4V3c was planted late and misnamed
  filter(Code != "16V1a") %>% # thinned but grew back
  mutate(Code = if_else(nchar(Code) == 4,substr(Code,1,3),substr(Code,1,4)), CO2Tmt = substring(Tmt,1,1), H2OTmt = substring(Tmt,2,2)) %>% 
  left_join(biomass2.[,c("Code","rootmass_g")]) %>% 
  mutate(SRL = Total.Root.Length.mm/rootmass_g) %>% 
  left_join(plot_CO2., by = "Plot") %>% 
  left_join(plot_SWC., by = "Plot") 
rootimage.[1,]

# SIF
SIF_raw <- read.csv("/Users/paigekouba/Documents/UC_Davis/2021_Winter/Quals/Proposal/Chapter 1/TinyFACE/GitHub/QuailFACE/RawData/PVK_SIF.csv")
SIF_raw <- SIF_raw %>% 
  filter(!is.na(Internal.ID))
SIF. <- SIF_raw %>% 
  filter(Sample.ID != "4V3c") %>% 
  mutate(Spp = substr(Sample.ID, nchar(Sample.ID)-2,nchar(Sample.ID)-2)) %>% 
  mutate(Plot = if_else(nchar(Sample.ID) == 4, substr(Sample.ID,1,1), substr(Sample.ID,1,2))) %>% 
  dplyr::select(Sample.ID, δ13CVPDB...., Spp, Plot) 
colnames(SIF.) <- c("Code","d13C","Spp","Plot")
SIF. <- left_join(SIF., lookup, by = "Plot") %>% 
  mutate(Code=if_else(nchar(Code) == 4,substr(Code,1,3),substr(Code,1,4)), CO2Tmt = substring(Tmt,1,1), H2OTmt = substring(Tmt,2,2)) %>% 
  left_join(plot_CO2., by = "Plot") %>% 
  left_join(plot_SWC., by = "Plot") 
SIF.[1,]


## model workflow for
# LiCOR_df. responses Photo.y, Cond.y, WUE.350
# biomass2. responses totmass, rootshoot, LWC
# rootimage. responses ratio_2to1, dia_range_1 or w/e, # branchpoints
# SIF. response d13C


library(lmerTest)  

# Step 2: for each of the four data types, and each response, go through this modeling protocol:
# test effect of CO2Tmt*H2OTmt on Plot means, for both species; then for each species
# test effect of CO2Tmt*H2OTmt on individual datapoints with (1|Plot), for both species; then for each species
# test effect of (rescaled)CO2*SWC on Plot means, for both species; then for each species
# test effect of (rescaled)CO2*SWC on individual datapoints with (1|Plot), for both species; then for each species
# ggeffects plot to visualize significant and/or interesting results

# models with transformed data and **weighted plot means** for variable-specific weights
# priority is CO2*H2OTmt, like in plots; can also try CO2*SWC (rescaled)
# get that dataset here:




# now do each variable for each species, and do ggpredict with facet_grid
# (this kind of worked?)
# m1 <- lmer(Photo.y ~ CO2*H2OTmt*Spp + (1|Plot) , data = LiCOR_df.)
# ggpredict(m1, terms=c("CO2","H2OTmt", "Spp"), facet = "Spp") %>% 
#   plot(add.data = TRUE, ci = T, facet ="Spp") 

# Photo.y: no transformation, no outliers
m1V <- lmer(Photo.y ~ CO2*H2OTmt + (1|Plot), data = filter(LiCOR_df., Spp == "V"))
m1L <- lmer(Photo.y ~ CO2*H2OTmt + (1|Plot), data = filter(LiCOR_df., Spp == "L"))
#ggpredict(m1L, terms=c("CO2","H2OTmt")) %>% 
  #plot(add.data = TRUE, ci = FALSE)
plot(simulateResiduals(m10V))
plot(simulateResiduals(m10L))

# Cond.y: log transformations
m2V <- lmer(log(Cond.y) ~ CO2*H2OTmt + (1|Plot), data = filter(LiCOR_df., Spp == "V"))
m2L <- lmer(log(Cond.y) ~ CO2*H2OTmt + (1|Plot), data = filter(LiCOR_df., Spp == "L"))

# totmass: sqrt
m4V <- lmer(sqrt(totmass) ~ CO2*H2OTmt + (1|Plot), data = filter(biomass2., Spp == "V"))
m4L <- lmer(sqrt(totmass) ~ CO2*H2OTmt + (1|Plot), data = filter(biomass2., Spp == "L"))

# rootshoot: filter >9
m5V <- lmer(rootshoot ~ CO2*H2OTmt + (1|Plot), data = filter(biomass2., Spp == "V" & rootshoot<9))
m5L <- lmer(rootshoot ~ CO2*H2OTmt + (1|Plot), data = filter(biomass2., Spp == "L" & rootshoot<9))

# lwc: ok
m6V <- lmer(lwc ~ CO2*H2OTmt + (1|Plot), data = filter(biomass2., Spp == "V")) 
m6L <- lmer(lwc ~ CO2*H2OTmt + (1|Plot), data = filter(biomass2., Spp == "L"))

# SLA

# change this to SRL
m7V <- lmer(log(ratio_2to1) ~ CO2*H2OTmt + (1|Plot), data = filter(rootimage., Spp == "V" & ratio_2to1>0.01))
m7L <- lmer(log(ratio_2to1) ~ CO2*H2OTmt + (1|Plot), data = filter(rootimage., Spp == "L" & ratio_2to1>0.01))

# SIF: ok
m10V <- lmer(d13C ~ CO2*H2OTmt + (1|Plot), data = filter(SIF., Spp == "V"))
m10L <- lmer(d13C ~ CO2*H2OTmt + (1|Plot), data = filter(SIF., Spp == "L"))

plot(simulateResiduals(m10V))
plot(simulateResiduals(m10L))

## manova on up to 5(?) variables out of the 7 (not SIF anyway)

# reasonable subset: Photo.y, logCond.y, sqrt_totmass, rootshoot
summary(manova(small.cbind ~ CO2Tmt*H2OTmt, manova.df.small, weights = Photo.y_w))
# with weights from just Photo.y_w, all p < .05
summary(manova(small.cbind ~ CO2Tmt*H2OTmt, manova.df.small, weights = all_w)) 
# with combined weights, * *** **
summary(manova(small.cbind ~ CO2Tmt*H2OTmt, manova.df.small, weights = quad_w)) # **, ***, ***

## now for fun, do ggpredict with weighted model, and with weighted model and outlier filter
ggpredict(lm(Photo.y ~ CO2*H2OTmt , data = manova.df.small, weights = Photo.y_w), terms=c("CO2","H2OTmt")) %>% 
  plot(add.data = TRUE, ci = TRUE)
ggpredict(lm(Photo.y ~ CO2*H2OTmt , data = manova.df.small, weights = quad_w), terms=c("CO2","H2OTmt")) %>% 
  plot(add.data = TRUE, ci = TRUE)


