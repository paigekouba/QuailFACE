# 5/20/24
# Ch 3 Analysis

# load all data types
## load the data fresh with "." suffix; do steps from Ch3_explore and Ch3_LiCOR. Examine colnames

# plot_CO2, plot_SWC
plot_SWC. <- LiCOR_df.[,c("Plot", "SWC")] %>% 
  group_by(Plot) %>% 
  summarise(meanSWC=mean(SWC)) %>% 
  as.data.frame()

plot_CO2. <- avg_between %>% # from CO2_Ch2.R
  dplyr::select(Plot, mDeltaTest) %>% 
  mutate(CO2 = 420+mDeltaTest) %>% 
  as.data.frame()

# LiCOR_df
# actually just load this one after running Ch3_LiCOR
LiCOR_df. <- LiCOR_df
LiCOR_df.[,80:94] # has SWC, Plot, Spp, Tmt, Photo.y, Cond.y, WUE.350, CO2Tmt and H2OTmt
# needs shortcode, CO2
LiCOR_df. <- LiCOR_df. %>% 
  mutate(Code = if_else(nchar(ID) == 4,substr(ID,1,3),substr(ID,1,4))) %>% 
  left_join(plot_CO2., by = "Plot")

# biomass2
# jeez too many steps. get this one after Ch3_explore, it is messy but w/e
## add leaf area columns to biomass2 in explore script ##
biomass2. <- biomass2
biomass2.[1,]
biomass2. <- biomass2. %>% 
  mutate(rootshoot = rootmass_g/(StemWet_expanded + LeafWet_expanded), totmass = rootmass_g + StemWet_expanded + LeafWet_expanded, lwc = (LeafWet_g-LeafDry_g)*100/LeafWet_g) %>% 
  left_join(plot_CO2., by = "Plot") %>% 
  left_join(plot_SWC., by = "Plot") 

# exclude rootshoot for 6V2, an outlier: was herbivory list but predicted values failed to adjust appropriately
biomass2.[which.max(biomass2.$rootshoot),"rootshoot"] <- NA

# now remove lwc for seedlings from herbivory list, likely to have no leaves 
biomass2.[which(biomass2.$Code %in% firstherb$Code),]$lwc <- NA 

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

# Step 0: check data for normality and normality of residuals (dharma package to check residuals)
# wait! have to do this by spp
# try transformed data # check SWC by eCO2 to see if there's a feedback # specific root length
# LiCOR_df.
hist((LiCOR_df.[LiCOR_df.$Spp == "V",]$Photo.y), breaks = 2*sqrt(nrow(LiCOR_df.)))
qqPlot(LiCOR_df.[LiCOR_df.$Spp == "V",]$Photo.y) 
hist((LiCOR_df.[LiCOR_df.$Spp == "L",]$Photo.y), breaks = 2*sqrt(nrow(LiCOR_df.)))
qqPlot((LiCOR_df.[LiCOR_df.$Spp == "L",]$Photo.y)) # pretty ok by spp!

hist(log(LiCOR_df.[LiCOR_df.$Spp=="V",]$Cond.y))
qqPlot(log(LiCOR_df.[LiCOR_df.$Spp=="V",]$Cond.y))
hist(log(LiCOR_df.[LiCOR_df.$Spp=="L",]$Cond.y))
qqPlot(log(LiCOR_df.[LiCOR_df.$Spp=="L",]$Cond.y))

LiCOR_df. %>% 
  filter(WUE.350 < 550) %>% 
  filter(Spp=="V") %>% 
  dplyr::select(WUE.350) %>% 
  unlist() %>% 
  as.numeric() %>% 
  sqrt() %>% 
  #  hist(breaks = sqrt(nrow(LiCOR_df.)))
  qqPlot()

# biomass2.
hist(sqrt(biomass2.[biomass2.$Spp=="L",]$totmass), breaks=2*sqrt(nrow(biomass2.))) # outlier
qqPlot(sqrt(biomass2.[biomass2.$Spp=="L",]$totmass)) 

biomass2. %>% 
  filter(rootshoot < 9) %>% 
  filter(Spp=="V") %>% 
  dplyr::select(rootshoot) %>% 
  unlist() %>% 
  as.numeric() %>% 
  # log() %>% 
  #hist(breaks = sqrt(nrow(LiCOR_df.)))
  qqPlot()
hist((biomass2.[biomass2.$Spp=="L",]$rootshoot), breaks=2*sqrt(nrow(biomass2.))) # outliers
qqPlot((biomass2.[biomass2.$Spp=="L",]$rootshoot)) # fatty right tail
qqPlot(log(biomass2.[biomass2.$Spp=="L",]$rootshoot)) # better for V, worse for L
# this one is tough

hist(biomass2.[biomass2.$Spp=="V",]$lwc, breaks=2*sqrt(nrow(biomass2.))) 
qqPlot(biomass2.[biomass2.$Spp=="V",]$lwc) 
# fine by spp

# check leaf area variables here


# rootimage.
rootimage. %>% 
  #  filter(SRL < 1500) %>% 
  filter(Spp=="V") %>% 
  dplyr::select(SRL) %>% 
  unlist() %>% 
  as.numeric() %>% 
  log() %>% 
  #hist(breaks = sqrt(nrow(rootimage.)))
  qqPlot()
hist(log(rootimage.[rootimage.$Spp=="V",]$SRL), breaks=sqrt(nrow(rootimage.)))
qqPlot(log(rootimage.[rootimage.$Spp=="V",]$SRL))
hist(log(rootimage.[rootimage.$Spp=="L",]$SRL), breaks=sqrt(nrow(rootimage.)))
qqPlot(log(rootimage.[rootimage.$Spp=="L",]$SRL))

hist(log(rootimage.[rootimage.$Spp=="L",]$Root.Length.Diameter.Range.1.mm))
qqPlot(log(rootimage.[rootimage.$Spp=="L",]$Root.Length.Diameter.Range.1.mm))

rootimage. %>% 
  filter(Number.of.Branch.Points > 5) %>% 
  filter(Spp=="V") %>% 
  dplyr::select(Number.of.Branch.Points) %>% 
  unlist() %>% 
  as.numeric() %>% 
  log() %>% 
  #hist(breaks = sqrt(nrow(LiCOR_df.)))
  qqPlot()
hist(log(rootimage.[rootimage.$Spp=="L",]$Number.of.Branch.Points))
qqPlot(log(rootimage.[rootimage.$Spp=="L",]$Number.of.Branch.Points))

# SIF.
hist(SIF.[SIF.$Spp=="V",]$d13C)
qqPlot(SIF.[SIF.$Spp=="V",]$d13C) # cute

# check correlation between variables
cor(manova.df2[, c(4:9,14:20)], use="pairwise.complete.obs") %>% 
  ggcorrplot()

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


