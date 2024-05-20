# Monday 5/13/24
# Ch 3 Results
# AKA Final Round Final Round, Best of Three

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

library(lmerTest)  

# Step 2: for each of the four data types, and each response, go through this modeling protocol:
# test effect of CO2Tmt*H2OTmt on Plot means, for both species; then for each species
# test effect of CO2Tmt*H2OTmt on individual datapoints with (1|Plot), for both species; then for each species
# test effect of (rescaled)CO2*SWC on Plot means, for both species; then for each species
# test effect of (rescaled)CO2*SWC on individual datapoints with (1|Plot), for both species; then for each species
# ggeffects plot to visualize significant and/or interesting results

# models with transformed data
# go again; if intx is not significant, drop it and run the main effects

# Photo.y: no transformation, no outliers
m1V <- lmer(Photo.y ~ CO2*H2OTmt + (1|Plot), data = filter(LiCOR_df., Spp == "V"))
m1L <- lmer(Photo.y ~ CO2*H2OTmt + (1|Plot), data = filter(LiCOR_df., Spp == "L"))

# Cond.y: log transformations
m2V <- lmer(log(Cond.y) ~ CO2*H2OTmt + (1|Plot), data = filter(LiCOR_df., Spp == "V"))
m2L <- lmer(log(Cond.y) ~ CO2*H2OTmt + (1|Plot), data = filter(LiCOR_df., Spp == "L"))

# WUE.350: filter <550, sqrt
m3V <- lmer(WUE.350tfm ~ CO2*H2OTmt + (1|Plot), data = filter(LiCOR_df.tfm, Spp == "V"))
m3L <- lmer(WUE.350tfm ~ CO2*H2OTmt + (1|Plot), data = filter(LiCOR_df.tfm, Spp == "L"))

# totmass: sqrt
m4V <- lmer(sqrt(totmass) ~ CO2*H2OTmt + (1|Plot), data = filter(biomass2., Spp == "V"))
m4L <- lmer(sqrt(totmass) ~ CO2*H2OTmt + (1|Plot), data = filter(biomass2., Spp == "L"))

# rootshoot: filter >9
m5V <- lmer(rootshoot ~ CO2*H2OTmt + (1|Plot), data = filter(biomass2., Spp == "V" & rootshoot<9))
m5L <- lmer(rootshoot ~ CO2*H2OTmt + (1|Plot), data = filter(biomass2., Spp == "L" & rootshoot<9))

# lwc: ok
m6V <- lmer(lwc ~ CO2*H2OTmt + (1|Plot), data = filter(biomass2., Spp == "V")) 
m6L <- lmer(lwc ~ CO2*H2OTmt + (1|Plot), data = filter(biomass2., Spp == "L"))

# ratio_2to1: filter>0.01, log
m7V <- lmer(log(ratio_2to1) ~ CO2*H2OTmt + (1|Plot), data = filter(rootimage., Spp == "V" & ratio_2to1>0.01))
m7L <- lmer(log(ratio_2to1) ~ CO2*H2OTmt + (1|Plot), data = filter(rootimage., Spp == "L" & ratio_2to1>0.01))

## redo these with random effect!
# Root range 1: log
m8V <- lmer(log(Root.Length.Diameter.Range.1.mm) ~ CO2*H2OTmt + (1|Plot), data = filter(rootimage., Spp == "V"))
m8L <- lmer(log(Root.Length.Diameter.Range.1.mm) ~ CO2*H2OTmt + (1|Plot), data = filter(rootimage., Spp == "L"))

# # branch points: filter >5, log
m9V <- lmer(log(Number.of.Branch.Points) ~ CO2*H2OTmt + (1|Plot), data = filter(rootimage., Spp == "V" & Number.of.Branch.Points > 5))
m9L <- lmer(log(Number.of.Branch.Points) ~ CO2*H2OTmt + (1|Plot), data = filter(rootimage., Spp == "L" & Number.of.Branch.Points > 5))

# SIF: ok
m10V <- lmer(d13C ~ CO2*H2OTmt + (1|Plot), data = filter(SIF., Spp == "V"))
m10L <- lmer(d13C ~ CO2*H2OTmt + (1|Plot), data = filter(SIF., Spp == "L"))

# LiCOR_df. : Photo.y, Cond.y, WUE.350
LiCOR_df.pmeans <- LiCOR_df. %>% 
  dplyr::select(Code, Plot, Spp, CO2Tmt, H2OTmt, CO2, SWC, Photo.y, Cond.y, WUE.350) %>% 
  group_by(Plot, CO2Tmt, H2OTmt, Spp) %>% 
  summarise(across(c(CO2, SWC, Photo.y, Cond.y, WUE.350), ~ mean(.x, na.rm=TRUE)))
# Photo.y meets assumptions of normality (by spp)
summary(lm(Photo.y ~ CO2Tmt*H2OTmt, data = filter(LiCOR_df.pmeans, Spp == "V" | Spp == "L"))) # H2O p <.0003
summary(lm(Photo.y ~ CO2Tmt*H2OTmt, data = filter(LiCOR_df.pmeans, Spp == "L" | Spp == "L"))) # H2O p <.005 
summary(lm(Photo.y ~ CO2Tmt*H2OTmt, data = filter(LiCOR_df.pmeans, Spp == "V" | Spp == "V"))) # H2O p <.02
summary(lmer(Photo.y ~ CO2Tmt*H2OTmt + (1|Plot), data = filter(LiCOR_df., Spp == "V" | Spp == "L"))) # H2O p <.0004
summary(lmer(Photo.y ~ CO2Tmt*H2OTmt + (1|Plot), data = filter(LiCOR_df., Spp == "L" | Spp == "L"))) # H2O p <.008
summary(lmer(Photo.y ~ CO2Tmt*H2OTmt + (1|Plot), data = filter(LiCOR_df., Spp == "V" | Spp == "V"))) # H2O p <.015
summary(lm(Photo.y ~ rescale(CO2)*rescale(SWC), data = filter(LiCOR_df.pmeans, Spp == "L" | Spp == "V"))) # SWC p < .005
summary(lm(Photo.y ~ rescale(CO2)*rescale(SWC), data = filter(LiCOR_df.pmeans, Spp == "L" | Spp == "L"))) # SWC p < 0.03
summary(lm(Photo.y ~ rescale(CO2)*rescale(SWC), data = filter(LiCOR_df.pmeans, Spp == "V" | Spp == "V"))) # SWC p = 0.07
summary(lmer(Photo.y ~ rescale(CO2)*rescale(SWC) + (1|Plot), data = filter(LiCOR_df., Spp == "L" | Spp == "V"))) # SWC p<.0055
summary(lmer(Photo.y ~ rescale(CO2)*rescale(SWC) + (1|Plot), data = filter(LiCOR_df., Spp == "L" | Spp == "L"))) # SWC p<.05
summary(lmer(Photo.y ~ rescale(CO2)*rescale(SWC) + (1|Plot), data = filter(LiCOR_df., Spp == "V" | Spp == "V"))) # SWC p<.04
m1 <- lmer(Photo.y ~ CO2*H2OTmt + (1|Plot), data = filter(LiCOR_df., Spp == "V" | Spp == "V"))
ggpredict(m1L, terms=c("CO2","H2OTmt")) %>% 
  plot(add.data = TRUE, ci = FALSE)

cooksd1 <- cooks.distance(m1V)
sample_size <- nobs(m1V)
plot(cooksd1, pch="*", cex=2, main="Influential Obs by Cooks distance")  # plot cook's distance
abline(h = 4/sample_size, col="red")  # add cutoff line
text(x=1:length(cooksd1)+1, y=cooksd1, labels=ifelse(cooksd1>4/sample_size, names(cooksd1),""), col="red")  # add labels

influential1 <- as.numeric(names(cooksd1)[(cooksd1 > (4/sample_size))])
ggpredict(lmer(Photo.y ~ CO2*H2OTmt + (1|Plot), data = LiCOR_df. %>% 
  filter(Spp == "V") %>% 
  filter(!row_number() %in% influential1) ), terms=c("CO2","H2OTmt")) %>% plot(add.data = TRUE, ci = TRUE)
summary(lmer(Photo.y ~ CO2*H2OTmt + (1|Plot), data = LiCOR_df. %>% 
               filter(Spp == "V") %>% 
               filter(!row_number() %in% influential1) ))

# test model residuals
library(DHARMa)
plot(simulateResiduals(m1V))
plot(simulateResiduals(m1L))

# Cond.y: log
#summary(lm(Cond.y ~ CO2Tmt*H2OTmt, data = filter(LiCOR_df.pmeans, Spp == "V" | Spp == "L"))) # H2O p <.0015
summary(lm(log(Cond.y) ~ CO2Tmt*H2OTmt, data = filter(LiCOR_df.pmeans, Spp == "L" | Spp == "L"))) # H2O p <.002
summary(lm(log(Cond.y) ~ CO2Tmt*H2OTmt, data = filter(LiCOR_df.pmeans, Spp == "V" | Spp == "V"))) # H2O p <<<, CO2 <.01, intx<.025
#summary(lmer(Cond.y ~ CO2Tmt*H2OTmt + (1|Plot), data = filter(LiCOR_df., Spp == "V" | Spp == "L"))) # H2O p<.0006
summary(lmer(log(Cond.y) ~ CO2Tmt*H2OTmt + (1|Plot), data = filter(LiCOR_df., Spp == "L" | Spp == "L"))) # H2O p<.0003, isSing
summary(lmer(log(Cond.y) ~ CO2Tmt*H2OTmt + (1|Plot), data = filter(LiCOR_df., Spp == "V" | Spp == "V"))) # H2O p<.0002, CO2 <.015, int<.03
#summary(lm(Cond.y ~ rescale(CO2)*rescale(SWC), data = filter(LiCOR_df.pmeans, Spp == "L" | Spp == "V"))) # SWC p < .002
summary(lm(log(Cond.y) ~ rescale(CO2)*rescale(SWC), data = filter(LiCOR_df.pmeans, Spp == "L" | Spp == "L"))) # SWC p < 0.003
summary(lm(log(Cond.y) ~ rescale(CO2)*rescale(SWC), data = filter(LiCOR_df.pmeans, Spp == "V" | Spp == "V"))) # SWC p<<<, CO2<.0085, intx<.03
#summary(lmer(Cond.y ~ rescale(CO2)*rescale(SWC) + (1|Plot), data = filter(LiCOR_df., Spp == "L" | Spp == "V"))) # SWC p<<<.05
summary(lmer(log(Cond.y) ~ rescale(CO2)*rescale(SWC) + (1|Plot), data = filter(LiCOR_df., Spp == "L" | Spp == "L"))) # SWC p<<<, isSing
summary(lmer(log(Cond.y) ~ rescale(CO2)*rescale(SWC) + (1|Plot), data = filter(LiCOR_df., Spp == "V" | Spp == "V"))) # SWC p<<<, CO2<.02, int<.065
summary(lmer(log(Cond.y) ~ CO2*H2OTmt + (1|Plot), data = filter(LiCOR_df., Spp == "V" | Spp == "V"))) # H2) .003, CO2 .008, intx .02
m2 <- lmer(log(Cond.y) ~ CO2*H2OTmt + (1|Plot), data = filter(LiCOR_df., Spp == "V" | Spp == "V"))
ggpredict(m2, terms=c("CO2","H2OTmt")) %>% 
  plot(add.data = TRUE, ci = TRUE)
# Model has log-transformed response. Back-transforming predictions to original response scale. Standard errors are still on the log-scale.
# once again, why would eCO2 plants have *higher* gs when compared with their same-watering buddies ?

cooksd2 <- cooks.distance(m2L)
sample_size <- nobs(m2L)
plot(cooksd2, pch="*", cex=2, main="Influential Obs by Cooks distance")  # plot cook's distance
abline(h = 4/sample_size, col="red")  # add cutoff line
text(x=1:length(cooksd2)+1, y=cooksd2, labels=ifelse(cooksd2>4/sample_size, names(cooksd2),""), col="red")  # add labels

influential2 <- as.numeric(names(cooksd2)[(cooksd2 > (4/sample_size))]) # none are outliers! :)
ggpredict(lmer(log(Cond.y) ~ CO2*H2OTmt + (1|Plot), data = LiCOR_df. %>% 
                 filter(Spp == "L") %>% 
                 filter(!row_number() %in% influential2) ), terms=c("CO2","H2OTmt")) %>% plot(add.data = TRUE, ci = TRUE)
summary(lmer(log(Cond.y) ~ CO2*H2OTmt + (1|Plot), data = LiCOR_df. %>% 
               filter(Spp == "L") %>% 
               filter(!row_number() %in% influential2) )) 
plot(simulateResiduals(m2V))
plot(simulateResiduals(m2L))


LiCOR_df.tfm <- LiCOR_df. %>% 
  filter(WUE.350 < 550) %>% 
  mutate(WUE.350tfm = sqrt((WUE.350)))

LiCOR_df.tfm_pmeans <- LiCOR_df. %>% 
  filter(WUE.350 < 550) %>% 
  dplyr::select(Code, Plot, Spp, CO2Tmt, H2OTmt, CO2, SWC, Photo.y, Cond.y, WUE.350) %>% 
  group_by(Plot, CO2Tmt, H2OTmt, Spp) %>% 
  summarise(across(c(CO2, SWC, Photo.y, Cond.y, WUE.350), ~ mean(.x, na.rm=TRUE))) %>% 
  mutate(WUE.350tfm = sqrt(WUE.350))
  

#summary(lm(WUE.350 ~ CO2Tmt*H2OTmt, data = filter(LiCOR_df.pmeans, Spp == "V" | Spp == "L"))) 
summary(lm(WUE.350tfm ~ CO2Tmt*H2OTmt, data = filter(LiCOR_df.tfm_pmeans, Spp == "L" | Spp == "L"))) 
summary(lm(WUE.350tfm ~ CO2Tmt*H2OTmt, data = filter(LiCOR_df.tfm_pmeans, Spp == "V" | Spp == "V"))) 
#summary(lmer(WUE.350 ~ CO2Tmt*H2OTmt + (1|Plot), data = filter(LiCOR_df., Spp == "V" | Spp == "L"))) 
summary(lmer(WUE.350tfm ~ CO2Tmt*H2OTmt + (1|Plot), data = filter(LiCOR_df.tfm, Spp == "L" | Spp == "L")))
summary(lmer(WUE.350tfm ~ CO2Tmt*H2OTmt + (1|Plot), data = filter(LiCOR_df.tfm, Spp == "V" | Spp == "V"))) 
#summary(lm(WUE.350 ~ rescale(CO2)*rescale(SWC), data = filter(LiCOR_df.pmeans, Spp == "L" | Spp == "V"))) 
summary(lm(WUE.350tfm ~ rescale(CO2)*rescale(SWC), data = filter(LiCOR_df.tfm_pmeans, Spp == "L" | Spp == "L")))
summary(lm(WUE.350tfm ~ rescale(CO2)*rescale(SWC), data = filter(LiCOR_df.tfm_pmeans, Spp == "V" | Spp == "V"))) 
#summary(lmer(WUE.350 ~ rescale(CO2)*rescale(SWC) + (1|Plot), data = filter(LiCOR_df., Spp == "L" | Spp == "V")))
summary(lmer(WUE.350tfm ~ rescale(CO2)*rescale(SWC) + (1|Plot), data = filter(LiCOR_df.tfm, Spp == "L" | Spp == "L")))
summary(lmer(WUE.350tfm ~ rescale(CO2)*rescale(SWC) + (1|Plot), data = filter(LiCOR_df.tfm, Spp == "V" | Spp == "V")))
m3 <- lmer(WUE.350tfm ~ CO2*H2OTmt + (1|Plot), data = filter(LiCOR_df.tfm, Spp == "V" | Spp == "V"))
ggpredict(m3L, terms=c("CO2","H2OTmt")) %>% 
  plot(add.data = TRUE, ci = TRUE) # really nothing to see here

cooksd3 <- cooks.distance(m3L)
sample_size <- nobs(m3L)
plot(cooksd3, pch="*", cex=2, main="Influential Obs by Cooks distance")  # plot cook's distance
abline(h = 4/sample_size, col="red")  # add cutoff line
text(x=1:length(cooksd3)+1, y=cooksd3, labels=ifelse(cooksd3>4/sample_size, names(cooksd3),""), col="red")  # add labels

influential3 <- as.numeric(names(cooksd3)[(cooksd3 > (4/sample_size))])
ggpredict(lmer(WUE.350tfm ~ CO2*H2OTmt + (1|Plot), data = LiCOR_df.tfm %>% 
                 filter(Spp == "L") %>% 
                 filter(!row_number() %in% influential3) ), terms=c("CO2","H2OTmt")) %>% plot(add.data = TRUE, ci = TRUE)
summary(lmer(WUE.350tfm ~ CO2*H2OTmt + (1|Plot), data = LiCOR_df.tfm %>% 
               filter(Spp == "L") %>% 
               filter(!row_number() %in% influential3) ))

plot(simulateResiduals(m3V))
plot(simulateResiduals(m3L)) # quantile deviations detected (red curves) combined adjusted quantile test n.s.

# biomass2. : totmass, rootshoot, LWC
biomass2.pmeans <- biomass2. %>% 
  group_by(Plot, CO2Tmt, H2OTmt, Spp) %>% 
  summarise(across(c(CO2, meanSWC, totmass, rootshoot, lwc), ~ mean(.x, na.rm=TRUE)))

#summary(lm(totmass ~ CO2Tmt*H2OTmt, data = filter(biomass2.pmeans, Spp == "V" | Spp == "L"))) # H2O p <.03
summary(lm(sqrt(totmass) ~ CO2Tmt*H2OTmt, data = filter(biomass2.pmeans, Spp == "L" | Spp == "L"))) # H2O p<.065
summary(lm(sqrt(totmass) ~ CO2Tmt*H2OTmt, data = filter(biomass2.pmeans, Spp == "V" | Spp == "V"))) # H2O p <.007
#summary(lmer(totmass ~ CO2Tmt*H2OTmt + (1|Plot), data = filter(biomass2., Spp == "V" | Spp == "L")))
summary(lmer(sqrt(totmass) ~ CO2Tmt*H2OTmt + (1|Plot), data = filter(biomass2., Spp == "L" | Spp == "L"))) 
summary(lmer(sqrt(totmass) ~ CO2Tmt*H2OTmt + (1|Plot), data = filter(biomass2., Spp == "V" | Spp == "V"))) # H2O p<.001
#summary(lm(totmass ~ rescale(CO2)*rescale(meanSWC), data = filter(biomass2.pmeans, Spp == "L" | Spp == "V"))) # SWC p < .032
summary(lm(sqrt(totmass) ~ rescale(CO2)*rescale(meanSWC), data = filter(biomass2.pmeans, Spp == "L" | Spp == "L"))) # SWC p < 0.05
summary(lm(sqrt(totmass) ~ rescale(CO2)*rescale(meanSWC), data = filter(biomass2.pmeans, Spp == "V" | Spp == "V"))) # SWC p<.05
#summary(lmer(totmass ~ rescale(CO2)*rescale(meanSWC) + (1|Plot), data = filter(biomass2., Spp == "L" | Spp == "V"))) 
summary(lmer(sqrt(totmass) ~ rescale(CO2)*rescale(meanSWC) + (1|Plot), data = filter(biomass2., Spp == "L" | Spp == "L"))) # SWC < 0.1
summary(lmer(sqrt(totmass) ~ rescale(CO2)*rescale(meanSWC) + (1|Plot), data = filter(biomass2., Spp == "V" | Spp == "V"))) # SWC p<.055
m4 <- lmer(sqrt(totmass) ~ CO2*H2OTmt + (1|Plot), data = filter(biomass2., Spp == "V" | Spp == "V"))
ggpredict(m4, terms=c("CO2","H2OTmt")) %>% 
  plot(add.data = TRUE, ci = TRUE)

cooksd4 <- cooks.distance(m4L)
sample_size <- nobs(m4L)
plot(cooksd4, pch="*", cex=2, main="Influential Obs by Cooks distance")  # plot cook's distance
abline(h = 4/sample_size, col="red")  # add cutoff line
text(x=1:length(cooksd4)+1, y=cooksd4, labels=ifelse(cooksd4>4/sample_size, names(cooksd4),""), col="red")  # add labels

influential4 <- as.numeric(names(cooksd4)[(cooksd4 > (4/sample_size))])
ggpredict(lmer(sqrt(totmass) ~ CO2*H2OTmt + (1|Plot), data = biomass2. %>% 
                 filter(Spp == "V") %>% 
                 filter(!row_number() %in% influential4) ), terms=c("CO2","H2OTmt")) %>% plot(add.data = TRUE, ci = TRUE)
summary(lmer(sqrt(totmass) ~ CO2*H2OTmt + (1|Plot), data = biomass2. %>% 
               filter(Spp == "V") %>% 
               filter(!row_number() %in% influential4) )) # + with H2O, + with CO2, but not significant and no intxn

plot(simulateResiduals(m4V))
plot(simulateResiduals(m4L))

## leave out > 9 but otherwise leave alone
# rootshoot
#summary(lm(rootshoot ~ CO2Tmt*H2OTmt, data = filter(biomass2.pmeans, Spp == "V" | Spp == "L"))) 
summary(lm(rootshoot ~ CO2Tmt*H2OTmt, data = filter(biomass2.pmeans, Spp == "L" | Spp == "L" & rootshoot<9))) # H2O p < .009
summary(lm(rootshoot ~ CO2Tmt*H2OTmt, data = filter(biomass2.pmeans, Spp == "V" | Spp == "V" & rootshoot<9))) 
#summary(lmer(rootshoot ~ CO2Tmt*H2OTmt + (1|Plot), data = filter(biomass2., Spp == "V" | Spp == "L")))
summary(lmer(rootshoot ~ CO2Tmt*H2OTmt + (1|Plot), data = filter(biomass2., Spp == "L" | Spp == "L" & rootshoot<9))) # H2O p< .0021
summary(lmer(rootshoot ~ CO2Tmt*H2OTmt + (1|Plot), data = filter(biomass2., Spp == "V" | Spp == "V" & rootshoot<9)))
# summary(lm(rootshoot ~ rescale(CO2)*rescale(meanSWC), data = filter(biomass2.pmeans, Spp == "L" | Spp == "V"))) 
summary(lm(rootshoot ~ rescale(CO2)*rescale(meanSWC), data = filter(biomass2.pmeans, Spp == "L" | Spp == "L" & rootshoot<9))) # SWC p < 0.05
summary(lm(rootshoot ~ rescale(CO2)*rescale(meanSWC), data = filter(biomass2.pmeans, Spp == "V" | Spp == "V" & rootshoot<9)))  # CO2 < .085
# summary(lmer(rootshoot ~ rescale(CO2)*rescale(meanSWC) + (1|Plot), data = filter(biomass2., Spp == "L" | Spp == "V"))) 
summary(lmer(rootshoot ~ rescale(CO2)*rescale(meanSWC) + (1|Plot), data = filter(biomass2., Spp == "L" | Spp == "L" & rootshoot<9))) # SWC p<.075
summary(lmer(rootshoot ~ rescale(CO2)*rescale(meanSWC) + (1|Plot), data = filter(biomass2., Spp == "V" | Spp == "V" & rootshoot<9))) # CO2 p<.1
m5 <- lmer(rootshoot ~ CO2*H2OTmt + (1|Plot), data = filter(biomass2., Spp == "V" | Spp == "V" & rootshoot<9))
ggpredict(m5, terms=c("CO2","H2OTmt")) %>% 
  plot(add.data = TRUE, ci = TRUE)

cooksd5 <- cooks.distance(m5L)
sample_size <- nobs(m5L)
plot(cooksd5, pch="*", cex=2, main="Influential Obs by Cooks distance")  # plot cook's distance
abline(h = 4/sample_size, col="red")  # add cutoff line
text(x=1:length(cooksd5)+1, y=cooksd5, labels=ifelse(cooksd5>4/sample_size, names(cooksd5),""), col="red")  # add labels

influential5 <- as.numeric(names(sort(cooksd5, decreasing = TRUE)[1:2]))
ggpredict(lmer(rootshoot ~ CO2*H2OTmt + (1|Plot), data = biomass2. %>% 
                 filter(Spp == "L") %>% 
                 filter(rootshoot<9) %>% 
                 filter(!row_number() %in% influential5) ), terms=c("CO2","H2OTmt")) %>% plot(add.data = TRUE, ci = TRUE)
summary(lmer(rootshoot ~ CO2*H2OTmt + (1|Plot), data = biomass2. %>% 
               filter(Spp == "L") %>% 
               filter(rootshoot <9) %>% 
               filter(!row_number() %in% influential5) )) # nah

plot(simulateResiduals(m5V))
plot(simulateResiduals(m5L))

## come back to this one
# lwc
#summary(lm(lwc ~ CO2Tmt*H2OTmt, data = filter(biomass2.pmeans, Spp == "V" | Spp == "L"))) 
summary(lm(lwc ~ CO2Tmt*H2OTmt, data = filter(biomass2.pmeans, Spp == "L" | Spp == "L")))
summary(lm(lwc ~ CO2Tmt*H2OTmt, data = filter(biomass2.pmeans, Spp == "V" | Spp == "V"))) # H2O < .02, CO2 < .075
#summary(lmer(lwc ~ CO2Tmt*H2OTmt + (1|Plot), data = filter(biomass2., Spp == "V" | Spp == "L")))
summary(lmer(lwc ~ CO2Tmt*H2OTmt + (1|Plot), data = filter(biomass2., Spp == "L" | Spp == "L"))) 
summary(lmer(lwc ~ CO2Tmt*H2OTmt + (1|Plot), data = filter(biomass2., Spp == "V" | Spp == "V"))) # H2O < 0.025, CO2 < 0.1
#summary(lm(lwc ~ rescale(CO2)*rescale(meanSWC), data = filter(biomass2.pmeans, Spp == "L" | Spp == "V"))) 
summary(lm(lwc ~ rescale(CO2)*rescale(meanSWC), data = filter(biomass2.pmeans, Spp == "L" | Spp == "L"))) 
summary(lm(lwc ~ rescale(CO2)*rescale(meanSWC), data = filter(biomass2.pmeans, Spp == "V" | Spp == "V"))) # SWC < .075
#summary(lmer(lwc ~ rescale(CO2)*rescale(meanSWC) + (1|Plot), data = filter(biomass2., Spp == "L" | Spp == "V"))) 
summary(lmer(lwc ~ rescale(CO2)*rescale(meanSWC) + (1|Plot), data = filter(biomass2., Spp == "L" | Spp == "L"))) 
summary(lmer(lwc ~ rescale(CO2)*rescale(meanSWC) + (1|Plot), data = filter(biomass2., Spp == "V" | Spp == "V"))) # SWC < .07
m6 <- lmer(lwc ~ CO2+H2OTmt + (1|Plot), data = filter(biomass2., Spp == "V" | Spp == "V"))
ggpredict(m6, terms=c("CO2","H2OTmt")) %>% 
  plot(add.data = TRUE, ci = TRUE)

cooksd6 <- cooks.distance(m6L)
sample_size <- nobs(m6L)
plot(cooksd6, pch="*", cex=2, main="Influential Obs by Cooks distance")  # plot cook's distance
abline(h = 4/sample_size, col="red")  # add cutoff line
text(x=1:length(cooksd6)+1, y=cooksd6, labels=ifelse(cooksd6>4/sample_size, names(cooksd6),""), col="red")  # add labels

influential6 <- as.numeric(names(cooksd6)[(cooksd6 > (4/sample_size))])
ggpredict(lmer(lwc ~ CO2*H2OTmt + (1|Plot), data = biomass2. %>% 
                 filter(Spp == "L") %>% 
                 filter(!row_number() %in% influential6) ), terms=c("CO2","H2OTmt")) %>% plot(add.data = TRUE, ci = TRUE)
summary(lmer(lwc ~ CO2*H2OTmt + (1|Plot), data = biomass2. %>% 
               filter(Spp == "L") %>% 
               filter(!row_number() %in% influential6) )) # nada

plot(simulateResiduals(m6V))
plot(simulateResiduals(m6L)) # quantile deviations detected; combine adjusted quantile test significant
   
# rootimage. : ratio_2to1, dia_range_1 or w/e, # branchpoints
rootimage.pmeans <- rootimage. %>% 
  group_by(Plot, CO2Tmt, H2OTmt, Spp) %>% 
  summarise(across(c(CO2, meanSWC, ratio_2to1, Root.Length.Diameter.Range.1.mm, Number.of.Branch.Points), ~ mean(.x, na.rm=TRUE)))

#summary(lm(ratio_2to1 ~ CO2Tmt*H2OTmt, data = filter(rootimage.pmeans, Spp == "V" | Spp == "L"))) 
summary(lm(log(ratio_2to1) ~ CO2Tmt*H2OTmt, data = filter(rootimage.pmeans, Spp == "L" | Spp == "L" & ratio_2to1>0.01))) # CO2 <.07
summary(lm(log(ratio_2to1) ~ CO2Tmt*H2OTmt, data = filter(rootimage.pmeans, Spp == "V" | Spp == "V" & ratio_2to1>0.01))) 
#summary(lmer(ratio_2to1 ~ CO2Tmt*H2OTmt + (1|Plot), data = filter(rootimage., Spp == "V" | Spp == "L"))) 
summary(lmer(log(ratio_2to1) ~ CO2Tmt*H2OTmt + (1|Plot), data = filter(rootimage., Spp == "L" | Spp == "L" & ratio_2to1>0.01))) # CO2 < .09
summary(lmer(log(ratio_2to1) ~ CO2Tmt*H2OTmt + (1|Plot), data = filter(rootimage., Spp == "V" | Spp == "V" & ratio_2to1>0.01))) 
#summary(lm(ratio_2to1 ~ rescale(CO2)*rescale(meanSWC), data = filter(rootimage.pmeans, Spp == "L" | Spp == "V"))) 
summary(lm(log(ratio_2to1) ~ rescale(CO2)*rescale(meanSWC), data = filter(rootimage.pmeans, Spp == "L" | Spp == "L" & ratio_2to1>0.01))) # CO2 <.03, intx<.08
summary(lm(log(ratio_2to1) ~ rescale(CO2)*rescale(meanSWC), data = filter(rootimage.pmeans, Spp == "V" | Spp == "V" & ratio_2to1>0.01))) 
#summary(lmer(ratio_2to1 ~ rescale(CO2)*rescale(meanSWC) + (1|Plot), data = filter(rootimage., Spp == "L" | Spp == "V"))) 
summary(lmer(log(ratio_2to1) ~ rescale(CO2)*rescale(meanSWC) + (1|Plot), data = filter(rootimage., Spp == "L" | Spp == "L" & ratio_2to1>0.01))) # CO2 < .06
summary(lmer(log(ratio_2to1) ~ rescale(CO2)*rescale(meanSWC) + (1|Plot), data = filter(rootimage., Spp == "V" | Spp == "V" & ratio_2to1>0.01)))
m7 <- lmer(log(SRL) ~ CO2*H2OTmt + (1|Plot), data = filter(rootimage., Spp == "V" & SRL<1500))
ggpredict(m7, terms=c("CO2","H2OTmt")) %>% 
  plot(add.data = TRUE, ci = TRUE)

cooksd7 <- cooks.distance(m7)
sample_size <- nobs(m7)
plot(cooksd7, pch="*", cex=2, main="Influential Obs by Cooks distance")  # plot cook's distance
abline(h = 4/sample_size, col="red")  # add cutoff line
text(x=1:length(cooksd7)+1, y=cooksd7, labels=ifelse(cooksd7>4/sample_size, names(cooksd7),""), col="red")  # add labels

influential7 <- as.numeric(names(cooksd7)[(cooksd7 > (4/sample_size))])
ggpredict(lmer(log(SRL) ~ CO2*H2OTmt + (1|Plot), data = rootimage. %>% 
                 filter(Spp == "V" & SRL<1500) %>% 
                 filter(!row_number() %in% influential7) ), terms=c("CO2","H2OTmt")) %>% plot(add.data = TRUE, ci = TRUE)
summary(lmer(log(SRL) ~ CO2*H2OTmt + (1|Plot), data = rootimage. %>% 
               filter(Spp == "V" & SRL<1500) %>% 
               filter(!row_number() %in% influential7) )) # something is maybe up with the coarse:fine roots; maybe not

plot(simulateResiduals(m7V))
plot(simulateResiduals(m7L)) # quantile deviations detected (red curves) combined adjusted quantile test n.s.

#summary(lm(Root.Length.Diameter.Range.1.mm ~ CO2Tmt*H2OTmt, data = filter(rootimage.pmeans, Spp == "V" | Spp == "L"))) # H2O <.0024
summary(lm(log(Root.Length.Diameter.Range.1.mm) ~ CO2Tmt*H2OTmt, data = filter(rootimage.pmeans, Spp == "L" | Spp == "L"))) # H2O<.0085
summary(lm(log(Root.Length.Diameter.Range.1.mm) ~ CO2Tmt*H2OTmt, data = filter(rootimage.pmeans, Spp == "V" | Spp == "V"))) # H2O<.02
#summary(lmer(Root.Length.Diameter.Range.1.mm ~ CO2Tmt*H2OTmt + (1|Plot), data = filter(rootimage., Spp == "V" | Spp == "L"))) # H2O < .015
summary(lmer(log(Root.Length.Diameter.Range.1.mm) ~ CO2Tmt*H2OTmt + (1|Plot), data = filter(rootimage., Spp == "L" | Spp == "L"))) # H2O <.025
summary(lmer(log(Root.Length.Diameter.Range.1.mm) ~ CO2Tmt*H2OTmt + (1|Plot), data = filter(rootimage., Spp == "V" | Spp == "V"))) # H2O<.035
#summary(lm(Root.Length.Diameter.Range.1.mm ~ rescale(CO2)*rescale(meanSWC), data = filter(rootimage.pmeans, Spp == "L" | Spp == "V"))) # H2O < .005
summary(lm(log(Root.Length.Diameter.Range.1.mm) ~ rescale(CO2)*rescale(meanSWC), data = filter(rootimage.pmeans, Spp == "L" | Spp == "L"))) # H2O<.015
summary(lm(log(Root.Length.Diameter.Range.1.mm) ~ rescale(CO2)*rescale(meanSWC), data = filter(rootimage.pmeans, Spp == "V" | Spp == "V"))) 
#summary(lmer(Root.Length.Diameter.Range.1.mm ~ rescale(CO2)*rescale(meanSWC) + (1|Plot), data = filter(rootimage., Spp == "L" | Spp == "V"))) # H2O < .025
summary(lmer(log(Root.Length.Diameter.Range.1.mm) ~ rescale(CO2)*rescale(meanSWC) + (1|Plot), data = filter(rootimage., Spp == "L" | Spp == "L"))) # H2O < .02
summary(lmer(Root.Length.Diameter.Range.1.mm ~ rescale(CO2)*rescale(meanSWC) + (1|Plot), data = filter(rootimage., Spp == "V" | Spp == "V")))
m8 <- lm(log(Root.Length.Diameter.Range.1.mm) ~ CO2Tmt*H2OTmt, data = filter(rootimage.pmeans, Spp == "L" | Spp == "L"))
ggpredict(m8L, terms=c("CO2","H2OTmt")) %>% 
  plot(add.data = TRUE, ci = TRUE)

cooksd8 <- cooks.distance(m8L)
sample_size <- nobs(m8L)
plot(cooksd8, pch="*", cex=2, main="Influential Obs by Cooks distance")  # plot cook's distance
abline(h = 4/sample_size, col="red")  # add cutoff line
text(x=1:length(cooksd8)+1, y=cooksd8, labels=ifelse(cooksd8>4/sample_size, names(cooksd8),""), col="red")  # add labels

## go back and filter on spp for the cook's distance filtered plots
influential8 <- as.numeric(names(cooksd8)[(cooksd8 > (4/sample_size))])
ggpredict(lm(log(Root.Length.Diameter.Range.1.mm) ~ CO2*H2OTmt, data = rootimage. %>% 
                 filter(Spp == "L") %>% 
                 filter(!row_number() %in% influential8) ), terms=c("CO2","H2OTmt")) %>% plot(add.data = TRUE, ci = TRUE)
summary(lm(log(Root.Length.Diameter.Range.1.mm) ~ CO2*H2OTmt, data = rootimage. %>% 
               filter(Spp == "L") %>% 
               filter(!row_number() %in% influential8) )) # wet plants have more fine root length

plot(simulateResiduals(m8V))
plot(simulateResiduals(m8L)) # qu = 0.25, log(sigma) = -2.911984 : outer Newton did not converge fully.
# We had to increase `err` for some of the quantiles. See fit$calibr$err
# quantile deviations detected; combine adjusted quantile test significant

#summary(lm(Number.of.Branch.Points ~ CO2Tmt*H2OTmt, data = filter(rootimage.pmeans, Spp == "V" | Spp == "L"))) # H2O <.0065
summary(lm(Number.of.Branch.Points ~ CO2Tmt*H2OTmt, data = filter(rootimage.pmeans, Spp == "L" | Spp == "L"))) # H2O<.05
summary(lm(Number.of.Branch.Points ~ CO2Tmt*H2OTmt, data = filter(rootimage.pmeans, Spp == "V" | Spp == "V"))) # H2O<.055
#summary(lmer(Number.of.Branch.Points ~ CO2Tmt*H2OTmt + (1|Plot), data = filter(rootimage., Spp == "V" | Spp == "L"))) # H2O < .035
summary(lmer(Number.of.Branch.Points ~ CO2Tmt*H2OTmt + (1|Plot), data = filter(rootimage., Spp == "L" | Spp == "L"))) # H2O < .065
summary(lmer(Number.of.Branch.Points ~ CO2Tmt*H2OTmt + (1|Plot), data = filter(rootimage., Spp == "V" | Spp == "V"))) # H2O < .07
#summary(lm(Number.of.Branch.Points ~ rescale(CO2)*rescale(meanSWC), data = filter(rootimage.pmeans, Spp == "L" | Spp == "V"))) # H2O < .035
summary(lm(Number.of.Branch.Points ~ rescale(CO2)*rescale(meanSWC), data = filter(rootimage.pmeans, Spp == "L" | Spp == "L"))) # H2O < .035
summary(lm(Number.of.Branch.Points ~ rescale(CO2)*rescale(meanSWC), data = filter(rootimage.pmeans, Spp == "V" | Spp == "V"))) 
#summary(lmer(Number.of.Branch.Points ~ rescale(CO2)*rescale(meanSWC) + (1|Plot), data = filter(rootimage., Spp == "L" | Spp == "V"))) # H2O < .025
summary(lmer(Number.of.Branch.Points ~ rescale(CO2)*rescale(meanSWC) + (1|Plot), data = filter(rootimage., Spp == "L" | Spp == "L"))) # H2O < .045
summary(lmer(Number.of.Branch.Points ~ rescale(CO2)*rescale(meanSWC) + (1|Plot), data = filter(rootimage., Spp == "V" | Spp == "V")))
m9 <- lm(Number.of.Branch.Points ~ CO2*H2OTmt, data = filter(rootimage.pmeans, Spp == "V" | Spp == "L"))
ggpredict(m9L, terms=c("CO2","H2OTmt")) %>% 
  plot(add.data = TRUE, ci = FALSE)

cooksd9 <- cooks.distance(m9L)
sample_size <- nobs(m9L)
plot(cooksd9, pch="*", cex=2, main="Influential Obs by Cooks distance")  # plot cook's distance
abline(h = 4/sample_size, col="red")  # add cutoff line
text(x=1:length(cooksd9)+1, y=cooksd9, labels=ifelse(cooksd9>4/sample_size, names(cooksd9),""), col="red")  # add labels

influential9 <- as.numeric(names(cooksd9)[(cooksd9 > (4/sample_size))])
ggpredict(lmer(log(Number.of.Branch.Points) ~ CO2*H2OTmt + (1|Plot), data = rootimage. %>% 
                 filter(Spp == "L" & Number.of.Branch.Points > 5) %>% 
                 filter(!row_number() %in% influential9) ), terms=c("CO2","H2OTmt")) %>% plot(add.data = TRUE, ci = TRUE)
summary(lmer(log(Number.of.Branch.Points) ~ CO2*H2OTmt + (1|Plot), data = rootimage. %>% 
               filter(Spp == "L" & Number.of.Branch.Points > 5) %>% 
               filter(!row_number() %in% influential9) )) # wet plants have more branchy roots

plot(simulateResiduals(m9V))
plot(simulateResiduals(m9L))

# SIF. : d13C
SIF.pmeans <- SIF. %>% 
  group_by(Plot, CO2Tmt, H2OTmt, Spp) %>% 
  summarise(across(c(CO2, meanSWC, d13C), ~ mean(.x, na.rm=TRUE)))

#summary(lm(d13C ~ CO2Tmt*H2OTmt, data = filter(SIF.pmeans, Spp == "V" | Spp == "L"))) # H2O <.03, CO2<.035
summary(lm(d13C ~ CO2Tmt*H2OTmt, data = filter(SIF.pmeans, Spp == "L" | Spp == "L"))) 
summary(lm(d13C ~ CO2Tmt*H2OTmt, data = filter(SIF.pmeans, Spp == "V" | Spp == "V"))) # H2O<.07, CO2<.025
#summary(lmer(d13C ~ CO2Tmt*H2OTmt + (1|Plot), data = filter(SIF., Spp == "V" | Spp == "L"))) # H2O <.095, CO2<.075
summary(lmer(d13C ~ CO2Tmt*H2OTmt + (1|Plot), data = filter(SIF., Spp == "L" | Spp == "L"))) 
summary(lmer(d13C ~ CO2Tmt*H2OTmt + (1|Plot), data = filter(SIF., Spp == "V" | Spp == "V"))) # CO2 < .025, H2O<.07
#summary(lm(d13C ~ rescale(CO2)*rescale(meanSWC), data = filter(SIF.pmeans, Spp == "L" | Spp == "V"))) # SWC < .075
summary(lm(d13C ~ rescale(CO2)*rescale(meanSWC), data = filter(SIF.pmeans, Spp == "L" | Spp == "L"))) 
summary(lm(d13C ~ rescale(CO2)*rescale(meanSWC), data = filter(SIF.pmeans, Spp == "V" | Spp == "V"))) # CO2 < .085
#summary(lmer(d13C ~ rescale(CO2)*rescale(meanSWC) + (1|Plot), data = filter(SIF., Spp == "L" | Spp == "V"))) 
summary(lmer(d13C ~ rescale(CO2)*rescale(meanSWC) + (1|Plot), data = filter(SIF., Spp == "L" | Spp == "L"))) 
summary(lmer(d13C ~ rescale(CO2)*rescale(meanSWC) + (1|Plot), data = filter(SIF., Spp == "V" | Spp == "V"))) # CO2<.08
m10 <- lm(d13C ~ CO2*H2OTmt, data = filter(SIF.pmeans, Spp == "V" | Spp == "L"))
ggpredict(m10L, terms=c("CO2","H2OTmt")) %>% 
  plot(add.data = TRUE, ci = TRUE)

cooksd10 <- cooks.distance(m10L)
sample_size <- nobs(m10L)
plot(cooksd10, pch="*", cex=2, main="Influential Obs by Cooks distance")  # plot cook's distance
abline(h = 4/sample_size, col="red")  # add cutoff line
text(x=1:length(cooksd10)+1, y=cooksd10, labels=ifelse(cooksd10>4/sample_size, names(cooksd10),""), col="red")  # add labels

influential10 <- as.numeric(names(cooksd10)[(cooksd10 > (4/sample_size))])
ggpredict(lmer(d13C ~ CO2*H2OTmt + (1|Plot), data = SIF. %>% 
                    filter(Spp == "L") %>% 
                 filter(!row_number() %in% influential10) ), terms=c("CO2","H2OTmt")) %>% plot(add.data = TRUE, ci = TRUE)
summary(lmer(d13C ~ CO2*H2OTmt + (1|Plot), data = SIF. %>% 
                filter(Spp == "L") %>% 
               filter(!row_number() %in% influential10) )) # CO2 < .1; eCO2 less water stressed per d13C

plot(simulateResiduals(m10V))
plot(simulateResiduals(m10L))


# Pick PERMANOVA variables based on the above analysis
# Photo.y, totmass, rootshoot, lwc(?), ratio_2to1
library(tidyr)
# combine dataframes and rescale responses
permanova.df <- biomass2. %>% 
  dplyr::select(Plot, Spp, Code, LeafWet_expanded, StemWet_expanded, rootmass_g, totmass, rootshoot, lwc, CO2Tmt, H2OTmt, CO2, meanSWC) %>% 
  left_join(LiCOR_df.tfm[,c("Code","Photo.y","Cond.y", "WUE.350tfm")], by = "Code") %>% 
  left_join(rootimage.[,c("Code","ratio_2to1", "Root.Length.Diameter.Range.1.mm","Number.of.Branch.Points")], by = "Code") %>% 
  # filter(!is.na(Photo.y)) %>% 
  # filter(!is.na(rootshoot)) %>% 
  left_join(SIF.[,c("Code","d13C")]) %>% 
 # mutate(logCond.y = log(Cond.y), sqrt_totmass = sqrt(totmass), logRootRatio = log(ratio_2to1), logFineRoot = log(Root.Length.Diameter.Range.1.mm), logBranch=log(Number.of.Branch.Points)) %>% 
  mutate(across(where(is.numeric), ~ rescale(.x))) %>% 
  left_join(lookup, by = "Plot") 

nrow(biomass2.) # 127
nrow(LiCOR_df.tfm) # 73
nrow(rootimage.) # 127
nrow(SIF.) # 91

permanova.dfL <- permanova.df %>% 
  filter(Spp=="L")
permanova.dfV <- permanova.df %>% 
  filter(Spp=="V")

# select variables for response matrix
# all of them
response_matrix. <- permanova.df %>% dplyr::select(where(is.numeric)) %>% dplyr::select(!c(CO2, meanSWC)) # 127
response_matrix.L <- permanova.dfL %>% dplyr::select(where(is.numeric)) %>% dplyr::select(!c(CO2, meanSWC)) # 66
response_matrix.V <- permanova.dfV %>% dplyr::select(where(is.numeric)) %>% dplyr::select(!c(CO2, meanSWC)) # 61
colnames(response_matrix.)
# [1] "LeafWet_expanded"                "StemWet_expanded"                "rootmass_g"                      "totmass"                        
# [5] "rootshoot"                       "lwc"                             "Photo.y"                         "Cond.y"                         
# [9] "WUE.350tfm"                      "ratio_2to1"                      "Root.Length.Diameter.Range.1.mm" "Number.of.Branch.Points"  

adonis2(response_matrix.[complete.cases(response_matrix.),] ~ CO2*H2OTmt, permanova.df[complete.cases(response_matrix.),], na.rm=TRUE, method="mahalanobis")
adonis2(response_matrix.L[complete.cases(response_matrix.L),] ~ CO2*H2OTmt, permanova.dfL[complete.cases(response_matrix.L),], na.rm=TRUE, method="mahalanobis")
adonis2(response_matrix.V[complete.cases(response_matrix.V),] ~ CO2*H2OTmt, permanova.dfV[complete.cases(response_matrix.V),], na.rm=TRUE, method="mahalanobis")
colnames(response_matrix.)

adonis2(response_matrix.[complete.cases(response_matrix.[,-c(6,9:12)]),-c(6,9:12)] ~ CO2*H2OTmt, permanova.df[complete.cases(response_matrix.[,-c(6,9:12)]),], na.rm=TRUE, method="mahalanobis")
adonis2(response_matrix.L[complete.cases(response_matrix.L[,-c(6,9:12)]),-c(6,9:12)] ~ CO2*H2OTmt, permanova.dfL[complete.cases(response_matrix.L[,-c(6,9:12)]),], na.rm=TRUE, method="mahalanobis")
adonis2(response_matrix.V[complete.cases(response_matrix.V[,-c(6,9:12)]),-c(6,9:12)] ~ CO2*H2OTmt, permanova.dfV[complete.cases(response_matrix.V[,-c(6,9:12)]),], na.rm=TRUE, method="mahalanobis")
colnames(response_matrix.[,-c(6,9:12)])
# "LeafWet_expanded" "StemWet_expanded" "rootmass_g"       "totmass"          "rootshoot"        "Photo.y"          "Cond.y"   
# marginal

adonis2(response_matrix.[complete.cases(response_matrix.[,-c(1,2,6,9:12)]),-c(1,2,6,9:12)] ~ CO2*H2OTmt, permanova.df[complete.cases(response_matrix.[,-c(1,2,6,9:12)]),], na.rm=TRUE)
adonis2(response_matrix.L[complete.cases(response_matrix.L[,-c(1,2,6,9:12)]),-c(1,2,6,9:12)] ~ CO2*H2OTmt, permanova.dfL[complete.cases(response_matrix.L[,-c(1,2,6,9:12)]),], na.rm=TRUE)
adonis2(response_matrix.V[complete.cases(response_matrix.V[,-c(1,2,6,9:12)]),-c(1,2,6,9:12)] ~ CO2*H2OTmt, permanova.dfV[complete.cases(response_matrix.V[,-c(1,2,6,9:12)]),], na.rm=TRUE)
colnames(response_matrix.[,-c(1,2,6,9:12)])
# "rootmass_g" "totmass"    "rootshoot"  "Photo.y"    "Cond.y"   # was marginal

adonis2(response_matrix.[complete.cases(response_matrix.[,-c(1:3,6,9:12)]),-c(1:3,6,9:12)] ~ CO2*H2OTmt, permanova.df[complete.cases(response_matrix.[,-c(1:3,6,9:12)]),], na.rm=TRUE, method="mahalanobis")
adonis2(response_matrix.L[complete.cases(response_matrix.L[,-c(1:3,6,9:12)]),-c(1:3,6,9:12)] ~ CO2*H2OTmt, permanova.dfL[complete.cases(response_matrix.L[,-c(1:3,6,9:12)]),], na.rm=TRUE, method="mahalanobis")
adonis2(response_matrix.V[complete.cases(response_matrix.V[,-c(1:3,6,9:12)]),-c(1:3,6,9:12)] ~ CO2*H2OTmt, permanova.dfV[complete.cases(response_matrix.V[,-c(1:3,6,9:12)]),], na.rm=TRUE, method="mahalanobis")
colnames(response_matrix.[,-c(1:3,6,9:12)])
# "totmass"   "rootshoot" "Photo.y"   "Cond.y"   # was 0.45-0.6

adonis2(response_matrix.[complete.cases(response_matrix.[,-c(1,2,4,6,9:12)]),-c(1,2,4,6,9:12)] ~ CO2*H2OTmt, permanova.df[complete.cases(response_matrix.[,-c(1,2,4,6,9:12)]),], na.rm=TRUE, method="mahalanobis")
adonis2(response_matrix.L[complete.cases(response_matrix.L[,-c(1,2,4,6,9:12)]),-c(1,2,4,6,9:12)] ~ CO2*H2OTmt, permanova.dfL[complete.cases(response_matrix.L[,-c(1,2,4,6,9:12)]),], na.rm=TRUE, method="mahalanobis")
adonis2(response_matrix.V[complete.cases(response_matrix.V[,-c(1,2,4,6,9:12)]),-c(1,2,4,6,9:12)] ~ CO2*H2OTmt, permanova.dfV[complete.cases(response_matrix.V[,-c(1,2,4,6,9:12)]),], na.rm=TRUE, method="mahalanobis")
colnames(response_matrix.[,-c(1,2,4,6,9:12)])
# "rootmass_g" "rootshoot"  "Photo.y"    "Cond.y"  # was marginal

adonis2(response_matrix.[complete.cases(response_matrix.[,c(7,2,1,3,13)]),c(7,2,1,3,13)] ~ CO2*H2OTmt, permanova.df[complete.cases(response_matrix.[,c(7,2,1,3,13)]),], na.rm=TRUE, method="mahalanobis")
adonis2(response_matrix.L[complete.cases(response_matrix.L[,c(7,2,1,3,13)]),c(7,2,1,3,13)] ~ CO2*H2OTmt, permanova.dfL[complete.cases(response_matrix.L[,c(7,2,1,3,13)]),], na.rm=TRUE, method="mahalanobis")
adonis2(response_matrix.V[complete.cases(response_matrix.V[,c(7,2,1,3,13)]),c(7,2,1,3,13)] ~ CO2*H2OTmt, permanova.dfV[complete.cases(response_matrix.V[,c(7,2,1,3,13)]),], na.rm=TRUE, method="mahalanobis")
colnames(response_matrix.[,c(7,2,1,3,13)])

adonis2(response_matrix.[complete.cases(response_matrix.[,-c(9:12)]),-c(9:12)] ~ CO2*H2OTmt, permanova.df[complete.cases(response_matrix.[,-c(9:12)]),], na.rm=TRUE, method="mahalanobis")
adonis2(response_matrix.L[complete.cases(response_matrix.L[,-c(9:12)]),-c(9:12)] ~ CO2*H2OTmt, permanova.dfL[complete.cases(response_matrix.L[,-c(9:12)]),], na.rm=TRUE, method="mahalanobis")
adonis2(response_matrix.V[complete.cases(response_matrix.V[,-c(9:12)]),-c(9:12)] ~ CO2*H2OTmt, permanova.dfV[complete.cases(response_matrix.V[,-c(9:12)]),], na.rm=TRUE, method="mahalanobis")
colnames(response_matrix.[,-c(9:12)]) # intx .021
colnames(response_matrix.[,-c(9:11)]) # intx .05
# [1] "LeafWet_expanded" "StemWet_expanded" "rootmass_g"       "totmass"          "rootshoot"       
# [6] "lwc"              "Photo.y"          "Cond.y"  


# this one is significant (.05); n = 17
adonis2(response_matrix.V[complete.cases(response_matrix.V[,-c(9:11)]),-c(9:11)] ~ CO2Tmt*H2OTmt, permanova.dfV[complete.cases(response_matrix.V[,-c(9:11)]),], na.rm=TRUE, method="mahalanobis")
adonis2(response_matrix.V[complete.cases(response_matrix.V[,-c(9:11)]),-c(9:11)] ~ CO2*H2OTmt, permanova.dfV[complete.cases(response_matrix.V[,-c(9:11)]),], na.rm=TRUE, method="mahalanobis") # marginal

X = response_matrix.V[,-c(1:3,6,9:12)] 
X=PERMANOVA::IniTransform(X)
D = PERMANOVA::DistContinuous(X)
V.permanova = PERMANOVA::PERMANOVA(D,as.factor(permanova.dfV$Tmt), CoordPrinc=TRUE)
PERMANOVA::plot.PERMANOVA(V.permanova, ColorGroup = c("pink", "lightblue", "red", "blue"), VoronoiColor = c("pink", "lightblue", "red", "blue"))


# I heard a MANOVA has more power, so long as your data is normally distributed, which I've already ensured
# now doing manova: transformed data, plot means
# 5/19 add normalized 1/variance term for weighted plot means
## filter out SRL > 1500: what is that data point doing?
## add n = to plot means, this may account for exceptionally low variance in cases where n = 1
## go undo seedlings you took off the herbivory list :| (see if there's still enough data left to work with)

# original version for arithmetic plot means
manova.df <- biomass2. %>% 
  dplyr::select(Plot, Spp, Code, LeafWet_expanded, StemWet_expanded, rootmass_g, totmass, rootshoot, lwc, CO2Tmt, H2OTmt, CO2, meanSWC) %>% 
  left_join(LiCOR_df.[,c("Code","Photo.y","Cond.y", "WUE.350")], by = "Code") %>% 
  left_join(rootimage.[,c("Code","SRL", "Root.Length.Diameter.Range.1.mm","Number.of.Branch.Points")], by = "Code") %>% 
  left_join(SIF.[,c("Code","d13C")]) %>% 
  group_by(Plot, Spp) %>% 
  summarise(across(where(is.numeric), ~ mean(.x, na.rm=TRUE))) %>% 
  ungroup() %>% 
  mutate(logCond.y = log(Cond.y), sqrt_WUE.350 = sqrt(WUE.350), sqrt_totmass = sqrt(totmass), logSRL = log(SRL), logFineRoot = log(Root.Length.Diameter.Range.1.mm), logBranch=log(Number.of.Branch.Points)) %>% 
  left_join(lookup, by = "Plot") %>% 
  mutate(H2OTmt = substr(Tmt,2,2), CO2Tmt = substr(Tmt,1,1)) 
#write.csv(manova.df, "QuailFACE_plotmeans.csv")

# I can write my own function
# function on a list of numbers that calculates their 1/sigma^2; then get column sum and divide each by that sum so they add up to one. Last, use these as the weights in univariate models. Still have to figure out what to do about MANOVA; can use just one variable's weights (Photo.y) or some kind of combined? Do this on a smaller dataset to keep things simple: Photo.y, Cond.y, rootshoot, logSRL
one_over_var <- function(x){
  1/(sd(x, na.rm=TRUE)^2)
}

manova.df.small0 <- biomass2. %>% 
  filter(Spp =="V") %>% 
  dplyr::select(Plot, Spp, Code, rootshoot, CO2, meanSWC) %>% 
  left_join(LiCOR_df.[,c("Code","Photo.y","Cond.y")], by = "Code") %>% 
  left_join(rootimage.[,c("Code","SRL")], by = "Code") %>% 
  mutate(logCond.y = log(Cond.y), logSRL = log(SRL)) %>%
  group_by(Plot, Spp) %>% 
  mutate(n = n()) %>% 
  mutate_at(c("Photo.y", "rootshoot", "logCond.y", "logSRL"), list(oov=one_over_var)) %>%
  summarise(across(where(is.numeric), ~ mean(.x, na.rm=TRUE))) %>% 
  ungroup() %>% 
  left_join(lookup, by = "Plot") %>% 
  mutate(H2OTmt = substr(Tmt,2,2), CO2Tmt = substr(Tmt,1,1)) 
# for plot 3, L has n = 1, but measurements for all data types
# assign it a weight based on the variance of all other points from its treatment group (AD):

biomass2. %>% 
  filter(Spp =="L") %>% 
  dplyr::select(Plot, Spp, Code, rootshoot, CO2, meanSWC) %>% 
  left_join(LiCOR_df.[,c("Code","Photo.y","Cond.y")], by = "Code") %>% 
  left_join(rootimage.[,c("Code","SRL")], by = "Code") %>% 
  mutate(logCond.y = log(Cond.y), logSRL = log(SRL)) %>%
  left_join(lookup, by = "Plot") %>% 
  filter(Tmt == "AD") %>% # get 1/var
  summarise_at(c("Photo.y", "rootshoot", "logCond.y", "logSRL"), ~ 1/sd(.x, na.rm = T)^2)
# add this to 1/var columns in row for plot 3, then do next step to scale 1/var and get weights

#manova.df.small0[manova.df.small0$Plot=="3",12:15] <- list(0.02501195,  30.50882,  1.673602, 4.632909)
# only do this for L !!

manova.df.small <- manova.df.small0 %>% 
  mutate(Photo.y_w = Photo.y_oov/(sum(manova.df.small0$Photo.y_oov, na.rm = T)),
         logCond.y_w = logCond.y_oov/(sum(manova.df.small0$logCond.y_oov, na.rm = T)),
         rootshoot_w = rootshoot_oov/(sum(manova.df.small0$rootshoot_oov, na.rm = T) ),
         logSRL_w = logSRL_oov/(sum(manova.df.small0$logSRL_oov, na.rm = T))) %>% 
  mutate(all_w = Photo.y_w+logCond.y_w+rootshoot_w+logSRL_w) %>% 
  mutate(all_w = all_w/sum(all_w, na.rm=T)) %>% 
  mutate(quad_w = sqrt((Photo.y_w^2)+(logCond.y_w^2)+(rootshoot_w^2)+(logSRL_w^2)) ) %>% 
  mutate(quad_w = quad_w/sum(quad_w, na.rm=T)) 

small.cbind <- cbind(unlist(manova.df.small$Photo.y_w),unlist(manova.df.small$logCond.y_w),unlist(manova.df.small$rootshoot),unlist(manova.df.small$logSRL_w))

# the weights come from inverse variance; check for outliers
manova.df.small00 <- biomass2. %>% 
  filter(Spp =="L") %>% 
  dplyr::select(Plot, Spp, Code, rootshoot, CO2, meanSWC) %>% 
  left_join(LiCOR_df.[,c("Code","Photo.y","Cond.y")], by = "Code") %>% 
  left_join(rootimage.[,c("Code","SRL")], by = "Code") %>% 
  mutate(logCond.y = log(Cond.y), logSRL = log(SRL)) %>% 
  left_join(lookup, by = "Plot") %>% 
  mutate(H2OTmt = substr(Tmt,2,2), CO2Tmt = substr(Tmt,1,1)) 
ggplot(manova.df.small00) +
  geom_point(aes(x=Plot, y=Photo.y, color=Tmt)) + 
  scale_color_manual(values = c("pink", "lightblue", "red", "blue"))

# first try the weights in an lm, with plot means, for V:
summary(lm(Photo.y ~ CO2*H2OTmt , data = manova.df.small, weights = Photo.y_w)) # ** for CO2, ** for water, . for intx
summary(lm(Photo.y ~ CO2*H2OTmt , data = manova.df.small)) # unweighted, NS
summary(lm(Photo.y ~ CO2*H2OTmt , data = manova.df.small, weights = all_w)) # combined weight, NS
summary(lm(Photo.y ~ CO2*H2OTmt , data = manova.df.small, weights = quad_w)) # quadrature NS

summary(lm(logCond.y ~ CO2*H2OTmt , data = manova.df.small, weights = logCond.y_w)) # NS
summary(lm(logCond.y ~ CO2*H2OTmt , data = manova.df.small)) # **, **, *
summary(lm(logCond.y ~ CO2*H2OTmt , data = manova.df.small, weights = Photo.y_w)) # CO2***, H2O***, intx*
summary(lm(logCond.y ~ CO2*H2OTmt , data = manova.df.small, weights = all_w)) # *, **, *
summary(lm(logCond.y ~ CO2*H2OTmt , data = manova.df.small, weights = quad_w)) # *, **, *

summary(lm(rootshoot ~ CO2*H2OTmt , data = manova.df.small, weights = rootshoot_w)) # CO2 .
summary(lm(rootshoot ~ CO2*H2OTmt , data = manova.df.small, weights = Photo.y_w)) # CO2 **
summary(lm(rootshoot ~ CO2*H2OTmt , data = manova.df.small, weights = all_w)) # CO2 .
summary(lm(rootshoot ~ CO2*H2OTmt , data = manova.df.small, weights = quad_w)) # CO2 *

summary(lm(logSRL ~ CO2*H2OTmt , data = manova.df.small, weights = logSRL_w)) # NS
summary(lm(logSRL ~ CO2*H2OTmt , data = manova.df.small, weights = Photo.y_w)) # NS
summary(lm(logSRL ~ CO2*H2OTmt , data = manova.df.small, weights = all_w)) # NS
summary(lm(logSRL ~ CO2*H2OTmt , data = manova.df.small, weights = quad_w)) # NS

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


# now for L: (HAVE TO RE RUN manova.df.small)
summary(lm(Photo.y ~ CO2*H2OTmt , data = manova.df.small, weights = Photo.y_w)) # *** for CO2, ** for water, * for intx
summary(lm(Photo.y ~ CO2*H2OTmt , data = manova.df.small)) # unweighted, NS

summary(lm(logCond.y ~ CO2*H2OTmt , data = manova.df.small, weights = logCond.y_w)) # NS
summary(lm(logCond.y ~ CO2*H2OTmt , data = manova.df.small)) # NS
summary(lm(logCond.y ~ CO2*H2OTmt , data = manova.df.small, weights = Photo.y_w)) # CO2***, H2O*, intxNS

summary(lm(rootshoot ~ CO2*H2OTmt , data = manova.df.small, weights = rootshoot_w)) # CO2 *
summary(lm(rootshoot ~ CO2*H2OTmt , data = manova.df.small, weights = Photo.y_w)) # CO2 ***

summary(lm(logSRL ~ CO2*H2OTmt , data = manova.df.small, weights = logSRL_w)) # CO2 .
summary(lm(logSRL ~ CO2*H2OTmt , data = manova.df.small, weights = Photo.y_w)) # CO2***, H2O., intx**

# reasonable subset: Photo.y, logCond.y, sqrt_totmass, rootshoot
summary(manova(small.cbind ~ CO2Tmt*H2OTmt, manova.df.small, weights = Photo.y_w))
# with weights from just Photo.y_w, all p < .05


  

### 
# This was regular manova without weights:

manova.dfL <- manova.df %>% 
  filter(Spp=="L") %>% 
  dplyr::select(Photo.y, logCond.y, sqrt_WUE.350, sqrt_totmass, rootshoot, lwc, logSRL, logFineRoot) %>% 
  mutate_at(vars("Photo.y", "logCond.y", "sqrt_WUE.350", "sqrt_totmass", "rootshoot", "lwc", "logSRL", "logFineRoot"), list(unlist)) 

manova.dfV <- manova.df %>% 
  filter(Spp=="V") %>% 
  dplyr::select(Photo.y, logCond.y, sqrt_WUE.350, sqrt_totmass, rootshoot, lwc, logSRL, logFineRoot) %>% 
  mutate_at(vars("Photo.y", "logCond.y", "sqrt_WUE.350", "sqrt_totmass", "rootshoot", "lwc", "logSRL", "logFineRoot"), list(unlist)) 

manova.cbind <- cbind(unlist(manova.df$Photo.y),unlist(manova.df$logCond.y),unlist(manova.df$sqrt_WUE.350),unlist(manova.df$sqrt_totmass),unlist(manova.df$rootshoot),unlist(manova.df$lwc),unlist(manova.df$logSRL),unlist(manova.df$logFineRoot))
manova.cbindL <- cbind(unlist(manova.dfL$Photo.y),unlist(manova.dfL$logCond.y),unlist(manova.dfL$sqrt_WUE.350),unlist(manova.dfL$sqrt_totmass),unlist(manova.dfL$rootshoot),unlist(manova.dfL$lwc),unlist(manova.dfL$logSRL),unlist(manova.dfL$logFineRoot))
manova.cbindV <- cbind(unlist(manova.dfV$Photo.y),unlist(manova.dfV$logCond.y),unlist(manova.dfV$sqrt_WUE.350),unlist(manova.dfV$sqrt_totmass),unlist(manova.dfV$rootshoot),unlist(manova.dfV$lwc),unlist(manova.dfV$logSRL),unlist(manova.dfV$logFineRoot))

# reasonable subset: Photo.y, logCond.y, sqrt_totmass, rootshoot
summary(manova(manova.cbind[,c(1,2,4,5)] ~ CO2Tmt*H2OTmt, manova.df, weights = manova.df[,c(3,4,6,7)]))
summary(manova(manova.cbindL[,c(1,2,4,5)] ~ CO2Tmt*H2OTmt, filter(manova.df, Spp=="L")))
summary(manova(manova.cbindV[,c(1,2,4,5)] ~ CO2Tmt*H2OTmt, filter(manova.df, Spp=="V")))

# ummm why is it not like this anymore
#  summary(manova(manova.cbind[,c(1,2,4,5)] ~ CO2Tmt*H2OTmt, manova.df))
#                 Df  Pillai approx F num Df den Df    Pr(>F)    
#   CO2Tmt         1 0.36876    3.067      4     21  0.038881 *  
#   H2OTmt         1 0.91197   54.391      4     21 8.765e-11 ***
#   CO2Tmt:H2OTmt  1 0.48919    5.028      4     21  0.005304 ** 
#   Residuals     24         

# kitchen sink oh wait this did not work bc too many variables
summary(manova(manova.cbind[,c(1,2,5,7)] ~ CO2Tmt*H2OTmt, manova.df))
summary(manova(manova.cbindL[,c(1,2,5,7)] ~ CO2Tmt*H2OTmt, filter(manova.df, Spp=="L")))
summary(manova(manova.cbindV[,c(1,2,5,7)] ~ CO2Tmt*H2OTmt, filter(manova.df, Spp=="V")))

## this time try it without taking plot means

manova.df2 <- biomass2. %>% 
  dplyr::select(Plot, Spp, Code, LeafWet_expanded, StemWet_expanded, rootmass_g, totmass, rootshoot, lwc, CO2Tmt, H2OTmt, CO2, meanSWC) %>% 
  left_join(LiCOR_df.[,c("Code","Photo.y","Cond.y", "WUE.350", "CO2Tmt", "H2OTmt")], by = "Code") %>% 
  left_join(rootimage.[,c("Code","SRL", "Root.Length.Diameter.Range.1.mm","Number.of.Branch.Points")], by = "Code") %>% 
  left_join(SIF.[,c("Code","d13C")]) %>% 
  mutate(logCond.y = log(Cond.y), sqrt_WUE.350 = sqrt(WUE.350), sqrt_totmass = sqrt(totmass), logSRL = log(SRL), logFineRoot = log(Root.Length.Diameter.Range.1.mm), logBranch=log(Number.of.Branch.Points)) %>% 
  left_join(lookup, by = "Plot") %>% 
  mutate(H2OTmt = substr(Tmt,2,2), CO2Tmt = substr(Tmt,1,1)) 
#write.csv(manova.df2, "QuailFACE_all.csv")

# check correlation between variables
cor(manova.df2[, c(4:9,14:20)], use="pairwise.complete.obs") %>% 
  ggcorrplot()


manova.df2L <- manova.df2 %>% 
  filter(Spp=="L") %>% 
  dplyr::select(Photo.y, logCond.y, sqrt_WUE.350, sqrt_totmass, rootshoot, lwc, logRootRatio, logFineRoot) %>% 
  mutate_at(vars("Photo.y", "logCond.y", "sqrt_WUE.350", "sqrt_totmass", "rootshoot", "lwc", "logRootRatio", "logFineRoot"), list(unlist)) 

manova.df2V <- manova.df2 %>% 
  filter(Spp=="V") %>% 
  dplyr::select(Photo.y, logCond.y, sqrt_WUE.350, sqrt_totmass, rootshoot, lwc, logRootRatio, logFineRoot) %>% 
  mutate_at(vars("Photo.y", "logCond.y", "sqrt_WUE.350", "sqrt_totmass", "rootshoot", "lwc", "logRootRatio", "logFineRoot"), list(unlist)) 

manova.cbind <- cbind(unlist(manova.df2$Photo.y),unlist(manova.df2$logCond.y),unlist(manova.df2$sqrt_WUE.350),unlist(manova.df2$sqrt_totmass),unlist(manova.df2$rootshoot),unlist(manova.df2$lwc),unlist(manova.df2$logRootRatio),unlist(manova.df2$logFineRoot))
manova.cbind2L <- cbind(unlist(manova.df2L$Photo.y),unlist(manova.df2L$logCond.y),unlist(manova.df2L$sqrt_WUE.350),unlist(manova.df2L$sqrt_totmass),unlist(manova.df2L$rootshoot),unlist(manova.df2L$lwc),unlist(manova.df2L$logRootRatio),unlist(manova.df2L$logFineRoot))
manova.cbind2V <- cbind(unlist(manova.df2V$Photo.y),unlist(manova.df2V$logCond.y),unlist(manova.df2V$sqrt_WUE.350),unlist(manova.df2V$sqrt_totmass),unlist(manova.df2V$rootshoot),unlist(manova.df2V$lwc),unlist(manova.df2V$logRootRatio),unlist(manova.df2V$logFineRoot))

# reasonable subset: Photo.y, logCond.y, sqrt_totmass, rootshoot
summary(manova(manova.cbind[,c(1,2,4,5)] ~ CO2Tmt*H2OTmt, manova.df2))
summary(manova(manova.cbind2L[,c(1,2,4,5)] ~ CO2Tmt*H2OTmt, filter(manova.df2, Spp=="L")))
summary(manova(manova.cbind2V[,c(1,2,4,5)] ~ CO2Tmt*H2OTmt, filter(manova.df2, Spp=="V")))

summary(manova(manova.cbind[,c(1,2,5,7)] ~ CO2Tmt*H2OTmt, manova.df2))
summary(manova(manova.cbind2L[,c(1,2,5,7)] ~ CO2Tmt*H2OTmt, filter(manova.df2, Spp=="L")))
summary(manova(manova.cbind2V[,c(1,2,5,7)] ~ CO2Tmt*H2OTmt, filter(manova.df2, Spp=="V")))