# Ch 3 Results
# AKA Final Round Final Round, Best of Three

# load all data types
## load the data fresh with "." suffix; do steps from Ch3_explore and Ch3_LiCOR. Examine colnames
# need Ch3_LiCOR and Ch3_explore

# plot_SWC. <- LiCOR_all[,c("Plot", "SWC")] %>% 
#   group_by(Plot) %>% 
#   summarise(meanSWC=mean(SWC)) %>% 
#   as.data.frame()
plot_SWC. <- rbind(df3, df4) %>% 
  select(ID, Plot, Date, HHMMSS, Ci, Photo, Cond, Tleaf, PARi, VpdL, CO2R, RH_R, RH_S, SWC, Spp) %>% 
  rbind(select(LiCOR_1, ID, Plot, Date, HHMMSS, Ci, Photo, Cond, Tleaf, PARi, VpdL, CO2R, RH_R, RH_S, SWC, Spp)) %>% 
  rbind(select(LiCOR_2, ID, Plot, Date, HHMMSS, Ci, Photo, Cond, Tleaf, PARi, VpdL, CO2R, RH_R, RH_S, SWC, Spp)) %>% 
  left_join(lookup, by = "Plot") %>% 
  mutate(Tmt = as.factor(Tmt)) %>% 
  filter(!is.na(Tmt)) %>% 
  filter(Ci > 0, Photo > 0) %>% 
  # select(Plot, SWC) %>% 
  select(Plot, SWC, Date) %>% 
  group_by(Plot, Date) %>% 
  summarise(meanSWC=mean(SWC)) %>% 
  group_by(Plot) %>% 
  summarise(meanSWC=mean(meanSWC)) %>% 
  as.data.frame()

# licor_SWC. <- df_all[,c("Plot", "SWC")] %>%
#   group_by(Plot) %>%
#   summarise(licorSWC=mean(SWC)) %>%
#   as.data.frame()
# want to redo this so that the codes in survey 4 get the SWC from survey 4; ones from 3 get 3



LiCOR_df. <- LiCOR_new %>% 
  mutate(WUE = Anet/gs) %>% 
  mutate(CO2Tmt = substring(Tmt,1,1), H2OTmt = substring(Tmt,2,2)) %>% 
  mutate(Code = if_else(nchar(ID) == 4,substr(ID,1,3),substr(ID,1,4))) %>% 
  left_join(plot_CO2., by = "Plot") # %>% 
 # left_join(licor_SWC., by = "Plot")

# biomass2
# jeez too many steps. get this one after Ch3_explore, it is messy but w/e
## add leaf area columns to biomass2 in explore script ##
biomass2. <- biomass2
biomass2.[1,]
biomass2. <- biomass2. %>% 
   mutate(rootshoot = rootmass_g/(StemWet_g + LeafWet_g), totmass = rootmass_g + StemWet_g + LeafWet_g, lwc = (LeafWet_g-LeafDry_g)*100/LeafWet_g) %>% 
 # mutate(totmass = rootmass_g + StemWet_g + LeafWet_g, lwc = (LeafWet_g-LeafDry_g)*100/LeafWet_g) %>% 
#  mutate(RMF = 100*rootmass_g/totmass) %>% 
    # mutate(rootshoot = rootmass_g/(StemWet_expanded + LeafWet_expanded), totmass = rootmass_g + StemWet_expanded + LeafWet_expanded, lwc = (LeafWet_g-LeafDry_g)*100/LeafWet_g) %>% 
  left_join(plot_CO2., by = "Plot") %>% 
  left_join(plot_SWC., by = "Plot") 

##
#biomass2.[which(biomass2.$totmass==47.05),"totmass"] <- NA # filter outlier for totmass

# exclude rootshoot for 6V2 and 8L2, an outlier per outlierTest: was herbivory list but probably should've been in fullherb list
#biomass2.[which(biomass2.$Code=="6V2"),"rootshoot"] <- NA

# don't need to remove lwc for seedlings from herbivory list; water content won't be affected by leaf herbivory, it's proportional
# I will just replace the 0s with NAs
biomass2.[which(biomass2.$lwc ==0),]$lwc <- NA 
# and remove outlier in L
biomass2.[which(biomass2.$Code =="10L3"),]$lwc <- NA 

lai. <- lai #%>% 
#  filter(!Code %in% firstherb$Code) # remove rows of seedlings from herbivory list
# dplyr::select(tot_area) %>% # 21798 for V; 52406 for L
# dplyr::select(SLA) %>% # 12458 for L

##
# lai.[which(lai.$tot_area == 21798.626),]$tot_area <- NA
# lai.[which(lai.$tot_area == 52406.236),]$tot_area <- NA
# lai.[which(lai.$Code == "3L2"),]$SLA <- NA

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

# testing for normality of distribution 
# this is not what you need; you need to test the distribution of residuals of the model, and their variance
library(car)


### combine into one giant, all-inclusive dataset

 final_df <- biomass2. %>% 
  # dplyr::select(Plot, Spp, Code, StemWet_g, totmass, rootshoot, rootmass_g, Ht.mm..5, Ht.mm..8, lwc, CO2, meanSWC) %>% 
   dplyr::select(Plot, Spp, Code, StemWet_g, totmass, rootshoot, rootmass_g, Ht.mm..5, Ht.mm..8, lwc, CO2, meanSWC) %>% 
  left_join(LiCOR_df.[,c("Code","Anet","gs", "WUE", "HHMMSS")], by = "Code") %>% 
  left_join(lai.[,c("Code","avg_area","perim_per_A","tot_area", "SLA")], by = "Code") %>% 
  left_join(rootimage.[,c("Code","SRL")], by = "Code") %>% 
  left_join(SIF.[,c("Code","d13C")], by = "Code") %>% 
#  mutate(logAnet = log(Anet), sqrtgs = sqrt(gs), sqrtTotmass = sqrt(totmass), sqrtAvg_area = sqrt(avg_area), logPerimArea = log(perim_per_A), logSRL = log(SRL)) %>%
  group_by(Plot, Spp) %>% 
  mutate(n = n()) %>% 
  left_join(lookup, by = "Plot") %>% 
  mutate(H2OTmt = substr(Tmt,2,2), CO2Tmt = substr(Tmt,1,1)) %>% 
   ungroup() %>% 
   mutate(time_scaled = rescale(HHMMSS)) 
 
final_df <- left_join(final_df, licor_SWC.update, by = "Code")

# response variables
names(final_df)
# [1] "Plot"        "Spp"         "Code"        "StemWet_g"   "totmass"     "rootshoot"   "rootmass_g"  "Ht.mm..5"   
# [9] "Ht.mm..8"    "lwc"         "CO2"         "meanSWC"     "Anet"        "gs"          "WUE"         "HHMMSS"     
# [17] "licorSWC"    "avg_area"    "perim_per_A" "tot_area"    "SLA"         "SRL"         "d13C"        "n"    
# [25] "Tmt"         "H2OTmt"      "CO2Tmt"      "time_scaled" "ID"          "SWC"  

# want to compare full data with filtered(for herbivory)

final_df_nh <- final_df %>% 
  select(Code, StemWet_g, totmass, rootshoot, Ht.mm..8) %>% 
  filter(!Code %in% c(firstfullherb$Code,"8L2", "6V2")) %>% 
  right_join(select(final_df, !c(totmass, rootshoot, Ht.mm..8)), by="Code") 

final_df_nh[final_df_nh$Code %in% firstherb$Code,]$tot_area <- NA
# now final_df_nh has NAs for stem-dominant variables in *full* herbivory list, and NAs for leaf area in *any* herbivory list

# univariate models for each response variable, with plot as a random effect
# I have taken out the plots where I modeled H2OTmt (vs meanSWC)
library(ggeffects)
library(lme4)
library(lmerTest)
library(scales)
library(lattice)
library(MuMIn)
library(gridExtra)

# Redoing the univariate models with plot means
plotmeans.V <- final_df_nh %>% 
  filter(Spp == "V") %>% 
  mutate(time_scaled = rescale(HHMMSS)) %>% 
  group_by(Plot, H2OTmt, CO2Tmt, meanSWC, CO2) %>% 
  summarise(across(is.numeric, ~ mean(.x, na.rm=T))) %>% 
  rename(licorSWC = SWC) %>% # retain per-measurement SWC values and rename "licorSWC"
  rename(SWC = meanSWC) # now the per-plot all time mean SWC has the same name in this df as the licor-measurement-specific SWC in the final_df
# this will be confusing, sorry; need to run all models in a loop with the same predictor variables. "SWC" in the licor models refers to per-measurement, while "meanSWC" in plot-means level models has referred to average of all sampling dates. 

plotmeans.L <- final_df_nh %>% 
  filter(Spp == "L") %>% 
  mutate(time_scaled = rescale(HHMMSS)) %>% 
  group_by(Plot, H2OTmt, CO2Tmt, meanSWC, CO2) %>%  # adding SWC, time-of-licor SWC
  summarise(across(is.numeric, ~ mean(.x, na.rm=T))) %>% 
  rename(licorSWC = SWC) %>% # retain per-measurement SWC values and rename "licorSWC"
  rename(SWC = meanSWC)

# and plot means with non-filtered data, just to see
# plotmeans.V2 <- final_df %>% 
#   filter(Spp == "V") %>% 
#   mutate(time_scaled = rescale(HHMMSS)) %>% 
#   group_by(Plot, H2OTmt, CO2Tmt, meanSWC, CO2,) %>% 
#   summarise(across(is.numeric, ~ mean(.x, na.rm=T)))
# plotmeans.L2 <- final_df %>% 
#   filter(Spp == "L") %>% 
#   mutate(time_scaled = rescale(HHMMSS)) %>% 
#   group_by(Plot, H2OTmt, CO2Tmt, meanSWC, CO2,) %>% 
#   summarise(across(is.numeric, ~ mean(.x, na.rm=T)))

# workflow: AICc of full (intx), reduced (additive) and null (~ 1 or ~(1|Plot). Then, interpret results the old way (summary) 
# AICc(lm(totmass ~ rescale(CO2) * rescale(SWC), data = plotmeans.V),
#      lm(totmass ~ rescale(CO2) + rescale(SWC), data = plotmeans.V),
#      lm(totmass ~ 1, data = plotmeans.V) ,
#      lm(totmass ~ rescale(CO2) * rescale(SWC), data = plotmeans.L),
#      lm(totmass ~ rescale(CO2) + rescale(SWC), data = plotmeans.L),
#      lm(totmass ~ 1, data = plotmeans.L))

# Anet
AICc(lmer(Anet~rescale(CO2)*rescale(SWC)+time_scaled + (1|Plot), data=filter(final_df,Spp=="V")), 
     lmer(Anet~rescale(CO2)*rescale(SWC) + (1|Plot), data=filter(final_df,Spp=="V")),
     lmer(Anet~rescale(CO2)+rescale(SWC)+time_scaled + (1|Plot), data=filter(final_df,Spp=="V")),
     lmer(Anet~rescale(CO2)+rescale(SWC) + (1|Plot), data=filter(final_df,Spp=="V")),
     lmer(Anet~ (1|Plot), data=filter(final_df,Spp=="V")))
#                                                                                                      df     AICc
# lmer(Anet ~ rescale(CO2) * rescale(SWC) + time_scaled + (1 | Plot), data = filter(final_df, Spp == "V"), REML = F) 199.9021
# lmer(Anet ~ rescale(CO2) * rescale(SWC) + (1 | Plot), data = filter(final_df, Spp == "V"), REML = F)               202.0897
# lmer(Anet ~ rescale(CO2) + rescale(SWC) + time_scaled + (1 | Plot), data = filter(final_df, Spp == "V"), REML = F) 196.6538 !
# lmer(Anet ~ rescale(CO2) + rescale(SWC) + (1 | Plot), data = filter(final_df, Spp == "V"), REML = F)               199.4203
# lmer(Anet ~ rescale(SWC) + time_scaled + (1 | Plot), data = filter(final_df, Spp == "V"), REML = F)                201.9527
# lmer(Anet ~ rescale(SWC) + (1 | Plot), data = filter(final_df, Spp == "V"), REML = F)                              202.3613
# lmer(Anet ~ (1 | Plot), data = filter(final_df, Spp == "V"))                                                       212.4424
# lmer(Anet ~ rescale(CO2) * rescale(SWC) + time_scaled + (1 | Plot), data = filter(final_df, Spp=="L"))7 201.4709 !
# lmer(Anet ~ rescale(CO2) * rescale(SWC) + (1 | Plot), data = filter(final_df, Spp == "L"))            6 205.0818
# lmer(Anet ~ rescale(CO2) + rescale(SWC) + time_scaled + (1 | Plot), data = filter(final_df, Spp=="L"))6 206.5464
# lmer(Anet ~ rescale(CO2) + rescale(SWC) + (1 | Plot), data = filter(final_df, Spp == "L"))            5 209.6873
# lmer(Anet ~ (1 | Plot), data = filter(final_df, Spp == "L"))                                          3 238.3906
grid.arrange(
  ggpredict(lmer(Anet~rescale(CO2)*rescale(SWC)+time_scaled + (1|Plot), data=filter(final_df,Spp=="V"), REML = F), 
            terms=c("CO2","SWC [4,42]"))%>% plot(rawdata=T,ci=T,colors=c("red","blue")) + labs(title="Anet, V *"),
  ggpredict(lmer(Anet~rescale(CO2)+rescale(SWC)+time_scaled + (1|Plot), data=filter(final_df,Spp=="V"), REML = F), 
            terms=c("CO2","SWC [4,42]"))%>% plot(rawdata=T,ci=T,colors=c("red","blue")) + labs(title="Anet, V +"),
  ggpredict(lm(Anet~rescale(CO2)*rescale(SWC)+time_scaled, data=plotmeans.L),
            terms=c("CO2","SWC [4,42]"))%>% plot(rawdata=T,ci=T,colors=c("red","blue")) + labs(title="Anet, L *"),
  ggpredict(lm(Anet~rescale(CO2)+rescale(SWC)+time_scaled, data=plotmeans.L),
            terms=c("CO2","SWC [4,42]"))%>% plot(rawdata=T,ci=T,colors=c("red","blue")) + labs(title="Anet, L +") )

lmtest::lrtest(lmer(Anet~rescale(CO2)*rescale(SWC)+time_scaled + (1|Plot), data=filter(final_df,Spp=="V"), REML = F), lmer(Anet~rescale(CO2)+rescale(SWC)+time_scaled + (1|Plot), data=filter(final_df,Spp=="V"), REML = F))
lmtest::lrtest(lmer(Anet~rescale(CO2)+rescale(SWC)+time_scaled + (1|Plot), data=filter(final_df,Spp=="V"), REML = F), lmer(Anet~(1|Plot), data=filter(final_df,Spp=="V"), REML = F))
summary(lmer(Anet~rescale(CO2)+rescale(SWC)+time_scaled + (1|Plot), data=filter(final_df,Spp=="V"), REML = F))
Anova(lmer(Anet~rescale(CO2)+rescale(SWC)+time_scaled + (1|Plot), data=filter(final_df,Spp=="V"), REML = F))
#   rescale(CO2)  8.8655  1 10.490 0.0131965 *  
#   rescale(SWC) 25.8083  1 12.148 0.0002605 ***
#   time_scaled   4.7224  1 28.994 0.0380844 *  
plot(lmer(Anet~rescale(CO2)+rescale(SWC)+time_scaled + (1|Plot), data=filter(final_df,Spp=="V"), REML = F))
qqmath(lmer(Anet~rescale(CO2)+rescale(SWC)+time_scaled + (1|Plot), data=filter(final_df,Spp=="V"), REML = F))
shapiro.test(resid(lmer(Anet~rescale(CO2)+rescale(SWC)+time_scaled + (1|Plot), data=filter(final_df,Spp=="V")), REML = F)) # W = 0.96187, p-value = 0.2916

lmtest::lrtest(lm(Anet~rescale(CO2)*rescale(SWC)+time_scaled, data=plotmeans.L), lm(Anet~rescale(CO2)+rescale(SWC)+time_scaled, data=plotmeans.L)) # Chisq 7.4256  p = 0.00643
m9L <- lm(Anet~rescale(CO2)*rescale(SWC)+time_scaled, data=plotmeans.L)
summary(lm(Anet~rescale(CO2)*rescale(SWC)+time_scaled, data=plotmeans.L))
Anova(lm(Anet~rescale(CO2)*rescale(SWC)+time_scaled, data=plotmeans.L),test = "Chisq") 
plot(lm(Anet~rescale(CO2)*rescale(SWC)+time_scaled, data=plotmeans.L))
qqPlot(lm(Anet~rescale(CO2)*rescale(SWC)+time_scaled, data=plotmeans.L))
shapiro.test(resid(lm(Anet~rescale(CO2)*rescale(SWC)+time_scaled, data=plotmeans.L))) 
# W = 0.96421, p-value = 0.7384

# gs
#                                                                                                        df      AICc
# lmer(gs ~ rescale(CO2) * rescale(SWC) + time_scaled + (1 | Plot), data = filter(final_df, Spp == "V"), REML = F) -88.74690
# lmer(gs ~ rescale(CO2) * rescale(SWC) + (1 | Plot), data = filter(final_df, Spp == "V"), REML = F)               -78.38325
# lmer(gs ~ rescale(CO2) + rescale(SWC) + time_scaled + (1 | Plot), data = filter(final_df, Spp == "V"), REML = F) -90.95246 !
# lmer(gs ~ rescale(CO2) + rescale(SWC) + (1 | Plot), data = filter(final_df, Spp == "V"), REML = F)               -81.32323
# lmer(gs ~ rescale(SWC) + time_scaled + (1 | Plot), data = filter(final_df, Spp == "V"), REML = F)                -91.97357
# lmer(gs ~ rescale(SWC) + (1 | Plot), data = filter(final_df, Spp == "V"), REML = F)                              -83.08519
# lmer(gs ~ (1 | Plot), data = filter(final_df, Spp == "V"))                                                       -68.87320
# lmer(gs ~ rescale(CO2) * rescale(SWC) + time_scaled + (1 | Plot), data = filter(final_df, Spp == "L"))  7 -126.4734
# lmer(gs ~ rescale(CO2) * rescale(SWC) + (1 | Plot), data = filter(final_df, Spp == "L"))                6 -131.3191
# lmer(gs ~ rescale(CO2) + rescale(SWC) + time_scaled + (1 | Plot), data = filter(final_df, Spp == "L"))  6 -133.7673
# lmer(gs ~ rescale(CO2) + rescale(SWC) + (1 | Plot), data = filter(final_df, Spp == "L"))                5 -138.4280
# lmer(gs ~ (1 | Plot), data = filter(final_df, Spp == "L"))                                              3 -128.9129

grid.arrange(
  ggpredict(lmer(gs~rescale(CO2)*rescale(SWC)+time_scaled + (1|Plot), data=filter(final_df,Spp=="V")), 
            terms=c("CO2","SWC [4,42]"))%>% plot(rawdata=T,ci=T,colors=c("red","blue")) + labs(title="gs, V *"),
  ggpredict(lmer(gs~rescale(CO2)+rescale(SWC)+time_scaled + (1|Plot), data=filter(final_df,Spp=="V")), 
            terms=c("CO2","SWC [4,42]"))%>% plot(rawdata=T,ci=T,colors=c("red","blue")) + labs(title="gs, V +"),
  ggpredict(lmer(gs~rescale(CO2)*rescale(SWC)+time_scaled + (1|Plot), data=filter(final_df,Spp=="L",)), 
            terms=c("CO2","SWC [4,42]"))%>% plot(rawdata=T,ci=T,colors=c("red","blue")) + labs(title="gs, L *"),
  ggpredict(lmer(gs~rescale(CO2)+rescale(SWC)+time_scaled + (1|Plot), data=filter(final_df,Spp=="L",)), 
            terms=c("CO2","SWC [4,42]"))%>% plot(rawdata=T,ci=T,colors=c("red","blue")) + labs(title="gs, L +") )
lmtest::lrtest(lmer(gs~rescale(CO2)*rescale(SWC)+time_scaled + (1|Plot), data=filter(final_df,Spp=="V"), REML = F), lmer(gs~rescale(CO2)+rescale(SWC)+time_scaled + (1|Plot), data=filter(final_df,Spp=="V"), REML = F))
lmtest::lrtest(lmer(gs~rescale(CO2)+rescale(SWC)+time_scaled + (1|Plot), data=filter(final_df,Spp=="V"), REML = F), lmer(gs~(1|Plot), data=filter(final_df,Spp=="V"), REML = F))
summary(lmer(gs~rescale(CO2)+rescale(SWC)+time_scaled + (1|Plot), data=filter(final_df,Spp=="V"), REML =F))
Anova(lmer(gs~rescale(CO2)+rescale(SWC)+time_scaled + (1|Plot), data=filter(final_df,Spp=="V")),  REML = F)
plot(lmer(gs~rescale(CO2)+rescale(SWC)+time_scaled + (1|Plot), data=filter(final_df,Spp=="V"),  REML = F))
qqmath(lmer(gs~rescale(CO2)+rescale(SWC)+time_scaled + (1|Plot), data=filter(final_df,Spp=="V"),  REML = F))
shapiro.test(resid(lmer(gs~rescale(CO2)+rescale(SWC)+time_scaled + (1|Plot), data=filter(final_df,Spp=="V"),  REML = F))) # W = 0.98615, p-value = 0.9404

lmtest::lrtest(lmer(gs~rescale(CO2)*rescale(SWC)+time_scaled + (1|Plot), data=filter(final_df,Spp=="L"), REML = F), lmer(gs~rescale(CO2)+rescale(SWC)+time_scaled + (1|Plot), data=filter(final_df,Spp=="L"), REML = F))
lmtest::lrtest(lmer(gs~rescale(CO2)+rescale(SWC)+time_scaled + (1|Plot), data=filter(final_df,Spp=="L"), REML = F), lmer(gs~(1|Plot), data=filter(final_df,Spp=="L"), REML = F))
# lmtest::lrtest(lm(gs~rescale(CO2)*rescale(SWC)+time_scaled, data=plotmeans.L), lm(gs~rescale(CO2)+rescale(SWC)+time_scaled, data=plotmeans.L))
# lmtest::lrtest(lm(gs~rescale(CO2)+rescale(SWC)+time_scaled, data=plotmeans.L), lm(gs~1, data=plotmeans.L))
m10L <- lmer(gs~rescale(CO2)+rescale(SWC)+time_scaled + (1|Plot), data=filter(final_df,Spp=="L"), REML=F)
summary(lmer(gs~rescale(CO2)+rescale(SWC)+time_scaled + (1|Plot), data=filter(final_df,Spp=="L"), REML=F))
Anova(lmer(gs~rescale(CO2)+rescale(SWC)+time_scaled + (1|Plot), data=filter(final_df,Spp=="L")), test = "Chisq")
plot(lmer(gs~rescale(CO2)+rescale(SWC)+time_scaled + (1|Plot), data=filter(final_df,Spp=="L")))
qqmath(lmer(gs~rescale(CO2)+rescale(SWC)+time_scaled + (1|Plot), data=filter(final_df,Spp=="L")))
shapiro.test(resid(lmer(gs~rescale(CO2)*rescale(SWC)+time_scaled + (1|Plot), data=filter(final_df,Spp=="L")))) # W = 0.98024, p-value = 0.6986

# WUE
#                                                                                                         df     AICc
# lmer(WUE~rescale(CO2)*rescale(SWC)+time_scaled+(1 | Plot), data = filter(final_df, Spp == "V"), REML = F) 342.6410
# lmer(WUE~rescale(CO2)*rescale(SWC)+(1|Plot), data = filter(final_df, Spp=="V"), REML = F)               341.1428
# lmer(WUE~rescale(CO2)+rescale(SWC)+time_scaled + (1 | Plot), data=filter(final_df, Spp == "V"), REML = F) 339.4176
# lmer(WUE~rescale(CO2)+rescale(SWC)+(1|Plot), data = filter(final_df, Spp == "V"), REML = F)               338.2818
# lmer(WUE~rescale(SWC)+time_scaled+(1|Plot), data = filter(final_df, Spp == "V"), REML = F)                339.1313
# lmer(WUE~rescale(SWC)+(1|Plot), data = filter(final_df,Spp=="V"), REML = F)                              338.0053
# lmer(WUE~(1|Plot), data = filter(final_df, Spp == "V"))                                                 336.5598 !
grid.arrange(
  ggpredict(lmer(WUE~rescale(CO2)*rescale(SWC)+time_scaled + (1|Plot), data=filter(final_df,Spp=="V"), REML = F), 
            terms=c("CO2","SWC [4,42]"))%>% plot(rawdata=T,ci=T,colors=c("red","blue")) + labs(title="WUE, V *"),
  ggpredict(lmer(WUE~rescale(CO2)+rescale(SWC)+time_scaled + (1|Plot), data=filter(final_df,Spp=="V"), REML = F), 
            terms=c("CO2","SWC [4,42]"))%>% plot(rawdata=T,ci=T,colors=c("red","blue")) + labs(title="WUE, V +"),
  ggpredict(lmer(WUE~rescale(CO2)*rescale(SWC) + (1|Plot), data=filter(final_df,Spp=="L"), REML = F), 
            terms=c("CO2","SWC [4,42]"))%>% plot(rawdata=T,ci=T,colors=c("red","blue")) + labs(title="WUE, L *"),
  ggpredict(lmer(WUE~rescale(CO2)+rescale(SWC) + (1|Plot), data=filter(final_df,Spp=="L"), REML = F), 
            terms=c("CO2","SWC [4,42]"))%>% plot(rawdata=T,ci=T,colors=c("red","blue")) + labs(title="WUE, L +") )
lmtest::lrtest(lmer(WUE~rescale(CO2)*rescale(SWC)+time_scaled + (1|Plot), data=filter(final_df,Spp=="V"), REML = F), lmer(WUE~rescale(CO2)+rescale(SWC)+time_scaled + (1|Plot), data=filter(final_df,Spp=="V"), REML = F))
lmtest::lrtest(lmer(WUE~rescale(CO2)+rescale(SWC)+time_scaled + (1|Plot), data=filter(final_df,Spp=="V"), REML = F), lmer(WUE~(1|Plot), data=filter(final_df,Spp=="V"), REML = F))
# Null model wins  Chisq 5.5196  p = 0.1375
summary(lmer(WUE~rescale(CO2)+rescale(SWC)+time_scaled + (1|Plot), data=filter(final_df,Spp=="V")))
Anova(lmer(WUE~rescale(CO2)+rescale(SWC)+time_scaled + (1|Plot), data=filter(final_df,Spp=="V")), test = "F")
plot(lmer(WUE~rescale(CO2)+rescale(SWC)+time_scaled + (1|Plot), data=filter(final_df,Spp=="V")))
qqmath(lmer(WUE~rescale(CO2)+rescale(SWC)+time_scaled + (1|Plot), data=filter(final_df,Spp=="V")))
shapiro.test(resid(lmer(WUE~rescale(CO2)+rescale(SWC)+time_scaled + (1|Plot), data=filter(final_df,Spp=="V")))) # W = 0.97225, p-value = 0.5447

lmtest::lrtest(lmer(WUE~rescale(CO2)*rescale(SWC)+time_scaled + (1|Plot), data=filter(final_df,Spp=="L"), REML = F), lmer(WUE~rescale(CO2)+rescale(SWC)+time_scaled + (1|Plot), data=filter(final_df,Spp=="L"), REML = F))
lmtest::lrtest(lmer(WUE~rescale(CO2)+rescale(SWC)+time_scaled + (1|Plot), data=filter(final_df,Spp=="L"), REML = F), lmer(WUE~(1|Plot), data=filter(final_df,Spp=="L"), REML = F))
# Null model wins Chisq 4.2804  p = 0.2327
m11L <- lmer(WUE~rescale(CO2)+rescale(SWC) + (1|Plot), data=filter(final_df,Spp=="L"))
summary(lmer(WUE~rescale(CO2)+rescale(SWC) + (1|Plot), data=filter(final_df,Spp=="L")))
Anova(lmer(WUE~rescale(CO2)+rescale(SWC)+time_scaled + (1|Plot), data=filter(final_df,Spp=="L")), REML = F, test = "Chisq")
plot(lmer(WUE~rescale(CO2)+rescale(SWC)+time_scaled + (1|Plot), data=filter(final_df,Spp=="L")))
qqmath(lmer(WUE~rescale(CO2)+rescale(SWC)+time_scaled + (1|Plot), data=filter(final_df,Spp=="L")))
shapiro.test(resid(lmer(WUE~rescale(CO2)+rescale(SWC)+time_scaled + (1|Plot), data=filter(final_df,Spp=="L")))) # W = 0.97522, p-value = 0.5175

# totmass
#                                                                df     AICc
# lm(totmass ~ rescale(CO2) * rescale(SWC), data = plotmeans.V)  5  72.81457
# lm(totmass ~ rescale(CO2) + rescale(SWC), data = plotmeans.V)  4  65.20936 !
# lm(totmass ~ 1, data = plotmeans.V)                            2  64.63016
# lm(totmass ~ rescale(CO2) * rescale(SWC), data = plotmeans.L)  5 110.97208
# lm(totmass ~ rescale(CO2) + rescale(SWC), data = plotmeans.L)  4 106.62490 !
# lm(totmass ~ 1, data = plotmeans.L)                            2 108.56790
grid.arrange(
ggpredict(lm(totmass~rescale(CO2)*rescale(SWC), data=plotmeans.V), 
     terms=c("CO2","SWC [4,42]"))%>% plot(rawdata=T,ci=T,colors=c("red","blue")) + labs(title="totmass, V *"),
ggpredict(lm(totmass~rescale(CO2)+rescale(SWC), data=plotmeans.V), 
     terms=c("CO2","SWC [4,42]"))%>% plot(rawdata=T,ci=T,colors=c("red","blue")) + labs(title="totmass, V +"),
ggpredict(lm(totmass~rescale(CO2)*rescale(SWC), data=plotmeans.L), 
     terms=c("CO2","SWC [4,42]"))%>% plot(rawdata=T,ci=T,colors=c("red","blue")) + labs(title="totmass, L *"),
ggpredict(lm(totmass~rescale(CO2)+rescale(SWC), data=plotmeans.L), 
     terms=c("CO2","SWC [4,42]"))%>% plot(rawdata=T,ci=T,colors=c("red","blue")) + labs(title="totmass, L +"),
 nrow=2)
lmtest::lrtest(lm(totmass~rescale(CO2)*rescale(SWC), data=plotmeans.V), lm(totmass~rescale(CO2)+rescale(SWC), data=plotmeans.V))
lmtest::lrtest(lm(totmass~rescale(CO2)+rescale(SWC), data=plotmeans.V), lm(totmass~1, data=plotmeans.V)) 
summary(lm(totmass~rescale(CO2)+rescale(SWC), data=plotmeans.V)) 
Anova(lm(totmass~rescale(CO2)+rescale(SWC), data=plotmeans.V), test = "F") 
plot(lm(totmass~rescale(CO2)+rescale(SWC), data=plotmeans.V))
qqPlot(lm(totmass~rescale(CO2)+rescale(SWC), data=plotmeans.V))
shapiro.test(resid(lm(totmass~rescale(CO2)+rescale(SWC), data=plotmeans.V))) # W = 0.89025, p-value = 0.1707

lmtest::lrtest(lm(totmass~rescale(CO2)*rescale(SWC), data=plotmeans.L), lm(totmass~rescale(CO2)+rescale(SWC), data=plotmeans.L))
lmtest::lrtest(lm(totmass~rescale(CO2)+rescale(SWC), data=plotmeans.L), lm(totmass~1, data=plotmeans.L)) 
m1L <- lm(totmass~rescale(CO2)+rescale(SWC), data=plotmeans.L)
summary(lm(totmass~rescale(CO2)+rescale(SWC), data=plotmeans.L))
Anova(lm(totmass~rescale(CO2)+rescale(SWC), data=plotmeans.L))
plot(lm(totmass~rescale(CO2)+rescale(SWC), data=plotmeans.L))
qqPlot(lm(totmass~rescale(CO2)+rescale(SWC), data=plotmeans.L))
shapiro.test(resid(lm(totmass~rescale(CO2)+rescale(SWC), data=plotmeans.L))) # W = 0.94137, p-value = 0.3661

# rootmass
# lm(rootmass_g ~ rescale(CO2) * rescale(SWC), data = plotmeans.V)  5 71.33310
# lm(rootmass_g ~ rescale(CO2) + rescale(SWC), data = plotmeans.V)  4 67.51369 !
# lm(rootmass_g ~ 1, data = plotmeans.V)                            2 69.63197
# lm(rootmass_g ~ rescale(CO2) * rescale(SWC), data = plotmeans.L)  5 81.41214
# lm(rootmass_g ~ rescale(CO2) + rescale(SWC), data = plotmeans.L)  4 77.19455 !
# lm(rootmass_g ~ 1, data = plotmeans.L)                            2 75.97478 
grid.arrange(
  ggpredict(lm(rootmass_g~rescale(CO2)*rescale(SWC), data=plotmeans.V), 
            terms=c("CO2","SWC [4,42]"))%>% plot(rawdata=T,ci=T,colors=c("red","blue")) + labs(title="rootmass_g, V *"),
  ggpredict(lm(rootmass_g~rescale(CO2)+rescale(SWC), data=plotmeans.V), 
            terms=c("CO2","SWC [4,42]"))%>% plot(rawdata=T,ci=T,colors=c("red","blue")) + labs(title="rootmass_g, V +"),
  ggpredict(lm(rootmass_g~rescale(CO2)*rescale(SWC), data=plotmeans.L), 
            terms=c("CO2","SWC [4,42]"))%>% plot(rawdata=T,ci=T,colors=c("red","blue")) + labs(title="rootmass_g, L *"),
  ggpredict(lm(rootmass_g~rescale(CO2)+rescale(SWC), data=plotmeans.L), 
            terms=c("CO2","SWC [4,42]"))%>% plot(rawdata=T,ci=T,colors=c("red","blue")) + labs(title="rootmass_g, L +") )
lmtest::lrtest(lm(rootmass_g~rescale(CO2)*rescale(SWC), data=plotmeans.V), lm(rootmass_g~rescale(CO2)+rescale(SWC), data=plotmeans.V))
lmtest::lrtest(lm(rootmass_g~rescale(CO2)+rescale(SWC), data=plotmeans.V), lm(rootmass_g~1, data=plotmeans.V)) 
summary(lm(rootmass_g~rescale(CO2)+rescale(SWC), data=plotmeans.V)) 
Anova(lm(rootmass_g~rescale(CO2)+rescale(SWC), data=plotmeans.V)) 
plot(lm(rootmass_g~rescale(CO2)+rescale(SWC), data=plotmeans.V))
qqPlot(lm(rootmass_g~rescale(CO2)+rescale(SWC), data=plotmeans.V))
shapiro.test(resid(lm(rootmass_g~rescale(CO2)+rescale(SWC), data=plotmeans.V))) # W = 0.96716, p-value = 0.7908

lmtest::lrtest(lm(rootmass_g~rescale(CO2)*rescale(SWC), data=plotmeans.L), lm(rootmass_g~rescale(CO2)+rescale(SWC), data=plotmeans.L))
lmtest::lrtest(lm(rootmass_g~rescale(CO2)+rescale(SWC), data=plotmeans.L), lm(rootmass_g~1, data=plotmeans.L))
# Chisq 5.4935    p = 0.06414 .
m2L <- lm(rootmass_g~rescale(CO2)+rescale(SWC), data=plotmeans.L)
summary(lm(rootmass_g~rescale(CO2)+rescale(SWC), data=plotmeans.L)) 
Anova(lm(rootmass_g~rescale(CO2)+rescale(SWC), data=plotmeans.L)) 
plot(lm(rootmass_g~rescale(CO2)+rescale(SWC), data=plotmeans.L))
qqPlot(lm(rootmass_g~rescale(CO2)+rescale(SWC), data=plotmeans.L))
shapiro.test(resid(lm(rootmass_g~rescale(CO2)+rescale(SWC), data=plotmeans.L))) # W = 0.9654, p-value = 0.7597

# Ht 8
# lm(Ht.mm..8 ~ rescale(CO2) * rescale(SWC), data = plotmeans.V)  5 137.7383
# lm(Ht.mm..8 ~ rescale(CO2) + rescale(SWC), data = plotmeans.V)  4 129.2687
# lm(Ht.mm..8 ~ rescale(SWC), data = plotmeans.V)                 3 124.7039 !
# lm(Ht.mm..8 ~ 1, data = plotmeans.V)                            2 126.8910 
# lm(Ht.mm..8 ~ rescale(CO2) * rescale(SWC), data = plotmeans.L)  5 187.9650 !
# lm(Ht.mm..8 ~ rescale(CO2) + rescale(SWC), data = plotmeans.L)  4 188.4157
# lm(Ht.mm..8 ~ 1, data = plotmeans.L)                            2 199.4428
grid.arrange(
  ggpredict(lm(Ht.mm..8~rescale(CO2)*rescale(SWC), data=plotmeans.V), 
            terms=c("CO2","SWC [4,42]"))%>% plot(rawdata=T,ci=T,colors=c("red","blue")) + labs(title="Ht.mm..8, V *"),
  ggpredict(lm(Ht.mm..8~rescale(CO2)+rescale(SWC), data=plotmeans.V), 
            terms=c("CO2","SWC [4,42]"))%>% plot(rawdata=T,ci=T,colors=c("red","blue")) + labs(title="Ht.mm..8, V +"),
  ggpredict(lm(Ht.mm..8~rescale(CO2)*rescale(SWC), data=plotmeans.L), 
            terms=c("CO2","SWC [4,42]"))%>% plot(rawdata=T,ci=T,colors=c("red","blue")) + labs(title="Ht.mm..8, L *"),
  ggpredict(lm(Ht.mm..8~rescale(CO2)+rescale(SWC), data=plotmeans.L), 
            terms=c("CO2","SWC [4,42]"))%>% plot(rawdata=T,ci=T,colors=c("red","blue")) + labs(title="Ht.mm..8, L +"),
  nrow=2)
lmtest::lrtest(lm(Ht.mm..8~rescale(CO2)*rescale(SWC), data=plotmeans.V), lm(Ht.mm..8~rescale(CO2)+rescale(SWC), data=plotmeans.V))
lmtest::lrtest(lm(Ht.mm..8~rescale(CO2)+rescale(SWC), data=plotmeans.V), lm(Ht.mm..8~1, data=plotmeans.V))
summary(lm(Ht.mm..8~rescale(CO2)+rescale(SWC), data=plotmeans.V)) 
Anova(lm(Ht.mm..8~rescale(CO2)+rescale(SWC), data=plotmeans.V)) 
plot(lm(Ht.mm..8~rescale(CO2)+rescale(SWC), data=plotmeans.V))
qqPlot(lm(Ht.mm..8~rescale(CO2)+rescale(SWC), data=plotmeans.V))
shapiro.test(resid(lm(Ht.mm..8~rescale(CO2)+rescale(SWC), data=plotmeans.V))) # W = 0.98633, p-value = 0.99

lmtest::lrtest(lm(Ht.mm..8~rescale(CO2)*rescale(SWC), data=plotmeans.L), lm(Ht.mm..8~rescale(CO2)+rescale(SWC), data=plotmeans.L)) # Chisq 4.8144   p = 0.02822 *
m3L <- lm(Ht.mm..8~rescale(CO2)*rescale(SWC), data=plotmeans.L)
summary(lm(Ht.mm..8~rescale(CO2)*rescale(SWC), data=plotmeans.L)) 
Anova(lm(Ht.mm..8~rescale(CO2)*rescale(SWC), data=plotmeans.L)) 
plot(lm(Ht.mm..8~rescale(CO2)*rescale(SWC), data=plotmeans.L))
qqPlot(lm(Ht.mm..8~rescale(CO2)*rescale(SWC), data=plotmeans.L))
shapiro.test(resid(lm(Ht.mm..8~rescale(CO2)*rescale(SWC), data=plotmeans.L))) # W = 0.96865, p-value = 0.8162

# tot_area
# lm(tot_area ~ rescale(CO2) * rescale(SWC), data = plotmeans.V)  5 188.1339
# lm(tot_area ~ rescale(CO2) + rescale(SWC), data = plotmeans.V)  4 173.1879
# lm(tot_area ~ 1, data = plotmeans.V)                            2 158.9249 !
# lm(tot_area ~ rescale(CO2) * rescale(SWC), data = plotmeans.L)  5 339.1754
# lm(tot_area ~ rescale(CO2) + rescale(SWC), data = plotmeans.L)  4 334.8203 !
# lm(tot_area ~ 1, data = plotmeans.L)                            2 334.1268
grid.arrange(
  ggpredict(lm(tot_area~rescale(CO2)*rescale(SWC), data=plotmeans.V), 
            terms=c("CO2","SWC [4,42]"))%>% plot(rawdata=T,ci=T,colors=c("red","blue")) + labs(title="tot_area, V *"),
  ggpredict(lm(tot_area~rescale(CO2)+rescale(SWC), data=plotmeans.V), 
            terms=c("CO2","SWC [4,42]"))%>% plot(rawdata=T,ci=T,colors=c("red","blue")) + labs(title="tot_area, V +"),
  ggpredict(lm(tot_area~rescale(CO2)*rescale(SWC), data=plotmeans.L), 
            terms=c("CO2","SWC [4,42]"))%>% plot(rawdata=T,ci=T,colors=c("red","blue")) + labs(title="tot_area, L *"),
  ggpredict(lm(tot_area~rescale(CO2)+rescale(SWC), data=plotmeans.L), 
            terms=c("CO2","SWC [4,42]"))%>% plot(rawdata=T,ci=T,colors=c("red","blue")) + labs(title="tot_area, L +"),
  nrow=2)
lmtest::lrtest(lm(tot_area~rescale(CO2)*rescale(SWC), data=plotmeans.V), lm(tot_area~rescale(CO2)+rescale(SWC), data=plotmeans.V)) # Chisq 3.7207   p = 0.05374 .
summary(lm(tot_area~rescale(CO2)*rescale(SWC), data=plotmeans.V)) 
Anova(lm(tot_area~rescale(CO2)+rescale(SWC), data=plotmeans.V)) 
plot(lm(tot_area~rescale(CO2)+rescale(SWC), data=plotmeans.V))
qqPlot(lm(tot_area~rescale(CO2)+rescale(SWC), data=plotmeans.V))
shapiro.test(resid(lm(tot_area~rescale(CO2)+rescale(SWC), data=plotmeans.V))) # W = 0.88352, p-value = 0.2034

lmtest::lrtest(lm(tot_area~rescale(CO2)*rescale(SWC), data=plotmeans.L), lm(tot_area~rescale(CO2)+rescale(SWC), data=plotmeans.L))
lmtest::lrtest(lm(tot_area~rescale(CO2)+rescale(SWC), data=plotmeans.L), lm(tot_area~1, data=plotmeans.L))
m4L <- lm(tot_area~rescale(CO2)+rescale(SWC), data=plotmeans.L)
summary(lm(tot_area~rescale(CO2)+rescale(SWC), data=plotmeans.L))
Anova(lm(tot_area~rescale(CO2)+rescale(SWC), data=plotmeans.L))
plot(lm(tot_area~rescale(CO2)+rescale(SWC), data=plotmeans.L))
qqPlot(lm(tot_area~rescale(CO2)+rescale(SWC), data=plotmeans.L))
shapiro.test(resid(lm(tot_area~rescale(CO2)+rescale(SWC), data=plotmeans.L))) # W = 0.97846, p-value = 0.9502

# rootshoot
#                                                                  df     AICc
# lm(rootshoot ~ rescale(CO2) * rescale(SWC), data = plotmeans.V)  5  40.098178
# lm(rootshoot ~ rescale(CO2) + rescale(SWC), data = plotmeans.V)  4  32.922536
# lm(rootshoot ~ 1, data = plotmeans.V)                            2  26.744399 !
# lm(rootshoot ~ rescale(CO2) * rescale(SWC), data = plotmeans.L)  5  -7.336573
# lm(rootshoot ~ rescale(CO2) + rescale(SWC), data = plotmeans.L)  4 -11.695431 !
# lm(rootshoot ~ 1, data = plotmeans.L)                            2  -8.422251
grid.arrange(
  ggpredict(lm(rootshoot~rescale(CO2)*rescale(SWC), data=plotmeans.V), 
            terms=c("CO2","SWC [4,42]"))%>% plot(rawdata=T,ci=T,colors=c("red","blue")) + labs(title="rootshoot, V *"),
  ggpredict(lm(rootshoot~rescale(CO2)+rescale(SWC), data=plotmeans.V), 
            terms=c("CO2","SWC [4,42]"))%>% plot(rawdata=T,ci=T,colors=c("red","blue")) + labs(title="rootshoot, V +"),
  ggpredict(lm(rootshoot~rescale(CO2)*rescale(SWC), data=plotmeans.L), 
            terms=c("CO2","SWC [4,42]"))%>% plot(rawdata=T,ci=T,colors=c("red","blue")) + labs(title="rootshoot, L *"),
  ggpredict(lm(rootshoot~rescale(CO2)+rescale(SWC), data=plotmeans.L), 
            terms=c("CO2","SWC [4,42]"))%>% plot(rawdata=T,ci=T,colors=c("red","blue")) + labs(title="rootshoot, L +"),  nrow=2)
lmtest::lrtest(lm(rootshoot~rescale(CO2)*rescale(SWC), data=plotmeans.V), lm(rootshoot~rescale(CO2)+rescale(SWC), data=plotmeans.V))
lmtest::lrtest(lm(rootshoot~rescale(CO2)+rescale(SWC), data=plotmeans.V), lm(rootshoot~1, data=plotmeans.V))
# 4.1076   p =  0.1282 Null model wins
summary(lm(rootshoot~rescale(CO2)+rescale(SWC), data=plotmeans.V)) 
Anova(lm(rootshoot~rescale(CO2)+rescale(SWC), data=plotmeans.V)) 
plot(lm(rootshoot~rescale(CO2)+rescale(SWC), data=plotmeans.V))
qqPlot(lm(rootshoot~rescale(CO2)+rescale(SWC), data=plotmeans.V))
shapiro.test(resid(lm(rootshoot~rescale(CO2)+rescale(SWC), data=plotmeans.V))) # W = 0.91843, p-value = 0.344

lmtest::lrtest(lm(rootshoot~rescale(CO2)*rescale(SWC), data=plotmeans.L), lm(rootshoot~rescale(CO2)+rescale(SWC), data=plotmeans.L))
lmtest::lrtest(lm(rootshoot~rescale(CO2)+rescale(SWC), data=plotmeans.L), lm(rootshoot~1, data=plotmeans.L))
m5L <- lm(rootshoot~rescale(CO2)+rescale(SWC), data=plotmeans.L)
summary(lm(rootshoot~rescale(CO2)+rescale(SWC), data=plotmeans.L)) 
Anova(lm(rootshoot~rescale(CO2)+rescale(SWC), data=plotmeans.L), test = "F") 
plot(lm(rootshoot~rescale(CO2)+rescale(SWC), data=plotmeans.L))
qqPlot(lm(rootshoot~rescale(CO2)+rescale(SWC), data=plotmeans.L))
shapiro.test(resid(lm(rootshoot~rescale(CO2)+rescale(SWC), data=plotmeans.L))) # W = 0.9268, p-value = 0.2168

# SRL
# lm(SRL ~ rescale(CO2) * rescale(SWC), data = plotmeans.V)  5 220.3302
# lm(SRL ~ rescale(CO2) + rescale(SWC), data = plotmeans.V)  4 216.0080
# lm(SRL ~ 1, data = plotmeans.V)                            2 209.6870 !
# lm(SRL ~ rescale(CO2) * rescale(SWC), data = plotmeans.L)  5 219.6149 !
# lm(SRL ~ rescale(CO2) + rescale(SWC), data = plotmeans.L)  4 220.5342 
# lm(SRL ~ 1, data = plotmeans.L)                            2 219.1741
grid.arrange(
  ggpredict(lm(SRL~rescale(CO2)*rescale(SWC), data=plotmeans.V), 
            terms=c("CO2","SWC [4,42]"))%>% plot(rawdata=T,ci=T,colors=c("red","blue")) + labs(title="SRL, V *"),
  ggpredict(lm(SRL~rescale(CO2)+rescale(SWC), data=plotmeans.V), 
            terms=c("CO2","SWC [4,42]"))%>% plot(rawdata=T,ci=T,colors=c("red","blue")) + labs(title="SRL, V +"),
  ggpredict(lm(SRL~rescale(CO2)*rescale(SWC), data=plotmeans.L), 
            terms=c("CO2","SWC [4,42]"))%>% plot(rawdata=T,ci=T,colors=c("red","blue")) + labs(title="SRL, L *"),
  ggpredict(lm(SRL~rescale(CO2)+rescale(SWC), data=plotmeans.L), 
            terms=c("CO2","SWC [4,42]"))%>% plot(rawdata=T,ci=T,colors=c("red","blue")) + labs(title="SRL, L +") )
lmtest::lrtest(lm(SRL~rescale(CO2)*rescale(SWC), data=plotmeans.V), lm(SRL~rescale(CO2)+rescale(SWC), data=plotmeans.V))
lmtest::lrtest(lm(SRL~rescale(CO2)+rescale(SWC), data=plotmeans.V), lm(SRL~1, data=plotmeans.V))
# Null model wins, Chisq 0.3923  p = 0.8219
summary(lm(SRL~rescale(CO2)+rescale(SWC), data=plotmeans.V)) 
Anova(lm(SRL~rescale(CO2)+rescale(SWC), data=plotmeans.V)) 
plot(lm(SRL~rescale(CO2)+rescale(SWC), data=plotmeans.V))
qqPlot(lm(SRL~rescale(CO2)+rescale(SWC), data=plotmeans.V))
shapiro.test(resid(lm(SRL~rescale(CO2)+rescale(SWC), data=plotmeans.V))) # W = 0.83486, p-value = 0.008222

lmtest::lrtest(lm(SRL~rescale(CO2)*rescale(SWC), data=plotmeans.L), lm(SRL~rescale(CO2)+rescale(SWC), data=plotmeans.L)) # Chisq 5.2829   p = 0.02154
m6L <- lm(SRL~rescale(CO2)*rescale(SWC), data=plotmeans.L)
summary(lm(SRL~rescale(CO2)*rescale(SWC), data=plotmeans.L)) 
Anova(lm(SRL~rescale(CO2)*rescale(SWC), data=plotmeans.L), test = "F") 
plot(lm(SRL~rescale(CO2)*rescale(SWC), data=plotmeans.L))
qqPlot(lm(SRL~rescale(CO2)*rescale(SWC), data=plotmeans.L))
shapiro.test(resid(lm(SRL~rescale(CO2)*rescale(SWC), data=plotmeans.L))) # W = 0.91069, p-value = 0.1194

# lwc
#                                                               df      AICc
# lm(lwc ~ rescale(CO2) * rescale(SWC), data = plotmeans.V)  5 101.60680
# lm(lwc ~ rescale(CO2) + rescale(SWC), data = plotmeans.V)  4  97.60382 !
# lm(lwc ~ 1, data = plotmeans.V)                            2  99.44339
# lm(lwc ~ rescale(CO2) * rescale(SWC), data = plotmeans.L)  5  96.49542
# lm(lwc ~ rescale(CO2) + rescale(SWC), data = plotmeans.L)  4  92.67017
# lm(lwc ~ 1, data = plotmeans.L)                            2  87.86428 !
grid.arrange(
  ggpredict(lm(lwc~rescale(CO2)*rescale(SWC), data=plotmeans.V), 
            terms=c("CO2","SWC [4,42]"))%>% plot(rawdata=T,ci=T,colors=c("red","blue")) + labs(title="lwc, V *"),
  ggpredict(lm(lwc~rescale(CO2)+rescale(SWC), data=plotmeans.V), 
            terms=c("CO2","SWC [4,42]"))%>% plot(rawdata=T,ci=T,colors=c("red","blue")) + labs(title="lwc, V +"),
  ggpredict(lm(lwc~rescale(CO2)*rescale(SWC), data=plotmeans.L), 
            terms=c("CO2","SWC [4,42]"))%>% plot(rawdata=T,ci=T,colors=c("red","blue")) + labs(title="lwc, L *"),
  ggpredict(lm(lwc~rescale(CO2)+rescale(SWC), data=plotmeans.L), 
            terms=c("CO2","SWC [4,42]"))%>% plot(rawdata=T,ci=T,colors=c("red","blue")) + labs(title="lwc, L +") )
lmtest::lrtest(lm(lwc~rescale(CO2)*rescale(SWC), data=plotmeans.V), lm(lwc~rescale(CO2)+rescale(SWC), data=plotmeans.V))
lmtest::lrtest(lm(lwc~rescale(CO2)+rescale(SWC), data=plotmeans.V), lm(lwc~1, data=plotmeans.V))
summary(lm(lwc~rescale(CO2)+rescale(SWC), data=plotmeans.V)) 
Anova(lm(lwc~rescale(CO2)+rescale(SWC), data=plotmeans.V)) 
plot(lm(lwc~rescale(CO2)+rescale(SWC), data=plotmeans.V))
qqPlot(lm(lwc~rescale(CO2)+rescale(SWC), data=plotmeans.V))
shapiro.test(resid(lm(lwc~rescale(CO2)+rescale(SWC), data=plotmeans.V))) # W = 0.90199, p-value = 0.1205

lmtest::lrtest(lm(lwc~rescale(CO2)*rescale(SWC), data=plotmeans.L), lm(lwc~rescale(CO2)+rescale(SWC), data=plotmeans.L))
lmtest::lrtest(lm(lwc~rescale(CO2)+rescale(SWC), data=plotmeans.L), lm(lwc~1, data=plotmeans.L))
# Null model wins  Chisq 1.9074  p = 0.3853
m7L <- lm(lwc~rescale(CO2)+rescale(SWC), data=plotmeans.L)
summary(lm(lwc~rescale(CO2)+rescale(SWC), data=plotmeans.L)) 
Anova(lm(lwc~rescale(CO2)+rescale(SWC), data=plotmeans.L)) 
plot(lm(lwc~rescale(CO2)+rescale(SWC), data=plotmeans.L))
qqPlot(lm(lwc~rescale(CO2)+rescale(SWC), data=plotmeans.L))
shapiro.test(resid(lm(lwc~rescale(CO2)+rescale(SWC), data=plotmeans.L))) # W = 0.97831, p-value = 0.9488

# d13C
# lm(d13C ~ rescale(CO2) * rescale(SWC), data = plotmeans.V)  5 53.68888
# lm(d13C ~ rescale(CO2) + rescale(SWC), data = plotmeans.V)  4 49.09709 !
# lm(d13C ~ 1, data = plotmeans.V)                            2 52.71464
# lm(d13C ~ rescale(CO2) * rescale(SWC), data = plotmeans.L)  5 41.76920
# lm(d13C ~ rescale(CO2) + rescale(SWC), data = plotmeans.L)  4 38.99320 !
# lm(d13C ~ 1, data = plotmeans.L)                            2 40.00270
grid.arrange(
  ggpredict(lm(d13C~rescale(CO2)*rescale(SWC), data=plotmeans.V), 
            terms=c("CO2","SWC [4,42]"))%>% plot(rawdata=T,ci=T,colors=c("red","blue")) + labs(title="d13C, V *"),
  ggpredict(lm(d13C~rescale(CO2)+rescale(SWC), data=plotmeans.V), 
            terms=c("CO2","SWC [4,42]"))%>% plot(rawdata=T,ci=T,colors=c("red","blue")) + labs(title="d13C, V +"),
  ggpredict(lm(d13C~rescale(CO2)*rescale(SWC), data=plotmeans.L), 
            terms=c("CO2","SWC [4,42]"))%>% plot(rawdata=T,ci=T,colors=c("red","blue")) + labs(title="d13C, L *"),
  ggpredict(lm(d13C~rescale(CO2)+rescale(SWC), data=plotmeans.L), 
            terms=c("CO2","SWC [4,42]"))%>% plot(rawdata=T,ci=T,colors=c("red","blue")) + labs(title="d13C, L +") )
lmtest::lrtest(lm(d13C~rescale(CO2)*rescale(SWC), data=plotmeans.V), lm(d13C~rescale(CO2)+rescale(SWC), data=plotmeans.V))
lmtest::lrtest(lm(d13C~rescale(CO2)+rescale(SWC), data=plotmeans.V), lm(d13C~1, data=plotmeans.V))
summary(lm(d13C~rescale(CO2)+rescale(SWC), data=plotmeans.V)) 
Anova(lm(d13C~rescale(CO2)+rescale(SWC), data=plotmeans.V)) 
plot(lm(d13C~rescale(CO2)+rescale(SWC), data=plotmeans.V))
qqPlot(lm(d13C~rescale(CO2)+rescale(SWC), data=plotmeans.V))
shapiro.test(resid(lm(d13C~rescale(CO2)+rescale(SWC), data=plotmeans.V))) # W = 0.95545, p-value = 0.614

lmtest::lrtest(lm(d13C~rescale(CO2)*rescale(SWC), data=plotmeans.L), lm(d13C~rescale(CO2)+rescale(SWC), data=plotmeans.L))
lmtest::lrtest(lm(d13C~rescale(CO2)+rescale(SWC), data=plotmeans.L), lm(d13C~1, data=plotmeans.L))
m8L <- lm(d13C~rescale(CO2)+rescale(SWC), data=plotmeans.L)
summary(lm(d13C~rescale(CO2)+rescale(SWC), data=plotmeans.L))
Anova(lm(d13C~rescale(CO2)+rescale(SWC), data=plotmeans.L), test="F")
plot(lm(d13C~rescale(CO2)+rescale(SWC), data=plotmeans.L))
qqPlot(lm(d13C~rescale(CO2)+rescale(SWC), data=plotmeans.L))
shapiro.test(resid(lm(d13C~rescale(CO2)+rescale(SWC), data=plotmeans.L))) # W = 0.95301, p-value = 0.573


#####

# model plots with interaction terms
grid.arrange(
  ggpredict(lm(Anet~rescale(CO2)*rescale(SWC)+time_scaled, data=plotmeans.L),
            terms=c("CO2","SWC [4,42]"))%>% plot(rawdata=T,ci=T,colors=c("red","blue")) + labs(title="Anet (µmol CO2/m2/s), Live Oak"), 
  ggpredict(lm(Ht.mm..8~rescale(CO2)*rescale(SWC), data=plotmeans.L), 
            terms=c("CO2","SWC [4,42]"))%>% plot(rawdata=T,ci=T,colors=c("red","blue")) + labs(title="Final Ht (mm), Live Oak"),
  ggpredict(lm(SRL~rescale(CO2)*rescale(SWC), data=plotmeans.L), 
            terms=c("CO2","SWC [4,42]"))%>% plot(rawdata=T,ci=T,colors=c("red","blue")) + labs(title="Specifc Root Length (mm/g), Live Oak"),
  ggpredict(lm(tot_area~rescale(CO2)*rescale(SWC), data=plotmeans.V), 
            terms=c("CO2","SWC [4,42]"))%>% plot(rawdata=T,ci=T,colors=c("red","blue")) + labs(title="Total Leaf Area (mm2), Valley Oak"))

# same plots but with all obs (lmer; NB some singular fit (models 1 and 4))
grid.arrange(
  ggpredict(lmer(Ht.mm..8~rescale(CO2)*rescale(SWC) +(1|Plot), data=filter(final_df_nh, Spp == "L")), 
            terms=c("CO2","SWC [4,42]"))%>% plot(rawdata=T,ci=T,colors=c("red","blue")) + labs(title="Final Ht (mm), Live Oak"),
  ggpredict(lmer(SRL~rescale(CO2)*rescale(SWC) + (1|Plot), data=filter(final_df, Spp == "L")), 
            terms=c("CO2","SWC [4,42]"))%>% plot(rawdata=T,ci=T,colors=c("red","blue")) + labs(title="Specifc Root Length (mm/g), Live Oak"),
  ggpredict(lmer(Anet~rescale(CO2)*rescale(SWC)+time_scaled +(1|Plot), data=filter(final_df, Spp == "L")),
            terms=c("CO2","SWC [4,42]"))%>% plot(rawdata=T,ci=T,colors=c("red","blue")) + labs(title="Anet (µmol CO2/m2/s), Live Oak"),
  ggpredict(lmer(tot_area~rescale(CO2)*rescale(SWC)+(1|Plot), data=filter(final_df_nh, Spp == "V")), 
            terms=c("CO2","SWC [4,42]"))%>% plot(rawdata=T,ci=T,colors=c("red","blue")) + labs(title="Total Leaf Area (mm2), Valley Oak"))

# was June height associated with herbivory, once controlling for water (and species)?
summary(glm(herb ~ Ht.mm..5+H2OTmt, family = "binomial", data = final_df %>% 
              filter(Spp == "V") %>% 
              mutate(herb = as.numeric(Code %in% firstherb$Code)) )) # Ht + 0.014, p = 0.08
summary(glm(herb ~ Ht.mm..5+H2OTmt, family = "binomial", data = final_df %>% 
              filter(Spp == "L") %>% 
              mutate(herb = as.numeric(Code %in% firstherb$Code)) )) # Ht -0.025, p = 0.045

sum(substr(firstherb$Code, nchar(firstherb$Code)-1, nchar(firstherb$Code)-1) == "L")/nrow(firstherb)
# only 18.2% of herbivory occurred on Ls
sum(substr(firstfullherb$Code, nchar(firstfullherb$Code)-1, nchar(firstfullherb$Code)-1) == "L")/nrow(firstfullherb)
# only 13.0% of full herbivory
####

# Fig 2 from Raubenheimer and Ripley
# need a df where x = variable name, y, ymin, ymax are mean, mean-se, and mean+se of %∆ with CO2; group by H2OTmt
# want to see %∆ for wet and dry plants with eCO2

# start with every obs of every variable (long format) and columns for Tmt. Then summarise y, ymin, and ymax with mutate and reference to AD or AW column value

# functions
mean_narm <- function(x){
  mean(x, na.rm=TRUE)
}

minus_se <- function(x){
  mean(x, na.rm=TRUE) - (sd(x, na.rm = TRUE)/sqrt(length(x[!is.na(x)])))
}
plus_se <- function(x){
  mean(x, na.rm=TRUE) + (sd(x, na.rm = TRUE)/sqrt(length(x[!is.na(x)])))
}

variable_order <- c("Anet", "gs", "WUE", "tot.mass", "root.mass", "final.ht", "leaf.area", "rootshoot", "SRL")
fig2_nequals <- final_df_nh %>% 
  filter(Spp=="L") %>% 
  select(Tmt, H2OTmt, totmass, rootshoot, Ht.mm..8, Anet, gs, WUE, tot_area, SRL, rootmass_g) %>% 
  # select(Tmt, H2OTmt, totmass, rootshoot, Ht.mm..8, Anet, gs, WUE, tot_area, SRL, rootmass_g) %>% 
  rename(leaf.area = tot_area) %>% 
  rename(root.mass = rootmass_g) %>% 
  rename(tot.mass = totmass) %>% 
  rename(final.ht = Ht.mm..8) %>% 
  group_by(Tmt, H2OTmt) %>% 
  summarise_if(is.numeric, ~ sum(!is.na(.x))) %>% 
  group_by(H2OTmt) %>% 
  summarise_if(is.numeric, ~ min(.x)) %>% 
  t() %>% data.frame()

fig2_nequals <- fig2_nequals[-1,]
fig2_nequals[,3] <- rownames(fig2_nequals)
colnames(fig2_nequals) <- c("aCO2","eCO2", "variable")

fig2_nequalsV <- final_df_nh %>% 
  filter(Spp=="V") %>% 
  select(Tmt, H2OTmt, totmass, rootshoot, Ht.mm..8, Anet, gs, WUE, tot_area, SRL, rootmass_g) %>% 
  rename(leaf.area = tot_area) %>% 
  rename(root.mass = rootmass_g) %>% 
  rename(tot.mass = totmass) %>% 
  rename(final.ht = Ht.mm..8) %>% 
  group_by(Tmt, H2OTmt) %>% 
  summarise_if(is.numeric, ~ sum(!is.na(.x))) %>% 
  group_by(H2OTmt) %>% 
  summarise_if(is.numeric, ~ min(.x)) %>% 
  t() %>% data.frame()

fig2_nequalsV <- fig2_nequalsV[-1,]
fig2_nequalsV[,3] <- rownames(fig2_nequalsV)
colnames(fig2_nequalsV) <- c("aCO2","eCO2", "variable")

## z-score change with watering
fig2_boot <- final_df_nh %>% 
  # fig2_meanse <- final_df %>% 
  filter(Spp=="L") %>% 
  select(Tmt, totmass, rootshoot, Ht.mm..8, Anet, gs, WUE, tot_area, SRL, rootmass_g) %>%
  rename(leaf.area = tot_area) %>% 
  rename(root.mass = rootmass_g) %>% 
  rename(tot.mass = totmass) %>% 
  rename(final.ht = Ht.mm..8)

# percentage_difference <- function(value, value_two) {   
#   (mean(value, na.rm = T) - mean(value_two, na.rm=T))*100 / mean(value_two, na.rm=T)
# }  
# instead of a percent difference caused by watering, I now want to standardize all the responses as z scores
# then find the ∆z with watering

zscore <- function(x){
  (x - mean(x, na.rm = T))/sd(x, na.rm = T)
}

fig2_boot <- fig2_boot %>% mutate_if(is.numeric, list(z = zscore)) 

# start with the mean of ∆z with watering
zmean <- data.frame("AW1" = c(1:9), "EW1" = c(1:9)) # initialize results df
for(i in c(2:10)){zmean[(i-1),1] <-          # this is saying each of the 9 variables gets 1 row, and this is defining the first column in the results df
  # the top part is to get the mean ∆z, with watering, for aCO2 
  mean(
    do.call(c,lapply(1:1000, function(boot){
      a <- sample(unlist(fig2_boot[fig2_boot$Tmt=="AW",i+9])[!is.na(unlist(fig2_boot[fig2_boot$Tmt=="AW",i+9]))], replace = T)
      b <- sample(unlist(fig2_boot[fig2_boot$Tmt=="AD",i+9])[!is.na(unlist(fig2_boot[fig2_boot$Tmt=="AD",i+9]))], replace = T)
      mean(a)-mean(b)
    })))
# the second part is to get the mean ∆z, with watering, for eCO2 
zmean[(i-1),2] <- mean(
  do.call(c,lapply(1:1000, function(boot){
    a <- sample(unlist(fig2_boot[fig2_boot$Tmt=="EW",i+9])[!is.na(unlist(fig2_boot[fig2_boot$Tmt=="EW",i+9]))], replace = T)
    b <- sample(unlist(fig2_boot[fig2_boot$Tmt=="ED",i+9])[!is.na(unlist(fig2_boot[fig2_boot$Tmt=="ED",i+9]))], replace = T)
    mean(a)-mean(b)
  })))
}

# then mean - sd
zmin <- data.frame("AW1" = c(1:9), "EW1" = c(1:9)) # initialize results df
for(i in c(2:10)){ x <- do.call(c,lapply(1:1000, function(boot){
  a <- sample(unlist(fig2_boot[fig2_boot$Tmt=="AW",i+9])[!is.na(unlist(fig2_boot[fig2_boot$Tmt=="AW",i+9]))], replace = T)
  b <- sample(unlist(fig2_boot[fig2_boot$Tmt=="AD",i+9])[!is.na(unlist(fig2_boot[fig2_boot$Tmt=="AD",i+9]))], replace = T)
  mean(a)-mean(b)
}))
zmin[(i-1),1] <- mean(x) - sd(x)

y <- do.call(c,lapply(1:1000, function(boot){
  a <- sample(unlist(fig2_boot[fig2_boot$Tmt=="EW",i+9])[!is.na(unlist(fig2_boot[fig2_boot$Tmt=="EW",i+9]))], replace = T)
  b <- sample(unlist(fig2_boot[fig2_boot$Tmt=="ED",i+9])[!is.na(unlist(fig2_boot[fig2_boot$Tmt=="ED",i+9]))], replace = T)
  mean(a)-mean(b)
}))
zmin[(i-1),2] <- mean(y) - sd(y)
}

# then mean + sd
zmax <- data.frame("AW1" = c(1:9), "EW1" = c(1:9)) # initialize results df
for(i in c(2:10)){ x <- do.call(c,lapply(1:1000, function(boot){
  a <- sample(unlist(fig2_boot[fig2_boot$Tmt=="AW",i+9])[!is.na(unlist(fig2_boot[fig2_boot$Tmt=="AW",i+9]))], replace = T)
  b <- sample(unlist(fig2_boot[fig2_boot$Tmt=="AD",i+9])[!is.na(unlist(fig2_boot[fig2_boot$Tmt=="AD",i+9]))], replace = T)
  mean(a)-mean(b)
}))
zmax[(i-1),1] <- mean(x) + sd(x)

y <- do.call(c,lapply(1:1000, function(boot){
  a <- sample(unlist(fig2_boot[fig2_boot$Tmt=="EW",i+9])[!is.na(unlist(fig2_boot[fig2_boot$Tmt=="EW",i+9]))], replace = T)
  b <- sample(unlist(fig2_boot[fig2_boot$Tmt=="ED",i+9])[!is.na(unlist(fig2_boot[fig2_boot$Tmt=="ED",i+9]))], replace = T)
  mean(a)-mean(b)
}))
zmax[(i-1),2] <- mean(y) + sd(y)
}

z_bootL <- cbind(rbind(zmean, zmin, zmax), 
                 data.frame("variable" = rep(c("tot.mass", "rootshoot", "final.ht", "Anet", "gs", "WUE", "leaf.area", "SRL", "root.mass"),3), 
                            "y" = rep(c("y","ymin","ymax"), each=9))) %>% 
  rename(aCO2=AW1, eCO2=EW1) %>% 
  pivot_longer(cols=c("aCO2","eCO2"), names_to="Treatment", values_to="value") %>% 
  select(variable, y, Treatment, value) %>% 
  pivot_wider(names_from = "y", values_from = "value") %>% 
  ggplot() +
  geom_abline(color= "red", linetype="dashed", slope = 0, intercept= 0) +
  geom_pointrange(aes(x=factor(variable, level=variable_order), y=y, ymin=ymin, ymax=ymax, group=Treatment, color=Treatment, shape=Treatment), size=1, linewidth=1, position=position_dodge(width=0.2)) + scale_color_manual(values=c("darkgray","black")) + scale_shape_manual(values = c(1,16)) +
  ylim(-2.1, 2.6) +
  geom_text(data = fig2_nequals, aes(x = variable, y = -1.75, label = paste0("n = ",aCO2)), color="darkgray", size = 4) +
  geom_text(data = fig2_nequals, aes(x = variable, y = -2, label = paste0("n = ",eCO2)), color="black", size = 4) +
  ggtitle("B. Quercus wislizeni (live oak)") +
  ylab("change with watering") + xlab("Plant Response") +
  theme_classic(base_size = 18) 

# now for V!
fig2_bootV <- final_df_nh %>% 
  # fig2_meanse <- final_df %>% 
  filter(Spp=="V") %>% 
  select(Tmt, totmass, rootshoot, Ht.mm..8, Anet, gs, WUE, tot_area, SRL, rootmass_g) %>%
  rename(leaf.area = tot_area) %>% 
  rename(root.mass = rootmass_g) %>% 
  rename(tot.mass = totmass) %>% 
  rename(final.ht = Ht.mm..8)

fig2_bootV <- fig2_bootV %>% mutate_if(is.numeric, list(z = zscore))

# start with the mean of ∆z with watering
zmeanV <- data.frame("AW1" = c(1:9), "EW1" = c(1:9)) # initialize results df
for(i in c(2:10)){zmeanV[(i-1),1] <-          # this is saying each of the 9 variables gets 1 row, and this is defining the first column in the results df
  # the top part is to get the mean ∆z, with watering, for aCO2 
  mean(
    do.call(c,lapply(1:1000, function(boot){
      a <- sample(unlist(fig2_bootV[fig2_bootV$Tmt=="AW",i+9])[!is.na(unlist(fig2_bootV[fig2_bootV$Tmt=="AW",i+9]))], replace = T)
      b <- sample(unlist(fig2_bootV[fig2_bootV$Tmt=="AD",i+9])[!is.na(unlist(fig2_bootV[fig2_bootV$Tmt=="AD",i+9]))], replace = T)
      mean(a)-mean(b)
    })))
# the second part is to get the mean ∆z, with watering, for eCO2 
zmeanV[(i-1),2] <- mean(
  do.call(c,lapply(1:1000, function(boot){
    a <- sample(unlist(fig2_bootV[fig2_bootV$Tmt=="EW",i+9])[!is.na(unlist(fig2_bootV[fig2_bootV$Tmt=="EW",i+9]))], replace = T)
    b <- sample(unlist(fig2_bootV[fig2_bootV$Tmt=="ED",i+9])[!is.na(unlist(fig2_bootV[fig2_bootV$Tmt=="ED",i+9]))], replace = T)
    mean(a)-mean(b)
  })))
}

# then mean - sd
zminV <- data.frame("AW1" = c(1:9), "EW1" = c(1:9)) # initialize results df
for(i in c(2:10)){ x <- do.call(c,lapply(1:1000, function(boot){
  a <- sample(unlist(fig2_bootV[fig2_bootV$Tmt=="AW",i+9])[!is.na(unlist(fig2_bootV[fig2_bootV$Tmt=="AW",i+9]))], replace = T)
  b <- sample(unlist(fig2_bootV[fig2_bootV$Tmt=="AD",i+9])[!is.na(unlist(fig2_bootV[fig2_bootV$Tmt=="AD",i+9]))], replace = T)
  mean(a)-mean(b)
}))
zminV[(i-1),1] <- mean(x) - sd(x)

y <- do.call(c,lapply(1:1000, function(boot){
  a <- sample(unlist(fig2_bootV[fig2_bootV$Tmt=="EW",i+9])[!is.na(unlist(fig2_bootV[fig2_bootV$Tmt=="EW",i+9]))], replace = T)
  b <- sample(unlist(fig2_bootV[fig2_bootV$Tmt=="ED",i+9])[!is.na(unlist(fig2_bootV[fig2_bootV$Tmt=="ED",i+9]))], replace = T)
  mean(a)-mean(b)
}))
zminV[(i-1),2] <- mean(y) - sd(y)
}

# then mean + sd
zmaxV <- data.frame("AW1" = c(1:9), "EW1" = c(1:9)) # initialize results df
for(i in c(2:10)){ x <- do.call(c,lapply(1:1000, function(boot){
  a <- sample(unlist(fig2_bootV[fig2_bootV$Tmt=="AW",i+9])[!is.na(unlist(fig2_bootV[fig2_bootV$Tmt=="AW",i+9]))], replace = T)
  b <- sample(unlist(fig2_bootV[fig2_bootV$Tmt=="AD",i+9])[!is.na(unlist(fig2_bootV[fig2_bootV$Tmt=="AD",i+9]))], replace = T)
  mean(a)-mean(b)
}))
zmaxV[(i-1),1] <- mean(x) + sd(x)

y <- do.call(c,lapply(1:1000, function(boot){
  a <- sample(unlist(fig2_bootV[fig2_bootV$Tmt=="EW",i+9])[!is.na(unlist(fig2_bootV[fig2_bootV$Tmt=="EW",i+9]))], replace = T)
  b <- sample(unlist(fig2_bootV[fig2_bootV$Tmt=="ED",i+9])[!is.na(unlist(fig2_bootV[fig2_bootV$Tmt=="ED",i+9]))], replace = T)
  mean(a)-mean(b)
}))
zmaxV[(i-1),2] <- mean(y) + sd(y)
}

z_bootV <- cbind(rbind(zmeanV, zminV, zmaxV), 
                 data.frame("variable" = rep(c("tot.mass", "rootshoot", "final.ht", "Anet", "gs", "WUE", "leaf.area", "SRL", "root.mass"),3), 
                            "y" = rep(c("y","ymin","ymax"), each=9))) %>% 
  rename(aCO2=AW1, eCO2=EW1) %>% 
  pivot_longer(cols=c("aCO2","eCO2"), names_to="Treatment", values_to="value") %>% 
  select(variable, y, Treatment, value) %>% 
  pivot_wider(names_from = "y", values_from = "value") %>% 
  ggplot() +
  geom_abline(color= "red", linetype="dashed", slope = 0, intercept= 0) +
  geom_pointrange(aes(x=factor(variable, level=variable_order), y=y, ymin=ymin, ymax=ymax, group=Treatment, color=Treatment, shape=Treatment), size=1, linewidth=1, position=position_dodge(width=0.2)) + scale_color_manual(values=c("darkgray","black")) + scale_shape_manual(values = c(1,16)) +
  ylim(-2.1, 2.6) +
  geom_text(data = fig2_nequalsV, aes(x = variable, y = -1.75, label = paste0("n = ",aCO2)), color="darkgray", size = 4) +
  geom_text(data = fig2_nequalsV, aes(x = variable, y = -2, label = paste0("n = ",eCO2)), color="black", size = 4) +
  ggtitle("A. Quercus lobata (valley oak)") + xlab("") +
  ylab("change with watering") + 
  theme_classic(base_size = 18) 

grid.arrange(z_bootV, z_bootL, nrow=2)

# Anet vs gs

final_df %>% 
  filter(Spp=="L") %>% 
  select(gs, Anet, Tmt) %>% 
  group_by(Tmt) %>% 
  summarise_if(is.numeric, list(y=mean_narm, ymin=minus_se, ymax=plus_se)) %>% 
  ggplot(aes(x=gs_y, y=Anet_y, fill=Tmt)) + 
  geom_errorbar(aes(x=gs_y, ymin = Anet_ymin, ymax = Anet_ymax, color=Tmt), width = 0.001) +
  geom_errorbarh(aes(y=Anet_y, xmin = gs_ymin, xmax = gs_ymax, color=Tmt), height = 0.2) +
  geom_point(aes(color=Tmt, shape=Tmt), size = 4) +
  scale_shape_manual(values = c(21, 21, 16, 16)) +
  scale_fill_manual(values =c ("white","white","white","white")) +
  xlab(label = "Stomatal Conductance (mol H2O/m2/s)") +
  ylab(label = "Photosynthesis (µmol CO2/m2/s)") +
  labs(title="Q. wislizeni") +
  scale_x_continuous(expand = c(0,0), limits=c(0,0.25)) + scale_y_continuous(expand = c(0,0), limits=c(0,25)) +
  scale_color_manual(values = c("red", "blue", "red", "blue")) + theme_classic(base_size = 19)


final_df %>% 
  filter(Spp=="V") %>% 
  select(gs, Anet, Tmt) %>% 
  group_by(Tmt) %>% 
  summarise_if(is.numeric, list(y=mean_narm, ymin=minus_se, ymax=plus_se)) %>% 
  ggplot(aes(x=gs_y, y=Anet_y, fill=Tmt)) + 
  geom_errorbar(aes(x=gs_y, ymin = Anet_ymin, ymax = Anet_ymax, color=Tmt), width = 0.001) +
  geom_errorbarh(aes(y=Anet_y, xmin = gs_ymin, xmax = gs_ymax, color=Tmt), height = 0.2) +
  geom_point(aes(color=Tmt, shape=Tmt), size = 4) +
  scale_shape_manual(values = c(21, 21, 16, 16)) +
  scale_fill_manual(values =c ("white","white","white","white")) +
  xlab(label = "Stomatal Conductance (mol H2O/m2/s)") +
  ylab(label = "Photosynthesis (µmol CO2/m2/s)") +
  labs(title="Q. lobata") +
  scale_x_continuous(expand = c(0,0), limits=c(0,0.25)) + scale_y_continuous(expand = c(0,0), limits=c(0,25)) +
  scale_color_manual(values = c("red", "blue", "red", "blue")) + theme_classic(base_size = 19)
  

