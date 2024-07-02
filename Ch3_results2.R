# Ch 3 Results
# AKA Final Round Final Round, Best of Three

# load all data types
## load the data fresh with "." suffix; do steps from Ch3_explore and Ch3_LiCOR. Examine colnames
# need Ch3_LiCOR and Ch3_explore

plot_SWC. <- LiCOR_all[,c("Plot", "SWC")] %>% 
  group_by(Plot) %>% 
  summarise(meanSWC=mean(SWC)) %>% 
  as.data.frame()


LiCOR_df. <- LiCOR_new %>% 
  mutate(WUE = Anet/gs) %>% 
  mutate(CO2Tmt = substring(Tmt,1,1), H2OTmt = substring(Tmt,2,2)) %>% 
  mutate(Code = if_else(nchar(ID) == 4,substr(ID,1,3),substr(ID,1,4))) %>% 
  left_join(plot_CO2., by = "Plot")

# biomass2
# jeez too many steps. get this one after Ch3_explore, it is messy but w/e
## add leaf area columns to biomass2 in explore script ##
biomass2. <- biomass2
biomass2.[1,]
biomass2. <- biomass2. %>% 
  mutate(rootshoot = rootmass_g/(StemWet_g + LeafWet_g), totmass = rootmass_g + StemWet_g + LeafWet_g, lwc = (LeafWet_g-LeafDry_g)*100/LeafWet_g) %>% 
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

# response variables
names(final_df)
# [1] "Plot"        "Spp"         "Code"        "totmass"     "rootshoot"   "rootmass_g"  "Ht.mm..8"    "lwc"        
# [9] "CO2"         "meanSWC"     "Anet"        "gs"          "WUE"         "avg_area"    "perim_per_A" "tot_area"   
# [17] "SLA"         "SRL"         "d13C"        "n"           "Tmt"         "H2OTmt"      "CO2Tmt"  

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

# Redoing the univariate models with plot means
plotmeans.V <- final_df_nh %>% 
  filter(Spp == "V") %>% 
  mutate(time_scaled = rescale(HHMMSS)) %>% 
  group_by(Plot, H2OTmt, CO2Tmt, meanSWC, CO2,) %>% 
  summarise(across(is.numeric, ~ mean(.x, na.rm=T)))
plotmeans.L <- final_df_nh %>% 
  filter(Spp == "L") %>% 
  mutate(time_scaled = rescale(HHMMSS)) %>% 
  group_by(Plot, H2OTmt, CO2Tmt, meanSWC, CO2,) %>% 
  summarise(across(is.numeric, ~ mean(.x, na.rm=T)))

# and plot means with non-filtered data, just to see
plotmeans.V2 <- final_df %>% 
  filter(Spp == "V") %>% 
  mutate(time_scaled = rescale(HHMMSS)) %>% 
  group_by(Plot, H2OTmt, CO2Tmt, meanSWC, CO2,) %>% 
  summarise(across(is.numeric, ~ mean(.x, na.rm=T)))
plotmeans.L2 <- final_df %>% 
  filter(Spp == "L") %>% 
  mutate(time_scaled = rescale(HHMMSS)) %>% 
  group_by(Plot, H2OTmt, CO2Tmt, meanSWC, CO2,) %>% 
  summarise(across(is.numeric, ~ mean(.x, na.rm=T)))

# workflow: AICc of full (intx), reduced (additive) and null (~ 1 or ~(1|Plot). Then, interpret results the old way (summary) and new way (Anova)

# totmass
#                                                                   df     AICc
# lm(totmass ~ rescale(CO2) * rescale(meanSWC), data = plotmeans.V)  5 73.56438
# lm(totmass ~ rescale(CO2) + rescale(meanSWC), data = plotmeans.V)  4 65.57302 !
# lm(totmass ~ 1, data = plotmeans.V)                                2 64.63016
# lm(totmass ~ rescale(CO2) * rescale(meanSWC), data = plotmeans.L)  5 111.3463
# lm(totmass ~ rescale(CO2) + rescale(meanSWC), data = plotmeans.L)  4 107.0700 !
# lm(totmass ~ 1, data = plotmeans.L)                                2 108.5679
grid.arrange(
ggpredict(lm(totmass~rescale(CO2)*rescale(meanSWC), data=plotmeans.V), 
     terms=c("CO2","meanSWC [4,42]"))%>% plot(rawdata=T,ci=T,colors=c("red","blue")) + labs(title="totmass, V *"),
ggpredict(lm(totmass~rescale(CO2)+rescale(meanSWC), data=plotmeans.V), 
     terms=c("CO2","meanSWC [4,42]"))%>% plot(rawdata=T,ci=T,colors=c("red","blue")) + labs(title="totmass, V +"),
#ggpredict(lm(totmass~rescale(CO2)*rescale(meanSWC), data=plotmeans.V2), 
#     terms=c("CO2","meanSWC [4,42]"))%>% plot(rawdata=T,ci=T,colors=c("red","blue")) + labs(title="totmass, V *, unfiltered"),
#ggpredict(lm(totmass~rescale(CO2)+rescale(meanSWC), data=plotmeans.V2), 
#     terms=c("CO2","meanSWC [4,42]"))%>% plot(rawdata=T,ci=T,colors=c("red","blue")) + labs(title="totmass, V +, unfiltered"),
ggpredict(lm(totmass~rescale(CO2)*rescale(meanSWC), data=plotmeans.L), 
     terms=c("CO2","meanSWC [4,42]"))%>% plot(rawdata=T,ci=T,colors=c("red","blue")) + labs(title="totmass, L *"),
ggpredict(lm(totmass~rescale(CO2)+rescale(meanSWC), data=plotmeans.L), 
     terms=c("CO2","meanSWC [4,42]"))%>% plot(rawdata=T,ci=T,colors=c("red","blue")) + labs(title="totmass, L +"),
#ggpredict(lm(totmass~rescale(CO2)*rescale(meanSWC), data=plotmeans.L2), 
#    terms=c("CO2","meanSWC [4,42]"))%>% plot(rawdata=T,ci=T,colors=c("red","blue")) + labs(title="totmass, L *, unfiltered"),
#ggpredict(lm(totmass~rescale(CO2)+rescale(meanSWC), data=plotmeans.L2), 
#    terms=c("CO2","meanSWC [4,42]"))%>% plot(rawdata=T,ci=T,colors=c("red","blue")) + labs(title="totmass, L +, unfiltered")
 nrow=2)
summary(lm(totmass~rescale(CO2)+rescale(meanSWC), data=plotmeans.V)) 
Anova(lm(totmass~rescale(CO2)+rescale(meanSWC), data=plotmeans.V), test = "F") 
plot(lm(totmass~rescale(CO2)+rescale(meanSWC), data=plotmeans.V))
qqPlot(lm(totmass~rescale(CO2)+rescale(meanSWC), data=plotmeans.V))
shapiro.test(resid(lm(totmass~rescale(CO2)+rescale(meanSWC), data=plotmeans.V))) # W = 0.90013, p-value = 0.2198

summary(lm(totmass~rescale(CO2)+rescale(meanSWC), data=plotmeans.L))
Anova(lm(totmass~rescale(CO2)+rescale(meanSWC), data=plotmeans.L), test = "F")
plot(lm(totmass~rescale(CO2)+rescale(meanSWC), data=plotmeans.L))
qqPlot(lm(totmass~rescale(CO2)+rescale(meanSWC), data=plotmeans.L))
shapiro.test(resid(lm(totmass~rescale(CO2)+rescale(meanSWC), data=plotmeans.L))) # W = 0.93925, p-value = 0.34

# rootshoot
#                                                                     df     AICc
# lm(rootshoot ~ rescale(CO2) * rescale(meanSWC), data = plotmeans.V)  5 40.39952
# lm(rootshoot ~ rescale(CO2) + rescale(meanSWC), data = plotmeans.V)  4 32.99506
# lm(rootshoot ~ 1, data = plotmeans.V)                                2 26.74440 !
# lm(rootshoot ~ rescale(CO2) * rescale(meanSWC), data = plotmeans.L)  5  -7.002381
# lm(rootshoot ~ rescale(CO2) + rescale(meanSWC), data = plotmeans.L)  4 -11.358826 !
# lm(rootshoot ~ 1, data = plotmeans.L)                                2  -8.422251
grid.arrange(
  ggpredict(lm(rootshoot~rescale(CO2)*rescale(meanSWC), data=plotmeans.V), 
            terms=c("CO2","meanSWC [4,42]"))%>% plot(rawdata=T,ci=T,colors=c("red","blue")) + labs(title="rootshoot, V *"),
  ggpredict(lm(rootshoot~rescale(CO2)+rescale(meanSWC), data=plotmeans.V), 
            terms=c("CO2","meanSWC [4,42]"))%>% plot(rawdata=T,ci=T,colors=c("red","blue")) + labs(title="rootshoot, V +"),
  ggpredict(lm(rootshoot~rescale(CO2)*rescale(meanSWC), data=plotmeans.L), 
            terms=c("CO2","meanSWC [4,42]"))%>% plot(rawdata=T,ci=T,colors=c("red","blue")) + labs(title="rootshoot, L *"),
  ggpredict(lm(rootshoot~rescale(CO2)+rescale(meanSWC), data=plotmeans.L), 
            terms=c("CO2","meanSWC [4,42]"))%>% plot(rawdata=T,ci=T,colors=c("red","blue")) + labs(title="rootshoot, L +"),  nrow=2)
summary(lm(rootshoot~rescale(CO2)+rescale(meanSWC), data=plotmeans.V)) 
Anova(lm(rootshoot~rescale(CO2)+rescale(meanSWC), data=plotmeans.V)) 
plot(lm(rootshoot~rescale(CO2)+rescale(meanSWC), data=plotmeans.V))
qqPlot(lm(rootshoot~rescale(CO2)+rescale(meanSWC), data=plotmeans.V))
shapiro.test(resid(lm(rootshoot~rescale(CO2)+rescale(meanSWC), data=plotmeans.V))) # W = 0.91734, p-value = 0.3353

summary(lm(rootshoot~rescale(CO2)+rescale(meanSWC), data=plotmeans.L)) 
Anova(lm(rootshoot~rescale(CO2)+rescale(meanSWC), data=plotmeans.L), test = "F") 
plot(lm(rootshoot~rescale(CO2)+rescale(meanSWC), data=plotmeans.L))
qqPlot(lm(rootshoot~rescale(CO2)+rescale(meanSWC), data=plotmeans.L))
shapiro.test(resid(lm(rootshoot~rescale(CO2)+rescale(meanSWC), data=plotmeans.L))) # W = 0.93686, p-value = 0.3123


# lwc
#                                                               df      AICc
# lm(lwc ~ rescale(CO2) * rescale(meanSWC), data = plotmeans.V)  5 102.02371
# lm(lwc ~ rescale(CO2) + rescale(meanSWC), data = plotmeans.V)  4  97.77497 !
# lm(lwc ~ 1, data = plotmeans.V)                                2  99.44339
# lm(lwc ~ rescale(CO2) * rescale(meanSWC), data = plotmeans.L)  5 96.51549
# lm(lwc ~ rescale(CO2) + rescale(meanSWC), data = plotmeans.L)  4 92.78481
# lm(lwc ~ 1, data = plotmeans.L)                                2 87.86428 !
grid.arrange(
  ggpredict(lm(lwc~rescale(CO2)*rescale(meanSWC), data=plotmeans.V), 
            terms=c("CO2","meanSWC [4,42]"))%>% plot(rawdata=T,ci=T,colors=c("red","blue")) + labs(title="lwc, V *"),
  ggpredict(lm(lwc~rescale(CO2)+rescale(meanSWC), data=plotmeans.V), 
            terms=c("CO2","meanSWC [4,42]"))%>% plot(rawdata=T,ci=T,colors=c("red","blue")) + labs(title="lwc, V +"),
  ggpredict(lm(lwc~rescale(CO2)*rescale(meanSWC), data=plotmeans.L), 
            terms=c("CO2","meanSWC [4,42]"))%>% plot(rawdata=T,ci=T,colors=c("red","blue")) + labs(title="lwc, L *"),
  ggpredict(lm(lwc~rescale(CO2)+rescale(meanSWC), data=plotmeans.L), 
            terms=c("CO2","meanSWC [4,42]"))%>% plot(rawdata=T,ci=T,colors=c("red","blue")) + labs(title="lwc, L +") )
summary(lm(lwc~rescale(CO2)+rescale(meanSWC), data=plotmeans.V)) 
Anova(lm(lwc~rescale(CO2)+rescale(meanSWC), data=plotmeans.V)) 
plot(lm(lwc~rescale(CO2)+rescale(meanSWC), data=plotmeans.V))
qqPlot(lm(lwc~rescale(CO2)+rescale(meanSWC), data=plotmeans.V))
shapiro.test(resid(lm(lwc~rescale(CO2)+rescale(meanSWC), data=plotmeans.V))) # W = 0.89815, p-value = 0.106

summary(lm(lwc~rescale(CO2)+rescale(meanSWC), data=plotmeans.L)) 
Anova(lm(lwc~rescale(CO2)+rescale(meanSWC), data=plotmeans.L)) 
plot(lm(lwc~rescale(CO2)+rescale(meanSWC), data=plotmeans.L))
qqPlot(lm(lwc~rescale(CO2)+rescale(meanSWC), data=plotmeans.L))
shapiro.test(resid(lm(lwc~rescale(CO2)+rescale(meanSWC), data=plotmeans.L))) # W = 0.97913, p-value = 0.9563

# Anet
#                                                                                                      df     AICc
# lmer(Anet ~ rescale(CO2) * rescale(meanSWC) + time_scaled + (1|Plot), data = filter(final_df, Spp == "V")) 7 180.7978
# lmer(Anet ~ rescale(CO2) * rescale(meanSWC) + (1|Plot), data = filter(final_df, Spp == "V"))               6 187.9334
# lmer(Anet ~ rescale(CO2) + rescale(meanSWC) + time_scaled + (1|Plot), data = filter(final_df, Spp == "V")) 6 184.0331 !
# lmer(Anet ~ rescale(CO2) + rescale(meanSWC) + (1|Plot), data = filter(final_df, Spp == "V"))               5 192.5006
# lmer(Anet ~ (1|Plot), data = filter(final_df, Spp == "V"))                                                 3 209.2259
# lm(Anet ~ rescale(CO2) * rescale(meanSWC) + time_scaled, data = plotmeans.L)  6 80.93713 !
# lm(Anet ~ rescale(CO2) * rescale(meanSWC), data = plotmeans.L)                5 80.58094
# lm(Anet ~ rescale(CO2) + rescale(meanSWC) + time_scaled, data = plotmeans.L)  5 82.50817
# lm(Anet ~ rescale(CO2) + rescale(meanSWC), data = plotmeans.L)                4 81.96787
# lm(Anet ~ 1, data = plotmeans.L)                                              2 97.49752
grid.arrange(
  ggpredict(lmer(Anet~rescale(CO2)*rescale(meanSWC)+time_scaled + (1|Plot), data=filter(final_df,Spp=="V" & Code!="7V1")), 
            terms=c("CO2","meanSWC [4,42]"))%>% plot(rawdata=T,ci=T,colors=c("red","blue")) + labs(title="Anet, V *"),
  ggpredict(lmer(Anet~rescale(CO2)+rescale(meanSWC)+time_scaled + (1|Plot), data=filter(final_df,Spp=="V" & Code!="7V1")), 
            terms=c("CO2","meanSWC [4,42]"))%>% plot(rawdata=T,ci=T,colors=c("red","blue")) + labs(title="Anet, V +"),
    ggpredict(lm(Anet~rescale(CO2)*rescale(meanSWC), data=plotmeans.L),
              terms=c("CO2","meanSWC [4,42]"))%>% plot(rawdata=T,ci=T,colors=c("red","blue")) + labs(title="Anet, L *"),
    ggpredict(lm(Anet~rescale(CO2)+rescale(meanSWC), data=plotmeans.L),
              terms=c("CO2","meanSWC [4,42]"))%>% plot(rawdata=T,ci=T,colors=c("red","blue")) + labs(title="Anet, L +") )
#(lm(Anet~rescale(CO2)+rescale(meanSWC), data=plotmeans.V)) 
#summary(lm(Anet~rescale(CO2)+rescale(meanSWC)+time_scaled, data=plotmeans.V)) 
# summary(lmer(Anet~rescale(CO2)*rescale(meanSWC)+time_scaled + (1|Plot), data=filter(final_df,Spp=="V")))
# Anova(lmer(Anet~rescale(CO2)*rescale(meanSWC)+time_scaled + (1|Plot), data=filter(final_df,Spp=="V")))
summary(lmer(Anet~rescale(CO2)+rescale(meanSWC)+time_scaled + (1|Plot), data=filter(final_df,Spp=="V")))
Anova(lmer(Anet~rescale(CO2)+rescale(meanSWC)+time_scaled + (1|Plot), data=filter(final_df,Spp=="V")), test="F")
#   rescale(CO2)     14.838  1  0.0001172 ***
#   rescale(meanSWC) 36.594  1  1.455e-09 ***
#   time_scaled      10.420  1  0.0012463 **

plot(lmer(Anet~rescale(CO2)+rescale(meanSWC)+time_scaled + (1|Plot), data=filter(final_df,Spp=="V")))
qqmath(lmer(Anet~rescale(CO2)+rescale(meanSWC)+time_scaled + (1|Plot), data=filter(final_df,Spp=="V")))
shapiro.test(resid(lmer(Anet~rescale(CO2)+rescale(meanSWC)+time_scaled + (1|Plot), data=filter(final_df,Spp=="V")))) # W = 0.94877, p-value = 0.1225

AICc(lmer(Anet~rescale(CO2)*rescale(meanSWC)+time_scaled + (1|Plot), data=filter(final_df,Spp=="V")), 
     lmer(Anet~rescale(CO2)*rescale(meanSWC) + (1|Plot), data=filter(final_df,Spp=="V")),
     lmer(Anet~rescale(CO2)+rescale(meanSWC)+time_scaled + (1|Plot), data=filter(final_df,Spp=="V")),
     lmer(Anet~rescale(CO2)+rescale(meanSWC) + (1|Plot), data=filter(final_df,Spp=="V")))

#summary(lm(Anet~rescale(CO2)*rescale(meanSWC), data=plotmeans.L)) 
summary(lm(Anet~rescale(CO2)*rescale(meanSWC)+time_scaled, data=plotmeans.L)) 
Anova(lm(Anet~rescale(CO2)*rescale(meanSWC)+time_scaled, data=plotmeans.L)) 
#summary(lmer(Anet~rescale(CO2)*rescale(meanSWC)+time_scaled + (1|Plot), data=filter(final_df,Spp=="L")))
plot(lmer(Anet~rescale(CO2)+rescale(meanSWC)+time_scaled + (1|Plot), data=filter(final_df,Spp=="L")))
qqmath(lmer(Anet~rescale(CO2)+rescale(meanSWC)+time_scaled + (1|Plot), data=filter(final_df,Spp=="L")))
shapiro.test(resid(lmer(Anet~rescale(CO2)+rescale(meanSWC)+time_scaled + (1|Plot), data=filter(final_df,Spp=="L")))) #
qqPlot(lm(Anet~rescale(CO2)*rescale(meanSWC)+time_scaled, data=plotmeans.L))
shapiro.test(resid(lm(Anet~rescale(CO2)*rescale(meanSWC)+time_scaled, data=plotmeans.L))) # W = 0.95777, p-value = 0.6217

AICc(lmer(Anet~rescale(CO2)*rescale(meanSWC)+time_scaled + (1|Plot), data=filter(final_df,Spp=="L")), lmer(Anet~rescale(CO2)*rescale(meanSWC) + (1|Plot), data=filter(final_df,Spp=="L")),lmer(Anet~rescale(CO2)+rescale(meanSWC)+time_scaled + (1|Plot), data=filter(final_df,Spp=="L")),lmer(Anet~rescale(CO2)+rescale(meanSWC) + (1|Plot), data=filter(final_df,Spp=="L")), lm(Anet~rescale(CO2)+rescale(meanSWC), data=plotmeans.L), lm(Anet~rescale(CO2)+rescale(meanSWC)+time_scaled, data=plotmeans.L), lm(Anet~rescale(CO2)*rescale(meanSWC)+time_scaled, data=plotmeans.L), lm(Anet~rescale(CO2)*rescale(meanSWC), data=plotmeans.L))

# gs
#                                                                                                            df      AICc
# lmer(gs ~ rescale(CO2) * rescale(meanSWC) + time_scaled + (1 | Plot), data = filter(final_df, Spp == "V"))  7 -58.35602
# lmer(gs ~ rescale(CO2) * rescale(meanSWC) + (1 | Plot), data = filter(final_df, Spp == "V"))                6 -55.16158
# lmer(gs ~ rescale(CO2) + rescale(meanSWC) + time_scaled + (1 | Plot), data = filter(final_df, Spp == "V"))  6 -63.40992
# lmer(gs ~ rescale(CO2) + rescale(meanSWC) + (1 | Plot), data = filter(final_df, Spp == "V"))                5 -59.88700
# lmer(gs ~ (1 | Plot), data = filter(final_df, Spp == "V"))                                                  3 -63.30670
# lmer(gs ~ rescale(CO2) * rescale(meanSWC) + time_scaled + (1 | Plot), data = filter(final_df, Spp == "L"))  7 -121.2466
# lmer(gs ~ rescale(CO2) * rescale(meanSWC) + (1 | Plot), data = filter(final_df, Spp == "L"))                6 -125.5095
# lmer(gs ~ rescale(CO2) + rescale(meanSWC) + time_scaled + (1 | Plot), data = filter(final_df, Spp == "L"))  6 -125.4599
# lmer(gs ~ rescale(CO2) + rescale(meanSWC) + (1 | Plot), data = filter(final_df, Spp == "L"))                5 -129.6790
# lmer(gs ~ (1 | Plot), data = filter(final_df, Spp == "L"))                                                  3 -128.9129 
#grid.arrange(
#   ggpredict(lm(gs~rescale(CO2)*rescale(meanSWC), data=plotmeans.V), 
#             terms=c("CO2","meanSWC [4,42]"))%>% plot(rawdata=T,ci=T,colors=c("red","blue")) + labs(title="gs, V *"),
#   ggpredict(lm(gs~rescale(CO2)+rescale(meanSWC), data=plotmeans.V), 
#             terms=c("CO2","meanSWC [4,42]"))%>% plot(rawdata=T,ci=T,colors=c("red","blue")) + labs(title="gs, V +"),
#   ggpredict(lm(gs~rescale(CO2)*rescale(meanSWC), data=plotmeans.L), 
#             terms=c("CO2","meanSWC [4,42]"))%>% plot(rawdata=T,ci=T,colors=c("red","blue")) + labs(title="gs, L *"),
#   ggpredict(lm(gs~rescale(CO2)+rescale(meanSWC), data=plotmeans.L), 
#             terms=c("CO2","meanSWC [4,42]"))%>% plot(rawdata=T,ci=T,colors=c("red","blue")) + labs(title="gs, L +") )
grid.arrange(
  ggpredict(lmer(gs~rescale(CO2)*rescale(meanSWC)+time_scaled + (1|Plot), data=filter(final_df,Spp=="V")), 
            terms=c("CO2","meanSWC [4,42]"))%>% plot(rawdata=T,ci=T,colors=c("red","blue")) + labs(title="gs, V *"),
  ggpredict(lmer(gs~rescale(CO2)+rescale(meanSWC)+time_scaled + (1|Plot), data=filter(final_df,Spp=="V")), 
            terms=c("CO2","meanSWC [4,42]"))%>% plot(rawdata=T,ci=T,colors=c("red","blue")) + labs(title="gs, V +"),
  ggpredict(lmer(gs~rescale(CO2)*rescale(meanSWC)+time_scaled + (1|Plot), data=filter(final_df,Spp=="L",)), 
            terms=c("CO2","meanSWC [4,42]"))%>% plot(rawdata=T,ci=T,colors=c("red","blue")) + labs(title="gs, L *"),
  ggpredict(lmer(gs~rescale(CO2)+rescale(meanSWC)+time_scaled + (1|Plot), data=filter(final_df,Spp=="L",)), 
            terms=c("CO2","meanSWC [4,42]"))%>% plot(rawdata=T,ci=T,colors=c("red","blue")) + labs(title="gs, L +") )
#summary(lm(gs~rescale(CO2)+rescale(meanSWC), data=plotmeans.V)) 
#summary(lm(gs~rescale(CO2)+rescale(meanSWC)+time_scaled, data=plotmeans.V)) 
summary(lmer(gs~rescale(CO2)+rescale(meanSWC)+time_scaled + (1|Plot), data=filter(final_df,Spp=="V")))
Anova(lmer(gs~rescale(CO2)+rescale(meanSWC)+time_scaled + (1|Plot), data=filter(final_df,Spp=="V")),  test="F")
plot(lmer(gs~rescale(CO2)+rescale(meanSWC)+time_scaled + (1|Plot), data=filter(final_df,Spp=="V")))
qqmath(lmer(gs~rescale(CO2)+rescale(meanSWC)+time_scaled + (1|Plot), data=filter(final_df,Spp=="V")))
shapiro.test(resid(lmer(gs~rescale(CO2)+rescale(meanSWC)+time_scaled + (1|Plot), data=filter(final_df,Spp=="V")))) # W = 0.98469, p-value = 0.9113

#summary(lm(gs~rescale(CO2)+rescale(meanSWC), data=plotmeans.L)) 
#summary(lm(gs~rescale(CO2)*rescale(meanSWC)+time_scaled, data=plotmeans.L)) 
summary(lmer(gs~rescale(CO2)*rescale(meanSWC)+time_scaled + (1|Plot), data=filter(final_df,Spp=="L")))
Anova(lmer(gs~rescale(CO2)*rescale(meanSWC)+time_scaled + (1|Plot), data=filter(final_df,Spp=="L")), test = "F")
plot(lmer(gs~rescale(CO2)*rescale(meanSWC)+time_scaled + (1|Plot), data=filter(final_df,Spp=="L")))
qqmath(lmer(gs~rescale(CO2)*rescale(meanSWC)+time_scaled + (1|Plot), data=filter(final_df,Spp=="L")))
shapiro.test(resid(lmer(gs~rescale(CO2)*rescale(meanSWC)+time_scaled + (1|Plot), data=filter(final_df,Spp=="L")))) # W = 0.98152, p-value = 0.7457

AICc(lm(gs~rescale(CO2)*rescale(meanSWC)+time_scaled, data=plotmeans.L, REML = FALSE),
     lm(gs~rescale(CO2)*rescale(meanSWC), data=plotmeans.L, REML = FALSE),
     lm(gs~rescale(CO2)+rescale(meanSWC)+time_scaled, data=plotmeans.L, REML = F),
     lm(gs~rescale(CO2)+rescale(meanSWC), data=plotmeans.L, REML = F),
     lm(gs~ 1, data = plotmeans.L, REML = F))

# WUE
# lmer(WUE ~ rescale(CO2) * rescale(meanSWC) + time_scaled + (1 | Plot), data = filter(final_df, Spp == "V"))  7 298.4672
# lmer(WUE ~ rescale(CO2) * rescale(meanSWC) + (1 | Plot), data = filter(final_df, Spp == "V"))                6 306.5223
# lmer(WUE ~ rescale(CO2) + rescale(meanSWC) + time_scaled + (1 | Plot), data = filter(final_df, Spp == "V"))  6 307.2435
# lmer(WUE ~ rescale(CO2) + rescale(meanSWC) + (1 | Plot), data = filter(final_df, Spp == "V"))                5 315.0561
# lmer(WUE ~ (1 | Plot), data = filter(final_df, Spp == "V"))                                                  3 329.6529

grid.arrange(
  ggpredict(lmer(WUE~rescale(CO2)*rescale(meanSWC)+time_scaled + (1|Plot), data=filter(final_df,Spp=="V")), 
            terms=c("CO2","meanSWC [4,42]"))%>% plot(rawdata=T,ci=T,colors=c("red","blue")) + labs(title="WUE, V *"),
  ggpredict(lmer(WUE~rescale(CO2)+rescale(meanSWC)+time_scaled + (1|Plot), data=filter(final_df,Spp=="V")), 
            terms=c("CO2","meanSWC [4,42]"))%>% plot(rawdata=T,ci=T,colors=c("red","blue")) + labs(title="WUE, V +"),
  ggpredict(lmer(WUE~rescale(CO2)*rescale(meanSWC)+time_scaled + (1|Plot), data=filter(final_df,Spp=="L",)), 
            terms=c("CO2","meanSWC [4,42]"))%>% plot(rawdata=T,ci=T,colors=c("red","blue")) + labs(title="WUE, L *"),
  ggpredict(lmer(WUE~rescale(CO2)+rescale(meanSWC)+time_scaled + (1|Plot), data=filter(final_df,Spp=="L",)), 
            terms=c("CO2","meanSWC [4,42]"))%>% plot(rawdata=T,ci=T,colors=c("red","blue")) + labs(title="WUE, L +") )
#summary(lm(WUE~rescale(CO2)+rescale(meanSWC), data=plotmeans.V)) 
#summary(lm(WUE~rescale(CO2)+rescale(meanSWC)+time_scaled, data=plotmeans.V)) 
summary(lmer(WUE~rescale(CO2)*rescale(meanSWC)+time_scaled + (1|Plot), data=filter(final_df,Spp=="V")))
summary(lmer(WUE~rescale(CO2)+rescale(meanSWC)+time_scaled + (1|Plot), data=filter(final_df,Spp=="V")))
plot(lmer(WUE~rescale(CO2)+rescale(meanSWC)+time_scaled + (1|Plot), data=filter(final_df,Spp=="V")))
qqmath(lmer(WUE~rescale(CO2)+rescale(meanSWC)+time_scaled + (1|Plot), data=filter(final_df,Spp=="V")))
shapiro.test(resid(lmer(WUE~rescale(CO2)+rescale(meanSWC)+time_scaled + (1|Plot), data=filter(final_df,Spp=="V")))) # W = 0.97932, p-value = 0.7514



#summary(lm(WUE~rescale(CO2)+rescale(meanSWC), data=plotmeans.L)) 
#summary(lm(WUE~rescale(CO2)+rescale(meanSWC)+time_scaled, data=plotmeans.L)) 
summary(lmer(WUE~rescale(CO2)+rescale(meanSWC)+time_scaled + (1|Plot), data=filter(final_df,Spp=="L")))
plot(lmer(WUE~rescale(CO2)+rescale(meanSWC)+time_scaled + (1|Plot), data=filter(final_df,Spp=="L")))
qqmath(lmer(WUE~rescale(CO2)+rescale(meanSWC)+time_scaled + (1|Plot), data=filter(final_df,Spp=="L")))
shapiro.test(resid(lmer(WUE~rescale(CO2)+rescale(meanSWC)+time_scaled + (1|Plot), data=filter(final_df,Spp=="L")))) # W = 0.97765, p-value = 0.5721

AICc(lmer(WUE~rescale(CO2)*rescale(meanSWC)+time_scaled + (1|Plot), data=filter(final_df,Spp=="L")), lmer(WUE~rescale(CO2)*rescale(meanSWC) + (1|Plot), data=filter(final_df,Spp=="L")),lmer(WUE~rescale(CO2)+rescale(meanSWC)+time_scaled + (1|Plot), data=filter(final_df,Spp=="L")),lmer(WUE~rescale(CO2)+rescale(meanSWC) + (1|Plot), data=filter(final_df,Spp=="L")), lm(WUE~rescale(CO2)+rescale(meanSWC), data=plotmeans.L), lm(WUE~rescale(CO2)+rescale(meanSWC)+time_scaled, data=plotmeans.L), lm(WUE~rescale(CO2)*rescale(meanSWC)+time_scaled, data=plotmeans.L), lm(WUE~rescale(CO2)*rescale(meanSWC), data=plotmeans.L))

# tot_area
# lm(tot_area ~ rescale(CO2) * rescale(meanSWC), data = plotmeans.V)  5 188.47553
# lm(tot_area ~ rescale(CO2) + rescale(meanSWC), data = plotmeans.V)  4 173.16955
# lm(tot_area ~ 1, data = plotmeans.V)                                2 158.9249 !
# lm(tot_area ~ rescale(CO2) * rescale(meanSWC), data = plotmeans.L)  5 339.5168
# lm(tot_area ~ rescale(CO2) + rescale(meanSWC), data = plotmeans.L)  4 335.2133 !
# lm(tot_area ~ 1, data = plotmeans.L)                                2 334.1268
grid.arrange(
  ggpredict(lm(tot_area~rescale(CO2)*rescale(meanSWC), data=plotmeans.V), 
            terms=c("CO2","meanSWC [4,42]"))%>% plot(rawdata=T,ci=T,colors=c("red","blue")) + labs(title="tot_area, V *"),
  ggpredict(lm(tot_area~rescale(CO2)+rescale(meanSWC), data=plotmeans.V), 
            terms=c("CO2","meanSWC [4,42]"))%>% plot(rawdata=T,ci=T,colors=c("red","blue")) + labs(title="tot_area, V +"),
  ggpredict(lm(tot_area~rescale(CO2)*rescale(meanSWC), data=plotmeans.L), 
            terms=c("CO2","meanSWC [4,42]"))%>% plot(rawdata=T,ci=T,colors=c("red","blue")) + labs(title="tot_area, L *"),
  ggpredict(lm(tot_area~rescale(CO2)+rescale(meanSWC), data=plotmeans.L), 
            terms=c("CO2","meanSWC [4,42]"))%>% plot(rawdata=T,ci=T,colors=c("red","blue")) + labs(title="tot_area, L +"),
  nrow=2)
summary(lm(tot_area~rescale(CO2)+rescale(meanSWC), data=plotmeans.V)) 
Anova(lm(tot_area~rescale(CO2)+rescale(meanSWC), data=plotmeans.V)) 
plot(lm(tot_area~rescale(CO2)+rescale(meanSWC), data=plotmeans.V))
qqPlot(lm(tot_area~rescale(CO2)+rescale(meanSWC), data=plotmeans.V))
shapiro.test(resid(lm(tot_area~rescale(CO2)+rescale(meanSWC), data=plotmeans.V))) # W = 0.88671, p-value = 0.218

summary(lm(tot_area~rescale(CO2)+rescale(meanSWC), data=plotmeans.L))
Anova(lm(tot_area~rescale(CO2)+rescale(meanSWC), data=plotmeans.L), test="F")
plot(lm(tot_area~rescale(CO2)+rescale(meanSWC), data=plotmeans.L))
qqPlot(lm(tot_area~rescale(CO2)+rescale(meanSWC), data=plotmeans.L))
shapiro.test(resid(lm(tot_area~rescale(CO2)+rescale(meanSWC), data=plotmeans.L))) # W = 0.98008, p-value = 0.9642

# SRL
# lm(SRL ~ rescale(CO2) * rescale(meanSWC), data = plotmeans.V)  5 220.3935
# lm(SRL ~ rescale(CO2) + rescale(meanSWC), data = plotmeans.V)  4 216.0423
# lm(SRL ~ 1, data = plotmeans.V)                                2 209.6870 1
# lm(SRL ~ rescale(CO2) * rescale(meanSWC), data = plotmeans.L)  5 219.0240 !
# lm(SRL ~ rescale(CO2) + rescale(meanSWC), data = plotmeans.L)  4 220.1671
# lm(SRL ~ 1, data = plotmeans.L)                                2 219.1741
grid.arrange(
  ggpredict(lm(SRL~rescale(CO2)*rescale(meanSWC), data=plotmeans.V), 
            terms=c("CO2","meanSWC [4,42]"))%>% plot(rawdata=T,ci=T,colors=c("red","blue")) + labs(title="SRL, V *"),
  ggpredict(lm(SRL~rescale(CO2)+rescale(meanSWC), data=plotmeans.V), 
            terms=c("CO2","meanSWC [4,42]"))%>% plot(rawdata=T,ci=T,colors=c("red","blue")) + labs(title="SRL, V +"),
  ggpredict(lm(SRL~rescale(CO2)*rescale(meanSWC), data=plotmeans.L), 
            terms=c("CO2","meanSWC [4,42]"))%>% plot(rawdata=T,ci=T,colors=c("red","blue")) + labs(title="SRL, L *"),
  ggpredict(lm(SRL~rescale(CO2)+rescale(meanSWC), data=plotmeans.L), 
            terms=c("CO2","meanSWC [4,42]"))%>% plot(rawdata=T,ci=T,colors=c("red","blue")) + labs(title="SRL, L +") )
summary(lm(SRL~rescale(CO2)+rescale(meanSWC), data=plotmeans.V)) 
Anova(lm(SRL~rescale(CO2)+rescale(meanSWC), data=plotmeans.V)) 
plot(lm(SRL~rescale(CO2)+rescale(meanSWC), data=plotmeans.V))
qqPlot(lm(SRL~rescale(CO2)+rescale(meanSWC), data=plotmeans.V))
shapiro.test(resid(lm(SRL~rescale(CO2)+rescale(meanSWC), data=plotmeans.V))) # W = 0.83255, p-value = 0.007623

summary(lm(SRL~rescale(CO2)*rescale(meanSWC), data=plotmeans.L)) 
Anova(lm(SRL~rescale(CO2)*rescale(meanSWC), data=plotmeans.L), test = "F") 
plot(lm(SRL~rescale(CO2)*rescale(meanSWC), data=plotmeans.L))
qqPlot(lm(SRL~rescale(CO2)*rescale(meanSWC), data=plotmeans.L))
shapiro.test(resid(lm(SRL~rescale(CO2)*rescale(meanSWC), data=plotmeans.L))) # W = 0.90979, p-value = 0.1155

# d13C
# lm(d13C ~ rescale(CO2) * rescale(meanSWC), data = plotmeans.V)  5 53.35948 
# lm(d13C ~ rescale(CO2) + rescale(meanSWC), data = plotmeans.V)  4 48.75018 !
# lm(d13C ~ 1, data = plotmeans.V)                                2 52.71464
# lm(d13C ~ rescale(CO2) * rescale(meanSWC), data = plotmeans.L)  5 41.14793
# lm(d13C ~ rescale(CO2) + rescale(meanSWC), data = plotmeans.L)  4 38.36978 !
# lm(d13C ~ 1, data = plotmeans.L)                                2 40.00270
grid.arrange(
  ggpredict(lm(d13C~rescale(CO2)*rescale(meanSWC), data=plotmeans.V), 
            terms=c("CO2","meanSWC [4,42]"))%>% plot(rawdata=T,ci=T,colors=c("red","blue")) + labs(title="d13C, V *"),
  ggpredict(lm(d13C~rescale(CO2)+rescale(meanSWC), data=plotmeans.V), 
            terms=c("CO2","meanSWC [4,42]"))%>% plot(rawdata=T,ci=T,colors=c("red","blue")) + labs(title="d13C, V +"),
  ggpredict(lm(d13C~rescale(CO2)*rescale(meanSWC), data=plotmeans.L), 
            terms=c("CO2","meanSWC [4,42]"))%>% plot(rawdata=T,ci=T,colors=c("red","blue")) + labs(title="d13C, L *"),
  ggpredict(lm(d13C~rescale(CO2)+rescale(meanSWC), data=plotmeans.L), 
            terms=c("CO2","meanSWC [4,42]"))%>% plot(rawdata=T,ci=T,colors=c("red","blue")) + labs(title="d13C, L +") )
summary(lm(d13C~rescale(CO2)+rescale(meanSWC), data=plotmeans.V)) 
Anova(lm(d13C~rescale(CO2)+rescale(meanSWC), data=plotmeans.V)) 
plot(lm(d13C~rescale(CO2)+rescale(meanSWC), data=plotmeans.V))
qqPlot(lm(d13C~rescale(CO2)+rescale(meanSWC), data=plotmeans.V))
shapiro.test(resid(lm(d13C~rescale(CO2)+rescale(meanSWC), data=plotmeans.V))) # W = 0.95619, p-value = 0.6265

summary(lm(d13C~rescale(CO2)+rescale(meanSWC), data=plotmeans.L))
Anova(lm(d13C~rescale(CO2)+rescale(meanSWC), data=plotmeans.L), test="F")
plot(lm(d13C~rescale(CO2)+rescale(meanSWC), data=plotmeans.L))
qqPlot(lm(d13C~rescale(CO2)+rescale(meanSWC), data=plotmeans.L))
shapiro.test(resid(lm(d13C~rescale(CO2)+rescale(meanSWC), data=plotmeans.L))) # W = 0.94774, p-value = 0.4897
# note intx (not significant) that mirrors gs effect:
# lm(formula = d13C ~ rescale(CO2) * rescale(meanSWC), data = plotmeans.L)
# rescale(CO2):rescale(meanSWC)  -1.78170    p = 0.250

# Ht 8
# lm(Ht.mm..8 ~ rescale(CO2) * rescale(meanSWC), data = plotmeans.V)  5 138.2738
# lm(Ht.mm..8 ~ rescale(CO2) + rescale(meanSWC), data = plotmeans.V)  4 129.6069
# lm(Ht.mm..8 ~ 1, data = plotmeans.V)                                2 126.8910 !!!
# lm(Ht.mm..8 ~ rescale(CO2) * rescale(meanSWC), data = plotmeans.L)  5 188.0261 !
# lm(Ht.mm..8 ~ rescale(CO2) + rescale(meanSWC), data = plotmeans.L)  4 189.2139
# lm(Ht.mm..8 ~ 1, data = plotmeans.L)                                2 199.4428
grid.arrange(
  ggpredict(lm(Ht.mm..8~rescale(CO2)*rescale(meanSWC), data=plotmeans.V), 
            terms=c("CO2","meanSWC [4,42]"))%>% plot(rawdata=T,ci=T,colors=c("red","blue")) + labs(title="Ht.mm..8, V *"),
  ggpredict(lm(Ht.mm..8~rescale(CO2)+rescale(meanSWC), data=plotmeans.V), 
            terms=c("CO2","meanSWC [4,42]"))%>% plot(rawdata=T,ci=T,colors=c("red","blue")) + labs(title="Ht.mm..8, V +"),
  ggpredict(lm(Ht.mm..8~rescale(CO2)*rescale(meanSWC), data=plotmeans.L), 
            terms=c("CO2","meanSWC [4,42]"))%>% plot(rawdata=T,ci=T,colors=c("red","blue")) + labs(title="Ht.mm..8, L *"),
  ggpredict(lm(Ht.mm..8~rescale(CO2)+rescale(meanSWC), data=plotmeans.L), 
            terms=c("CO2","meanSWC [4,42]"))%>% plot(rawdata=T,ci=T,colors=c("red","blue")) + labs(title="Ht.mm..8, L +"),
  nrow=2)
summary(lm(Ht.mm..8~rescale(CO2)+rescale(meanSWC), data=plotmeans.V)) 
Anova(lm(Ht.mm..8~rescale(CO2)+rescale(meanSWC), data=plotmeans.V)) 
plot(lm(Ht.mm..8~rescale(CO2)+rescale(meanSWC), data=plotmeans.V))
qqPlot(lm(Ht.mm..8~rescale(CO2)+rescale(meanSWC), data=plotmeans.V))
shapiro.test(resid(lm(Ht.mm..8~rescale(CO2)+rescale(meanSWC), data=plotmeans.V))) # W = 0.98947, p-value = 0.9961

summary(lm(Ht.mm..8~rescale(CO2)*rescale(meanSWC), data=plotmeans.L)) 
Anova(lm(Ht.mm..8~rescale(CO2)*rescale(meanSWC), data=plotmeans.L), test="F") 
plot(lm(Ht.mm..8~rescale(CO2)*rescale(meanSWC), data=plotmeans.L))
qqPlot(lm(Ht.mm..8~rescale(CO2)*rescale(meanSWC), data=plotmeans.L))
shapiro.test(resid(lm(Ht.mm..8~rescale(CO2)*rescale(meanSWC), data=plotmeans.L))) # W = 0.97135, p-value = 0.8599

# rootmass
# lm(rootmass_g ~ rescale(CO2) * rescale(meanSWC), data = plotmeans.V)  5 71.86762
# lm(rootmass_g ~ rescale(CO2) + rescale(meanSWC), data = plotmeans.V)  4 67.81968 !
# lm(rootmass_g ~ 1, data = plotmeans.V)                                2 69.63197
# lm(rootmass_g ~ rescale(CO2) * rescale(meanSWC), data = plotmeans.L)  5 81.60029
# lm(rootmass_g ~ rescale(CO2) + rescale(meanSWC), data = plotmeans.L)  4 77.51149
# lm(rootmass_g ~ 1, data = plotmeans.L)                                2 75.97478 !
grid.arrange(
  ggpredict(lm(rootmass_g~rescale(CO2)*rescale(meanSWC), data=plotmeans.V), 
            terms=c("CO2","meanSWC [4,42]"))%>% plot(rawdata=T,ci=T,colors=c("red","blue")) + labs(title="rootmass_g, V *"),
  ggpredict(lm(rootmass_g~rescale(CO2)+rescale(meanSWC), data=plotmeans.V), 
            terms=c("CO2","meanSWC [4,42]"))%>% plot(rawdata=T,ci=T,colors=c("red","blue")) + labs(title="rootmass_g, V +"),
  ggpredict(lm(rootmass_g~rescale(CO2)*rescale(meanSWC), data=plotmeans.L), 
            terms=c("CO2","meanSWC [4,42]"))%>% plot(rawdata=T,ci=T,colors=c("red","blue")) + labs(title="rootmass_g, L *"),
  ggpredict(lm(rootmass_g~rescale(CO2)+rescale(meanSWC), data=plotmeans.L), 
            terms=c("CO2","meanSWC [4,42]"))%>% plot(rawdata=T,ci=T,colors=c("red","blue")) + labs(title="rootmass_g, L +") )
summary(lm(rootmass_g~rescale(CO2)+rescale(meanSWC), data=plotmeans.V)) 
Anova(lm(rootmass_g~rescale(CO2)+rescale(meanSWC), data=plotmeans.V)) 
plot(lm(rootmass_g~rescale(CO2)+rescale(meanSWC), data=plotmeans.V))
qqPlot(lm(rootmass_g~rescale(CO2)+rescale(meanSWC), data=plotmeans.V))
shapiro.test(resid(lm(rootmass_g~rescale(CO2)+rescale(meanSWC), data=plotmeans.V))) # W = 0.97232, p-value = 0.8746

summary(lm(rootmass_g~rescale(CO2)+rescale(meanSWC), data=plotmeans.L)) 
plot(lm(rootmass_g~rescale(CO2)+rescale(meanSWC), data=plotmeans.L))
qqPlot(lm(rootmass_g~rescale(CO2)+rescale(meanSWC), data=plotmeans.L))
shapiro.test(resid(lm(rootmass_g~rescale(CO2)+rescale(meanSWC), data=plotmeans.L))) # W = 0.96603, p-value = 0.7709

#####

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

# try thinking about it for just one species, just for totmass, just for mean
fig2_meanse <- final_df_nh %>% 
# fig2_meanse <- final_df %>% 
  filter(Spp=="L") %>% 
  select(Tmt, totmass, rootshoot, Ht.mm..8, lwc, Anet, gs, WUE, tot_area, SRL, d13C, rootmass_g) %>%
  rename(leaf.area = tot_area) %>% 
  rename(root.mass = rootmass_g) %>% 
  rename(tot.mass = totmass) %>% 
  rename(final.ht = Ht.mm..8) %>% 
  mutate(d13C = abs(d13C)) %>% 
  group_by(Tmt) %>% 
  summarise_if(is.numeric, list(y=mean_narm, ymin=minus_se, ymax=plus_se)) %>%
  t() %>% data.frame() 
# this is good! I want the columns to be the Tmts, and I want to move rownames into 2 columns: variable and y*

#names(fig2_meanse) <- fig2_meanse[1,]
fig2_meanse <- fig2_meanse[-1,]
fig2_meanse[,5] <- rownames(fig2_meanse)
colnames(fig2_meanse) <- c("AD","AW","ED","EW","var_y")

fig2_meanse <- separate(fig2_meanse, var_y, into = c("variable","y"), sep="_")
# still need to percentify the tmt columns with reference to AD

fig2_meanse$standardD <- c(rep(fig2_meanse[1:11,1], 3))
fig2_meanse$standardD2 <- c(fig2_meanse[1:11,1], fig2_meanse[23:33,1], fig2_meanse[12:22,1])
fig2_meanse$standardW <- c(rep(fig2_meanse[1:11,2], 3))
fig2_meanse$standardW2 <- c(fig2_meanse[1:11,2], fig2_meanse[23:33,2], fig2_meanse[12:22,2])

fig2_meanse <- fig2_meanse %>% 
  mutate(across(!c("variable","y"), as.numeric)) %>% 
  # mutate(ED1 = (ED - standardD)*100/standardD) %>%
  # mutate(EW1 = (EW - standardW)*100/standardW) # %>% 
  mutate(ED1 = (ED - standardD2)*100/standardD2) %>%
  mutate(EW1 = (EW - standardW2)*100/standardW2)

fig2_meanse["d13C_y","ED1"] <- -fig2_meanse["d13C_y","ED1"]
fig2_meanse["d13C_ymin","ED1"] <- -fig2_meanse["d13C_ymin","ED1"]
fig2_meanse["d13C_ymax","ED1"] <- -fig2_meanse["d13C_ymax","ED1"]

fig2_meanse["d13C_y","EW1"] <- -fig2_meanse["d13C_y","EW1"]
fig2_meanse["d13C_ymin","EW1"] <- -fig2_meanse["d13C_ymin","EW1"]
fig2_meanse["d13C_ymax","EW1"] <- -fig2_meanse["d13C_ymax","EW1"]

L_lwcd13C <- final_df_nh %>% 
  filter(Spp=="L") %>% 
  select(Tmt, H2OTmt, lwc, d13C) %>% 
  mutate(d13C = abs(d13C)) %>% 
  #mutate(overallmin = min(d13C, na.rm=T), overallmax=max(d13C, na.rm=T)) %>% View() # 30.71429, 46.54088; 26.29, 31.09
  group_by(Tmt) %>%
  summarise_if(is.numeric, list(y=mean_narm, ymin=minus_se, ymax=plus_se)) %>% 
  t() %>% data.frame() 

L_lwcd13C <- L_lwcd13C[-1,]
L_lwcd13C[,5] <- rownames(L_lwcd13C)
colnames(L_lwcd13C) <- c("AD","AW","ED","EW","var_y")
L_lwcd13C <- separate(L_lwcd13C, var_y, into = c("variable","y"), sep="_")
L_lwcd13C$standardD2 <- c(L_lwcd13C[1:2,1], L_lwcd13C[5:6,1], L_lwcd13C[3:4,1])
L_lwcd13C$standardW2 <- c(L_lwcd13C[1:2,2], L_lwcd13C[5:6,2], L_lwcd13C[3:4,2])

L_lwcd13C <- L_lwcd13C %>% 
  mutate(across(!c("variable","y"), as.numeric)) %>% 
  mutate(ED1_lwc = (ED - standardD2)*100/(46.54088- 30.71429)) %>%
  mutate(EW1_lwc = (EW - standardW2)*100/(46.54088- 30.71429)) %>% 
  mutate(ED1_d13C = (ED - standardD2)*100/(31.09-26.29)) %>%
  mutate(EW1_d13C = (EW - standardW2)*100/(31.09-26.29))

# reverse sign
L_lwcd13C["d13C_y","ED1_d13C"] <- -L_lwcd13C["d13C_y","ED1_d13C"]
L_lwcd13C["d13C_ymin","ED1_d13C"] <- -L_lwcd13C["d13C_ymin","ED1_d13C"]
L_lwcd13C["d13C_ymax","ED1_d13C"] <- -L_lwcd13C["d13C_ymax","ED1_d13C"]

L_lwcd13C["d13C_y","EW1_d13C"] <- -L_lwcd13C["d13C_y","EW1_d13C"]
L_lwcd13C["d13C_ymin","EW1_d13C"] <- -L_lwcd13C["d13C_ymin","EW1_d13C"]
L_lwcd13C["d13C_ymax","EW1_d13C"] <- -L_lwcd13C["d13C_ymax","EW1_d13C"]

# add corrected d13C to table 
fig2_meanse["d13C_y","ED1"] <- L_lwcd13C["d13C_y","ED1_d13C"]
fig2_meanse["d13C_ymin","ED1"] <- L_lwcd13C["d13C_ymin","ED1_d13C"] 
fig2_meanse["d13C_ymax","ED1"] <- L_lwcd13C["d13C_ymax","ED1_d13C"]

fig2_meanse["d13C_y","EW1"] <- L_lwcd13C["d13C_y","EW1_d13C"]
fig2_meanse["d13C_ymin","EW1"] <- L_lwcd13C["d13C_ymin","EW1_d13C"]
fig2_meanse["d13C_ymax","EW1"] <- L_lwcd13C["d13C_ymax","EW1_d13C"]

fig2_meanse["lwc_y","ED1"] <- L_lwcd13C["lwc_y","ED1_d13C"]
fig2_meanse["lwc_ymin","ED1"] <- L_lwcd13C["lwc_ymin","ED1_d13C"] 
fig2_meanse["lwc_ymax","ED1"] <- L_lwcd13C["lwc_ymax","ED1_d13C"]

fig2_meanse["lwc_y","EW1"] <- L_lwcd13C["lwc_y","EW1_d13C"]
fig2_meanse["lwc_ymin","EW1"] <- L_lwcd13C["lwc_ymin","EW1_d13C"]
fig2_meanse["lwc_ymax","EW1"] <- L_lwcd13C["lwc_ymax","EW1_d13C"]

  #  mutate_if(is.numeric, list(pct_chg = (AD - .x)*100/AD)) %>% View()
# get Tmt to be its own column with pivot_longer
variable_order <- c("tot.mass", "root.mass", "final.ht", "leaf.area", "rootshoot", "SRL", "lwc", "d13C", "Anet", "gs", "WUE")
#variable_order <- c("tot.mass", "root.mass", "final.ht", "leaf.area", "rootshoot", "SRL", "Anet", "gs", "WUE")

# add n= ... + geom_text(data = nequals_licor, aes(x = Tmt, y = 0.01, label = paste0("N = ",n)))
fig2_nequals <- final_df_nh %>% 
  filter(Spp=="L") %>% 
  select(Tmt, H2OTmt, totmass, rootshoot, Ht.mm..8, lwc, Anet, gs, WUE, tot_area, SRL, d13C, rootmass_g) %>% 
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
colnames(fig2_nequals) <- c("D","W", "variable")

fig2L_nh <- fig2_meanse %>% 
# fig2L <- fig2_meanse %>% 
  rename(Dry = ED1, Watered = EW1) %>% 
  pivot_longer(cols=c("Dry","Watered"), names_to="Treatment", values_to="value") %>% 
  select(variable, y, Treatment, value) %>% 
  # filter(variable != "d13C") %>% 
  # filter(variable != "lwc") %>% 
  pivot_wider(names_from = "y", values_from = "value") %>% 
ggplot() +
  geom_abline(color= "red", linetype="dashed", slope = 0, intercept= 0) +
  geom_pointrange(aes(x=factor(variable, level=variable_order), y=y, ymin=ymin, ymax=ymax, group=Treatment, color=Treatment, shape=Treatment), size=1, linewidth=1, position=position_dodge(width=0.2)) + scale_color_manual(values=c("red","blue")) + scale_shape_manual(values = c(1,16)) +
  ylim(-105, 217) +
  geom_text(data = fig2_nequals, aes(x = variable, y = -75, label = paste0("N = ",D)), color="red", size = 4) +
  geom_text(data = fig2_nequals, aes(x = variable, y = -95, label = paste0("N = ",W)), color="blue", size = 4) +
  ggtitle("B. Quercus wislizenii (live oak)") +
  ylab("% change with eCO2") + xlab("Plant Response") +
  theme_classic(base_size = 18) 
  

# now for V!
fig2_meanseV <- final_df_nh %>% 
#  fig2_meanseV <- final_df %>% 
  filter(Spp=="V") %>% 
  select(Tmt, totmass, rootmass_g, Ht.mm..8, tot_area, rootshoot, SRL, lwc, d13C, Anet, gs, WUE) %>% 
  # select(Tmt, totmass, rootshoot, Ht.mm..8, lwc, Anet, gs, WUE, tot_area, SRL, d13C, rootmass_g) %>%
  rename(leaf.area = tot_area) %>% 
  rename(root.mass = rootmass_g) %>% 
  rename(tot.mass = totmass) %>% 
  rename(final.ht = Ht.mm..8) %>% 
  group_by(Tmt) %>% 
  summarise_if(is.numeric, list(y=mean_narm, ymin=minus_se, ymax=plus_se)) %>% 
  t() %>% data.frame()
# this is good! I want the columns to be the Tmts, and I want to move rownames into 2 columns: variable and y*

fig2_meanseV <- fig2_meanseV[-1,]
fig2_meanseV[,5] <- rownames(fig2_meanseV)
colnames(fig2_meanseV) <- c("AD","AW","ED","EW","var_y")

fig2_meanseV <- separate(fig2_meanseV, var_y, into = c("variable","y"), sep="_")
# still need to percentify the tmt columns with reference to AD

fig2_meanseV$standardD <- c(rep(fig2_meanseV[1:11,1], 3))
fig2_meanseV$standardD2 <- c(fig2_meanseV[1:11,1], fig2_meanseV[23:33,1], fig2_meanseV[12:22,1])
fig2_meanseV$standardW <- c(rep(fig2_meanseV[1:11,2], 3))
fig2_meanseV$standardW2 <- c(fig2_meanseV[1:11,2], fig2_meanseV[23:33,2], fig2_meanseV[12:22,2])

fig2_meanseV <- fig2_meanseV %>% 
  mutate(across(!c("variable","y"), as.numeric)) %>% 
  # mutate(ED1 = (ED - standardD)*100/standardD) %>%
  # mutate(EW1 = (EW - standardW)*100/standardW) # %>% 
  mutate(ED1 = (ED - standardD2)*100/standardD2) %>%
  mutate(EW1 = (EW - standardW2)*100/standardW2)

fig2_meanseV["d13C_y","ED1"] <- -fig2_meanseV["d13C_y","ED1"]
fig2_meanseV["d13C_ymin","ED1"] <- -fig2_meanseV["d13C_ymin","ED1"]
fig2_meanseV["d13C_ymax","ED1"] <- -fig2_meanseV["d13C_ymax","ED1"]

fig2_meanseV["d13C_y","EW1"] <- -fig2_meanseV["d13C_y","EW1"]
fig2_meanseV["d13C_ymin","EW1"] <- -fig2_meanseV["d13C_ymin","EW1"]
fig2_meanseV["d13C_ymax","EW1"] <- -fig2_meanseV["d13C_ymax","EW1"]

V_lwcd13C <- final_df_nh %>% 
  filter(Spp=="V") %>% 
  select(Tmt, H2OTmt, lwc, d13C) %>% 
  mutate(d13C = abs(d13C)) %>% 
  #mutate(overallmin = min(lwc, na.rm=T), overallmax=max(lwc, na.rm=T)) %>% View() # 14.28571, 50; 26.62, 31.77
  group_by(Tmt) %>%
  summarise_if(is.numeric, list(y=mean_narm, ymin=minus_se, ymax=plus_se)) %>% 
  t() %>% data.frame() 

V_lwcd13C <- V_lwcd13C[-1,]
V_lwcd13C[,5] <- rownames(V_lwcd13C)
colnames(V_lwcd13C) <- c("AD","AW","ED","EW","var_y")
V_lwcd13C <- separate(V_lwcd13C, var_y, into = c("variable","y"), sep="_")
V_lwcd13C$standardD2 <- c(V_lwcd13C[1:2,1], V_lwcd13C[5:6,1], V_lwcd13C[3:4,1])
V_lwcd13C$standardW2 <- c(V_lwcd13C[1:2,2], V_lwcd13C[5:6,2], V_lwcd13C[3:4,2])

V_lwcd13C <- V_lwcd13C %>% 
  mutate(across(!c("variable","y"), as.numeric)) %>% 
  mutate(ED1_lwc = (ED - standardD2)*100/(50 - 14.28571)) %>%
  mutate(EW1_lwc = (EW - standardW2)*100/(50 - 14.28571)) %>% 
  mutate(ED1_d13C = (ED - standardD2)*100/(31.77-26.62)) %>%
  mutate(EW1_d13C = (EW - standardW2)*100/(31.77-26.62))

# reverse sign
V_lwcd13C["d13C_y","ED1_d13C"] <- -V_lwcd13C["d13C_y","ED1_d13C"]
V_lwcd13C["d13C_ymin","ED1_d13C"] <- -V_lwcd13C["d13C_ymin","ED1_d13C"]
V_lwcd13C["d13C_ymax","ED1_d13C"] <- -V_lwcd13C["d13C_ymax","ED1_d13C"]

V_lwcd13C["d13C_y","EW1_d13C"] <- -V_lwcd13C["d13C_y","EW1_d13C"]
V_lwcd13C["d13C_ymin","EW1_d13C"] <- -V_lwcd13C["d13C_ymin","EW1_d13C"]
V_lwcd13C["d13C_ymax","EW1_d13C"] <- -V_lwcd13C["d13C_ymax","EW1_d13C"]

# add corrected d13C to table 
fig2_meanseV["d13C_y","ED1"] <- V_lwcd13C["d13C_y","ED1_d13C"]
fig2_meanseV["d13C_ymin","ED1"] <- V_lwcd13C["d13C_ymin","ED1_d13C"] 
fig2_meanseV["d13C_ymax","ED1"] <- V_lwcd13C["d13C_ymax","ED1_d13C"]

fig2_meanseV["d13C_y","EW1"] <- V_lwcd13C["d13C_y","EW1_d13C"]
fig2_meanseV["d13C_ymin","EW1"] <- V_lwcd13C["d13C_ymin","EW1_d13C"]
fig2_meanseV["d13C_ymax","EW1"] <- V_lwcd13C["d13C_ymax","EW1_d13C"]

fig2_meanseV["lwc_y","ED1"] <- V_lwcd13C["lwc_y","ED1_d13C"]
fig2_meanseV["lwc_ymin","ED1"] <- V_lwcd13C["lwc_ymin","ED1_d13C"] 
fig2_meanseV["lwc_ymax","ED1"] <- V_lwcd13C["lwc_ymax","ED1_d13C"]

fig2_meanseV["lwc_y","EW1"] <- V_lwcd13C["lwc_y","EW1_d13C"]
fig2_meanseV["lwc_ymin","EW1"] <- V_lwcd13C["lwc_ymin","EW1_d13C"]
fig2_meanseV["lwc_ymax","EW1"] <- V_lwcd13C["lwc_ymax","EW1_d13C"]

# remove leaf.area for EW bc n = 2
fig2_meanseV[fig2_meanseV$variable == "leaf.area", "EW1"] <- NA

fig2_nequalsV <- final_df_nh %>% 
  filter(Spp=="V") %>% 
  select(Tmt, H2OTmt, totmass, rootshoot, Ht.mm..8, lwc, Anet, gs, WUE, tot_area, SRL, d13C, rootmass_g) %>% 
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

fig2_nequalsV <- fig2_nequalsV[-1,]
fig2_nequalsV[,3] <- rownames(fig2_nequalsV)
colnames(fig2_nequalsV) <- c("D","W", "variable")

fig2V_nh <- fig2_meanseV %>% 
#fig2V <- fig2_meanseV %>% 
  rename(Dry = ED1, Watered = EW1) %>% 
  pivot_longer(cols=c("Dry","Watered"), names_to="Treatment", values_to="value") %>% 
  select(variable, y, Treatment, value) %>% 
  # filter(variable != "d13C") %>% 
  # filter(variable != "lwc") %>% 
  pivot_wider(names_from = "y", values_from = "value") %>% 
  ggplot() +
  geom_abline(color= "red", linetype="dashed", slope = 0, intercept= 0) +
  geom_pointrange(aes(x=factor(variable, level=variable_order), y=y, ymin=ymin, ymax=ymax, group=Treatment, color=Treatment, shape=Treatment), size=1, linewidth=1, position=position_dodge(width=0.2)) + scale_color_manual(values=c("red","blue")) + scale_shape_manual(values = c(1,16)) +
  geom_text(data = fig2_nequalsV, aes(x = variable, y = -75, label = paste0("N = ",D)), color="red", size = 4) +
  geom_text(data = fig2_nequalsV, aes(x = variable, y = -95, label = paste0("N = ",W)), color="blue", size = 4) +
  ylim(-105, 217) +
  ggtitle("A. Quercus lobata (valley oak)") +
  ylab("% change with eCO2") + xlab("Plant Response") +
  theme_classic(base_size = 18) 

# compare filtered versions
grid.arrange(fig2V_nh, fig2L_nh, nrow=2)

# compare unfiltered versions
# grid.arrange(fig2V, fig2L, nrow=2) # not that interesting

