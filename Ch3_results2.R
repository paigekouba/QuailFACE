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
  dplyr::select(Plot, Spp, Code, totmass, rootshoot, rootmass_g, Ht.mm..8, lwc, CO2, meanSWC) %>% 
  left_join(LiCOR_df.[,c("Code","Anet","gs", "WUE")], by = "Code") %>% 
  left_join(lai.[,c("Code","avg_area","perim_per_A","tot_area", "SLA")], by = "Code") %>% 
  left_join(rootimage.[,c("Code","SRL")], by = "Code") %>% 
  left_join(SIF.[,c("Code","d13C")], by = "Code") %>% 
#  mutate(logAnet = log(Anet), sqrtgs = sqrt(gs), sqrtTotmass = sqrt(totmass), sqrtAvg_area = sqrt(avg_area), logPerimArea = log(perim_per_A), logSRL = log(SRL)) %>%
  group_by(Plot, Spp) %>% 
  mutate(n = n()) %>% 
  left_join(lookup, by = "Plot") %>% 
  mutate(H2OTmt = substr(Tmt,2,2), CO2Tmt = substr(Tmt,1,1)) %>% 
   ungroup()

# response variables
names(final_df)
# [1] "Plot"        "Spp"         "Code"        "totmass"     "rootshoot"   "rootmass_g"  "Ht.mm..8"    "lwc"        
# [9] "CO2"         "meanSWC"     "Anet"        "gs"          "WUE"         "avg_area"    "perim_per_A" "tot_area"   
# [17] "SLA"         "SRL"         "d13C"        "n"           "Tmt"         "H2OTmt"      "CO2Tmt"  

# want to compare full data with filtered(for herbivory)

final_df_nh <- final_df %>% 
  select(Code, totmass, rootshoot, Ht.mm..8) %>% 
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

# totmass -- sqrt
grid.arrange( 
ggpredict(lmer(sqrt(totmass)~rescale(CO2)+rescale(meanSWC) + (1|Plot), data=filter(final_df[final_df$Spp=="V",])), 
           terms=c("CO2","meanSWC [4,42]"))%>% plot(rawdata = TRUE, ci = TRUE, colors=c("red","blue")) 
+ labs(title = "Totmass, V (all)"),
ggpredict(lmer(sqrt(totmass)~rescale(CO2)+rescale(meanSWC) + (1|Plot), data=filter(final_df_nh[final_df_nh$Spp=="V",])), 
          terms=c("CO2","meanSWC [4,42]"))%>% plot(rawdata = TRUE, ci = TRUE, colors=c("red","blue")) 
+ labs(title = "Totmass, V (filtered)"),
 ggpredict(lmer(sqrt(totmass)~rescale(CO2)+rescale(meanSWC) + (1|Plot), data=filter(final_df[final_df$Spp=="L",])), 
           terms=c("CO2","meanSWC [4,42]"))%>% plot(rawdata = TRUE, ci = TRUE, colors=c("red","blue")) 
+ labs(title = "Totmass, L (all)"),
ggpredict(lmer(sqrt(totmass)~rescale(CO2)+rescale(meanSWC) + (1|Plot), data=filter(final_df_nh[final_df_nh$Spp=="L",])), 
          terms=c("CO2","meanSWC [4,42]"))%>% plot(rawdata = TRUE, ci = TRUE, colors=c("red","blue")) 
+ labs(title = "Totmass, L (filtered)"))
summary(lmer(sqrt(totmass)~rescale(CO2)+rescale(meanSWC) + (1|Plot), data=filter(final_df_nh[final_df_nh$Spp=="V",]))) 
 plot(lmer(sqrt(totmass)~rescale(CO2)+rescale(meanSWC) + (1|Plot), data=filter(final_df_nh[final_df_nh$Spp=="V",])))
qqmath(lmer(sqrt(totmass)~rescale(CO2)+rescale(meanSWC) + (1|Plot), data=filter(final_df_nh[final_df_nh$Spp=="V",])), id=.05)
shapiro.test(resid(lmer(sqrt(totmass)~rescale(CO2)+rescale(meanSWC) + (1|Plot), data=filter(final_df_nh[final_df_nh$Spp=="V",])))) 
# W = 0.97222, p-value = 0.7816

# summary(lmer(totmass~rescale(CO2)+rescale(meanSWC) + (1|Plot), data=filter(final_df_nh[final_df_nh$Spp=="V",])))
# plot(lmer(totmass~rescale(CO2)+rescale(meanSWC) + (1|Plot), data=filter(final_df_nh[final_df_nh$Spp=="V",])))
# qqmath(lmer(totmass~rescale(CO2)+rescale(meanSWC) + (1|Plot), data=filter(final_df_nh[final_df_nh$Spp=="V",])), id=.05)
# shapiro.test(resid(lmer(totmass~rescale(CO2)+rescale(meanSWC) + (1|Plot), data=filter(final_df_nh[final_df_nh$Spp=="V",])))) # OK
# # W = 0.97535, p-value = 0.8302
        
summary(lmer(sqrt(totmass)~rescale(CO2)+rescale(meanSWC) + (1|Plot), data=filter(final_df_nh[final_df_nh$Spp=="L",])))
plot(lmer(sqrt(totmass)~rescale(CO2)+rescale(meanSWC) + (1|Plot), data=filter(final_df_nh[final_df_nh$Spp=="L",])))
qqmath(lmer(sqrt(totmass)~rescale(CO2)+rescale(meanSWC) + (1|Plot), data=filter(final_df_nh[final_df_nh$Spp=="L",])), id=.05)
shapiro.test(resid(lmer(sqrt(totmass)~rescale(CO2)+rescale(meanSWC) + (1|Plot), data=filter(final_df_nh[final_df_nh$Spp=="L",]))))
# W = 0.98955, p-value = 0.8833

# summary(lmer(sqrt(totmass)~rescale(CO2)+rescale(meanSWC) + (1|Plot), data=filter(final_df[final_df$Spp=="L",])))

# rootshoot -- log
grid.arrange( 
  ggpredict(lmer(log(rootshoot)~rescale(CO2)+rescale(meanSWC) + (1|Plot), data=filter(final_df[final_df$Spp=="V",])), 
            terms=c("CO2","meanSWC [4,42]"))%>% plot(rawdata = TRUE, ci = TRUE, colors=c("red","blue")) 
  + labs(title = "log(rootshoot), V (all)"),
  ggpredict(lmer(log(rootshoot)~rescale(CO2)+rescale(meanSWC) + (1|Plot), data=filter(final_df_nh[final_df_nh$Spp=="V",])), 
            terms=c("CO2","meanSWC [4,42]"))%>% plot(rawdata = TRUE, ci = TRUE, colors=c("red","blue")) 
  + labs(title = "log(rootshoot), V (filtered)"),  
  ggpredict(lmer(log(rootshoot)~rescale(CO2)+rescale(meanSWC) + (1|Plot), data=filter(final_df[final_df$Spp=="L",])), 
            terms=c("CO2","meanSWC [4,42]"))%>% plot(rawdata = TRUE, ci = TRUE, colors=c("red","blue")) 
  + labs(title = "log(rootshoot), L (all)"),
  ggpredict(lmer(log(rootshoot)~rescale(CO2)+rescale(meanSWC) + (1|Plot), data=filter(final_df_nh[final_df_nh$Spp=="L",])), 
            terms=c("CO2","meanSWC [4,42]"))%>% plot(rawdata = TRUE, ci = TRUE, colors=c("red","blue")) 
  + labs(title = "log(rootshoot), L (filtered)"))

library(caret)
summary(lmer(log(rootshoot)~rescale(CO2)+rescale(meanSWC) + (1|Plot), data=filter(final_df_nh[final_df_nh$Spp=="V",])))
plot(lmer(log(rootshoot)~rescale(CO2)+rescale(meanSWC) + (1|Plot), data=filter(final_df_nh[final_df_nh$Spp=="V",])))
qqmath(lmer(log(rootshoot)~rescale(CO2)+rescale(meanSWC) + (1|Plot), data=filter(final_df_nh[final_df_nh$Spp=="V",])), id=.05)
shapiro.test(resid(lmer(log(rootshoot)~rescale(CO2)+rescale(meanSWC) + (1|Plot), data=filter(final_df_nh[final_df_nh$Spp=="V",]))))
# W = 0.97202, p-value = 0.7772

summary(lmer(log(rootshoot)~rescale(CO2)+rescale(meanSWC) + (1|Plot), data=filter(final_df_nh[final_df_nh$Spp=="L",])))
plot(lmer(log(rootshoot)~rescale(CO2)+rescale(meanSWC) + (1|Plot), data=filter(final_df_nh[final_df_nh$Spp=="L",])))
qqmath(lmer(log(rootshoot)~rescale(CO2)+rescale(meanSWC) + (1|Plot), data=filter(final_df_nh[final_df_nh$Spp=="L",])), id=.05)
shapiro.test(resid(lmer(log(rootshoot)~rescale(CO2)+rescale(meanSWC) + (1|Plot), data=filter(final_df_nh[final_df_nh$Spp=="L",]))))
# W = 0.96739, p-value = 0.1087
hist(log(final_df_nh[final_df_nh$Spp=="V",]$rootshoot))
hist(log(final_df_nh[final_df_nh$Spp=="L",]$rootshoot))

# lwc -- none
grid.arrange( 
  ggpredict(lmer(lwc~rescale(CO2)+rescale(meanSWC) + (1|Plot), data=filter(final_df[final_df$Spp=="V",])), 
            terms=c("CO2","meanSWC [4,42]"))%>% plot(rawdata = TRUE, ci = TRUE, colors=c("red","blue")) 
  + labs(title = "lwc, V (all)"),
  ggpredict(lmer(lwc~rescale(CO2)+rescale(meanSWC) + (1|Plot), data=filter(final_df_nh[final_df_nh$Spp=="V",])), 
            terms=c("CO2","meanSWC [4,42]"))%>% plot(rawdata = TRUE, ci = TRUE, colors=c("red","blue")) 
  + labs(title = "lwc, V (filtered)"),  
  ggpredict(lmer(lwc~rescale(CO2)+rescale(meanSWC) + (1|Plot), data=filter(final_df[final_df$Spp=="L",])), 
            terms=c("CO2","meanSWC [4,42]"))%>% plot(rawdata = TRUE, ci = TRUE, colors=c("red","blue")) 
  + labs(title = "lwc, L (all)"),
  ggpredict(lmer(lwc~rescale(CO2)+rescale(meanSWC) + (1|Plot), data=filter(final_df_nh[final_df_nh$Spp=="L",])), 
            terms=c("CO2","meanSWC [4,42]"))%>% plot(rawdata = TRUE, ci = TRUE, colors=c("red","blue")) 
  + labs(title = "lwc, L (filtered)"))
summary(lmer(lwc~rescale(CO2)+rescale(meanSWC) + (1|Plot), data=filter(final_df_nh[final_df_nh$Spp=="V",]))) 
plot(lmer(lwc~rescale(CO2)+rescale(meanSWC) + (1|Plot), data=filter(final_df_nh[final_df_nh$Spp=="V",])))
qqmath(lmer(lwc~rescale(CO2)+rescale(meanSWC) + (1|Plot), data=filter(final_df_nh[final_df_nh$Spp=="V",])), id=.05)
shapiro.test(resid(lmer(lwc~rescale(CO2)+rescale(meanSWC) + (1|Plot), data=filter(final_df_nh[final_df_nh$Spp=="V",]))))
# W = 0.98208, p-value = 0.6422
summary(lmer(lwc~rescale(CO2)+rescale(meanSWC) + (1|Plot), data=filter(final_df[final_df$Spp=="L",])))
plot(lmer(lwc~rescale(CO2)+rescale(meanSWC) + (1|Plot), data=filter(final_df_nh[final_df_nh$Spp=="L",])))
qqmath(lmer(lwc~rescale(CO2)+rescale(meanSWC) + (1|Plot), data=filter(final_df_nh[final_df_nh$Spp=="L",])), id=.05)
shapiro.test(resid(lmer(lwc~rescale(CO2)+rescale(meanSWC) + (1|Plot), data=filter(final_df_nh[final_df_nh$Spp=="L",]))))
# W = 0.99198, p-value = 0.954 (if filter 10L3)
plot(cooks.distance(lmer(lwc~rescale(CO2)+rescale(meanSWC) + (1|Plot), data=filter(final_df_nh[final_df_nh$Spp=="L",]))))
abline(h = 4/nobs(lmer(lwc~rescale(CO2)+rescale(meanSWC) + (1|Plot), data=filter(final_df_nh[final_df_nh$Spp=="L",]))), col="red") # agh!
hist(final_df_nh[final_df_nh$Spp=="V",]$lwc)
hist(final_df_nh[final_df_nh$Spp=="L",]$lwc)

# Anet -- none
grid.arrange( 
  ggpredict(lmer(Anet~rescale(CO2)+rescale(meanSWC) + (1|Plot), data=filter(final_df[final_df$Spp=="V",])), 
            terms=c("CO2","meanSWC [4,42]"))%>% plot(rawdata = TRUE, ci = TRUE, colors=c("red","blue")) 
  + labs(title = "Anet, V (all)"),
  ggpredict(lmer(Anet~rescale(CO2)*rescale(meanSWC) + (1|Plot), data=filter(final_df[final_df$Spp=="L",])), 
            terms=c("CO2","meanSWC [4,42]"))%>% plot(rawdata = TRUE, ci = TRUE, colors=c("red","blue")) 
  + labs(title = "Anet, L (all)"))
summary(lmer(Anet~rescale(CO2)+rescale(meanSWC) + (1|Plot), data=filter(final_df_nh[final_df_nh$Spp=="V",]))) 
plot(lmer(Anet~rescale(CO2)+rescale(meanSWC) + (1|Plot), data=filter(final_df_nh[final_df_nh$Spp=="V",])))
qqmath(lmer(Anet~rescale(CO2)+rescale(meanSWC) + (1|Plot), data=filter(final_df_nh[final_df_nh$Spp=="V",])), id=.05)
shapiro.test(resid(lmer(Anet~rescale(CO2)+rescale(meanSWC) + (1|Plot), data=filter(final_df_nh[final_df_nh$Spp=="V",]))))
# W = 0.95161, p-value = 0.1372
summary(lmer(Anet~rescale(CO2)*rescale(meanSWC) + (1|Plot), data=filter(final_df_nh[final_df_nh$Spp=="L",])))
plot(lmer(Anet~rescale(CO2)*rescale(meanSWC) + (1|Plot), data=filter(final_df_nh[final_df_nh$Spp=="L",])))
qqmath(lmer(Anet~rescale(CO2)*rescale(meanSWC) + (1|Plot), data=filter(final_df_nh[final_df_nh$Spp=="L",])), id=.05)
shapiro.test(resid(lmer(Anet~rescale(CO2)*rescale(meanSWC) + (1|Plot), data=filter(final_df_nh[final_df_nh$Spp=="L",]))))
# W = 0.97784, p-value = 0.5793

ggpredict(lmer(Anet~rescale(CO2)*rescale(meanSWC) + (1|Plot), data=final_df), 
          terms=c("CO2","meanSWC [4,42]"))%>% plot(rawdata = TRUE, ci = TRUE, colors=c("red","blue")) 
+ labs(title = "Anet, both")
# summary(lmer(sqrt(Anet)~rescale(CO2)*rescale(meanSWC) + (1|Plot), data=final_df))
# summary(lmer(sqrt(Anet)~CO2*H2OTmt + (1|Plot), data=final_df))
        

# gs -- none
grid.arrange( 
  ggpredict(lmer(gs~rescale(CO2)+rescale(meanSWC) + (1|Plot), data=filter(final_df[final_df$Spp=="V",])), 
            terms=c("CO2","meanSWC [4,42]"))%>% plot(rawdata = TRUE, ci = TRUE, colors=c("red","blue")) 
  + labs(title = "gs, V (all)"),
  ggpredict(lmer(gs~rescale(CO2)+rescale(meanSWC) + (1|Plot), data=filter(final_df_nh[final_df_nh$Spp=="V",])), 
            terms=c("CO2","meanSWC [4,42]"))%>% plot(rawdata = TRUE, ci = TRUE, colors=c("red","blue")) 
  + labs(title = "gs, V (filtered)"),  
  ggpredict(lmer(gs~rescale(CO2)+rescale(meanSWC) + (1|Plot), data=filter(final_df[final_df$Spp=="L",])), 
            terms=c("CO2","meanSWC [4,42]"))%>% plot(rawdata = TRUE, ci = TRUE, colors=c("red","blue")) 
  + labs(title = "gs, L (all)"),
  ggpredict(lmer(gs~rescale(CO2)+rescale(meanSWC) + (1|Plot), data=filter(final_df_nh[final_df_nh$Spp=="L",])), 
            terms=c("CO2","meanSWC [4,42]"))%>% plot(rawdata = TRUE, ci = TRUE, colors=c("red","blue")) 
  + labs(title = "gs, L (filtered)"))
summary(lmer(gs~rescale(CO2)+rescale(meanSWC) + (1|Plot), data=filter(final_df_nh[final_df_nh$Spp=="V",]))) 
plot(lmer(gs~rescale(CO2)+rescale(meanSWC) + (1|Plot), data=filter(final_df_nh[final_df_nh$Spp=="V",])))
qqmath(lmer(gs~rescale(CO2)+rescale(meanSWC) + (1|Plot), data=filter(final_df_nh[final_df_nh$Spp=="V",])), id=.05)
shapiro.test(resid(lmer(gs~rescale(CO2)+rescale(meanSWC) + (1|Plot), data=filter(final_df_nh[final_df_nh$Spp=="V",]))))
# W = 0.97325, p-value = 0.5566
summary(lmer(gs~rescale(CO2)+rescale(meanSWC) + (1|Plot), data=filter(final_df_nh[final_df_nh$Spp=="L",])))
plot(lmer(gs~rescale(CO2)+rescale(meanSWC) + (1|Plot), data=filter(final_df_nh[final_df_nh$Spp=="L",])))
qqmath(lmer(gs~rescale(CO2)+rescale(meanSWC) + (1|Plot), data=filter(final_df_nh[final_df_nh$Spp=="L",])), id=.05)
shapiro.test(resid(lmer(gs~rescale(CO2)+rescale(meanSWC) + (1|Plot), data=filter(final_df_nh[final_df_nh$Spp=="L",]))))
# W = 0.95598, p-value = 0.1059

# ggpredict(lmer(gs~rescale(CO2)+rescale(meanSWC) + (1|Plot), data=final_df), 
#           terms=c("CO2","meanSWC [4,42]"))%>% plot(rawdata = TRUE, ci = TRUE, colors=c("red","blue")) 
# + labs(title = "gs, both")
# summary(lmer(sqrt(gs)~rescale(CO2)+rescale(meanSWC) + (1|Plot), data=final_df))
# summary(lmer(sqrt(gs)~CO2+H2OTmt + (1|Plot), data=final_df))

# WUE -- none
grid.arrange( 
  ggpredict(lmer(WUE~rescale(CO2)+rescale(meanSWC) + (1|Plot), data=filter(final_df[final_df$Spp=="V",])), 
            terms=c("CO2","meanSWC [4,42]"))%>% plot(rawdata = TRUE, ci = TRUE, colors=c("red","blue")) 
  + labs(title = "WUE, V (all)"),
  ggpredict(lmer(WUE~rescale(CO2)+rescale(meanSWC) + (1|Plot), data=filter(final_df_nh[final_df_nh$Spp=="V",])), 
            terms=c("CO2","meanSWC [4,42]"))%>% plot(rawdata = TRUE, ci = TRUE, colors=c("red","blue")) 
  + labs(title = "WUE, V (filtered)"),  
  ggpredict(lmer(WUE~rescale(CO2)+rescale(meanSWC) + (1|Plot), data=filter(final_df[final_df$Spp=="L",])), 
            terms=c("CO2","meanSWC [4,42]"))%>% plot(rawdata = TRUE, ci = TRUE, colors=c("red","blue")) 
  + labs(title = "WUE, L (all)"),
  ggpredict(lmer(WUE~rescale(CO2)+rescale(meanSWC) + (1|Plot), data=filter(final_df_nh[final_df_nh$Spp=="L",])), 
            terms=c("CO2","meanSWC [4,42]"))%>% plot(rawdata = TRUE, ci = TRUE, colors=c("red","blue")) 
  + labs(title = "WUE, L (filtered)"))
summary(lmer(WUE~rescale(CO2)+rescale(meanSWC) + (1|Plot), data=filter(final_df_nh[final_df_nh$Spp=="V",]))) 
plot(lmer(WUE~rescale(CO2)+rescale(meanSWC) + (1|Plot), data=filter(final_df_nh[final_df_nh$Spp=="V",])))
qqmath(lmer(WUE~rescale(CO2)+rescale(meanSWC) + (1|Plot), data=filter(final_df_nh[final_df_nh$Spp=="V",])), id=.05)
shapiro.test(resid(lmer(WUE~rescale(CO2)+rescale(meanSWC) + (1|Plot), data=filter(final_df_nh[final_df_nh$Spp=="V",]))))
# W = 0.95747, p-value = 0.2049
summary(lmer(WUE~rescale(CO2)+rescale(meanSWC) + (1|Plot), data=filter(final_df_nh[final_df_nh$Spp=="L",])))
plot(lmer(WUE~rescale(CO2)+rescale(meanSWC) + (1|Plot), data=filter(final_df_nh[final_df_nh$Spp=="L",])))
qqmath(lmer(WUE~rescale(CO2)+rescale(meanSWC) + (1|Plot), data=filter(final_df_nh[final_df_nh$Spp=="L",])), id=.05)
shapiro.test(resid(lmer(WUE~rescale(CO2)+rescale(meanSWC) + (1|Plot), data=filter(final_df_nh[final_df_nh$Spp=="L",]))))
# W = 0.97766, p-value = 0.5726


# tot_area -- sqrt
grid.arrange( 
  ggpredict(lmer(sqrt(tot_area)~rescale(CO2)+rescale(meanSWC) + (1|Plot), data=filter(final_df[final_df$Spp=="V",])), 
            terms=c("CO2","meanSWC [4,42]"))%>% plot(rawdata = TRUE, ci = TRUE, colors=c("red","blue")) 
  + labs(title = "sqrt(tot_area), V (all)"),
  ggpredict(lmer(sqrt(tot_area)~rescale(CO2)+rescale(meanSWC) + (1|Plot), data=filter(final_df_nh[final_df_nh$Spp=="V",])), 
            terms=c("CO2","meanSWC [4,42]"))%>% plot(rawdata = TRUE, ci = TRUE, colors=c("red","blue")) 
  + labs(title = "sqrt(tot_area), V (filtered)"),  
  ggpredict(lmer(sqrt(tot_area)~rescale(CO2)+rescale(meanSWC) + (1|Plot), data=filter(final_df[final_df$Spp=="L",])), 
            terms=c("CO2","meanSWC [4,42]"))%>% plot(rawdata = TRUE, ci = TRUE, colors=c("red","blue")) 
  + labs(title = "sqrt(tot_area), L (all)"),
  ggpredict(lmer(sqrt(tot_area)~rescale(CO2)+rescale(meanSWC) + (1|Plot), data=filter(final_df_nh[final_df_nh$Spp=="L",])), 
            terms=c("CO2","meanSWC [4,42]"))%>% plot(rawdata = TRUE, ci = TRUE, colors=c("red","blue")) 
  + labs(title = "sqrt(tot_area), L (filtered)"))
summary(lmer(sqrt(tot_area)~rescale(CO2)+rescale(meanSWC) + (1|Plot), data=filter(final_df_nh[final_df_nh$Spp=="V",]))) 
plot(lmer(sqrt(tot_area)~rescale(CO2)+rescale(meanSWC) + (1|Plot), data=filter(final_df_nh[final_df_nh$Spp=="V",])))
qqmath(lmer(sqrt(tot_area)~rescale(CO2)+rescale(meanSWC) + (1|Plot), data=filter(final_df_nh[final_df_nh$Spp=="V",])), id=.05)
shapiro.test(resid(lmer(sqrt(tot_area)~rescale(CO2)+rescale(meanSWC) + (1|Plot), data=filter(final_df_nh[final_df_nh$Spp=="V",]))))
# W = 0.97489, p-value = 0.883
summary(lmer(sqrt(tot_area)~rescale(CO2)+rescale(meanSWC) + (1|Plot), data=filter(final_df_nh[final_df_nh$Spp=="L",])))
plot(lmer(sqrt(tot_area)~rescale(CO2)+rescale(meanSWC) + (1|Plot), data=filter(final_df_nh[final_df_nh$Spp=="L",])))
qqmath(lmer(sqrt(tot_area)~rescale(CO2)+rescale(meanSWC) + (1|Plot), data=filter(final_df_nh[final_df_nh$Spp=="L",])), id=.05)
shapiro.test(resid(lmer(sqrt(tot_area)~rescale(CO2)+rescale(meanSWC) + (1|Plot), data=filter(final_df_nh[final_df_nh$Spp=="L",]))))
# W = 0.97725, p-value = 0.3563
hist(sqrt(final_df_nh[final_df_nh$Spp=="V",]$tot_area))
hist(sqrt(final_df_nh[final_df_nh$Spp=="L",]$tot_area))


# SRL -- log
grid.arrange( 
  ggpredict(lmer(log(SRL)~rescale(CO2)+rescale(meanSWC) + (1|Plot), data=filter(final_df[final_df$Spp=="V",])), 
            terms=c("CO2","meanSWC [4,42]"))%>% plot(rawdata = TRUE, ci = TRUE, colors=c("red","blue")) 
  + labs(title = "log(SRL), V (all)"),
  ggpredict(lmer(log(SRL)~rescale(CO2)+rescale(meanSWC) + (1|Plot), data=filter(final_df_nh[final_df_nh$Spp=="V",])), 
            terms=c("CO2","meanSWC [4,42]"))%>% plot(rawdata = TRUE, ci = TRUE, colors=c("red","blue")) 
  + labs(title = "log(SRL), V (filtered)"),  
  ggpredict(lmer(log(SRL)~rescale(CO2)*rescale(meanSWC) + (1|Plot), data=filter(final_df[final_df$Spp=="L",])), 
            terms=c("CO2","meanSWC [4,42]"))%>% plot(rawdata = TRUE, ci = TRUE, colors=c("red","blue")) 
  + labs(title = "log(SRL), L (all)"),
  ggpredict(lmer(log(SRL)~rescale(CO2)*rescale(meanSWC) + (1|Plot), data=filter(final_df_nh[final_df_nh$Spp=="L",])), 
            terms=c("CO2","meanSWC [4,42]"))%>% plot(rawdata = TRUE, ci = TRUE, colors=c("red","blue")) 
  + labs(title = "log(SRL), L (filtered)"))
summary(lmer(log(SRL)~rescale(CO2)+rescale(meanSWC) + (1|Plot), data=filter(final_df_nh[final_df_nh$Spp=="V",]))) 
plot(lmer(log(SRL)~rescale(CO2)+rescale(meanSWC) + (1|Plot), data=filter(final_df_nh[final_df_nh$Spp=="V",])))
qqmath(lmer(log(SRL)~rescale(CO2)+rescale(meanSWC) + (1|Plot), data=filter(final_df_nh[final_df_nh$Spp=="V",])), id=.05)
shapiro.test(resid(lmer(log(SRL)~rescale(CO2)+rescale(meanSWC) + (1|Plot), data=filter(final_df_nh[final_df_nh$Spp=="V",]))))
# W = 0.99179, p-value = 0.9566
summary(lmer(log(SRL)~rescale(CO2)*rescale(meanSWC) + (1|Plot), data=filter(final_df_nh[final_df_nh$Spp=="L",])))
plot(lmer(log(SRL)~rescale(CO2)*rescale(meanSWC) + (1|Plot), data=filter(final_df_nh[final_df_nh$Spp=="L",])))
qqmath(lmer(log(SRL)~rescale(CO2)*rescale(meanSWC) + (1|Plot), data=filter(final_df_nh[final_df_nh$Spp=="L",])), id=.05)
shapiro.test(resid(lmer(log(SRL)~rescale(CO2)*rescale(meanSWC) + (1|Plot), data=filter(final_df_nh[final_df_nh$Spp=="L",]))))
# W = 0.98945, p-value = 0.8508

# d13 -- none
grid.arrange( 
  ggpredict(lmer(d13C~rescale(CO2)+rescale(meanSWC) + (1|Plot), data=filter(final_df[final_df$Spp=="V",])), 
            terms=c("CO2","meanSWC [4,42]"))%>% plot(rawdata = TRUE, ci = TRUE, colors=c("red","blue")) 
  + labs(title = "d13C, V (all)"),
  ggpredict(lmer(d13C~rescale(CO2)+rescale(meanSWC) + (1|Plot), data=filter(final_df_nh[final_df_nh$Spp=="V",])), 
            terms=c("CO2","meanSWC [4,42]"))%>% plot(rawdata = TRUE, ci = TRUE, colors=c("red","blue")) 
  + labs(title = "d13C, V (filtered)"),  
  ggpredict(lmer(d13C~rescale(CO2)+rescale(meanSWC) + (1|Plot), data=filter(final_df[final_df$Spp=="L",])), 
            terms=c("CO2","meanSWC [4,42]"))%>% plot(rawdata = TRUE, ci = TRUE, colors=c("red","blue")) 
  + labs(title = "d13C, L (all)"),
  ggpredict(lmer(d13C~rescale(CO2)+rescale(meanSWC) + (1|Plot), data=filter(final_df_nh[final_df_nh$Spp=="L",])), 
            terms=c("CO2","meanSWC [4,42]"))%>% plot(rawdata = TRUE, ci = TRUE, colors=c("red","blue")) 
  + labs(title = "d13C, L (filtered)"))
summary(lmer(d13C~rescale(CO2)+rescale(meanSWC) + (1|Plot), data=filter(final_df_nh[final_df_nh$Spp=="V",]))) 
plot(lmer(d13C~rescale(CO2)+rescale(meanSWC) + (1|Plot), data=filter(final_df_nh[final_df_nh$Spp=="V",])))
qqmath(lmer(d13C~rescale(CO2)+rescale(meanSWC) + (1|Plot), data=filter(final_df_nh[final_df_nh$Spp=="V",])), id=.05)
shapiro.test(resid(lmer(d13C~rescale(CO2)+rescale(meanSWC) + (1|Plot), data=filter(final_df_nh[final_df_nh$Spp=="V",]))))
# W = 0.97802, p-value = 0.5562
summary(lmer(d13C~rescale(CO2)+rescale(meanSWC) + (1|Plot), data=filter(final_df_nh[final_df_nh$Spp=="L",])))
plot(lmer(d13C~rescale(CO2)+rescale(meanSWC) + (1|Plot), data=filter(final_df_nh[final_df_nh$Spp=="L",])))
qqmath(lmer(d13C~rescale(CO2)+rescale(meanSWC) + (1|Plot), data=filter(final_df_nh[final_df_nh$Spp=="L",])), id=.05)
shapiro.test(resid(lmer(d13C~rescale(CO2)+rescale(meanSWC) + (1|Plot), data=filter(final_df_nh[final_df_nh$Spp=="L",]))))
# W = 0.96813, p-value = 0.214

# Ht.mm..8 -- sqrt
grid.arrange( 
  ggpredict(lmer(sqrt(Ht.mm..8)~rescale(CO2)+rescale(meanSWC) + (1|Plot), data=filter(final_df[final_df$Spp=="V",])), 
            terms=c("CO2","meanSWC [4,42]"))%>% plot(rawdata = TRUE, ci = TRUE, colors=c("red","blue")) 
  + labs(title = "sqrt(Ht.mm..8), V (all)"),
  ggpredict(lmer(sqrt(Ht.mm..8)~rescale(CO2)+rescale(meanSWC) + (1|Plot), data=filter(final_df_nh[final_df_nh$Spp=="V",])), 
            terms=c("CO2","meanSWC [4,42]"))%>% plot(rawdata = TRUE, ci = TRUE, colors=c("red","blue")) 
  + labs(title = "sqrt(Ht.mm..8), V (filtered)"),  
  ggpredict(lmer(sqrt(Ht.mm..8)~rescale(CO2)+rescale(meanSWC) + (1|Plot), data=filter(final_df[final_df$Spp=="L",])), 
            terms=c("CO2","meanSWC [4,42]"))%>% plot(rawdata = TRUE, ci = TRUE, colors=c("red","blue")) 
  + labs(title = "sqrt(Ht.mm..8), L (all)"),
  ggpredict(lmer(sqrt(Ht.mm..8)~rescale(CO2)+rescale(meanSWC) + (1|Plot), data=filter(final_df_nh[final_df_nh$Spp=="L",])), 
            terms=c("CO2","meanSWC [4,42]"))%>% plot(rawdata = TRUE, ci = TRUE, colors=c("red","blue")) 
  + labs(title = "sqrt(Ht.mm..8), L (filtered)"))
summary(lmer(sqrt(Ht.mm..8)~rescale(CO2)+rescale(meanSWC) + (1|Plot), data=filter(final_df_nh[final_df_nh$Spp=="V",]))) 
plot(lmer(sqrt(Ht.mm..8)~rescale(CO2)+rescale(meanSWC) + (1|Plot), data=filter(final_df_nh[final_df_nh$Spp=="V",])))
qqmath(lmer(sqrt(Ht.mm..8)~rescale(CO2)+rescale(meanSWC) + (1|Plot), data=filter(final_df_nh[final_df_nh$Spp=="V",])), id=.05)
shapiro.test(resid(lmer(sqrt(Ht.mm..8)~rescale(CO2)+rescale(meanSWC) + (1|Plot), data=filter(final_df_nh[final_df_nh$Spp=="V",]))))
# W = 0.96285, p-value = 0.5489
summary(lmer(sqrt(Ht.mm..8)~rescale(CO2)+rescale(meanSWC) + (1|Plot), data=filter(final_df_nh[final_df_nh$Spp=="L",])))
plot(lmer(sqrt(Ht.mm..8)~rescale(CO2)+rescale(meanSWC) + (1|Plot), data=filter(final_df_nh[final_df_nh$Spp=="L",])))
qqmath(lmer(sqrt(Ht.mm..8)~rescale(CO2)+rescale(meanSWC) + (1|Plot), data=filter(final_df_nh[final_df_nh$Spp=="L",])), id=.05)
shapiro.test(resid(lmer(sqrt(Ht.mm..8)~rescale(CO2)+rescale(meanSWC) + (1|Plot), data=filter(final_df_nh[final_df_nh$Spp=="L",]))))
# W = 0.98934, p-value = 0.8801

# rootmass_g -- sqrt
grid.arrange( 
  ggpredict(lmer(sqrt(rootmass_g)~rescale(CO2)+rescale(meanSWC) + (1|Plot), data=filter(final_df[final_df$Spp=="V",])), 
            terms=c("CO2","meanSWC [4,42]"))%>% plot(rawdata = TRUE, ci = TRUE, colors=c("red","blue")) 
  + labs(title = "sqrt(rootmass_g), V (all)"),
  ggpredict(lmer(sqrt(rootmass_g)~rescale(CO2)+rescale(meanSWC) + (1|Plot), data=filter(final_df_nh[final_df_nh$Spp=="V",])), 
            terms=c("CO2","meanSWC [4,42]"))%>% plot(rawdata = TRUE, ci = TRUE, colors=c("red","blue")) 
  + labs(title = "sqrt(rootmass_g), V (filtered)"),  
  ggpredict(lmer(sqrt(rootmass_g)~rescale(CO2)+rescale(meanSWC) + (1|Plot), data=filter(final_df[final_df$Spp=="L",])), 
            terms=c("CO2","meanSWC [4,42]"))%>% plot(rawdata = TRUE, ci = TRUE, colors=c("red","blue")) 
  + labs(title = "sqrt(rootmass_g), L (all)"),
  ggpredict(lmer(sqrt(rootmass_g)~rescale(CO2)+rescale(meanSWC) + (1|Plot), data=filter(final_df_nh[final_df_nh$Spp=="L",])), 
            terms=c("CO2","meanSWC [4,42]"))%>% plot(rawdata = TRUE, ci = TRUE, colors=c("red","blue")) 
  + labs(title = "sqrt(rootmass_g), L (filtered)"))
summary(lmer(sqrt(rootmass_g)~rescale(CO2)+rescale(meanSWC) + (1|Plot), data=filter(final_df_nh[final_df_nh$Spp=="V",]))) 
plot(lmer(sqrt(rootmass_g)~rescale(CO2)+rescale(meanSWC) + (1|Plot), data=filter(final_df_nh[final_df_nh$Spp=="V",])))
qqmath(lmer(sqrt(rootmass_g)~rescale(CO2)+rescale(meanSWC) + (1|Plot), data=filter(final_df_nh[final_df_nh$Spp=="V",])), id=.05)
shapiro.test(resid(lmer(sqrt(rootmass_g)~rescale(CO2)+rescale(meanSWC) + (1|Plot), data=filter(final_df_nh[final_df_nh$Spp=="V",]))))
# W = 0.98112, p-value = 0.4664
summary(lmer(sqrt(rootmass_g)~rescale(CO2)+rescale(meanSWC) + (1|Plot), data=filter(final_df_nh[final_df_nh$Spp=="L",])))
plot(lmer(sqrt(rootmass_g)~rescale(CO2)+rescale(meanSWC) + (1|Plot), data=filter(final_df_nh[final_df_nh$Spp=="L",])))
qqmath(lmer(sqrt(rootmass_g)~rescale(CO2)+rescale(meanSWC) + (1|Plot), data=filter(final_df_nh[final_df_nh$Spp=="L",])), id=.05)
shapiro.test(resid(lmer(sqrt(rootmass_g)~rescale(CO2)+rescale(meanSWC) + (1|Plot), data=filter(final_df_nh[final_df_nh$Spp=="L",]))))
# W = 0.98765, p-value = 0.7575

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
  filter(Spp=="L") %>% 
  select(Tmt, totmass, rootshoot, Ht.mm..8, lwc, Anet, gs, WUE, tot_area, SRL, d13C, rootmass_g) %>% 
  rename(totarea = tot_area) %>% 
  rename(rootmass = rootmass_g) %>% 
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

  #  mutate_if(is.numeric, list(pct_chg = (AD - .x)*100/AD)) %>% View()
# get Tmt to be its own column with pivot_longer

fig2L_nh <- fig2_meanse %>% 
  pivot_longer(cols=c("ED1","EW1"), names_to="Tmt", values_to="value") %>% 
  select(variable, y, Tmt, value) %>% 
  pivot_wider(names_from = "y", values_from = "value") %>% 
ggplot() +
  geom_pointrange(aes(x=variable, y=y, ymin=ymin, ymax=ymax, group=Tmt, color=Tmt), size=1.2, linewidth=1.2, position=position_dodge(width=0.2)) + scale_color_manual(values=c("red","blue")) +
  ggtitle("Quercus wislizenii (live oak)") +
  ylab("% change with eCO2") + xlab("Plant Response") +
  theme_classic(base_size = 20) + geom_abline(color= "red", linetype="dashed", slope = 0, intercept= 0)
  

# now for V!
fig2_meanseV <- final_df_nh %>% 
  filter(Spp=="V") %>% 
  select(Tmt, totmass, rootshoot, Ht.mm..8, lwc, Anet, gs, WUE, tot_area, SRL, d13C, rootmass_g) %>% 
  rename(totarea = tot_area) %>% 
  rename(rootmass = rootmass_g) %>% 
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

#  mutate_if(is.numeric, list(pct_chg = (AD - .x)*100/AD)) %>% View()
# get Tmt to be its own column with pivot_longer

fig2V_nh <- fig2_meanseV %>% 
  pivot_longer(cols=c("ED1","EW1"), names_to="Tmt", values_to="value") %>% 
  select(variable, y, Tmt, value) %>% 
  pivot_wider(names_from = "y", values_from = "value") %>% 
  ggplot() +
  geom_pointrange(aes(x=variable, y=y, ymin=ymin, ymax=ymax, group=Tmt, color=Tmt), size=1.2, linewidth=1.2, position=position_dodge(width=0.2)) + scale_color_manual(values=c("red","blue")) +
  ggtitle("Quercus lobata (valley oak)") +
  ylab("% change with eCO2") + xlab("Plant Response") +
  theme_classic(base_size = 20) + geom_abline(color= "red", linetype="dashed", slope = 0, intercept= 0)

# compare filtered versions

grid.arrange(fig2V_nh, fig2L_nh, nrow=2)
