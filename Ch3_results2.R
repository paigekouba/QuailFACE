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
  dplyr::select(Plot, Spp, Code, totmass, rootshoot, rootmass_g, Ht.mm..5, Ht.mm..8, lwc, CO2, meanSWC) %>% 
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

# totmass
grid.arrange(
ggpredict(lm(totmass~rescale(CO2)*rescale(meanSWC), data=plotmeans.V), 
     terms=c("CO2","meanSWC [4,42]"))%>% plot(rawdata=T,ci=T,colors=c("red","blue")) + labs(title="totmass, V *"),
ggpredict(lm(totmass~rescale(CO2)+rescale(meanSWC), data=plotmeans.V), 
     terms=c("CO2","meanSWC [4,42]"))%>% plot(rawdata=T,ci=T,colors=c("red","blue")) + labs(title="totmass, V +"),
ggpredict(lm(totmass~rescale(CO2)*rescale(meanSWC), data=plotmeans.V2), 
     terms=c("CO2","meanSWC [4,42]"))%>% plot(rawdata=T,ci=T,colors=c("red","blue")) + labs(title="totmass, V *, unfiltered"),
ggpredict(lm(totmass~rescale(CO2)+rescale(meanSWC), data=plotmeans.V2), 
     terms=c("CO2","meanSWC [4,42]"))%>% plot(rawdata=T,ci=T,colors=c("red","blue")) + labs(title="totmass, V +, unfiltered"),
ggpredict(lm(totmass~rescale(CO2)*rescale(meanSWC), data=plotmeans.L), 
     terms=c("CO2","meanSWC [4,42]"))%>% plot(rawdata=T,ci=T,colors=c("red","blue")) + labs(title="totmass, L *"),
ggpredict(lm(totmass~rescale(CO2)+rescale(meanSWC), data=plotmeans.L), 
     terms=c("CO2","meanSWC [4,42]"))%>% plot(rawdata=T,ci=T,colors=c("red","blue")) + labs(title="totmass, L +"),
ggpredict(lm(totmass~rescale(CO2)*rescale(meanSWC), data=plotmeans.L2), 
    terms=c("CO2","meanSWC [4,42]"))%>% plot(rawdata=T,ci=T,colors=c("red","blue")) + labs(title="totmass, L *, unfiltered"),
ggpredict(lm(totmass~rescale(CO2)+rescale(meanSWC), data=plotmeans.L2), 
    terms=c("CO2","meanSWC [4,42]"))%>% plot(rawdata=T,ci=T,colors=c("red","blue")) + labs(title="totmass, L +, unfiltered"), nrow=2)
summary(lm(totmass~rescale(CO2)+rescale(meanSWC), data=plotmeans.V)) 
plot(lm(totmass~rescale(CO2)+rescale(meanSWC), data=plotmeans.V))
qqPlot(lm(totmass~rescale(CO2)+rescale(meanSWC), data=plotmeans.V))
shapiro.test(resid(lm(totmass~rescale(CO2)+rescale(meanSWC), data=plotmeans.V))) # W = 0.90013, p-value = 0.2198

summary(lm(totmass~rescale(CO2)+rescale(meanSWC), data=plotmeans.L)) 
plot(lm(totmass~rescale(CO2)+rescale(meanSWC), data=plotmeans.L))
qqPlot(lm(totmass~rescale(CO2)+rescale(meanSWC), data=plotmeans.L))
shapiro.test(resid(lm(totmass~rescale(CO2)+rescale(meanSWC), data=plotmeans.L))) # W = 0.93925, p-value = 0.34

# rootshoot
grid.arrange(
  ggpredict(lm(rootshoot~rescale(CO2)*rescale(meanSWC), data=plotmeans.V), 
            terms=c("CO2","meanSWC [4,42]"))%>% plot(rawdata=T,ci=T,colors=c("red","blue")) + labs(title="rootshoot, V *"),
  ggpredict(lm(rootshoot~rescale(CO2)+rescale(meanSWC), data=plotmeans.V), 
            terms=c("CO2","meanSWC [4,42]"))%>% plot(rawdata=T,ci=T,colors=c("red","blue")) + labs(title="rootshoot, V +"),
  ggpredict(lm(rootshoot~rescale(CO2)*rescale(meanSWC), data=plotmeans.V2), 
            terms=c("CO2","meanSWC [4,42]"))%>% plot(rawdata=T,ci=T,colors=c("red","blue")) + labs(title="rootshoot, V *, unfiltered"),
  ggpredict(lm(rootshoot~rescale(CO2)+rescale(meanSWC), data=plotmeans.V2), 
            terms=c("CO2","meanSWC [4,42]"))%>% plot(rawdata=T,ci=T,colors=c("red","blue")) + labs(title="rootshoot, V +, unfiltered"),
  ggpredict(lm(rootshoot~rescale(CO2)*rescale(meanSWC), data=plotmeans.L), 
            terms=c("CO2","meanSWC [4,42]"))%>% plot(rawdata=T,ci=T,colors=c("red","blue")) + labs(title="rootshoot, L *"),
  ggpredict(lm(rootshoot~rescale(CO2)+rescale(meanSWC), data=plotmeans.L), 
            terms=c("CO2","meanSWC [4,42]"))%>% plot(rawdata=T,ci=T,colors=c("red","blue")) + labs(title="rootshoot, L +"),
  ggpredict(lm(rootshoot~rescale(CO2)*rescale(meanSWC), data=plotmeans.L2), 
            terms=c("CO2","meanSWC [4,42]"))%>% plot(rawdata=T,ci=T,colors=c("red","blue")) + labs(title="rootshoot, L *, unfiltered"),
  ggpredict(lm(rootshoot~rescale(CO2)+rescale(meanSWC), data=plotmeans.L2), 
            terms=c("CO2","meanSWC [4,42]"))%>% plot(rawdata=T,ci=T,colors=c("red","blue")) + labs(title="rootshoot, L +, unfiltered"), nrow=2)
summary(lm(rootshoot~rescale(CO2)+rescale(meanSWC), data=plotmeans.V)) 
plot(lm(rootshoot~rescale(CO2)+rescale(meanSWC), data=plotmeans.V))
qqPlot(lm(rootshoot~rescale(CO2)+rescale(meanSWC), data=plotmeans.V))
shapiro.test(resid(lm(rootshoot~rescale(CO2)+rescale(meanSWC), data=plotmeans.V))) # W = 0.91734, p-value = 0.3353

summary(lm(rootshoot~rescale(CO2)+rescale(meanSWC), data=plotmeans.L)) 
plot(lm(rootshoot~rescale(CO2)+rescale(meanSWC), data=plotmeans.L))
qqPlot(lm(rootshoot~rescale(CO2)+rescale(meanSWC), data=plotmeans.L))
shapiro.test(resid(lm(rootshoot~rescale(CO2)+rescale(meanSWC), data=plotmeans.L))) # W = 0.93686, p-value = 0.3123


# lwc
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
plot(lm(lwc~rescale(CO2)+rescale(meanSWC), data=plotmeans.V))
qqPlot(lm(lwc~rescale(CO2)+rescale(meanSWC), data=plotmeans.V))
shapiro.test(resid(lm(lwc~rescale(CO2)+rescale(meanSWC), data=plotmeans.V))) # W = 0.89815, p-value = 0.106

summary(lm(lwc~rescale(CO2)+rescale(meanSWC), data=plotmeans.L)) 
plot(lm(lwc~rescale(CO2)+rescale(meanSWC), data=plotmeans.L))
qqPlot(lm(lwc~rescale(CO2)+rescale(meanSWC), data=plotmeans.L))
shapiro.test(resid(lm(lwc~rescale(CO2)+rescale(meanSWC), data=plotmeans.L))) # W = 0.97913, p-value = 0.9563

# Anet
grid.arrange(
  ggpredict(lmer(Anet~rescale(CO2)*rescale(meanSWC)+time_scaled + (1|Plot), data=filter(final_df,Spp=="V" & Code!="7V1")), 
            terms=c("CO2","meanSWC [4,42]"))%>% plot(rawdata=T,ci=T,colors=c("red","blue")) + labs(title="Anet, V *"),
  ggpredict(lmer(Anet~rescale(CO2)+rescale(meanSWC)+time_scaled + (1|Plot), data=filter(final_df,Spp=="V" & Code!="7V1")), 
            terms=c("CO2","meanSWC [4,42]"))%>% plot(rawdata=T,ci=T,colors=c("red","blue")) + labs(title="Anet, V +"),
    ggpredict(lm(Anet~rescale(CO2)*rescale(meanSWC), data=plotmeans.L),
              terms=c("CO2","meanSWC [4,42]"))%>% plot(rawdata=T,ci=T,colors=c("red","blue")) + labs(title="gs, L *"),
    ggpredict(lm(Anet~rescale(CO2)+rescale(meanSWC), data=plotmeans.L),
              terms=c("CO2","meanSWC [4,42]"))%>% plot(rawdata=T,ci=T,colors=c("red","blue")) + labs(title="gs, L +") )
summary(lm(Anet~rescale(CO2)+rescale(meanSWC), data=plotmeans.V)) 
summary(lm(Anet~rescale(CO2)+rescale(meanSWC)+time_scaled, data=plotmeans.V)) 
summary(lmer(Anet~rescale(CO2)+rescale(meanSWC)+time_scaled + (1|Plot), data=filter(final_df,Spp=="V" & Code!="7V1")))
plot(lmer(Anet~rescale(CO2)+rescale(meanSWC)+time_scaled + (1|Plot), data=filter(final_df,Spp=="V" & Code!="7V1")))
qqmath(lmer(Anet~rescale(CO2)+rescale(meanSWC)+time_scaled + (1|Plot), data=filter(final_df,Spp=="V" & Code!="7V1")))
shapiro.test(resid(lmer(Anet~rescale(CO2)+rescale(meanSWC)+time_scaled + (1|Plot), data=filter(final_df,Spp=="V" & Code!="7V1")))) # W = 0.94877, p-value = 0.1225

cooksd <- cooks.distance(lmer(Anet~rescale(CO2)+rescale(meanSWC)+time_scaled + (1|Plot), data=filter(final_df,Spp=="V")))
plot(cooks.distance(lmer(Anet~rescale(CO2)+rescale(meanSWC)+time_scaled + (1|Plot), data=filter(final_df,Spp=="V"))))
sample_size <- nobs(lmer(Anet~rescale(CO2)+rescale(meanSWC)+time_scaled + (1|Plot), data=filter(final_df,Spp=="V")))
abline(h = 4/nobs(lmer(Anet~rescale(CO2)+rescale(meanSWC)+time_scaled + (1|Plot), data=filter(final_df,Spp=="V"))), col="red")
text(x=1:length(cooksd)+1, y=cooksd, labels=ifelse(cooksd>4/sample_size, names(cooksd),""), col="red")  # add labels
# eliminate outlier from row 52, "7V1" (EW with very low Anet)

summary(lm(Anet~rescale(CO2)*rescale(meanSWC), data=plotmeans.L)) 
summary(lm(Anet~rescale(CO2)*rescale(meanSWC)+time_scaled, data=plotmeans.L)) 
summary(lmer(Anet~rescale(CO2)*rescale(meanSWC)+time_scaled + (1|Plot), data=filter(final_df,Spp=="L")))
plot(lmer(Anet~rescale(CO2)+rescale(meanSWC)+time_scaled + (1|Plot), data=filter(final_df,Spp=="L")))
qqmath(lmer(Anet~rescale(CO2)+rescale(meanSWC)+time_scaled + (1|Plot), data=filter(final_df,Spp=="L")))
shapiro.test(resid(lmer(Anet~rescale(CO2)+rescale(meanSWC)+time_scaled + (1|Plot), data=filter(final_df,Spp=="L")))) #
qqPlot(lm(Anet~rescale(CO2)*rescale(meanSWC)+time_scaled, data=plotmeans.L))
shapiro.test(resid(lm(Anet~rescale(CO2)*rescale(meanSWC)+time_scaled, data=plotmeans.L))) # W = 0.97525, p-value = 0.9148

cooksd <- cooks.distance(lmer(Anet~rescale(CO2)+rescale(meanSWC)+time_scaled + (1|Plot), data=filter(final_df,Spp=="L")))
plot(cooks.distance(lmer(Anet~rescale(CO2)+rescale(meanSWC)+time_scaled + (1|Plot), data=filter(final_df,Spp=="L"))))
sample_size <- nobs(lmer(Anet~rescale(CO2)+rescale(meanSWC)+time_scaled + (1|Plot), data=filter(final_df,Spp=="L")))
abline(h = 4/nobs(lmer(Anet~rescale(CO2)+rescale(meanSWC)+time_scaled + (1|Plot), data=filter(final_df,Spp=="L"))), col="red")
text(x=1:length(cooksd)+1, y=cooksd, labels=ifelse(cooksd>4/sample_size, names(cooksd),""), col="red")  # add labels
# try it without row 53, "13L6" ... still singular

prior2 <- prior(normal(0,15),class = b)
Anet_full_L <- brm(Anet ~ rescale(CO2)*rescale(meanSWC)+time_scaled + (1|Plot), data=filter(final_df,Spp=="L"), iter=3000, prior=prior2, control = list(adapt_delta =0.99, max_treedepth = 12))
summary(Anet_full_L)

# gs
# grid.arrange(
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
summary(lm(gs~rescale(CO2)+rescale(meanSWC), data=plotmeans.V)) 
summary(lm(gs~rescale(CO2)+rescale(meanSWC)+time_scaled, data=plotmeans.V)) 
summary(lmer(gs~rescale(CO2)+rescale(meanSWC)+time_scaled + (1|Plot), data=filter(final_df,Spp=="V")))
plot(lmer(gs~rescale(CO2)+rescale(meanSWC)+time_scaled + (1|Plot), data=filter(final_df,Spp=="V")))
qqmath(lmer(gs~rescale(CO2)+rescale(meanSWC)+time_scaled + (1|Plot), data=filter(final_df,Spp=="V")))
shapiro.test(resid(lmer(gs~rescale(CO2)+rescale(meanSWC)+time_scaled + (1|Plot), data=filter(final_df,Spp=="V")))) # W = 0.9926, p-value = 0.9976

summary(lm(gs~rescale(CO2)+rescale(meanSWC), data=plotmeans.L)) 
summary(lm(gs~rescale(CO2)*rescale(meanSWC)+time_scaled, data=plotmeans.L)) 
summary(lmer(gs~rescale(CO2)*rescale(meanSWC)+time_scaled + (1|Plot), data=filter(final_df,Spp=="L")))
plot(lmer(gs~rescale(CO2)*rescale(meanSWC)+time_scaled + (1|Plot), data=filter(final_df,Spp=="L")))
qqmath(lmer(gs~rescale(CO2)*rescale(meanSWC)+time_scaled + (1|Plot), data=filter(final_df,Spp=="L")))
shapiro.test(resid(lmer(gs~rescale(CO2)*rescale(meanSWC)+time_scaled + (1|Plot), data=filter(final_df,Spp=="L")))) # W = 0.9778, p-value = 0.5779

# WUE
grid.arrange(
  ggpredict(lmer(WUE~rescale(CO2)*rescale(meanSWC)+time_scaled + (1|Plot), data=filter(final_df,Spp=="V")), 
            terms=c("CO2","meanSWC [4,42]"))%>% plot(rawdata=T,ci=T,colors=c("red","blue")) + labs(title="WUE, V *"),
  ggpredict(lmer(WUE~rescale(CO2)+rescale(meanSWC)+time_scaled + (1|Plot), data=filter(final_df,Spp=="V")), 
            terms=c("CO2","meanSWC [4,42]"))%>% plot(rawdata=T,ci=T,colors=c("red","blue")) + labs(title="WUE, V +"),
  ggpredict(lmer(WUE~rescale(CO2)*rescale(meanSWC)+time_scaled + (1|Plot), data=filter(final_df,Spp=="L",)), 
            terms=c("CO2","meanSWC [4,42]"))%>% plot(rawdata=T,ci=T,colors=c("red","blue")) + labs(title="WUE, L *"),
  ggpredict(lmer(WUE~rescale(CO2)+rescale(meanSWC)+time_scaled + (1|Plot), data=filter(final_df,Spp=="L",)), 
            terms=c("CO2","meanSWC [4,42]"))%>% plot(rawdata=T,ci=T,colors=c("red","blue")) + labs(title="WUE, L +") )
summary(lm(WUE~rescale(CO2)+rescale(meanSWC), data=plotmeans.V)) 
summary(lm(WUE~rescale(CO2)+rescale(meanSWC)+time_scaled, data=plotmeans.V)) 
summary(lmer(WUE~rescale(CO2)+rescale(meanSWC)+time_scaled + (1|Plot), data=filter(final_df,Spp=="V")))
plot(lmer(WUE~rescale(CO2)+rescale(meanSWC)+time_scaled + (1|Plot), data=filter(final_df,Spp=="V")))
qqmath(lmer(WUE~rescale(CO2)+rescale(meanSWC)+time_scaled + (1|Plot), data=filter(final_df,Spp=="V")))
shapiro.test(resid(lmer(WUE~rescale(CO2)+rescale(meanSWC)+time_scaled + (1|Plot), data=filter(final_df,Spp=="V")))) # W = 0.97932, p-value = 0.7514

summary(lm(WUE~rescale(CO2)+rescale(meanSWC), data=plotmeans.L)) 
summary(lm(WUE~rescale(CO2)+rescale(meanSWC)+time_scaled, data=plotmeans.L)) 
summary(lmer(WUE~rescale(CO2)+rescale(meanSWC)+time_scaled + (1|Plot), data=filter(final_df,Spp=="L")))
plot(lmer(WUE~rescale(CO2)+rescale(meanSWC)+time_scaled + (1|Plot), data=filter(final_df,Spp=="L")))
qqmath(lmer(WUE~rescale(CO2)+rescale(meanSWC)+time_scaled + (1|Plot), data=filter(final_df,Spp=="L")))
shapiro.test(resid(lmer(WUE~rescale(CO2)+rescale(meanSWC)+time_scaled + (1|Plot), data=filter(final_df,Spp=="L")))) # W = 0.97765, p-value = 0.5721

# tot_area
grid.arrange(
  ggpredict(lm(tot_area~rescale(CO2)*rescale(meanSWC), data=plotmeans.V), 
            terms=c("CO2","meanSWC [4,42]"))%>% plot(rawdata=T,ci=T,colors=c("red","blue")) + labs(title="tot_area, V *"),
  ggpredict(lm(tot_area~rescale(CO2)+rescale(meanSWC), data=plotmeans.V), 
            terms=c("CO2","meanSWC [4,42]"))%>% plot(rawdata=T,ci=T,colors=c("red","blue")) + labs(title="tot_area, V +"),
  ggpredict(lm(tot_area~rescale(CO2)*rescale(meanSWC), data=plotmeans.V2), 
            terms=c("CO2","meanSWC [4,42]"))%>% plot(rawdata=T,ci=T,colors=c("red","blue")) + labs(title="tot_area, V *, unfiltered"),
  ggpredict(lm(tot_area~rescale(CO2)+rescale(meanSWC), data=plotmeans.V2), 
            terms=c("CO2","meanSWC [4,42]"))%>% plot(rawdata=T,ci=T,colors=c("red","blue")) + labs(title="tot_area, V +, unfiltered"),
  ggpredict(lm(tot_area~rescale(CO2)*rescale(meanSWC), data=plotmeans.L), 
            terms=c("CO2","meanSWC [4,42]"))%>% plot(rawdata=T,ci=T,colors=c("red","blue")) + labs(title="tot_area, L *"),
  ggpredict(lm(tot_area~rescale(CO2)+rescale(meanSWC), data=plotmeans.L), 
            terms=c("CO2","meanSWC [4,42]"))%>% plot(rawdata=T,ci=T,colors=c("red","blue")) + labs(title="tot_area, L +"),
  ggpredict(lm(tot_area~rescale(CO2)*rescale(meanSWC), data=plotmeans.L2), 
            terms=c("CO2","meanSWC [4,42]"))%>% plot(rawdata=T,ci=T,colors=c("red","blue")) + labs(title="tot_area, L *, unfiltered"),
  ggpredict(lm(tot_area~rescale(CO2)+rescale(meanSWC), data=plotmeans.L2), 
            terms=c("CO2","meanSWC [4,42]"))%>% plot(rawdata=T,ci=T,colors=c("red","blue")) + labs(title="tot_area, L +, unfiltered"), nrow=2)
summary(lm(tot_area~rescale(CO2)+rescale(meanSWC), data=plotmeans.V)) 
plot(lm(tot_area~rescale(CO2)+rescale(meanSWC), data=plotmeans.V))
qqPlot(lm(tot_area~rescale(CO2)+rescale(meanSWC), data=plotmeans.V))
shapiro.test(resid(lm(tot_area~rescale(CO2)+rescale(meanSWC), data=plotmeans.V))) # W = 0.88671, p-value = 0.218

summary(lm(tot_area~rescale(CO2)+rescale(meanSWC), data=plotmeans.L)) 
plot(lm(tot_area~rescale(CO2)+rescale(meanSWC), data=plotmeans.L))
qqPlot(lm(tot_area~rescale(CO2)+rescale(meanSWC), data=plotmeans.L))
shapiro.test(resid(lm(tot_area~rescale(CO2)+rescale(meanSWC), data=plotmeans.L))) # W = 0.98008, p-value = 0.9642

# SRL
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
plot(lm(SRL~rescale(CO2)+rescale(meanSWC), data=plotmeans.V))
qqPlot(lm(SRL~rescale(CO2)+rescale(meanSWC), data=plotmeans.V))
shapiro.test(resid(lm(SRL~rescale(CO2)+rescale(meanSWC), data=plotmeans.V))) # W = 0.83255, p-value = 0.007623

summary(lm(SRL~rescale(CO2)*rescale(meanSWC), data=plotmeans.L)) 
plot(lm(SRL~rescale(CO2)*rescale(meanSWC), data=plotmeans.L))
qqPlot(lm(SRL~rescale(CO2)*rescale(meanSWC), data=plotmeans.L))
shapiro.test(resid(lm(SRL~rescale(CO2)*rescale(meanSWC), data=plotmeans.L))) # W = 0.90979, p-value = 0.1155

# d13C
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
plot(lm(d13C~rescale(CO2)+rescale(meanSWC), data=plotmeans.V))
qqPlot(lm(d13C~rescale(CO2)+rescale(meanSWC), data=plotmeans.V))
shapiro.test(resid(lm(d13C~rescale(CO2)+rescale(meanSWC), data=plotmeans.V))) # W = 0.95619, p-value = 0.6265

summary(lm(d13C~rescale(CO2)+rescale(meanSWC), data=plotmeans.L)) 
plot(lm(d13C~rescale(CO2)+rescale(meanSWC), data=plotmeans.L))
qqPlot(lm(d13C~rescale(CO2)+rescale(meanSWC), data=plotmeans.L))
shapiro.test(resid(lm(d13C~rescale(CO2)+rescale(meanSWC), data=plotmeans.L))) # W = 0.94774, p-value = 0.4897
# note intx (not significant) that mirrors gs effect:
# lm(formula = d13C ~ rescale(CO2) * rescale(meanSWC), data = plotmeans.L)
# rescale(CO2):rescale(meanSWC)  -1.78170    p = 0.250

# Ht 8
grid.arrange(
  ggpredict(lm(Ht.mm..8~rescale(CO2)*rescale(meanSWC), data=plotmeans.V), 
            terms=c("CO2","meanSWC [4,42]"))%>% plot(rawdata=T,ci=T,colors=c("red","blue")) + labs(title="Ht.mm..8, V *"),
  ggpredict(lm(Ht.mm..8~rescale(CO2)+rescale(meanSWC), data=plotmeans.V), 
            terms=c("CO2","meanSWC [4,42]"))%>% plot(rawdata=T,ci=T,colors=c("red","blue")) + labs(title="Ht.mm..8, V +"),
  ggpredict(lm(Ht.mm..8~rescale(CO2)*rescale(meanSWC), data=plotmeans.V2), 
            terms=c("CO2","meanSWC [4,42]"))%>% plot(rawdata=T,ci=T,colors=c("red","blue")) + labs(title="Ht.mm..8, V *, unfiltered"),
  ggpredict(lm(Ht.mm..8~rescale(CO2)+rescale(meanSWC), data=plotmeans.V2), 
            terms=c("CO2","meanSWC [4,42]"))%>% plot(rawdata=T,ci=T,colors=c("red","blue")) + labs(title="Ht.mm..8, V +, unfiltered"),
  ggpredict(lm(Ht.mm..8~rescale(CO2)*rescale(meanSWC), data=plotmeans.L), 
            terms=c("CO2","meanSWC [4,42]"))%>% plot(rawdata=T,ci=T,colors=c("red","blue")) + labs(title="Ht.mm..8, L *"),
  ggpredict(lm(Ht.mm..8~rescale(CO2)+rescale(meanSWC), data=plotmeans.L), 
            terms=c("CO2","meanSWC [4,42]"))%>% plot(rawdata=T,ci=T,colors=c("red","blue")) + labs(title="Ht.mm..8, L +"),
  ggpredict(lm(Ht.mm..8~rescale(CO2)*rescale(meanSWC), data=plotmeans.L2), 
            terms=c("CO2","meanSWC [4,42]"))%>% plot(rawdata=T,ci=T,colors=c("red","blue")) + labs(title="Ht.mm..8, L *, unfiltered"),
  ggpredict(lm(Ht.mm..8~rescale(CO2)+rescale(meanSWC), data=plotmeans.L2), 
            terms=c("CO2","meanSWC [4,42]"))%>% plot(rawdata=T,ci=T,colors=c("red","blue")) + labs(title="Ht.mm..8, L +, unfiltered"), nrow=2)
summary(lm(Ht.mm..8~rescale(CO2)+rescale(meanSWC), data=plotmeans.V)) 
plot(lm(Ht.mm..8~rescale(CO2)+rescale(meanSWC), data=plotmeans.V))
qqPlot(lm(Ht.mm..8~rescale(CO2)+rescale(meanSWC), data=plotmeans.V))
shapiro.test(resid(lm(Ht.mm..8~rescale(CO2)+rescale(meanSWC), data=plotmeans.V))) # W = 0.98947, p-value = 0.9961

summary(lm(Ht.mm..8~rescale(CO2)*rescale(meanSWC), data=plotmeans.L)) 
plot(lm(Ht.mm..8~rescale(CO2)*rescale(meanSWC), data=plotmeans.L))
qqPlot(lm(Ht.mm..8~rescale(CO2)*rescale(meanSWC), data=plotmeans.L))
shapiro.test(resid(lm(Ht.mm..8~rescale(CO2)*rescale(meanSWC), data=plotmeans.L))) # W = 0.97135, p-value = 0.8599

# rootmass
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
plot(lm(rootmass_g~rescale(CO2)+rescale(meanSWC), data=plotmeans.V))
qqPlot(lm(rootmass_g~rescale(CO2)+rescale(meanSWC), data=plotmeans.V))
shapiro.test(resid(lm(rootmass_g~rescale(CO2)+rescale(meanSWC), data=plotmeans.V))) # W = 0.97232, p-value = 0.8746

summary(lm(rootmass_g~rescale(CO2)+rescale(meanSWC), data=plotmeans.L)) 
plot(lm(rootmass_g~rescale(CO2)+rescale(meanSWC), data=plotmeans.L))
qqPlot(lm(rootmass_g~rescale(CO2)+rescale(meanSWC), data=plotmeans.L))
shapiro.test(resid(lm(rootmass_g~rescale(CO2)+rescale(meanSWC), data=plotmeans.L))) # W = 0.96603, p-value = 0.7709

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
# fig2L <- fig2_meanse %>% 
  pivot_longer(cols=c("ED1","EW1"), names_to="Tmt", values_to="value") %>% 
  select(variable, y, Tmt, value) %>% 
  pivot_wider(names_from = "y", values_from = "value") %>% 
ggplot() +
  geom_pointrange(aes(x=variable, y=y, ymin=ymin, ymax=ymax, group=Tmt, color=Tmt), size=1.2, linewidth=1.2, position=position_dodge(width=0.2)) + scale_color_manual(values=c("red","blue")) +
  ggtitle("Quercus wislizenii (live oak), filtered") +
  ylab("% change with eCO2") + xlab("Plant Response") +
  theme_classic(base_size = 20) + geom_abline(color= "red", linetype="dashed", slope = 0, intercept= 0)
  

# now for V!
fig2_meanseV <- final_df_nh %>% 
#  fig2_meanseV <- final_df %>% 
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
#fig2V <- fig2_meanseV %>% 
  pivot_longer(cols=c("ED1","EW1"), names_to="Tmt", values_to="value") %>% 
  select(variable, y, Tmt, value) %>% 
  pivot_wider(names_from = "y", values_from = "value") %>% 
  ggplot() +
  geom_pointrange(aes(x=variable, y=y, ymin=ymin, ymax=ymax, group=Tmt, color=Tmt), size=1.2, linewidth=1.2, position=position_dodge(width=0.2)) + scale_color_manual(values=c("red","blue")) +
  ggtitle("Quercus lobata (valley oak), filtered") +
  ylab("% change with eCO2") + xlab("Plant Response") +
  theme_classic(base_size = 20) + geom_abline(color= "red", linetype="dashed", slope = 0, intercept= 0)

# compare filtered versions
grid.arrange(fig2V_nh, fig2L_nh, nrow=2)

# compare unfiltered versions
grid.arrange(fig2V, fig2L, nrow=2) # not that interesting


# Last but not least, PERMANOVA
library(vegan)
plotmeans.V <- final_df_nh %>% 
  filter(Spp == "V") %>% 
  group_by(Plot, H2OTmt, CO2Tmt, meanSWC, CO2) %>% 
  summarise(across(is.numeric, ~ mean(.x, na.rm=T)))
pm.V_responsemat <- plotmeans.V[,c("Anet","SRL","Ht.mm..8")]

adonis2(pm.V_responsemat ~ rescale(CO2)*rescale(meanSWC), plotmeans.V, na.rm=T) 
adonis2(pm.V_responsemat ~ CO2Tmt*H2OTmt, plotmeans.V, na.rm=T) 

plotmeans.L <- final_df_nh %>% 
  filter(Spp == "L") %>% 
  group_by(Plot, H2OTmt, CO2Tmt, meanSWC, CO2) %>% 
  summarise(across(is.numeric, ~ mean(.x, na.rm=T)))
pm.L_responsemat <- plotmeans.L[,c("SRL")]
# pm.L_responsemat <- plotmeans.L[,c("Anet","SRL","Ht.mm..8")]

adonis2(pm.L_responsemat ~ rescale(CO2)*rescale(meanSWC), plotmeans.L, na.rm=T) 
#adonis2(pm.L_responsemat ~ CO2Tmt*H2OTmt, plotmeans.L, na.rm=T) 

anova(lm(Ht.mm..8 ~ rescale(CO2)*rescale(meanSWC), data=plotmeans.L))



# gs, but categorical H2OTmt
# gs
grid.arrange(
  ggpredict(lm(gs~CO2*H2OTmt, data=plotmeans.V), 
            terms=c("CO2","H2OTmt"))%>% plot(rawdata=T,ci=T,colors=c("red","blue")) + labs(title="gs, V *"),
  ggpredict(lm(gs~CO2+H2OTmt, data=plotmeans.V), 
            terms=c("CO2","H2OTmt"))%>% plot(rawdata=T,ci=T,colors=c("red","blue")) + labs(title="gs, V +"),
  ggpredict(lm(gs~CO2*H2OTmt, data=plotmeans.L), 
            terms=c("CO2","H2OTmt"))%>% plot(rawdata=T,ci=T,colors=c("red","blue")) + labs(title="gs, L *"),
  ggpredict(lm(gs~CO2+H2OTmt, data=plotmeans.L), 
            terms=c("CO2","H2OTmt"))%>% plot(rawdata=T,ci=T,colors=c("red","blue")) + labs(title="gs, L +") )
summary(lm(gs~CO2*H2OTmt, data=plotmeans.V)) 
plot(lm(gs~CO2*H2OTmt, data=plotmeans.V))
qqPlot(lm(gs~CO2*H2OTmt, data=plotmeans.V))
shapiro.test(resid(lm(gs~CO2*H2OTmt, data=plotmeans.V))) # W = 0.96484, p-value = 0.8012

summary(lm(gs~CO2*H2OTmt, data=plotmeans.L)) 
plot(lm(gs~CO2*H2OTmt, data=plotmeans.L))
qqPlot(lm(gs~CO2*H2OTmt, data=plotmeans.L))
shapiro.test(resid(lm(gs~CO2*H2OTmt, data=plotmeans.L))) 

# is Ht.mm..5 associated with herb?
# both spp
summary(glm(herb ~ Ht.mm..5*H2OTmt, data = final_df %>% 
  mutate(herb = as.numeric(Code %in% firstfullherb$Code)) ))
ggpredict(glm(herb ~ Ht.mm..5*H2OTmt, data = final_df %>% 
                mutate(herb = as.numeric(Code %in% firstfullherb$Code))), terms=c("Ht.mm..5","H2OTmt") ) %>% plot(rawdata=T, colors=c("blue","red")) 
# two things: live oaks tended not to get eaten, and also tended to be taller. So not actually going through herbivory at all, all the eaten ones are Vs

# just V
summary(glm(herb ~ Ht.mm..5*H2OTmt, data = final_df %>% 
              filter(Spp == "V") %>% 
              mutate(herb = as.numeric(Code %in% firstfullherb$Code)) ))
ggpredict(glm(herb ~ Ht.mm..5*H2OTmt, data = final_df %>% 
                filter(Spp == "V") %>% 
                mutate(herb = as.numeric(Code %in% firstfullherb$Code))), terms=c("Ht.mm..5","H2OTmt") ) %>% plot(rawdata=T, colors=c("blue","red")) # est = 0.002460, p = 0.05103
# it sure looks like the relationship between height and herbivory depends on spp. Probably interacting with water

summary(glm(herb ~ Ht.mm..5*H2OTmt, data = final_df %>% 
              filter(Spp == "V") %>% 
              mutate(herb = as.numeric(Code %in% firstherb$Code)) ))
ggpredict(glm(herb ~ Ht.mm..5*H2OTmt, data = final_df %>% 
                filter(Spp == "V") %>% 
                mutate(herb = as.numeric(Code %in% firstherb$Code))), terms=c("Ht.mm..5","H2OTmt") ) %>% plot(rawdata=T, colors=c("blue","red")) 

# now for L
summary(glm(herb ~ Ht.mm..5*H2OTmt, data = final_df %>% 
              filter(Spp == "L") %>% 
              mutate(herb = as.numeric(Code %in% firstfullherb$Code)) ))
ggpredict(glm(herb ~ Ht.mm..5*H2OTmt, data = final_df %>% 
                filter(Spp == "L") %>% 
                mutate(herb = as.numeric(Code %in% firstfullherb$Code))), terms=c("Ht.mm..5","H2OTmt") ) %>% plot(rawdata=T, colors=c("blue","red")) 
summary(glm(herb ~ Ht.mm..5*H2OTmt, data = final_df %>% 
              filter(Spp == "L") %>% 
              mutate(herb = as.numeric(Code %in% firstherb$Code)) ))
ggpredict(glm(herb ~ Ht.mm..5*H2OTmt, data = final_df %>% 
                filter(Spp == "L") %>% 
                mutate(herb = as.numeric(Code %in% firstherb$Code))), terms=c("Ht.mm..5","H2OTmt") ) %>% plot(rawdata=T, colors=c("blue","red")) 
