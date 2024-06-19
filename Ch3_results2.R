# Ch 3 Results
# AKA Final Round Final Round, Best of Three
library(car)

hist(sqrt(LiCOR_df.[LiCOR_df.$Spp == "V",]$Anet), breaks = 2*sqrt(nrow(LiCOR_df.)))
qqPlot(sqrt(LiCOR_df.[LiCOR_df.$Spp == "V",]$Anet))
hist(sqrt(LiCOR_df.[LiCOR_df.$Spp == "L",]$Anet), breaks = 2*sqrt(nrow(LiCOR_df.)))
qqPlot(sqrt(LiCOR_df.[LiCOR_df.$Spp == "L",]$Anet))
# log transform for Anet

hist(sqrt(LiCOR_df.[LiCOR_df.$Spp=="V",]$gs))
qqPlot(sqrt(LiCOR_df.[LiCOR_df.$Spp=="V",]$gs))
hist(sqrt(LiCOR_df.[LiCOR_df.$Spp=="L",]$gs))
qqPlot(sqrt(LiCOR_df.[LiCOR_df.$Spp=="L",]$gs))
# sqrt gs

hist(LiCOR_df.[LiCOR_df.$Spp=="V",]$WUE)
qqPlot((LiCOR_df.[LiCOR_df.$Spp=="V",]$WUE))
hist(LiCOR_df.[LiCOR_df.$Spp=="L",]$WUE)
qqPlot(LiCOR_df.[LiCOR_df.$Spp=="L",]$WUE)
# WUE ok

# biomass2.
hist(sqrt(biomass2.[biomass2.$Spp=="L",]$totmass), breaks=2*sqrt(nrow(biomass2.))) # outlier, ok
qqPlot(sqrt(biomass2.[biomass2.$Spp=="V",]$totmass)) 
qqPlot(sqrt(biomass2.[biomass2.$Spp=="L",]$totmass)) 

biomass2. %>% 
  filter(Spp=="V") %>%
  filter(!Code %in% firstfullherb$Code) %>% 
  select(Ht.mm..8) %>%
  unlist() %>% 
  as.numeric() %>%
   log() %>% 
  # sqrt() %>% 
  #   hist(breaks = 2*sqrt(nrow(lai.)))
  qqPlot()

hist(sqrt(biomass2.[biomass2.$Spp=="L",]$Ht.mm..8), breaks=2*sqrt(nrow(biomass2.)))
qqPlot(sqrt(biomass2.[biomass2.$Spp=="L",]$Ht.mm..8))

hist((biomass2.[biomass2.$Spp=="L",]$rootshoot), breaks=2*sqrt(nrow(biomass2.))) 
qqPlot((biomass2.[biomass2.$Spp=="L",]$rootshoot)) 
qqPlot((biomass2.[biomass2.$Spp=="V",]$rootshoot)) 
# rootshoot ok

hist(biomass2.[biomass2.$Spp=="L",]$lwc, breaks=2*sqrt(nrow(biomass2.))) 
qqPlot(biomass2.[biomass2.$Spp=="V",]$lwc)
qqPlot(biomass2.[biomass2.$Spp=="L",]$lwc)
# lwc fine by spp

# since aboveground biomass is unreliable, what about root mass?
hist(sqrt(biomass2.[biomass2.$Spp=="L",]$rootmass_g), breaks=2*sqrt(nrow(biomass2.))) 
qqPlot(sqrt(biomass2.[biomass2.$Spp=="L",]$rootmass_g))
hist(sqrt(biomass2.[biomass2.$Spp=="V",]$rootmass_g), breaks=2*sqrt(nrow(biomass2.))) 
qqPlot(sqrt(biomass2.[biomass2.$Spp=="V",]$rootmass_g))

library(caret)
# check leaf area variables here
lai. %>% 
  filter(Spp=="L") %>% 
  #  dplyr::select(avg_area) %>% # sqrt
  # dplyr::select(perim_per_A) %>% # < 1 and log
  # dplyr::select(tot_area) %>%
   dplyr::select(SLA) %>% 
  unlist() %>% 
  as.numeric() %>%
  #  BoxCoxTrans(na.rm=TRUE) %>% predict(newdata=unlist(lai.[lai$Spp=="V",][,"avg_area"])) %>% 
  # log() %>% 
  #sqrt() %>% 
#   hist(breaks = 2*sqrt(nrow(lai.)))
  qqPlot()
# filtered by tot_area > 1000 
# avg_area stinks; log is best
# perim_per_A: < 1 and log
# tot_area okay with sqrt
# SLA just fine if you filter < 4000 and the L > 12000

# rootimage.
hist(log(rootimage.[rootimage.$Spp=="V",]$SRL), breaks=sqrt(nrow(rootimage.)))
qqPlot(log(rootimage.[rootimage.$Spp=="V",]$SRL))
hist(log(rootimage.[rootimage.$Spp=="L",]$SRL), breaks=sqrt(nrow(rootimage.)))
qqPlot(log(rootimage.[rootimage.$Spp=="L",]$SRL))
# logSRL is great

# SIF.
hist(SIF.[SIF.$Spp=="V",]$d13C)
qqPlot(SIF.[SIF.$Spp=="V",]$d13C) # cute
hist(SIF.[SIF.$Spp=="L",]$d13C)
qqPlot(SIF.[SIF.$Spp=="L",]$d13C) # cute!


### combine into one giant, all-inclusive dataset

 final_df <- biomass2. %>% 
  dplyr::select(Plot, Spp, Code, totmass, rootshoot, Ht.mm..8, lwc, CO2, meanSWC) %>% 
  left_join(LiCOR_df.[,c("Code","Anet","gs", "WUE")], by = "Code") %>% 
  left_join(lai.[,c("Code","avg_area","perim_per_A","tot_area", "SLA")], by = "Code") %>% 
  left_join(rootimage.[,c("Code","SRL")], by = "Code") %>% 
  left_join(SIF.[,c("Code","d13C")], by = "Code") %>% 
#  mutate(logAnet = log(Anet), sqrtgs = sqrt(gs), sqrtTotmass = sqrt(totmass), sqrtAvg_area = sqrt(avg_area), logPerimArea = log(perim_per_A), logSRL = log(SRL)) %>%
  group_by(Plot, Spp) %>% 
  mutate(n = n()) %>% 
  left_join(lookup, by = "Plot") %>% 
  mutate(H2OTmt = substr(Tmt,2,2), CO2Tmt = substr(Tmt,1,1)) 

# response variables
names(final_df)
# "totmass"     "rootshoot"   "Ht.mm..8"   "lwc"    "Anet"        "gs"          "WUE"         "avg_area"    "perim_per_A" "tot_area"    "SLA"         "SRL"        "d13C"

# univariate models for each response variable, with plot as a random effect
library(ggeffects)
library(lme4)
library(lmerTest)
library(scales)
# totmass -- sqrt
grid.arrange( 
ggpredict(lmer(sqrt(totmass)~rescale(CO2)+rescale(meanSWC) + (1|Plot), data=filter(final_df[final_df$Spp=="V",])), 
           terms=c("CO2","meanSWC [4,42]"))%>% plot(rawdata = TRUE, ci = TRUE, colors=c("red","blue")) 
+ labs(title = "Totmass, V"),
 ggpredict(lmer(sqrt(totmass)~rescale(CO2)+rescale(meanSWC) + (1|Plot), data=filter(final_df[final_df$Spp=="L",])), 
           terms=c("CO2","meanSWC [4,42]"))%>% plot(rawdata = TRUE, ci = TRUE, colors=c("red","blue")) 
+ labs(title = "Totmass, L"),
ggpredict(lmer(sqrt(totmass)~CO2+H2OTmt + (1|Plot), data=filter(final_df[final_df$Spp=="V",])), 
          terms=c("CO2","H2OTmt"))%>% plot(rawdata = TRUE, ci = TRUE, colors=c("red","blue")) 
+ labs(title = "Totmass, V"),
ggpredict(lmer(sqrt(totmass)~CO2+H2OTmt + (1|Plot), data=filter(final_df[final_df$Spp=="L",])), 
          terms=c("CO2","H2OTmt"))%>% plot(rawdata = TRUE, ci = TRUE, colors=c("red","blue")) 
+ labs(title = "Totmass, L"))
summary(lmer(sqrt(totmass)~rescale(CO2)+rescale(meanSWC) + (1|Plot), data=filter(final_df[final_df$Spp=="V",]))) 
#summary(lmer(sqrt(totmass)~CO2+H2OTmt + (1|Plot), data=filter(final_df[final_df$Spp=="V",]))) 
        
summary(lmer(sqrt(totmass)~rescale(CO2)+rescale(meanSWC) + (1|Plot), data=filter(final_df[final_df$Spp=="L",])))
#summary(lmer(sqrt(totmass)~CO2+H2OTmt + (1|Plot), data=filter(final_df[final_df$Spp=="L",]))) 

# rootshoot
grid.arrange( 
  ggpredict(lmer(rootshoot~rescale(CO2)+rescale(meanSWC) + (1|Plot), data=filter(final_df[final_df$Spp=="V",])), 
            terms=c("CO2","meanSWC [4,42]"))%>% plot(rawdata = TRUE, ci = TRUE, colors=c("red","blue")) 
  + labs(title = "rootshoot, V"),
  ggpredict(lmer(rootshoot~rescale(CO2)+rescale(meanSWC) + (1|Plot), data=filter(final_df[final_df$Spp=="L",])), 
            terms=c("CO2","meanSWC [4,42]"))%>% plot(rawdata = TRUE, ci = TRUE, colors=c("red","blue")) 
  + labs(title = "rootshoot, L"),
  ggpredict(lmer(rootshoot~CO2+H2OTmt + (1|Plot), data=filter(final_df[final_df$Spp=="V",])), 
            terms=c("CO2","H2OTmt"))%>% plot(rawdata = TRUE, ci = TRUE, colors=c("red","blue")) 
  + labs(title = "rootshoot, V"),
  ggpredict(lmer(rootshoot~CO2+H2OTmt + (1|Plot), data=filter(final_df[final_df$Spp=="L",])), 
            terms=c("CO2","H2OTmt"))%>% plot(rawdata = TRUE, ci = TRUE, colors=c("red","blue")) 
  + labs(title = "rootshoot, L"))
#summary(lmer(rootshoot~rescale(CO2)+rescale(meanSWC) + (1|Plot), data=filter(final_df[final_df$Spp=="V",]))) 
#summary(lmer(rootshoot~CO2+H2OTmt + (1|Plot), data=filter(final_df[final_df$Spp=="V",]))) 

summary(lmer(rootshoot~rescale(CO2)+rescale(meanSWC) + (1|Plot), data=filter(final_df[final_df$Spp=="L",])))
#summary(lmer(rootshoot~CO2+H2OTmt + (1|Plot), data=filter(final_df[final_df$Spp=="L",]))) 

# lwc
grid.arrange( 
  ggpredict(lmer(lwc~rescale(CO2)+rescale(meanSWC) + (1|Plot), data=filter(final_df[final_df$Spp=="V",])), 
            terms=c("CO2","meanSWC [4,42]"))%>% plot(rawdata = TRUE, ci = TRUE, colors=c("red","blue")) 
  + labs(title = "lwc, V"),
  ggpredict(lmer(lwc~rescale(CO2)+rescale(meanSWC) + (1|Plot), data=filter(final_df[final_df$Spp=="L",])), 
            terms=c("CO2","meanSWC [4,42]"))%>% plot(rawdata = TRUE, ci = TRUE, colors=c("red","blue")) 
  + labs(title = "lwc, L"),
  ggpredict(lmer(lwc~CO2+H2OTmt + (1|Plot), data=filter(final_df[final_df$Spp=="V",])), 
            terms=c("CO2","H2OTmt"))%>% plot(rawdata = TRUE, ci = TRUE, colors=c("red","blue")) 
  + labs(title = "lwc, V"),
  ggpredict(lmer(lwc~CO2+H2OTmt + (1|Plot), data=filter(final_df[final_df$Spp=="L",])), 
            terms=c("CO2","H2OTmt"))%>% plot(rawdata = TRUE, ci = TRUE, colors=c("red","blue")) 
  + labs(title = "lwc, L"))
summary(lmer(lwc~rescale(CO2)+rescale(meanSWC) + (1|Plot), data=filter(final_df[final_df$Spp=="V",]))) 
#summary(lmer(lwc~CO2+H2OTmt + (1|Plot), data=filter(final_df[final_df$Spp=="V",]))) 

summary(lmer(lwc~rescale(CO2)+rescale(meanSWC) + (1|Plot), data=filter(final_df[final_df$Spp=="L",])))
#summary(lmer(lwc~CO2*H2OTmt + (1|Plot), data=filter(final_df[final_df$Spp=="L",]))) 

# Anet -- sqrt
grid.arrange( 
  ggpredict(lmer(sqrt(Anet)~rescale(CO2)+rescale(meanSWC) + (1|Plot), data=filter(final_df[final_df$Spp=="V",])), 
            terms=c("CO2","meanSWC [4,42]"))%>% plot(rawdata = TRUE, ci = TRUE, colors=c("red","blue")) 
  + labs(title = "sqrt(Anet), V"),
  ggpredict(lmer(sqrt(Anet)~rescale(CO2)*rescale(meanSWC) + (1|Plot), data=filter(final_df[final_df$Spp=="L",])), 
            terms=c("CO2","meanSWC [4,42]"))%>% plot(rawdata = TRUE, ci = TRUE, colors=c("red","blue")) 
  + labs(title = "sqrt(Anet), L"),
  ggpredict(lmer(sqrt(Anet)~CO2+H2OTmt + (1|Plot), data=filter(final_df[final_df$Spp=="V",])), 
            terms=c("CO2","H2OTmt"))%>% plot(rawdata = TRUE, ci = TRUE, colors=c("red","blue")) 
  + labs(title = "sqrt(Anet), V"),
  ggpredict(lmer(sqrt(Anet)~CO2+H2OTmt + (1|Plot), data=filter(final_df[final_df$Spp=="L",])), 
            terms=c("CO2","H2OTmt"))%>% plot(rawdata = TRUE, ci = TRUE, colors=c("red","blue")) 
  + labs(title = "sqrt(Anet), L"))
summary(lmer((sqrt(Anet))~rescale(CO2)+rescale(meanSWC) + (1|Plot), data=filter(final_df[final_df$Spp=="V",]))) 
#summary(lmer(sqrt(Anet)~CO2+H2OTmt + (1|Plot), data=filter(final_df[final_df$Spp=="V",]))) 

summary(lmer((sqrt(Anet))~rescale(CO2)*rescale(meanSWC) + (1|Plot), data=filter(final_df[final_df$Spp=="L",])))
#summary(lmer(sqrt(Anet)~CO2+H2OTmt + (1|Plot), data=filter(final_df[final_df$Spp=="L",]))) 

ggpredict(lmer(sqrt(Anet)~rescale(CO2)*rescale(meanSWC) + (1|Plot), data=final_df), 
          terms=c("CO2","meanSWC [4,42]"))%>% plot(rawdata = TRUE, ci = TRUE, colors=c("red","blue")) 
+ labs(title = "log(sqrt(Anet)), both")
# summary(lmer(sqrt(Anet)~rescale(CO2)*rescale(meanSWC) + (1|Plot), data=final_df))
# summary(lmer(sqrt(Anet)~CO2*H2OTmt + (1|Plot), data=final_df))
        

# gs -- sqrt
grid.arrange( 
  ggpredict(lmer(sqrt(gs)~rescale(CO2)+rescale(meanSWC) + (1|Plot), data=filter(final_df[final_df$Spp=="V",])), 
            terms=c("CO2","meanSWC [4,42]"))%>% plot(rawdata = TRUE, ci = TRUE, colors=c("red","blue")) 
  + labs(title = "sqrt(gs), V"),
  ggpredict(lmer(sqrt(gs)~rescale(CO2)+rescale(meanSWC) + (1|Plot), data=filter(final_df[final_df$Spp=="L",])), 
            terms=c("CO2","meanSWC [4,42]"))%>% plot(rawdata = TRUE, ci = TRUE, colors=c("red","blue")) 
  + labs(title = "sqrt(gs), L"),
  ggpredict(lmer(sqrt(gs)~CO2+H2OTmt + (1|Plot), data=filter(final_df[final_df$Spp=="V",])), 
            terms=c("CO2","H2OTmt"))%>% plot(rawdata = TRUE, ci = TRUE, colors=c("red","blue")) 
  + labs(title = "sqrt(gs), V"),
  ggpredict(lmer(sqrt(gs)~CO2+H2OTmt + (1|Plot), data=filter(final_df[final_df$Spp=="L",])), 
            terms=c("CO2","H2OTmt"))%>% plot(rawdata = TRUE, ci = TRUE, colors=c("red","blue")) 
  + labs(title = "sqrt(gs), L"))
summary(lmer(sqrt(gs)~rescale(CO2)+rescale(meanSWC) + (1|Plot), data=filter(final_df[final_df$Spp=="V",]))) 
#summary(lmer(sqrt(gs)~CO2+H2OTmt + (1|Plot), data=filter(final_df[final_df$Spp=="V",]))) 

summary(lmer(sqrt(gs)~rescale(CO2)+rescale(meanSWC) + (1|Plot), data=filter(final_df[final_df$Spp=="L",])))
#summary(lmer(sqrt(gs)~CO2+H2OTmt + (1|Plot), data=filter(final_df[final_df$Spp=="L",]))) 

ggpredict(lmer(sqrt(gs)~rescale(CO2)*rescale(meanSWC) + (1|Plot), data=final_df), 
          terms=c("CO2","meanSWC [4,42]"))%>% plot(rawdata = TRUE, ci = TRUE, colors=c("red","blue")) 
+ labs(title = "gs, both")
summary(lmer(sqrt(gs)~rescale(CO2)+rescale(meanSWC) + (1|Plot), data=final_df))
summary(lmer(sqrt(gs)~CO2+H2OTmt + (1|Plot), data=final_df))

# WUE
grid.arrange( 
  ggpredict(lmer(WUE~rescale(CO2)+rescale(meanSWC) + (1|Plot), data=filter(final_df[final_df$Spp=="V",])), 
            terms=c("CO2","meanSWC [4,42]"))%>% plot(rawdata = TRUE, ci = TRUE, colors=c("red","blue")) + labs(title = "WUE, V"),
  ggpredict(lmer(WUE~rescale(CO2)+rescale(meanSWC) + (1|Plot), data=filter(final_df[final_df$Spp=="L",])), 
            terms=c("CO2","meanSWC [4,42]"))%>% plot(rawdata = TRUE, ci = TRUE, colors=c("red","blue")) 
  + labs(title = "WUE, L"),
  ggpredict(lmer(WUE~CO2+H2OTmt + (1|Plot), data=filter(final_df[final_df$Spp=="V",])), 
            terms=c("CO2","H2OTmt"))%>% plot(rawdata = TRUE, ci = TRUE, colors=c("red","blue")) 
  + labs(title = "WUE, V"),
  ggpredict(lmer(WUE~CO2+H2OTmt + (1|Plot), data=filter(final_df[final_df$Spp=="L",])), 
            terms=c("CO2","H2OTmt"))%>% plot(rawdata = TRUE, ci = TRUE, colors=c("red","blue")) 
  + labs(title = "WUE, L"))
summary(lmer(WUE~rescale(CO2)+rescale(meanSWC) + (1|Plot), data=filter(final_df[final_df$Spp=="V",]))) 
summary(lmer(WUE~CO2+H2OTmt + (1|Plot), data=filter(final_df[final_df$Spp=="V",]))) 

summary(lmer(WUE~rescale(CO2)+rescale(meanSWC) + (1|Plot), data=filter(final_df[final_df$Spp=="L",])))
summary(lmer(WUE~CO2+H2OTmt + (1|Plot), data=filter(final_df[final_df$Spp=="L",]))) 

ggpredict(lmer(WUE~rescale(CO2)+rescale(meanSWC) + (1|Plot), data=final_df), 
          terms=c("CO2","meanSWC [4,42]"))%>% plot(rawdata = TRUE, ci = TRUE, colors=c("red","blue")) + labs(title = "WUE, both")
summary(lmer(WUE~rescale(CO2)+rescale(meanSWC) + (1|Plot), data=final_df))
summary(lmer(WUE~CO2+H2OTmt + (1|Plot), data=final_df))


# avg_area -- sqrt
grid.arrange( 
  ggpredict(lmer(sqrt(avg_area)~rescale(CO2)+rescale(meanSWC) + (1|Plot), data=filter(final_df[final_df$Spp=="V",])), 
            terms=c("CO2","meanSWC [4,42]"))%>% plot(rawdata = TRUE, ci = TRUE, colors=c("red","blue")) + labs(title = "sqrt(avg_area), V"),
  ggpredict(lmer(sqrt(avg_area)~rescale(CO2)+rescale(meanSWC) + (1|Plot), data=filter(final_df[final_df$Spp=="L",])), 
            terms=c("CO2","meanSWC [4,42]"))%>% plot(rawdata = TRUE, ci = TRUE, colors=c("red","blue")) 
  + labs(title = "sqrt(avg_area), L"),
  ggpredict(lmer(sqrt(avg_area)~CO2+H2OTmt + (1|Plot), data=filter(final_df[final_df$Spp=="V",])), 
            terms=c("CO2","H2OTmt"))%>% plot(rawdata = TRUE, ci = TRUE, colors=c("red","blue")) 
  + labs(title = "sqrt(avg_area), V"),
  ggpredict(lmer(sqrt(avg_area)~CO2+H2OTmt + (1|Plot), data=filter(final_df[final_df$Spp=="L",])), 
            terms=c("CO2","H2OTmt"))%>% plot(rawdata = TRUE, ci = TRUE, colors=c("red","blue")) 
  + labs(title = "sqrt(avg_area), L"))
summary(lmer(sqrt(avg_area)~rescale(CO2)+rescale(meanSWC) + (1|Plot), data=filter(final_df[final_df$Spp=="V",]))) 
summary(lmer(sqrt(avg_area)~CO2+H2OTmt + (1|Plot), data=filter(final_df[final_df$Spp=="V",]))) 

summary(lmer(sqrt(avg_area)~rescale(CO2)+rescale(meanSWC) + (1|Plot), data=filter(final_df[final_df$Spp=="L",])))
summary(lmer(sqrt(avg_area)~CO2+H2OTmt + (1|Plot), data=filter(final_df[final_df$Spp=="L",]))) 

# perim_per_A -- log
grid.arrange( 
  ggpredict(lmer(log(perim_per_A)~rescale(CO2)+rescale(meanSWC) + (1|Plot), data=filter(final_df[final_df$Spp=="V",])), 
            terms=c("CO2","meanSWC [4,42]"))%>% plot(rawdata = TRUE, ci = TRUE, colors=c("red","blue")) + labs(title = "log(perim_per_A), V"),
  ggpredict(lmer(log(perim_per_A)~rescale(CO2)+rescale(meanSWC) + (1|Plot), data=filter(final_df[final_df$Spp=="L",])), 
            terms=c("CO2","meanSWC [4,42]"))%>% plot(rawdata = TRUE, ci = TRUE, colors=c("red","blue")) 
  + labs(title = "log(perim_per_A), L"),
  ggpredict(lmer(log(perim_per_A)~CO2+H2OTmt + (1|Plot), data=filter(final_df[final_df$Spp=="V",])), 
            terms=c("CO2","H2OTmt"))%>% plot(rawdata = TRUE, ci = TRUE, colors=c("red","blue")) 
  + labs(title = "log(perim_per_A), V"),
  ggpredict(lmer(log(perim_per_A)~CO2+H2OTmt + (1|Plot), data=filter(final_df[final_df$Spp=="L",])), 
            terms=c("CO2","H2OTmt"))%>% plot(rawdata = TRUE, ci = TRUE, colors=c("red","blue")) 
  + labs(title = "log(perim_per_A), L"))
summary(lmer(log(perim_per_A)~rescale(CO2)+rescale(meanSWC) + (1|Plot), data=filter(final_df[final_df$Spp=="V",]))) 
summary(lmer(log(perim_per_A)~CO2+H2OTmt + (1|Plot), data=filter(final_df[final_df$Spp=="V",]))) 

summary(lmer(log(perim_per_A)~rescale(CO2)+rescale(meanSWC) + (1|Plot), data=filter(final_df[final_df$Spp=="L",])))
summary(lmer(log(perim_per_A)~CO2+H2OTmt + (1|Plot), data=filter(final_df[final_df$Spp=="L",]))) 


# tot_area 
grid.arrange( 
  ggpredict(lmer(tot_area~rescale(CO2)+rescale(meanSWC) + (1|Plot), data=filter(final_df[final_df$Spp=="V",])), 
            terms=c("CO2","meanSWC [4,42]"))%>% plot(rawdata = TRUE, ci = TRUE, colors=c("red","blue")) + labs(title = "tot_area, V"),
  ggpredict(lmer(tot_area~rescale(CO2)+rescale(meanSWC) + (1|Plot), data=filter(final_df[final_df$Spp=="L",])), 
            terms=c("CO2","meanSWC [4,42]"))%>% plot(rawdata = TRUE, ci = TRUE, colors=c("red","blue")) 
  + labs(title = "tot_area, L"),
  ggpredict(lmer(tot_area~CO2+H2OTmt + (1|Plot), data=filter(final_df[final_df$Spp=="V",])), 
            terms=c("CO2","H2OTmt"))%>% plot(rawdata = TRUE, ci = TRUE, colors=c("red","blue")) 
  + labs(title = "tot_area, V"),
  ggpredict(lmer(tot_area~CO2+H2OTmt + (1|Plot), data=filter(final_df[final_df$Spp=="L",])), 
            terms=c("CO2","H2OTmt"))%>% plot(rawdata = TRUE, ci = TRUE, colors=c("red","blue")) 
  + labs(title = "tot_area, L"))
summary(lmer(tot_area~rescale(CO2)+rescale(meanSWC) + (1|Plot), data=filter(final_df[final_df$Spp=="V",]))) 
summary(lmer(tot_area~CO2+H2OTmt + (1|Plot), data=filter(final_df[final_df$Spp=="V",]))) 

summary(lmer(tot_area~rescale(CO2)+rescale(meanSWC) + (1|Plot), data=filter(final_df[final_df$Spp=="L",])))
summary(lmer(tot_area~CO2+H2OTmt + (1|Plot), data=filter(final_df[final_df$Spp=="L",]))) 

# SLA
grid.arrange( 
  ggpredict(lmer(SLA~rescale(CO2)+rescale(meanSWC) + (1|Plot), data=filter(final_df[final_df$Spp=="V",])), 
            terms=c("CO2","meanSWC [4,42]"))%>% plot(rawdata = TRUE, ci = TRUE, colors=c("red","blue")) + labs(title = "SLA, V"),
  ggpredict(lmer(SLA~rescale(CO2)+rescale(meanSWC) + (1|Plot), data=filter(final_df[final_df$Spp=="L",])), 
            terms=c("CO2","meanSWC [4,42]"))%>% plot(rawdata = TRUE, ci = TRUE, colors=c("red","blue")) 
  + labs(title = "SLA, L"),
  ggpredict(lmer(SLA~CO2+H2OTmt + (1|Plot), data=filter(final_df[final_df$Spp=="V",])), 
            terms=c("CO2","H2OTmt"))%>% plot(rawdata = TRUE, ci = TRUE, colors=c("red","blue")) 
  + labs(title = "SLA, V"),
  ggpredict(lmer(SLA~CO2+H2OTmt + (1|Plot), data=filter(final_df[final_df$Spp=="L",])), 
            terms=c("CO2","H2OTmt"))%>% plot(rawdata = TRUE, ci = TRUE, colors=c("red","blue")) 
  + labs(title = "SLA, L"))
summary(lmer(SLA~rescale(CO2)+rescale(meanSWC) + (1|Plot), data=filter(final_df[final_df$Spp=="V",]))) 
summary(lmer(SLA~CO2+H2OTmt + (1|Plot), data=filter(final_df[final_df$Spp=="V",]))) 

summary(lmer(SLA~rescale(CO2)+rescale(meanSWC) + (1|Plot), data=filter(final_df[final_df$Spp=="L",])))
summary(lmer(SLA~CO2+H2OTmt + (1|Plot), data=filter(final_df[final_df$Spp=="L",]))) 

# SRL -- log
grid.arrange( 
  ggpredict(lmer(log(SRL)~rescale(CO2)+rescale(meanSWC) + (1|Plot), data=filter(final_df[final_df$Spp=="V",])), 
            terms=c("CO2","meanSWC [4,42]"))%>% plot(rawdata = TRUE, ci = TRUE, colors=c("red","blue")) + labs(title = "log(SRL), V"),
  ggpredict(lmer(log(SRL)~rescale(CO2)*rescale(meanSWC) + (1|Plot), data=filter(final_df[final_df$Spp=="L",])), 
            terms=c("CO2","meanSWC [4,42]"))%>% plot(rawdata = TRUE, ci = TRUE, colors=c("red","blue")) 
  + labs(title = "log(SRL), L"),
  ggpredict(lmer(log(SRL)~CO2+H2OTmt + (1|Plot), data=filter(final_df[final_df$Spp=="V",])), 
            terms=c("CO2","H2OTmt"))%>% plot(rawdata = TRUE, ci = TRUE, colors=c("red","blue")) 
  + labs(title = "log(SRL), V"),
  ggpredict(lmer(log(SRL)~CO2+H2OTmt + (1|Plot), data=filter(final_df[final_df$Spp=="L",])), 
            terms=c("CO2","H2OTmt"))%>% plot(rawdata = TRUE, ci = TRUE, colors=c("red","blue")) 
  + labs(title = "log(SRL), L"))
summary(lmer(log(SRL)~rescale(CO2)+rescale(meanSWC) + (1|Plot), data=filter(final_df[final_df$Spp=="V",]))) 
summary(lmer(log(SRL)~CO2+H2OTmt + (1|Plot), data=filter(final_df[final_df$Spp=="V",]))) 

summary(lmer(log(SRL)~rescale(CO2)*rescale(meanSWC) + (1|Plot), data=filter(final_df[final_df$Spp=="L",])))
summary(lmer(log(SRL)~CO2+H2OTmt + (1|Plot), data=filter(final_df[final_df$Spp=="L",]))) 

# d13
grid.arrange( 
  ggpredict(lmer(d13C~rescale(CO2)+rescale(meanSWC) + (1|Plot), data=filter(final_df[final_df$Spp=="V",])), 
            terms=c("CO2","meanSWC [4,42]"))%>% plot(rawdata = TRUE, ci = TRUE, colors=c("red","blue")) + labs(title = "d13, V"),
  ggpredict(lmer(d13C~rescale(CO2)+rescale(meanSWC) + (1|Plot), data=filter(final_df[final_df$Spp=="L",])), 
            terms=c("CO2","meanSWC [4,42]"))%>% plot(rawdata = TRUE, ci = TRUE, colors=c("red","blue")) 
  + labs(title = "d13C, L"),
  ggpredict(lmer(d13C~CO2+H2OTmt + (1|Plot), data=filter(final_df[final_df$Spp=="V",])), 
            terms=c("CO2","H2OTmt"))%>% plot(rawdata = TRUE, ci = TRUE, colors=c("red","blue")) 
  + labs(title = "d13C, V"),
  ggpredict(lmer(d13C~CO2+H2OTmt + (1|Plot), data=filter(final_df[final_df$Spp=="L",])), 
            terms=c("CO2","H2OTmt"))%>% plot(rawdata = TRUE, ci = TRUE, colors=c("red","blue")) 
  + labs(title = "d13C, L"))
summary(lmer(d13C~rescale(CO2)+rescale(meanSWC) + (1|Plot), data=filter(final_df[final_df$Spp=="V",]))) 
summary(lmer(d13C~CO2+H2OTmt + (1|Plot), data=filter(final_df[final_df$Spp=="V",]))) 

summary(lmer(d13C~rescale(CO2)+rescale(meanSWC) + (1|Plot), data=filter(final_df[final_df$Spp=="L",])))
summary(lmer(d13C~CO2+H2OTmt + (1|Plot), data=filter(final_df[final_df$Spp=="L",]))) 

# Ht.mm..8
grid.arrange( 
  ggpredict(lmer(log(Ht.mm..8)~rescale(CO2)*rescale(meanSWC) + (1|Plot), data=biomass2.[biomass2.$Spp=="V",]), 
            terms=c("CO2","meanSWC [4,42]"))%>% plot(rawdata = TRUE, ci = TRUE, colors=c("red","blue")) + labs(title = "log(Ht.mm..8), V"),
  ggpredict(lmer(log(Ht.mm..8)~rescale(CO2)*rescale(meanSWC) + (1|Plot), data=biomass2.[biomass2.$Spp=="L",]), 
            terms=c("CO2","meanSWC [4,42]"))%>% plot(rawdata = TRUE, ci = TRUE, colors=c("red","blue")) 
  + labs(title = "log(Ht.mm..8), L"),
  ggpredict(lmer(log(Ht.mm..8)~CO2*H2OTmt + (1|Plot), data=biomass2.[biomass2.$Spp=="V",]), 
            terms=c("CO2","H2OTmt"))%>% plot(rawdata = TRUE, ci = TRUE, colors=c("red","blue")) 
  + labs(title = "log(Ht.mm..8), V"),
  ggpredict(lmer(log(Ht.mm..8)~CO2*H2OTmt + (1|Plot), data=biomass2.[biomass2.$Spp=="L",]), 
            terms=c("CO2","H2OTmt"))%>% plot(rawdata = TRUE, ci = TRUE, colors=c("red","blue")) 
  + labs(title = "log(Ht.mm..8), L"))
summary(lmer(log(Ht.mm..8)~rescale(CO2)*rescale(meanSWC) + (1|Plot), data=biomass2.[biomass2.$Spp=="V",])) 
summary(lmer(log(Ht.mm..8)~CO2*H2OTmt + (1|Plot), data=biomass2.[biomass2.$Spp=="V",])) 

summary(lmer(log(Ht.mm..8)~rescale(CO2)*rescale(meanSWC) + (1|Plot), data=biomass2.[biomass2.$Spp=="L",]))
summary(lmer(log(Ht.mm..8)~rescale(CO2)*rescale(meanSWC) + (1|Plot), data=biomass2.[biomass2.$Spp=="L",]))

summary(lmer(log(Ht.mm..8)~CO2*H2OTmt + (1|Plot), data=biomass2.[biomass2.$Spp=="L",])) 

# rootmass_g
grid.arrange( 
  ggpredict(lmer(sqrt(rootmass_g)~rescale(CO2)+rescale(meanSWC) + (1|Plot), data=biomass2.), 
            terms=c("CO2","meanSWC [4,42]"))%>% plot(rawdata = TRUE, ci = TRUE, colors=c("red","blue")) + labs(title = "sqrt(rootmass_g), V"),
  ggpredict(lmer(sqrt(rootmass_g)~rescale(CO2)+rescale(meanSWC) + (1|Plot), data=biomass2.), 
            terms=c("CO2","meanSWC [4,42]"))%>% plot(rawdata = TRUE, ci = TRUE, colors=c("red","blue")) 
  + labs(title = "sqrt(rootmass_g), L"),
  ggpredict(lmer(sqrt(rootmass_g)~CO2+H2OTmt + (1|Plot), data=biomass2.), 
            terms=c("CO2","H2OTmt"))%>% plot(rawdata = TRUE, ci = TRUE, colors=c("red","blue")) 
  + labs(title = "sqrt(rootmass_g), V"),
  ggpredict(lmer(sqrt(rootmass_g)~CO2+H2OTmt + (1|Plot), data=biomass2.), 
            terms=c("CO2","H2OTmt"))%>% plot(rawdata = TRUE, ci = TRUE, colors=c("red","blue")) 
  + labs(title = "sqrt(rootmass_g), L"))
summary(lmer(sqrt(rootmass_g)~rescale(CO2)+rescale(meanSWC) + (1|Plot), data=biomass2.)) 
summary(lmer(sqrt(rootmass_g)~CO2+H2OTmt + (1|Plot), data=biomass2.)) 

summary(lmer(sqrt(rootmass_g)~rescale(CO2)+rescale(meanSWC) + (1|Plot), data=biomass2.[biomass2.$Spp=="L",]))
summary(lmer(sqrt(rootmass_g)~rescale(CO2)+rescale(meanSWC) + (1|Plot), data=biomass2.[biomass2.$Spp=="L",]))


# Fig 2 from Raubenheimer and Ripley
# need a df where x = variable name, y, ymin, ymax are mean, mean-se, and mean+se of %∆ with CO2; group by H2OTmt
# also want to see %∆ with just water, so need another (third) group. That means groups are Tmt, and we plot ED and EW, plus AW as supplement. AD is the baseline.

# start with every obs of every variable (long format) and columns for Tmt. Then summarise y, ymin, and ymax with mutate and reference to AD column value

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
fig2_meanse <- final_df %>% 
  filter(Spp=="L") %>% 
  select(Tmt, totmass, rootshoot, Ht.mm..8, lwc, Anet, gs, WUE, tot_area, SRL, d13C) %>% 
  rename(totarea = tot_area) %>% 
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

fig2_meanse$standardD <- c(rep(fig2_meanse[1:10,1], 3))
fig2_meanse$standardD2 <- c(fig2_meanse[1:10,1], fig2_meanse[21:30,1], fig2_meanse[11:20,1])
fig2_meanse$standardW <- c(rep(fig2_meanse[1:10,2], 3))
fig2_meanse$standardW2 <- c(fig2_meanse[1:10,2], fig2_meanse[21:30,2], fig2_meanse[11:20,2])

fig2_meanse <- fig2_meanse %>% 
  mutate(across(!c("variable","y"), as.numeric)) %>% 
  # mutate(ED1 = (ED - standardD)*100/standardD) %>%
  # mutate(EW1 = (EW - standardW)*100/standardW) # %>% 
  mutate(ED1 = (ED - standardD2)*100/standardD2) %>%
  mutate(EW1 = (EW - standardW2)*100/standardW2)

  #  mutate_if(is.numeric, list(pct_chg = (AD - .x)*100/AD)) %>% View()
# get Tmt to be its own column with pivot_longer

fig2_meanse %>% 
  pivot_longer(cols=c("ED1","EW1"), names_to="Tmt", values_to="value") %>% 
  select(variable, y, Tmt, value) %>% 
  pivot_wider(names_from = "y", values_from = "value") %>% 
ggplot() +
  geom_pointrange(aes(x=variable, y=y, ymin=ymin, ymax=ymax, group=Tmt, color=Tmt), size=1.2, linewidth=1.2, position=position_dodge(width=0.2)) + scale_color_manual(values=c("red","blue")) +
  ggtitle("Quercus wislizenii (live oak)") +
  ylab("% change with eCO2") + xlab("Plant Response") +
  theme_classic(base_size = 20) + geom_abline(color= "red", linetype="dashed", slope = 0, intercept= 0)
  

# now for V!
fig2_meanseV <- final_df %>% 
  filter(Spp=="V") %>% 
  select(Tmt, totmass, rootshoot, Ht.mm..8, lwc, Anet, gs, WUE, tot_area, SRL, d13C) %>% 
  rename(totarea = tot_area) %>% 
  group_by(Tmt) %>% 
  summarise_if(is.numeric, list(y=mean_narm, ymin=minus_se, ymax=plus_se)) %>% 
  t() %>% data.frame()
# this is good! I want the columns to be the Tmts, and I want to move rownames into 2 columns: variable and y*

fig2_meanseV <- fig2_meanseV[-1,]
fig2_meanseV[,5] <- rownames(fig2_meanseV)
colnames(fig2_meanseV) <- c("AD","AW","ED","EW","var_y")

fig2_meanseV <- separate(fig2_meanseV, var_y, into = c("variable","y"), sep="_")
# still need to percentify the tmt columns with reference to AD

fig2_meanseV$standardD <- c(rep(fig2_meanseV[1:10,1], 3))
fig2_meanseV$standardD2 <- c(fig2_meanseV[1:10,1], fig2_meanseV[21:30,1], fig2_meanseV[11:20,1])
fig2_meanseV$standardW <- c(rep(fig2_meanseV[1:10,2], 3))
fig2_meanseV$standardW2 <- c(fig2_meanseV[1:10,2], fig2_meanseV[21:30,2], fig2_meanseV[11:20,2])

fig2_meanseV <- fig2_meanseV %>% 
  mutate(across(!c("variable","y"), as.numeric)) %>% 
  # mutate(ED1 = (ED - standardD)*100/standardD) %>%
  # mutate(EW1 = (EW - standardW)*100/standardW) # %>% 
  mutate(ED1 = (ED - standardD2)*100/standardD2) %>%
  mutate(EW1 = (EW - standardW2)*100/standardW2)

#  mutate_if(is.numeric, list(pct_chg = (AD - .x)*100/AD)) %>% View()
# get Tmt to be its own column with pivot_longer

fig2_meanseV %>% 
  pivot_longer(cols=c("ED1","EW1"), names_to="Tmt", values_to="value") %>% 
  select(variable, y, Tmt, value) %>% 
  pivot_wider(names_from = "y", values_from = "value") %>% 
  ggplot() +
  geom_pointrange(aes(x=variable, y=y, ymin=ymin, ymax=ymax, group=Tmt, color=Tmt), size=1.2, linewidth=1.2, position=position_dodge(width=0.2)) + scale_color_manual(values=c("red","blue")) +
  ggtitle("Quercus lobata (valley oak)") +
  theme_classic(base_size = 20) + geom_abline(color= "red", linetype="dashed", slope = 0, intercept= 0)

# you get the mean and se for the tmt groups, then you normalize y ymin and ymax with reference to AD mean
