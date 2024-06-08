# Monday 5/13/24
# Ch 3 Results
# AKA Final Round Final Round, Best of Three
library(car)
# load the data here after preparing in Ch3_analysis
# preparing involves filter outliers and ensuring normality
# Step 0: check data for normality and normality of residuals (dharma package to check residuals)
# wait! have to do this by spp
# try transformed data # check SWC by eCO2 to see if there's a feedback # specific root length
# LiCOR_df.
hist((LiCOR_df.[LiCOR_df.$Spp == "V",]$Photo.y), breaks = 2*sqrt(nrow(LiCOR_df.)))
qqPlot(LiCOR_df.[LiCOR_df.$Spp == "V",]$Photo.y) 
hist((LiCOR_df.[LiCOR_df.$Spp == "L",]$Photo.y), breaks = 2*sqrt(nrow(LiCOR_df.)))
qqPlot((LiCOR_df.[LiCOR_df.$Spp == "L",]$Photo.y)) # pretty ok by spp!
# Photo.y ok by species

hist(log(LiCOR_df.[LiCOR_df.$Spp=="V",]$Cond.y))
qqPlot(log(LiCOR_df.[LiCOR_df.$Spp=="V",]$Cond.y))
hist(log(LiCOR_df.[LiCOR_df.$Spp=="L",]$Cond.y))
qqPlot(log(LiCOR_df.[LiCOR_df.$Spp=="L",]$Cond.y))
# Cond.y ok if logged

LiCOR_df. %>% 
  filter(WUE.350 < 550) %>% 
  filter(Spp=="L") %>% dplyr::select(WUE.350) %>% unlist() %>% as.numeric() %>% sqrt() %>% 
  # hist(breaks = sqrt(nrow(LiCOR_df.)))
  qqPlot()
LiCOR_df. %>% 
    filter(Spp=="V") %>% dplyr::select(WUE.350) %>% unlist() %>% as.numeric() %>% sqrt() %>% 
  #  hist(breaks = sqrt(nrow(LiCOR_df.)))
     qqPlot()
# need to filter WUE.350 < 550; ok if sqrt
  
# biomass2.
hist(sqrt(biomass2.[biomass2.$Spp=="L",]$totmass), breaks=2*sqrt(nrow(biomass2.))) # outlier, ok
qqPlot(sqrt(biomass2.[biomass2.$Spp=="V",]$totmass)) 
qqPlot(sqrt(biomass2.[biomass2.$Spp=="L",]$totmass)) 
# outlier for totmass in L; ok with sqrt

hist((biomass2.[biomass2.$Spp=="L",]$rootshoot), breaks=2*sqrt(nrow(biomass2.))) 
qqPlot((biomass2.[biomass2.$Spp=="L",]$rootshoot)) 
qqPlot((biomass2.[biomass2.$Spp=="V",]$rootshoot)) 
# rootshoot ok

hist(biomass2.[biomass2.$Spp=="V",]$lwc, breaks=2*sqrt(nrow(biomass2.))) 
qqPlot(biomass2.[biomass2.$Spp=="V",]$lwc)
qqPlot(biomass2.[biomass2.$Spp=="L",]$lwc)
# lwc fine by spp

library(caret)
# check leaf area variables here
lai. %>% 
  filter(Spp=="V") %>% 
#  dplyr::select(avg_area) %>% # log
 # dplyr::select(perim_per_A) %>% # < 1 and log
  # filter(SLA < 12000) %>% # L has outlier > 12000 but this is normal for V; ok otherwise
  #dplyr::select(tot_area) %>%
  dplyr::select(SLA) %>% 
  unlist() %>% 
  as.numeric() %>%
#  BoxCoxTrans(na.rm=TRUE) %>% predict(newdata=unlist(lai.[lai$Spp=="V",][,"avg_area"])) %>% 
 # log() %>% 
 #sqrt() %>% 
#  hist(breaks = 2*sqrt(nrow(lai.)))
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


### combine into one giant, all-inclusive, transformed dataset
# one_over_var <- function(x){
#   1/(sd(x, na.rm=TRUE)^2)
# }
one_over_se <- function(x){
  1/(sd(x, na.rm=TRUE)/sqrt(length(x[!is.na(x)])))
}

trans_df <- biomass2. %>% 
  dplyr::select(Plot, Spp, Code, totmass, rootshoot, lwc, CO2, meanSWC) %>% 
  left_join(LiCOR_df.[,c("Code","Photo.y","Cond.y", "WUE.350")], by = "Code") %>% 
  left_join(lai.[,c("Code","avg_area","perim_per_A","tot_area", "SLA")], by = "Code") %>% 
  left_join(rootimage.[,c("Code","SRL")], by = "Code") %>% 
  left_join(SIF.[,c("Code","d13C")], by = "Code") %>% 
  mutate(logCond.y = log(Cond.y), sqrtWUE = sqrt(WUE.350), sqrtTotmass = sqrt(totmass), logAvg_area = log(avg_area), logPerimArea = log(perim_per_A), sqrtTotArea = sqrt(tot_area), logSRL = log(SRL)) %>%
  group_by(Plot, Spp) %>% 
  dplyr::mutate(n = n()) %>% 
#  mutate_at(c("sqrtTotmass", "rootshoot", "lwc", "logAvg_area", "logPerimArea", "sqrtTotArea", "logSRL", "d13C"), list(oov=one_over_se))  %>% 
  mutate_at(c("sqrtTotmass", "rootshoot", "lwc", "logAvg_area", "logPerimArea", "sqrtTotArea", "SLA", "logSRL", "d13C", "Photo.y", "logCond.y", "sqrtWUE"), list(oov=one_over_se))  %>% 
#  ungroup() %>% 
  # left_join(lookup, by = "Plot") %>% 
  # group_by(Spp, Tmt) %>% 
  # mutate_at(c("Photo.y", "logCond.y", "sqrtWUE"), list(oov=one_over_se))  %>% 
  # ungroup() %>% 
  # group_by(Plot, Spp) %>% 
  dplyr::summarise(across(where(is.numeric), ~ mean(.x, na.rm=TRUE))) %>% 
  ungroup() %>% 
  left_join(lookup, by = "Plot") %>% 
  mutate(H2OTmt = substr(Tmt,2,2), CO2Tmt = substr(Tmt,1,1)) 
# for plot 3, L has n = 1, but measurements for all data types. Same for plot 16, V
# assign it a weight based on the variance of all other points from its treatment group (AD):
#write.csv(trans_df, "QuailFACE_plotmeans.csv")

plot3L_vars <- biomass2. %>% 
  dplyr::select(Plot, Spp, Code, totmass, rootshoot, lwc, CO2, meanSWC) %>% 
  left_join(LiCOR_df.[,c("Code","Photo.y","Cond.y", "WUE.350")], by = "Code") %>% 
  left_join(lai.[,c("Code","avg_area","perim_per_A","tot_area", "SLA")], by = "Code") %>% 
  left_join(rootimage.[,c("Code","SRL")], by = "Code") %>% 
  left_join(SIF.[,c("Code","d13C")], by = "Code") %>% 
  mutate(logCond.y = log(Cond.y), sqrtWUE = sqrt(WUE.350), sqrtTotmass = sqrt(totmass), logAvg_area = log(avg_area), logPerimArea = log(perim_per_A), sqrtTotArea = sqrt(tot_area), logSRL = log(SRL)) %>%
  left_join(lookup, by = "Plot") %>% 
  group_by(Spp, Tmt) %>% 
  summarise_at(c("sqrtTotmass", "rootshoot", "lwc", "logAvg_area", "logPerimArea", "sqrtTotArea", "SLA", "logSRL", "d13C", "Photo.y", "logCond.y", "sqrtWUE"), list(oov=one_over_se)) %>% 
  filter(Spp=="L" & Tmt == "AD") %>% 
  unlist()
as.vector(as.numeric(unlist(plot3L_vars[3:14])))
# add this to 1/var columns in row for plot 3, then do next step to scale 1/var and get weights
# trans_df[trans_df$Plot=="3" & trans_df$Spp=="L",24:34] <- list(6.663002e-01, 2.655873e+01, 3.921653e-02, 3.963671e-02, 2.706536e+00, 9.376510e-02, 1.648682e+01, 3.025422e+01, 5.375108e-04, 4.618791e+00, 9.799790e+00)
trans_df[trans_df$Plot=="3" & trans_df$Spp=="L",25:36] <- list(3.65400837, 20.66696664,  0.97963889,  8.66588962, 12.88699028,  0.09953649, 0.005829665, 8.33628397,  3.89290435,  0.54785341,  4.48143137,  1.00380582)

# same for plot 16, V
plot16V_vars <- biomass2. %>% 
  dplyr::select(Plot, Spp, Code, totmass, rootshoot, lwc, CO2, meanSWC) %>% 
  left_join(LiCOR_df.[,c("Code","Photo.y","Cond.y", "WUE.350")], by = "Code") %>% 
  left_join(lai.[,c("Code","avg_area","perim_per_A","tot_area", "SLA")], by = "Code") %>% 
  left_join(rootimage.[,c("Code","SRL")], by = "Code") %>% 
  left_join(SIF.[,c("Code","d13C")], by = "Code") %>% 
  mutate(logCond.y = log(Cond.y), sqrtWUE = sqrt(WUE.350), sqrtTotmass = sqrt(totmass), logAvg_area = log(avg_area), logPerimArea = log(perim_per_A), sqrtTotArea = sqrt(tot_area), logSRL = log(SRL)) %>%
  left_join(lookup, by = "Plot") %>% 
  group_by(Spp, Tmt) %>% 
  summarise_at(c("sqrtTotmass", "rootshoot", "lwc", "logAvg_area", "logPerimArea", "sqrtTotArea", "SLA", "logSRL", "d13C", "Photo.y", "logCond.y", "sqrtWUE"), list(oov=one_over_se)) %>% 
  filter(Spp=="V" & Tmt == "AD") %>% 
  unlist()
as.vector(as.numeric(unlist(plot16V_vars[3:14])))
# trans_df[trans_df$Plot=="16" & trans_df$Spp=="V",24:34] <- list(9.917998e+00, 8.742674e-01, 7.279417e-03, 7.063863e-02, 1.519038e+02, 1.883699e-01, 1.258911e+00, 5.279206e+00, 6.309727e-03, 1.165876e+02, 1.134142e+01)
trans_df[trans_df$Plot=="16" & trans_df$Spp=="V",25:36] <- list(9.5139324, 2.8393556, 0.2239555, 2.3770735, 5.6281224, 0.1268094, 0.002696064, 9.2005139, 4.2598168, 0.3804126, 7.7273218, 0.7254999)

# need to calculate the weights AFTER filtering for species
# now add the weights; note that the combined weights will include variables not in the final model
## !!! trying 1/se to see if it fixes the weighting issue; "oov" is now "one over SE", will rename if it works
## addendum; 1/se is better but I also want to pool Tmt groups for LiCOR variables
trans_dfwV <- trans_df %>% 
  filter(Spp=="V") %>% 
  mutate(sqrtTotmass_w = sqrtTotmass_oov/(sum(trans_df$sqrtTotmass_oov, na.rm = T)),
         rootshoot_w = rootshoot_oov/(sum(trans_df$rootshoot_oov, na.rm = T)),
         lwc_w = lwc_oov/(sum(trans_df$lwc_oov, na.rm = T)),
         logAvg_area_w = logAvg_area_oov/(sum(trans_df$logAvg_area_oov, na.rm = T)),
         logPerimArea_w = logPerimArea_oov/(sum(trans_df$logPerimArea_oov, na.rm = T)),
         sqrtTotArea_w = sqrtTotArea_oov/(sum(trans_df$sqrtTotArea_oov, na.rm = T)),
         SLA_w = SLA_oov/(sum(trans_df$SLA_oov, na.rm = T)),
         logSRL_w = logSRL_oov/(sum(trans_df$logSRL_oov, na.rm = T)),
         d13C_w = d13C_oov/sum(trans_df$d13C_oov, na.rm = T),
         Photo.y_w = Photo.y_oov/(sum(trans_df$Photo.y_oov, na.rm = T)),
         logCond.y_w = logCond.y_oov/(sum(trans_df$logCond.y_oov, na.rm = T)),
         sqrtWUE_w = sqrtWUE_oov/(sum(trans_df$sqrtWUE_oov, na.rm =T))) %>% 
    mutate(all_w = sqrtTotmass_w+rootshoot_w+lwc_w+Photo.y_w+logCond.y_w+sqrtWUE_w+logAvg_area_w+logPerimArea_w+sqrtTotArea_w+SLA_w+logSRL_w+d13C_w) %>% 
 # mutate(all_w = logAvg_area_w+logPerimArea_w+Photo.y_w+logCond.y_w) %>% 
  mutate(all_w = all_w/sum(all_w, na.rm=T)) %>% 
   mutate(quad_w = (sqrtTotmass_w^2)+(rootshoot_w^2)+(lwc_w^2)+(Photo.y_w^2)+(logCond.y_w^2)+(sqrtWUE_w^2)+(logAvg_area_w^2)+(logPerimArea_w^2)+(sqrtTotArea_w^2)+(SLA_w^2)+(logSRL_w^2)+(d13C_w^2)) %>% 
  #mutate(quad_w = (Photo.y_w^2)+(logCond.y_w^2)+(logAvg_area_w^2)+(logPerimArea_w^2)) %>% 
  mutate(quad_w = quad_w/sum(quad_w, na.rm=T)) 

trans_dfwL <- trans_df %>% 
  filter(Spp=="L") %>% 
  mutate(sqrtTotmass_w = sqrtTotmass_oov/(sum(trans_df$sqrtTotmass_oov, na.rm = T)),
         rootshoot_w = rootshoot_oov/(sum(trans_df$rootshoot_oov, na.rm = T)),
         lwc_w = lwc_oov/(sum(trans_df$lwc_oov, na.rm = T)),
         logAvg_area_w = logAvg_area_oov/(sum(trans_df$logAvg_area_oov, na.rm = T)),
         logPerimArea_w = logPerimArea_oov/(sum(trans_df$logPerimArea_oov, na.rm = T)),
         sqrtTotArea_w = sqrtTotArea_oov/(sum(trans_df$sqrtTotArea_oov, na.rm = T)),
         SLA_w = SLA_oov/(sum(trans_df$SLA_oov, na.rm = T)),
         logSRL_w = logSRL_oov/(sum(trans_df$logSRL_oov, na.rm = T)),
         d13C_w = d13C_oov/sum(trans_df$d13C_oov, na.rm = T),
         Photo.y_w = Photo.y_oov/(sum(trans_df$Photo.y_oov, na.rm = T)),
         logCond.y_w = logCond.y_oov/(sum(trans_df$logCond.y_oov, na.rm = T)),
         sqrtWUE_w = sqrtWUE_oov/(sum(trans_df$sqrtWUE_oov, na.rm =T))) %>% 
  mutate(all_w = sqrtTotmass_w+rootshoot_w+lwc_w+Photo.y_w+logCond.y_w+sqrtWUE_w+logAvg_area_w+logPerimArea_w+sqrtTotArea_w+logSRL_w+d13C_w) %>% 
  #       mutate(all_w = logAvg_area_w+logSRL_w+Photo.y_w+logCond.y_w) %>% 
  mutate(all_w = all_w/sum(all_w, na.rm=T)) %>% 
   mutate(quad_w = (sqrtTotmass_w^2)+(rootshoot_w^2)+(lwc_w^2)+(Photo.y_w^2)+(logCond.y_w^2)+(sqrtWUE_w^2)+(logAvg_area_w^2)+(logPerimArea_w^2)+(sqrtTotArea_w^2)+(SLA_w^2)+(logSRL_w^2)+(d13C_w^2)) %>% 
  #mutate(quad_w = (Photo.y_w^2)+(logCond.y_w^2)+(logAvg_area_w^2)+(logSRL_w^2)) %>% 
  mutate(quad_w = quad_w/sum(quad_w, na.rm=T)) 

library(ggcorrplot)
# check correlation between variables
cor(trans_dfwL[, c("sqrtTotmass", "rootshoot", "lwc", "logAvg_area", "logPerimArea", "sqrtTotArea", "SLA", "logSRL", "d13C", "Photo.y", "logCond.y", "sqrtWUE")], use="pairwise.complete.obs") %>% 
  ggcorrplot()

response_list <- c("sqrtTotmass", "rootshoot", "lwc", "logAvg_area", "logPerimArea", "sqrtTotArea", "SLA", "logSRL", "d13C", "Photo.y", "logCond.y", "sqrtWUE")
# the weights come from inverse variance; check for outliers # do this tomorrow
trans_df_full <- biomass2. %>% 
#  filter(Spp =="V") %>% 
  dplyr::select(Plot, Spp, Code, totmass, rootshoot, lwc, CO2, meanSWC) %>% 
  left_join(LiCOR_df.[,c("Code","Photo.y","Cond.y", "WUE.350")], by = "Code") %>% 
  left_join(lai.[,c("Code","avg_area","perim_per_A","tot_area", "SLA")], by = "Code") %>% 
  left_join(rootimage.[,c("Code","SRL")], by = "Code") %>% 
  left_join(SIF.[,c("Code","d13C")], by = "Code") %>% 
  mutate(logCond.y = log(Cond.y), sqrtWUE = sqrt(WUE.350), sqrtTotmass = sqrt(totmass), logAvg_area = log(avg_area), logPerimArea = log(perim_per_A), sqrtTotArea = sqrt(tot_area), logSRL = log(SRL)) %>%
  group_by(Plot, Spp) %>% 
  mutate(n = n()) %>% 
  left_join(lookup, by = "Plot") %>% 
  mutate(H2OTmt = substr(Tmt,2,2), CO2Tmt = substr(Tmt,1,1)) 
#write.csv(trans_df_full, "QuailFACE_all.csv")

trans_df_full %>% filter(Spp=="V") %>% 
  ggplot() +
  geom_point(aes(x=Plot, y=SLA, color=Tmt)) + 
  scale_color_manual(values = c("pink", "lightblue", "red", "blue")) +
  labs(title = paste(response_list[7], "Individual Responses per Plot"))

# run the three plots (weighted, unweighted, raw data) for each variable, each spp
library(gridExtra)
library(ggeffects)
## Spp == V
grid.arrange( # total mass
  ggpredict(lm(sqrtTotmass~CO2*H2OTmt , data=trans_dfwV, weights=sqrtTotmass_w), terms=c("CO2","H2OTmt"))%>% 
    plot(rawdata = TRUE, ci = TRUE, colors=c("blue","red")),
  ggpredict(lm(sqrtTotmass ~ CO2*H2OTmt , data = trans_dfwV), terms=c("CO2","H2OTmt")) %>% 
    plot(rawdata = TRUE, ci = TRUE, colors=c("blue","red")),
  ggplot(trans_dfwV, aes(x=CO2, y=sqrtTotmass, group = H2OTmt)) + geom_point(aes(group = H2OTmt, color = H2OTmt, size=sqrtTotmass_w)) + scale_color_manual(values = c("red", "blue")) , nrow=3 )
summary(lm(sqrtTotmass~CO2*H2OTmt , data=trans_dfwV, weights=sqrtTotmass_w)) # NS

grid.arrange( # root:shoot
  ggpredict(lm(rootshoot~CO2*H2OTmt , data=trans_dfwV, weights=rootshoot_w), terms=c("CO2","H2OTmt"))%>% 
    plot(rawdata = TRUE, ci = TRUE, colors=c("blue","red")),
  ggpredict(lm(rootshoot ~ CO2*H2OTmt , data = trans_dfwV), terms=c("CO2","H2OTmt")) %>% 
    plot(rawdata = TRUE, ci = TRUE, colors=c("blue","red")),
  ggplot(trans_dfwV, aes(x=CO2, y=rootshoot, group = H2OTmt)) + geom_point(aes(group = H2OTmt, color = H2OTmt, size=rootshoot_w)) + scale_color_manual(values = c("red", "blue")) , nrow=3 )
summary(lm(rootshoot~CO2*H2OTmt , data=trans_dfwV, weights=rootshoot_w)) # NS

grid.arrange( # lwc
  ggpredict(lm(lwc~CO2*H2OTmt , data=trans_dfwV, weights=lwc_w), terms=c("CO2","H2OTmt"))%>% 
    plot(rawdata = TRUE, ci = TRUE, colors=c("blue","red")),
  ggpredict(lm(lwc ~ CO2*H2OTmt , data = trans_dfwV), terms=c("CO2","H2OTmt")) %>% 
    plot(rawdata = TRUE, ci = TRUE, colors=c("blue","red")),
  ggplot(trans_dfwV, aes(x=CO2, y=lwc, group = H2OTmt)) + geom_point(aes(group = H2OTmt, color = H2OTmt, size=lwc_w)) + scale_color_manual(values = c("red", "blue")) , nrow=3 )
summary(lm(lwc~CO2*H2OTmt , data=trans_dfwV, weights=lwc_w)) # NS

grid.arrange( # logAvg_area
  ggpredict(lm(logAvg_area~CO2*H2OTmt , data=trans_dfwV, weights=logAvg_area_w), terms=c("CO2","H2OTmt"))%>% 
    plot(rawdata = TRUE, ci = TRUE, colors=c("blue","red")),
  ggpredict(lm(logAvg_area ~ CO2*H2OTmt , data = trans_dfwV), terms=c("CO2","H2OTmt")) %>% 
    plot(rawdata = TRUE, ci = TRUE, colors=c("blue","red")),
  ggplot(trans_dfwV, aes(x=CO2, y=logAvg_area, group = H2OTmt)) + geom_point(aes(group = H2OTmt, color = H2OTmt, size=logAvg_area_w)) + scale_color_manual(values = c("red", "blue")) , nrow=3 )
summary(lm(logAvg_area~CO2*H2OTmt , data=trans_dfwV, weights=logAvg_area_w)) 
# log-transformed average leaf area: CO2 p < .00065, H2O p < .005, intx < .0095

grid.arrange( # logPerimArea
  ggpredict(lm(logPerimArea~CO2*H2OTmt , data=trans_dfwV, weights=logPerimArea_w), terms=c("CO2","H2OTmt"))%>% 
    plot(rawdata = TRUE, ci = TRUE, colors=c("blue","red")),
  ggpredict(lm(logPerimArea ~ CO2*H2OTmt , data = trans_dfwV), terms=c("CO2","H2OTmt")) %>% 
    plot(rawdata = TRUE, ci = TRUE, colors=c("blue","red")),
  ggplot(trans_dfwV, aes(x=CO2, y=logPerimArea, group = H2OTmt)) + geom_point(aes(group = H2OTmt, color = H2OTmt, size=logPerimArea_w)) + scale_color_manual(values = c("red", "blue")) , nrow=3 )
summary(lm(logPerimArea~CO2*H2OTmt , data=trans_dfwV, weights=logPerimArea_w))
# log-transformed perimeter-per-area: CO2 p < 0.07

grid.arrange( # sqrtTotArea
  ggpredict(lm(sqrtTotArea~CO2*H2OTmt , data=trans_dfwV, weights=sqrtTotArea_w), terms=c("CO2","H2OTmt"))%>% 
    plot(rawdata = TRUE, ci = TRUE, colors=c("blue","red")),
  ggpredict(lm(sqrtTotArea ~ CO2*H2OTmt , data = trans_dfwV), terms=c("CO2","H2OTmt")) %>% 
    plot(rawdata = TRUE, ci = TRUE, colors=c("blue","red")),
  ggplot(trans_dfwV, aes(x=CO2, y=sqrtTotArea, group = H2OTmt)) + geom_point(aes(group = H2OTmt, color = H2OTmt, size=sqrtTotArea_w)) + scale_color_manual(values = c("red", "blue")) , nrow=3 )
summary(lm(sqrtTotArea~CO2*H2OTmt , data=trans_dfwV, weights=sqrtTotArea_w)) # NS

grid.arrange( # logSRL
  ggpredict(lm(logSRL~CO2*H2OTmt , data=trans_dfwV, weights=logSRL_w), terms=c("CO2","H2OTmt"))%>% 
    plot(rawdata = TRUE, ci = TRUE, colors=c("blue","red")),
  ggpredict(lm(logSRL ~ CO2*H2OTmt , data = trans_dfwV), terms=c("CO2","H2OTmt")) %>% 
    plot(rawdata = TRUE, ci = TRUE, colors=c("blue","red")),
  ggplot(trans_dfwV, aes(x=CO2, y=logSRL, group = H2OTmt)) + geom_point(aes(group = H2OTmt, color = H2OTmt, size=logSRL_w)) + scale_color_manual(values = c("red", "blue")) , nrow=3 )
summary(lm(logSRL~CO2*H2OTmt , data=trans_dfwV, weights=logSRL_w)) # NS

grid.arrange( # d13C
  ggpredict(lm(d13C~CO2*H2OTmt , data=trans_dfwV, weights=d13C_w), terms=c("CO2","H2OTmt"))%>% 
    plot(rawdata = TRUE, ci = TRUE, colors=c("blue","red")),
  ggpredict(lm(d13C ~ CO2*H2OTmt , data = trans_dfwV), terms=c("CO2","H2OTmt")) %>% 
    plot(rawdata = TRUE, ci = TRUE, colors=c("blue","red")),
  ggplot(trans_dfwV, aes(x=CO2, y=d13C, group = H2OTmt)) + geom_point(aes(group = H2OTmt, color = H2OTmt, size=d13C_w)) + scale_color_manual(values = c("red", "blue")) , nrow=3 )
summary(lm(d13C~CO2*H2OTmt , data=trans_dfwV, weights=d13C_w)) 
# d13C: CO2 p < .015

grid.arrange( # Photo.y
ggpredict(lm(Photo.y ~ CO2*H2OTmt , data = trans_dfwV, weights = Photo.y_w), terms=c("CO2","H2OTmt")) %>% 
  plot(rawdata = TRUE, ci = TRUE, colors=c("blue","red")) + labs(title = "A(net) weights"),
ggpredict(lm(Photo.y ~ CO2*H2OTmt , data = trans_dfwV), terms=c("CO2","H2OTmt")) %>% 
  plot(rawdata = TRUE, ci = TRUE, colors=c("blue","red")) + labs(title = "Unweighted A(net)"),
ggplot(trans_dfwV, aes(x=CO2, y=Photo.y, group = H2OTmt)) + geom_point(aes(group = H2OTmt, color = H2OTmt, size = Photo.y_w)) + scale_color_manual(values = c("red", "blue")) + labs(title = "A(net) Plot Means Sized by Inverse-SE Weight") + geom_text(data = trans_dfwV, aes(x=CO2, y=Photo.y+1.2, label=Plot)), nrow=3 )
summary(lm(Photo.y~CO2*H2OTmt , data=trans_dfwV, weights=Photo.y_w))
# Photo.y: CO2 p < .04, H2O p < .035

grid.arrange( # Cond.y
  ggpredict(lm(logCond.y ~ CO2*H2OTmt , data=trans_dfwV, weights=logCond.y_w), terms=c("CO2","H2OTmt")) %>% 
    plot(rawdata = TRUE, ci = TRUE, colors=c("blue","red")) + labs(title = "g(s) weights"),
  ggpredict(lm(logCond.y ~ CO2*H2OTmt , data = trans_dfwV), terms=c("CO2","H2OTmt")) %>% 
    plot(rawdata = TRUE, ci = TRUE, colors=c("blue","red"))+ labs(title = "Unweighted g(s)"),
  ggplot(trans_dfwV, aes(x=CO2, y=logCond.y, group = H2OTmt)) + geom_point(aes(group = H2OTmt, color = H2OTmt, size=logCond.y_w)) + scale_color_manual(values = c("red", "blue")) + labs(title = "g(s) Plot Means Sized by Inverse-SE Weight") + geom_text(data = trans_dfwV, aes(x=CO2, y=logCond.y+0.2, label=Plot)), nrow=3 )
summary(lm(logCond.y~CO2*H2OTmt , data=trans_dfwV, weights=logCond.y_w))
# log-transformed stomatal conductance: H2O p < .04

grid.arrange( # sqrtWUE
  ggpredict(lm(sqrtWUE ~ CO2*H2OTmt , data=trans_dfwV, weights=sqrtWUE_w), terms=c("CO2","H2OTmt")) %>% 
    plot(rawdata = TRUE, ci = TRUE, colors=c("blue","red")) + labs(title = "g(s) weights"),
  ggpredict(lm(sqrtWUE ~ CO2*H2OTmt , data = trans_dfwV), terms=c("CO2","H2OTmt")) %>% 
    plot(rawdata = TRUE, ci = TRUE, colors=c("blue","red"))+ labs(title = "Unweighted g(s)"),
  ggplot(trans_dfwV, aes(x=CO2, y=sqrtWUE, group = H2OTmt)) + geom_point(aes(group = H2OTmt, color = H2OTmt, size=sqrtWUE_w)) + scale_color_manual(values = c("red", "blue")) + labs(title = "g(s) Plot Means Sized by Inverse-SE Weight") + geom_text(data = trans_dfwV, aes(x=CO2, y=sqrtWUE+0.2, label=Plot)), nrow=3 )
summary(lm(sqrtWUE~CO2*H2OTmt , data=trans_dfwV, weights=sqrtWUE_w)) # NS

grid.arrange( # SLA
  ggpredict(lm(SLA ~ CO2*H2OTmt , data=trans_dfwV, weights=SLA_w), terms=c("CO2","H2OTmt")) %>% 
    plot(rawdata = TRUE, ci = TRUE, colors=c("blue","red")) + labs(title = "g(s) weights"),
  ggpredict(lm(SLA ~ CO2*H2OTmt , data = trans_dfwV), terms=c("CO2","H2OTmt")) %>% 
    plot(rawdata = TRUE, ci = TRUE, colors=c("blue","red"))+ labs(title = "Unweighted g(s)"),
  ggplot(trans_dfwV, aes(x=CO2, y=SLA, group = H2OTmt)) + geom_point(aes(group = H2OTmt, color = H2OTmt, size=SLA_w)) + scale_color_manual(values = c("red", "blue")) + labs(title = "g(s) Plot Means Sized by Inverse-SE Weight") + geom_text(data = trans_dfwV, aes(x=CO2, y=SLA+0.2, label=Plot)), nrow=3 )
summary(lm(SLA~CO2*H2OTmt , data=trans_dfwV, weights=SLA_w)) 

## Spp == L

grid.arrange( # total mass
  ggpredict(lm(sqrtTotmass~CO2*H2OTmt , data=trans_dfwL, weights=sqrtTotmass_w), terms=c("CO2","H2OTmt"))%>% 
    plot(rawdata = TRUE, ci = TRUE, colors=c("blue","red")),
  ggpredict(lm(sqrtTotmass ~ CO2*H2OTmt , data = trans_dfwL), terms=c("CO2","H2OTmt")) %>% 
    plot(rawdata = TRUE, ci = TRUE, colors=c("blue","red")),
  ggplot(trans_dfwL, aes(x=CO2, y=sqrtTotmass, group = H2OTmt)) + geom_point(aes(group = H2OTmt, color = H2OTmt, size=sqrtTotmass_w)) + scale_color_manual(values = c("red", "blue")) , nrow=3 )
summary(lm(sqrtTotmass~CO2*H2OTmt , data=trans_dfwL, weights=sqrtTotmass_w)) # NS

grid.arrange( # root:shoot
  ggpredict(lm(rootshoot~CO2*H2OTmt , data=trans_dfwL, weights=rootshoot_w), terms=c("CO2","H2OTmt"))%>% 
    plot(rawdata = TRUE, ci = TRUE, colors=c("blue","red")),
  ggpredict(lm(rootshoot ~ CO2*H2OTmt , data = trans_dfwL), terms=c("CO2","H2OTmt")) %>% 
    plot(rawdata = TRUE, ci = TRUE, colors=c("blue","red")),
  ggplot(trans_dfwL, aes(x=CO2, y=rootshoot, group = H2OTmt)) + geom_point(aes(group = H2OTmt, color = H2OTmt, size=rootshoot_w)) + scale_color_manual(values = c("red", "blue")) , nrow=3 )
summary(lm(rootshoot~CO2*H2OTmt , data=trans_dfwL, weights=rootshoot_w)) # NS

grid.arrange( # lwc
  ggpredict(lm(lwc~CO2*H2OTmt , data=trans_dfwL, weights=lwc_w), terms=c("CO2","H2OTmt"))%>% 
    plot(rawdata = TRUE, ci = TRUE, colors=c("blue","red")),
  ggpredict(lm(lwc ~ CO2*H2OTmt , data = trans_dfwL), terms=c("CO2","H2OTmt")) %>% 
    plot(rawdata = TRUE, ci = TRUE, colors=c("blue","red")),
  ggplot(trans_dfwL, aes(x=CO2, y=lwc, group = H2OTmt)) + geom_point(aes(group = H2OTmt, color = H2OTmt, size=lwc_w)) + scale_color_manual(values = c("red", "blue")) , nrow=3 )
summary(lm(lwc~CO2*H2OTmt , data=trans_dfwL, weights=lwc_w)) # NS

grid.arrange( # logAvg_area
  ggpredict(lm(logAvg_area~CO2*H2OTmt , data=trans_dfwL, weights=logAvg_area_w), terms=c("CO2","H2OTmt"))%>% 
    plot(rawdata = TRUE, ci = TRUE, colors=c("blue","red")),
  ggpredict(lm(logAvg_area ~ CO2*H2OTmt , data = trans_dfwL), terms=c("CO2","H2OTmt")) %>% 
    plot(rawdata = TRUE, ci = TRUE, colors=c("blue","red")),
  ggplot(trans_dfwL, aes(x=CO2, y=logAvg_area, group = H2OTmt)) + geom_point(aes(group = H2OTmt, color = H2OTmt, size=logAvg_area_w)) + scale_color_manual(values = c("red", "blue")) , nrow=3 )
summary(lm(logAvg_area~CO2*H2OTmt , data=trans_dfwL, weights=logAvg_area_w)) 
# log-transformed average leaf area: CO2 p < .085, H2O p < .06, intx < .025

grid.arrange( # logPerimArea
  ggpredict(lm(logPerimArea~CO2*H2OTmt , data=trans_dfwL, weights=logPerimArea_w), terms=c("CO2","H2OTmt"))%>% 
    plot(rawdata = TRUE, ci = TRUE, colors=c("blue","red")),
  ggpredict(lm(logPerimArea ~ CO2*H2OTmt , data = trans_dfwL), terms=c("CO2","H2OTmt")) %>% 
    plot(rawdata = TRUE, ci = TRUE, colors=c("blue","red")),
  ggplot(trans_dfwL, aes(x=CO2, y=logPerimArea, group = H2OTmt)) + geom_point(aes(group = H2OTmt, color = H2OTmt, size=logPerimArea_w)) + scale_color_manual(values = c("red", "blue")) , nrow=3 )
summary(lm(logPerimArea~CO2*H2OTmt , data=trans_dfwL, weights=logPerimArea_w)) # NS

grid.arrange( # sqrtTotArea
  ggpredict(lm(sqrtTotArea~CO2*H2OTmt , data=trans_dfwL, weights=sqrtTotArea_w), terms=c("CO2","H2OTmt"))%>% 
    plot(rawdata = TRUE, ci = TRUE, colors=c("blue","red")),
  ggpredict(lm(sqrtTotArea ~ CO2*H2OTmt , data = trans_dfwL), terms=c("CO2","H2OTmt")) %>% 
    plot(rawdata = TRUE, ci = TRUE, colors=c("blue","red")),
  ggplot(trans_dfwL, aes(x=CO2, y=sqrtTotArea, group = H2OTmt)) + geom_point(aes(group = H2OTmt, color = H2OTmt, size=sqrtTotArea_w)) + scale_color_manual(values = c("red", "blue")) , nrow=3 )
summary(lm(sqrtTotArea~CO2*H2OTmt , data=trans_dfwL, weights=sqrtTotArea_w)) # NS

grid.arrange( # logSRL
  ggpredict(lm(logSRL~CO2*H2OTmt , data=trans_dfwL, weights=logSRL_w), terms=c("CO2","H2OTmt"))%>% 
    plot(rawdata = TRUE, ci = TRUE, colors=c("blue","red")),
  ggpredict(lm(logSRL ~ CO2*H2OTmt , data = trans_dfwL), terms=c("CO2","H2OTmt")) %>% 
    plot(rawdata = TRUE, ci = TRUE, colors=c("blue","red")),
  ggplot(trans_dfwL, aes(x=CO2, y=logSRL, group = H2OTmt)) + geom_point(aes(group = H2OTmt, color = H2OTmt, size=logSRL_w)) + scale_color_manual(values = c("red", "blue")) , nrow=3 )
summary(lm(logSRL~CO2*H2OTmt , data=trans_dfwL, weights=logSRL_w)) 
# log-transformed specific root length: CO2 p < .085

grid.arrange( # d13C
  ggpredict(lm(d13C~CO2*H2OTmt , data=trans_dfwL, weights=d13C_w), terms=c("CO2","H2OTmt"))%>% 
    plot(rawdata = TRUE, ci = TRUE, colors=c("blue","red")),
  ggpredict(lm(d13C ~ CO2*H2OTmt , data = trans_dfwL), terms=c("CO2","H2OTmt")) %>% 
    plot(rawdata = TRUE, ci = TRUE, colors=c("blue","red")),
  ggplot(trans_dfwL, aes(x=CO2, y=d13C, group = H2OTmt)) + geom_point(aes(group = H2OTmt, color = H2OTmt, size=d13C_w)) + scale_color_manual(values = c("red", "blue")) , nrow=3 )
summary(lm(d13C~CO2*H2OTmt , data=trans_dfwL, weights=d13C_w)) # NS

grid.arrange( # Photo.y
  ggpredict(lm(Photo.y ~ CO2*H2OTmt , data = trans_dfwL, weights = Photo.y_w), terms=c("CO2","H2OTmt")) %>% 
    plot(rawdata = TRUE, ci = TRUE, colors=c("blue","red")) + labs(title = "A(net) weights"),
  ggpredict(lm(Photo.y ~ CO2*H2OTmt , data = trans_dfwL), terms=c("CO2","H2OTmt")) %>% 
    plot(rawdata = TRUE, ci = TRUE, colors=c("blue","red")) + labs(title = "Unweighted A(net)"),
  ggplot(trans_dfwL, aes(x=CO2, y=Photo.y, group = H2OTmt)) + geom_point(aes(group = H2OTmt, color = H2OTmt, size = Photo.y_w)) + scale_color_manual(values = c("red", "blue")) + labs(title = "A(net) Plot Means Sized by Inverse-SE Weight") + geom_text(data = trans_dfwL, aes(x=CO2, y=Photo.y+1.2, label=Plot)), nrow=3 )
summary(lm(Photo.y~CO2*H2OTmt , data=trans_dfwL, weights=Photo.y_w))
# Photo.y: H2O p < .035

grid.arrange( # Cond.y
  ggpredict(lm(logCond.y ~ CO2*H2OTmt , data=trans_dfwL, weights=logCond.y_w), terms=c("CO2","H2OTmt")) %>% 
    plot(rawdata = TRUE, ci = TRUE, colors=c("blue","red")) + labs(title = "g(s) weights"),
  ggpredict(lm(logCond.y ~ CO2*H2OTmt , data = trans_dfwL), terms=c("CO2","H2OTmt")) %>% 
    plot(rawdata = TRUE, ci = TRUE, colors=c("blue","red"))+ labs(title = "Unweighted g(s)"),
  ggplot(trans_dfwL, aes(x=CO2, y=logCond.y, group = H2OTmt)) + geom_point(aes(group = H2OTmt, color = H2OTmt, size=logCond.y_w)) + scale_color_manual(values = c("red", "blue")) + labs(title = "g(s) Plot Means Sized by Inverse-SE Weight") + geom_text(data = trans_dfwL, aes(x=CO2, y=logCond.y+0.2, label=Plot)), nrow=3 )
summary(lm(logCond.y~CO2*H2OTmt , data=trans_dfwL, weights=logCond.y_w))
# log-transformed stomatal conductance: CO2 p < .08

grid.arrange( # sqrtWUE
  ggpredict(lm(sqrtWUE ~ CO2*H2OTmt , data=trans_dfwL, weights=sqrtWUE_w), terms=c("CO2","H2OTmt")) %>% 
    plot(rawdata = TRUE, ci = TRUE, colors=c("blue","red")) + labs(title = "g(s) weights"),
  ggpredict(lm(sqrtWUE ~ CO2*H2OTmt , data = trans_dfwL), terms=c("CO2","H2OTmt")) %>% 
    plot(rawdata = TRUE, ci = TRUE, colors=c("blue","red"))+ labs(title = "Unweighted g(s)"),
  ggplot(trans_dfwL, aes(x=CO2, y=sqrtWUE, group = H2OTmt)) + geom_point(aes(group = H2OTmt, color = H2OTmt, size=sqrtWUE_w)) + scale_color_manual(values = c("red", "blue")) + labs(title = "g(s) Plot Means Sized by Inverse-SE Weight") + geom_text(data = trans_dfwL, aes(x=CO2, y=sqrtWUE+0.2, label=Plot)), nrow=3 )
summary(lm(sqrtWUE~CO2*H2OTmt , data=trans_dfwL, weights=sqrtWUE_w)) # NS

grid.arrange( # SLA
  ggpredict(lm(SLA ~ CO2*H2OTmt , data=trans_dfwL, weights=SLA_w), terms=c("CO2","H2OTmt")) %>% 
    plot(rawdata = TRUE, ci = TRUE, colors=c("blue","red")) + labs(title = "g(s) weights"),
  ggpredict(lm(SLA ~ CO2*H2OTmt , data = trans_dfwL), terms=c("CO2","H2OTmt")) %>% 
    plot(rawdata = TRUE, ci = TRUE, colors=c("blue","red"))+ labs(title = "Unweighted g(s)"),
  ggplot(trans_dfwL, aes(x=CO2, y=SLA, group = H2OTmt)) + geom_point(aes(group = H2OTmt, color = H2OTmt, size=SLA_w)) + scale_color_manual(values = c("red", "blue")) + labs(title = "g(s) Plot Means Sized by Inverse-SE Weight") + geom_text(data = trans_dfwL, aes(x=CO2, y=SLA+0.2, label=Plot)), nrow=3 )
summary(lm(SLA~CO2*H2OTmt , data=trans_dfwL, weights=SLA_w))

# manova time
# I heard a MANOVA has more power, so long as your data is normally distributed, which I've already ensured
# now doing manova: transformed data, plot means, with weights 

# add the other seven variables in order here
trans_dfwV.cbind <- cbind(unlist(trans_dfwV$sqrtTotmass), unlist(trans_dfwV$rootshoot), unlist(trans_dfwV$lwc), unlist(trans_dfwV$logAvg_area), unlist(trans_dfwV$logPerimArea), unlist(trans_dfwV$sqrtTotArea), unlist(trans_dfwV$SLA), unlist(trans_dfwV$logSRL), unlist(trans_dfwV$d13C),unlist(trans_dfwV$Photo.y),unlist(trans_dfwV$logCond.y),unlist(trans_dfwV$sqrtWUE))

trans_dfwL.cbind <- cbind(unlist(trans_dfwL$sqrtTotmass), unlist(trans_dfwL$rootshoot), unlist(trans_dfwL$lwc), unlist(trans_dfwL$logAvg_area), unlist(trans_dfwL$logPerimArea), unlist(trans_dfwL$sqrtTotArea), unlist(trans_dfwL$SLA), unlist(trans_dfwL$logSRL), unlist(trans_dfwL$d13C),unlist(trans_dfwL$Photo.y),unlist(trans_dfwL$logCond.y),unlist(trans_dfwL$sqrtWUE))

# doing this for now with the weights calculated from all 12. After selecting a subset, I'll recalculate grand weights with just those variables
summary(manova(trans_dfwV.cbind ~ rescale(CO2)*rescale(meanSWC), trans_dfwV, weights = quad_w))
summary(manova(trans_dfwV.cbind[,c(4,5,8,10,11,7)] ~ rescale(CO2)*rescale(meanSWC), trans_dfwV))
summary(manova(trans_dfwV.cbind[,c(4,5,8,10,11,7)] ~ rescale(CO2)*rescale(meanSWC), trans_dfwV))
summary(manova(trans_dfwV.cbind[,c(4,5)] ~ rescale(CO2)*rescale(meanSWC), trans_dfwV))

summary(manova(trans_dfwL.cbind[,c(1,2,3,4,6,7,8)] ~ rescale(CO2)*rescale(meanSWC), trans_dfwL))
summary(manova(trans_dfwL.cbind[,c(1,2,3,4,6,7)] ~ rescale(CO2)*rescale(meanSWC), trans_dfwL))
summary(manova(trans_dfwL.cbind[,c(1,2,3,4,6)] ~ rescale(CO2)*rescale(meanSWC), trans_dfwL))

# doing this for now with the weights calculated from all 11. After selecting a subset, I'll recalculate grand weights with just those variables
# try these for V: logAvg_area, logPerimArea, Photo.y, Cond.y
summary(manova(trans_dfwV.cbind[,c(4,5,9,10)] ~ CO2Tmt*H2OTmt, trans_dfwV, weights = quad_w)) # CO2*, H2O*
summary(manova(trans_dfwV.cbind[,c(4,5,9,10)] ~ CO2Tmt*H2OTmt, trans_dfwV, weights = all_w)) # CO2*
summary(manova(trans_dfwV.cbind[,c(4,5,9,10)] ~ CO2Tmt*H2OTmt, trans_dfwV)) # H2O**
summary(manova(trans_dfwV.cbind[,c(5,9,10)] ~ CO2Tmt*H2OTmt, trans_dfwV, weights = quad_w)) # CO2**, H2O**
summary(manova(trans_dfwV.cbind[,c(4,5)] ~ CO2Tmt*H2OTmt, trans_dfwV, weights = quad_w))

# these for L: logAvg_area, logSRL, Photo.y, logCond.y
summary(manova(trans_dfwL.cbind[,c(4,7,9,10)] ~ CO2Tmt*H2OTmt, trans_dfwL, weights = quad_w)) # CO2*,H2O***,intx .
summary(manova(trans_dfwL.cbind[,c(4,7,9,10)] ~ CO2Tmt*H2OTmt, trans_dfwL, weights = all_w)) # H2O ***
summary(manova(trans_dfwL.cbind[,c(4,7,9,10)] ~ CO2Tmt*H2OTmt, trans_dfwL)) # H2O***
summary(manova(trans_dfwL.cbind[,c(5,9,10)] ~ CO2Tmt*H2OTmt, trans_dfwL, weights = quad_w)) # CO2** H2O***
summary(manova(trans_dfwL.cbind[,c(4,7)] ~ CO2Tmt*H2OTmt, trans_dfwL, weights = quad_w)) 

# redo above with all_w and quad_w just from logAvg_area, logPerimArea, Photo.y, Cond.y
# redo above with all_w and quad_w just from logAvg_area, logSRL, Photo.y, Cond.y



  

