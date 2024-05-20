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


  

