# 5/5/24
# Ch 3 Analysis

# Trying PCA
## Install packages
#install.packages("corrr")
library('corrr')
#install.packages("ggcorrplot")
library(ggcorrplot)
#install.packages("FactoMineR")
library("FactoMineR")
#install.packages("factoextra")
library(factoextra)

# data from Ch3_explore
colSums(is.na(biomass_nfh))

df_pca <- biomass_nh %>% 
#  filter(Spp == "V") %>% 
#  filter(Tmt == "EW") %>% 
  select(Code, Tmt, StemWet_g, LeafDry_g, LeafWet_g, rootmass_g) %>% 
  mutate(rootshoot = rootmass_g/(StemWet_g + LeafWet_g), lwc = (LeafWet_g - LeafDry_g)/LeafWet_g) %>% 
  filter(!if_any(everything(),is.na)) 

df_pca_new <- df_pca[,-c(1)]
df_pca_new <- as.data.frame(df_pca_new)
rownames(df_pca_new) <- df_pca[,1]
#rownames(df_pca_new) <- c("AD", "AW", "ED", "EW")

#df_pca_norm <- scale(df_pca_new)
df_pca_norm <- as.data.frame(cbind(df_pca_new[,1],scale(df_pca_new[,-1])))
colnames(df_pca_norm)[colnames(df_pca_norm) == 'V1'] <- 'Tmt'
df_pca_norm <- df_pca_norm %>% 
  mutate(H2OTmt = substr(Tmt,2,2), CO2Tmt = substr(Tmt,1,1)) %>% 
  mutate(Spp = if_else(nchar(rownames(df_pca_norm))==3,substr(rownames(df_pca_norm),2,2),substr(rownames(df_pca_norm),3,3))) %>% 
  mutate_at(c("StemWet_g","LeafDry_g","LeafWet_g","rootmass_g","rootshoot","lwc"), as.numeric) 

# separate by spp
df_pca_normL <- df_pca_norm %>% 
  filter(Spp == "L")
df_pca_normV <- df_pca_norm %>% 
  filter(Spp == "V")

## Covariance matrix
corr_matrix <- cor(df_pca_norm[,c(2:7)])
ggcorrplot(corr_matrix)

res.pca <- prcomp(df_pca_norm[,c(2:7)])
res.pcaL <- prcomp(df_pca_normL[,c(2:7)])
res.pcaV <- prcomp(df_pca_normV[,c(2:7)])

fviz_pca_ind(res.pca, label = "none", habillage = df_pca_norm$Tmt)

##
fviz_pca_biplot(res.pca, label = "var", habillage = df_pca_norm$Spp,  addEllipses = TRUE, repel = TRUE) + scale_color_manual(values=c("pink", "lightblue", "red", "blue")) + scale_fill_manual(values=c("pink", "lightblue", "red", "blue"))
# across both spp, W and D treatments seem aligned with different PCs; aCO2 and eCO2 not as clear

fviz_pca_biplot(res.pca, habillage = df_pca_norm$H2OTmt, addEllipses = TRUE, repel = TRUE) + scale_color_manual(values=c("red", "blue")) + scale_fill_manual(values=c("red", "blue"))
# maybe: for droughted plants, higher root:shoot correlates with lower LWC, but no such relationship exists for watered plants

fviz_pca_biplot(res.pca, habillage = df_pca_norm$CO2Tmt, addEllipses = TRUE, repel = TRUE) + scale_color_manual(values=c("gray", "black")) + scale_fill_manual(values=c("gray", "black"))
# nothing terribly exciting going on here

# do it again but by spp; because separating out by Spp instead of Tmt led to two different ellipses
fviz_pca_biplot(res.pcaV, label = "var", habillage = df_pca_normV$Tmt,  addEllipses = TRUE, repel = TRUE) + scale_color_manual(values=c("pink", "lightblue", "red", "blue")) + scale_fill_manual(values=c("pink", "lightblue", "red", "blue"))

fviz_pca_biplot(res.pcaV, habillage = df_pca_normV$H2OTmt, addEllipses = TRUE, repel = TRUE) + scale_color_manual(values=c("red", "blue")) + scale_fill_manual(values=c("red", "blue"))

fviz_pca_biplot(res.pcaV, habillage = df_pca_normV$CO2Tmt, addEllipses = TRUE, repel = TRUE) + scale_color_manual(values=c("gray", "black")) + scale_fill_manual(values=c("gray", "black"))
# nothing terribly exciting going on here


## do it again but add root features, LiCOR data, and d13C
# first, prep those three data types for a left join (get ID or Code variable --> Code w/o a or b format)
rootimage_pca <- rootimage[, c(4,6,7,8,9,11,18,19)]
rootimage_pca$Code <- if_else(nchar(rootimage_pca$Code) == 4,substr(rootimage_pca$Code,1,3),substr(rootimage_pca$Code,1,4))

df_all_pca <- df_all[, c(3,11,15,16)]
df_all_pca$ID <- if_else(nchar(df_all_pca$ID) == 4,substr(df_all_pca$ID,1,3),substr(df_all_pca$ID,1,4))
colnames(df_all_pca)[colnames(df_all_pca) == 'ID'] <- 'Code'
df_all_pca <-df_all_pca %>% 
  group_by(Code) %>% 
  summarise_all(mean) 

SIF_pca <- SIF[,1:2]
SIF_pca$Code <- if_else(nchar(SIF_pca$Code) == 4,substr(SIF_pca$Code,1,3),substr(SIF_pca$Code,1,4))

# then, left_join one at a time
#pca_full <- left_join(df_pca, rootimage_pca, by = "Code")
#pca_full <- left_join(pca_full, df_all_pca, by = "Code")
pca_full <- left_join(df_pca, df_all_pca, by = "Code")
pca_full <- left_join(pca_full, SIF_pca, by = "Code")

# filter any rows with NAs
pca_full <- filter(pca_full, !if_any(everything(),is.na)) 
# replace mass categories with total biomass; keep root:shoot and LWC
pca_full <- pca_full %>% 
  mutate(totmass_g = StemWet_g+LeafWet_g+rootmass_g) %>% 
  select(!(StemWet_g:rootmass_g))


# normalize and examine corr plot, Scree plot, biplot
pca_full_new <- pca_full[,-c(1)]
pca_full_new <- as.data.frame(pca_full_new)
rownames(pca_full_new) <- pca_full[,1]

pca_full_norm <- as.data.frame(cbind(pca_full_new[,1],scale(pca_full_new[,-1])))
colnames(pca_full_norm)[colnames(pca_full_norm) == 'V1'] <- 'Tmt'
pca_full_norm <- pca_full_norm %>% 
  mutate(H2OTmt = substr(Tmt,2,2), CO2Tmt = substr(Tmt,1,1)) %>% 
  mutate(Spp = if_else(nchar(rownames(pca_full_norm))==3,substr(rownames(pca_full_norm),2,2),substr(rownames(pca_full_norm),3,3))) %>% 
  mutate_at(.vars = c(2:8), as.numeric)

# separate by spp
pca_full_normL <- pca_full_norm %>% 
  filter(Spp == "L")
pca_full_normV <- pca_full_norm %>% 
  filter(Spp == "V")

## Covariance matrix
corr_matrix <- cor(df_pca_norm[,c(2:7)])
ggcorrplot(corr_matrix)

res.pca <- prcomp(df_pca_norm[,c(2:7)])
res.pcaL <- prcomp(df_pca_normL[,c(2:7)])
res.pcaV <- prcomp(df_pca_normV[,c(2:7)])

## Covariance matrix
corr_matrix_full <- cor(pca_full_norm[,c(2:8)])
ggcorrplot(corr_matrix_full)

res.pca_full <- prcomp(pca_full_norm[,c(2:8)])
res.pca_fullL <- prcomp(pca_full_normL[,c(2:8)])
res.pca_fullV <- prcomp(pca_full_normV[,c(2:8)])

res.pca_full_thingy <- princomp(pca_full_norm[,c(2:8)])
res.pca_full_thingy$loadings[,1:2]

summary(res.pca_full)
res.pca_full$loadings[, 1:2]
fviz_eig(res.pca_full, addlabels = TRUE)
biplot(res.pca_full)

##
fviz_pca_biplot(res.pca_full, label = "var", habillage = pca_full_norm$Tmt, addEllipses = TRUE) + scale_color_manual(values=c("pink", "lightblue", "red", "blue")) + scale_fill_manual(values=c("pink", "lightblue", "red", "blue"))

fviz_pca_biplot(res.pca_full, habillage = pca_full_norm$H2OTmt, addEllipses = TRUE, repel = TRUE) + scale_color_manual(values=c("red", "blue")) + scale_fill_manual(values=c("red", "blue"))

fviz_pca_biplot(res.pca_full, habillage = pca_full_norm$CO2Tmt, addEllipses = TRUE, repel = TRUE) + scale_color_manual(values=c("gray", "black")) + scale_fill_manual(values=c("gray", "black"))


# do it again but by spp; because separating out by Spp instead of Tmt led to two different ellipses
fviz_pca_biplot(res.pca_fullV, label = "var", habillage = pca_full_normV$Tmt,  addEllipses = TRUE, repel = TRUE) + scale_color_manual(values=c("pink", "lightblue", "red", "blue")) + scale_fill_manual(values=c("pink", "lightblue", "red", "blue"))

fviz_pca_biplot(res.pca_fullV, habillage = pca_full_normV$H2OTmt, addEllipses = TRUE, repel = TRUE) + scale_color_manual(values=c("red", "blue")) + scale_fill_manual(values=c("red", "blue"))

fviz_pca_biplot(res.pca_fullV, habillage = pca_full_normV$CO2Tmt, addEllipses = TRUE, repel = TRUE) + scale_color_manual(values=c("gray", "black")) + scale_fill_manual(values=c("gray", "black"))


## add in modeling stuff here

# photosynthesis model with *cleaned* data
# data from Ch3_explore
summary(glmer(interpol ~ CO2Tmt * H2OTmt + (1|Plot), data = filter(df_all, Spp == "L" | Spp == "V")))
# Fixed effects:
#                 Estimate Std. Error t value
# (Intercept)        5.841      2.065   2.828
# CO2TmtE            4.339      2.633   1.648
# H2OTmtW           13.450      2.530   5.317
# CO2TmtE:H2OTmtW   -5.806      3.402  -1.707
# this is really close! I think it could cross over (|t|>1.96) if I had more datapoints, which I will get

# biomass model
biomass_nh <- biomass_nh %>% 
  mutate(CO2Tmt = substring(Tmt,1,1), H2OTmt = substring(Tmt,2,2))
summary(glmer((rootmass_g+StemWet_g+LeafWet_g) ~ H2OTmt*CO2Tmt + (1|Plot), data = biomass_nh))

biomass <- biomass %>% 
  mutate(CO2Tmt = substring(Tmt,1,1), H2OTmt = substring(Tmt,2,2))
summary(glmer(rootmass_g ~ H2OTmt*CO2Tmt + (1|Plot), data = filter(biomass,  Spp == "L" | Spp == "V")))

# SIF model
SIF <- SIF %>% 
  mutate(CO2Tmt = substring(Tmt,1,1), H2OTmt = substring(Tmt,2,2))
summary(glmer(d13C ~ H2OTmt*CO2Tmt + (1|Plot), data = SIF))
# Fixed effects:
#                 Estimate Std. Error t value
# (Intercept)     -27.9708     0.3428 -81.601
# H2OTmtW          -0.8829     0.4865  -1.815
# CO2TmtE          -0.9483     0.4817  -1.968
# H2OTmtW:CO2TmtE  -0.2761     0.6823  -0.405

# EVERYTHING model
colnames(pca_full)
# "Code"  "Tmt"  "rootshoot" "lwc"  "Cond" "interpol"  "WUE"   "d13C"  "totmass_g"
model_full <-  pca_full %>% 
  mutate(H2OTmt = substr(Tmt,2,2), CO2Tmt = substr(Tmt,1,1)) %>% 
  mutate(Spp = if_else(nchar(rownames(pca_full_norm))==3,substr(rownames(pca_full_norm),2,2),substr(rownames(pca_full_norm),3,3))) %>% 
  mutate_at(.vars = c(3:9), as.numeric)
pairs(model_full[,c(3:9)], col = as.factor(model_full$Spp))

# add Plot as random effect
model_full <- model_full %>% 
  mutate(Plot = if_else(nchar(Code)==3,substr(Code,1,1),substr(Code,1,2)))
library(devtools)
install_github("wbonat/mcglm")
library(mcglm)
form1 <- totmass_g ~ CO2Tmt + H2OTmt
form2 <- interpol ~ CO2Tmt + H2OTmt

mlm1 <- glmer(cbind(rootshoot, lwc, Cond, interpol, WUE, d13C, totmass_g) ~ CO2Tmt*H2OTmt + (1|Plot), data = model_full)
summary(mlm1)  
library(car)
library(lme4)
Anova(mlm1)

mlm2 <- lm(cbind(rootshoot, lwc, Cond, interpol, WUE, d13C, totmass_g) ~ H2OTmt, data = model_full)
anova(mlm1, mlm2) # indicates the model with just H2O is just as good as the one with CO2

# which data types do I have the most data for? *unique* seedlings
nrow(biomass) # 128
nrow(df_all) # 63
nrow(SIF) # 92
nrow(rootimage) # 128

# how many overlap from biomass_nh and SIF?
sum(biomass_nh$Code %in% SIF_pca$Code) # 63

# how many overlap from biomass_nh and SIF?
sum(biomass_nh$Code %in% rootimage_pca$Code) # 81
model_df <- left_join(biomass_nh, rootimage_pca, by = "Code") %>% 
  mutate(totmass_g = StemWet_g+LeafWet_g+rootmass_g) %>% 
  mutate(H2OTmt = substr(Tmt,2,2), CO2Tmt = substr(Tmt,1,1)) %>% 
  select(!(c(StemWet_g, LeafWet_g, LeafDry_g, SeedMass.g., Cond..7, Total.Root.Length.mm, Number.of.Branch.Points))) 
  
pairs(model_df[,c(5:11)])
summary(lm(cbind(rootmass_g, totmass_g, Branching.frequency.per.mm, Root.Length.Diameter.Range.2.mm) ~ CO2Tmt+H2OTmt, data = model_df))

summary(glmer(Root.Length.Diameter.Range.2.mm ~ CO2Tmt*H2OTmt + (1|Plot), data = model_df))

# can I predict final biomass from ht and dia at Inv..5 ?
