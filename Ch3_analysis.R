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



# do it again but add root features, LiCOR data, and d13C
# first, prep those three data types for a left join (get ID or Code variable --> Code w/o a or b format)

rootimage_pca <- rootimage[, c(4,6,7,8,9,11,18,19)]
rootimage_pca$Code <- if_else(nchar(rootimage_pca$Code) == 4,substr(rootimage_pca$Code,1,3),substr(rootimage_pca$Code,1,4))

df_all_pca <- df_all[, c(3,11,15,18)]
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

# normalize and examine corr plot, Scree plot, biplot

pca_full_new <- pca_full[,-c(1)]
pca_full_new <- as.data.frame(pca_full_new)
rownames(pca_full_new) <- pca_full[,1]

pca_full_norm <- as.data.frame(cbind(pca_full_new[,1],scale(pca_full_new[,-1])))
colnames(pca_full_norm)[colnames(pca_full_norm) == 'V1'] <- 'Tmt'
pca_full_norm <- pca_full_norm %>% 
  mutate(H2OTmt = substr(Tmt,2,2), CO2Tmt = substr(Tmt,1,1)) %>% 
  mutate(Spp = if_else(nchar(rownames(pca_full_norm))==3,substr(rownames(pca_full_norm),2,2),substr(rownames(pca_full_norm),3,3))) %>% 
  mutate_at(.vars = c(2:11), as.numeric)

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
corr_matrix_full <- cor(pca_full_norm[,c(2:11)])
ggcorrplot(corr_matrix_full)


res.pca_full <- prcomp(pca_full_norm[,c(2:11)])
res.pca_fullL <- prcomp(pca_full_normL[,c(2:11)])
res.pca_fullV <- prcomp(pca_full_normV[,c(2:11)])

summary(res.pca_full)
res.pca_full$loadings[, 1:2]
fviz_eig(res.pca_full, addlabels = TRUE)
biplot(res.pca_full)

fviz_pca_biplot(res.pca_full, label = "var", habillage = pca_full_norm$Tmt, addEllipses = TRUE) + scale_color_manual(values=c("pink", "lightblue", "red", "blue")) + scale_fill_manual(values=c("pink", "lightblue", "red", "blue"))

fviz_pca_biplot(res.pca_full, habillage = pca_full_norm$H2OTmt, addEllipses = TRUE, repel = TRUE) + scale_color_manual(values=c("red", "blue")) + scale_fill_manual(values=c("red", "blue"))

fviz_pca_biplot(res.pca_full, habillage = pca_full_norm$CO2Tmt, addEllipses = TRUE, repel = TRUE) + scale_color_manual(values=c("gray", "black")) + scale_fill_manual(values=c("gray", "black"))


# do it again but by spp; because separating out by Spp instead of Tmt led to two different ellipses
fviz_pca_biplot(res.pca_fullV, label = "var", habillage = pca_full_normV$Tmt,  addEllipses = TRUE, repel = TRUE) + scale_color_manual(values=c("pink", "lightblue", "red", "blue")) + scale_fill_manual(values=c("pink", "lightblue", "red", "blue"))

fviz_pca_biplot(res.pca_fullV, habillage = pca_full_normV$H2OTmt, addEllipses = TRUE, repel = TRUE) + scale_color_manual(values=c("red", "blue")) + scale_fill_manual(values=c("red", "blue"))

fviz_pca_biplot(res.pca_fullV, habillage = pca_full_normV$CO2Tmt, addEllipses = TRUE, repel = TRUE) + scale_color_manual(values=c("gray", "black")) + scale_fill_manual(values=c("gray", "black"))
