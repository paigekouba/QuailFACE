# 5/5/24
# Ch 3 Analysis

## add in modeling stuff here

# photosynthesis model with *cleaned* data
# data from Ch3_explore
LiCOR_df <- LiCOR_df %>% 
  mutate(CO2Tmt = substring(Tmt,1,1), H2OTmt = substring(Tmt,2,2))
summary(lmer(Photo.y ~ rescale(CO2)*rescale(SWC) + (1|Plot), data = filter(LiCOR_df.intx, Spp == "V" | Spp == "V")))
print(lmer(Photo.y ~ CO2Tmt*SWC + (1|Plot), data = filter(LiCOR_df, Spp == "V" | Spp == "V")))

# biomass model
biomass_nh <- biomass_nh %>% 
  mutate(CO2Tmt = substring(Tmt,1,1), H2OTmt = substring(Tmt,2,2))
summary(glmer((rootmass_g+StemWet_g+LeafWet_g) ~ H2OTmt*CO2Tmt + (1|Plot), data = biomass_nh))
summary(glmer((rootmass_g+StemWet_g+LeafWet_g) ~ H2OTmt*CO2Tmt + (1|Plot), data = biomass_nh))

biomass <- biomass %>% 
  mutate(CO2Tmt = substring(Tmt,1,1), H2OTmt = substring(Tmt,2,2))
summary(glmer(rootmass_g ~ H2OTmt*CO2Tmt + (1|Plot), data = filter(biomass,  Spp == "L" | Spp == "V")))

## EXPANDED biomass model
# biomass2 <- biomass2 %>%
#   mutate(CO2Tmt = substring(Tmt,1,1), H2OTmt = substring(Tmt,2,2)) %>% 

# total biomass, expanded data
summary(glmer(totmass ~ rescale(meanSWC)*rescale(CO2) + (1|Plot), data = filter(biomass2_intx, Spp == "V" | Spp == "V")))
# root:shoot, expanded data
summary(glmer(rootshoot ~ H2OTmt*CO2Tmt + (1|Plot), data = filter(biomass2_intx, Spp == "V" | Spp == "V")))
summary(lmer(rootshoot ~ rescale(meanSWC)*rescale(CO2) + (1|Plot), data = filter(biomass2_intx, Spp == "V" | Spp == "V")))

# root mass 
summary(glmer(rootmass_g ~ H2OTmt*CO2Tmt + (1|Plot), data = filter(biomass,  Spp == "V" | Spp == "V")))

# SIF model
SIF <- SIF %>% 
  mutate(CO2Tmt = substring(Tmt,1,1), H2OTmt = substring(Tmt,2,2)) 
  
summary(glmer(d13C ~ H2OTmt*CO2Tmt + (1|Plot), data = SIF))
summary(glmer(d13C ~ meanSWC*CO2 + (1|Plot), data = SIF_intx))
# Fixed effects:
#                 Estimate Std. Error t value
# (Intercept)     -27.9708     0.3428 -81.601
# H2OTmtW          -0.8829     0.4865  -1.815
# CO2TmtE          -0.9483     0.4817  -1.968
# H2OTmtW:CO2TmtE  -0.2761     0.6823  -0.405



# which data types do I have the most data for? *unique* seedlings
nrow(biomass) # 127
nrow(LiCOR_df) # 75
nrow(SIF) # 92
nrow(rootimage) # 128

# how many overlap from biomass_nh and SIF?
sum(biomass_nh$Code %in% SIF_pca$Code) # 69

# last but not least a PERMANOVA
df_pca.e <- biomass2 %>%
  dplyr::select(Code, Tmt, StemWet_expanded, LeafWet_expanded, rootmass_g) %>%
  mutate(rootshoot = rootmass_g/(StemWet_expanded + LeafWet_expanded)) %>% 
  filter(!if_any(everything(),is.na))

pca_full.e <- left_join(df_pca.e, SIF_pca, by = "Code") 

head(pca_full.e) # expanded data: stem, leaf, root, root:shoot, d13C
colnames(pca_full.e) 
# "Code" "Tmt"  "StemWet_expanded" "LeafWet_expanded" "rootmass_g"  "rootshoot"  "d13C"
# add Anet and cond ?

LiCOR_df %>% 
  mutate(Code = if_else(nchar(ID) == 4,substr(ID,1,3),substr(ID,1,4))) %>% 
  dplyr::select(Code) %>% 
  tally(Code %in% pca_full.e$Code) # 61 overlapping

# rootimage <- rootimage %>% 
#   mutate(Code = if_else(nchar(Code) == 4,substr(Code,1,3),substr(Code,1,4)))

permanova_df <- LiCOR_df %>% 
  mutate(Code = if_else(nchar(ID) == 4,substr(ID,1,3),substr(ID,1,4))) %>% 
  dplyr::select(Code, Photo.y, Cond.y, WUE.350) %>% 
  left_join(dplyr::select(rootimage, Plot, Code, Average.Diameter.mm, Root.Length.Diameter.Range.1.mm), by = "Code") %>%  
  left_join(pca_full.e, by = "Code") %>% 
  filter(!is.na(Tmt)) %>% 
  dplyr::select(Tmt, Plot, Photo.y, Cond.y, WUE.350, Average.Diameter.mm, Root.Length.Diameter.Range.1.mm, StemWet_expanded, LeafWet_expanded, rootmass_g, rootshoot, d13C) %>% 
  mutate(d13C = abs(d13C)) %>% 
  #  mutate(across(where(is.numeric), ~ rescale(.x))) %>% 
  mutate(H2OTmt = substr(Tmt,2,2), CO2Tmt = substr(Tmt,1,1))

permanova_dfL <- LiCOR_df %>% 
  filter(Spp == "L") %>% 
  mutate(Code = if_else(nchar(ID) == 4,substr(ID,1,3),substr(ID,1,4))) %>% 
  dplyr::select(Code, Photo.y, Cond.y, WUE.350) %>%
  left_join(dplyr::select(rootimage, Plot, Code, Average.Diameter.mm, Root.Length.Diameter.Range.1.mm), by = "Code") %>% 
  left_join(pca_full.e, by = "Code") %>%
  filter(!is.na(Tmt)) %>% 
  dplyr::select(Tmt, Plot, Photo.y, Cond.y, WUE.350, Average.Diameter.mm, Root.Length.Diameter.Range.1.mm, StemWet_expanded, LeafWet_expanded, rootmass_g, rootshoot, d13C) %>% 
  mutate(d13C = abs(d13C)) %>% 
  mutate(across(where(is.numeric), ~ rescale(.x))) %>% 
  mutate(H2OTmt = substr(Tmt,2,2), CO2Tmt = substr(Tmt,1,1))

permanova_dfV <- LiCOR_df %>% 
  filter(Spp == "V") %>% 
  mutate(Code = if_else(nchar(ID) == 4,substr(ID,1,3),substr(ID,1,4))) %>% 
  dplyr::select(Code, Photo.y, Cond.y, WUE.350) %>%
  left_join(dplyr::select(rootimage, Plot, Code, Average.Diameter.mm, Root.Length.Diameter.Range.1.mm), by = "Code") %>% 
  left_join(pca_full.e, by = "Code") %>%
  filter(!is.na(Tmt)) %>% 
  dplyr::select(Tmt, Plot, Photo.y, Cond.y, WUE.350, Average.Diameter.mm, Root.Length.Diameter.Range.1.mm, StemWet_expanded, LeafWet_expanded, rootmass_g, rootshoot, d13C) %>% 
  mutate(d13C = abs(d13C)) %>% 
  mutate(across(where(is.numeric), ~ rescale(.x))) %>% 
  mutate(H2OTmt = substr(Tmt,2,2), CO2Tmt = substr(Tmt,1,1))
colnames(permanova_df)
# [1] "Code"                            "Photo.y"                         "Cond.y"                          "WUE.350"                        
# [5] "Plot"                            "Average.Diameter.mm"             "Root.Length.Diameter.Range.1.mm" "Tmt"                            
# [9] "StemWet_expanded"                "LeafWet_expanded"                "rootmass_g"                      "rootshoot"                      
# [13] "d13C"  


library(MASS)
library(tidyverse)
library(scales)
# first get data into wide format
# ex: (Year, Plot, Bin1_ct, Bin2_ct, Bin3_ct, Bin4_ct)

#  # I think it is already in wide format where each number is like a count for its cell

# Create a multivariate response variable that includes only the columns of the data frame that include the counts in bins
# response_matrix <- permanova_df[,-c(1:2,13:14)]
# response_matrixL <- permanova_dfL[,-c(1:2,13:14)]
# response_matrixV <- permanova_dfV[,-c(1:2,13:14)]
response_matrix <- permanova_df[,c(3,8,9,10,12)] # only the five most interesting (?)
response_matrixL <- permanova_dfL[,c(3,8,9,10,12)]
response_matrixV <- permanova_dfV[,c(3,8,9,10,12)]

library(vegan)
adonis2(response_matrix ~ Tmt, permanova_df)
adonis2(response_matrix ~ H2OTmt*CO2Tmt, permanova_df)

adonis2(response_matrixV ~ Tmt, permanova_dfV)
adonis2(response_matrixV ~ H2OTmt*CO2Tmt, permanova_dfV) # for H2OTmt:CO2Tmt, p = 0.048! (rescaled, with Photo.y, Stem, Leaf, root, abs(d13C))
# adonis2(formula = response_matrixV ~ H2OTmt * CO2Tmt, data = permanova_dfV)
# Df SumOfSqs      R2       F Pr(>F)    
#   H2OTmt         1  0.77539 0.28387 11.4156  0.001 ***
#   CO2Tmt         1  0.34893 0.12774  5.1371  0.005 ** 
#   H2OTmt:CO2Tmt  1  0.18075 0.06617  2.6611  0.048 *  
#   Residual      21  1.42640 0.52221                   
#   Total         24  2.73146 1.00000 

adonis2(response_matrixV ~ H2OTmt*CO2, left_join(permanova_dfV, plot_CO2, by = "Plot")) ######
# Df    SumOfSqs      R2       F Pr(>F)    
# H2OTmt      1  0.77539 0.28387 11.3219  0.001 ***
# CO2         1  0.28275 0.10352  4.1287  0.005 ** 
# H2OTmt:CO2  1  0.23512 0.08608  3.4331  0.015 *  
# Residual   21  1.43820 0.52653                   
# Total      24  2.73146 1.00000  
colnames(response_matrixV)
# "Photo.y"          "StemWet_expanded" "LeafWet_expanded" "rootmass_g"       "d13C" 

X = permanova_dfV[,c(3,8,9,10,12)]
X=PERMANOVA::IniTransform(X)
D = PERMANOVA::DistContinuous(X)
V_permanova = PERMANOVA::PERMANOVA(D,as.factor(permanova_dfV$Tmt), CoordPrinc=TRUE)
PERMANOVA::plot.PERMANOVA(V_permanova, ColorGroup = c("pink", "lightblue", "red", "blue"),voronoi = TRUE, ColorInd = c("pink", "lightblue", "red", "blue"), PlotInd=TRUE, LabelInd=TRUE, AddLegend = TRUE)

biplot.pcoa(D)

adonis2(response_matrixL ~ Tmt, permanova_dfL)
adonis2(response_matrixL ~ H2OTmt*CO2Tmt, permanova_dfL)

# following this tutorial https://sites.ualberta.ca/~ahamann/teaching/renr690/labs/Lab6.pdf
H2OTmts <- permanova_dfV[,13]
CO2Tmts <- permanova_dfV[,14]
response_matrixV <- permanova_dfV[,c(3,8,9,10,12)]
adonis2(response_matrixV ~ unlist(H2OTmts)*unlist(CO2Tmts), distance = "mahalanobis") # same result as above :)
# choosing mahalanobis because some biomass variables are correlated
# pairwise comparisons adjusted for multiple inference just like a regular ANOVA
library(RVAideMemoire)
library(ecodist)
pairwise.perm.manova(sqrt(distance(response_matrixV, method="mahalanobis")), unlist(permanova_dfV[,1]))
#      AD    AW    ED   
#   AW 0.013 -     -    
#   ED 0.013 0.013 -    
#   EW 0.013 0.451 0.118
# P value adjustment method: fdr 

# NMDS
dm=sqrt(distance(response_matrixV, method="mahalanobis"))
nmds_out = nmds(dm, mindim=2, maxdim=2)
scores=nmds.min(nmds_out)
# Minimum stress for given dimensionality:  0.2214695 
# r^2 for minimum stress configuration:  0.6916589 
nmds_out$stress
#  [1] 0.2408827 0.2297706 0.2576101 0.2437968 0.2385648 0.2438741 0.2456996 0.2393398 0.2214695 0.2444444

mygroups = sort(unique(permanova_dfV$Tmt)) # shows order
mycol = c("pink", "lightblue", "red", "blue") # color in order
plot(scores, pch=19, col=mycol[as.factor(unique(permanova_dfV$Tmt))])
text(scores+0.2, labels=row.names(response_matrixV)) # add offset labels
vectors = vf(scores, response_matrixV, nperm=10) # calculate vectors
plot(vectors, len=0.1, col="red") # add vectors
ordiellipse(scores, permanova_dfV$Tmt, conf=0.6, col=mycol, show.groups=mygroups)

# adding Kruskal-Wallis test of significance to a boxplot (SIF)
library(ggsignif)
levels(SIF$Tmt) <- c("AD","AW","ED","EW")
SIF %>% # d13C
  ggplot(aes(x=Tmt, y=d13C)) + 
  geom_boxplot(aes(color=Tmt)) + 
  facet_grid(rows = vars(Spp), scales = "free") + 
  scale_color_manual(values = c("pink", "lightblue", "red", "blue")) +
  geom_text(data = nequals_SIF, aes(x = Tmt, y = -26, label = paste0("N = ",n))) +
  stat_compare_means(comparisons = list(c("AD","ED"), c("AW","EW")), method = "wilcox.test", na.rm = TRUE)
#  geom_signif(test="kruskal.test", exact=FALSE, comparisons = list(c("AD","ED"), c("AW","EW")),step_increase=0.2, map_signif_level = TRUE)

kruskal.test(SIF[,"d13C"],SIF[,"Tmt"])
require(graphics)
boxplot(d13C ~ Tmt, data = SIF)
kruskal.test(d13C ~ Tmt, data = SIF)

# messing with interaction model
main_effect_mod <- lmer(Photo.y ~ CO2Tmt+SWC + (1|Plot), data=filter(LiCOR_df, Spp=="V"))
interaction_mod <- lmer(Photo.y ~ CO2Tmt*SWC + (1|Plot), data=filter(LiCOR_df, Spp=="V"))
# main_effect_mod <- lm(Photo.y ~ CO2Tmt+SWC , data=filter(LiCOR_df, Spp=="V"))
# interaction_mod <- lm(Photo.y ~ CO2Tmt*SWC , data=filter(LiCOR_df, Spp=="V"))
anova(main_effect_mod, interaction_mod)
Anova(interaction_mod)
library(ggeffects)
ggpredict(interaction_mod, terms=c("SWC","CO2Tmt")) %>% 
  plot(add.data = TRUE, ci = FALSE, colors = "bw") + labs(linetype="CO2Tmt") 


# do it for other data types: total biomass
# first need SWC associated per plot
plot_SWC <- LiCOR_df[,c("Plot", "SWC")] %>% 
  group_by(Plot) %>% 
  summarise(meanSWC=mean(SWC)) %>% 
  as.data.frame()

biomass2_intx <- biomass2 %>% 
  mutate(totmass = StemWet_expanded+LeafWet_expanded+rootmass_g) %>% 
  mutate(rootshoot = rootmass_g/(StemWet_expanded+LeafWet_expanded)) %>% 
  left_join(plot_SWC, by = "Plot") %>% 
  left_join(plot_CO2, by = "Plot")

main_effect_mod <- lmer(rootshoot ~ CO2Tmt+meanSWC + (1|Plot), data=filter(biomass2_intx, Spp=="L"|Spp=="L"))
interaction_mod <- lmer(rootshoot ~ CO2Tmt*meanSWC + (1|Plot), data=filter(biomass2_intx, Spp=="L"|Spp=="L"))
anova(main_effect_mod, interaction_mod)
Anova(interaction_mod)
ggpredict(interaction_mod, terms=c("meanSWC","CO2Tmt")) %>% 
  plot(add.data = TRUE, ci = FALSE)

SIF_intx <- SIF %>% 
  left_join(plot_SWC, by = "Plot") %>% 
  left_join(plot_CO2, by = "Plot")

main_effect_mod <- lmer(d13C ~ CO2Tmt+meanSWC + (1|Plot), data=filter(SIF_intx, Spp=="V"|Spp=="V"))
interaction_mod <- lmer(d13C ~ CO2Tmt*meanSWC + (1|Plot), data=filter(SIF_intx, Spp=="V"|Spp=="V"))
anova(main_effect_mod, interaction_mod)
Anova(interaction_mod)
ggpredict(interaction_mod, terms=c("meanSWC","CO2Tmt")) %>% 
  plot(add.data = TRUE, ci = FALSE)

# switch the categorical and continuous variables
plot_CO2 <- avg_between %>% # from CO2_Ch2.R
  dplyr::select(Plot, mDeltaTest) %>% 
  mutate(CO2 = 420+mDeltaTest) %>% 
  as.data.frame()

LiCOR_df.intx <- LiCOR_df %>% 
  left_join(plot_CO2, by = "Plot")


main_effect_mod <- lmer(Photo.y ~ CO2+H2OTmt + (1|Plot), data=filter(LiCOR_df.intx, Spp=="V"|Spp=="V"))
interaction_mod <- lmer(Photo.y ~ CO2*H2OTmt + (1|Plot), data=filter(LiCOR_df.intx, Spp=="V"|Spp=="V"))
# Warning message:
# Some predictor variables are on very different scales: consider rescaling 
anova(main_effect_mod, interaction_mod)
Anova(interaction_mod)
ggpredict(interaction_mod, terms=c("CO2","H2OTmt")) %>% 
  plot(add.data = TRUE, ci = FALSE)

# photosynthesis
main_effect_mod <- lmer(Photo.y ~ CO2+H2OTmt + (1|Plot), data=filter(LiCOR_df.intx, Spp=="V"|Spp=="V"))
interaction_mod <- lmer(Photo.y ~ CO2*H2OTmt + (1|Plot), data=filter(LiCOR_df.intx, Spp=="V"|Spp=="V"))
# Warning message:
# Some predictor variables are on very different scales: consider rescaling 
anova(main_effect_mod, interaction_mod)
Anova(interaction_mod)
ggpredict(interaction_mod, terms=c("CO2","H2OTmt")) %>% 
  plot(add.data = TRUE, ci = FALSE)

# biomass
main_effect_mod <- lmer(totmass ~ CO2+H2OTmt + (1|Plot), data=filter(biomass2_intx, Spp=="V"|Spp=="V"))
interaction_mod <- lmer(totmass ~ CO2*H2OTmt + (1|Plot), data=filter(biomass2_intx, Spp=="V"|Spp=="V"))
anova(main_effect_mod, interaction_mod)
Anova(interaction_mod)
ggpredict(interaction_mod, terms=c("CO2","H2OTmt")) %>% 
  plot(add.data = TRUE, ci = FALSE)

# d13C
