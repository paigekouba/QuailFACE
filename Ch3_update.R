# Wed 8/21/24
# Structural Causal Model for Ch 3 Results

# Error Propagation for Anet Estimates

# Addressing Pseudoreplication/Data Loss Issue

# trying out SEM with Sushmita's code
library(piecewiseSEM)

lmer_Anet_L <- lmer(Anet ~ rescale(CO2) * rescale(SWC) + time_scaled + (1 | Plot), data = filter(final_df, Spp=="L"))

lm_totmass_L <- lm(totmass~rescale(CO2)*rescale(SWC), data=plotmeans.L)

psem_totmass <- psem(lmer_Anet_L, lm_totmass_L, data = final_df)
summary(psem_totmass)
plot(psem_totmass)

lmer_Anet_L <- lmer(Anet ~ rescale(CO2) * rescale(SWC) + time_scaled + (1 | Plot), data = filter(final_df, Spp=="L"))

lmer_totmass_L <- lmer(totmass~rescale(CO2)*rescale(SWC) + (1 | Plot), data = filter(final_df, Spp=="L"))

psem_totmass <- psem(lmer_Anet_L, lmer_totmass_L, data = filter(final_df, Spp=="L"))
summary(psem_totmass)
plot(psem_totmass)

lmer_Anet_L <- lmer(Anet ~ rescale(CO2) * rescale(SWC) + time_scaled + (1 | Plot), data = filter(final_df, Spp=="L"))

lmer_final.ht <- lmer(Ht.mm..8 ~ rescale(CO2)*rescale(SWC) + (1|Plot), data = filter(final_df, Spp=="L"))

psem_final.ht <- psem(lmer_Anet_L, lmer_final.ht, data = filter(final_df, Spp=="L"))
summary(psem_final.ht)
plot(psem_final.ht)


lmer_Anet_L <- lmer(Anet ~ rescale(CO2) + rescale(meanSWC) + time_scaled + (1|Plot), data = filter(final_df, Spp=="L"))

lmer_gs_L <- lmer(gs ~ rescale(CO2) + rescale(meanSWC) + time_scaled + (1|Plot), data = filter(final_df, Spp=="L"))

lmer_totmass_L <- lmer(totmass~Anet + (1 | Plot), data = filter(final_df, Spp=="L"))

psem_totmass <- psem(lmer_Anet_L, lmer_gs_L, lmer_totmass_L, data = filter(final_df, Spp=="L"))
summary(psem_totmass)
plot(psem_totmass)

lmer_Anet_L <- lmer(Anet ~ rescale(CO2) * rescale(meanSWC) + time_scaled + (1|Plot), data = filter(final_df, Spp=="L"))

lmer_totmass_L <- lmer(totmass ~ Anet + (1|Plot), data = filter(final_df, Spp=="L"))

psem_totmass <- psem(lmer_Anet_L, lmer_totmass_L, data = filter(final_df, Spp=="L"))
summary(psem_totmass)
plot(psem_totmass)


lmer_Anet <- lmer(Anet ~ rescale(CO2) * rescale(meanSWC) + time_scaled + (1|Plot), data = final_df)
lmer_final.ht_all <- lmer(Ht.mm..8 ~ Anet + (1|Plot), data = final_df)
psem_final.ht_all <- psem(lmer_Anet, lmer_final.ht_all, data = final_df)
summary(psem_final.ht_all)
plot(psem_final.ht_all)
#
lmer_totmass <- lmer(totmass ~ Anet + (1|Plot), data = final_df)
psem_totmass <- psem(lmer_Anet, lmer_totmass, data = final_df)
summary(psem_totmass)
plot(psem_totmass)

ggpredict(lmer(Anet~rescale(CO2)*rescale(SWC)+time_scaled + totmass + (1|Plot), data=final_df, REML = F), 
          terms=c("CO2","SWC [4,42]"))%>% plot(rawdata=T,ci=T,colors=c("red","blue")) + labs(title="Anet, all *")
summary(lmer(Anet~rescale(CO2)*rescale(SWC)+time_scaled + totmass + (1|Plot), data=final_df, REML = F))


lmer_Anet_L <- lmer(Anet ~ rescale(CO2) * rescale(meanSWC) + time_scaled + (1|Plot), data = filter(final_df, Spp=="L"))

lmer_final.ht <- lmer(Ht.mm..8 ~ Anet + (1|Plot), data = filter(final_df, Spp=="L"))

psem_final.ht <- psem(lmer_Anet_L, lmer_final.ht, data = filter(final_df, Spp=="L"))
summary(psem_final.ht)
plot(psem_final.ht)


# try it

lmer_gs <- lmer(gs ~ rescale(CO2)*rescale(meanSWC) + (1|Plot), data=final_df)
lmer_Anet <- lmer(Anet ~ rescale(CO2)*rescale(meanSWC) + gs + (1|Plot), data=final_df)
lmer_totmass <- lmer(totmass ~ rescale(CO2)*rescale(meanSWC) + Anet + (1|Plot), data=final_df)
psem_totmass <- psem(lmer_gs, lmer_Anet, lmer_totmass)
summary(psem_totmass)
plot(psem_totmass)

# add a categorical variable for herbivory Y/N

# want this structure:

# CO2                          total mass OR root mass
# CO2 x SWC                             Anet
# SWC               herbivory            gs

final_df2 <- final_df %>% 
  mutate(herbivory = as.numeric(Code %in% c(firstfullherb$Code,"8L2", "6V2"))) %>% 
  filter(!(is.na(Anet))) %>% 
  select(gs, CO2, meanSWC, Anet, herbivory, rootmass_g, totmass, Plot)

glm_herbivory <- glm(herbivory ~ rescale(CO2)*rescale(meanSWC), data=final_df2, family=binomial)
lmer_gs <- lmer(gs ~ rescale(CO2)*rescale(meanSWC) + (1|Plot), data=final_df2)
lmer_Anet <- lmer(Anet ~ rescale(CO2)*rescale(meanSWC) + gs + (1|Plot), data=final_df2)
# lmer_rootmass <- lmer(rootmass_g ~ rescale(CO2)*rescale(meanSWC) + Anet + herbivory + (1|Plot), data=final_df2)
lmer_rootmass <- lmer(rootmass_g ~ rescale(CO2)*rescale(meanSWC) + Anet + herbivory + (1|Plot), data=final_df2)
psem_rootmass <- psem(lmer_gs, glm_herbivory, lmer_Anet, lmer_rootmass)
summary(psem_rootmass)
plot(psem_rootmass)

psem_rootmass <- psem(
  lmer_gs,
  lmer_Anet,
  glm_herbivory,
  lmer_rootmass,
  CO2 %~~% CO2,
  meanSWC %~~% meanSWC
)

summary(psem_rootmass, trace = TRUE)

# try without rescales
glm_herbivory <- glm(herbivory ~ CO2*meanSWC, data=final_df2, family=binomial)
lmer_gs <- lmer(gs ~ CO2*meanSWC + (1|Plot), data=final_df2)
lmer_Anet <- lmer(Anet ~ CO2*meanSWC + gs + (1|Plot), data=final_df2)
# lmer_rootmass <- lmer(rootmass_g ~ CO2*meanSWC + Anet + herbivory + (1|Plot), data=final_df2)
lmer_rootmass <- lmer(rootmass_g ~ CO2*meanSWC + Anet + herbivory + (1|Plot), data=final_df2)
psem_rootmass <- psem(lmer_gs, glm_herbivory, lmer_Anet, lmer_rootmass)
summary(psem_rootmass, standardize = "scale")
plot(psem_rootmass)

glm_herbivory <- glm(herbivory ~ CO2*meanSWC, data=final_df2, family=binomial)
lmer_gs <- lmer(gs ~ CO2*meanSWC + (1|Plot), data=final_df2)
lmer_Anet <- lmer(Anet ~ CO2*meanSWC + gs + (1|Plot), data=final_df2)
# lmer_rootmass <- lmer(rootmass_g ~ CO2*meanSWC + Anet + herbivory + (1|Plot), data=final_df2)
lmer_totmass <- lmer(totmass ~ CO2*meanSWC + Anet + herbivory + (1|Plot), data=final_df2)
psem_totmass <- psem(lmer_gs, glm_herbivory, lmer_Anet, lmer_totmass)
summary(psem_totmass, standardize = "scale")
plot(psem_totmass)

final_df3 <- final_df %>% 
  mutate(herbivory = as.numeric(Code %in% c(firstfullherb$Code,"8L2", "6V2"))) %>% 
  select(gs, CO2, meanSWC, Anet, herbivory, rootmass_g, totmass, Plot, Spp, Ht.mm..8, StemWet_g, time_scaled, H2OTmt, CO2Tmt, SWC)

glm_herbivory <- glm(herbivory ~ meanSWC, data=final_df3, family=binomial)
lmer_gs <- lmer(gs ~ CO2*meanSWC + time_scaled + (1|Plot), data=final_df3)
lmer_Anet <- lmer(Anet ~ CO2*meanSWC + time_scaled + gs + (1|Plot), data=final_df3)
lmer_rootmass <- lmer(rootmass_g ~ CO2*meanSWC + Anet + herbivory + Spp + (1|Plot), data=final_df3)
lmer_totmass <- lmer(totmass ~ CO2*meanSWC + Anet + herbivory + (1|Plot), data=final_df3)
lmer_final.ht <- lmer(Ht.mm..8 ~ CO2*meanSWC + Anet + herbivory + (1|Plot), data=final_df3)
psem_totmass <- psem(lmer_gs, glm_herbivory, lmer_Anet, lmer_totmass)
summary(psem_totmass, standardize = "scale")
plot(psem_totmass)

psem_rootmass <- psem(lmer_gs, glm_herbivory, lmer_Anet, lmer_rootmass)
summary(psem_rootmass, standardize = "scale")
plot(psem_rootmass)

psem_final.ht <- psem(lmer_gs, glm_herbivory, lmer_Anet, lmer_final.ht)
summary(psem_final.ht, standardize = "scale")
plot(psem_final.ht)

# what if you drop interactions if not significant?
glm_herbivory <- glm(herbivory ~ SWC, data=final_df3, family=binomial)
lmer_gs <- lmer(gs ~ CO2+SWC + (1|Plot), data=final_df3)
lmer_Anet <- lmer(Anet ~ CO2*SWC + gs + (1|Plot), data=final_df3)
lmer_rootmass <- lmer(rootmass_g ~ CO2+SWC + Anet + herbivory + (1|Plot), data=final_df3)
lmer_totmass <- lmer(totmass ~ CO2+SWC + Anet + herbivory + (1|Plot), data=final_df3)
lmer_final.ht <- lmer(Ht.mm..8 ~ CO2+SWC + Anet + herbivory + (1|Plot), data=final_df3)
psem_totmass <- psem(lmer_gs, glm_herbivory, lmer_Anet, lmer_totmass)
summary(psem_totmass, standardize = "scale")
plot(psem_totmass)

psem_rootmass <- psem(lmer_gs, glm_herbivory, lmer_Anet, lmer_rootmass)
summary(psem_rootmass, standardize = "scale")
plot(psem_rootmass)

psem_final.ht <- psem(lmer_gs, glm_herbivory, lmer_Anet, lmer_final.ht)
summary(psem_final.ht, standardize = "scale")
plot(psem_final.ht)

# subset by species?
glm_herbivory <- glm(herbivory ~ meanSWC, data=filter(final_df3, Spp == "L"), family=binomial)
lmer_gs <- lmer(gs ~ CO2+meanSWC + (1|Plot), data=filter(final_df3, Spp == "L"))
lmer_Anet <- lmer(Anet ~ CO2*meanSWC + gs + (1|Plot), data=filter(final_df3, Spp == "L"))
lmer_rootmass <- lmer(rootmass_g ~ CO2+meanSWC + Anet + herbivory + (1|Plot), data=filter(final_df3, Spp == "L"))
lmer_totmass <- lmer(totmass ~ CO2+meanSWC + Anet + herbivory + (1|Plot), data=filter(final_df3, Spp == "L"))
lmer_final.ht <- lmer(Ht.mm..8 ~ CO2+meanSWC + Anet + herbivory + (1|Plot), data=filter(final_df3, Spp == "L"))
psem_totmass <- psem(lmer_gs, glm_herbivory, lmer_Anet, lmer_totmass)
summary(psem_totmass, standardize = "scale")
plot(psem_totmass)

psem_rootmass <- psem(lmer_gs, glm_herbivory, lmer_Anet, lmer_rootmass)
summary(psem_rootmass, standardize = "scale")
plot(psem_rootmass)

psem_final.ht <- psem(lmer_gs, glm_herbivory, lmer_Anet, lmer_final.ht)
summary(psem_final.ht, standardize = "scale")
plot(psem_final.ht)

# categorical predictors?
