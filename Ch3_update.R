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
