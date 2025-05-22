# Wed 5/21/25
# Piecewise SEMs for Quail Ridge eCO2 Oaks Experiment

library(piecewiseSEM)
library(lmerTest)
library(nlme)

final_df2.1 <- final_df %>% 
  mutate(herbivory = as.numeric(Code %in% c(firstfullherb$Code,"8L2", "6V2"))) #%>% 
 # filter(!(is.na(Anet))) %>% 
#  select(gs, CO2, meanSWC, Anet, herbivory, rootmass_g, totmass, Plot)

# start with the simplest model you can think of

modlist_1 <- psem(
  glmer(herbivory ~ meanSWC + (1|Plot), na.action = na.omit, family = binomial(link = "logit"), data = final_df2.1),
  lmer(totmass ~ meanSWC + herbivory + (1|Plot), na.action = na.omit, data = final_df2)
)

summary(modlist_1)
plot(modlist_1)
# Tests of directed separation:
#No independence claims present. Tests of directed separation not possible.
# this means there are no untested paths !
# experiment: if we leave out a path we KNOW should be included

modlist_1.1 <- psem(
  glmer(herbivory ~ meanSWC + (1|Plot), na.action = na.omit, family = binomial(link = "logit"), data = final_df2.1),
  lmer(totmass ~ meanSWC + (1|Plot), na.action = na.omit, data = final_df2.1)
)

summary(modlist_1.1)
plot(modlist_1.1)
#           Independ.Claim Test.Type       DF Crit.Value P.Value  
#totmass ~ herbivory + ...      coef 108.8644    -4.4727       0 ***
# this indicates that a missing path (herbivory --> totmass) is necessary for the best-fit model

# add CO2, Anet, gs to the model; CO2:SWC intxn
# choices: totmass or rootmass or final ht?  include or ignore species? does the order of the piecewise equations matter?

modlist_2 <- psem(
  # totmass is predicted by herb, SWC, Anet
  lmer(totmass ~ herbivory + meanSWC + Anet + (1|Plot), na.action = na.omit, final_df2.1),
  # herb is predicted by SWC
  glmer(herbivory ~ meanSWC + (1|Plot), na.action = na.omit, family = binomial(link = "logit"), final_df2.1),
  # gs is predicted by SWC
  lmer(gs ~ meanSWC + (1|Plot), na.action = na.omit, final_df2.1),
  # Anet is predicted by gs and CO2 and SWC:CO2
  lmer(Anet ~ gs + CO2*meanSWC + (1|Plot), na.action = na.omit, final_df2.1)
)
summary(modlist_2, standardize = "scale")
# test of d-sep
#  gs ~ herbivory + ...      coef  63.6873     5.3590  0.0000 ***
plot(modlist_2)

# need to add a path for herbivory --> gs

modlist_3 <- psem(
  # herbivory is predicted by SWC
  glmer(herbivory ~ meanSWC + (1|Plot), na.action = na.omit, family = binomial(link = "logit"), final_df2.1),
  # gs is predicted by SWC, herbivory
  lmer(gs ~ meanSWC + herbivory + (1|Plot), na.action = na.omit, final_df2.1),
  # Anet is predicted by gs and CO2 and SWC:CO2
  lmer(Anet ~ gs + CO2*meanSWC + (1|Plot), na.action = na.omit, final_df2.1),
  # totmass is predicted by herb, SWC, Anet
  lmer(totmass ~ herbivory + meanSWC + Anet + (1|Plot), na.action = na.omit, final_df2.1)
)
summary(modlist_3, standardize = "scale")
plot(modlist_3)

# one for email to Xiaoli
modlist_5 <- psem(
  # herbivory is predicted by Spp
  glmer(herbivory ~ Spp_num + (1|Plot), na.action = na.omit, family = binomial(link = "logit"), final_df2.1),
  # gs is predicted by SWC, herbivory, Spp
  lmer(gs ~ meanSWC + Spp_num + herbivory + (1|Plot), na.action = na.omit, final_df2.1),
  # Anet is predicted by gs, CO2*SWC, and Spp
  lmer(Anet ~ gs + CO2*meanSWC + Spp_num + (1|Plot), na.action = na.omit, final_df2.1),
  # totmass is predicted by herb, SWC, Anet, Spp
  lmer(totmass ~ herbivory + meanSWC + Anet + Spp_num + (1|Plot), na.action = na.omit, final_df2.1)
)
summary(modlist_5, standardize = "scale")
plot(modlist_5)

final_df2.2 <- final_df2.1 %>% 
  mutate(tot_Anet = Anet*tot_area)

modlist_3 <- psem(
  # totmass is predicted by herb, SWC, Anet
  # lmer(totmass ~ herbivory + meanSWC + Anet + (1|Plot), na.action = na.omit, final_df2.1),
  lmer(totmass ~ herbivory + meanSWC + tot_Anet + (1|Plot), na.action = na.omit, final_df2.2),
  # herb is predicted by SWC
  glmer(herbivory ~ meanSWC + (1|Plot), na.action = na.omit, family = binomial(link = "logit"), final_df2.2),
  # gs is predicted by SWC, herb
  lmer(gs ~ meanSWC + herbivory + totmass + (1|Plot), na.action = na.omit, final_df2.2),
  # Anet is predicted by gs and CO2 and SWC:CO2
  lmer(tot_Anet ~ gs + CO2*meanSWC + (1|Plot), na.action = na.omit, final_df2.2)
#  lmer(WUE ~ meanSWC + (1|Plot), na.action = na.omit, final_df2.1)
)
summary(modlist_3, standardize = "scale")
plot(modlist_3)

# herbivory -- what if we subset to just non-herbivorized seedlings?
final_df_nh <- final_df %>% 
  select(Code, StemWet_g, totmass, rootshoot, Ht.mm..8) %>% 
  filter(!Code %in% c(firstfullherb$Code,"8L2", "6V2")) %>% 
  right_join(select(final_df, !c(totmass, rootshoot, Ht.mm..8)), by="Code") 

modlist_3_nh <- psem(
  # totmass is predicted by herb, SWC, Anet
  lmer(totmass ~ meanSWC + Anet + (1|Plot), na.action = na.omit, final_df_nh),
  # gs is predicted by SWC, herb
  lmer(gs ~ meanSWC + (1|Plot), na.action = na.omit, final_df_nh),
  # Anet is predicted by gs and CO2 and SWC:CO2
  lmer(Anet ~ gs + CO2*meanSWC + (1|Plot), na.action = na.omit, final_df_nh)
)
summary(modlist_3_nh, standardize = "scale")
plot(modlist_3_nh)



# same model, but just for Ls
modlist_3.L <- psem(
  # totmass is predicted by herb, SWC, Anet
  lmer(totmass ~ meanSWC + Anet + herbivory + (1|Plot), na.action = na.omit, filter(final_df2.1, Spp == "L")),
  # herb is predicted by SWC
  glmer(herbivory ~ meanSWC + (1|Plot), na.action = na.omit, family = binomial(link = "logit"), filter(final_df2.1, Spp == "L")),
  # gs is predicted by SWC
  lmer(gs ~ meanSWC + herbivory + (1|Plot), na.action = na.omit, filter(final_df2.1, Spp == "L")),
  # Anet is predicted by gs and CO2 and SWC:CO2
  lmer(Anet ~ gs + CO2 + (1|Plot), na.action = na.omit, filter(final_df2.1, Spp == "L"))
)
summary(modlist_3.L, standardize = "scale")
plot(modlist_3.L)

modlist_3.V <- psem(
  # totmass is predicted by herb, SWC, Anet
  lmer(totmass ~ meanSWC + Anet + herbivory + (1|Plot), na.action = na.omit, filter(final_df2.1, Spp == "V")),
  # herb is predicted by SWC
  glmer(herbivory ~ meanSWC + (1|Plot), na.action = na.omit, family = binomial(link = "logit"), filter(final_df2.1, Spp == "V")),
  # gs is predicted by SWC
  lmer(gs ~ meanSWC + herbivory + (1|Plot), na.action = na.omit, filter(final_df2.1, Spp == "V")),
  # Anet is predicted by gs and CO2 and SWC:CO2
  lmer(Anet ~ gs + CO2 + (1|Plot), na.action = na.omit, filter(final_df2.1, Spp == "V"))
)
summary(modlist_3.V, standardize = "scale")
plot(modlist_3.V)

modlist_3.1 <- psem(
  # totmass is predicted by herb, SWC, Anet
  lmer(totmass ~ Anet + herbivory + (1|Plot), na.action = na.omit, final_df2.2),
  # herb is predicted by SWC
  glmer(herbivory ~ meanSWC + (1|Plot), na.action = na.omit, family = binomial(link = "logit"), final_df2.2),
  # gs is predicted by SWC
  lmer(gs ~ meanSWC + (1|Plot), na.action = na.omit, final_df2.2),
  # Anet is predicted by gs and CO2 and SWC:CO2
  lmer(Anet ~ gs + CO2:meanSWC + (1|Plot), na.action = na.omit, final_df2.2)
)
summary(modlist_3.1, standardize = "scale")
plot(modlist_3.1)

# double check! what things did I filter out by herbivory?

# what if I add species -- this made it mad bc Spp is categorical predictor. dummy variable with SppV == 1, SppL == 0 ?
final_df2.1 <- final_df2.1 %>% 
  mutate(Spp_num = as.numeric(Spp == "L"))

modlist_4 <- psem(
  # totmass is predicted by herb, SWC, Anet
  lmer(totmass ~ herbivory + Spp_num + meanSWC + Anet + (1|Plot), na.action = na.omit, final_df2.1),
  # herb is predicted by SWC
  glmer(herbivory ~ meanSWC + Spp_num + (1|Plot), na.action = na.omit, family = binomial(link = "logit"), final_df2.1),
  # gs is predicted by SWC
  lmer(gs ~ meanSWC + Spp_num + herbivory + (1|Plot), na.action = na.omit, final_df2.1),
  # Anet is predicted by gs and CO2 and SWC:CO2
  lmer(Anet ~ gs + Spp_num + CO2*meanSWC + (1|Plot), na.action = na.omit, final_df2.1)
)
summary(modlist_4, standardize = "scale")
plot(modlist_4)

modlist_4 <- psem(
  # totmass is predicted by herb, SWC, Anet
  lmer(totmass ~ herbivory + Spp_num + meanSWC + Anet + (1|Plot), na.action = na.omit, final_df2.1),
  # herb is predicted by SWC
  glmer(herbivory ~ meanSWC + Spp_num + (1|Plot), na.action = na.omit, family = binomial(link = "logit"), final_df2.1),
  # gs is predicted by SWC
  lmer(gs ~ meanSWC + Spp_num + herbivory + (1|Plot), na.action = na.omit, final_df2.1),
  # Anet is predicted by gs and CO2 and SWC:CO2
  lmer(Anet ~ gs + Spp_num + CO2*meanSWC + (1|Plot), na.action = na.omit, final_df2.1)
)
summary(modlist_4, standardize = "scale")
plot(modlist_4)

modlist_4.1 <- psem(
  # totmass is predicted by herb, SWC, Anet
  lmer(totmass ~ herbivory + Spp_num + meanSWC + Anet + (1|Plot), na.action = na.omit, final_df2.1),
  # herb is predicted by SWC
  glmer(herbivory ~ meanSWC + Spp_num + (1|Plot), na.action = na.omit, family = binomial(link = "logit"), final_df2.1),
  # gs is predicted by SWC
  lmer(gs ~ meanSWC + Spp_num + herbivory + (1|Plot), na.action = na.omit, final_df2.1),
  # Anet is predicted by gs and CO2 and SWC:CO2
  lmer(Anet ~ gs + Spp_num + CO2*meanSWC + CO2:meanSWC:Spp_num + (1|Plot), na.action = na.omit, final_df2.1)
)
summary(modlist_4.1, standardize = "scale")
plot(modlist_4.1)

# are we crazy?
ggplot(final_df2.1, aes(x=CO2, y=Anet, color = Spp)) + facet_wrap(~H2OTmt) + geom_point() # + geom_smooth()
ggplot(final_df2.1, aes(x=CO2, y=Anet, color = Spp, facet_wrap(~meanSWC))) + geom_point() + geom_smooth()

# what happens if you remove all rows with NAs?

# can I model resprouting behavior with CO2 treatment in the path?
