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

# correlation plot?

final_df2.1 %>% 
  select(where(is.numeric)) %>% 
  cor(use = "pairwise.complete.obs") %>% 
  corrplot::corrplot()

final_df2.1 %>% 
  select(where(is.numeric)) %>% 
  drop_na() %>% 
  cor() %>% 
  corrplot::corrplot()

# New version after zooming with Xiaoli! (5/27/25)

# want watering treatment as a predictor; needs to be numeric
final_df2.1 <- final_df2.1 %>% 
  mutate(H2O_num = as.numeric(H2OTmt == "W")) # creates a dummy variable where 1 = watered, 0 = dry

modlist_6 <- psem(
  # gs is predicted by watering, CO2
  lmer(gs ~ CO2 + H2O_num + (1|Plot), na.action = na.omit, final_df2.1),
  # Anet is predicted by gs, CO2, SWC
  lmer(Anet ~ gs + CO2 + meanSWC + (1|Plot), na.action = na.omit, final_df2.1),
  # SWC is predicted by watering, gs
  lmer(meanSWC ~ H2O_num + gs + (1|Plot), na.action = na.omit, final_df2.1),
  # biomass is predicted by Anet, herbivory
  lmer(totmass ~ Anet + herbivory + (1|Plot), na.action = na.omit, final_df2.1)
)
# Warning messages:
#   1: In optwrap(optimizer, devfun, getStart(start, rho$pp), lower = rho$lower,  :
#                   convergence code -4 from nloptwrap: NLOPT_ROUNDOFF_LIMITED: Roundoff errors led to a breakdown of the optimization algorithm. In this case, the returned minimum may still be useful. (e.g. this error occurs in NEWUOA if one tries to achieve a tolerance too close to machine precision.)
#                 2: In checkConv(attr(opt, "derivs"), opt$par, ctrl = control$checkConv,  :
#                                   unable to evaluate scaled gradient
#                                 3: In checkConv(attr(opt, "derivs"), opt$par, ctrl = control$checkConv,  :
#                                                   Model failed to converge: degenerate  Hessian with 1 negative eigenvalues
#                                                 4: NAs detected in the dataset. Consider removing all rows with NAs to prevent fitting to different subsets of data 

summary(modlist_6, standardize = "scale")
plot(modlist_6)

# Tests of directed separation:
#   
#            Independ.Claim Test.Type      DF Crit.Value P.Value    
#       meanSWC ~ CO2 + ...      coef  1.3524    -1.4486  0.3364    
#       totmass ~ CO2 + ...      coef 15.2375     0.6252  0.5411    
#      Anet ~ H2O_num + ...      coef 11.0953     2.2366  0.0468   *
#   totmass ~ H2O_num + ...      coef 19.1663     2.4934  0.0220   *
#      gs ~ herbivory + ...      coef 61.7705     5.3764  0.0000 ***
#    Anet ~ herbivory + ...      coef 65.6250     0.5878  0.5587    
# meanSWC ~ herbivory + ...      coef  0.5090     0.0000  1.0000    
#        totmass ~ gs + ...      coef 59.8481    -1.4746  0.1456    
#   totmass ~ meanSWC + ...      coef  8.5485     1.7766  0.1111    

# herbivory predicts gs but SWC does not predict herbivory ?? and direct paths from H2O to Anet, totmass, w/o passing through SWC ?

# --
#   Global goodness-of-fit:
#   
#   Chi-Squared = 370.609 with P-value = 0 and on 9 degrees of freedom
# Fisher's C = 53.796 with P-value = 0 and on 18 degrees of freedom
# 
# ---
# Coefficients:
# 
#   Response Predictor Estimate Std.Error      DF Crit.Value P.Value Std.Estimate    
#         gs       CO2   0.0002    0.0002 12.2090     0.9591  0.3561       0.1359    
#         gs   H2O_num   0.0857    0.0239 12.0551     3.5891  0.0037       0.5101  **
#       Anet        gs  67.6771    5.2138 68.8573    12.9803  0.0000       0.7760 ***
#       Anet       CO2   0.0299    0.0079 13.7779     3.8140  0.0019       0.2509  **
#       Anet   meanSWC   0.0617    0.0407 18.8251     1.5167  0.1459       0.1099    
#    meanSWC   H2O_num  24.5746    1.1578 69.5152    21.2248  0.0000       0.9424 ***
#    meanSWC        gs   0.0000    0.0001 19.9804     0.0000  1.0000       0.0000    
#    totmass      Anet   0.2023    0.1248 64.9039     1.6215  0.1098       0.2079    
#    totmass herbivory  -4.9817    1.8350 56.7115    -2.7148  0.0088      -0.3359  **

ggplot(final_df2.1, aes(x=herbivory, y=gs, color = Spp)) + geom_jitter()
ggplot(final_df2.1, aes(x=as.factor(herbivory), y=gs, color = Spp)) + facet_wrap(~H2OTmt) + geom_boxplot() + geom_jitter() 

library(dagitty)
library(ggdag)
# specify the DAG
g <- dagitty("dag{
             CO2 -> gs H2O_num -> gs;
             gs -> Anet CO2 -> Anet meanSWC -> Anet;
             H2O_num -> meanSWC gs-> meanSWC;
             Anet -> totmass herbivory -> totmass
             
             herbivory -> Anet herbivory -> gs
             H2O_num -> herbivory
             }") #      Spp_num -> Anet Spp_num -> herbivory

coordinates(g) <-
  list(x=c(CO2 = -1, gs = 0, Anet = 0, totmass = 0, H2O_num = 3, meanSWC = 1.75, herbivory = 1.5),
       y=c(CO2 = 1, gs = 2, Anet = 3, totmass = 4, H2O_num = 2, meanSWC = 2.5, herbivory = 4))
plot(g)
# ggdag(g) + theme_classic()
# test the DAG against data
localTests(
  x = g,
#  data = select(drop_na(final_df2.1), where(is.numeric))
  data = select(filter(drop_na(final_df2.1), Spp == "L"), where(is.numeric))
)
plotLocalTestResults(localTests(g, data = select(filter(drop_na(final_df2.1), Spp == "L"), where(is.numeric))))
ggplot(final_df2.1, aes(x=herbivory, y=Anet, color = Spp)) + geom_jitter()

impliedConditionalIndependencies(g)

# translate this DAG to a piecewiseSEM
modlist_7 <- psem(
  # gs is predicted by watering, CO2
  lmer(gs ~ CO2 + H2O_num + herbivory + (1|Plot), na.action = na.omit, filter(final_df2.1, Spp == "L")),
  # Anet is predicted by gs, CO2, SWC
  lmer(Anet ~ gs + CO2 + meanSWC + herbivory + (1|Plot), na.action = na.omit, filter(final_df2.1, Spp == "L")),
  # SWC is predicted by watering, gs
  lmer(meanSWC ~ H2O_num + gs + (1|Plot), na.action = na.omit, filter(final_df2.1, Spp == "L")),
  # biomass is predicted by Anet, herbivory
  lmer(totmass ~ Anet + herbivory + (1|Plot), na.action = na.omit, filter(final_df2.1, Spp == "L")),
  # herbivory is predicted by H2O_num
 # glmer(herbivory ~ H2O_num + (1|Plot), family = binomial(link = "logit"), na.action = na.omit, filter(final_df2.1, Spp == "L"))
)
summary(modlist_7, standardize = "scale")
plot(modlist_7)

modlist_7.1 <- psem(
  # gs is predicted by watering, CO2
  lmer(gs ~ CO2 + meanSWC + herbivory + (1|Plot), na.action = na.omit, final_df2.1),
  # Anet is predicted by gs, CO2, SWC
  lmer(Anet ~ gs + CO2 + meanSWC + herbivory + (1|Plot), na.action = na.omit, final_df2.1),
  # biomass is predicted by Anet, herbivory
  lmer(totmass ~ Anet + herbivory + (1|Plot), na.action = na.omit, final_df2.1),
  # herbivory is predicted by H2O_num
  glmer(herbivory ~ meanSWC + (1|Plot), family = binomial(link = "logit"), na.action = na.omit, final_df2.1)
)
summary(modlist_7.1, standardize = "scale")
plot(modlist_7.1)

final_df2.1s <- final_df2.1 %>% 
  mutate_if(is.numeric, ~rescale(.))

modlist_7.1s <- psem(
  # gs is predicted by watering, CO2
  lmer(gs ~ CO2 + meanSWC + herbivory + (1|Plot), na.action = na.omit, final_df2.1s),
  # Anet is predicted by gs, CO2, SWC
  lmer(Anet ~ gs + CO2 + meanSWC + herbivory + (1|Plot), na.action = na.omit, final_df2.1s),
  # biomass is predicted by Anet, herbivory
  lmer(totmass ~ Anet + herbivory + meanSWC + (1|Plot), na.action = na.omit, final_df2.1s),
  # herbivory is predicted by H2O_num
  glmer(herbivory ~ meanSWC + (1|Plot), family = binomial(link = "logit"), na.action = na.omit, final_df2.1s)
)
summary(modlist_7.1s)
plot(modlist_7.1s)


# try again with just one spp

modlist_6L <- psem(
  # gs is predicted by watering, CO2
  lmer(gs ~ CO2 + H2O_num + (1|Plot), na.action = na.omit, filter(final_df2.1, Spp == "L")),
  # Anet is predicted by gs, CO2, SWC
  lmer(Anet ~ gs + CO2 + meanSWC + (1|Plot), na.action = na.omit, filter(final_df2.1, Spp == "L")),
  # SWC is predicted by watering, gs
  lmer(meanSWC ~ H2O_num + gs + (1|Plot), na.action = na.omit, filter(final_df2.1, Spp == "L")),
  # biomass is predicted by Anet, herbivory
  lmer(totmass ~ Anet + herbivory + (1|Plot), na.action = na.omit, filter(final_df2.1, Spp == "L"))
)
summary(modlist_6L, standardize = "scale")
plot(modlist_6L)

modlist_6V <- psem(
  # gs is predicted by watering, CO2
  lmer(gs ~ CO2 + H2O_num + (1|Plot), na.action = na.omit, filter(final_df2.1, Spp == "V")),
  # Anet is predicted by gs, CO2, SWC
  lmer(Anet ~ gs + CO2 + meanSWC + (1|Plot), na.action = na.omit, filter(final_df2.1, Spp == "V")),
  # SWC is predicted by watering, gs
  lmer(meanSWC ~ H2O_num + gs + (1|Plot), na.action = na.omit, filter(final_df2.1, Spp == "V")),
  # biomass is predicted by Anet, herbivory
  lmer(totmass ~ Anet + herbivory + (1|Plot), na.action = na.omit, filter(final_df2.1, Spp == "V"))
)
summary(modlist_6V, standardize = "scale")
plot(modlist_6V)

# I think plant-related impacts to SWC are theoretically present but not detectable
# if I take out H2O_num
modlist_6.1 <- psem(
  # gs is predicted by SWC, CO2
  lmer(gs ~ CO2 + meanSWC + (1|Plot), na.action = na.omit, final_df2.1),
  # Anet is predicted by gs, CO2, SWC
  lmer(Anet ~ gs + CO2 + meanSWC + (1|Plot), na.action = na.omit, final_df2.1),
  # biomass is predicted by Anet, herbivory
  lmer(totmass ~ Anet + herbivory + (1|Plot), na.action = na.omit, final_df2.1)
)
summary(modlist_6.1, standardize = "scale")
plot(modlist_6.1)
#           Independ.Claim Test.Type      DF Crit.Value P.Value    
#      totmass ~ CO2 + ...      coef 15.2375     0.6252  0.5411    
#  totmass ~ meanSWC + ...      coef 19.6563     2.7278  0.0131   *
#     gs ~ herbivory + ...      coef 61.7124     5.1995  0.0000 ***
#   Anet ~ herbivory + ...      coef 65.6250     0.5878  0.5587    
#       totmass ~ gs + ...      coef 47.8307    -2.3063  0.0255   *

# saying meanSWC predicts total mass, herbivory predicts gs (??), gs predicts totmass

# if I add SWC predicts herbivory
modlist_6.2 <- psem(
  # gs is predicted by SWC, CO2
  lmer(gs ~ CO2 + meanSWC + (1|Plot), na.action = na.omit, final_df2.1),
  # Anet is predicted by gs, CO2, SWC
  lmer(Anet ~ gs + CO2 + meanSWC + (1|Plot), na.action = na.omit, final_df2.1),
  # biomass is predicted by Anet, herbivory
  lmer(totmass ~ Anet + herbivory + meanSWC + (1|Plot), na.action = na.omit, final_df2.1),
  # herbivory is predicted by SWC
  glmer(herbivory ~ meanSWC + (1|Plot), na.action = na.omit, family = binomial(link = "logit"), final_df2.1)
)
summary(modlist_6.2, standardize = "scale", conserve = TRUE)
plot(modlist_6.2)

# what happens if you remove all rows with NAs?

# can I model resprouting behavior with CO2 treatment in the path?
