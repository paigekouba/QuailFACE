# Mon June 2, 2025
# Bayesian framework for piecewise SEM
library(piecewiseSEM)
# having convergence issues with psem for my target model
xd.psem.re <- psem( # with random effect for Plot
  lmer(gs_z ~ CO2_z+meanSWC_z + Spp_num + (1|Plot), df_final_z),
  lmer(Anet_z ~ CO2_z*meanSWC_z + Spp_num + (1|Plot), df_final_z),
  lmer(totmass_z ~ Anet_z + meanSWC_z + Spp_num + (1|Plot), df_final_z)
)
plot(xd.psem.re)
summary(xd.psem.re)
AIC(xd.psem.re)
#       AIC  K  n
# 1 430.221 20 64

imp.1.1 <- imp.1 %>% 
  mutate(Spp_num = as.numeric(Spp == "V"))

xd.psem.imp <- psem( # with random effect for Plot
  lmer(gs_z ~ CO2_z+meanSWC_z + Spp_num + (1|Plot), imp.1.1),
  lmer(Anet_z ~ CO2_z*meanSWC_z + Spp_num + (1|Plot), imp.1.1), # key: does not include gs
  lmer(totmass_z ~ Anet_z + meanSWC_z + Spp_num + (1|Plot), imp.1.1) # also includes SWC
)
plot(xd.psem.imp)

library(brms)
# problem with NAs

# impute missing data with mice package
library(mice)

imp <- df_final %>% 
  filter(!is.na(totmass)) %>% # drop 37 rows with NA for totmass
  select(Plot, Tmt, Spp, max_ht, max_dia, rootmass_g, totmass, CO2, meanSWC, Anet, gs) %>% 
  mice(m = 5, print = FALSE) # impute using all data, then center

imp.1 <- complete(imp) %>% # this chooses just the first complete imputation (of 5)
  mutate_if(is.numeric, list(z = zscore))

# fit Bayesian models
gs_mod <- bf(gs_z | mi() ~ CO2_z+meanSWC_z + Spp + (1|Plot)) # have been using this!
gs_mod1 <- bf(gs_z | mi() ~ CO2_z*meanSWC_z + Spp + (1|Plot)) 

Anet_mod <- bf(Anet_z | mi() ~ CO2_z*meanSWC_z + Spp + (1|Plot)) # have been using this !!
Anet_mod1 <- bf(Anet_z | mi() ~ CO2_z*meanSWC_z + gs_z + Spp + (1|Plot)) 
Anet_mod2 <- bf(Anet_z | mi() ~ CO2_z*meanSWC_z + Spp + (1|Plot)) 
Anet_mod3 <- bf(Anet_z | mi() ~ CO2_z+meanSWC_z + gs_z + Spp + (1|Plot)) 
Anet_mod4 <- bf(Anet_z | mi() ~ CO2_z+meanSWC_z + Spp + (1|Plot)) 

totmass_mod <- bf(totmass_z | mi() ~ Anet_z + meanSWC_z + Spp + (1|Plot)) 

oaks_fit_full <- brm(       gs_mod1 + # includes interaction term for CO2 and meanSWC
                            Anet_mod1 + # includes gs as a predictor for Anet, and interaction
                            totmass_mod +
                            set_rescor(FALSE),
                          data = imp.1,
                          cores = 4, chains = 4, save_pars = save_pars(all=TRUE) )

oaks_fit_red1 <- brm(    gs_mod + # no intxn
                         Anet_mod1 + # includes gs as a predictor for Anet, and intxn
                         totmass_mod +
                         set_rescor(FALSE),
                       data = imp.1,
                       cores = 4, chains = 4, save_pars = save_pars(all=TRUE) )

oaks_fit_red2 <- brm(      gs_mod + # no intxn
                           Anet_mod2 + # includes intx, no gs as a predictor for Anet
                           totmass_mod +
                           set_rescor(FALSE),
                         data = imp.1,
                         cores = 4, chains = 4, save_pars = save_pars(all=TRUE) )

oaks_fit_red3 <- brm(        gs_mod + # no intxn
                             Anet_mod3 + # includes gs as a predictor for Anet, no intxn
                             totmass_mod +
                             set_rescor(FALSE),
                           data = imp.1,
                           cores = 4, chains = 4, save_pars = save_pars(all=TRUE) )

oaks_fit_red4 <- brm(      gs_mod + # no intxn
                           Anet_mod4 + # no intxn, no gs
                           totmass_mod +
                           set_rescor(FALSE),
                         data = imp.1,
                         cores = 4, chains = 4, save_pars = save_pars(all=TRUE) )

# check plots, summaries

# compare 4 candidate models
BIC(oaks_fit_full, oaks_fit_red1, oaks_fit_red2, oaks_fit_red3, oaks_fit_red4)
AIC(oaks_fit_full, oaks_fit_red1, oaks_fit_red2, oaks_fit_red3, oaks_fit_red4)

#  AIC(oaks_fit_full, oaks_fit_red1, oaks_fit_red2, oaks_fit_red3, oaks_fit_red4)
#                      df        AIC
# oaks_fit_full -1.676071 -0.1888453
# oaks_fit_red1 -1.722879  1.7797561
# oaks_fit_red2 -3.414514 -1.1618040
# oaks_fit_red3 -2.051144  0.9844026
# oaks_fit_red4 -2.530801 -0.5708625
# Warning message:
#   In AIC.default(oaks_fit_full, oaks_fit_red1, oaks_fit_red2, oaks_fit_red3,  :
#                    models are not all fitted to the same number of observations

# def some problems here but lowest AICs are from oaks_fit_red2:
# Formula: gs_z | mi() ~ CO2_z + meanSWC_z + Spp + (1 | Plot)  # no intxn
#        Anet_z | mi() ~ CO2_z * meanSWC_z + Spp + (1 | Plot)  # intxn, no gs
#     totmass_z | mi() ~ Anet_z + meanSWC_z + Spp + (1 | Plot)  

WAIC(oaks_fit_full, oaks_fit_red1, oaks_fit_red2, oaks_fit_red3, oaks_fit_red4)
LOO(oaks_fit_full, oaks_fit_red1, oaks_fit_red2, oaks_fit_red3, oaks_fit_red4, reloo = TRUE)
# Model comparisons:
#               elpd_diff se_diff
# oaks_fit_red3   0.0       0.0  
# oaks_fit_red1  -0.4       0.7  
# oaks_fit_full  -1.4       1.6  
# oaks_fit_red4 -59.9      10.3  
# oaks_fit_red2 -60.3      10.9  

# also selects oaks_fit_red2 !

get_variables(oaks_fit_red2)
fixef(oaks_fit_red2)

plot(conditional_effects(oaks_fit_red2)[[11]], effects = "CO2_z:meanSWC_z", int_conditions = setNames(c(-.5,.5), c("W","D"))) + xlab("Standardized [CO2]") + ylab("Standardized Photosynthesis") + 
  scale_colour_manual(values=c("blue", "maroon", "red"))

# plot(conditional_effects(oaks_fit_red2)[[11]], int_conditions = setNames(c(-.5,.5), c("W","D")))

p <- plot(conditional_effects(oaks_fit_red2, effects = "CO2_z:meanSWC_z"), points = TRUE, point_args = aes(colour = "darkseagreen4"), plot = FALSE)[[2]]
       
conditional_effects(oaks_fit_red2)[[2]]    


library(viridis)
library(RColorBrewer)
p + scale_colour_brewer(palette = "RdBu", direction=-1) + scale_fill_brewer(palette = "RdBu", direction=-1) +
  xlab("Standardized [CO2]") + ylab("Standardized Photosynthesis") + theme_bw() 

oaks_fit_red2 %>%
  gather_draws( b_gsz_CO2_z, b_gsz_meanSWC_z, b_gsz_SppV, #) %>% 
   b_Anetz_CO2_z, b_Anetz_meanSWC_z, b_Anetz_SppV, b_totmassz_Anet_z, b_totmassz_meanSWC_z, b_totmassz_SppV) %>% 
 #  b_Anetz_CO2_z:meanSWC_z, ) %>% 
  ggplot(aes(y = .variable, x = .value)) +
  stat_halfeye() + 
  geom_vline(color= "red", linetype="dashed", xintercept= 0)



# this one keeps the totmass = NA rows
imp_all <- df_final %>% 
  select(Plot, Tmt, H2OTmt, Spp, max_ht, max_dia, rootmass_g, totmass, CO2, meanSWC, Anet, gs) %>% 
  mice(m = 5, print = FALSE) # impute using all data, then center

imp1.1 <- complete(imp1) %>% 
  mutate_if(is.numeric, list(z = zscore))

oaks_fit_brms1 <- brm(  gs_mod +
                         Anet_mod +
                         totmass_mod +
                         set_rescor(FALSE),
                       data = imp1.1,
                       cores = 4, chains = 4, save_pars = save_pars(all=TRUE) )

# in case it works
plot(oaks_fit_brms1)
summary(oaks_fit_brms1)


library(tidybayes)
library(ggplot2)

(model_fit <- imp1.1 %>%
    group_by(H2OTmt, Spp) %>% 
    add_predicted_draws(oaks_fit_brms1) %>%  # adding the posterior distribution
    ggplot(aes(x = CO2_z, y = Anet_z, color = ordered(H2OTmt), fill = ordered(H2OTmt))) +  
    stat_smooth(method= "loess") +
    geom_point(data = imp1.1, size = 3, aes(color=H2OTmt)) +   # raw data
    facet_wrap(~Spp) +
    scale_fill_manual(values = c("blue","red")) +
   # scale_color_brewer(palette = "Dark2") +
    theme_bw() +
    ylab("Photosynthetic Rate\n") + 
    xlab("\nCO2 (ppm)") +
    theme_bw() +
    theme(legend.title = element_blank(),
          legend.position = c(0.15, 0.85)))

# two ideas: impute THEN center?; first drop rows with totmass = NA

# df_final_mice2 <- df_final %>% 
#   filter(is.na(totmass)) %>%  # drop 37 rows with NA for totmass
#   mutate_if(is.numeric, list(z = zscore)) %>% 
#   select(gs_z, CO2_z, meanSWC_z, Spp, Plot, Anet_z, totmass_z)
# 
# imp2 <- mice(df_final_mice2, m = 5, print = FALSE)

imp3 <- df_final %>% 
  filter(!is.na(totmass)) %>% # drop 37 rows with NA for totmass
  select(Plot, Tmt, Spp, max_ht, max_dia, rootmass_g, totmass, CO2, meanSWC, Anet, gs) %>% 
  mice(m = 5, print = FALSE) # impute using all data, then center

imp3.1 <- complete(imp3) %>% 
  mutate_if(is.numeric, list(z = zscore))

oaks_fit_brms2 <- brm(  gs_mod +
                         Anet_mod +
                         totmass_mod +
                         set_rescor(FALSE),
                       data = imp3.1,
                       cores = 4, chains = 4, save_pars = save_pars(all=TRUE) )

plot(oaks_fit_brms2)
summary(oaks_fit_brms2)

# I will do one last brms, just for Q. wislizeni, with imputed data
sum(df_final$Spp =="L")
# [1] 81
 sum(df_final$Spp =="L" & !is.na(df_final$Anet))
# [1] 46

 gs_mod_1spp <- bf(gs_z | mi() ~ CO2_z+meanSWC_z + (1|Plot)) 
 Anet_mod_1spp <- bf(Anet_z | mi() ~ CO2_z*meanSWC_z + (1|Plot)) 
 totmass_mod_1spp <- bf(totmass_z | mi() ~ Anet_z + meanSWC_z + (1|Plot)) 
 
 imp4 <- df_final %>% 
   filter(Spp == "L") %>% 
   filter(!is.na(totmass)) %>% # drop 37 rows with NA for totmass
   select(Plot, Tmt, max_ht, max_dia, rootmass_g, totmass, CO2, meanSWC, Anet, gs) %>% 
   mice(m = 5, print = FALSE) # impute using all data, then center
 
 imp4.1 <- complete(imp4) %>% 
   mutate_if(is.numeric, list(z = zscore))
 
 oaks_fit_brmsL <- brm(  gs_mod_1spp +
                           Anet_mod_1spp +
                           totmass_mod_1spp +
                           set_rescor(FALSE),
                         data = imp4.1,
                         cores = 4, chains = 4, save_pars = save_pars(all=TRUE) )
 
 plot(oaks_fit_brmsL)
 summary(oaks_fit_brmsL)
 
 
 # same, for Q. lobata ?
 sum(df_final$Spp =="V")
 # [1] 69
 sum(df_final$Spp =="V" & !is.na(df_final$Anet))
 # [1] 36
 
 imp5 <- df_final %>% 
   filter(Spp == "V") %>% 
   filter(!is.na(totmass)) %>% # drop 37 rows with NA for totmass
   select(Plot, Tmt, max_ht, max_dia, rootmass_g, totmass, CO2, meanSWC, Anet, gs) %>% 
   mice(m = 5, print = FALSE) # impute using all data, then center
 
 imp5.1 <- complete(imp5) %>% 
   mutate_if(is.numeric, list(z = zscore))
 
 oaks_fit_brmsV <- brm(  gs_mod_1spp +
                           Anet_mod_1spp +
                           totmass_mod_1spp +
                           set_rescor(FALSE),
                         data = imp5.1,
                         cores = 4, chains = 4, save_pars = save_pars(all=TRUE) )
 
 plot(oaks_fit_brmsV)
 summary(oaks_fit_brmsV)
 
 # leftovers
 # oaks_fit_brms <- brm(    gs_mod + # no intxn
 #                          Anet_mod + # no intxn, no gs
 #                          totmass_mod +
 #                          set_rescor(FALSE),
 #                        data = imp.1,
 #                        cores = 4, chains = 4, save_pars = save_pars(all=TRUE) )
 
 
 # oaks_fit_brms <- brm(  gs_mod +
 #                        Anet_mod +
 #                        totmass_mod +
 #                        set_rescor(FALSE),
 #                     data = filter(df_final_z, !is.na(totmass_z)),
 #                     cores = 4, chains = 4, save_pars = save_pars(all=TRUE) )
 # oaks_fit_brms.intx <- brm(  gs_mod1 +
 #                          Anet_mod1 +
 #                          totmass_mod +
 #                          set_rescor(FALSE),
 #                        data = filter(df_final_z, !is.na(totmass_z)),
 #                        cores = 4, chains = 4, save_pars = save_pars(all=TRUE) )