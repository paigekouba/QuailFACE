# Thu 5/29/25
# Script to analyze findings from Quail Ridge CO2 and watering experiment on oak seedlings,
# to follow script "CO2_Oaks_5.29.25.R"
library(tidyverse)
library(piecewiseSEM)
library(lme4)
library(corrplot)

# New version after zooming with Xiaoli! (5/27/25)

# want watering treatment as a predictor; needs to be numeric
final_df.1 <- final_df %>% 
  mutate(herbivory = as.numeric(Code %in% c(firstfullherb$Code,"8L2", "6V2"))) %>% # 1 = herbivory, 0 = no
  mutate(H2O_num = as.numeric(H2OTmt == "W")) %>%  # creates a dummy variable where 1 = watered, 0 = dry %>% 
  mutate(Spp_num = as.numeric(Spp == "L")) # 1 = L, 0 = V

## centering variables usually usually improves convergence
## a nice side effect is that it makes the intercepts interpretable
## also, I am rescaling the variables. This means that, for example, 
##  the CO2 will be interepreted in hundreds units
final_df.1 <- final_df.1 |>
  dplyr::mutate(CO2_c = (CO2 - mean(CO2, na.rm = TRUE)) / 100) |>
  dplyr::mutate(meanSWC_c = (meanSWC - mean(meanSWC, na.rm = TRUE)) / 10) |>
  dplyr::mutate(Anet_c = (Anet - mean(Anet, na.rm = TRUE)) / 5) |>
  dplyr::mutate(gs_c = (gs - mean(gs, na.rm = TRUE)) / 5)

modlist <- psem(
  # gs is predicted by watering, CO2
  lmer(gs_c ~ CO2_c + H2O_num + (1|Plot), na.action = na.omit, final_df.1),
  # Anet is predicted by gs, CO2, SWC
  lmer(Anet_c ~ gs_c + CO2_c + meanSWC + (1|Plot), na.action = na.omit, final_df.1),
  # SWC is predicted by watering, gs
  lmer(meanSWC_c ~ H2O_num + gs_c + (1|Plot), na.action = na.omit, final_df.1),
  # biomass is predicted by Anet, herbivory
  lmer(totmass ~ Anet_c + herbivory + (1|Plot), na.action = na.omit, final_df.1)
)

final_df.2 <- final_df.1 |>
  dplyr::filter(!is.na(gs_c)) |>
  dplyr::filter(!is.na(totmass)) |> 
  mutate(herbivory = as.numeric(Code %in% c(firstfullherb$Code,"8L2", "6V2"))) %>%  # 1 = herbivory, 0 = no
  mutate(Spp_num = as.numeric(Spp == "L")) # 1 = L, 0 = V
  
modlist <- psem(
  # gs is predicted by watering, CO2
  lm(gs_c ~ CO2_c + H2O_num, na.action = na.omit, final_df.2),
  # Anet is predicted by gs, CO2, SWC
  lm(Anet_c ~ gs_c + CO2_c + meanSWC_c, na.action = na.omit, final_df.2),
  # SWC is predicted by watering, gs
  lm(meanSWC_c ~ H2O_num + gs_c, na.action = na.omit, final_df.2),
  # biomass is predicted by Anet, herbivory
  lm(totmass ~ Anet_c + herbivory, na.action = na.omit, final_df.2)
)

summary(modlist) # removed meanSWC model
plot(modlist)

#model.matrix(totmass ~ Anet + herbivory + Plot, final_df.1)

final_df.2 %>% 
  select(where(is.numeric)) %>% 
  cor() %>% 
  corrplot()

modlist1 <- psem(
  # gs is predicted by watering, CO2
#  lm(gs_c ~ CO2_c + H2O_num, na.action = na.omit, final_df.2),
  # Anet is predicted by gs, CO2, SWC
  lm(Anet_c ~ Spp_num + CO2_c + meanSWC_c, na.action = na.omit, final_df.2),
  # SWC is predicted by watering, gs
  lm(meanSWC_c ~ H2O_num , na.action = na.omit, final_df.2),
  # biomass is predicted by Anet, herbivory
  lm(totmass ~ meanSWC_c + Spp_num + Anet_c + herbivory, na.action = na.omit, final_df.2) ,
  # Spp predicts everything, including herbivory
   glm(herbivory ~ Spp_num, family="binomial", na.action = na.omit, final_df.2)
)
summary(modlist1)
plot(modlist1)

modlist1.1 <- psem(
  # gs is predicted by watering, CO2
  lm(gs_c ~ CO2_c + H2O_num + Spp_num, na.action = na.omit, final_df.2),
  # Anet is predicted by gs, CO2, SWC
  lm(Anet_c ~ gs_c  + CO2_c + meanSWC_c, na.action = na.omit, final_df.2),
  # SWC is predicted by watering, gs
  lm(meanSWC_c ~ H2O_num + gs_c, na.action = na.omit, final_df.2),
  # biomass is predicted by Anet, herbivory
  lm(totmass ~ meanSWC_c + gs_c + Spp_num + Anet_c + herbivory, na.action = na.omit, final_df.2) ,
  # Spp predicts everything, including herbivory
  glm(herbivory ~ Spp_num + meanSWC_c + CO2_c, family="binomial", na.action = na.omit, final_df.2)
)
summary(modlist1.1)
plot(modlist1.1)

# leave out H2O_num
modlist1.2 <- psem(
  # gs is predicted by watering, CO2
  #  lm(gs_c ~ CO2_c + H2O_num, na.action = na.omit, final_df.2),
  # Anet is predicted by gs, CO2, SWC
  lm(Anet_c ~ Spp_num + CO2_c + meanSWC_c, na.action = na.omit, final_df.2),
  # SWC is predicted by watering, gs
  lm(meanSWC_c ~ H2O_num , na.action = na.omit, final_df.2),
  # biomass is predicted by Anet, herbivory
  lm(totmass ~ meanSWC_c + Spp_num + Anet_c + herbivory, na.action = na.omit, final_df.2) ,
  # Spp predicts everything, including herbivory
  glm(herbivory ~ Spp_num, family="binomial", na.action = na.omit, final_df.2)
)
summary(modlist1)
plot(modlist1)


# what about a random effect for block?
final_df.3 <- final_df.2 %>% 
  mutate(Block = as.character(case_when(
    Plot %in% c("15", "16", "1","2") ~ 1,
    Plot %in% c("3", "4", "5","6") ~ 2,
    Plot %in% c("7", "8", "9","10") ~ 3,
    Plot %in% c("11", "12", "13","14") ~ 4
  )))
modlist4 <- psem(
  # gs is predicted by watering, CO2
  lmer(gs_c ~ CO2_c + H2O_num + Spp_num + (1|Block), na.action = na.omit, final_df.3),
  # Anet is predicted by gs, CO2, SWC
  lmer(Anet_c ~ gs_c + CO2_c + meanSWC_c + Spp_num + (1|Block), na.action = na.omit, final_df.3),
  # # SWC is predicted by watering, gs
  # lmer(meanSWC_c ~ H2O_num + gs_c + (1|Block), na.action = na.omit, final_df.3),
  # biomass is predicted by Anet, herbivory
  lmer(totmass ~ Anet_c + herbivory + Spp_num + (1|Block), na.action = na.omit, final_df.3),
  # herbivory is predicted by watering, species
  glmer(herbivory ~ H2O_num + Spp_num + (1|Block), family="binomial", final_df.3)
)
summary(modlist4)



modlist2 <- psem(
  # gs is predicted by CO2, SWC, spp
  lm(gs_c ~ CO2_c + meanSWC_c + Spp_num, final_df.2),
  # Anet is predicted by gs, CO2, SWC, spp
  lm(Anet ~ gs_c + CO2_c + meanSWC_c + Spp_num, final_df.2),
  # totmass is predicted by Anet, herbivory, spp
  lm(totmass ~ Anet_c + herbivory + Spp_num, final_df.2),
  # herbivory is predicted by SWC, spp
  glm(herbivory ~ Spp_num, family = "binomial", final_df.2),
  herbivory %~~% gs_c
)
summary(modlist2, conserve = TRUE)


# what about the larger dataset with only analyses for non Anet variables?

modlist3 <- psem(
  # totmass is predicted by CO2, SWC, herbivory, spp
  lm(totmass ~ CO2_c + meanSWC_c + herbivory + Spp_num, final_df.1),
  # herbivory is predicted by SWC, spp
  glm(herbivory ~ Spp_num + meanSWC_c, family = "binomial", final_df.1)
)
summary(modlist3)
plot(modlist3)

# mortality data ?
# 16 seedlings IDed as dead by the final inventory
# how much of their data might I recover and add to the "biomass" df?
names(biomass2)
# [1] "Plot"  "Tmt"     "Code"     "StemWet_g"   "LeafWet_g"   "LeafDry_g"   "Spp"         "SeedMass.g."
# [9] "Cond..7"   "rootmass_g"  "Ht.mm..1"    "Ht.mm..2"    "Ht.mm..3"    "Ht.mm..4"    "Ht.mm..5"    "Ht.mm..6"
# [17] "Ht.mm..7"    "Ht.mm..8"    "H2OTmt"      "totmass"     "mDeltaTest"  "CO2"         "meanSWC"

biomass2 %>% 
  filter(Code %in% c(if_else(nchar(mort_df$Code)==4,substr(mort_df$Code,1,3),substr(mort_df$Code,1,4)))) %>% nrow() # 124 of the final biomass seedlings are in the mortality dataset
mort_df %>% 
  mutate(shortcode = if_else(nchar(Code)==4,substr(Code,1,3),substr(Code,1,4))) %>% 
  filter(shortcode %in% biomass2$Code) %>% nrow() # 132 of mort_df seedlings are in biomass dataset
# missing at least 10 ?

# join biomass_raw with mort_df
biomass_mort <- biomass_raw %>% 
  filter(Code %in% mort_df$Code) %>% 
  right_join(select(mort_df, Code, mortality)) %>% 
  dplyr::select(Code, StemWet_g, LeafWet_g, LeafDry_g, mortality) %>% 
  left_join(dplyr::select(rootmass_raw, Code, Plot, Spp, SeedMass.g., Cond..7, rootmass_g))  %>% 
  mutate(across(c(StemWet_g, LeafWet_g, LeafDry_g), na_if, -Inf))

lookup <- data.frame(as.character(c(1:16)), c("AW","ED","AD","EW",
                                              "AW","ED","EW","AD",
                                              "ED","AW","EW","AD",
                                              "ED","AW","EW","AD"))
names(lookup) <- c("Plot","Tmt")

biomass_mort <- merge(lookup, biomass_mort, by = 'Plot')

biomass_mort2 <- biomass_mort %>% # biomass data with full dataset and extra inventory columns
  mutate(Code = if_else(nchar(Code)==4,substr(Code,1,3),substr(Code,1,4))) %>% 
  left_join(inventory_thinned[,c("Code","Ht.mm..1", "Ht.mm..2", "Ht.mm..3", "Ht.mm..4", "Ht.mm..5", "Ht.mm..6", "Ht.mm..7", "Ht.mm..8")]) %>% 
  mutate(across(c(StemWet_g, LeafWet_g, LeafDry_g, rootmass_g, Ht.mm..1, Ht.mm..2, Ht.mm..3, Ht.mm..4, Ht.mm..5, Ht.mm..6, Ht.mm..7, Ht.mm..8), na_if, -Inf))  %>% 
  mutate(H2OTmt = substr(Tmt,2,2)) %>% 
  mutate(totmass = rootmass_g + StemWet_g + LeafWet_g) %>% 
  left_join(plot_CO2, by = "Plot") %>% 
  left_join(plot_SWC, by = "Plot") 

biomass_raw %>% 
  filter(Code %in% mort_df$Code) %>% View()
# can get StemWet_g, LeafWet_g, LeafDry_g
rootmass_raw %>% 
  filter(Code %in% mort_df$Code) %>% View()
# and rootmass from here, therefore totmass... mutate(totmass = rootmass_g + StemWet_g + LeafWet_g)
inventory_raw %>% 
  filter(Code %in% mort_df$Code) %>% View()

# get final ht for dead ones? group_by Code, pick latest date, return ht
# come back to that

# want watering treatment as a predictor; needs to be numeric
final_df_mort <- final_df_mort %>% 
  mutate(herbivory = as.numeric(Code %in% c(firstfullherb$Code,"8L2", "6V2"))) %>% # 1 = herbivory, 0 = no
  mutate(H2O_num = as.numeric(H2OTmt == "W")) %>%  # creates a dummy variable where 1 = watered, 0 = dry %>% 
  mutate(Spp_num = as.numeric(Spp == "L")) # 1 = L, 0 = V

## centering variables usually usually improves convergence
## a nice side effect is that it makes the intercepts interpretable
## also, I am rescaling the variables. This means that, for example, 
##  the CO2 will be interepreted in hundreds units
final_df_mort <- final_df_mort |>
  dplyr::mutate(CO2_c = (CO2 - mean(CO2, na.rm = TRUE)) / 100) |>
  dplyr::mutate(meanSWC_c = (meanSWC - mean(meanSWC, na.rm = TRUE)) / 10) |>
  dplyr::mutate(Anet_c = (Anet - mean(Anet, na.rm = TRUE)) / 5) |>
  dplyr::mutate(gs_c = (gs - mean(gs, na.rm = TRUE)) / 5)

modlist_mort <- psem(
  # gs is predicted by watering, CO2
  lm(gs_c ~ CO2_c + meanSWC_c + Spp_num, na.action = na.omit, final_df_mort),
  # Anet is predicted by gs, CO2, SWC
  lm(Anet_c ~ gs_c + Spp_num + CO2_c + meanSWC_c, na.action = na.omit, final_df_mort),
  # SWC is predicted by watering, gs
#  lm(meanSWC_c ~ H2O_num , na.action = na.omit, final_df_mort),
  # biomass is predicted by Anet, herbivory
  glm(mortality ~ CO2_c, family="binomial", na.action = na.omit, final_df_mort) ,
  # Spp predicts everything, including herbivory
  glm(herbivory ~ Spp_num + meanSWC_c, family="binomial", na.action = na.omit, final_df_mort)
)
summary(modlist_mort, conserve=T)
plot(modlist_mort)

modlist_ht <- psem(
  # gs is predicted by watering, CO2
  lm(gs_c ~ CO2_c + meanSWC_c + Spp_num, na.action = na.omit, final_df_mort),
  # Anet is predicted by gs, CO2, SWC
  lm(Anet_c ~ gs_c + Spp_num + CO2_c + meanSWC_c, na.action = na.omit, final_df_mort),
  # SWC is predicted by watering, gs
  #  lm(meanSWC_c ~ H2O_num , na.action = na.omit, final_df_mort),
  # biomass is predicted by Anet, herbivory
  glm(Ht.mm..8 ~ CO2_c + meanSWC_c + herbivory + Spp_num, family="binomial", na.action = na.omit, final_df_mort) ,
  # Spp predicts everything, including herbivory
  glm(herbivory ~ Spp_num + meanSWC_c, family="binomial", na.action = na.omit, final_df_mort)
)
summary(modlist_ht, conserve=T)
plot(modlist_ht)
