# Thu 6/5/25
# figures for GRS Talk + Poster

# plan right now is to use:
# hypothesis plots
# dot plots with imputed data (Q. wislizeni)

# cross plots (with 1:1 line), to show that WUE is slightly increased, but not more for dry plants
# poster only, probably. raw data. goal: add raw data points as x's

# z score plot showing *change with CO2*: distance from 0 is [change from now to 2050], and only blue dots get Anet boost of large size. (but what about the mass variables?)

# DAG ?

# something to wrap up.

# first, imputed data
library(mice)
library(ggeffects)
library(lme4)
library(lmerTest)

zscore <- function(x){
  (x - mean(x, na.rm = T))/sd(x, na.rm = T)
}

# imp <- df_final %>% 
#   filter(!is.na(totmass)) %>% # drop 37 rows with NA for totmass
#   select(Plot, Tmt, Spp, max_ht, max_dia, rootmass_g, totmass, CO2, meanSWC, Anet, gs) %>% 
#   mice(m = 10, print = FALSE) # impute using all data, then center

# ONLY RUN THIS ONCE AND SET SEED BEFORE; needs a sensitivity analysis

# note for later: this step affects results a lot

# imp.1 <- complete(imp, 2) %>% # this chooses just the nth complete imputation (of 5)

# this one is currently not allowing psem to converge:
# imp.1 <- complete(imp, 2) %>% # this chooses just the nth complete imputation (of 5, as default)
  
# suspect I was using all 150 with this below:
imp_all <- df_final %>% 
  select(Plot, Tmt, H2OTmt, Spp, max_ht, max_dia, rootmass_g, totmass, CO2, meanSWC, Anet, gs) %>% 
  mice(m = 5, print = FALSE) # impute using all data, then center

imp.1 <- complete(imp_all, 4) %>% # this chooses just the nth complete imputation (of 5, as default)
mutate_if(is.numeric, list(z = zscore))

imp.1.1 <- imp.1 %>% 
  mutate(Spp_num = as.numeric(Spp == "V"))

imp.1.2 <- # imputed data for Ls only, with watering treatment variable
  imp.1 %>% 
  filter(Spp == "L") %>% 
  mutate(H2OTmt = as.numeric(Tmt == "AW" | Tmt == "EW"))

# STOP RIGHT THERE I AM PRINTING/SAVING imp.1 RIGHT NOW because I once again forgot to set the seed
imp.1 %>% View()

# hypothesis plots: made in powerpoint/google slides
# Anet dot plot with imputed data (Q. wislizeni)
ggpredict(lmer(Anet~rescale(CO2)*rescale(meanSWC)+(1|Plot), data=filter(imp.1, Spp=="L")),
          terms=c("CO2","meanSWC [4,42]"))%>% plot(rawdata=T,ci=T,jitter=2, colors=c("red","blue")) + labs(title="Anet (µmol CO2/m2/s), Live Oak") + theme_classic()
summary(lmer(Anet~rescale(CO2)*rescale(meanSWC)+(1|Plot), data=filter(imp.1, Spp=="L")))
library(lmerTest)
# I think I was very very lucky on my first result:
# Fixed effects:
#   Estimate Std. Error     df t value Pr(>|t|)   
#   (Intercept)                      3.189      1.090 10.159   2.925  0.01493 * 
#   rescale(CO2)                     1.338      2.078  8.205   0.644  0.53729   
#   rescale(meanSWC)                 3.071      2.050 13.171   1.498  0.15781   
#   rescale(CO2):rescale(meanSWC)   16.770      4.200 10.986   3.993  0.00212 **

# here is the new model summary, after R crashed and I was forced to redo the mice step:
# Fixed effects:
#   Estimate Std. Error      df t value Pr(>|t|)  
# (Intercept)                     3.1442     1.3998  8.6596   2.246   0.0524 .
# rescale(CO2)                    0.5575     2.7028  7.2718   0.206   0.8422  
# rescale(meanSWC)                4.6541     2.5966 10.6258   1.792   0.1015  
# rescale(CO2):rescale(meanSWC)  12.4202     5.3724  9.0804   2.312   0.0459 *

# now with imp_all(4):
# Estimate Std. Error      df t value Pr(>|t|)   
#   (Intercept)                     3.6321     0.9584  7.8796   3.790  0.00546 **
#   rescale(CO2)                    0.0762     1.9235  7.4989   0.040  0.96943   
#   rescale(meanSWC)                5.3336     1.8137 10.8572   2.941  0.01360 * 
#   rescale(CO2):rescale(meanSWC)  12.8406     3.7082  8.3604   3.463  0.00797 **

# biomass  dot plot with imputed data (Q. wislizeni)
ggpredict(lmer(totmass~rescale(CO2)*rescale(meanSWC)+(1|Plot), data=filter(imp.1, Spp=="L")),
          terms=c("CO2","meanSWC [4,42]"))%>% plot(rawdata=T,ci=T,jitter=2, colors=c("red","blue")) + labs(title="Total mass (g), Live Oak") + theme_classic()

# cross plots with 1:1 line, to show that WUE is slightly increased, but not more for dry plants
# raw data to show gs and Anet highly correlated
# cross plots with measured data for Ls
# functions
mean_narm <- function(x){
  mean(x, na.rm=TRUE)   }

minus_se <- function(x){
  mean(x, na.rm=TRUE) - (sd(x, na.rm = TRUE)/sqrt(length(x[!is.na(x)]))) }

plus_se <- function(x){
  mean(x, na.rm=TRUE) + (sd(x, na.rm = TRUE)/sqrt(length(x[!is.na(x)]))) }  

library(ggnewscale)
df_final %>% 
  #    imp.1 %>% 
  filter(Spp=="L") %>% 
  select(gs, Anet, Tmt, Spp) %>% 
  group_by(Tmt, Spp) %>% 
  summarise_if(is.numeric, list(y=mean_narm, ymin=minus_se, ymax=plus_se)) %>% 
  ggplot(aes(x=gs_y, y=Anet_y, fill=Tmt)) + 
  geom_abline(slope = 100, intercept = 0, linetype="dashed") +
  geom_errorbar(aes(x=gs_y, ymin = Anet_ymin, ymax = Anet_ymax, color=Tmt), width = 0.001) +
  geom_errorbarh(aes(y=Anet_y, xmin = gs_ymin, xmax = gs_ymax, color=Tmt), height = 0.2) +
  geom_point(filter(df_final, Spp=="L"), mapping=aes(x=gs, y=Anet, color=Tmt, shape=Tmt, fill=Tmt), size=1.5, alpha = 0.75) +
  scale_shape_manual(values = c(21, 21, 16, 16)) +
  xlab(label = "Stomatal Conductance (mol H2O/m2/s)") +
  ylab(label = "Photosynthesis (µmol CO2/m2/s)") +
 # scale_x_continuous(expand = c(0,0), limits=c(0,0.14)) + scale_y_continuous(expand = c(0,0), limits=c(0,16)) +
  scale_color_manual(values = c("red", "blue", "red", "blue")) + 
  ggnewscale::new_scale("shape") +
  geom_point(aes(fill=Tmt, color=Tmt), size = 3.5, shape=23) +
  scale_fill_manual(values =c ("white","white","red","blue")) +
  theme_classic(base_size = 16)
# shows that WUE increases slightly  with eCO2, but at similar levels for wet and dry plants
# shows that eCO2 causes increased Anet, but much stronger for watered group
# shows that eCO2 causes *increased*, not decreased gs

# what is N per Tmt group, here?
df_final %>% 
  #  filter(! Code %in% firstfullherb$Code) %>% 
  filter(Spp=="L") %>% 
  group_by(Tmt, H2OTmt) %>% 
  select(gs, Anet) %>% 
#  tally() %>% 
    summarise_if(is.numeric, ~ sum(!is.na(.x))) %>% 
  group_by(Tmt) %>% 
  summarise_if(is.numeric, ~ min(.x)) %>% 
  t() %>% data.frame()

# Tmt      AD AW ED EW
# gs       12 10 12 12
# Anet     12 10 12 12

# z score plot showing *change with CO2*: distance from 0 is [change from now to 2050], and only blue dots get Anet boost of large size. (but what about the mass variables?)

## z-score change with CO2
variable_order <- c("Anet", "gs", "WUE", "tot.mass", "max.ht", "max.dia")
fig2_nequals <- df_final %>% 
  filter(! Code %in% firstfullherb$Code) %>% 
  filter(Spp=="L") %>% 
  select(Tmt, H2OTmt, totmass, max_ht, max_dia, Anet, gs, WUE) %>% 
  rename(tot.mass = totmass) %>% 
  rename(max.ht = max_ht) %>% 
  rename(max.dia = max_dia) %>% 
  group_by(Tmt, H2OTmt) %>% 
  summarise_if(is.numeric, ~ sum(!is.na(.x))) %>% 
  group_by(H2OTmt) %>% 
  summarise_if(is.numeric, ~ min(.x)) %>%
  t() %>% data.frame()

fig2_nequals <- fig2_nequals[-1,]
fig2_nequals[,3] <- rownames(fig2_nequals)
colnames(fig2_nequals) <- c("Dry","Wet", "variable")

fig2_boot <- df_final %>% 
  filter(Spp=="L") %>% 
  select(Tmt, totmass, max_ht, max_dia, Anet, gs, WUE) %>%
  rename(tot.mass = totmass) %>% 
  rename(max.ht = max_ht) %>% 
  rename(max.dia = max_dia)

# I want to standardize all the responses as z scores
# then find the ∆z with CO2

zscore <- function(x){
  (x - mean(x, na.rm = T))/sd(x, na.rm = T)
}

fig2_boot <- fig2_boot %>% mutate_if(is.numeric, list(z = zscore)) 

# start with the mean of ∆z with CO2
zmean <- data.frame("ED1" = c(1:6), "EW1" = c(1:6)) # initialize results df
for(i in c(2:7)){zmean[(i-1),1] <-          # this is saying each of the 6 variables gets 1 row, and this is defining the first column in the results df
  # the top part is to get the mean ∆z, with CO2, for dry plants
  mean(
    do.call(c,lapply(1:1000, function(boot){
      a <- sample(unlist(fig2_boot[fig2_boot$Tmt=="ED",i+6])[!is.na(unlist(fig2_boot[fig2_boot$Tmt=="ED",i+6]))], replace = T)
      b <- sample(unlist(fig2_boot[fig2_boot$Tmt=="AD",i+6])[!is.na(unlist(fig2_boot[fig2_boot$Tmt=="AD",i+6]))], replace = T)
      mean(a)-mean(b)
    })))
# the second part is to get the mean ∆z, with CO2, for watered plants 
zmean[(i-1),2] <- mean(
  do.call(c,lapply(1:1000, function(boot){
    a <- sample(unlist(fig2_boot[fig2_boot$Tmt=="EW",i+6])[!is.na(unlist(fig2_boot[fig2_boot$Tmt=="EW",i+6]))], replace = T)
    b <- sample(unlist(fig2_boot[fig2_boot$Tmt=="AW",i+6])[!is.na(unlist(fig2_boot[fig2_boot$Tmt=="AW",i+6]))], replace = T)
    mean(a)-mean(b)
  })))
}

# then mean - sd
zmin <- data.frame("ED1" = c(1:6), "EW1" = c(1:6)) # initialize results df
for(i in c(2:7)){ x <- do.call(c,lapply(1:1000, function(boot){
  a <- sample(unlist(fig2_boot[fig2_boot$Tmt=="ED",i+6])[!is.na(unlist(fig2_boot[fig2_boot$Tmt=="ED",i+6]))], replace = T)
  b <- sample(unlist(fig2_boot[fig2_boot$Tmt=="AD",i+6])[!is.na(unlist(fig2_boot[fig2_boot$Tmt=="AD",i+6]))], replace = T)
  mean(a)-mean(b)
}))
zmin[(i-1),1] <- mean(x) - sd(x)

y <- do.call(c,lapply(1:1000, function(boot){
  a <- sample(unlist(fig2_boot[fig2_boot$Tmt=="EW",i+6])[!is.na(unlist(fig2_boot[fig2_boot$Tmt=="EW",i+6]))], replace = T)
  b <- sample(unlist(fig2_boot[fig2_boot$Tmt=="AW",i+6])[!is.na(unlist(fig2_boot[fig2_boot$Tmt=="AW",i+6]))], replace = T)
  mean(a)-mean(b)
}))
zmin[(i-1),2] <- mean(y) - sd(y)
}

# then mean + sd
zmax <- data.frame("ED1" = c(1:6), "EW1" = c(1:6)) # initialize results df
for(i in c(2:7)){ x <- do.call(c,lapply(1:1000, function(boot){
  a <- sample(unlist(fig2_boot[fig2_boot$Tmt=="ED",i+6])[!is.na(unlist(fig2_boot[fig2_boot$Tmt=="ED",i+6]))], replace = T)
  b <- sample(unlist(fig2_boot[fig2_boot$Tmt=="AD",i+6])[!is.na(unlist(fig2_boot[fig2_boot$Tmt=="AD",i+6]))], replace = T)
  mean(a)-mean(b)
}))
zmax[(i-1),1] <- mean(x) + sd(x)

y <- do.call(c,lapply(1:1000, function(boot){
  a <- sample(unlist(fig2_boot[fig2_boot$Tmt=="EW",i+6])[!is.na(unlist(fig2_boot[fig2_boot$Tmt=="EW",i+6]))], replace = T)
  b <- sample(unlist(fig2_boot[fig2_boot$Tmt=="AW",i+6])[!is.na(unlist(fig2_boot[fig2_boot$Tmt=="AW",i+6]))], replace = T)
  mean(a)-mean(b)
}))
zmax[(i-1),2] <- mean(y) + sd(y)
}

z_bootL_CO2 <- cbind(rbind(zmean, zmin, zmax), 
                     data.frame("variable" = rep(c("tot.mass", "max.ht", "max.dia", "Anet", "gs", "WUE"),3), 
                                "y" = rep(c("y","ymin","ymax"), each=6))) %>% 
  rename(dry=ED1, wet=EW1) %>% 
  pivot_longer(cols=c("dry","wet"), names_to="Treatment", values_to="value") %>% 
  select(variable, y, Treatment, value) %>% 
  pivot_wider(names_from = "y", values_from = "value") %>% 
  ggplot() +
  geom_abline(color= "darkgray", linetype="dashed", slope = 0, intercept= 0) +
  geom_pointrange(aes(x=factor(variable, level=variable_order), y=y, ymin=ymin, ymax=ymax, group=Treatment, color=Treatment, shape=Treatment), size=1, linewidth=1, position=position_dodge(width=0.2)) + scale_color_manual(values=c("red","blue")) + scale_shape_manual(values = c(16,16)) +
  ylim(-1, 1.6) +
  geom_text(data = fig2_nequals, aes(x = variable, y = -.75, label = paste0("n = ",Dry)), color="red", size = 5) +
  geom_text(data = fig2_nequals, aes(x = variable, y = -.95, label = paste0("n = ",Wet)), color="blue", size = 5) +
  ggtitle("Quercus wislizeni (live oak)") +
  ylab("change with CO2") + xlab("Plant Response") +
  theme_classic(base_size = 20) 


# DAG Time

library(mice)
library(piecewiseSEM)

# imp <- df_final %>% 
#   filter(!is.na(totmass)) %>% # drop 37 rows with NA for totmass
#   select(Plot, Tmt, Spp, max_ht, max_dia, rootmass_g, totmass, CO2, meanSWC, Anet, gs) %>% 
#   mice(m = 5, print = FALSE) # impute using all data, then center
# 
# imp.1 <- complete(imp) %>% # this chooses just the first complete imputation (of 5)
#   mutate_if(is.numeric, list(z = zscore))

imp.1.1 <- imp.1 %>% 
  mutate(Spp_num = as.numeric(Spp == "V"))

imp.1.2 <- # imputed data for Ls only, with watering treatment variable
  imp.1 %>% 
  filter(Spp == "L") %>% 
  mutate(H2OTmt = as.numeric(Tmt == "AW" | Tmt == "EW"))

# why does it keep saying gs predicts totmass?
ggplot(imp.1.1, aes(x= gs, y=totmass, color=Spp)) + geom_point()
# maybe within a Spp high gs is correlated with high mass, but not sure why?
# I bet this is the imputation coming back out as an effect !
ggplot(df_final_z, aes(x= gs, y=totmass)) + geom_point() + geom_smooth() # no?
# the raw-data version of this SEM had a weaker but still significant gs->totmass

# plot as random effect
simple.psem.er <- psem( 
  # gs is predicted by CO2*SWC, Spp
  lmer(gs_z ~ CO2_z + meanSWC_z + Spp_num + (1|Plot), imp.1.1),
  # Anet is predicted by gs, CO2*SWC, Spp
  lmer(Anet_z ~ CO2_z * meanSWC_z + Spp_num + (1|Plot), imp.1.1),
  # biomass is predicted by Anet, herbivory
  lmer(totmass_z ~ Anet_z + Spp_num + (1|Plot), imp.1.1),
  gs_z %~~% totmass_z,
  gs_z %~~% Anet_z
) # estimates the total effect of CO2 x SWC on Anet
summary(simple.psem.er)
plot(simple.psem.er)
# gs_z is not significant if you leave Anet out of the totmass model--indicates we have suppression because of high correlation between Anet and gs! 

# I have dropped gs from my Anet model above, because they are highly correlated and including gs soaks up all the variation for Anet, meaning the effects of CO2xSWC get washed out:
car::crPlots(lm(totmass_z ~ Anet_z + Spp_num + gs_z, data = imp.1.1))

imp.1.1 %>% 
  select(max_ht_z:gs_z) %>% 
  select(where(is.numeric)) %>% 
  cor(use = "pairwise.complete.obs") %>% 
  corrplot::corrplot()

# OK we established that Anet and gs are (rightfully) highly correlated, so that is why the d-sep test was showing such an important path from gs to mass (suppressor effect, where we controlled for a mediator, Anet, and a variable ends up looking like it has a strong [and reverse-sign] effect on outcome. gs does NOT correlate with mass in the raw data, but the relationship between gs and mass is negative conditional on Anet: Anet suppresses shared variance, making gs seem like a impt predictor of mass.) so, model residual covariance between gs and mass, specifying that it is not a causal path

# Here below, as a separate question, I will ask whether gs mediates CO2 effects on Anet
model_mediation <- psem(
  # Step 1: gs as a mediator
  lmer(gs_z ~ CO2_z * meanSWC_z + Spp_num + (1|Plot), data = imp.1.1),
  
  # Step 2: Anet as the outcome
  lmer(Anet_z ~ gs_z + CO2_z + meanSWC_z + Spp_num + (1|Plot), data = imp.1.1)
)
summary(model_mediation)
plot(model_mediation)

simple.psem.Lintx <- psem( 
  # gs is predicted by CO2*SWC
  lmer(gs_z ~ CO2_z * meanSWC_z + (1|Plot), imp.1.2),
  # Anet is predicted by gs, CO2*SWC
  lmer(Anet_z ~ CO2_z * meanSWC_z + (1|Plot), imp.1.2),
  # biomass is predicted by Anet, herbivory
  lmer(totmass_z ~ Anet_z + (1|Plot), imp.1.2),
  gs_z %~~% totmass_z,
  gs_z %~~% Anet_z
) 
summary(simple.psem.Lintx) # intx is significant for gs with p = 0.04
plot(simple.psem.Lintx) 
AIC(simple.psem.L, simple.psem.Lintx)

# this might be the one; come back, draw a DAG and test against data
# will need a cooler name for this...
# a DAG and data dag consistency test for simple.psem.Lint
# a psem for, gs, Anet, and totmass of JUST live oaks, with residual correlation of gs, totmass; and gs, Anet
# see below

# DAG for totmass on just L, with intx for gs and Anet
# real variable names for testing against data
dag1 <- dagitty("dag {
  CO2_z -> gs_z
  CO2_z -> Anet_z
  meanSWC_z -> gs_z
  meanSWC_z -> Anet_z
  Anet_z -> totmass_z
  gs_z <-> totmass_z
  gs_z <-> Anet_z
}") 

dag1_pretty <- dagitty("dag {
  CO2 -> gs
  CO2 -> Anet
  H2O -> gs
  H2O -> Anet
  Anet -> mass
  CO2xH2O -> Anet
  CO2xH2O -> gs
  CO2 -> CO2xH2O
  H2O -> CO2xH2O
  gs <-> mass
  gs <-> Anet
}") 

coordinates(dag1_pretty) <- list(
  x = c(CO2 = 0, H2O = 0, CO2xH2O = 0.5, gs = 1, Anet = 1, mass = 1),
  y = c(CO2 = 3, H2O = 2, CO2xH2O = 2.5, gs = 3, Anet = 2, mass = 1)
)

# Plot the DAG using ggdag for a nice visual
ggdag(dag1_pretty, layout = "manual", text_size=2.5) +
  theme_classic() +
  ggtitle("CO2 and H2O Effects on Plant Mass via Anet") +
  theme(plot.title = element_text(size = 14, face = "bold"))

# test the DAG against the data
# test the DAG against data
localTests(
  x = dag1,
  #  data = select(drop_na(final_df2.1), where(is.numeric))
  data = select(imp.1.2, CO2_z, gs_z, Anet_z, meanSWC_z, totmass_z), # imputed data for Ls only, with watering treatment variable
  abbreviate.names = F)

plotLocalTestResults(localTests(dag1, 
                                data = select(imp.1.2, CO2_z, gs_z, Anet_z, meanSWC_z, totmass_z)))
# this DAG is all right for the data, once we do the %~~% thing for gs on mass and Anet
# it suggests a missing dependency direclty from SWC to totmass, conditional on Anet. I accept and ignore for now

impliedConditionalIndependencies(dag1)