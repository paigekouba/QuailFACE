# Thu 2/29/24
# Preliminary Model for ESA Abstract
library(lme4)
library(lubridate)

# df_all (from A_Ci_Survey3.R) has all the variables I need
# for each species, try this model as the full model:
# Anet ~ CO2*SWC + time + (1|plot)        (later might add herbivory yes/no)

# names(df_all)
# [1] "HHMMSS"    "Photo.x"   "Cond"      "Ci.x"      "CO2R"      "SWC"       "Date"      "Log"       "X."      
# [10] "Time"      "ID"        "Plot"      "Treatment" "Spp"       "interpol" 

# need to separate A v E and W v D
df_all$CO2Tmt <- substring(df_all$Treatment, 1,1)
df_all$H2OTmt <- substring(df_all$Treatment, 2,2)

# need to make HHMMSS a time
df_all$HHMMSS <- parse_date_time(df_all$HHMMSS, orders = c("HMS"), tz = "America/Los_Angeles")

df_all <- df_all %>% 
  filter(!is.na(interpol))

ggplot(data = df_all, aes(x=Treatment, y=interpol, color = Treatment)) +
  geom_jitter() +
  facet_wrap(~ Spp)

ggplot(data = df_all, aes(x=Treatment, y=WUE, color = Treatment)) +
  geom_jitter() +
  facet_wrap(~ Spp)

ggplot(data = df_all, aes(x=Treatment, y=Cond, color = Treatment)) +
  geom_jitter() +
  facet_wrap(~ Spp)

df_L <- filter(df_all, Spp == "L")
df_V <- filter(df_all, Spp == "V")

# full_L <- glmer(interpol ~ CO2Tmt*SWC + (1|Plot), data = df_L)
full_L <- glm(interpol ~ CO2Tmt*SWC, data = df_L)
red_L <- glmer(interpol ~ CO2Tmt + SWC + (1|Plot), data = df_L)


summary(full_L)
summary(red_L)
anova(full_L, red_L)

# full_V <- glmer(interpol ~ CO2Tmt*SWC + (1|Plot), data = df_V)
full_V <- glm(interpol ~ CO2Tmt*SWC, data = df_V)
red_V <- glmer(interpol ~ CO2Tmt + SWC + (1|Plot), data = df_V)


summary(full_V)
summary(red_V)
anova(full_V, red_V)


#full_all <- glmer(interpol ~ CO2Tmt*SWC + (1|Plot), data = df_all)
full_all <- glm(interpol ~ CO2Tmt*SWC, data = df_all)
red_all <- glmer(interpol ~ CO2Tmt + SWC + (1|Plot), data = df_all)
summary(full_all)
summary(red_all)
anova(full_all, red_all)

lm_all <- lm(interpol ~ CO2Tmt*SWC, data = df_all)
summary(lm_all)
lm_L <- lm(interpol ~ CO2Tmt*SWC, data = df_L)
summary(lm_L)
lm_V <- lm(interpol ~ CO2Tmt*SWC, data = df_V)
summary(lm_V)


full_cat <- lm(interpol ~ CO2Tmt*H2OTmt, data = df_all)
summary(full_cat)

L_cat <- lm(interpol ~ CO2Tmt*H2OTmt, data = df_L)
summary(L_cat)

V_cat <- lm(interpol ~ CO2Tmt*H2OTmt, data = df_V)
summary(V_cat)
