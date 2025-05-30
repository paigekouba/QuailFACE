# Thu 5/29/25
# Script to analyze findings from Quail Ridge CO2 and watering experiment on oak seedlings,
# to follow script "CO2_Oaks_5.29.25.R"

library(piecewiseSEM)
library(lme4)

# New version after zooming with Xiaoli! (5/27/25)

# want watering treatment as a predictor; needs to be numeric
final_df.1 <- final_df %>% 
  mutate(herbivory = as.numeric(Code %in% c(firstfullherb$Code,"8L2", "6V2"))) %>% # 1 = herbivory, 0 = no
  mutate(H2O_num = as.numeric(H2OTmt == "W")) # creates a dummy variable where 1 = watered, 0 = dry

modlist <- psem(
  # gs is predicted by watering, CO2
  lmer(gs ~ CO2 + H2O_num + (1|Plot), na.action = na.omit, final_df.1),
  # Anet is predicted by gs, CO2, SWC
  lmer(Anet ~ gs + CO2 + meanSWC + (1|Plot), na.action = na.omit, final_df.1),
  # SWC is predicted by watering, gs
#  lmer(meanSWC ~ H2O_num + gs + (1|Plot), na.action = na.omit, final_df.1),
  # biomass is predicted by Anet, herbivory
  lmer(totmass ~ Anet + herbivory + (1|Plot), na.action = na.omit, final_df.1)
)
# Warning messages:
#   1: In checkConv(attr(opt, "derivs"), opt$par, ctrl = control$checkConv,  :
#                     Model failed to converge with max|grad| = 0.149504 (tol = 0.002, component 1)
#   2: In checkConv(attr(opt, "derivs"), opt$par, ctrl = control$checkConv,  :
#                                     Model is nearly unidentifiable: very large eigenvalue
#                                   - Rescale variables?
#   3: NAs detected in the dataset. Consider removing all rows with NAs to prevent fitting to different subsets of data 
summary(modlist) # removed meanSWC model
plot(modlist)
