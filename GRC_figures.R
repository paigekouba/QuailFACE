# Fri 5/30/25
# figures for GRS Talk

# plan right now is to use:
# hypothesis plots
# dot plots with imputed data (Q. wislizeni)

# cross plots (with 1:1 line), to show that WUE is slightly increased, but not more for dry plants
# poster only, probably

# z score plot showing *change with CO2*: distance from 0 is [change from now to 2050], and only blue dots get Anet boost of large size. (but what about the mass variables?)

# DAG ?

# something to wrap up. 


# 1: ht change over time
# find Codes for seedlings with any herbivory and drop after the date of first herbivory
inv_all_nh0 <- right_join(firstherb, inv_all, by = "Code") 
inv_all_nfh0 <- right_join(firstfullherb, inv_all, by = "Code") 

inv_all_nh0[is.na(inv_all_nh0$firstherb),2] <- ymd("9999-09-09") # if not listed in firstherb (ie NA), assign a dummy date >> any in the surveys
inv_all_nfh0[is.na(inv_all_nfh0$firstherb),2] <- ymd("9999-09-09")

inv_all_nh <- inv_all_nh0 %>% 
  filter(firstherb > value)

inv_all_nfh <- inv_all_nfh0 %>% 
  filter(firstherb > value)

# get counts per sampling date, species and tmt 
inv_nequals <- inv_all_nfh %>%
  filter(!is.na(dia)) %>% 
  group_by(Spp, Tmt, value) %>%
   tally()
  
  inv_all_nfh %>% # for seedlings without *full* herbivory, how did height change over the course of the study?
  group_by(Spp, Tmt, value) %>% 
  # summarise(mean_ht = mean(ht_mm, na.rm = TRUE),sd_ht = sd(ht_mm, na.rm = TRUE)) %>% 
  summarise(mean_ht = mean(ht_mm, na.rm = TRUE),se_ht = sd(ht_mm, na.rm = TRUE)/sqrt(n())) %>% 
  # summarise(mean_dia = mean(dia, na.rm = TRUE),sd_dia = sd(dia, na.rm = TRUE)) %>% 
  # summarise(mean_cond = mean(cond, na.rm = TRUE),sd_cond = sd(cond, na.rm = TRUE)) %>% 
  ggplot(aes(x=value, y = mean_ht, group = Tmt)) + 
  geom_line(aes(color = Tmt), position = position_dodge(20, preserve = "total"), linewidth = 1.2) +
  geom_pointrange(aes(ymin = mean_ht - se_ht, ymax = mean_ht + se_ht, color = Tmt, shape = Tmt), position = position_dodge(20, preserve = "total"), size = 1.5, linewidth=1, alpha = 0.75) + 
  # geom_pointrange(aes(ymin = mean_dia - sd_dia, ymax = mean_dia + sd_dia, color = Tmt, shape = Tmt), position = position_dodge(20, preserve = "total"), size = 1.5, alpha = 0.75) + 
  # geom_pointrange(aes(ymin = mean_cond - sd_cond, ymax = mean_cond + sd_cond, color = Tmt, shape = Tmt), position = position_dodge(20, preserve = "total"), size = 1.5, alpha = 0.75) + 
  scale_color_manual(values = c("#e8665d", "#828cfa", "#850a01", "#010c85")) +
  scale_shape_manual(values = c(16,16,17,17)) +
  geom_text(data = inv_nequals, aes(x = value, y = as.numeric(as.factor(Tmt))*12-50, color = Tmt, label = paste0("N=",n))) +
  facet_grid(~ Spp) + theme_classic(base_size = 19)

# 1.b. What about diameter?
  
  #inv_all %>% # how did dia change over the course of the study?
  inv_all[which(!(inv_all$Code %in% firstfullherb$Code)),] %>% # how did dia change over the course of the study for non-chomped seedlings??
    group_by(Spp, Tmt, value) %>% 
    summarise(mean_dia = mean(dia, na.rm = TRUE),se_dia = sd(dia, na.rm = TRUE)/sqrt(n())) %>% 
    ggplot(aes(x=value, y = mean_dia, group = Tmt)) + 
    geom_line(aes(color = Tmt), position = position_dodge(20, preserve = "total"), linewidth = 1.2) +
    geom_pointrange(aes(ymin = mean_dia - se_dia, ymax = mean_dia + se_dia, color = Tmt, shape = Tmt), position = position_dodge(20, preserve = "total"), size = 1.5, alpha = 0.75) + 
    scale_color_manual(values = c("#e8665d", "#828cfa", "#850a01", "#010c85")) +
    scale_shape_manual(values = c(16,16,17,17)) +
    facet_grid(~ Spp) + theme_classic(base_size = 19)

# 2: Anet vs gs by Tmt

# functions
mean_narm <- function(x){
    mean(x, na.rm=TRUE)   }
  
minus_se <- function(x){
    mean(x, na.rm=TRUE) - (sd(x, na.rm = TRUE)/sqrt(length(x[!is.na(x)]))) }

plus_se <- function(x){
    mean(x, na.rm=TRUE) + (sd(x, na.rm = TRUE)/sqrt(length(x[!is.na(x)]))) }  
  
df_final %>% 
    filter(Spp=="L") %>% 
    select(gs, Anet, Tmt) %>% 
    group_by(Tmt) %>% 
    summarise_if(is.numeric, list(y=mean_narm, ymin=minus_se, ymax=plus_se)) %>% 
    ggplot(aes(x=gs_y, y=Anet_y, fill=Tmt)) + 
    geom_errorbar(aes(x=gs_y, ymin = Anet_ymin, ymax = Anet_ymax, color=Tmt), width = 0.001) +
    geom_errorbarh(aes(y=Anet_y, xmin = gs_ymin, xmax = gs_ymax, color=Tmt), height = 0.2) +
    geom_point(aes(color=Tmt, shape=Tmt), size = 4) +
    scale_shape_manual(values = c(21, 21, 16, 16)) +
    scale_fill_manual(values =c ("white","white","white","white")) +
    xlab(label = "Stomatal Conductance (mol H2O/m2/s)") +
    ylab(label = "Photosynthesis (µmol CO2/m2/s)") +
    labs(title="Q. wislizeni") +
    scale_x_continuous(expand = c(0,0), limits=c(0,0.25)) + scale_y_continuous(expand = c(0,0), limits=c(0,25)) +
    scale_color_manual(values = c("red", "blue", "red", "blue")) + theme_classic(base_size = 19)
  
df_final %>% 
    filter(Spp=="V") %>% 
    select(gs, Anet, Tmt) %>% 
    group_by(Tmt) %>% 
    summarise_if(is.numeric, list(y=mean_narm, ymin=minus_se, ymax=plus_se)) %>% 
    ggplot(aes(x=gs_y, y=Anet_y, fill=Tmt)) + 
    geom_errorbar(aes(x=gs_y, ymin = Anet_ymin, ymax = Anet_ymax, color=Tmt), width = 0.001) +
    geom_errorbarh(aes(y=Anet_y, xmin = gs_ymin, xmax = gs_ymax, color=Tmt), height = 0.2) +
    geom_point(aes(color=Tmt, shape=Tmt), size = 4) +
    scale_shape_manual(values = c(21, 21, 16, 16)) +
    scale_fill_manual(values =c ("white","white","white","white")) +
    xlab(label = "Stomatal Conductance (mol H2O/m2/s)") +
    ylab(label = "Photosynthesis (µmol CO2/m2/s)") +
    labs(title="Q. lobata") +
    scale_x_continuous(expand = c(0,0), limits=c(0,0.25)) + scale_y_continuous(expand = c(0,0), limits=c(0,25)) +
    scale_color_manual(values = c("red", "blue", "red", "blue")) + theme_classic(base_size = 19)
  
library(ggeffects)

ggpredict(lm(Anet~rescale(CO2)*rescale(meanSWC)+time_scaled, data=filter(df_final, Spp=="L")),
             terms=c("CO2","meanSWC [4,42]"))%>% plot(rawdata=T,ci=T,colors=c("red","blue")) + labs(title="Anet (µmol CO2/m2/s), Live Oak")
summary(lm(Anet~rescale(CO2)*rescale(meanSWC)+time_scaled, data=filter(df_final, Spp=="L")))
# missing plot as a random effect!!

ggpredict(lmer(Anet~rescale(CO2)*rescale(meanSWC)+(1|Plot), data=filter(df_final, Spp=="L")),
          terms=c("CO2","meanSWC [4,42]"))%>% plot(rawdata=T,ci=T,colors=c("red","blue")) + labs(title="Anet (µmol CO2/m2/s), Live Oak")
summary(lmer(Anet~rescale(CO2)*rescale(meanSWC)+(1|Plot), data=filter(df_final, Spp=="L")))
# is Singular!

# resprouting fig
ggpredict(glmer(as.factor(resprout)~(CO2Tmt)*(H2OTmt) + (1|Plot), family="binomial", data=as.data.frame(herb_list[herb_list$Spp =="V",])), 
          terms=c("CO2Tmt","H2OTmt"))%>% plot(rawdata=T,ci=T,colors=c("red","blue"), jitter=0.1) + labs(title="resprouting (Q. lobata)") # categorical
summary(glmer(as.factor(resprout)~(CO2Tmt)*(H2OTmt) + (1|Plot), family="binomial", data=as.data.frame(herb_list[herb_list$Spp =="V",])))

ggpredict(glmer(as.factor(resprout)~rescale(CO2)*rescale(meanSWC) + (1|Plot), family="binomial", data=as.data.frame(herb_list[herb_list$Spp =="V",])), 
          terms=c("CO2","meanSWC [4,42]"))%>% plot(rawdata=T,ci=T,colors=c("red","blue"), jitter=0.1) + labs(title="resprouting (Q. lobata)")
summary(glmer(as.factor(resprout)~rescale(CO2)*rescale(meanSWC) + (1|Plot), family="binomial", data=as.data.frame(herb_list[herb_list$Spp =="V",])))

# 3: ∆ with watering

# Fig 2 from Raubenheimer and Ripley
# need a df where x = variable name, y, ymin, ymax are mean, mean-se, and mean+se of %∆ with CO2; group by H2OTmt
# want to see %∆ for wet and dry plants with eCO2

# start with every obs of every variable (long format) and columns for Tmt. Then summarise y, ymin, and ymax with mutate and reference to AD or AW column value

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
colnames(fig2_nequals) <- c("aCO2","eCO2", "variable")

fig2_nequalsV <- df_final %>% 
  filter(! Code %in% firstfullherb$Code) %>% 
  filter(Spp=="V") %>% 
  select(Tmt, H2OTmt, totmass, max_ht, max_dia, Anet, gs, WUE) %>% 
  rename(tot.mass = totmass) %>% 
  rename(max.ht = max_ht) %>% 
  rename(max.dia = max_dia) %>% 
  group_by(Tmt, H2OTmt) %>% 
  summarise_if(is.numeric, ~ sum(!is.na(.x))) %>% 
  group_by(H2OTmt) %>% 
  summarise_if(is.numeric, ~ min(.x)) %>% 
  t() %>% data.frame()

fig2_nequalsV <- fig2_nequalsV[-1,]
fig2_nequalsV[,3] <- rownames(fig2_nequalsV)
colnames(fig2_nequalsV) <- c("aCO2","eCO2", "variable")

## z-score change with watering
fig2_boot <- df_final %>% 
  filter(Spp=="L") %>% 
  select(Tmt, totmass, max_ht, max_dia, Anet, gs, WUE) %>%
  rename(tot.mass = totmass) %>% 
  rename(max.ht = max_ht) %>% 
  rename(max.dia = max_dia)

# instead of a percent difference caused by watering, I now want to standardize all the responses as z scores
# then find the ∆z with watering

zscore <- function(x){
  (x - mean(x, na.rm = T))/sd(x, na.rm = T)
}

fig2_boot <- fig2_boot %>% mutate_if(is.numeric, list(z = zscore)) 

# start with the mean of ∆z with watering
zmean <- data.frame("AW1" = c(1:6), "EW1" = c(1:6)) # initialize results df
for(i in c(2:7)){zmean[(i-1),1] <-          # this is saying each of the 6 variables gets 1 row, and this is defining the first column in the results df
  # the top part is to get the mean ∆z, with watering, for aCO2 
  mean(
    do.call(c,lapply(1:1000, function(boot){
      a <- sample(unlist(fig2_boot[fig2_boot$Tmt=="AW",i+6])[!is.na(unlist(fig2_boot[fig2_boot$Tmt=="AW",i+6]))], replace = T)
      b <- sample(unlist(fig2_boot[fig2_boot$Tmt=="AD",i+6])[!is.na(unlist(fig2_boot[fig2_boot$Tmt=="AD",i+6]))], replace = T)
      mean(a)-mean(b)
    })))
# the second part is to get the mean ∆z, with watering, for eCO2 
zmean[(i-1),2] <- mean(
  do.call(c,lapply(1:1000, function(boot){
    a <- sample(unlist(fig2_boot[fig2_boot$Tmt=="EW",i+6])[!is.na(unlist(fig2_boot[fig2_boot$Tmt=="EW",i+6]))], replace = T)
    b <- sample(unlist(fig2_boot[fig2_boot$Tmt=="ED",i+6])[!is.na(unlist(fig2_boot[fig2_boot$Tmt=="ED",i+6]))], replace = T)
    mean(a)-mean(b)
  })))
}

# then mean - sd
zmin <- data.frame("AW1" = c(1:6), "EW1" = c(1:6)) # initialize results df
for(i in c(2:7)){ x <- do.call(c,lapply(1:1000, function(boot){
  a <- sample(unlist(fig2_boot[fig2_boot$Tmt=="AW",i+6])[!is.na(unlist(fig2_boot[fig2_boot$Tmt=="AW",i+6]))], replace = T)
  b <- sample(unlist(fig2_boot[fig2_boot$Tmt=="AD",i+6])[!is.na(unlist(fig2_boot[fig2_boot$Tmt=="AD",i+6]))], replace = T)
  mean(a)-mean(b)
}))
zmin[(i-1),1] <- mean(x) - sd(x)

y <- do.call(c,lapply(1:1000, function(boot){
  a <- sample(unlist(fig2_boot[fig2_boot$Tmt=="EW",i+6])[!is.na(unlist(fig2_boot[fig2_boot$Tmt=="EW",i+6]))], replace = T)
  b <- sample(unlist(fig2_boot[fig2_boot$Tmt=="ED",i+6])[!is.na(unlist(fig2_boot[fig2_boot$Tmt=="ED",i+6]))], replace = T)
  mean(a)-mean(b)
}))
zmin[(i-1),2] <- mean(y) - sd(y)
}

# then mean + sd
zmax <- data.frame("AW1" = c(1:6), "EW1" = c(1:6)) # initialize results df
for(i in c(2:7)){ x <- do.call(c,lapply(1:1000, function(boot){
  a <- sample(unlist(fig2_boot[fig2_boot$Tmt=="AW",i+6])[!is.na(unlist(fig2_boot[fig2_boot$Tmt=="AW",i+6]))], replace = T)
  b <- sample(unlist(fig2_boot[fig2_boot$Tmt=="AD",i+6])[!is.na(unlist(fig2_boot[fig2_boot$Tmt=="AD",i+6]))], replace = T)
  mean(a)-mean(b)
}))
zmax[(i-1),1] <- mean(x) + sd(x)

y <- do.call(c,lapply(1:1000, function(boot){
  a <- sample(unlist(fig2_boot[fig2_boot$Tmt=="EW",i+6])[!is.na(unlist(fig2_boot[fig2_boot$Tmt=="EW",i+6]))], replace = T)
  b <- sample(unlist(fig2_boot[fig2_boot$Tmt=="ED",i+6])[!is.na(unlist(fig2_boot[fig2_boot$Tmt=="ED",i+6]))], replace = T)
  mean(a)-mean(b)
}))
zmax[(i-1),2] <- mean(y) + sd(y)
}

z_bootL <- cbind(rbind(zmean, zmin, zmax), 
                 data.frame("variable" = rep(c("tot.mass", "max.ht", "max.dia", "Anet", "gs", "WUE"),3), 
                            "y" = rep(c("y","ymin","ymax"), each=6))) %>% 
  rename(aCO2=AW1, eCO2=EW1) %>% 
  pivot_longer(cols=c("aCO2","eCO2"), names_to="Treatment", values_to="value") %>% 
  select(variable, y, Treatment, value) %>% 
  pivot_wider(names_from = "y", values_from = "value") %>% 
  ggplot() +
  geom_abline(color= "red", linetype="dashed", slope = 0, intercept= 0) +
  geom_pointrange(aes(x=factor(variable, level=variable_order), y=y, ymin=ymin, ymax=ymax, group=Treatment, color=Treatment, shape=Treatment), size=1, linewidth=1, position=position_dodge(width=0.2)) + scale_color_manual(values=c("darkgray","black")) + scale_shape_manual(values = c(1,16)) +
  ylim(-2.2, 2.6) +
  geom_text(data = fig2_nequals, aes(x = variable, y = -1.75, label = paste0("n = ",aCO2)), color="darkgray", size = 5) +
  geom_text(data = fig2_nequals, aes(x = variable, y = -2.1, label = paste0("n = ",eCO2)), color="black", size = 5) +
  ggtitle("B. Quercus wislizeni (live oak)") +
  ylab("change with watering") + xlab("Plant Response") +
  theme_classic(base_size = 20) 

# now for V!
fig2_bootV <- df_final %>% 
  filter(Spp=="V") %>% 
  select(Tmt, totmass, max_ht, max_dia, Anet, gs, WUE) %>%
  rename(tot.mass = totmass) %>% 
  rename(max.ht = max_ht) %>% 
  rename(max.dia = max_dia)

fig2_bootV <- fig2_bootV %>% mutate_if(is.numeric, list(z = zscore)) 

# start with the mean of ∆z with watering
zmeanV <- data.frame("AW1" = c(1:6), "EW1" = c(1:6)) # initialize results df
for(i in c(2:7)){zmeanV[(i-1),1] <-          # this is saying each of the 6 variables gets 1 row, and this is defining the first column in the results df
  # the top part is to get the mean ∆z, with watering, for aCO2 
  mean(
    do.call(c,lapply(1:1000, function(boot){
      a <- sample(unlist(fig2_bootV[fig2_bootV$Tmt=="AW",i+6])[!is.na(unlist(fig2_bootV[fig2_bootV$Tmt=="AW",i+6]))], replace = T)
      b <- sample(unlist(fig2_bootV[fig2_bootV$Tmt=="AD",i+6])[!is.na(unlist(fig2_bootV[fig2_bootV$Tmt=="AD",i+6]))], replace = T)
      mean(a)-mean(b)
    })))
# the second part is to get the mean ∆z, with watering, for eCO2 
zmeanV[(i-1),2] <- mean(
  do.call(c,lapply(1:1000, function(boot){
    a <- sample(unlist(fig2_bootV[fig2_bootV$Tmt=="EW",i+6])[!is.na(unlist(fig2_bootV[fig2_bootV$Tmt=="EW",i+6]))], replace = T)
    b <- sample(unlist(fig2_bootV[fig2_bootV$Tmt=="ED",i+6])[!is.na(unlist(fig2_bootV[fig2_bootV$Tmt=="ED",i+6]))], replace = T)
    mean(a)-mean(b)
  })))
}

# then mean - sd
zminV <- data.frame("AW1" = c(1:6), "EW1" = c(1:6)) # initialize results df
for(i in c(2:7)){ x <- do.call(c,lapply(1:1000, function(boot){
  a <- sample(unlist(fig2_bootV[fig2_bootV$Tmt=="AW",i+6])[!is.na(unlist(fig2_bootV[fig2_bootV$Tmt=="AW",i+6]))], replace = T)
  b <- sample(unlist(fig2_bootV[fig2_bootV$Tmt=="AD",i+6])[!is.na(unlist(fig2_bootV[fig2_bootV$Tmt=="AD",i+6]))], replace = T)
  mean(a)-mean(b)
}))
zminV[(i-1),1] <- mean(x) - sd(x)

y <- do.call(c,lapply(1:1000, function(boot){
  a <- sample(unlist(fig2_bootV[fig2_bootV$Tmt=="EW",i+6])[!is.na(unlist(fig2_bootV[fig2_bootV$Tmt=="EW",i+6]))], replace = T)
  b <- sample(unlist(fig2_bootV[fig2_bootV$Tmt=="ED",i+6])[!is.na(unlist(fig2_bootV[fig2_bootV$Tmt=="ED",i+6]))], replace = T)
  mean(a)-mean(b)
}))
zminV[(i-1),2] <- mean(y) - sd(y)
}

# then mean + sd
zmaxV <- data.frame("AW1" = c(1:6), "EW1" = c(1:6)) # initialize results df
for(i in c(2:7)){ x <- do.call(c,lapply(1:1000, function(boot){
  a <- sample(unlist(fig2_bootV[fig2_bootV$Tmt=="AW",i+6])[!is.na(unlist(fig2_bootV[fig2_bootV$Tmt=="AW",i+6]))], replace = T)
  b <- sample(unlist(fig2_bootV[fig2_bootV$Tmt=="AD",i+6])[!is.na(unlist(fig2_bootV[fig2_bootV$Tmt=="AD",i+6]))], replace = T)
  mean(a)-mean(b)
}))
zmaxV[(i-1),1] <- mean(x) + sd(x)

y <- do.call(c,lapply(1:1000, function(boot){
  a <- sample(unlist(fig2_bootV[fig2_bootV$Tmt=="EW",i+6])[!is.na(unlist(fig2_bootV[fig2_bootV$Tmt=="EW",i+6]))], replace = T)
  b <- sample(unlist(fig2_bootV[fig2_bootV$Tmt=="ED",i+6])[!is.na(unlist(fig2_bootV[fig2_bootV$Tmt=="ED",i+6]))], replace = T)
  mean(a)-mean(b)
}))
zmaxV[(i-1),2] <- mean(y) + sd(y)
}

z_bootV <- cbind(rbind(zmeanV, zminV, zmaxV), 
                 data.frame("variable" = rep(c("tot.mass", "max.ht", "max.dia", "Anet", "gs", "WUE"),3), 
                            "y" = rep(c("y","ymin","ymax"), each=6))) %>% 
  rename(aCO2=AW1, eCO2=EW1) %>% 
  pivot_longer(cols=c("aCO2","eCO2"), names_to="Treatment", values_to="value") %>% 
  select(variable, y, Treatment, value) %>% 
  pivot_wider(names_from = "y", values_from = "value") %>% 
  ggplot() +
  geom_abline(color= "red", linetype="dashed", slope = 0, intercept= 0) +
  geom_pointrange(aes(x=factor(variable, level=variable_order), y=y, ymin=ymin, ymax=ymax, group=Treatment, color=Treatment, shape=Treatment), size=1, linewidth=1, position=position_dodge(width=0.2)) + scale_color_manual(values=c("darkgray","black")) + scale_shape_manual(values = c(1,16)) +
  ylim(-2.2, 2.6) +
  geom_text(data = fig2_nequalsV, aes(x = variable, y = -1.75, label = paste0("n = ",aCO2)), color="darkgray", size = 5) +
  geom_text(data = fig2_nequalsV, aes(x = variable, y = -2.1, label = paste0("n = ",eCO2)), color="black", size = 5) +
  ggtitle("B. Quercus lobata (valley oak)") +
  ylab("change with watering") + xlab("Plant Response") +
  theme_classic(base_size = 20) 

# 4: SEMs for totmass, mortality, resprouting
names(df_final) # what to work with
# [1] "Plot"        "Tmt"         "Code"        "Spp"         "StemWet_g"   "LeafWet_g"   "LeafDry_g"   "longcode"   
# [9] "max_ht"      "max_dia"     "rootmass_g"  "mortality"   "shortcode"   "resprout"    "H2OTmt"      "totmass"    
# [17] "mDeltaTest"  "CO2"         "meanSWC"     "Anet"        "gs"          "WUE"         "HHMMSS"      "time_scaled"

df_final_z <- df_final %>% 
   mutate_if(is.numeric, list(z = zscore)) %>% 
   mutate(Spp_num = as.numeric(Spp=="V")) %>% 
   mutate(herbivory = as.numeric(Code %in% firstfullherb$Code))

# totmass: use max_ht instead? ~ CO2, meanSWC, Anet, Spp
max_ht.psem <- psem(
  # max_ht is predicted by CO2, meanSWC, Anet, Spp, herbivory
  lm(max_ht_z ~ CO2_z + meanSWC_z + Anet_z + Spp_num + herbivory, na.action = na.omit, df_final_z),
  lm(Anet_z ~ CO2_z + meanSWC_z + Spp_num, na.action = na.omit, df_final_z),
  glm(herbivory ~ Spp_num + meanSWC_z, family = "binomial", na.action = na.omit, df_final_z)
)
summary(max_ht.psem)
plot(max_ht.psem)

totmass.psem <- psem(
  # max_ht is predicted by CO2, meanSWC, Anet, Spp, herbivory
  lm(totmass_z ~ CO2_z + meanSWC_z + Anet_z + Spp_num + herbivory, na.action = na.omit, df_final_z),
  lm(Anet_z ~ CO2_z*meanSWC_z + Spp_num, na.action = na.omit, df_final_z),
  glm(herbivory ~ Spp_num + meanSWC_z, family = "binomial", na.action = na.omit, df_final_z)
)
summary(totmass.psem)
plot(totmass.psem)

# mortality: use Spp, herbivory, CO2, meanSWC
mort.psem <- psem(
  # max_ht is predicted by Spp, herbivory, CO2, meanSWC
  glm(mortality ~ CO2_z+meanSWC_z + Spp_num + herbivory, family = "binomial", na.action = na.omit, df_final_z),
  glm(herbivory ~ Spp_num + meanSWC_z, family = "binomial", na.action = na.omit, df_final_z)
)
summary(mort.psem)
plot(mort.psem)


# resprouting: for [herbivory subset], use CO2, meanSWC, rootmass_g
resprout.psem <- psem(
  glm(resprout ~ CO2_z  + rootmass_g, family = "binomial", 
      na.action = na.omit, filter(df_final_z, herbivory==1)),
  lm(rootmass_g ~ CO2_z , na.action = na.omit, filter(df_final_z, herbivory==1))
)
summary(resprout.psem)
plot(resprout.psem)
summary(glm(resprout ~ CO2, family = "binomial", filter(df_final_z, herbivory==1)))
