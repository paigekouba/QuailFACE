# what about change with CO2? that is after all what I'm saying with the title


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
  ylim(-2.2, 2.6) +
  geom_text(data = fig2_nequals, aes(x = variable, y = -1.75, label = paste0("n = ",aCO2)), color="red", size = 5) +
  geom_text(data = fig2_nequals, aes(x = variable, y = -2.1, label = paste0("n = ",eCO2)), color="blue", size = 5) +
  ggtitle("B. Quercus wislizeni (live oak)") +
  ylab("change with CO2") + xlab("Plant Response") +
  theme_classic(base_size = 20) 

ggplot(df_final_z, aes(x = Tmt, y=Anet)) + geom_boxplot() + facet_wrap(~Spp)

ggplot(df_final_z, aes(x = Tmt, y=gs)) + geom_boxplot() + facet_wrap(~Spp)

# now for V

# what about change with CO2? that is after all what I'm saying with the title


CO2_bootV <- df_final %>% 
  filter(Spp=="V") %>% 
  select(Tmt, totmass, max_ht, max_dia, Anet, gs, WUE) %>%
  rename(tot.mass = totmass) %>% 
  rename(max.ht = max_ht) %>% 
  rename(max.dia = max_dia) %>% mutate_if(is.numeric, list(z = zscore)) 

# start with the mean of ∆z with CO2
zmeanV <- data.frame("ED1" = c(1:6), "EW1" = c(1:6)) # initialize results df
for(i in c(2:7)){zmeanV[(i-1),1] <-          # this is saying each of the 6 variables gets 1 row, and this is defining the first column in the results df
  # the top part is to get the mean ∆z, with CO2, for dry plants
  mean(
    do.call(c,lapply(1:1000, function(boot){
      a <- sample(unlist(CO2_bootV[CO2_bootV$Tmt=="ED",i+6])[!is.na(unlist(CO2_bootV[CO2_bootV$Tmt=="ED",i+6]))], replace = T)
      b <- sample(unlist(CO2_bootV[CO2_bootV$Tmt=="AD",i+6])[!is.na(unlist(CO2_bootV[CO2_bootV$Tmt=="AD",i+6]))], replace = T)
      mean(a)-mean(b)
    })))
# the second part is to get the mean ∆z, with CO2, for watered plants 
zmeanV[(i-1),2] <- mean(
  do.call(c,lapply(1:1000, function(boot){
    a <- sample(unlist(CO2_bootV[CO2_bootV$Tmt=="EW",i+6])[!is.na(unlist(CO2_bootV[CO2_bootV$Tmt=="EW",i+6]))], replace = T)
    b <- sample(unlist(CO2_bootV[CO2_bootV$Tmt=="AW",i+6])[!is.na(unlist(CO2_bootV[CO2_bootV$Tmt=="AW",i+6]))], replace = T)
    mean(a)-mean(b)
  })))
}

# then mean - sd
zminV <- data.frame("ED1" = c(1:6), "EW1" = c(1:6)) # initialize results df
for(i in c(2:7)){ x <- do.call(c,lapply(1:1000, function(boot){
  a <- sample(unlist(CO2_bootV[CO2_bootV$Tmt=="ED",i+6])[!is.na(unlist(CO2_bootV[CO2_bootV$Tmt=="ED",i+6]))], replace = T)
  b <- sample(unlist(CO2_bootV[CO2_bootV$Tmt=="AD",i+6])[!is.na(unlist(CO2_bootV[CO2_bootV$Tmt=="AD",i+6]))], replace = T)
  mean(a)-mean(b)
}))
zminV[(i-1),1] <- mean(x) - sd(x)

y <- do.call(c,lapply(1:1000, function(boot){
  a <- sample(unlist(CO2_bootV[CO2_bootV$Tmt=="EW",i+6])[!is.na(unlist(CO2_bootV[CO2_bootV$Tmt=="EW",i+6]))], replace = T)
  b <- sample(unlist(CO2_bootV[CO2_bootV$Tmt=="AW",i+6])[!is.na(unlist(CO2_bootV[CO2_bootV$Tmt=="AW",i+6]))], replace = T)
  mean(a)-mean(b)
}))
zminV[(i-1),2] <- mean(y) - sd(y)
}

# then mean + sd
zmaxV <- data.frame("ED1" = c(1:6), "EW1" = c(1:6)) # initialize results df
for(i in c(2:7)){ x <- do.call(c,lapply(1:1000, function(boot){
  a <- sample(unlist(CO2_bootV[CO2_bootV$Tmt=="ED",i+6])[!is.na(unlist(CO2_bootV[CO2_bootV$Tmt=="ED",i+6]))], replace = T)
  b <- sample(unlist(CO2_bootV[CO2_bootV$Tmt=="AD",i+6])[!is.na(unlist(CO2_bootV[CO2_bootV$Tmt=="AD",i+6]))], replace = T)
  mean(a)-mean(b)
}))
zmaxV[(i-1),1] <- mean(x) + sd(x)

y <- do.call(c,lapply(1:1000, function(boot){
  a <- sample(unlist(CO2_bootV[CO2_bootV$Tmt=="EW",i+6])[!is.na(unlist(CO2_bootV[CO2_bootV$Tmt=="EW",i+6]))], replace = T)
  b <- sample(unlist(CO2_bootV[CO2_bootV$Tmt=="AW",i+6])[!is.na(unlist(CO2_bootV[CO2_bootV$Tmt=="AW",i+6]))], replace = T)
  mean(a)-mean(b)
}))
zmaxV[(i-1),2] <- mean(y) + sd(y)
}

z_bootL_CO2 <- cbind(rbind(zmeanV, zminV, zmaxV), 
                     data.frame("variable" = rep(c("tot.mass", "max.ht", "max.dia", "Anet", "gs", "WUE"),3), 
                                "y" = rep(c("y","ymin","ymax"), each=6))) %>% 
  rename(dry=ED1, wet=EW1) %>% 
  pivot_longer(cols=c("dry","wet"), names_to="Treatment", values_to="value") %>% 
  select(variable, y, Treatment, value) %>% 
  pivot_wider(names_from = "y", values_from = "value") %>% 
  ggplot() +
  geom_abline(color= "red", linetype="dashed", slope = 0, intercept= 0) +
  geom_pointrange(aes(x=factor(variable, level=variable_order), y=y, ymin=ymin, ymax=ymax, group=Treatment, color=Treatment, shape=Treatment), size=1, linewidth=1, position=position_dodge(width=0.2)) + scale_color_manual(values=c("darkgray","black")) + scale_shape_manual(values = c(1,16)) +
  ylim(-2.2, 2.6) +
  geom_text(data = fig2_nequals, aes(x = variable, y = -1.75, label = paste0("n = ",aCO2)), color="darkgray", size = 5) +
  geom_text(data = fig2_nequals, aes(x = variable, y = -2.1, label = paste0("n = ",eCO2)), color="black", size = 5) +
  ggtitle("B. Quercus lobata (valley oak)") +
  ylab("change with CO2") + xlab("Plant Response") +
  theme_classic(base_size = 20) 

# now for both

CO2_boot_all <- df_final %>% 
  select(Tmt, totmass, max_ht, max_dia, Anet, gs, WUE) %>%
  rename(tot.mass = totmass) %>% 
  rename(max.ht = max_ht) %>% 
  rename(max.dia = max_dia) %>% mutate_if(is.numeric, list(z = zscore)) 

# start with the mean of ∆z with CO2
zmean_all <- data.frame("ED1" = c(1:6), "EW1" = c(1:6)) # initialize results df
for(i in c(2:7)){zmean_all[(i-1),1] <-          # this is saying each of the 6 variables gets 1 row, and this is defining the first column in the results df
  # the top part is to get the mean ∆z, with CO2, for dry plants
  mean(
    do.call(c,lapply(1:1000, function(boot){
      a <- sample(unlist(CO2_boot_all[CO2_boot_all$Tmt=="ED",i+6])[!is.na(unlist(CO2_boot_all[CO2_boot_all$Tmt=="ED",i+6]))], replace = T)
      b <- sample(unlist(CO2_boot_all[CO2_boot_all$Tmt=="AD",i+6])[!is.na(unlist(CO2_boot_all[CO2_boot_all$Tmt=="AD",i+6]))], replace = T)
      mean(a)-mean(b)
    })))
# the second part is to get the mean ∆z, with CO2, for watered plants 
zmean_all[(i-1),2] <- mean(
  do.call(c,lapply(1:1000, function(boot){
    a <- sample(unlist(CO2_boot_all[CO2_boot_all$Tmt=="EW",i+6])[!is.na(unlist(CO2_boot_all[CO2_boot_all$Tmt=="EW",i+6]))], replace = T)
    b <- sample(unlist(CO2_boot_all[CO2_boot_all$Tmt=="AW",i+6])[!is.na(unlist(CO2_boot_all[CO2_boot_all$Tmt=="AW",i+6]))], replace = T)
    mean(a)-mean(b)
  })))
}

# then mean - sd
zmin_all <- data.frame("ED1" = c(1:6), "EW1" = c(1:6)) # initialize results df
for(i in c(2:7)){ x <- do.call(c,lapply(1:1000, function(boot){
  a <- sample(unlist(CO2_boot_all[CO2_boot_all$Tmt=="ED",i+6])[!is.na(unlist(CO2_boot_all[CO2_boot_all$Tmt=="ED",i+6]))], replace = T)
  b <- sample(unlist(CO2_boot_all[CO2_boot_all$Tmt=="AD",i+6])[!is.na(unlist(CO2_boot_all[CO2_boot_all$Tmt=="AD",i+6]))], replace = T)
  mean(a)-mean(b)
}))
zmin_all[(i-1),1] <- mean(x) - sd(x)

y <- do.call(c,lapply(1:1000, function(boot){
  a <- sample(unlist(CO2_boot_all[CO2_boot_all$Tmt=="EW",i+6])[!is.na(unlist(CO2_boot_all[CO2_boot_all$Tmt=="EW",i+6]))], replace = T)
  b <- sample(unlist(CO2_boot_all[CO2_boot_all$Tmt=="AW",i+6])[!is.na(unlist(CO2_boot_all[CO2_boot_all$Tmt=="AW",i+6]))], replace = T)
  mean(a)-mean(b)
}))
zmin_all[(i-1),2] <- mean(y) - sd(y)
}

# then mean + sd
zmax_all <- data.frame("ED1" = c(1:6), "EW1" = c(1:6)) # initialize results df
for(i in c(2:7)){ x <- do.call(c,lapply(1:1000, function(boot){
  a <- sample(unlist(CO2_boot_all[CO2_boot_all$Tmt=="ED",i+6])[!is.na(unlist(CO2_boot_all[CO2_boot_all$Tmt=="ED",i+6]))], replace = T)
  b <- sample(unlist(CO2_boot_all[CO2_boot_all$Tmt=="AD",i+6])[!is.na(unlist(CO2_boot_all[CO2_boot_all$Tmt=="AD",i+6]))], replace = T)
  mean(a)-mean(b)
}))
zmax_all[(i-1),1] <- mean(x) + sd(x)

y <- do.call(c,lapply(1:1000, function(boot){
  a <- sample(unlist(CO2_boot_all[CO2_boot_all$Tmt=="EW",i+6])[!is.na(unlist(CO2_boot_all[CO2_boot_all$Tmt=="EW",i+6]))], replace = T)
  b <- sample(unlist(CO2_boot_all[CO2_boot_all$Tmt=="AW",i+6])[!is.na(unlist(CO2_boot_all[CO2_boot_all$Tmt=="AW",i+6]))], replace = T)
  mean(a)-mean(b)
}))
zmax_all[(i-1),2] <- mean(y) + sd(y)
}

z_boot_all_CO2 <- cbind(rbind(zmean_all, zmin_all, zmax_all), 
                     data.frame("variable" = rep(c("tot.mass", "max.ht", "max.dia", "Anet", "gs", "WUE"),3), 
                                "y" = rep(c("y","ymin","ymax"), each=6))) %>% 
  rename(dry=ED1, wet=EW1) %>% 
  pivot_longer(cols=c("dry","wet"), names_to="Treatment", values_to="value") %>% 
  select(variable, y, Treatment, value) %>% 
  pivot_wider(names_from = "y", values_from = "value") %>% 
  ggplot() +
  geom_abline(color= "red", linetype="dashed", slope = 0, intercept= 0) +
  geom_pointrange(aes(x=factor(variable, level=variable_order), y=y, ymin=ymin, ymax=ymax, group=Treatment, color=Treatment, shape=Treatment), size=1, linewidth=1, position=position_dodge(width=0.2)) + scale_color_manual(values=c("darkgray","black")) + scale_shape_manual(values = c(1,16)) +
  ylim(-2.2, 2.6) +
  geom_text(data = fig2_nequals, aes(x = variable, y = -1.75, label = paste0("n = ",aCO2)), color="darkgray", size = 5) +
  geom_text(data = fig2_nequals, aes(x = variable, y = -2.1, label = paste0("n = ",eCO2)), color="black", size = 5) +
  ggtitle("Both species") +
  ylab("change with CO2") + xlab("Plant Response") +
  theme_classic(base_size = 20) 



dia.psem <- psem(
  # max_ht is predicted by CO2, meanSWC, Anet, Spp, herbivory
  lm(max_dia_z ~ Anet_z, na.action = na.omit, filter(df_final_z, Spp == "L")),
  lm(Anet_z ~ CO2_z*meanSWC_z, na.action = na.omit, filter(df_final_z, Spp == "L"))
)
summary(dia.psem)
plot(dia.psem)

rootmass.psem <- psem(
  # max_ht is predicted by CO2, meanSWC, Anet, Spp, herbivory
  lm(rootmass_g_z ~ Anet_z, na.action = na.omit, filter(df_final_z, Spp == "L")),
  lm(Anet_z ~ CO2_z*meanSWC_z, na.action = na.omit, filter(df_final_z, Spp == "L"))
)
summary(rootmass.psem)
plot(rootmass.psem)

df_final %>% 
#  filter(Spp == "L") %>% 
  mutate(rootshoot = rootmass_g/(StemWet_g+LeafDry_g)) %>% 
ggplot(aes(x=CO2, y=max_ht, color = H2OTmt)) + geom_point() + facet_wrap(~Spp)


# here is what I will use: for just Q. wislizeni, the effects of CO2, H2O, and combination on gs, Anet, biomass

xd.psem <- psem(
  lm(gs_z ~ CO2_z * meanSWC_z, na.action = na.omit, filter(df_final_z, Spp == "L")),
  lm(Anet_z ~ CO2_z * meanSWC_z + gs_z, na.action = na.omit, filter(df_final_z, Spp == "L")),
  lm(totmass_z ~ Anet_z + meanSWC_z, na.action = na.omit, filter(df_final_z, Spp == "L"))
)
plot(xd.psem)
summary(xd.psem)

xd.psem.re <- psem( # with random effect for plot
  lmer(gs_z ~ CO2_z * meanSWC_z + (1|Plot), na.action = na.omit, filter(df_final_z, Spp == "L")),
  lmer(Anet_z ~ CO2_z * meanSWC_z + gs_z + (1|Plot), na.action = na.omit, filter(df_final_z, Spp == "L")),
  lmer(totmass_z ~ Anet_z + meanSWC_z + (1|Plot), na.action = na.omit, filter(df_final_z, Spp == "L"))
)
plot(xd.psem.re)
summary(xd.psem.re)


df_final_z_block <- df_final_z %>% 
  mutate(Block = case_when(
    Plot %in% c("1","2","15","16") ~ "1",
    Plot %in% c("3", "4","5","6") ~ "2",
    Plot %in% c("7","8","9","10") ~ "3",
    Plot %in% c("11","12","13","14") ~ "4"
  )) 

xd.psem.re <- psem( # with random effect for Block
  lmer(gs_z ~ CO2_z * meanSWC_z + (1|Block), na.action = na.omit, filter(df_final_z_block, Spp == "L")),
  lmer(Anet_z ~ CO2_z * meanSWC_z + gs_z + (1|Block), na.action = na.omit, filter(df_final_z_block, Spp == "L")),
  lmer(totmass_z ~ Anet_z + meanSWC_z + (1|Block), na.action = na.omit, filter(df_final_z_block, Spp == "L"))
)
plot(xd.psem.re)
summary(xd.psem.re)

# what if combine spp??
xd.psem.re <- psem( # with random effect for Plot
  lmer(gs_z ~ CO2_z+meanSWC_z + Spp_num + (1|Plot), df_final_z_block),
  lmer(Anet_z ~ CO2_z * meanSWC_z + gs_z + Spp_num + (1|Plot), df_final_z_block),
  lmer(totmass_z ~ Anet_z + meanSWC_z + Spp_num + (1|Plot), df_final_z_block)
)
plot(xd.psem.re)
summary(xd.psem.re) # Check model convergence: log-likelihood estimates lead to negative Chi-squared! 
car::vif(lmer(gs_z ~ CO2_z+meanSWC_z + Spp_num + (1|Plot), df_final_z_block))
car::vif(lmer(Anet_z ~ CO2_z * meanSWC_z + gs_z + Spp_num + (1|Plot), df_final_z_block))
car::vif(lmer(totmass_z ~ Anet_z + meanSWC_z + Spp_num + (1|Plot), df_final_z_block))

check_convergence(lmer(gs_z ~ CO2_z+meanSWC_z + Spp_num + (1|Plot), df_final_z_block))
check_convergence(lmer(Anet_z ~ CO2_z * meanSWC_z + gs_z + Spp_num + (1|Plot), df_final_z_block))
check_convergence(lmer(totmass_z ~ Anet_z + meanSWC_z + Spp_num + (1|Plot), df_final_z_block))




xd.psem.fe <- psem( # with random effect for Plot
  lm(gs_z ~ CO2_z+meanSWC_z + Spp_num, df_final_z_block),
  lm(Anet_z ~ CO2_z * meanSWC_z + gs_z + Spp_num, df_final_z_block),
  lm(totmass_z ~ Anet_z + meanSWC_z + Spp_num, df_final_z_block)
)
plot(xd.psem.fe)
summary(xd.psem.fe)

AIC(xd.psem.re, xd.psem.fe)
nobs(xd.psem.fe)

xd.psem.re2 <- psem( # with random effect for Block
  lmer(gs_z ~ CO2_z+meanSWC_z + Spp_num + (1|Block), df_final_z_block),
  lmer(Anet_z ~ CO2_z * meanSWC_z + gs_z + Spp_num + (1|Block), df_final_z_block),
  lmer(totmass_z ~ Anet_z + meanSWC_z + Spp_num + (1|Block), df_final_z_block)
)
plot(xd.psem.re2)
summary(xd.psem.re2)


# what is there's a random effect for species
xd.psem.re3 <- psem( # with random effect for spp
  lmer(gs_z ~ CO2_z+meanSWC_z + (1|Spp_num) + (1|Plot), df_final_z_block),
  lmer(Anet_z ~ CO2_z * meanSWC_z + gs_z + (1|Spp_num) + (1|Plot), df_final_z_block),
  lmer(totmass_z ~ Anet_z + meanSWC_z + (1|Spp_num) + (1|Plot), df_final_z_block)
)
plot(xd.psem.re3)
summary(xd.psem.re3)

# ... but not plot ?
xd.psem.re4 <- psem( # with random effect for spp
  lmer(gs_z ~ CO2_z*meanSWC_z + (1|Spp), df_final_z_block),
  lmer(Anet_z ~ CO2_z*meanSWC_z + gs_z + (1|Spp), df_final_z_block),
  lmer(totmass_z ~ Anet_z + meanSWC_z + (1|Spp), df_final_z_block)
)
plot(xd.psem.re4)
summary(xd.psem.re4)


# I am stuck because I am trying to tell a story with not enough data
# options are: simplify story, or get more data. can't do the latter (?)

# leave out interaction, keep species (random effect)
xd.psem.re5 <- psem( # with random effect for spp, plot
  lmer(gs_z ~ CO2_z + meanSWC_z + (1|Spp) + (1|Plot), df_final_z_block),
  lmer(Anet_z ~ CO2_z + meanSWC_z + gs_z + (1|Spp) + (1|Plot), df_final_z_block),
  lmer(totmass_z ~ Anet_z + meanSWC_z + (1|Spp) + (1|Plot), df_final_z_block)
)
plot(xd.psem.re5)
summary(xd.psem.re5)

# leave out interaction, keep species (fixed effect)
xd.psem.re6 <- psem( # with random effect for spp, plot
  lmer(gs_z ~ CO2_z + meanSWC_z + Spp_num + (1|Plot), df_final_z_block),
  lmer(Anet_z ~ CO2_z + meanSWC_z + gs_z + Spp_num + (1|Plot), df_final_z_block),
  lmer(totmass_z ~ Anet_z + meanSWC_z + Spp_num + (1|Plot), df_final_z_block)
)
plot(xd.psem.re6)
summary(xd.psem.re6)

# leave out interaction, keep species (fixed effect), use Block instead of plot
xd.psem.re7 <- psem( # with random effect for spp, block
  lmer(gs_z ~ CO2_z + meanSWC_z + Spp_num + (1|Block), df_final_z_block),
  lmer(Anet_z ~ CO2_z + meanSWC_z + gs_z + Spp_num + (1|Block), df_final_z_block),
  lmer(totmass_z ~ Anet_z + meanSWC_z + Spp_num + (1|Block), df_final_z_block)
)
plot(xd.psem.re7)
summary(xd.psem.re7)

# leave out interaction, keep species (random effect), use Block instead of plot
xd.psem.re8 <- psem( # with random effect for spp, block
  lmer(gs_z ~ CO2_z + meanSWC_z + (1|Spp) + (1|Block), df_final_z_block),
  lmer(Anet_z ~ CO2_z + meanSWC_z + gs_z + (1|Spp) + (1|Block), df_final_z_block),
  lmer(totmass_z ~ Anet_z + meanSWC_z + (1|Spp) + (1|Block), df_final_z_block)
)
plot(xd.psem.re8)
summary(xd.psem.re8)


ggpredict(lmer(Anet~CO2Tmt*H2OTmt+(1|Plot), data= df_final %>% filter(Spp=="L") %>% mutate(CO2Tmt = substr(Tmt,1,1))),
          terms=c("CO2Tmt","H2OTmt"))%>% plot(rawdata=T,ci=T,jitter=0.2,colors=c("red","blue")) + labs(title="Anet (µmol CO2/m2/s), Live Oak") # dot plots, but lose a LOT of power !

# can I make dot plots with imputed data
ggpredict(lmer(Anet~rescale(CO2)*rescale(meanSWC)+(1|Plot), data=filter(imp.1, Spp=="L")),
          terms=c("CO2","meanSWC [4,42]"))%>% plot(rawdata=T,ci=T, colors=c("red","blue")) + labs(title="Anet (µmol CO2/m2/s), Live Oak")
summary(lmer(Anet~rescale(CO2)*rescale(meanSWC)+(1|Plot), data=filter(imp.1, Spp=="L")))

ggpredict(lmer(Anet~rescale(CO2)*rescale(meanSWC)+(1|Plot), data=filter(imp.1, Spp=="V")),
          terms=c("CO2","meanSWC [4,42]"))%>% plot(rawdata=T,ci=T,colors=c("red","blue")) + labs(title="Anet (µmol CO2/m2/s), Valley Oak")
summary(lmer(Anet~rescale(CO2)*rescale(meanSWC)+(1|Plot), data=filter(imp.1, Spp=="V")))

# facetting by spp ?
ggpredict(lmer(Anet~rescale(CO2)*rescale(meanSWC)+(1|Plot), data=imp.1),
          terms=c("CO2","meanSWC [4,42]")) %>% 
  rename(Tmt = group) %>% 
  ggplot() + geom_line(aes(x=x, y=predicted)) + labs(title="Anet (µmol CO2/m2/s), Both Spp") + facet_wrap(~Tmt)
  
# what is going on with gs; the same pattern
ggpredict(lmer(gs~rescale(CO2)*rescale(meanSWC)+(1|Plot), data=filter(imp.1, Spp=="L")),
          terms=c("CO2","meanSWC [4,42]"))%>% plot(rawdata=T,ci=T, colors=c("red","blue")) + labs(title="gs (µmol CO2/m2/s), Live Oak")
summary(lmer(gs~rescale(CO2)*rescale(meanSWC)+(1|Plot), data=filter(imp.1, Spp=="L")))

imp.1.1 %>% 
  filter(Spp=="L") %>% 
  mutate(CO2Tmt = substr(Tmt,1,1)) %>% 
ggplot(aes(x = meanSWC, y = gs, color = CO2Tmt)) +
  geom_point() +
  geom_smooth(method = "lm") +
  theme_minimal()

imp.1.1 %>% 
  filter(Spp=="L") %>% 
  mutate(CO2Tmt = substr(Tmt,1,1)) %>% 
  mutate(H2OTmt = substr(Tmt,2,2)) %>% 
  ggplot(aes(x = CO2, y = gs, color = H2OTmt)) +
  geom_point() +
  geom_smooth(method = "lm") +
  theme_minimal()


# ∆ with watering figure with imputed data
## z-score change with watering
imp_boot <- imp.1 %>% 
  mutate(WUE = Anet/gs) %>% 
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

imp_boot <- imp_boot %>% mutate_if(is.numeric, list(z = zscore)) 

# start with the mean of ∆z with watering
zmean <- data.frame("AW1" = c(1:6), "EW1" = c(1:6)) # initialize results df
for(i in c(2:7)){zmean[(i-1),1] <-          # this is saying each of the 6 variables gets 1 row, and this is defining the first column in the results df
  # the top part is to get the mean ∆z, with watering, for aCO2 
  mean(
    do.call(c,lapply(1:1000, function(boot){
      a <- sample(unlist(imp_boot[imp_boot$Tmt=="AW",i+6])[!is.na(unlist(imp_boot[imp_boot$Tmt=="AW",i+6]))], replace = T)
      b <- sample(unlist(imp_boot[imp_boot$Tmt=="AD",i+6])[!is.na(unlist(imp_boot[imp_boot$Tmt=="AD",i+6]))], replace = T)
      mean(a)-mean(b)
    })))
# the second part is to get the mean ∆z, with watering, for eCO2 
zmean[(i-1),2] <- mean(
  do.call(c,lapply(1:1000, function(boot){
    a <- sample(unlist(imp_boot[imp_boot$Tmt=="EW",i+6])[!is.na(unlist(imp_boot[imp_boot$Tmt=="EW",i+6]))], replace = T)
    b <- sample(unlist(imp_boot[imp_boot$Tmt=="ED",i+6])[!is.na(unlist(imp_boot[imp_boot$Tmt=="ED",i+6]))], replace = T)
    mean(a)-mean(b)
  })))
}

# then mean - sd
zmin <- data.frame("AW1" = c(1:6), "EW1" = c(1:6)) # initialize results df
for(i in c(2:7)){ x <- do.call(c,lapply(1:1000, function(boot){
  a <- sample(unlist(imp_boot[imp_boot$Tmt=="AW",i+6])[!is.na(unlist(imp_boot[imp_boot$Tmt=="AW",i+6]))], replace = T)
  b <- sample(unlist(imp_boot[imp_boot$Tmt=="AD",i+6])[!is.na(unlist(imp_boot[imp_boot$Tmt=="AD",i+6]))], replace = T)
  mean(a)-mean(b)
}))
zmin[(i-1),1] <- mean(x) - sd(x)

y <- do.call(c,lapply(1:1000, function(boot){
  a <- sample(unlist(imp_boot[imp_boot$Tmt=="EW",i+6])[!is.na(unlist(imp_boot[imp_boot$Tmt=="EW",i+6]))], replace = T)
  b <- sample(unlist(imp_boot[imp_boot$Tmt=="ED",i+6])[!is.na(unlist(imp_boot[imp_boot$Tmt=="ED",i+6]))], replace = T)
  mean(a)-mean(b)
}))
zmin[(i-1),2] <- mean(y) - sd(y)
}

# then mean + sd
zmax <- data.frame("AW1" = c(1:6), "EW1" = c(1:6)) # initialize results df
for(i in c(2:7)){ x <- do.call(c,lapply(1:1000, function(boot){
  a <- sample(unlist(imp_boot[imp_boot$Tmt=="AW",i+6])[!is.na(unlist(imp_boot[imp_boot$Tmt=="AW",i+6]))], replace = T)
  b <- sample(unlist(imp_boot[imp_boot$Tmt=="AD",i+6])[!is.na(unlist(imp_boot[imp_boot$Tmt=="AD",i+6]))], replace = T)
  mean(a)-mean(b)
}))
zmax[(i-1),1] <- mean(x) + sd(x)

y <- do.call(c,lapply(1:1000, function(boot){
  a <- sample(unlist(imp_boot[imp_boot$Tmt=="EW",i+6])[!is.na(unlist(imp_boot[imp_boot$Tmt=="EW",i+6]))], replace = T)
  b <- sample(unlist(imp_boot[imp_boot$Tmt=="ED",i+6])[!is.na(unlist(imp_boot[imp_boot$Tmt=="ED",i+6]))], replace = T)
  mean(a)-mean(b)
}))
zmax[(i-1),2] <- mean(y) + sd(y)
}

imp_boot_nequals <- imp.1 %>% 
#  filter(! Code %in% firstfullherb$Code) %>% 
  filter(Spp=="L") %>% 
  mutate(WUE = Anet/gs, H2OTmt = substr(Tmt,2,2)) %>% 
  select(Tmt, H2OTmt, totmass, max_ht, max_dia, Anet, gs, WUE) %>% 
  rename(tot.mass = totmass) %>% 
  rename(max.ht = max_ht) %>% 
  rename(max.dia = max_dia) %>% 
  group_by(Tmt, H2OTmt) %>% 
  summarise_if(is.numeric, ~ sum(!is.na(.x))) %>% 
  group_by(H2OTmt) %>% 
  summarise_if(is.numeric, ~ min(.x)) %>% 
  t() %>% data.frame()

imp_boot_nequals <- imp_boot_nequals[-1,]
imp_boot_nequals[,3] <- rownames(imp_boot_nequals)
colnames(imp_boot_nequals) <- c("aCO2","eCO2", "variable")

variable_order <- c("Anet", "gs", "WUE", "tot.mass", "max.ht", "max.dia")
imp_bootL <- cbind(rbind(zmean, zmin, zmax), 
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
 # geom_text(data = fig2_nequals, aes(x = variable, y = -1.75, label = paste0("n = ",aCO2)), color="darkgray", size = 5) +
#  geom_text(data = fig2_nequals, aes(x = variable, y = -2.1, label = paste0("n = ",eCO2)), color="black", size = 5) +
  ggtitle("B. Quercus wislizeni (live oak)") +
  ylab("change with watering") + xlab("Plant Response") +
  theme_classic(base_size = 20) 

# facet_wrap and 1:1 line; imp.1 ?
df_final %>% 
#  imp.1 %>% 
 # filter(Spp=="L") %>% 
  select(gs, Anet, Tmt, Spp) %>% 
  group_by(Tmt, Spp) %>% 
  summarise_if(is.numeric, list(y=mean_narm, ymin=minus_se, ymax=plus_se)) %>% 
  ggplot(aes(x=gs_y, y=Anet_y, fill=Tmt)) + 
  geom_abline(slope = 100, intercept = 0, linetype="dashed") +
  geom_errorbar(aes(x=gs_y, ymin = Anet_ymin, ymax = Anet_ymax, color=Tmt), width = 0.001) +
  geom_errorbarh(aes(y=Anet_y, xmin = gs_ymin, xmax = gs_ymax, color=Tmt), height = 0.2) +
  geom_point(aes(color=Tmt, shape=Tmt), size = 4) +
  scale_shape_manual(values = c(21, 21, 16, 16)) +
  scale_fill_manual(values =c ("white","white","white","white")) +
  facet_wrap(~Spp, labeller = as_labeller(c("L" = "Q. wislizeni", "V" = "Q. lobata"))) +
  xlab(label = "Stomatal Conductance (mol H2O/m2/s)") +
  ylab(label = "Photosynthesis (µmol CO2/m2/s)") +
#  labs(title=c("Q. wislizeni", "Q. lobata")) +
  scale_x_continuous(expand = c(0,0), limits=c(0,0.25)) + scale_y_continuous(expand = c(0,0), limits=c(0,25)) +
  scale_color_manual(values = c("red", "blue", "red", "blue")) + theme_classic(base_size = 19)


# 
# I once more want to try Xiaoli's simple DAG which closes all backdoors, in case that is the reason we are seeing increased gs at high CO2

imp.1.2 <- # imputed data for Ls only, with watering treatment variable
  imp.1 %>% 
  filter(Spp == "L") %>% 
  mutate(H2OTmt = as.numeric(Tmt == "AW" | Tmt == "EW"))

xiaoli.psem <- psem(
  # gs is predicted by watering, CO2
  lm(gs_z ~ CO2_z + H2OTmt, na.action = na.omit, imp.1.2),
  # Anet is predicted by gs, CO2, SWC
  lm(Anet_z ~ gs_z + CO2_z + meanSWC_z, na.action = na.omit, imp.1.2),
  # SWC is predicted by watering, gs
  lm(meanSWC_z ~ H2OTmt + gs_z, na.action = na.omit, imp.1.2),
  # biomass is predicted by Anet, herbivory
  lm(totmass_z ~ Anet_z, na.action = na.omit, imp.1.2)
)
plot(xiaoli.psem)

library(dagitty)
dag <- dagitty("dag {
  CO2 -> gs
  CO2 -> Anet
  gs -> Anet
  gs -> SWC
  Anet -> mass
  H2OTmt -> gs
  H2OTmt -> SWC
  SWC -> Anet
}")
coordinates(dag) <-
  list(x=c(CO2 = -1, gs = 0, Anet = 0, mass = 0, H2OTmt = 3, SWC = 1.75),
       y=c(CO2 = 1, gs = 2, Anet = 3, mass = 4, H2OTmt = 2, SWC = 2.5))
plot(dag)

adjustmentSets(dag, exposure = "CO2", outcome = "mass")

xiaoli.psem.intx <- psem(
  # gs is predicted by watering, CO2
  lm(gs_z ~ CO2_z * H2OTmt, na.action = na.omit, imp.1.2),
  # Anet is predicted by gs, CO2, SWC
  lm(Anet_z ~ gs_z + CO2_z * meanSWC_z, na.action = na.omit, imp.1.2),
  # SWC is predicted by watering, gs
  lm(meanSWC_z ~ H2OTmt + gs_z, na.action = na.omit, imp.1.2),
  # biomass is predicted by Anet, herbivory
  lm(totmass_z ~ Anet_z, na.action = na.omit, imp.1.2)
)
plot(xiaoli.psem)
summary(xiaoli.psem.intx)

xiaoli.psem.intx2 <- psem(
  # gs is predicted by watering, CO2
  lm(gs_z ~ CO2_z * H2OTmt, na.action = na.omit, imp.1.2),
  # Anet is predicted by gs, CO2, SWC
  lm(Anet_z ~ gs_z + CO2_z * meanSWC_z, na.action = na.omit, imp.1.2),
  # SWC is predicted by watering, gs
  lm(meanSWC_z ~ H2OTmt + gs_z, na.action = na.omit, imp.1.2),
  # biomass is predicted by Anet, herbivory
  lm(totmass_z ~ Anet_z + gs_z, na.action = na.omit, imp.1.2)
)
summary(xiaoli.psem.intx2)
AIC(xiaoli.psem.intx, xiaoli.psem.intx2)



dag <- dagitty("dag {
  CO2 -> gs
  CO2 -> Anet
  SWC -> gs
  SWC -> Anet
  Spp -> gs
  Spp -> Anet
  Spp -> mass
  gs -> Anet
  Anet -> mass
}")
coordinates(dag) <-
  list(x=c(CO2 = -1, gs = 0, Anet = 0, mass = 0, SWC = -1, Spp = 3),
       y=c(CO2 = 2, gs = 2, Anet = 3, mass = 4, SWC = 3, Spp = 3))
plot(dag)

simple.psem <- psem(
  # gs is predicted by CO2*SWC, Spp
  lm(gs_z ~ CO2_z + meanSWC_z + Spp_num, imp.1.1),
  # Anet is predicted by gs, CO2*SWC, Spp
  lm(Anet_z ~ CO2_z + meanSWC_z + gs_z + Spp_num, imp.1.1),
  # biomass is predicted by Anet, herbivory
  lm(totmass_z ~ Anet_z + Spp_num, imp.1.1),
  gs_z %~~% totmass_z,
  totmass_z %~~% meanSWC_z)

summary(simple.psem)

simple.psem.raw <- psem(
  # gs is predicted by CO2*SWC, Spp
  lm(gs_z ~ CO2_z * meanSWC_z + Spp_num, na.action = na.omit, df_final_z),
  # Anet is predicted by gs, CO2*SWC, Spp
  lm(Anet_z ~ CO2_z * meanSWC_z + gs_z + Spp_num, na.action = na.omit, df_final_z),
  # biomass is predicted by Anet, herbivory
  lm(totmass_z ~ Anet_z + Spp_num, na.action = na.omit, df_final_z),
  gs_z %~~% totmass_z
)
summary(simple.psem.raw)

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

# I have dropped gs from my Anet model above, because they are highly correlated and including gs soaks up all the variation for Anet, meaning the effects of CO2xSWC get washed out

# Here below, as a separate question, I will ask whether gs mediates CO2 effects on Anet
model_mediation <- psem(
  # Step 1: gs as a mediator
  lmer(gs_z ~ CO2_z * meanSWC_z + Spp_num + (1|Plot), data = imp.1.1),
  
  # Step 2: Anet as the outcome
  lmer(Anet_z ~ gs_z + CO2_z + meanSWC_z + Spp_num + (1|Plot), data = imp.1.1)
)
summary(model_mediation)
plot(model_mediation)

# make a model with no interaction to do AIC comparison
simpler.psem.er <- psem( 
  # gs is predicted by CO2*SWC, Spp
  lmer(gs_z ~ CO2_z + meanSWC_z + Spp_num + (1|Plot), imp.1.1),
  # Anet is predicted by gs, CO2*SWC, Spp
  lmer(Anet_z ~ CO2_z + meanSWC_z + Spp_num + (1|Plot), imp.1.1),
  # biomass is predicted by Anet, Spp
  lmer(totmass_z ~ Anet_z + Spp_num + (1|Plot), imp.1.1),
  gs_z %~~% totmass_z,
  gs_z %~~% Anet_z
) # estimates the total effect of CO2 x SWC on Anet
summary(simpler.psem.er)
AIC(simple.psem.er, simpler.psem.er)

# I am going to run this last model as a brms
# fit Bayesian models
gs_mod <- bf(gs_z | mi() ~ CO2_z+meanSWC_z + Spp + (1|Plot)) 
Anet_mod3 <- bf(Anet_z | mi() ~ CO2_z*meanSWC_z + gs_z + Spp + (1|Plot)) 
totmass_mod1 <- bf(totmass_z | mi() ~ Anet_z + Spp + (1|Plot)) 

library(brms)
simple_fit <- brm(gs_mod + 
                  Anet_mod3 + 
                  totmass_mod1 +
                  set_rescor(FALSE),
                  data = imp.1,
                  cores = 4, chains = 4, save_pars = save_pars(all=TRUE) )
summary(simple_fit)


imp.1 %>% 
  select(max_ht:gs) %>% 
  select(where(is.numeric)) %>% 
  cor(use = "pairwise.complete.obs") %>% 
  corrplot::corrplot()

imp.1.1 %>% 
  select(max_ht_z:gs_z) %>% 
  select(where(is.numeric)) %>% 
  cor(use = "pairwise.complete.obs") %>% 
  corrplot::corrplot()

library(car)
# finding out why gs -> mass keeps showing up
vif(lm(Anet ~ CO2 * meanSWC + gs + Spp_num, data = df_final_z), type="predictor")
vif(lm(Anet ~ CO2 * meanSWC + Spp_num, data = df_final_z), type="predictor")
vif(lm(Anet ~ CO2 * meanSWC + gs + Spp_num, data = imp.1.1), type="predictor")

car::crPlots(lm(totmass_z ~ Anet_z + Spp_num + gs_z, data = imp.1.1))

# OK we established that Anet and gs are (rightfully) highly correlated, so that is why the d-sep test was showing such an important path from gs to mass (suppressor effect, where we controlled for a mediator, Anet, and a variable ends up looking like it has a strong [and reverse-sign] effect on outcome. gs does NOT correlate with mass in the raw data, but the relationship between gs and mass is negative conditional on Anet: Anet suppresses shared variance, making gs seem like a impt predictor of mass.) so, model residual covariance between gs and mass, specifying that it is not a causal path

AIC(lmer(Anet_z ~ CO2_z * meanSWC_z + gs_z + Spp_num + (1|Plot), data = df_final_z),lmer(Anet_z ~ CO2_z * meanSWC_z + Spp_num + (1|Plot), data = df_final_z))

# draw the DAG with residual correlation; cute names for plotting
dag <- dagitty("dag {
  CO2 -> gs
  CO2 -> Anet
  SWC -> gs
  SWC -> Anet
  Spp -> gs
  Spp -> Anet
  Spp -> mass
  Anet -> mass
  gs <-> mass
  gs <-> Anet
}") # gs <-> mass indicates residual (non-causal) correlation

# real variable names for testing against data
dag1 <- dagitty("dag {
  CO2_z -> gs_z
  CO2_z -> Anet_z
  meanSWC_z -> gs_z
  meanSWC_z -> Anet_z
  Spp_num -> gs_z
  Spp_num -> Anet_z
  Spp_num -> totmass_z
  Anet_z -> totmass_z
  gs_z <-> totmass_z
  gs_z <-> Anet_z
}") 

# Optional: set layout coordinates for a clean plot
# coordinates(dag) <- list(
#   x = c(CO2 = 0, SWC = 0, Spp = 0, gs = 1, Anet = 2, mass = 3),
#   y = c(CO2 = 3, SWC = 2, Spp = 1, gs = 2.5, Anet = 2.5, mass = 2.5)
# )
coordinates(dag) <- list(
  x = c(CO2 = 0, SWC = 0, Spp = 2, gs = 1, Anet = 1, mass = 1),
  y = c(CO2 = 3, SWC = 2, Spp = 2, gs = 3, Anet = 2, mass = 1)
)

# Plot the DAG using ggdag for a nice visual
ggdag(dag, layout = "manual") +
  theme_classic() +
  ggtitle("CO2 and SWC Effects on Plant Mass via gs and Anet") +
  theme(plot.title = element_text(size = 14, face = "bold"))

# test the DAG against the data
# test the DAG against data
localTests(
  x = dag1,
  #  data = select(drop_na(final_df2.1), where(is.numeric))
  data = select(imp.1.1, CO2_z, gs_z, Anet_z, meanSWC_z, Spp_num, totmass_z),
  abbreviate.names = F)

plotLocalTestResults(localTests(dag1, 
            data = select(imp.1.1, CO2_z, gs_z, Anet_z, meanSWC_z, Spp_num, totmass_z)))
# this DAG is all right for the data, once we do the %~~% thing for gs on mass and Anet

impliedConditionalIndependencies(dag1)

# problem, still not settled on the right model
# including gs as Anet predictor washes out intxn on Anet
vif(lmer(Anet_z ~ CO2_z*meanSWC_z + gs_z + Spp_num + (1|Plot), df_final_z), type="predictor")
vif(lmer(Anet_z ~ CO2_z*meanSWC_z + gs_z + Spp_num + (1|Plot), imp.1.1), type="predictor")

# Anet and gs look super correlated on a bivariate plot
df_final_z_means <- df_final_z %>% 
  group_by(Tmt, Spp) %>% 
  summarise(mean_gs = mean(gs, na.rm=T), mean_Anet = mean(Anet, na.rm=T))
  
ggplot() + geom_point(df_final_z, mapping=aes(x=gs, y=Anet, color=Tmt, shape=Tmt, fill=Tmt)) + 
  geom_point(df_final_z_means, mapping=aes(x=mean_gs, y=mean_Anet, color=Tmt, shape=Tmt, fill=Tmt), size=5, alpha = 0.5) +
  facet_wrap(~Spp) + 
  scale_shape_manual(values = c(21, 21, 16, 16)) +
  scale_color_manual(values = c("red", "blue", "red", "blue")) +
  scale_fill_manual(values =c ("white","white","white","white")) + theme_classic()
