
biomass <- read.csv("/Users/paigekouba/Documents/UC_Davis/2021_Winter/Quals/Proposal/Chapter 1/TinyFACE/Data/Biomass_final.csv")
#head(biomass)
# associate species and treatment with each code
biomass <- biomass %>% 
  mutate(Spp = str_sub(Code, - 3, - 3)) %>% 
  mutate(Plot = str_remove(str_remove(Code,str_sub(Code, -2)), '[[:alpha:]]'))

lookup <- data.frame(c(1:16), c("AW","ED","AD","EW",
                               "AW","ED","EW","AD",
                               "ED","AW","EW","AD",
                               "ED","AW","EW","AD"))
names(lookup) <- c("Plot","Tmt")

biomass <- merge(lookup, biomass, by = 'Plot')

# remove NAs of stemwet_g
stem_wet <- biomass %>% 
  filter(!is.na(StemWet_g))

leaf_wet <- biomass %>% 
  filter(!is.na(LeafWet_g))

all_wet <- biomass %>% 
  filter(!is.na(StemWet_g)) %>% 
  filter(!is.na(LeafWet_g)) %>% 
  mutate(abg_wet_g = StemWet_g + LeafWet_g)

# plot biomass of stem, leaf, vs treatment by species
ggplot(leaf_wet, aes(x= factor(Tmt, levels= c("AD","ED","AW","EW")), y= log(LeafWet_g))) +
  geom_boxplot()+
  geom_jitter()+
  facet_wrap( ~ Spp)
# even slopes more or less

ggplot(stem_wet, aes(x= factor(Tmt, levels= c("AD","ED","AW","EW")), y= log(StemWet_g))) +
  geom_boxplot(aes(colour=Spp))+
  geom_jitter(aes(colour=Spp, size=SeedMass.g.))
# steeper increase for dry + elevated than wet + elevated; decrease in wet + elevated for live oak?

ggplot(all_wet, aes(x= factor(Tmt, levels= c("AD","ED","AW","EW")), y= log(abg_wet_g))) +
  geom_boxplot()+
  geom_jitter()+
  facet_wrap( ~ Spp)
# steeper increase for dry + elevated than wet + elevated; decrease in wet + elevated for live oak?


all_wet$log_abg_wet_g <- log(all_wet$abg_wet_g)

abg_se <- summarySE(all_wet, measurevar = "log_abg_wet_g", groupvars=c("Tmt","Spp"), na.rm = TRUE)

abg_se_D <- abg_se %>% 
  filter(Tmt == "AD" | Tmt == "ED") %>% 
  mutate(Water = "Dry") %>% 
  mutate(CO2 = case_when(Tmt =="AD" ~ "Ambient",
                         TRUE ~ "Elevated"))

abg_se_W <- abg_se %>% 
  filter(Tmt == "AW" | Tmt == "EW") %>% 
  mutate(Water = "Wet") %>% 
  mutate(CO2 = case_when(Tmt =="AW" ~ "Ambient",
                         TRUE ~ "Elevated"))

ggplot() + 
  geom_point(data = abg_se_D, aes(x=factor(CO2), y=log_abg_wet_g), colour = "red", size = 7) +
  geom_errorbar(data = abg_se_D, aes(x=factor(CO2), 
    ymin=log_abg_wet_g-se, ymax=log_abg_wet_g+se), width=.1, colour = "red", linewidth = 1) +
  geom_point(data = abg_se_W, aes(x=factor(CO2), y=log_abg_wet_g), colour = "blue", size = 7)+
  geom_errorbar(data = abg_se_W, aes(x=factor(CO2), y=log_abg_wet_g, 
    ymin=log_abg_wet_g-se, ymax=log_abg_wet_g+se), width=.1, colour = "blue", linewidth = 1) +
  facet_wrap( ~ Spp) +
  ggtitle("Live and Valley Oak Response to eCO2 x Water Stress") +
  scale_x_discrete(labels=c("Ambient \nCO2", "Elevated \nCO2")) +
  xlab("Treatment") + ylab("log(Aboveground Biomass (g))") +
  theme_classic(base_size=22)
