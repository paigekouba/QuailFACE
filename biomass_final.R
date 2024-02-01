
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
  geom_boxplot(aes(colour=Spp))+
  geom_jitter(aes(colour=Spp, size=SeedMass.g.))
# even slopes more or less

ggplot(stem_wet, aes(x= factor(Tmt, levels= c("AD","ED","AW","EW")), y= log(StemWet_g))) +
  geom_boxplot(aes(colour=Spp))+
  geom_jitter(aes(colour=Spp, size=SeedMass.g.))
# steeper increase for dry + elevated than wet + elevated; decrease in wet + elevated for live oak?

ggplot(all_wet, aes(x= factor(Tmt, levels= c("AD","ED","AW","EW")), y= log(abg_wet_g))) +
  geom_boxplot(aes(colour=Spp))+
  geom_jitter(aes(colour=Spp))
# steeper increase for dry + elevated than wet + elevated; decrease in wet + elevated for live oak?