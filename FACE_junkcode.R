# FACE_junkcode
within_4.9.24 <- left_join(within_4.9.24, points_within, by = "position")
library(interp)
library(akima)
library(purrr)
interpolated <- interp(x = within_4.9.24$x_within, y = within_4.9.24$y_within, z = within_4.9.24$DeltaTest, linear=TRUE, extrap = TRUE, duplicate = "mean")
#convert this to a long form dataframe
interp_df <- expand_grid(i = seq_along(interpolated$x), 
                         j = seq_along(interpolated$y)) %>% 
  mutate(x = interpolated$x[i],
         y = interpolated$y[j],
         DeltaTest = map2_dbl(i, j, ~interpolated$z[.x,.y])) %>% 
  select(-i, -j)

# visualize all interpolated points
ggplot(data = interp_df, aes(x = x, y = y, color = DeltaTest)) + geom_point(size=6) + scale_color_continuous(type = "viridis") + theme_classic()

# visualize all interpolated points, with measured values overlaid
ggplot() + 
  geom_point(data = interp_df, aes(x = x, y = y, color = DeltaTest), size=4) + scale_color_continuous(type = "viridis") + theme_classic() +
  geom_text(data = summarise(group_by(within_4.9.24, position), mean=round(mean(DeltaTest),1)), aes(x=x_within, y=y_within, label = mean), size=5)

ggplot(data = interp_df, aes(x = x, y = y)) +
  geom_contour(aes(z = DeltaTest, colour = stat(level)), size = 1.2) +
  metR::geom_text_contour(aes(z=DeltaTest), stroke = 0.15) +
  coord_equal()

# also, mean and sd at different spots in the grid
within_4.9.24 %>% 
  group_by(position) %>% 
  summarise(mean = round(mean(DeltaTest), 1), sd = round(sd(DeltaTest), 1)) %>% 
  ggplot(aes(x=x_within, y=y_within, label = mean)) + geom_text(aes(color=sd), size = 5)

# points_out1 <- points_out[1:7,]
# points_out2 <- points_out[8:13,]
# points_out3 <- points_out[14:17,]
# points_out4 <- points_out[18:21,]


#interpolated2 <- interp(x = within_4.9.24$x_within, y = within_4.9.24$y_within, z = within_4.9.24$DeltaTest, linear=TRUE, extrap = FALSE, duplicate= "mean")

## Growth and mortality (Inventory)
library(lubridate)
library(RColorBrewer)
inventory_raw <- read.csv("/Users/paigekouba/Documents/UC_Davis/2021_Winter/Quals/Proposal/Chapter 1/TinyFACE/GitHub/QuailFACE/RawData/Inventory8.csv")

inventory_raw$Inv..8.Date <- "11/20/23"

# want a long format df with columns =  [ Code, Plot, Tmt, Date, Ht ] x 129 
# then, plot x = date y = ht, group_by Code, color = Tmt

# try it just with inventory date
inv_long <- inventory_raw %>% 
  select(Code, Inv..1.Date, Inv..2.Date, Inv..3.Date, Inv..4.Date, Inv..5.Date, Inv..6.Date, Inv..7.Date, Inv..8.Date) %>% 
  pivot_longer(!Code, names_to = "Date") %>% 
  mutate(value = mdy(value)) 

# now do it with ht data
inv_hts <- inventory_raw %>% 
  select(Code, paste0("Ht.mm..",c(1:8))) %>% 
  pivot_longer(!Code, names_to = "key", values_to = "ht_mm")

inv_long <- cbind(inv_long, inv_hts) %>%
  select(!4) %>% 
  mutate(Plot = if_else(nchar(Code) == 4, substr(Code, 1,1), substr(Code,1,2)))

inv_long <- left_join(inv_long, lookup, by = "Plot")

inv_long <- inv_long %>% 
  mutate(Spp = substr(Code, nchar(Code)-2,nchar(Code)-2))

ggplot(inv_long) +
  geom_jitter(aes(x = value, y = ht_mm, group = Code, color = Spp)) +
  geom_line(aes(x = value, y = ht_mm, group = Code, color = Spp)) + facet_grid(Spp ~ Tmt, scales = "free")

inv_cond <- inventory_raw %>% 
  select(Code, paste0("Cond..",c(1:8))) %>% 
  pivot_longer(!Code, names_to = "key", values_to = "cond")

cond_long <- cbind(inv_long, inv_cond) 

cond_long <- cond_long[,c(1:3,5:8,11)]

ggplot(cond_long) +
  geom_jitter(aes(x = value, y = cond, group = Code, color = Spp)) +
  geom_line(aes(x = value, y = cond, group = Code, color = Spp)) + facet_grid(Spp ~ Tmt, scales = "free")  

inv_dia <- inventory_raw %>% 
  select(Code, paste0("Dia.mm..",c(1:8))) %>% 
  pivot_longer(!Code, names_to = "key", values_to = "dia")

dia_long <- cbind(inv_long, inv_dia) 

dia_long <- dia_long[,c(1:3,5:8,11)]

dia_long[which.max(dia_long$dia),] # two outliers need adjusting
#     Code        Date      value ht_mm Plot Tmt Spp dia
# 104 2L1a Inv..8.Date 2023-11-20   156    2  ED   L 389
dia_long[104,8] <- 3.89 # assume it's a decimal error
dia_long[which.max(dia_long$dia),]
#      Code        Date      value ht_mm Plot Tmt Spp dia
# 1774 3V3b Inv..6.Date 2023-08-26    96    3  AD   V 54.9
dia_long[1774,8] <- 5.49 # assume a decimal error again, even though this is quite possiblya leaf length
#      Code        Date      value ht_mm Plot Tmt Spp dia
# 830 9L4b Inv..6.Date 2023-08-26  4.24    9  ED   L 37.8
dia_long[830,8] <- 3.78




ggplot(dia_long) +
  geom_jitter(aes(x = value, y = dia, group = Code, color = Spp)) +
  geom_line(aes(x = value, y = dia, group = Code, color = Spp)) + facet_grid(Spp ~ Tmt, scales = "free")  

dia_long %>% 
  group_by(Spp, Tmt, Date) %>% 
  summarise(mean_dia = mean(dia, na.rm = TRUE), mean_ht = mean(ht_mm, na.rm = TRUE),
            sd_dia = sd(dia, na.rm = TRUE), sd_ht = sd(ht_mm, na.rm = TRUE)) %>% View()
ggplot(aes(x=Date, y = mean_ht)) + geom_point(aes(x=Date, y = mean_ht)) + geom_line(aes(x=Date, y = mean_ht)) + facet_grid(Spp ~ Tmt, scales = "free")


dia_long %>% 
  group_by(Spp, Tmt, Date) %>% 
  summarise(mean_dia = mean(dia, na.rm = TRUE), mean_ht = mean(ht_mm, na.rm = TRUE),
            sd_dia = sd(dia, na.rm = TRUE), sd_ht = sd(ht_mm, na.rm = TRUE)) %>% 
  ggplot(aes(x=Date, y = mean_ht, group = Tmt)) + 
  geom_pointrange(aes(ymin = mean_ht - sd_ht/7, ymax = mean_ht + sd_ht/7, color = Tmt, shape = Tmt), position = position_dodge(0.2), size = 1.5, alpha = 0.75) + 
  scale_color_manual(values = c("red", "blue", "red", "blue")) +
  scale_shape_manual(values = c(16,16,17,17)) +
  geom_line(aes(color = Tmt), position = position_dodge(0.2), linewidth = 1.2) + facet_grid(~Spp)


dia_long %>% 
  group_by(Spp, Tmt, Date) %>%
  tally() %>% View()

# for dia_long, (get Codes w/o a and b), find Codes for seedlings with full herbivory and drop after the date of first full herbivory

dia_long$Code <- if_else(nchar(dia_long$Code)==4,substr(dia_long$Code,1,3),substr(dia_long$Code,1,4))
unique(dia_long$Date)

inv_dia <- inventory_raw %>% 
  select(Code, paste0("Dia.mm..",c(1:8))) %>% 
  pivot_longer(!Code, names_to = "key", values_to = "dia")

dia_long <- cbind(inv_long, inv_dia) 

dia_long <- dia_long[,c(1:3,5:8,11)]

dia_long[which.max(dia_long$dia),] # two outliers need adjusting
#     Code        Date      value ht_mm Plot Tmt Spp dia
# 104 2L1a Inv..8.Date 2023-11-20   156    2  ED   L 389
dia_long[104,8] <- 3.89 # assume it's a decimal error
dia_long[which.max(dia_long$dia),]
#      Code        Date      value ht_mm Plot Tmt Spp dia
# 1774 3V3b Inv..6.Date 2023-08-26    96    3  AD   V 54.9
dia_long[1774,8] <- 5.49 # assume a decimal error again, even though this is quite possiblya leaf length
#      Code        Date      value ht_mm Plot Tmt Spp dia
# 830 9L4b Inv..6.Date 2023-08-26  4.24    9  ED   L 37.8
dia_long[830,8] <- 3.78




ggplot(dia_long) +
  geom_jitter(aes(x = value, y = dia, group = Code, color = Spp)) +
  geom_line(aes(x = value, y = dia, group = Code, color = Spp)) + facet_grid(Spp ~ Tmt, scales = "free")  

dia_long %>% 
  group_by(Spp, Tmt, Date) %>% 
  summarise(mean_dia = mean(dia, na.rm = TRUE), mean_ht = mean(ht_mm, na.rm = TRUE),
            sd_dia = sd(dia, na.rm = TRUE), sd_ht = sd(ht_mm, na.rm = TRUE)) %>% View()
ggplot(aes(x=Date, y = mean_ht)) + geom_point(aes(x=Date, y = mean_ht)) + geom_line(aes(x=Date, y = mean_ht)) + facet_grid(Spp ~ Tmt, scales = "free")


dia_long %>% 
  group_by(Spp, Tmt, Date) %>% 
  summarise(mean_dia = mean(dia, na.rm = TRUE), mean_ht = mean(ht_mm, na.rm = TRUE),
            sd_dia = sd(dia, na.rm = TRUE), sd_ht = sd(ht_mm, na.rm = TRUE)) %>% 
  ggplot(aes(x=Date, y = mean_ht, group = Tmt)) + 
  geom_pointrange(aes(ymin = mean_ht - sd_ht/7, ymax = mean_ht + sd_ht/7, color = Tmt, shape = Tmt), position = position_dodge(0.2), size = 1.5, alpha = 0.75) + 
  scale_color_manual(values = c("red", "blue", "red", "blue")) +
  scale_shape_manual(values = c(16,16,17,17)) +
  geom_line(aes(color = Tmt), position = position_dodge(0.2), linewidth = 1.2) + facet_grid(~Spp)

# removes the a or b from the end of the seedling ID ("Code")

# find Codes for seedlings with any herbivory and drop after the date of first  herbivory

# PCA first try

df_pca <- biomass_nh %>% 
  filter(Spp == "L") %>% 
  select(Code, StemWet_g, LeafDry_g, LeafWet_g, rootmass_g) %>% 
  mutate(rootshoot = rootmass_g/(StemWet_g + LeafWet_g), lwc = (LeafWet_g - LeafDry_g)/LeafWet_g) %>% 
  filter(!if_any(everything(),is.na))

df_pca_new <- df_pca[,-1]
rownames(df_pca_new) <- df_pca[,1]

df_pca_norm <- scale(df_pca_new)

## Covariance matrix
corr_matrix <- cor(df_pca_norm)
ggcorrplot(corr_matrix)

## Selection of principal components
# data.pca <- princomp(corr_matrix)
# summary(data.pca)
# data.pca$loadings[, 1:2]
# screeplot(data.pca)
# fviz_eig(data.pca, addlabels = TRUE)
# biplot(data.pca)
res.pca <- PCA(df_pca_norm)

fviz_pca_ind(res.pca, col.ind = "cos2", gradient.cols = c("#00AFBB", "#E7B800", "#FC4E07"), repel = TRUE)
fviz_pca_biplot(res.pca, repel = TRUE)

## Selection of principal components
# data.pca <- princomp(corr_matrix)
# summary(data.pca)
# data.pca$loadings[, 1:2]
# screeplot(data.pca)
# fviz_eig(data.pca, addlabels = TRUE)
# biplot(data.pca)

df_pca <- biomass_nh %>% 
  #  filter(Spp == "V") %>% 
  #  filter(Tmt == "EW") %>% 
  select(Code, Tmt, StemWet_g, LeafDry_g, LeafWet_g, rootmass_g) %>% 
  mutate(rootshoot = rootmass_g/(StemWet_g + LeafWet_g), lwc = (LeafWet_g - LeafDry_g)/LeafWet_g) %>% 
  filter(!if_any(everything(),is.na)) 

df_pca_new <- df_pca[,-c(1)]
df_pca_new <- as.data.frame(df_pca_new)
rownames(df_pca_new) <- df_pca[,1]
#rownames(df_pca_new) <- c("AD", "AW", "ED", "EW")

#df_pca_norm <- scale(df_pca_new)
df_pca_norm <- as.data.frame(cbind(df_pca_new[,1],scale(df_pca_new[,-1])))
df_pca_norm <- df_pca_norm %>% 
  mutate_at(c("StemWet_g","LeafDry_g","LeafWet_g","rootmass_g","rootshoot","lwc"), as.numeric)

## Covariance matrix
corr_matrix <- cor(df_pca_norm[,-1])
ggcorrplot(corr_matrix)

#res.pca <- PCA(df_pca_norm)
#res.pca <- prcomp(df_pca_norm[,-1])
res.pca <- prcomp(df_pca_new[,-1], scale = TRUE)

fviz_pca_ind(res.pca, col.ind = "cos2", gradient.cols = c("#00AFBB", "#E7B800", "#FC4E07"), repel = TRUE)
fviz_pca_biplot(res.pca, repel = TRUE)

fviz_pca_ind(res.pca, label = "none", habillage = df_pca_new$Tmt)
fviz_pca_biplot(res.pca, habillage = df_pca_new$Tmt, addEllipses = TRUE, repel = TRUE) + scale_color_manual(values=c("pink", "lightblue", "red", "blue")) + scale_fill_manual(values=c("pink", "lightblue", "red", "blue"))

# biomass %>% ggplot() + geom_boxplot( aes(x=Tmt, y= log(StemWet_g+LeafDry_g+rootmass_g), color = Tmt)) + facet_grid(~ Spp) +
#   geom_text(data = nequals, aes(x = Tmt, y = 4, label = paste0("N =",n)))
# # I want number of observations for each box
# nequals <- biomass %>% 
#   group_by(Tmt, Spp) %>% 
#   tally()

inv_cond <- inventory_raw %>% 
  select(Code, paste0("Cond..",c(1:8))) %>% 
  pivot_longer(!Code, names_to = "key", values_to = "cond")

cond_long <- cbind(inv_long, inv_cond) 

cond_long <- cond_long[,c(1:3,5:8,11)]

cond_long %>% 
  filter(Spp == "V") %>% 
  ggplot() +
  geom_point(aes(x = value, y = cond, color = Tmt)) +
  geom_line(aes(x = value, y = cond, color = Tmt)) + facet_wrap( ~ Code)  


# want a long format df with columns =  [ Code, Plot, Tmt, Date, Ht ] x 129 
# then, plot x = date y = ht, group_by Code, color = Tmt

# # try it just with inventory date
# inv_long1 <- inventory_thinned %>% 
#   select(Code, Inv..1.Date, Inv..2.Date, Inv..3.Date, Inv..4.Date, Inv..5.Date, Inv..6.Date, Inv..7.Date, Inv..8.Date) %>% 
#   mutate(Code = if_else(nchar(Code)==4,substr(Code,1,3),substr(Code,1,4)))
# 
# inv_long <- inv_long1 %>% 
#   pivot_longer(!Code, names_to = "Date") %>% 
#   mutate(value = mdy(value)) 
# 
# # now do it with ht data
# inv_hts <- inventory_thinned %>% 
#   select(Code, paste0("Ht.mm..",c(1:8))) %>% 
#   pivot_longer(!Code, names_to = "key", values_to = "ht_mm")
# 
# inv_long <- cbind(inv_long, inv_hts) %>%
#   select(!4) %>% 
#   mutate(Plot = if_else(nchar(Code)==3,substr(Code,1,1),substr(Code,1,2)))
# 
# inv_long <- left_join(inv_long, lookup, by = "Plot")
# 
# inv_long <- inv_long %>% 
#   mutate(Spp = substr(Code, nchar(Code)-1,nchar(Code)-1))
# 
# ggplot(inv_long) +
#   geom_point(aes(x = value, y = ht_mm, group = Code, color = Spp)) +
#   geom_line(aes(x = value, y = ht_mm, group = Code, color = Spp)) + facet_grid(Spp ~ Tmt, scales = "free")
# 
# inv_long %>% 
#   group_by(Spp, Tmt, value) %>% 
#   summarise(mean_ht = mean(ht_mm, na.rm = TRUE),sd_ht = sd(ht_mm, na.rm = TRUE)) %>% 
#   ggplot(aes(x=value, y = mean_ht, group = Tmt)) + 
#   geom_pointrange(aes(ymin = mean_ht - sd_ht/7, ymax = mean_ht + sd_ht/7, color = Tmt, shape = Tmt), position = position_dodge(0.2), size = 1.5, alpha = 0.75) + 
#   scale_color_manual(values = c("red", "blue", "red", "blue")) +
#   scale_shape_manual(values = c(16,16,17,17)) +
#   geom_line(aes(color = Tmt), position = position_dodge(0.2), linewidth = 1.2) + facet_grid(rows = vars(Spp), scales = "free")
# 
# inv_long %>%
#   filter(!is.na(ht_mm)) %>% 
#   group_by(Spp, Tmt, value) %>%
#   tally()

# for inv_long, (get Codes w/o a and b), find Codes for seedlings with full herbivory and drop after the date of first full herbivory
inv_long_nh0 <- right_join(firstherb, inv_long, by = "Code") 
inv_long_nfh0 <- right_join(firstfullherb, inv_long, by = "Code") 

sum(is.na(inv_long_nh0$firstherb)) # 2304
nrow(inv_long_nh0[is.na(inv_long_nh0$firstherb),2]) # 2304
inv_long_nh0[is.na(inv_long_nh0$firstherb),2] <- ymd("9999-09-09")
inv_long_nfh0[is.na(inv_long_nfh0$firstherb),2] <- ymd("9999-09-09")

inv_long_nh <- inv_long_nh0 %>% 
  filter(firstherb > value)

inv_long_nfh <- inv_long_nfh0 %>% 
  filter(firstherb > value)

# now I need to use these to predict the final StemWet_g 
lm_all <- lm(StemWet_g ~ polym(Ht.mm..1,Ht.mm..4,Ht.mm..5, degree = 2, raw = TRUE), data = biomass_nh2) # test

biomass2$pred1 <- abs(predict(lm_all, newdata = biomass2))
biomass_nh2$pred2 <- predict(lm_LD, newdata = biomass_nh2)
biomass_nh2$pred3 <- predict(lm_VD, newdata = biomass_nh2)

ggplot(biomass_nh2, aes(x = StemWet_g, y=pred1)) + # plot
  geom_point() 

ggplot(biomass2, aes(x=Ht.mm..8, y = StemWet_g)) + # plot
  geom_point(biomass2, mapping= aes(x = Ht.mm..8, y=pred1)) +
  #  geom_point(filter(biomass_nh2, Spp == "L" & Tmt == "AD" | Tmt == "ED"), mapping= aes(x = Ht.mm..8, y=pred2)) +
  #geom_point(filter(biomass_nh2, Spp == "V" & Tmt == "AD" | Tmt == "ED"), mapping= aes(x = Ht.mm..8, y=pred3)) +
  geom_point(biomass2, mapping= aes(x= Ht.mm..8, y = StemWet_g, group = Tmt, color = Tmt)) + facet_grid( ~ Spp)

# ggplot(biomass_nh2, aes(x = StemWet_g, y=pred1)) + # plot
#   geom_point() 

# ggplot(biomass_nh2, aes(x=Ht.mm..8, y = StemWet_g)) + # plot
#   geom_point(biomass_nh2, mapping= aes(x = Ht.mm..8, y=pred1)) +
#   #  geom_point(filter(biomass_nh2, Spp == "L" & Tmt == "AD" | Tmt == "ED"), mapping= aes(x = Ht.mm..8, y=pred2)) +
#   #geom_point(filter(biomass_nh2, Spp == "V" & Tmt == "AD" | Tmt == "ED"), mapping= aes(x = Ht.mm..8, y=pred3)) +
#   geom_point(biomass_nh2, mapping= aes(x= Ht.mm..8, y = StemWet_g, group = Tmt, color = Tmt)) + facet_grid( ~ Spp)

# models to see how well hts predict final biomass
summary(lm(StemWet_g ~ Ht.mm..1+Ht.mm..2+Ht.mm..3+Ht.mm..4+Ht.mm..5+Ht.mm..6+Ht.mm..7+Ht.mm..8, data = filter(biomass_nh2, Spp == "L" & Tmt == "AW" | Tmt == "EW")))
summary(lm(StemWet_g ~ Ht.mm..1+Ht.mm..2+Ht.mm..3+Ht.mm..4+Ht.mm..5, data = filter(biomass_nh2, Spp == "L" & Tmt == "AD" | Tmt == "ED")))


# this is looking so weird for VW still. I'm going to plot just that subset and just that model
# df with just Spp == V, Tmt == W, non-herbivory
LW_df <- biomass_nh2 %>% 
  filter(Spp=="L") %>% 
  filter(H2OTmt =="W")

ggplot(LW_df, aes(x = Ht.mm..5, y=StemWet_g, color = H2OTmt)) + # plot
  geom_point() + geom_smooth(aes(Ht.mm..5, StemWet_g), method = "lm") 
# yes, this is what I want :)

# LiCOR_df_extp <- 
df_all %>%   
  select(HHMMSS, Photo, Cond, Ci, CO2R, SWC, Date, Log, X., Time, ID, Plot, Tmt, Spp) %>% 
  group_by(ID) %>% 
  summarise(A_net.350 = predict(lm(Photo ~ poly(Ci,2), data = df_all), newdata=list(350))) %>% View()
# this part is not working because data = needs to have just points from this ID grouping; time for a loop?

# is it working?
predict(lm(Photo ~ Ci, data = test7_df), newdata=data.frame(Ci = 350))
test7_df <- df_all %>%
  select(HHMMSS, Photo, Cond, Ci, CO2R, SWC, Date, Log, X., Time, ID, Plot, Tmt, Spp) %>%
  filter(ID == LiCOR_IDs[7])

# for(i in 1:length(LiCOR_IDs)){
#   LiCOR_extp[i] <- predict(lm(Photo ~ Ci, data = df_all), newdata=list(350))
# }

# in this part I want to get the mean of the numerical variables  (Cond, SWC, interpol) for cases where there are two observations for a given ID. 
df_all <- df_all %>% 
  filter(!is.na(interpol)) %>% 
  group_by(ID) %>% 
  mutate(HHMMSS = mean(HHMMSS), Cond = mean(Cond), SWC = mean(SWC), interpol = mean(interpol)) %>% 
  ungroup() %>% 
  distinct(ID, .keep_all = TRUE) %>% # removes the duplicate rows per ID
  mutate(WUE = interpol/Cond) %>% 
  mutate(CO2Tmt = substring(Tmt,1,1), H2OTmt = substring(Tmt,2,2))

## not junk, putting this here until I can compare with modeled data!
# I want number of observations for each box
nequals_nfh <- biomass_nfh %>% 
  group_by(Tmt, Spp) %>% 
  tally()
# total biomass grouped by Tmt, for all seedlings without FULL herbivory
biomass_nfh %>% ggplot() + geom_boxplot( aes(x=Tmt, y= (StemWet_g+LeafDry_g+rootmass_g), color = Tmt)) + facet_grid(rows = vars(Spp), scales = "free") +
  geom_text(data = nequals_nfh, aes(x = Tmt, y = 0.2, label = paste0("N = ",n))) + labs(y = "Total Biomass (g)") +
  scale_color_manual(values = c("pink", "lightblue", "red", "blue")) 

nequals_nh <- biomass_nh %>% 
  group_by(Tmt, Spp) %>% 
  tally()
# total biomass grouped by Tmt, for all seedlings without ANY herbivory
biomass_nh %>% ggplot() + geom_boxplot(aes(x = Tmt, y =(StemWet_g+LeafDry_g+rootmass_g), color = Tmt)) + facet_grid(rows = vars(Spp), scales = "free") + geom_text(data = nequals_nh, aes(x = Tmt, y = 0.2, label = paste0("N = ",n))) + labs(y = "Total Biomass (g)") + scale_color_manual(values = c("pink", "lightblue", "red", "blue")) 

# Leaf Water Content grouped by Tmt, for all seedlings without ANY herbivory
biomass_nh %>% ggplot() + geom_boxplot(aes(x = Tmt, y = (LeafWet_g - LeafDry_g)/LeafWet_g, color=Tmt)) + facet_grid(rows = vars(Spp), scales = "free") + geom_text(data = nequals_nh, aes(x = Tmt, y = 0.5, label = paste0("N = ",n))) + labs(y = "Leaf Water Content") +
  scale_color_manual(values = c("pink", "lightblue", "red", "blue")) 

nequals_bm <- biomass %>% 
  group_by(Tmt, Spp) %>% 
  tally()
biomass %>% # root mass for all seedlings (no need to filter on herbivory)
  ggplot() + geom_boxplot(aes(x = Tmt, y = rootmass_g, color=Tmt)) + facet_grid(rows = vars(Spp), scales = "free") + geom_text(data = nequals_bm, aes(x = Tmt, y = 0.5, label = paste0("N = ",n))) + labs(y = "Root mass (g)") +
  scale_color_manual(values = c("pink", "lightblue", "red", "blue")) 

biomass_nh %>% # root:shoot ratio for seedlings without any herbivory
  ggplot() + geom_boxplot(aes(x=Tmt, y= (rootmass_g/(StemWet_g+LeafDry_g)), color = Tmt)) + facet_grid(rows =  vars(Spp), scales = "free") +
  geom_text(data = nequals_nh, aes(x = Tmt, y = 2.5, label = paste0("N = ",n))) + labs(y = "Root:Shoot") +
  scale_color_manual(values = c("pink", "lightblue", "red", "blue")) 
# seems like: drought stressed plants invest more in roots, and (V?) eCO2 lets them double down


# will make a new df called biomass_pred, which has observed values for all the non-herbivory seedlings, but predicted values for the 47 eaten
sum(biomass$Code %in% herb_list$Code) # 47
View(biomass[which(biomass$Code %in% herb_list$Code),])


# there is a rogue point in the AW group, survey 7 height; around 20mm but then ~400 on either side. Find it
inv_all[which(inv_all$Code %in% firstherb$Code),] %>% 
  #inv_all_nh[which(inv_all_nh$Code %in% firstherb$Code),] %>% 
  na.omit() %>% 
  group_by(Spp, Tmt, value) %>% 
  ggplot(aes(x=value, y=ht_mm,  group = Code, color = Tmt, shape = Tmt, size = 1.5, alpha = 0.75)) +
  geom_line(aes(x=value, y=ht_mm,  group = Code), position = position_dodge(20, preserve = "total"), linewidth = 1.2, na.rm = TRUE) +
  geom_point(position = position_dodge(20, preserve = "total")) +
  scale_color_manual(values = c("#e8665d", "#828cfa", "#850a01", "#010c85")) +
  scale_shape_manual(values = c(16,16,17,17)) +
  facet_wrap(~ Code, scales = "free")

#lm_LW <- lm(StemWet_g ~ polym(Ht.mm..1,Ht.mm..3,Ht.mm..5, degree = 2, raw = TRUE), data = filter(biomass_nh2, Spp == "L" & H2OTmt == "W"))
#biomass_nh2$predVW <- predict(lm(StemWet_g ~ Ht.mm..5, data = filter(biomass_nh2, Spp == "V" & H2OTmt == "W")), newdata = biomass_nh2)
#biomass_nh2$predLW <- predict(lm(StemWet_g ~ Ht.mm..5, data = filter(biomass_nh2, Spp == "L" & H2OTmt == "W")), newdata = biomass_nh2)

# Everything seems ok; plot model line with real and fake data to be sure
ggplot(biomass_nh2, aes(x = Ht.mm..5, y=StemWet_g, color = H2OTmt)) + # plot
  geom_point() + facet_grid(~Spp) +
  geom_hline(aes(yintercept=0)) +
  geom_smooth(data = biomass_nh2, aes(x=Ht.mm..5, y=pred, group = H2OTmt), color = c("red")) +
  geom_point(data = biomass_nh2, aes(x=Ht.mm..5, y=pred, group = H2OTmt, color = "green"))

# now add a column to biomass_nh2 that is the slope (increase in mm per days its been) since ht 4 (54 days between 4/21 and 6/14)
# use slope[4-5] and ht[5] to model [6,7,8] and [final biomass]
biomass_nh2 <- biomass_nh2 %>% 
  mutate(slope4_5 = (Ht.mm..5 - Ht.mm..4)/54)

summary(lm(Ht.mm..6 ~ Ht.mm..5, data = biomass_nh2)) # R2 = 0.5126
summary(lm(Ht.mm..7 ~ Ht.mm..5, data = biomass_nh2)) # R2 = 0.4555
summary(lm(Ht.mm..8 ~ Ht.mm..5, data = biomass_nh2)) # R2 = 0.4381
summary(lm((StemWet_g) ~ Ht.mm..5, data = biomass_nh2)) # R2 = 0.3913

fviz_pca_biplot(res.pca, habillage = df_pca_norm$H2OTmt, addEllipses = TRUE, repel = TRUE) + scale_color_manual(values=c("red", "blue")) + scale_fill_manual(values=c("red", "blue"))
# maybe: for droughted plants, higher root:shoot correlates with lower LWC, but no such relationship exists for watered plants

fviz_pca_biplot(res.pca, habillage = df_pca_norm$CO2Tmt, addEllipses = TRUE, repel = TRUE) + scale_color_manual(values=c("gray", "black")) + scale_fill_manual(values=c("gray", "black"))
# nothing terribly exciting going on here

fviz_pca_biplot(res.pcaV, habillage = df_pca_normV$H2OTmt, addEllipses = TRUE, repel = TRUE) + scale_color_manual(values=c("red", "blue")) + scale_fill_manual(values=c("red", "blue"))

fviz_pca_biplot(res.pcaV, habillage = df_pca_normV$CO2Tmt, addEllipses = TRUE, repel = TRUE) + scale_color_manual(values=c("gray", "black")) + scale_fill_manual(values=c("gray", "black"))
# nothing terribly exciting going on here

## do it again but add root features, LiCOR data, and d13C
# first, prep those three data types for a left join (get ID or Code variable --> Code w/o a or b format)
rootimage_pca <- rootimage[, c(4,6,7,8,9,11,18,19)]
rootimage_pca$Code <- if_else(nchar(rootimage_pca$Code) == 4,substr(rootimage_pca$Code,1,3),substr(rootimage_pca$Code,1,4))

df_all_pca <- df_all[, c(3,11,15,16)]
df_all_pca$ID <- if_else(nchar(df_all_pca$ID) == 4,substr(df_all_pca$ID,1,3),substr(df_all_pca$ID,1,4))
colnames(df_all_pca)[colnames(df_all_pca) == 'ID'] <- 'Code'
df_all_pca <-df_all_pca %>% 
  group_by(Code) %>% 
  summarise_all(mean) 

#pca_full <- left_join(df_pca, rootimage_pca, by = "Code")
#pca_full <- left_join(pca_full, df_all_pca, by = "Code")

## Covariance matrix
corr_matrix <- cor(df_pca_norm[,c(2:7)])
ggcorrplot(corr_matrix)

res.pca <- prcomp(df_pca_norm[,c(2:7)])
res.pcaL <- prcomp(df_pca_normL[,c(2:7)])
res.pcaV <- prcomp(df_pca_normV[,c(2:7)])

fviz_pca_biplot(res.pca_full, habillage = pca_full_norm$H2OTmt, addEllipses = TRUE, repel = TRUE) + scale_color_manual(values=c("red", "blue")) + scale_fill_manual(values=c("red", "blue"))

fviz_pca_biplot(res.pca_full, habillage = pca_full_norm$CO2Tmt, addEllipses = TRUE, repel = TRUE) + scale_color_manual(values=c("gray", "black")) + scale_fill_manual(values=c("gray", "black"))

fviz_pca_biplot(res.pca_fullV, habillage = pca_full_normV$H2OTmt, addEllipses = TRUE, repel = TRUE) + scale_color_manual(values=c("red", "blue")) + scale_fill_manual(values=c("red", "blue"))

fviz_pca_biplot(res.pca_fullV, habillage = pca_full_normV$CO2Tmt, addEllipses = TRUE, repel = TRUE) + scale_color_manual(values=c("gray", "black")) + scale_fill_manual(values=c("gray", "black"))

model_full <- model_full %>% 
  mutate(Plot = if_else(nchar(Code)==3,substr(Code,1,1),substr(Code,1,2)))
library(devtools)
install_github("wbonat/mcglm")
library(mcglm)
form1 <- totmass_g ~ CO2Tmt + H2OTmt
form2 <- interpol ~ CO2Tmt + H2OTmt

mlm1 <- glmer(cbind(rootshoot, lwc, Cond, interpol, WUE, d13C, totmass_g) ~ CO2Tmt*H2OTmt + (1|Plot), data = model_full)
summary(mlm1)  
library(car)
library(lme4)
Anova(mlm1)

mlm2 <- lm(cbind(rootshoot, lwc, Cond, interpol, WUE, d13C, totmass_g) ~ H2OTmt, data = model_full)
anova(mlm1, mlm2) # indicates the model with just H2O is just as good as the one with CO2

sum(biomass_nh$Code %in% rootimage_pca$Code) # 81
model_df <- left_join(biomass_nh, rootimage_pca, by = "Code") %>% 
  mutate(totmass_g = StemWet_g+LeafWet_g+rootmass_g) %>% 
  mutate(H2OTmt = substr(Tmt,2,2), CO2Tmt = substr(Tmt,1,1)) %>% 
  select(!(c(StemWet_g, LeafWet_g, LeafDry_g, SeedMass.g., Cond..7, Total.Root.Length.mm, Number.of.Branch.Points))) 

pairs(model_df[,c(5:11)])
summary(lm(cbind(rootmass_g, totmass_g, Branching.frequency.per.mm, Root.Length.Diameter.Range.2.mm) ~ CO2Tmt+H2OTmt, data = model_df))

summary(glmer(Root.Length.Diameter.Range.2.mm ~ CO2Tmt*H2OTmt + (1|Plot), data = model_df))

# can I predict final biomass from ht and dia at Inv..5 ?

# Fixed effects:
#                 Estimate Std. Error t value
# (Intercept)        5.841      2.065   2.828
# CO2TmtE            4.339      2.633   1.648
# H2OTmtW           13.450      2.530   5.317
# CO2TmtE:H2OTmtW   -5.806      3.402  -1.707
# this is really close! I think it could cross over (|t|>1.96) if I had more datapoints, which I will get

# # Create an explanatory variable that is the Year column of the data frame
# explanatory_variable <- as.data.frame(permanova_df[,1])

# Run PERMANOVA on that response variable, using Year as the explanatory variable
# PERMANOVA(dist(response_matrix), explanatory_variable)
# permanova_bin_counts = PERMANOVA(dist(mv_counts_reponse), mv_counts_explanatory_variable)

###############
# PCA: nice, but not necessary
## Install packages
#install.packages("corrr")
library('corrr')
#install.packages("ggcorrplot")
library(ggcorrplot)
#install.packages("FactoMineR")
library("FactoMineR")
#install.packages("factoextra")
library(factoextra)
library(dplyr)

# data from Ch3_explore
colSums(is.na(biomass_nfh))

df_pca <- biomass_nh %>% 
  #  filter(Spp == "V") %>% 
  #  filter(Tmt == "EW") %>% 
  select(Code, Tmt, StemWet_g, LeafDry_g, LeafWet_g, rootmass_g) %>% 
  mutate(rootshoot = rootmass_g/(StemWet_g + LeafWet_g), lwc = (LeafWet_g - LeafDry_g)/LeafWet_g) %>% 
  filter(!if_any(everything(),is.na)) 

df_pca_new <- df_pca[,-c(1)]
df_pca_new <- as.data.frame(df_pca_new)
rownames(df_pca_new) <- df_pca[,1]
#rownames(df_pca_new) <- c("AD", "AW", "ED", "EW")

#df_pca_norm <- scale(df_pca_new)
df_pca_norm <- as.data.frame(cbind(df_pca_new[,1],scale(df_pca_new[,-1])))
colnames(df_pca_norm)[colnames(df_pca_norm) == 'V1'] <- 'Tmt'
df_pca_norm <- df_pca_norm %>% 
  mutate(H2OTmt = substr(Tmt,2,2), CO2Tmt = substr(Tmt,1,1)) %>% 
  mutate(Spp = if_else(nchar(rownames(df_pca_norm))==3,substr(rownames(df_pca_norm),2,2),substr(rownames(df_pca_norm),3,3))) %>% 
  mutate_at(c("StemWet_g","LeafDry_g","LeafWet_g","rootmass_g","rootshoot","lwc"), as.numeric) 

# separate by spp
df_pca_normL <- df_pca_norm %>% 
  filter(Spp == "L")
df_pca_normV <- df_pca_norm %>% 
  filter(Spp == "V")

## Covariance matrix
corr_matrixL <- cor(df_pca_normL[,c(2:7)])
ggcorrplot(corr_matrixL)
corr_matrixV <- cor(df_pca_normV[,c(2:7)])
ggcorrplot(corr_matrixV)

res.pca <- prcomp(df_pca_norm[,c(2:7)])
res.pcaL <- prcomp(df_pca_normL[,c(2:7)])
res.pcaV <- prcomp(df_pca_normV[,c(2:7)])

fviz_pca_ind(res.pca, label = "none", habillage = df_pca_norm$Tmt)

##
filter_biplot_spp <- fviz_pca_biplot(res.pca, label = "var", habillage = df_pca_norm$Spp,  addEllipses = TRUE, repel = TRUE, title = "Biomass PCA (filtered) -- Spp") + scale_color_manual(values=c("pink", "lightblue", "red", "blue")) + scale_fill_manual(values=c("pink", "lightblue", "red", "blue"))
filter_biplot_tmt <- fviz_pca_biplot(res.pca, label = "var", habillage = df_pca_norm$Tmt,  addEllipses = TRUE, repel = TRUE, title = "Biomass PCA (filtered) -- Tmt") + scale_color_manual(values=c("pink", "lightblue", "red", "blue")) + scale_fill_manual(values=c("pink", "lightblue", "red", "blue"))
# across both spp, W and D treatments seem aligned with different PCs; aCO2 and eCO2 not as clear

# do it again but by spp; because separating out by Spp instead of Tmt led to two different ellipses
Vfilter_biplot_tmt <- fviz_pca_biplot(res.pcaV, label = "var", habillage = df_pca_normV$Tmt,  addEllipses = TRUE, repel = TRUE, title = "Biomass PCA (filtered) -- Valley Oak") + scale_color_manual(values=c("pink", "lightblue", "red", "blue")) + scale_fill_manual(values=c("pink", "lightblue", "red", "blue"))
Lfilter_biplot_tmt <- fviz_pca_biplot(res.pcaL, label = "var", habillage = df_pca_normL$Tmt,  addEllipses = TRUE, repel = TRUE, title = "Biomass PCA (filtered) -- Live Oak") + scale_color_manual(values=c("pink", "lightblue", "red", "blue")) + scale_fill_manual(values=c("pink", "lightblue", "red", "blue"))

# add root image data?

SIF_pca <- SIF[,1:2]
SIF_pca$Code <- if_else(nchar(SIF_pca$Code) == 4,substr(SIF_pca$Code,1,3),substr(SIF_pca$Code,1,4))

# then, left_join one at a time
pca_full <- left_join(df_pca, SIF_pca, by = "Code")

# filter any rows with NAs
pca_full <- filter(pca_full, !if_any(everything(),is.na)) 
# replace mass categories with total biomass; keep root:shoot and LWC
pca_full <- pca_full %>% 
  mutate(totmass_g = StemWet_g+LeafWet_g+rootmass_g) %>% 
  select(!(StemWet_g:rootmass_g))

# normalize and examine corr plot, Scree plot, biplot
pca_full_new <- pca_full[,-c(1)]
pca_full_new <- as.data.frame(pca_full_new)
rownames(pca_full_new) <- pca_full[,1]

pca_full_norm <- as.data.frame(cbind(pca_full_new[,1],scale(pca_full_new[,-1])))
colnames(pca_full_norm)[colnames(pca_full_norm) == 'V1'] <- 'Tmt'
pca_full_norm <- pca_full_norm %>% 
  mutate(H2OTmt = substr(Tmt,2,2), CO2Tmt = substr(Tmt,1,1)) %>% 
  mutate(Spp = if_else(nchar(rownames(pca_full_norm))==3,substr(rownames(pca_full_norm),2,2),substr(rownames(pca_full_norm),3,3))) %>% 
  mutate_at(.vars = c(2:5), as.numeric)

# separate by spp
pca_full_normL <- pca_full_norm %>% 
  filter(Spp == "L")
pca_full_normV <- pca_full_norm %>% 
  filter(Spp == "V")

## Covariance matrix
corr_matrix_full <- cor(pca_full_norm[,c(2:5)])
ggcorrplot(corr_matrix_full)

res.pca_full <- prcomp(pca_full_norm[,c(2:5)])
res.pca_fullL <- prcomp(pca_full_normL[,c(2:5)])
res.pca_fullV <- prcomp(pca_full_normV[,c(2:5)])

res.pca_full_thingy <- princomp(pca_full_norm[,c(2:5)])
res.pca_full_thingy$loadings[,1:2]

summary(res.pca_full)
res.pca_full$loadings[, 1:2]
fviz_eig(res.pca_full, addlabels = TRUE)
biplot(res.pca_full)

##
fviz_pca_biplot(res.pca_full, label = "var", habillage = pca_full_norm$Spp, addEllipses = TRUE, title = "Biomass + d13C PCA (filtered) -- Spp") + scale_color_manual(values=c("pink", "lightblue", "red", "blue")) + scale_fill_manual(values=c("pink", "lightblue", "red", "blue"))
fviz_pca_biplot(res.pca_full, label = "var", habillage = pca_full_norm$Tmt, addEllipses = TRUE, title = "Biomass + d13C PCA (filtered) -- Tmt") + scale_color_manual(values=c("pink", "lightblue", "red", "blue")) + scale_fill_manual(values=c("pink", "lightblue", "red", "blue"))

# do it again but by spp; because separating out by Spp instead of Tmt led to two different ellipses
fviz_pca_biplot(res.pca_fullV, label = "var", habillage = pca_full_normV$Tmt,  addEllipses = TRUE, repel = TRUE, title = "Biomass PCA (filtered) -- Valley Oak") + scale_color_manual(values=c("pink", "lightblue", "red", "blue")) + scale_fill_manual(values=c("pink", "lightblue", "red", "blue"))
fviz_pca_biplot(res.pca_fullL, label = "var", habillage = pca_full_normL$Tmt,  addEllipses = TRUE, repel = TRUE, title = "Biomass PCA (filtered) -- Live Oak") + scale_color_manual(values=c("pink", "lightblue", "red", "blue")) + scale_fill_manual(values=c("pink", "lightblue", "red", "blue"))


## now do the same with expanded data
df_pca.e <- biomass2 %>%
  select(Code, Tmt, StemWet_expanded, LeafWet_expanded, rootmass_g) %>%
  mutate(rootshoot = rootmass_g/(StemWet_expanded + LeafWet_expanded)) %>%
  filter(!if_any(everything(),is.na))

df_pca_new.e <- df_pca.e[,-c(1)]
df_pca_new.e <- as.data.frame(df_pca_new.e)
rownames(df_pca_new.e) <- df_pca.e[,1]

df_pca_norm.e <- as.data.frame(cbind(df_pca_new.e[,1],scale(df_pca_new.e[,-1])))
colnames(df_pca_norm.e)[colnames(df_pca_norm.e) == 'V1'] <- 'Tmt'
df_pca_norm.e <- df_pca_norm.e %>%
  mutate(H2OTmt = substr(Tmt,2,2), CO2Tmt = substr(Tmt,1,1)) %>%
  mutate(Spp = if_else(nchar(rownames(df_pca_norm.e))==3,substr(rownames(df_pca_norm.e),2,2),substr(rownames(df_pca_norm.e),3,3))) %>%
  mutate_at(c("StemWet_expanded","LeafWet_expanded","rootmass_g","rootshoot"), as.numeric)

# separate by spp
df_pca_normL.e <- df_pca_norm.e %>%
  filter(Spp == "L")
df_pca_normV.e <- df_pca_norm.e %>%
  filter(Spp == "V")

## Covariance matrix
corr_matrixL.e <- cor(df_pca_normL.e[,c(2:5)])
ggcorrplot(corr_matrixL.e)
corr_matrixV.e <- cor(df_pca_normV.e[,c(2:5)])
ggcorrplot(corr_matrixV.e)

res.pca.e <- prcomp(df_pca_norm.e[,c(2:5)])
res.pcaL.e <- prcomp(df_pca_normL.e[,c(2:5)])
res.pcaV.e <- prcomp(df_pca_normV.e[,c(2:5)])

fviz_pca_ind(res.pca.e, label = "none", habillage = df_pca_norm.e$Tmt)

#
expanded_biplot_spp <- fviz_pca_biplot(res.pca.e, label = "var", habillage = df_pca_norm.e$Spp,  addEllipses = TRUE, repel = TRUE, title = "Biomass PCA (expanded) -- Spp") + scale_color_manual(values=c("pink", "lightblue", "red", "blue")) + scale_fill_manual(values=c("pink", "lightblue", "red", "blue"))
expanded_biplot_tmt <- fviz_pca_biplot(res.pca.e, label = "var", habillage = df_pca_norm.e$Tmt,  addEllipses = TRUE, repel = TRUE, title = "Biomass PCA (expanded) -- Tmt") + scale_color_manual(values=c("pink", "lightblue", "red", "blue")) + scale_fill_manual(values=c("pink", "lightblue", "red", "blue"))
# across both spp, W and D treatments seem aligned with different PCs; aCO2 and eCO2 not as clear

# do it again but by spp; because separating out by Spp instead of Tmt led to two different ellipses
Vexpanded_biplot_tmt <- fviz_pca_biplot(res.pcaV.e, label = "var", habillage = df_pca_normV.e$Tmt,  addEllipses = TRUE, repel = TRUE, title = "Biomass PCA (expanded) -- Valley Oak") + scale_color_manual(values=c("pink", "lightblue", "red", "blue")) + scale_fill_manual(values=c("pink", "lightblue", "red", "blue"))
Lexpanded_biplot_tmt <- fviz_pca_biplot(res.pcaL.e, label = "var", habillage = df_pca_normL.e$Tmt,  addEllipses = TRUE, repel = TRUE, title = "Biomass PCA (expanded) -- Live Oak") + scale_color_manual(values=c("pink", "lightblue", "red", "blue")) + scale_fill_manual(values=c("pink", "lightblue", "red", "blue"))

# # add root image data?
# 
# 
# # then, left_join one at a time
pca_full.e <- left_join(df_pca.e, SIF_pca, by = "Code")

# filter any rows with NAs
pca_full.e <- filter(pca_full.e, !if_any(everything(),is.na)) %>% 
  filter(Code != "4V3")

# normalize and examine corr plot, Scree plot, biplot
pca_full_new.e <- pca_full.e[,-c(1)]
pca_full_new.e <- as.data.frame(pca_full_new.e)
rownames(pca_full_new.e) <- pca_full.e[,1]

pca_full_norm.e <- as.data.frame(cbind(pca_full_new.e[,1],scale(pca_full_new.e[,-1])))
colnames(pca_full_norm.e)[colnames(pca_full_norm.e) == 'V1'] <- 'Tmt'
pca_full_norm.e <- pca_full_norm.e %>%
  mutate(H2OTmt = substr(Tmt,2,2), CO2Tmt = substr(Tmt,1,1)) %>%
  mutate(Spp = if_else(nchar(rownames(pca_full_norm.e))==3,substr(rownames(pca_full_norm.e),2,2),substr(rownames(pca_full_norm.e),3,3))) %>%
  mutate_at(.vars = c(2:6), as.numeric)

# separate by spp
pca_full_normL.e <- pca_full_norm.e %>%
  filter(Spp == "L")
pca_full_normV.e <- pca_full_norm.e %>%
  filter(Spp == "V")

## Covariance matrix
corr_matrix_full.e <- cor(pca_full_norm.e[,c(2:6)])
ggcorrplot(corr_matrix_full.e)

res.pca_full.e <- prcomp(pca_full_norm.e[,c(2:6)])
res.pca_fullL.e <- prcomp(pca_full_normL.e[,c(2:6)])
res.pca_fullV.e <- prcomp(pca_full_normV.e[,c(2:6)])

res.pca_full_thingy.e <- princomp(pca_full_norm.e[,c(2:6)])
res.pca_full_thingy.e$loadings[,1:2]

summary(res.pca_full.e)
res.pca_full.e$loadings[, 1:2]
fviz_eig(res.pca_full.e, addlabels = TRUE)
biplot(res.pca_full.e)

##
fviz_pca_biplot(res.pca_full.e, label = "var", habillage = pca_full_norm.e$Spp, addEllipses = TRUE, title = "Biomass + d13C PCA (expanded) -- Spp") + scale_color_manual(values=c("pink", "lightblue", "red", "blue")) + scale_fill_manual(values=c("pink", "lightblue", "red", "blue"))
fviz_pca_biplot(res.pca_full.e, label = "var", habillage = pca_full_norm.e$Tmt, addEllipses = TRUE, title = "Biomass + d13C PCA (expanded) -- Tmt") + scale_color_manual(values=c("pink", "lightblue", "red", "blue")) + scale_fill_manual(values=c("pink", "lightblue", "red", "blue"))

# do it again but by spp; because separating out by Spp instead of Tmt led to two different ellipses
fviz_pca_biplot(res.pca_fullV.e, label = "var", habillage = pca_full_normV.e$Tmt,  addEllipses = TRUE, repel = TRUE, title = "Biomass PCA (expanded) -- Valley Oak") + scale_color_manual(values=c("pink", "lightblue", "red", "blue")) + scale_fill_manual(values=c("pink", "lightblue", "red", "blue"))
fviz_pca_biplot(res.pca_fullL.e, label = "var", habillage = pca_full_normL.e$Tmt,  addEllipses = TRUE, repel = TRUE, title = "Biomass PCA (expanded) -- Live Oak") + scale_color_manual(values=c("pink", "lightblue", "red", "blue")) + scale_fill_manual(values=c("pink", "lightblue", "red", "blue"))

###############

# univariate models before data transform

# LiCOR_df. : Photo.y, Cond.y, WUE.350
LiCOR_df.pmeans <- LiCOR_df. %>% 
  dplyr::select(Code, Plot, Spp, CO2Tmt, H2OTmt, CO2, SWC, Photo.y, Cond.y, WUE.350) %>% 
  group_by(Plot, CO2Tmt, H2OTmt, Spp) %>% 
  summarise(across(c(CO2, SWC, Photo.y, Cond.y, WUE.350), ~ mean(.x, na.rm=TRUE)))

summary(lm(Photo.y ~ CO2Tmt*H2OTmt, data = filter(LiCOR_df.pmeans, Spp == "V" | Spp == "L"))) # H2O p <.0003
summary(lm(Photo.y ~ CO2Tmt*H2OTmt, data = filter(LiCOR_df.pmeans, Spp == "L" | Spp == "L"))) # H2O p <.005 
summary(lm(Photo.y ~ CO2Tmt*H2OTmt, data = filter(LiCOR_df.pmeans, Spp == "V" | Spp == "V"))) # H2O p <.02
summary(lmer(Photo.y ~ CO2Tmt*H2OTmt + (1|Plot), data = filter(LiCOR_df., Spp == "V" | Spp == "L"))) # H2O p <.0004
summary(lmer(Photo.y ~ CO2Tmt*H2OTmt + (1|Plot), data = filter(LiCOR_df., Spp == "L" | Spp == "L"))) # H2O p <.008
summary(lmer(Photo.y ~ CO2Tmt*H2OTmt + (1|Plot), data = filter(LiCOR_df., Spp == "V" | Spp == "V"))) # H2O p <.015
summary(lm(Photo.y ~ rescale(CO2)*rescale(SWC), data = filter(LiCOR_df.pmeans, Spp == "L" | Spp == "V"))) # SWC p < .005
summary(lm(Photo.y ~ rescale(CO2)*rescale(SWC), data = filter(LiCOR_df.pmeans, Spp == "L" | Spp == "L"))) # SWC p < 0.03
summary(lm(Photo.y ~ rescale(CO2)*rescale(SWC), data = filter(LiCOR_df.pmeans, Spp == "V" | Spp == "V"))) # SWC p = 0.07
summary(lmer(Photo.y ~ rescale(CO2)*rescale(SWC) + (1|Plot), data = filter(LiCOR_df., Spp == "L" | Spp == "V"))) # SWC p<.0055
summary(lmer(Photo.y ~ rescale(CO2)*rescale(SWC) + (1|Plot), data = filter(LiCOR_df., Spp == "L" | Spp == "L"))) # SWC p<.05
summary(lmer(Photo.y ~ rescale(CO2)*rescale(SWC) + (1|Plot), data = filter(LiCOR_df., Spp == "V" | Spp == "V"))) # SWC p<.04
m1 <- lmer(Photo.y ~ CO2*H2OTmt + (1|Plot), data = filter(LiCOR_df., Spp == "V" | Spp == "V"))
m1 <- lm(Photo.y ~ rescale(CO2)*rescale(meanSWC), data = trans_dfwV)
m1 <- lm(sqrtTotArea ~ rescale(CO2)*rescale(meanSWC), data = trans_dfwL)
ggpredict(m1, terms=c("CO2","meanSWC [4,42]")) %>% 
  plot(add.data = TRUE, ci = T)

cooksd1 <- cooks.distance(m1)
sample_size <- nobs(m1)
plot(cooksd1, pch="*", cex=2, main="Influential Obs by Cooks distance")  # plot cook's distance
abline(h = 4/sample_size, col="red")  # add cutoff line
text(x=1:length(cooksd1)+1, y=cooksd1, labels=ifelse(cooksd1>4/sample_size, names(cooksd1),""), col="red")  # add labels

influential1 <- as.numeric(names(cooksd1)[(cooksd1 > (4/sample_size))])
ggpredict(lm(sqrtTotArea ~ rescale(CO2)*rescale(meanSWC) , data = trans_dfwL %>% 
                # filter(Spp == "L") %>% 
                 filter(!row_number() %in% influential1) ), terms=c("CO2","meanSWC [4,42]")) %>% plot(add.data = TRUE, ci = T)
summary(lm(sqrtTotArea ~ rescale(CO2)*rescale(meanSWC), data = trans_dfwL %>% 
             #  filter(Spp == "L") %>% 
               filter(!row_number() %in% influential1) ))
library(lmerTest)  

summary(lm(Cond.y ~ CO2Tmt*H2OTmt, data = filter(LiCOR_df.pmeans, Spp == "V" | Spp == "L"))) # H2O p <.0015
summary(lm(Cond.y ~ CO2Tmt*H2OTmt, data = filter(LiCOR_df.pmeans, Spp == "L" | Spp == "L"))) # H2O p <.003
summary(lm(Cond.y ~ CO2Tmt*H2OTmt, data = filter(LiCOR_df.pmeans, Spp == "V" | Spp == "V"))) # H2O p <.004
summary(lmer(Cond.y ~ CO2Tmt*H2OTmt + (1|Plot), data = filter(LiCOR_df., Spp == "V" | Spp == "L"))) # H2O p<.0006
summary(lmer(Cond.y ~ CO2Tmt*H2OTmt + (1|Plot), data = filter(LiCOR_df., Spp == "L" | Spp == "L"))) # H2O p<.002, inx=.19
summary(lmer(Cond.y ~ CO2Tmt*H2OTmt + (1|Plot), data = filter(LiCOR_df., Spp == "V" | Spp == "V"))) # H2O p<.004
summary(lm(Cond.y ~ rescale(CO2)*rescale(SWC), data = filter(LiCOR_df.pmeans, Spp == "L" | Spp == "V"))) # SWC p < .002
summary(lm(Cond.y ~ rescale(CO2)*rescale(SWC), data = filter(LiCOR_df.pmeans, Spp == "L" | Spp == "L"))) # SWC p < 0.004
summary(lm(Cond.y ~ rescale(CO2)*rescale(SWC), data = filter(LiCOR_df.pmeans, Spp == "V" | Spp == "V"))) # SWC p = 0.0008
summary(lmer(Cond.y ~ rescale(CO2)*rescale(SWC) + (1|Plot), data = filter(LiCOR_df., Spp == "L" | Spp == "V"))) # SWC p<<<.05
summary(lmer(Cond.y ~ rescale(CO2)*rescale(SWC) + (1|Plot), data = filter(LiCOR_df., Spp == "L" | Spp == "L"))) # SWC p<.005
summary(lmer(Cond.y ~ rescale(CO2)*rescale(SWC) + (1|Plot), data = filter(LiCOR_df., Spp == "V" | Spp == "V"))) # SWC p<.0007
m2 <- lmer(Cond.y ~ CO2*H2OTmt + (1|Plot), data = filter(LiCOR_df., Spp == "L" | Spp == "L"))
ggpredict(m2, terms=c("CO2","H2OTmt")) %>% 
  plot(add.data = TRUE, ci = FALSE)

cooksd2 <- cooks.distance(m2)
sample_size <- nobs(m2)
plot(cooksd2, pch="*", cex=2, main="Influential Obs by Cooks distance")  # plot cook's distance
abline(h = 4/sample_size, col="red")  # add cutoff line
text(x=1:length(cooksd2)+1, y=cooksd2, labels=ifelse(cooksd2>4/sample_size, names(cooksd2),""), col="red")  # add labels

influential2 <- as.numeric(names(cooksd2)[(cooksd2 > (4/sample_size))])
ggpredict(lmer(Cond.y ~ CO2*H2OTmt + (1|Plot), data = LiCOR_df. %>% 
                 filter(Spp == "L") %>% 
                 filter(!row_number() %in% influential2) ), terms=c("CO2","H2OTmt")) %>% plot(add.data = TRUE, ci = FALSE)
summary(lmer(Cond.y ~ CO2*H2OTmt + (1|Plot), data = LiCOR_df. %>% 
               filter(Spp == "L") %>% 
               filter(!row_number() %in% influential2) )) # nothing much going on

summary(lm(WUE.350 ~ CO2Tmt*H2OTmt, data = filter(LiCOR_df.pmeans, Spp == "V" | Spp == "L"))) 
summary(lm(WUE.350 ~ CO2Tmt*H2OTmt, data = filter(LiCOR_df.pmeans, Spp == "L" | Spp == "L"))) 
summary(lm(WUE.350 ~ CO2Tmt*H2OTmt, data = filter(LiCOR_df.pmeans, Spp == "V" | Spp == "V"))) 
summary(lmer(WUE.350 ~ CO2Tmt*H2OTmt + (1|Plot), data = filter(LiCOR_df., Spp == "V" | Spp == "L"))) 
summary(lmer(WUE.350 ~ CO2Tmt*H2OTmt + (1|Plot), data = filter(LiCOR_df., Spp == "L" | Spp == "L")))
summary(lmer(WUE.350 ~ CO2Tmt*H2OTmt + (1|Plot), data = filter(LiCOR_df., Spp == "V" | Spp == "V"))) 
summary(lm(WUE.350 ~ rescale(CO2)*rescale(SWC), data = filter(LiCOR_df.pmeans, Spp == "L" | Spp == "V"))) 
summary(lm(WUE.350 ~ rescale(CO2)*rescale(SWC), data = filter(LiCOR_df.pmeans, Spp == "L" | Spp == "L")))
summary(lm(WUE.350 ~ rescale(CO2)*rescale(SWC), data = filter(LiCOR_df.pmeans, Spp == "V" | Spp == "V"))) 
summary(lmer(WUE.350 ~ rescale(CO2)*rescale(SWC) + (1|Plot), data = filter(LiCOR_df., Spp == "L" | Spp == "V")))
summary(lmer(WUE.350 ~ rescale(CO2)*rescale(SWC) + (1|Plot), data = filter(LiCOR_df., Spp == "L" | Spp == "L")))
summary(lmer(WUE.350 ~ rescale(CO2)*rescale(SWC) + (1|Plot), data = filter(LiCOR_df., Spp == "V" | Spp == "V")))
m3 <- lmer(WUE.350 ~ CO2*H2OTmt + (1|Plot), data = filter(LiCOR_df., Spp == "V" | Spp == "V"))
ggpredict(m3, terms=c("CO2","H2OTmt")) %>% 
  plot(add.data = TRUE, ci = FALSE) # really nothing to see here

# biomass2. : totmass, rootshoot, LWC
biomass2.pmeans <- biomass2. %>% 
  group_by(Plot, CO2Tmt, H2OTmt, Spp) %>% 
  summarise(across(c(CO2, meanSWC, totmass, rootshoot, lwc), ~ mean(.x, na.rm=TRUE)))

summary(lm(totmass ~ CO2Tmt*H2OTmt, data = filter(biomass2.pmeans, Spp == "V" | Spp == "L"))) # H2O p <.03
summary(lm(totmass ~ CO2Tmt*H2OTmt, data = filter(biomass2.pmeans, Spp == "L" | Spp == "L"))) 
summary(lm(totmass ~ CO2Tmt*H2OTmt, data = filter(biomass2.pmeans, Spp == "V" | Spp == "V"))) # H2O p <.02
summary(lmer(totmass ~ CO2Tmt*H2OTmt + (1|Plot), data = filter(biomass2., Spp == "V" | Spp == "L")))
summary(lmer(totmass ~ CO2Tmt*H2OTmt + (1|Plot), data = filter(biomass2., Spp == "L" | Spp == "L"))) 
summary(lmer(totmass ~ CO2Tmt*H2OTmt + (1|Plot), data = filter(biomass2., Spp == "V" | Spp == "V"))) # H2O p<.01
summary(lm(totmass ~ rescale(CO2)*rescale(meanSWC), data = filter(biomass2.pmeans, Spp == "L" | Spp == "V"))) # SWC p < .032
summary(lm(totmass ~ rescale(CO2)*rescale(meanSWC), data = filter(biomass2.pmeans, Spp == "L" | Spp == "L"))) # SWC p < 0.1
summary(lm(totmass ~ rescale(CO2)*rescale(meanSWC), data = filter(biomass2.pmeans, Spp == "V" | Spp == "V"))) # SWC p = 0.08
summary(lmer(totmass ~ rescale(CO2)*rescale(meanSWC) + (1|Plot), data = filter(biomass2., Spp == "L" | Spp == "V"))) 
summary(lmer(totmass ~ rescale(CO2)*rescale(meanSWC) + (1|Plot), data = filter(biomass2., Spp == "L" | Spp == "L")))
summary(lmer(totmass ~ rescale(CO2)*rescale(meanSWC) + (1|Plot), data = filter(biomass2., Spp == "V" | Spp == "V"))) # SWC p<.07
m4 <- lmer(totmass ~ CO2*H2OTmt + (1|Plot), data = filter(biomass2., Spp == "V" | Spp == "V"))
ggpredict(m4, terms=c("CO2","H2OTmt")) %>% 
  plot(add.data = TRUE, ci = FALSE)

cooksd4 <- cooks.distance(m4)
sample_size <- nobs(m4)
plot(cooksd4, pch="*", cex=2, main="Influential Obs by Cooks distance")  # plot cook's distance
abline(h = 4/sample_size, col="red")  # add cutoff line
text(x=1:length(cooksd4)+1, y=cooksd4, labels=ifelse(cooksd4>4/sample_size, names(cooksd4),""), col="red")  # add labels

influential4 <- as.numeric(names(cooksd4)[(cooksd4 > (4/sample_size))])
ggpredict(lmer(totmass ~ CO2*H2OTmt + (1|Plot), data = biomass2. %>% 
                 filter(Spp == "V") %>% 
                 filter(!row_number() %in% influential4) ), terms=c("CO2","H2OTmt")) %>% plot(add.data = TRUE, ci = FALSE)
summary(lmer(totmass ~ CO2*H2OTmt + (1|Plot), data = biomass2. %>% 
               filter(Spp == "V") %>% 
               filter(!row_number() %in% influential4) )) # nothing doing

# rootshoot
summary(lm(rootshoot ~ CO2Tmt*H2OTmt, data = filter(biomass2.pmeans, Spp == "V" | Spp == "L"))) 
summary(lm(rootshoot ~ CO2Tmt*H2OTmt, data = filter(biomass2.pmeans, Spp == "L" | Spp == "L"))) # H2O p < 009
summary(lm(rootshoot ~ CO2Tmt*H2OTmt, data = filter(biomass2.pmeans, Spp == "V" | Spp == "V"))) 
summary(lmer(rootshoot ~ CO2Tmt*H2OTmt + (1|Plot), data = filter(biomass2., Spp == "V" | Spp == "L")))
summary(lmer(rootshoot ~ CO2Tmt*H2OTmt + (1|Plot), data = filter(biomass2., Spp == "L" | Spp == "L"))) # H2O p< .0021
summary(lmer(rootshoot ~ CO2Tmt*H2OTmt + (1|Plot), data = filter(biomass2., Spp == "V" | Spp == "V")))
summary(lm(rootshoot ~ rescale(CO2)*rescale(meanSWC), data = filter(biomass2.pmeans, Spp == "L" | Spp == "V"))) 
summary(lm(rootshoot ~ rescale(CO2)*rescale(meanSWC), data = filter(biomass2.pmeans, Spp == "L" | Spp == "L"))) # SWC p < 0.05
summary(lm(rootshoot ~ rescale(CO2)*rescale(meanSWC), data = filter(biomass2.pmeans, Spp == "V" | Spp == "V"))) 
summary(lmer(rootshoot ~ rescale(CO2)*rescale(meanSWC) + (1|Plot), data = filter(biomass2., Spp == "L" | Spp == "V"))) 
summary(lmer(rootshoot ~ rescale(CO2)*rescale(meanSWC) + (1|Plot), data = filter(biomass2., Spp == "L" | Spp == "L"))) # SWC p<.08
summary(lmer(rootshoot ~ rescale(CO2)*rescale(meanSWC) + (1|Plot), data = filter(biomass2., Spp == "V" | Spp == "V"))) # CO2 p<.1
m5 <- lmer(rootshoot ~ CO2*H2OTmt + (1|Plot), data = filter(biomass2., Spp == "V" | Spp == "V"))
ggpredict(m5, terms=c("CO2","H2OTmt")) %>% 
  plot(add.data = TRUE, ci = FALSE)

cooksd5 <- cooks.distance(m5)
sample_size <- nobs(m5)
plot(cooksd5, pch="*", cex=2, main="Influential Obs by Cooks distance")  # plot cook's distance
abline(h = 4/sample_size, col="red")  # add cutoff line
text(x=1:length(cooksd5)+1, y=cooksd5, labels=ifelse(cooksd5>4/sample_size, names(cooksd5),""), col="red")  # add labels


influential5 <- as.numeric(names(sort(cooksd5, decreasing = TRUE)[1:2]))
ggpredict(lmer(rootshoot ~ CO2*H2OTmt + (1|Plot), data = biomass2. %>% 
                 filter(Spp == "V") %>% 
                 filter(!row_number() %in% influential5) ), terms=c("CO2","H2OTmt")) %>% plot(add.data = TRUE, ci = FALSE)
summary(lmer(totmass ~ CO2*H2OTmt + (1|Plot), data = biomass2. %>% 
               filter(Spp == "V") %>% 
               filter(!row_number() %in% influential5) )) # nah

# lwc
summary(lm(lwc ~ CO2Tmt*H2OTmt, data = filter(biomass2.pmeans, Spp == "V" | Spp == "L"))) 
summary(lm(lwc ~ CO2Tmt*H2OTmt, data = filter(biomass2.pmeans, Spp == "L" | Spp == "L")))
summary(lm(lwc ~ CO2Tmt*H2OTmt, data = filter(biomass2.pmeans, Spp == "V" | Spp == "V"))) # H2O < .02, CO2 < .075
summary(lmer(lwc ~ CO2Tmt*H2OTmt + (1|Plot), data = filter(biomass2., Spp == "V" | Spp == "L")))
summary(lmer(lwc ~ CO2Tmt*H2OTmt + (1|Plot), data = filter(biomass2., Spp == "L" | Spp == "L"))) 
summary(lmer(lwc ~ CO2Tmt*H2OTmt + (1|Plot), data = filter(biomass2., Spp == "V" | Spp == "V"))) # H2O < 0.025, CO2 < 0.1
summary(lm(lwc ~ rescale(CO2)*rescale(meanSWC), data = filter(biomass2.pmeans, Spp == "L" | Spp == "V"))) 
summary(lm(lwc ~ rescale(CO2)*rescale(meanSWC), data = filter(biomass2.pmeans, Spp == "L" | Spp == "L"))) 
summary(lm(lwc ~ rescale(CO2)*rescale(meanSWC), data = filter(biomass2.pmeans, Spp == "V" | Spp == "V"))) # SWC < .075
summary(lmer(lwc ~ rescale(CO2)*rescale(meanSWC) + (1|Plot), data = filter(biomass2., Spp == "L" | Spp == "V"))) 
summary(lmer(lwc ~ rescale(CO2)*rescale(meanSWC) + (1|Plot), data = filter(biomass2., Spp == "L" | Spp == "L"))) 
summary(lmer(lwc ~ rescale(CO2)*rescale(meanSWC) + (1|Plot), data = filter(biomass2., Spp == "V" | Spp == "V"))) # SWC < .07
m6 <- lmer(lwc ~ CO2*H2OTmt + (1|Plot), data = filter(biomass2., Spp == "V" | Spp == "V"))
ggpredict(m6, terms=c("CO2","H2OTmt")) %>% 
  plot(add.data = TRUE, ci = FALSE)

cooksd6 <- cooks.distance(m6)
sample_size <- nobs(m6)
plot(cooksd6, pch="*", cex=2, main="Influential Obs by Cooks distance")  # plot cook's distance
abline(h = 4/sample_size, col="red")  # add cutoff line
text(x=1:length(cooksd6)+1, y=cooksd6, labels=ifelse(cooksd6>4/sample_size, names(cooksd6),""), col="red")  # add labels


influential6 <- as.numeric(names(cooksd6)[(cooksd6 > (4/sample_size))])
ggpredict(lmer(lwc ~ CO2*H2OTmt + (1|Plot), data = biomass2. %>% 
                 filter(Spp == "V") %>% 
                 filter(!row_number() %in% influential6) ), terms=c("CO2","H2OTmt")) %>% plot(add.data = TRUE, ci = FALSE)
summary(lmer(totmass ~ CO2*H2OTmt + (1|Plot), data = biomass2. %>% 
               filter(Spp == "V") %>% 
               filter(!row_number() %in% influential6) )) # nada

# rootimage. : ratio_2to1, dia_range_1 or w/e, # branchpoints
rootimage.pmeans <- rootimage. %>% 
  group_by(Plot, CO2Tmt, H2OTmt, Spp) %>% 
  summarise(across(c(CO2, meanSWC, ratio_2to1, Root.Length.Diameter.Range.1.mm, Number.of.Branch.Points), ~ mean(.x, na.rm=TRUE)))

summary(lm(ratio_2to1 ~ CO2Tmt*H2OTmt, data = filter(rootimage.pmeans, Spp == "V" | Spp == "L"))) # CO2 p <.02, intx<.06
summary(lm(ratio_2to1 ~ CO2Tmt*H2OTmt, data = filter(rootimage.pmeans, Spp == "L" | Spp == "L"))) # CO2 <.03
summary(lm(ratio_2to1 ~ CO2Tmt*H2OTmt, data = filter(rootimage.pmeans, Spp == "V" | Spp == "V"))) 
summary(lmer(ratio_2to1 ~ CO2Tmt*H2OTmt + (1|Plot), data = filter(rootimage., Spp == "V" | Spp == "L"))) # CO2 < .085
summary(lmer(ratio_2to1 ~ CO2Tmt*H2OTmt + (1|Plot), data = filter(rootimage., Spp == "L" | Spp == "L"))) # CO2 < .09
summary(lmer(ratio_2to1 ~ CO2Tmt*H2OTmt + (1|Plot), data = filter(rootimage., Spp == "V" | Spp == "V"))) 
summary(lm(ratio_2to1 ~ rescale(CO2)*rescale(meanSWC), data = filter(rootimage.pmeans, Spp == "L" | Spp == "V"))) # CO2 <.005, intx<.017
summary(lm(ratio_2to1 ~ rescale(CO2)*rescale(meanSWC), data = filter(rootimage.pmeans, Spp == "L" | Spp == "L"))) # CO2 <.015, intx<.045
summary(lm(ratio_2to1 ~ rescale(CO2)*rescale(meanSWC), data = filter(rootimage.pmeans, Spp == "V" | Spp == "V"))) 
summary(lmer(ratio_2to1 ~ rescale(CO2)*rescale(meanSWC) + (1|Plot), data = filter(rootimage., Spp == "L" | Spp == "V"))) # CO2 < .03, intx<.05
summary(lmer(ratio_2to1 ~ rescale(CO2)*rescale(meanSWC) + (1|Plot), data = filter(rootimage., Spp == "L" | Spp == "L"))) # CO2 < .035, intx<.1
summary(lmer(ratio_2to1 ~ rescale(CO2)*rescale(meanSWC) + (1|Plot), data = filter(rootimage., Spp == "V" | Spp == "V")))
m7 <- lmer(ratio_2to1 ~ CO2*H2OTmt + (1|Plot), data = filter(rootimage., Spp == "L" | Spp == "V"))
ggpredict(m7, terms=c("CO2","H2OTmt")) %>% 
  plot(add.data = TRUE, ci = FALSE)
m7L <- lmer(ratio_2to1 ~ CO2*H2OTmt + (1|Plot), data = filter(rootimage., Spp == "L" | Spp == "L"))
ggpredict(m7L, terms=c("CO2","H2OTmt")) %>% 
  plot(add.data = TRUE, ci = FALSE)
m7V <- lmer(ratio_2to1 ~ CO2*H2OTmt + (1|Plot), data = filter(rootimage., Spp == "V" | Spp == "V"))
ggpredict(m7V, terms=c("CO2","H2OTmt")) %>% 
  plot(add.data = TRUE, ci = FALSE)

cooksd7 <- cooks.distance(m7V)
sample_size <- nobs(m7V)
plot(cooksd7, pch="*", cex=2, main="Influential Obs by Cooks distance")  # plot cook's distance
abline(h = 4/sample_size, col="red")  # add cutoff line
text(x=1:length(cooksd7)+1, y=cooksd7, labels=ifelse(cooksd7>4/sample_size, names(cooksd7),""), col="red")  # add labels


influential7 <- as.numeric(names(cooksd7)[(cooksd7 > (4/sample_size))])
ggpredict(lmer(ratio_2to1 ~ CO2*H2OTmt + (1|Plot), data = rootimage. %>% 
                 filter(Spp == "V") %>% 
                 filter(!row_number() %in% influential7) ), terms=c("CO2","H2OTmt")) %>% plot(add.data = TRUE, ci = FALSE)
summary(lmer(ratio_2to1 ~ CO2*H2OTmt + (1|Plot), data = rootimage. %>% 
               filter(Spp == "V") %>% 
               filter(!row_number() %in% influential7) )) # something is maybe up with the coarse:fine roots

summary(lm(Root.Length.Diameter.Range.1.mm ~ CO2Tmt*H2OTmt, data = filter(rootimage.pmeans, Spp == "V" | Spp == "L"))) # H2O <.0024
summary(lm(Root.Length.Diameter.Range.1.mm ~ CO2Tmt*H2OTmt, data = filter(rootimage.pmeans, Spp == "L" | Spp == "L"))) # H2O<.025
summary(lm(Root.Length.Diameter.Range.1.mm ~ CO2Tmt*H2OTmt, data = filter(rootimage.pmeans, Spp == "V" | Spp == "V"))) # H2O<.025
summary(lmer(Root.Length.Diameter.Range.1.mm ~ CO2Tmt*H2OTmt + (1|Plot), data = filter(rootimage., Spp == "V" | Spp == "L"))) # H2O < .015
summary(lmer(Root.Length.Diameter.Range.1.mm ~ CO2Tmt*H2OTmt + (1|Plot), data = filter(rootimage., Spp == "L" | Spp == "L"))) # H2O < .035
summary(lmer(Root.Length.Diameter.Range.1.mm ~ CO2Tmt*H2OTmt + (1|Plot), data = filter(rootimage., Spp == "V" | Spp == "V"))) # H2O < 0.04
summary(lm(Root.Length.Diameter.Range.1.mm ~ rescale(CO2)*rescale(meanSWC), data = filter(rootimage.pmeans, Spp == "L" | Spp == "V"))) # H2O < .005
summary(lm(Root.Length.Diameter.Range.1.mm ~ rescale(CO2)*rescale(meanSWC), data = filter(rootimage.pmeans, Spp == "L" | Spp == "L"))) # H2O < .015
summary(lm(Root.Length.Diameter.Range.1.mm ~ rescale(CO2)*rescale(meanSWC), data = filter(rootimage.pmeans, Spp == "V" | Spp == "V"))) 
summary(lmer(Root.Length.Diameter.Range.1.mm ~ rescale(CO2)*rescale(meanSWC) + (1|Plot), data = filter(rootimage., Spp == "L" | Spp == "V"))) # H2O < .025
summary(lmer(Root.Length.Diameter.Range.1.mm ~ rescale(CO2)*rescale(meanSWC) + (1|Plot), data = filter(rootimage., Spp == "L" | Spp == "L"))) # H2O < .025
summary(lmer(Root.Length.Diameter.Range.1.mm ~ rescale(CO2)*rescale(meanSWC) + (1|Plot), data = filter(rootimage., Spp == "V" | Spp == "V")))
m8 <- lm(Root.Length.Diameter.Range.1.mm ~ rescale(CO2)*H2OTmt, data = filter(rootimage.pmeans, Spp == "L" | Spp == "V"))
ggpredict(m8, terms=c("CO2","H2OTmt")) %>% 
  plot(add.data = TRUE, ci = FALSE)

cooksd8 <- cooks.distance(m8)
sample_size <- nobs(m8)
plot(cooksd8, pch="*", cex=2, main="Influential Obs by Cooks distance")  # plot cook's distance
abline(h = 4/sample_size, col="red")  # add cutoff line
text(x=1:length(cooksd8)+1, y=cooksd8, labels=ifelse(cooksd8>4/sample_size, names(cooksd8),""), col="red")  # add labels


influential8 <- as.numeric(names(cooksd8)[(cooksd8 > (4/sample_size))])
ggpredict(lmer(Root.Length.Diameter.Range.1.mm ~ CO2*H2OTmt + (1|Plot), data = rootimage. %>% 
                 #  filter(Spp == "V") %>% 
                 filter(!row_number() %in% influential8) ), terms=c("CO2","H2OTmt")) %>% plot(add.data = TRUE, ci = FALSE)
summary(lmer(Root.Length.Diameter.Range.1.mm ~ CO2*H2OTmt + (1|Plot), data = rootimage. %>% 
               # filter(Spp == "V") %>% 
               filter(!row_number() %in% influential8) )) # wet plants have more fine root length

summary(lm(Number.of.Branch.Points ~ CO2Tmt*H2OTmt, data = filter(rootimage.pmeans, Spp == "V" | Spp == "L"))) # H2O <.0065
summary(lm(Number.of.Branch.Points ~ CO2Tmt*H2OTmt, data = filter(rootimage.pmeans, Spp == "L" | Spp == "L"))) # H2O<.05
summary(lm(Number.of.Branch.Points ~ CO2Tmt*H2OTmt, data = filter(rootimage.pmeans, Spp == "V" | Spp == "V"))) # H2O<.055
summary(lmer(Number.of.Branch.Points ~ CO2Tmt*H2OTmt + (1|Plot), data = filter(rootimage., Spp == "V" | Spp == "L"))) # H2O < .035
summary(lmer(Number.of.Branch.Points ~ CO2Tmt*H2OTmt + (1|Plot), data = filter(rootimage., Spp == "L" | Spp == "L"))) # H2O < .065
summary(lmer(Number.of.Branch.Points ~ CO2Tmt*H2OTmt + (1|Plot), data = filter(rootimage., Spp == "V" | Spp == "V"))) # H2O < .07
summary(lm(Number.of.Branch.Points ~ rescale(CO2)*rescale(meanSWC), data = filter(rootimage.pmeans, Spp == "L" | Spp == "V"))) # H2O < .035
summary(lm(Number.of.Branch.Points ~ rescale(CO2)*rescale(meanSWC), data = filter(rootimage.pmeans, Spp == "L" | Spp == "L"))) # H2O < .035
summary(lm(Number.of.Branch.Points ~ rescale(CO2)*rescale(meanSWC), data = filter(rootimage.pmeans, Spp == "V" | Spp == "V"))) 
summary(lmer(Number.of.Branch.Points ~ rescale(CO2)*rescale(meanSWC) + (1|Plot), data = filter(rootimage., Spp == "L" | Spp == "V"))) # H2O < .025
summary(lmer(Number.of.Branch.Points ~ rescale(CO2)*rescale(meanSWC) + (1|Plot), data = filter(rootimage., Spp == "L" | Spp == "L"))) # H2O < .045
summary(lmer(Number.of.Branch.Points ~ rescale(CO2)*rescale(meanSWC) + (1|Plot), data = filter(rootimage., Spp == "V" | Spp == "V")))
m9 <- lm(Number.of.Branch.Points ~ CO2*H2OTmt, data = filter(rootimage.pmeans, Spp == "V" | Spp == "L"))
ggpredict(m9, terms=c("CO2","H2OTmt")) %>% 
  plot(add.data = TRUE, ci = FALSE)

cooksd9 <- cooks.distance(m9)
sample_size <- nobs(m9)
plot(cooksd9, pch="*", cex=2, main="Influential Obs by Cooks distance")  # plot cook's distance
abline(h = 4/sample_size, col="red")  # add cutoff line
text(x=1:length(cooksd9)+1, y=cooksd9, labels=ifelse(cooksd9>4/sample_size, names(cooksd9),""), col="red")  # add labels


influential9 <- as.numeric(names(cooksd9)[(cooksd9 > (4/sample_size))])
ggpredict(lmer(Number.of.Branch.Points ~ CO2*H2OTmt + (1|Plot), data = rootimage. %>% 
                 #   filter(Spp == "V") %>% 
                 filter(!row_number() %in% influential9) ), terms=c("CO2","H2OTmt")) %>% plot(add.data = TRUE, ci = FALSE)
summary(lmer(Number.of.Branch.Points ~ CO2*H2OTmt + (1|Plot), data = rootimage.pmeans %>% 
               # filter(Spp == "V") %>% 
               filter(!row_number() %in% influential9) )) # wet plants have more branchy roots

# SIF. : d13C
SIF.pmeans <- SIF. %>% 
  group_by(Plot, CO2Tmt, H2OTmt, Spp) %>% 
  summarise(across(c(CO2, meanSWC, d13C), ~ mean(.x, na.rm=TRUE)))

summary(lm(d13C ~ CO2Tmt*H2OTmt, data = filter(SIF.pmeans, Spp == "V" | Spp == "L"))) # H2O <.03, CO2<.035
summary(lm(d13C ~ CO2Tmt*H2OTmt, data = filter(SIF.pmeans, Spp == "L" | Spp == "L"))) 
summary(lm(d13C ~ CO2Tmt*H2OTmt, data = filter(SIF.pmeans, Spp == "V" | Spp == "V"))) # H2O<.07, CO2<.025
summary(lmer(d13C ~ CO2Tmt*H2OTmt + (1|Plot), data = filter(SIF., Spp == "V" | Spp == "L"))) # H2O <.095, CO2<.075
summary(lmer(d13C ~ CO2Tmt*H2OTmt + (1|Plot), data = filter(SIF., Spp == "L" | Spp == "L"))) 
summary(lmer(d13C ~ CO2Tmt*H2OTmt + (1|Plot), data = filter(SIF., Spp == "V" | Spp == "V"))) # CO2 < .025, H2O<.07
summary(lm(d13C ~ rescale(CO2)*rescale(meanSWC), data = filter(SIF.pmeans, Spp == "L" | Spp == "V"))) # SWC < .075
summary(lm(d13C ~ rescale(CO2)*rescale(meanSWC), data = filter(SIF.pmeans, Spp == "L" | Spp == "L"))) 
summary(lm(d13C ~ rescale(CO2)*rescale(meanSWC), data = filter(SIF.pmeans, Spp == "V" | Spp == "V"))) # CO2 < .085
summary(lmer(d13C ~ rescale(CO2)*rescale(meanSWC) + (1|Plot), data = filter(SIF., Spp == "L" | Spp == "V"))) 
summary(lmer(d13C ~ rescale(CO2)*rescale(meanSWC) + (1|Plot), data = filter(SIF., Spp == "L" | Spp == "L"))) 
summary(lmer(d13C ~ rescale(CO2)*rescale(meanSWC) + (1|Plot), data = filter(SIF., Spp == "V" | Spp == "V"))) # CO2<.08
m10 <- lm(d13C ~ CO2*H2OTmt, data = filter(SIF.pmeans, Spp == "V" | Spp == "L"))
ggpredict(m10, terms=c("CO2","H2OTmt")) %>% 
  plot(add.data = TRUE, ci = FALSE)

cooksd10 <- cooks.distance(m10)
sample_size <- nobs(m10)
plot(cooksd10, pch="*", cex=2, main="Influential Obs by Cooks distance")  # plot cook's distance
abline(h = 4/sample_size, col="red")  # add cutoff line
text(x=1:length(cooksd10)+1, y=cooksd10, labels=ifelse(cooksd10>4/sample_size, names(cooksd10),""), col="red")  # add labels


influential10 <- as.numeric(names(cooksd10)[(cooksd10 > (4/sample_size))])
ggpredict(lmer(d13C ~ CO2*H2OTmt + (1|Plot), data = SIF.pmeans %>% 
                 #   filter(Spp == "V") %>% 
                 filter(!row_number() %in% influential10) ), terms=c("CO2","H2OTmt")) %>% plot(add.data = TRUE, ci = FALSE)
summary(lmer(d13C ~ CO2*H2OTmt + (1|Plot), data = SIF.pmeans %>% 
               # filter(Spp == "V") %>% 
               filter(!row_number() %in% influential10) )) # CO2 < .1; eCO2 less water stressed per d13C


# Pick PERMANOVA variables based on the above analysis
# Photo.y, totmass, rootshoot, lwc(?), ratio_2to1

# combine dataframes and rescale responses
permanova.df <- biomass2. %>% 
  dplyr::select(Plot, Spp, Code, totmass, rootshoot, lwc, CO2Tmt, H2OTmt, CO2, meanSWC) %>% 
  left_join(LiCOR_df.[,c("Code","Photo.y")], by = "Code") %>% 
  left_join(rootimage.[,c("Code","ratio_2to1")], by = "Code") %>% 
  filter(!is.na(Photo.y)) %>% 
  filter(!is.na(lwc)) %>% 
  mutate(across(where(is.numeric), ~ rescale(.x))) 
permanova.dfL <- permanova.df %>% 
  filter(Spp=="L")
permanova.dfV <- permanova.df %>% 
  filter(Spp=="V")

# do it with and without strata=df$Plot

response_matrix. <- permanova.df[,c(4:6,11,12)] 
response_matrix.L <- permanova.dfL[,c(4:6,11,12)] 
response_matrix.V <- permanova.dfV[,c(4:6,11,12)] 

adonis2(response_matrix. ~ H2OTmt*CO2Tmt, permanova.df) # H2OTmt p < .001
adonis2(response_matrix.L ~ H2OTmt*CO2Tmt, permanova.dfL) # H2OTmt p < .001
adonis2(response_matrix.V ~ H2OTmt*CO2Tmt, permanova.dfV) # H2OTmt p < .002

X = permanova_dfV[,c(3,8,9,10,12)]
X=PERMANOVA::IniTransform(X)
D = PERMANOVA::DistContinuous(X)
V_permanova = PERMANOVA::PERMANOVA(D,as.factor(permanova_dfV$Tmt), CoordPrinc=TRUE)
PERMANOVA::plot.PERMANOVA(V_permanova, ColorGroup = c("pink", "lightblue", "red", "blue"),voronoi = TRUE, ColorInd = c("pink", "lightblue", "red", "blue"), PlotInd=TRUE, LabelInd=TRUE, AddLegend = TRUE)

# with strata = df$Plot, nothing cool happens
adonis2(response_matrix. ~ H2OTmt*CO2Tmt, strata=permanova.df$Plot, permanova.df)
adonis2(response_matrix.L ~ H2OTmt*CO2Tmt, strata=permanova.dfL$Plot, permanova.dfL)
adonis2(response_matrix.V ~ H2OTmt*CO2Tmt, strata=permanova.dfV$Plot, permanova.dfV)

# last time I did photo, stem, leaf, root, d13C; try again with those
permanova.df2 <- biomass2. %>% 
  dplyr::select(Plot, Spp, Code, StemWet_expanded, LeafWet_expanded, rootmass_g, CO2Tmt, H2OTmt, CO2, meanSWC) %>% 
  left_join(LiCOR_df.[,c("Code","Photo.y")], by = "Code") %>% 
  left_join(SIF.[,c("Code","d13C")], by = "Code") %>% 
  filter(!is.na(Photo.y)) %>% 
  filter(!is.na(LeafWet_expanded)) %>% 
  mutate(across(where(is.numeric), ~ rescale(.x))) 
permanova.dfL2 <- permanova.df2 %>% 
  filter(Spp=="L")
permanova.dfV2 <- permanova.df2 %>% 
  filter(Spp=="V")

# do it with and without strata=df$Plot
response_matrix.2 <- permanova.df2[,c(4:6,10,11)] 
response_matrix.L2 <- permanova.dfL2[,c(4:6,10,11)] 
response_matrix.V2 <- permanova.dfV2[,c(4:6,10,11)] 

adonis2(response_matrix.2 ~ H2OTmt*CO2Tmt, permanova.df2) # H2O <.001
adonis2(response_matrix.L2 ~ H2OTmt*CO2Tmt, permanova.dfL2) # H2O <.001
adonis2(response_matrix.V2 ~ H2OTmt*CO2Tmt, permanova.dfV2) # H2O < .001, CO2 < .095, intx < .075
# when I fixed this to include d13C the interaction was no longer significant (p= 0.8). 
# how do I choose the right variables to include for PERMANOVA? all of them??

# last time I did photo, stem, leaf, root, d13C; try again with those
permanova.df2 <- biomass2. %>% 
  dplyr::select(Plot, Spp, Code, StemWet_expanded, LeafWet_expanded, rootmass_g, CO2Tmt, H2OTmt, CO2, meanSWC) %>% 
  left_join(LiCOR_df.[,c("Code","Photo.y")], by = "Code") %>% 
  left_join(SIF.[,c("Code","d13C")], by = "Code") %>% 
  filter(!is.na(Photo.y)) %>% 
  filter(!is.na(LeafWet_expanded)) %>% 
  mutate(across(where(is.numeric), ~ rescale(.x))) 
permanova.dfL2 <- permanova.df2 %>% 
  filter(Spp=="L")
permanova.dfV2 <- permanova.df2 %>% 
  filter(Spp=="V")

# do it with and without strata=df$Plot
response_matrix.2 <- permanova.df2[,c(4:6,10,11)] 
response_matrix.L2 <- permanova.dfL2[,c(4:6,10,11)] 
response_matrix.V2 <- permanova.dfV2[,c(4:6,10,11)] 

adonis2(response_matrix.2 ~ H2OTmt*CO2Tmt, permanova.df2) # H2O <.001
adonis2(response_matrix.L2 ~ H2OTmt*CO2Tmt, permanova.dfL2) # H2O <.001
adonis2(response_matrix.V2 ~ H2OTmt*CO2Tmt, permanova.dfV2) # H2O < .001, CO2 < .095, intx < .075
# when I fixed this to include d13C the interaction was no longer significant (p= 0.8). 
# how do I choose the right variables to include for PERMANOVA? all of them??


# PERMANOVA 
response_matrix. <- permanova.df[,c(7,8,14,15,20)]
response_matrix.L <- permanova.dfL[,c(7,8,14,15,20)]
# response_matrix.V <- permanova.dfV[,c(5,6,8,9, 14:15)] ###
# response_matrix.V <- permanova.dfV[,c(5,6,8,14:15)] ####
#response_matrix.V <- permanova.dfV[,c(5,6,8,14:15,20)] #####
response_matrix.V <- permanova.dfV[,c(7,8,14,15,20)]
# totmass, rootshoot, Photo.y, Cond.y, d13C: total mass, allocation, Anet, water behavior, water stress ?

# response_matrix. <- permanova.df[,c(18,5,6,19,20,16)] 
# response_matrix.L <- permanova.dfL[,c(18,5,6,19,20,16)] 
# response_matrix.V <- permanova.dfV[,c(18,5,6,19,20,16)] 
colnames(permanova.df[,c(5,6,8,14:15,20)])
# "StemWet_expanded" "rootmass_g"   "rootshoot"    "Photo.y"     "Cond.y"     "d13C"      

adonis2(response_matrix. ~ H2OTmt*CO2, permanova.df, na.rm=TRUE) # H2OTmt p < .001, CO2 <.035
adonis2(response_matrix.L ~ H2OTmt*CO2, permanova.dfL, na.rm=TRUE) # H2OTmt p < .001
adonis2(response_matrix.V ~ H2OTmt*CO2, permanova.dfV, na.rm=TRUE) # H2OTmt p < .001, CO2 <.02

adonis2(response_matrix. ~ H2OTmt*CO2Tmt, permanova.df, na.rm=TRUE) # H2OTmt p < .001, CO2 <.035
adonis2(response_matrix.L ~ H2OTmt*CO2Tmt, permanova.dfL, na.rm=TRUE) # H2OTmt p < .001
adonis2(response_matrix.V ~ H2OTmt*CO2Tmt, permanova.dfV, na.rm=TRUE) # H2OTmt p < .001, CO2 <.02

X = permanova.dfV[,c(7,8,14,15,20)] %>% filter(!is.na(d13C))
X=PERMANOVA::IniTransform(X)
D = PERMANOVA::DistContinuous(X)
V.permanova = PERMANOVA::PERMANOVA(D,as.factor(permanova.dfV[!is.na(permanova.dfV$d13C),]$Tmt), CoordPrinc=TRUE)
PERMANOVA::plot.PERMANOVA(V.permanova, ColorGroup = c("pink", "lightblue", "red", "blue"), VoronoiColor = c("pink", "lightblue", "red", "blue"))

# with strata = df$Plot, nothing cool happens
adonis2(response_matrix. ~ H2OTmt*CO2Tmt, strata=permanova.df$Plot, permanova.df, na.rm=TRUE)
adonis2(response_matrix.L ~ H2OTmt*CO2Tmt, strata=permanova.dfL$Plot, permanova.dfL, na.rm=TRUE)
adonis2(response_matrix.V ~ H2OTmt*CO2Tmt, strata=permanova.dfV$Plot, permanova.dfV, na.rm=TRUE)



# models with transformed data
# go again; if intx is not significant, drop it and run the main effects

# Photo.y: no transformation, no outliers
m1V <- lmer(Photo.y ~ CO2*H2OTmt + (1|Plot), data = filter(LiCOR_df., Spp == "V"))
m1L <- lmer(Photo.y ~ CO2*H2OTmt + (1|Plot), data = filter(LiCOR_df., Spp == "L"))

# Cond.y: log transformations
m2V <- lmer(log(Cond.y) ~ CO2*H2OTmt + (1|Plot), data = filter(LiCOR_df., Spp == "V"))
m2L <- lmer(log(Cond.y) ~ CO2*H2OTmt + (1|Plot), data = filter(LiCOR_df., Spp == "L"))

# WUE.350: filter <550, sqrt
m3V <- lmer(WUE.350tfm ~ CO2*H2OTmt + (1|Plot), data = filter(LiCOR_df.tfm, Spp == "V"))
m3L <- lmer(WUE.350tfm ~ CO2*H2OTmt + (1|Plot), data = filter(LiCOR_df.tfm, Spp == "L"))

# totmass: sqrt
m4V <- lmer(sqrt(totmass) ~ CO2*H2OTmt + (1|Plot), data = filter(biomass2., Spp == "V"))
m4L <- lmer(sqrt(totmass) ~ CO2*H2OTmt + (1|Plot), data = filter(biomass2., Spp == "L"))

# rootshoot: filter >9
m5V <- lmer(rootshoot ~ CO2*H2OTmt + (1|Plot), data = filter(biomass2., Spp == "V" & rootshoot<9))
m5L <- lmer(rootshoot ~ CO2*H2OTmt + (1|Plot), data = filter(biomass2., Spp == "L" & rootshoot<9))

# lwc: ok
m6V <- lmer(lwc ~ CO2*H2OTmt + (1|Plot), data = filter(biomass2., Spp == "V")) 
m6L <- lmer(lwc ~ CO2*H2OTmt + (1|Plot), data = filter(biomass2., Spp == "L"))

# ratio_2to1: filter>0.01, log
m7V <- lmer(log(ratio_2to1) ~ CO2*H2OTmt + (1|Plot), data = filter(rootimage., Spp == "V" & ratio_2to1>0.01))
m7L <- lmer(log(ratio_2to1) ~ CO2*H2OTmt + (1|Plot), data = filter(rootimage., Spp == "L" & ratio_2to1>0.01))

## redo these with random effect!
# Root range 1: log
m8V <- lmer(log(Root.Length.Diameter.Range.1.mm) ~ CO2*H2OTmt + (1|Plot), data = filter(rootimage., Spp == "V"))
m8L <- lmer(log(Root.Length.Diameter.Range.1.mm) ~ CO2*H2OTmt + (1|Plot), data = filter(rootimage., Spp == "L"))

# # branch points: filter >5, log
m9V <- lmer(log(Number.of.Branch.Points) ~ CO2*H2OTmt + (1|Plot), data = filter(rootimage., Spp == "V" & Number.of.Branch.Points > 5))
m9L <- lmer(log(Number.of.Branch.Points) ~ CO2*H2OTmt + (1|Plot), data = filter(rootimage., Spp == "L" & Number.of.Branch.Points > 5))

# SIF: ok
m10V <- lmer(d13C ~ CO2*H2OTmt + (1|Plot), data = filter(SIF., Spp == "V"))
m10L <- lmer(d13C ~ CO2*H2OTmt + (1|Plot), data = filter(SIF., Spp == "L"))

ggpredict(model, terms=c("continuous","categorical")) %>% 
  plot(add.data = TRUE, ci = TRUE)


summary(manova(cbind(unlist(LiCOR_df.tfm_pmeans$Photo.y),unlist(LiCOR_df.tfm_pmeans$Cond.y),unlist(LiCOR_df.tfm_pmeans$WUE.350tfm))~CO2Tmt*H2OTmt, LiCOR_df.tfm_pmeans))

# I heard a MANOVA has more power, so long as your data is normally distributed, which I've already ensured

manova.df <- biomass2. %>% 
  dplyr::select(Plot, Spp, Code, LeafWet_expanded, StemWet_expanded, rootmass_g, totmass, rootshoot, lwc, CO2Tmt, H2OTmt, CO2, meanSWC) %>% 
  left_join(LiCOR_df.[,c("Code","Photo.y","Cond.y", "WUE.350")], by = "Code") %>% 
  left_join(rootimage.[,c("Code","SRL", "Root.Length.Diameter.Range.1.mm","Number.of.Branch.Points")], by = "Code") %>% 
  left_join(SIF.[,c("Code","d13C")]) %>% 
  group_by(Plot, Spp) %>% 
  summarise(across(where(is.numeric), ~ mean(.x, na.rm=TRUE))) %>% 
  ungroup() %>% 
  mutate(logCond.y = log(Cond.y), sqrt_WUE.350 = sqrt(WUE.350), sqrt_totmass = sqrt(totmass), logSRL = log(SRL), logFineRoot = log(Root.Length.Diameter.Range.1.mm), logBranch=log(Number.of.Branch.Points)) %>% 
  left_join(lookup, by = "Plot") %>% 
  mutate(H2OTmt = substr(Tmt,2,2), CO2Tmt = substr(Tmt,1,1)) 
#write.csv(manova.df, "QuailFACE_plotmeans.csv")

manova.dfL <- manova.df %>% 
  filter(Spp=="L") %>% 
  dplyr::select(Photo.y, logCond.y, sqrt_WUE.350, sqrt_totmass, rootshoot, lwc, logSRL, logFineRoot) %>% 
  mutate_at(vars("Photo.y", "logCond.y", "sqrt_WUE.350", "sqrt_totmass", "rootshoot", "lwc", "logSRL", "logFineRoot"), list(unlist)) 

manova.dfV <- manova.df %>% 
  filter(Spp=="V") %>% 
  dplyr::select(Photo.y, logCond.y, sqrt_WUE.350, sqrt_totmass, rootshoot, lwc, logSRL, logFineRoot) %>% 
  mutate_at(vars("Photo.y", "logCond.y", "sqrt_WUE.350", "sqrt_totmass", "rootshoot", "lwc", "logSRL", "logFineRoot"), list(unlist)) 

manova.cbind <- cbind(unlist(manova.df$Photo.y),unlist(manova.df$logCond.y),unlist(manova.df$sqrt_WUE.350),unlist(manova.df$sqrt_totmass),unlist(manova.df$rootshoot),unlist(manova.df$lwc),unlist(manova.df$logSRL),unlist(manova.df$logFineRoot))
manova.cbindL <- cbind(unlist(manova.dfL$Photo.y),unlist(manova.dfL$logCond.y),unlist(manova.dfL$sqrt_WUE.350),unlist(manova.dfL$sqrt_totmass),unlist(manova.dfL$rootshoot),unlist(manova.dfL$lwc),unlist(manova.dfL$logSRL),unlist(manova.dfL$logFineRoot))
manova.cbindV <- cbind(unlist(manova.dfV$Photo.y),unlist(manova.dfV$logCond.y),unlist(manova.dfV$sqrt_WUE.350),unlist(manova.dfV$sqrt_totmass),unlist(manova.dfV$rootshoot),unlist(manova.dfV$lwc),unlist(manova.dfV$logSRL),unlist(manova.dfV$logFineRoot))

# reasonable subset: Photo.y, logCond.y, sqrt_totmass, rootshoot
summary(manova(manova.cbind[,c(1,2,4,5)] ~ CO2Tmt*H2OTmt, manova.df))
summary(manova(manova.cbindL[,c(1,2,4,5)] ~ CO2Tmt*H2OTmt, filter(manova.df, Spp=="L")))
summary(manova(manova.cbindV[,c(1,2,4,5)] ~ CO2Tmt*H2OTmt, filter(manova.df, Spp=="V")))

# ummm why is it not like this anymore
#  summary(manova(manova.cbind[,c(1,2,4,5)] ~ CO2Tmt*H2OTmt, manova.df))
#                 Df  Pillai approx F num Df den Df    Pr(>F)    
#   CO2Tmt         1 0.36876    3.067      4     21  0.038881 *  
#   H2OTmt         1 0.91197   54.391      4     21 8.765e-11 ***
#   CO2Tmt:H2OTmt  1 0.48919    5.028      4     21  0.005304 ** 
#   Residuals     24         

# kitchen sink oh wait this did not work bc too many variables
summary(manova(manova.cbind[,c(1,2,5,7)] ~ CO2Tmt*H2OTmt, manova.df))
summary(manova(manova.cbindL[,c(1,2,5,7)] ~ CO2Tmt*H2OTmt, filter(manova.df, Spp=="L")))
summary(manova(manova.cbindV[,c(1,2,5,7)] ~ CO2Tmt*H2OTmt, filter(manova.df, Spp=="V")))

# this time ignore species (meh)
# this time rescale
manova.df3 <- biomass2. %>% 
  dplyr::select(Plot, Spp, Code, LeafWet_expanded, StemWet_expanded, rootmass_g, totmass, rootshoot, lwc, CO2Tmt, H2OTmt, CO2, meanSWC) %>% 
  left_join(LiCOR_df.[,c("Code","Photo.y","Cond.y", "WUE.350")], by = "Code") %>% 
  left_join(rootimage.[,c("Code","ratio_2to1", "Root.Length.Diameter.Range.1.mm","Number.of.Branch.Points")], by = "Code") %>% 
  left_join(SIF.[,c("Code","d13C")]) %>% 
  group_by(Plot) %>% 
  summarise(across(where(is.numeric), ~ mean(.x, na.rm=TRUE))) %>% 
  ungroup() %>% 
  mutate(logCond.y = log(Cond.y), sqrt_WUE.350 = sqrt(WUE.350), sqrt_totmass = sqrt(totmass), logRootRatio = log(ratio_2to1), logFineRoot = log(Root.Length.Diameter.Range.1.mm), logBranch=log(Number.of.Branch.Points)) %>% 
  mutate(across(where(is.numeric), ~ rescale(.x))) %>% 
  left_join(lookup, by = "Plot") %>% 
  mutate(H2OTmt = substr(Tmt,2,2), CO2Tmt = substr(Tmt,1,1)) 

manova.cbind3 <- cbind(unlist(manova.df3$Photo.y),unlist(manova.df3$logCond.y),unlist(manova.df3$sqrt_WUE.350),unlist(manova.df3$sqrt_totmass),unlist(manova.df3$rootshoot),unlist(manova.df3$lwc),unlist(manova.df3$logRootRatio),unlist(manova.df3$logFineRoot))

# reasonable subset: Photo.y, logCond.y, sqrt_totmass, rootshoot
summary(manova(manova.cbind3[,c(1,2,4,5)] ~ CO2Tmt*H2OTmt, manova.df3))

# ummm why is it not like this anymore
#  summary(manova(manova.cbind[,c(1,2,4,5)] ~ CO2Tmt*H2OTmt, manova.df))
#                 Df  Pillai approx F num Df den Df    Pr(>F)    
#   CO2Tmt         1 0.36876    3.067      4     21  0.038881 *  
#   H2OTmt         1 0.91197   54.391      4     21 8.765e-11 ***
#   CO2Tmt:H2OTmt  1 0.48919    5.028      4     21  0.005304 ** 
#   Residuals     24         

# kitchen sink oh wait this did not work bc too many variables
summary(manova(manova.cbind[,c(1,2,5,7)] ~ CO2Tmt*H2OTmt, manova.df))

## this time try it without taking plot means

manova.df2 <- biomass2. %>% 
  dplyr::select(Plot, Spp, Code, LeafWet_expanded, StemWet_expanded, rootmass_g, totmass, rootshoot, lwc, CO2Tmt, H2OTmt, CO2, meanSWC) %>% 
  left_join(LiCOR_df.[,c("Code","Photo.y","Cond.y", "WUE.350")], by = "Code") %>% 
  left_join(rootimage.[,c("Code","SRL", "Root.Length.Diameter.Range.1.mm","Number.of.Branch.Points")], by = "Code") %>% 
  left_join(SIF.[,c("Code","d13C")]) %>% 
  mutate(logCond.y = log(Cond.y), sqrt_WUE.350 = sqrt(WUE.350), sqrt_totmass = sqrt(totmass), logSRL = log(SRL), logFineRoot = log(Root.Length.Diameter.Range.1.mm), logBranch=log(Number.of.Branch.Points)) %>% 
  left_join(lookup, by = "Plot") %>% 
  mutate(H2OTmt = substr(Tmt,2,2), CO2Tmt = substr(Tmt,1,1)) 
write.csv(manova.df2, "QuailFACE_all.csv")

manova.df2L <- manova.df2 %>% 
  filter(Spp=="L") %>% 
  dplyr::select(Photo.y, logCond.y, sqrt_WUE.350, sqrt_totmass, rootshoot, lwc, logRootRatio, logFineRoot) %>% 
  mutate_at(vars("Photo.y", "logCond.y", "sqrt_WUE.350", "sqrt_totmass", "rootshoot", "lwc", "logRootRatio", "logFineRoot"), list(unlist)) 

manova.df2V <- manova.df2 %>% 
  filter(Spp=="V") %>% 
  dplyr::select(Photo.y, logCond.y, sqrt_WUE.350, sqrt_totmass, rootshoot, lwc, logRootRatio, logFineRoot) %>% 
  mutate_at(vars("Photo.y", "logCond.y", "sqrt_WUE.350", "sqrt_totmass", "rootshoot", "lwc", "logRootRatio", "logFineRoot"), list(unlist)) 

manova.cbind <- cbind(unlist(manova.df2$Photo.y),unlist(manova.df2$logCond.y),unlist(manova.df2$sqrt_WUE.350),unlist(manova.df2$sqrt_totmass),unlist(manova.df2$rootshoot),unlist(manova.df2$lwc),unlist(manova.df2$logRootRatio),unlist(manova.df2$logFineRoot))
manova.cbind2L <- cbind(unlist(manova.df2L$Photo.y),unlist(manova.df2L$logCond.y),unlist(manova.df2L$sqrt_WUE.350),unlist(manova.df2L$sqrt_totmass),unlist(manova.df2L$rootshoot),unlist(manova.df2L$lwc),unlist(manova.df2L$logRootRatio),unlist(manova.df2L$logFineRoot))
manova.cbind2V <- cbind(unlist(manova.df2V$Photo.y),unlist(manova.df2V$logCond.y),unlist(manova.df2V$sqrt_WUE.350),unlist(manova.df2V$sqrt_totmass),unlist(manova.df2V$rootshoot),unlist(manova.df2V$lwc),unlist(manova.df2V$logRootRatio),unlist(manova.df2V$logFineRoot))

# reasonable subset: Photo.y, logCond.y, sqrt_totmass, rootshoot
summary(manova(manova.cbind[,c(1,2,4,5)] ~ CO2Tmt*H2OTmt, manova.df2))
summary(manova(manova.cbind2L[,c(1,2,4,5)] ~ CO2Tmt*H2OTmt, filter(manova.df2, Spp=="L")))
summary(manova(manova.cbind2V[,c(1,2,4,5)] ~ CO2Tmt*H2OTmt, filter(manova.df2, Spp=="V")))

summary(manova(manova.cbind[,c(1,2,5,7)] ~ CO2Tmt*H2OTmt, manova.df2))
summary(manova(manova.cbind2L[,c(1,2,5,7)] ~ CO2Tmt*H2OTmt, filter(manova.df2, Spp=="L")))
summary(manova(manova.cbind2V[,c(1,2,5,7)] ~ CO2Tmt*H2OTmt, filter(manova.df2, Spp=="V")))


## prepping dataframe for manova on transformed plot means
manova.df <- biomass2. %>% 
  dplyr::select(Plot, Spp, Code, LeafWet_expanded, StemWet_expanded, rootmass_g, totmass, rootshoot, lwc, CO2Tmt, H2OTmt, CO2, meanSWC) %>% 
  left_join(LiCOR_df.[,c("Code","Photo.y","Cond.y", "WUE.350")], by = "Code") %>% 
  left_join(rootimage.[,c("Code","SRL", "Root.Length.Diameter.Range.1.mm","Number.of.Branch.Points")], by = "Code") %>% 
  left_join(SIF.[,c("Code","d13C")]) %>% 
  group_by(Plot, Spp) %>% 
  summarise(across(where(is.numeric), ~ mean(.x, na.rm=TRUE))) %>% 
  ungroup() %>% 
  mutate(logCond.y = log(Cond.y), sqrt_WUE.350 = sqrt(WUE.350), sqrt_totmass = sqrt(totmass), logSRL = log(SRL), logFineRoot = log(Root.Length.Diameter.Range.1.mm), logBranch=log(Number.of.Branch.Points)) %>% 
  left_join(lookup, by = "Plot") %>% 
  mutate(H2OTmt = substr(Tmt,2,2), CO2Tmt = substr(Tmt,1,1)) 
#write.csv(manova.df, "QuailFACE_plotmeans.csv")

# trying to get plot mean * 1/se
manova.df <- biomass2. %>% 
  dplyr::select(Plot, Spp, Code, LeafWet_expanded, StemWet_expanded, rootmass_g, totmass, rootshoot, lwc, CO2, meanSWC) %>% 
  left_join(LiCOR_df.[,c("Code","Photo.y","Cond.y", "WUE.350", "CO2Tmt", "H2OTmt")], by = "Code") %>% 
  left_join(rootimage.[,c("Code","SRL", "Root.Length.Diameter.Range.1.mm","Number.of.Branch.Points")], by = "Code") %>% 
  left_join(SIF.[,c("Code","d13C")]) %>% 
  group_by(Plot, Spp) %>% 
  mutate(n = length(Code)) %>% 
  summarise_if(is.numeric, list(~ sd(., na.rm = TRUE), ~ mean(., na.rm = TRUE))) %>% 
  summarise(across(where(is.numeric), ~ mean(.x, na.rm=TRUE))) %>% View()
ungroup() %>%
  mutate(logCond.y = log(Cond.y), sqrt_WUE.350 = sqrt(WUE.350), sqrt_totmass = sqrt(totmass), logSRL = log(SRL), logFineRoot = log(Root.Length.Diameter.Range.1.mm), logBranch=log(Number.of.Branch.Points)) %>% 
  left_join(lookup, by = "Plot") %>% 
  mutate(H2OTmt = substr(Tmt,2,2), CO2Tmt = substr(Tmt,1,1)) 

mean_over_se <- function(x){
  mean(x, na.rm = TRUE)*sqrt(sum(!is.na(x)))/sd(x, na.rm = TRUE)
}

manova.df <- biomass2. %>% 
  dplyr::select(Plot, Spp, Code, LeafWet_expanded, StemWet_expanded, rootmass_g, totmass, rootshoot, lwc, CO2, meanSWC) %>% 
  left_join(LiCOR_df.[,c("Code","Photo.y","Cond.y", "WUE.350", "CO2Tmt", "H2OTmt")], by = "Code") %>% 
  left_join(rootimage.[,c("Code","SRL", "Root.Length.Diameter.Range.1.mm","Number.of.Branch.Points")], by = "Code") %>% 
  left_join(SIF.[,c("Code","d13C")]) %>% 
  group_by(Plot, Spp) %>% 
  mutate(n = length(Code)) %>% 
  summarise_if(is.numeric, mean_over_se) %>% View()
ungroup() %>%
  mutate(logCond.y = log(Cond.y), sqrt_WUE.350 = sqrt(WUE.350), sqrt_totmass = sqrt(totmass), logSRL = log(SRL), logFineRoot = log(Root.Length.Diameter.Range.1.mm), logBranch=log(Number.of.Branch.Points)) %>% 
  left_join(lookup, by = "Plot") %>% 
  mutate(H2OTmt = substr(Tmt,2,2), CO2Tmt = substr(Tmt,1,1)) 

library(sjstats)
v <- sample(1:4, 20, TRUE)
table(v)
w <- abs(rnorm(20))
table(weight(v, w))
table(weight2(v, w))

set.seed(1)
x <- sample(letters[1:5], size = 20, replace = TRUE)
w <- runif(n = 20)

table(x)
table(weight(x, w))

wt <- c(5,  5,  4,  1)/15
x <- c(3.7,3.3,3.5,2.8)
xm <- weighted.mean(x, wt)

### Keeping first attempts at modeling, permanova:
## add in modeling stuff here

# photosynthesis model with *cleaned* data
# data from Ch3_explore
LiCOR_df <- LiCOR_df %>% 
  mutate(CO2Tmt = substring(Tmt,1,1), H2OTmt = substring(Tmt,2,2))
summary(lmer(Photo.y ~ rescale(CO2)*rescale(SWC) + (1|Plot), data = filter(LiCOR_df.intx, Spp == "V" | Spp == "V")))
print(lmer(Photo.y ~ CO2Tmt*SWC + (1|Plot), data = filter(LiCOR_df, Spp == "V" | Spp == "V")))

# biomass model
biomass_nh <- biomass_nh %>% 
  mutate(CO2Tmt = substring(Tmt,1,1), H2OTmt = substring(Tmt,2,2))
summary(glmer((rootmass_g+StemWet_g+LeafWet_g) ~ H2OTmt*CO2Tmt + (1|Plot), data = biomass_nh))
summary(glmer((rootmass_g+StemWet_g+LeafWet_g) ~ H2OTmt*CO2Tmt + (1|Plot), data = biomass_nh))

biomass <- biomass %>% 
  mutate(CO2Tmt = substring(Tmt,1,1), H2OTmt = substring(Tmt,2,2))
summary(glmer(rootmass_g ~ H2OTmt*CO2Tmt + (1|Plot), data = filter(biomass,  Spp == "L" | Spp == "V")))

## EXPANDED biomass model
# biomass2 <- biomass2 %>%
#   mutate(CO2Tmt = substring(Tmt,1,1), H2OTmt = substring(Tmt,2,2)) %>% 

# total biomass, expanded data
summary(glmer(totmass ~ rescale(meanSWC)*rescale(CO2) + (1|Plot), data = filter(biomass2_intx, Spp == "V" | Spp == "V")))
# root:shoot, expanded data
summary(glmer(rootshoot ~ H2OTmt*CO2Tmt + (1|Plot), data = filter(biomass2_intx, Spp == "V" | Spp == "V")))
summary(lmer(rootshoot ~ rescale(meanSWC)*rescale(CO2) + (1|Plot), data = filter(biomass2_intx, Spp == "V" | Spp == "V")))

# root mass 
summary(glmer(rootmass_g ~ H2OTmt*CO2Tmt + (1|Plot), data = filter(biomass,  Spp == "V" | Spp == "V")))

# SIF model
SIF <- SIF %>% 
  mutate(CO2Tmt = substring(Tmt,1,1), H2OTmt = substring(Tmt,2,2)) 

summary(glmer(d13C ~ H2OTmt*CO2Tmt + (1|Plot), data = SIF))
summary(glmer(d13C ~ meanSWC*CO2 + (1|Plot), data = SIF_intx))
# Fixed effects:
#                 Estimate Std. Error t value
# (Intercept)     -27.9708     0.3428 -81.601
# H2OTmtW          -0.8829     0.4865  -1.815
# CO2TmtE          -0.9483     0.4817  -1.968
# H2OTmtW:CO2TmtE  -0.2761     0.6823  -0.405



# which data types do I have the most data for? *unique* seedlings
nrow(biomass) # 127
nrow(LiCOR_df) # 75
nrow(SIF) # 92
nrow(rootimage) # 128

# how many overlap from biomass_nh and SIF?
sum(biomass_nh$Code %in% SIF_pca$Code) # 69

# last but not least a PERMANOVA
df_pca.e <- biomass2 %>%
  dplyr::select(Code, Tmt, StemWet_expanded, LeafWet_expanded, rootmass_g) %>%
  mutate(rootshoot = rootmass_g/(StemWet_expanded + LeafWet_expanded)) %>% 
  filter(!if_any(everything(),is.na))

pca_full.e <- left_join(df_pca.e, SIF_pca, by = "Code") 

head(pca_full.e) # expanded data: stem, leaf, root, root:shoot, d13C
colnames(pca_full.e) 
# "Code" "Tmt"  "StemWet_expanded" "LeafWet_expanded" "rootmass_g"  "rootshoot"  "d13C"
# add Anet and cond ?

LiCOR_df %>% 
  mutate(Code = if_else(nchar(ID) == 4,substr(ID,1,3),substr(ID,1,4))) %>% 
  dplyr::select(Code) %>% 
  tally(Code %in% pca_full.e$Code) # 61 overlapping

# rootimage <- rootimage %>% 
#   mutate(Code = if_else(nchar(Code) == 4,substr(Code,1,3),substr(Code,1,4)))

permanova_df <- LiCOR_df %>% 
  mutate(Code = if_else(nchar(ID) == 4,substr(ID,1,3),substr(ID,1,4))) %>% 
  dplyr::select(Code, Photo.y, Cond.y, WUE.350) %>% 
  left_join(dplyr::select(rootimage, Plot, Code, Average.Diameter.mm, Root.Length.Diameter.Range.1.mm), by = "Code") %>%  
  left_join(pca_full.e, by = "Code") %>% 
  filter(!is.na(Tmt)) %>% 
  dplyr::select(Tmt, Plot, Photo.y, Cond.y, WUE.350, Average.Diameter.mm, Root.Length.Diameter.Range.1.mm, StemWet_expanded, LeafWet_expanded, rootmass_g, rootshoot, d13C) %>% 
  mutate(d13C = abs(d13C)) %>% 
  #  mutate(across(where(is.numeric), ~ rescale(.x))) %>% 
  mutate(H2OTmt = substr(Tmt,2,2), CO2Tmt = substr(Tmt,1,1))

permanova_dfL <- LiCOR_df %>% 
  filter(Spp == "L") %>% 
  mutate(Code = if_else(nchar(ID) == 4,substr(ID,1,3),substr(ID,1,4))) %>% 
  dplyr::select(Code, Photo.y, Cond.y, WUE.350) %>%
  left_join(dplyr::select(rootimage, Plot, Code, Average.Diameter.mm, Root.Length.Diameter.Range.1.mm), by = "Code") %>% 
  left_join(pca_full.e, by = "Code") %>%
  filter(!is.na(Tmt)) %>% 
  dplyr::select(Tmt, Plot, Photo.y, Cond.y, WUE.350, Average.Diameter.mm, Root.Length.Diameter.Range.1.mm, StemWet_expanded, LeafWet_expanded, rootmass_g, rootshoot, d13C) %>% 
  mutate(d13C = abs(d13C)) %>% 
  mutate(across(where(is.numeric), ~ rescale(.x))) %>% 
  mutate(H2OTmt = substr(Tmt,2,2), CO2Tmt = substr(Tmt,1,1))

permanova_dfV <- LiCOR_df %>% 
  filter(Spp == "V") %>% 
  mutate(Code = if_else(nchar(ID) == 4,substr(ID,1,3),substr(ID,1,4))) %>% 
  dplyr::select(Code, Photo.y, Cond.y, WUE.350) %>%
  left_join(dplyr::select(rootimage, Plot, Code, Average.Diameter.mm, Root.Length.Diameter.Range.1.mm), by = "Code") %>% 
  left_join(pca_full.e, by = "Code") %>%
  filter(!is.na(Tmt)) %>% 
  dplyr::select(Tmt, Plot, Photo.y, Cond.y, WUE.350, Average.Diameter.mm, Root.Length.Diameter.Range.1.mm, StemWet_expanded, LeafWet_expanded, rootmass_g, rootshoot, d13C) %>% 
  mutate(d13C = abs(d13C)) %>% 
  mutate(across(where(is.numeric), ~ rescale(.x))) %>% 
  mutate(H2OTmt = substr(Tmt,2,2), CO2Tmt = substr(Tmt,1,1))
colnames(permanova_df)
# [1] "Code"                            "Photo.y"                         "Cond.y"                          "WUE.350"                        
# [5] "Plot"                            "Average.Diameter.mm"             "Root.Length.Diameter.Range.1.mm" "Tmt"                            
# [9] "StemWet_expanded"                "LeafWet_expanded"                "rootmass_g"                      "rootshoot"                      
# [13] "d13C"  


library(MASS)
library(tidyverse)
library(scales)
# first get data into wide format
# ex: (Year, Plot, Bin1_ct, Bin2_ct, Bin3_ct, Bin4_ct)

#  # I think it is already in wide format where each number is like a count for its cell

# Create a multivariate response variable that includes only the columns of the data frame that include the counts in bins
# response_matrix <- permanova_df[,-c(1:2,13:14)]
# response_matrixL <- permanova_dfL[,-c(1:2,13:14)]
# response_matrixV <- permanova_dfV[,-c(1:2,13:14)]
response_matrix <- permanova_df[,c(3,8,9,10,12)] # only the five most interesting (?)
response_matrixL <- permanova_dfL[,c(3,8,9,10,12)]
response_matrixV <- permanova_dfV[,c(3,8,9,10,12)]

library(vegan)
adonis2(response_matrix ~ Tmt, permanova_df)
adonis2(response_matrix ~ H2OTmt*CO2Tmt, permanova_df)

adonis2(response_matrixV ~ Tmt, permanova_dfV)
adonis2(response_matrixV ~ H2OTmt*CO2Tmt, permanova_dfV) # for H2OTmt:CO2Tmt, p = 0.048! (rescaled, with Photo.y, Stem, Leaf, root, abs(d13C))
# adonis2(formula = response_matrixV ~ H2OTmt * CO2Tmt, data = permanova_dfV)
# Df SumOfSqs      R2       F Pr(>F)    
#   H2OTmt         1  0.77539 0.28387 11.4156  0.001 ***
#   CO2Tmt         1  0.34893 0.12774  5.1371  0.005 ** 
#   H2OTmt:CO2Tmt  1  0.18075 0.06617  2.6611  0.048 *  
#   Residual      21  1.42640 0.52221                   
#   Total         24  2.73146 1.00000 

adonis2(response_matrixV ~ H2OTmt*CO2, left_join(permanova_dfV, plot_CO2, by = "Plot")) ######
# Df    SumOfSqs      R2       F Pr(>F)    
# H2OTmt      1  0.77539 0.28387 11.3219  0.001 ***
# CO2         1  0.28275 0.10352  4.1287  0.005 ** 
# H2OTmt:CO2  1  0.23512 0.08608  3.4331  0.015 *  
# Residual   21  1.43820 0.52653                   
# Total      24  2.73146 1.00000  
colnames(response_matrixV)
# "Photo.y"          "StemWet_expanded" "LeafWet_expanded" "rootmass_g"       "d13C" 

X = permanova_dfV[,c(3,8,9,10,12)]
X=PERMANOVA::IniTransform(X)
D = PERMANOVA::DistContinuous(X)
V_permanova = PERMANOVA::PERMANOVA(D,as.factor(permanova_dfV$Tmt), CoordPrinc=TRUE)
PERMANOVA::plot.PERMANOVA(V_permanova, ColorGroup = c("pink", "lightblue", "red", "blue"),voronoi = TRUE, ColorInd = c("pink", "lightblue", "red", "blue"), PlotInd=TRUE, LabelInd=TRUE, AddLegend = TRUE)

biplot.pcoa(D)

adonis2(response_matrixL ~ Tmt, permanova_dfL)
adonis2(response_matrixL ~ H2OTmt*CO2Tmt, permanova_dfL)

# following this tutorial https://sites.ualberta.ca/~ahamann/teaching/renr690/labs/Lab6.pdf
H2OTmts <- permanova_dfV[,13]
CO2Tmts <- permanova_dfV[,14]
response_matrixV <- permanova_dfV[,c(3,8,9,10,12)]
adonis2(response_matrixV ~ unlist(H2OTmts)*unlist(CO2Tmts), distance = "mahalanobis") # same result as above :)
# choosing mahalanobis because some biomass variables are correlated
# pairwise comparisons adjusted for multiple inference just like a regular ANOVA
library(RVAideMemoire)
library(ecodist)
pairwise.perm.manova(sqrt(distance(response_matrixV, method="mahalanobis")), unlist(permanova_dfV[,1]))
#      AD    AW    ED   
#   AW 0.013 -     -    
#   ED 0.013 0.013 -    
#   EW 0.013 0.451 0.118
# P value adjustment method: fdr 

# NMDS
dm=sqrt(distance(response_matrixV, method="mahalanobis"))
nmds_out = nmds(dm, mindim=2, maxdim=2)
scores=nmds.min(nmds_out)
# Minimum stress for given dimensionality:  0.2214695 
# r^2 for minimum stress configuration:  0.6916589 
nmds_out$stress
#  [1] 0.2408827 0.2297706 0.2576101 0.2437968 0.2385648 0.2438741 0.2456996 0.2393398 0.2214695 0.2444444

mygroups = sort(unique(permanova_dfV$Tmt)) # shows order
mycol = c("pink", "lightblue", "red", "blue") # color in order
plot(scores, pch=19, col=mycol[as.factor(unique(permanova_dfV$Tmt))])
text(scores+0.2, labels=row.names(response_matrixV)) # add offset labels
vectors = vf(scores, response_matrixV, nperm=10) # calculate vectors
plot(vectors, len=0.1, col="red") # add vectors
ordiellipse(scores, permanova_dfV$Tmt, conf=0.6, col=mycol, show.groups=mygroups)

# adding Kruskal-Wallis test of significance to a boxplot (SIF)
library(ggsignif)
levels(SIF$Tmt) <- c("AD","AW","ED","EW")
SIF %>% # d13C
  ggplot(aes(x=Tmt, y=d13C)) + 
  geom_boxplot(aes(color=Tmt)) + 
  facet_grid(rows = vars(Spp), scales = "free") + 
  scale_color_manual(values = c("pink", "lightblue", "red", "blue")) +
  geom_text(data = nequals_SIF, aes(x = Tmt, y = -26, label = paste0("N = ",n))) +
  stat_compare_means(comparisons = list(c("AD","ED"), c("AW","EW")), method = "wilcox.test", na.rm = TRUE)
#  geom_signif(test="kruskal.test", exact=FALSE, comparisons = list(c("AD","ED"), c("AW","EW")),step_increase=0.2, map_signif_level = TRUE)

kruskal.test(SIF[,"d13C"],SIF[,"Tmt"])
require(graphics)
boxplot(d13C ~ Tmt, data = SIF)
kruskal.test(d13C ~ Tmt, data = SIF)

# messing with interaction model
main_effect_mod <- lmer(Photo.y ~ CO2Tmt+SWC + (1|Plot), data=filter(LiCOR_df, Spp=="V"))
interaction_mod <- lmer(Photo.y ~ CO2Tmt*SWC + (1|Plot), data=filter(LiCOR_df, Spp=="V"))
# main_effect_mod <- lm(Photo.y ~ CO2Tmt+SWC , data=filter(LiCOR_df, Spp=="V"))
# interaction_mod <- lm(Photo.y ~ CO2Tmt*SWC , data=filter(LiCOR_df, Spp=="V"))
anova(main_effect_mod, interaction_mod)
Anova(interaction_mod)
library(ggeffects)
ggpredict(interaction_mod, terms=c("SWC","CO2Tmt")) %>% 
  plot(add.data = TRUE, ci = FALSE, colors = "bw") + labs(linetype="CO2Tmt") 


# do it for other data types: total biomass
# first need SWC associated per plot
plot_SWC <- LiCOR_df[,c("Plot", "SWC")] %>% 
  group_by(Plot) %>% 
  summarise(meanSWC=mean(SWC)) %>% 
  as.data.frame()

biomass2_intx <- biomass2 %>% 
  mutate(totmass = StemWet_expanded+LeafWet_expanded+rootmass_g) %>% 
  mutate(rootshoot = rootmass_g/(StemWet_expanded+LeafWet_expanded)) %>% 
  left_join(plot_SWC, by = "Plot") %>% 
  left_join(plot_CO2, by = "Plot")

main_effect_mod <- lmer(rootshoot ~ CO2Tmt+meanSWC + (1|Plot), data=filter(biomass2_intx, Spp=="L"|Spp=="L"))
interaction_mod <- lmer(rootshoot ~ CO2Tmt*meanSWC + (1|Plot), data=filter(biomass2_intx, Spp=="L"|Spp=="L"))
anova(main_effect_mod, interaction_mod)
Anova(interaction_mod)
ggpredict(interaction_mod, terms=c("meanSWC","CO2Tmt")) %>% 
  plot(add.data = TRUE, ci = FALSE)

SIF_intx <- SIF %>% 
  left_join(plot_SWC, by = "Plot") %>% 
  left_join(plot_CO2, by = "Plot")

main_effect_mod <- lmer(d13C ~ CO2Tmt+meanSWC + (1|Plot), data=filter(SIF_intx, Spp=="V"|Spp=="V"))
interaction_mod <- lmer(d13C ~ CO2Tmt*meanSWC + (1|Plot), data=filter(SIF_intx, Spp=="V"|Spp=="V"))
anova(main_effect_mod, interaction_mod)
Anova(interaction_mod)
ggpredict(interaction_mod, terms=c("meanSWC","CO2Tmt")) %>% 
  plot(add.data = TRUE, ci = FALSE)

# switch the categorical and continuous variables
plot_CO2 <- avg_between %>% # from CO2_Ch2.R
  dplyr::select(Plot, mDeltaTest) %>% 
  mutate(CO2 = 420+mDeltaTest) %>% 
  as.data.frame()

LiCOR_df.intx <- LiCOR_df %>% 
  left_join(plot_CO2, by = "Plot")


main_effect_mod <- lmer(Photo.y ~ CO2+H2OTmt + (1|Plot), data=filter(LiCOR_df.intx, Spp=="V"|Spp=="V"))
interaction_mod <- lmer(Photo.y ~ CO2*H2OTmt + (1|Plot), data=filter(LiCOR_df.intx, Spp=="V"|Spp=="V"))
# Warning message:
# Some predictor variables are on very different scales: consider rescaling 
anova(main_effect_mod, interaction_mod)
Anova(interaction_mod)
ggpredict(interaction_mod, terms=c("CO2","H2OTmt")) %>% 
  plot(add.data = TRUE, ci = FALSE)

# photosynthesis
main_effect_mod <- lmer(Photo.y ~ CO2+H2OTmt + (1|Plot), data=filter(LiCOR_df.intx, Spp=="V"|Spp=="V"))
interaction_mod <- lmer(Photo.y ~ CO2*H2OTmt + (1|Plot), data=filter(LiCOR_df.intx, Spp=="V"|Spp=="V"))
# Warning message:
# Some predictor variables are on very different scales: consider rescaling 
anova(main_effect_mod, interaction_mod)
Anova(interaction_mod)
ggpredict(interaction_mod, terms=c("CO2","H2OTmt")) %>% 
  plot(add.data = TRUE, ci = FALSE)

# biomass
main_effect_mod <- lmer(totmass ~ CO2+H2OTmt + (1|Plot), data=filter(biomass2_intx, Spp=="V"|Spp=="V"))
interaction_mod <- lmer(totmass ~ CO2*H2OTmt + (1|Plot), data=filter(biomass2_intx, Spp=="V"|Spp=="V"))
anova(main_effect_mod, interaction_mod)
Anova(interaction_mod)
ggpredict(interaction_mod, terms=c("CO2","H2OTmt")) %>% 
  plot(add.data = TRUE, ci = FALSE)

# d13C
##########

### original attempts at univariate modeling with outliers by cook's distance; PERMANOVA
## model workflow for
# LiCOR_df. responses Photo.y, Cond.y, WUE.350
# biomass2. responses totmass, rootshoot, LWC
# rootimage. responses ratio_2to1, dia_range_1 or w/e, # branchpoints
# SIF. response d13C

# Step 0: check data for normality and normality of residuals (dharma package to check residuals)
# wait! have to do this by spp
# try transformed data # check SWC by eCO2 to see if there's a feedback # specific root length
# LiCOR_df.
hist((LiCOR_df.[LiCOR_df.$Spp == "V",]$Photo.y), breaks = 2*sqrt(nrow(LiCOR_df.)))
qqPlot(LiCOR_df.[LiCOR_df.$Spp == "V",]$Photo.y) 
hist((LiCOR_df.[LiCOR_df.$Spp == "L",]$Photo.y), breaks = 2*sqrt(nrow(LiCOR_df.)))
qqPlot((LiCOR_df.[LiCOR_df.$Spp == "L",]$Photo.y)) # pretty ok by spp!

hist(log(LiCOR_df.[LiCOR_df.$Spp=="V",]$Cond.y))
qqPlot(log(LiCOR_df.[LiCOR_df.$Spp=="V",]$Cond.y))
hist(log(LiCOR_df.[LiCOR_df.$Spp=="L",]$Cond.y))
qqPlot(log(LiCOR_df.[LiCOR_df.$Spp=="L",]$Cond.y))

LiCOR_df. %>% 
  filter(WUE.350 < 550) %>% 
  filter(Spp=="V") %>% 
  dplyr::select(WUE.350) %>% 
  unlist() %>% 
  as.numeric() %>% 
  sqrt() %>% 
  #  hist(breaks = sqrt(nrow(LiCOR_df.)))
  qqPlot()

# biomass2.
hist(sqrt(biomass2.[biomass2.$Spp=="L",]$totmass), breaks=2*sqrt(nrow(biomass2.))) # outlier
qqPlot(sqrt(biomass2.[biomass2.$Spp=="L",]$totmass)) 

biomass2. %>% 
  filter(rootshoot < 9) %>% 
  filter(Spp=="V") %>% 
  dplyr::select(rootshoot) %>% 
  unlist() %>% 
  as.numeric() %>% 
  # log() %>% 
  #hist(breaks = sqrt(nrow(LiCOR_df.)))
  qqPlot()
hist((biomass2.[biomass2.$Spp=="L",]$rootshoot), breaks=2*sqrt(nrow(biomass2.))) # outliers
qqPlot((biomass2.[biomass2.$Spp=="L",]$rootshoot)) # fatty right tail
qqPlot(log(biomass2.[biomass2.$Spp=="L",]$rootshoot)) # better for V, worse for L
# this one is tough

hist(biomass2.[biomass2.$Spp=="V",]$lwc, breaks=2*sqrt(nrow(biomass2.))) 
qqPlot(biomass2.[biomass2.$Spp=="V",]$lwc) 
# fine by spp

# rootimage.
rootimage. %>% 
  #  filter(SRL < 1500) %>% 
  filter(Spp=="V") %>% 
  dplyr::select(SRL) %>% 
  unlist() %>% 
  as.numeric() %>% 
  log() %>% 
  #hist(breaks = sqrt(nrow(rootimage.)))
  qqPlot()
hist(log(rootimage.[rootimage.$Spp=="V",]$SRL), breaks=sqrt(nrow(rootimage.)))
qqPlot(log(rootimage.[rootimage.$Spp=="V",]$SRL))
hist(log(rootimage.[rootimage.$Spp=="L",]$SRL), breaks=sqrt(nrow(rootimage.)))
qqPlot(log(rootimage.[rootimage.$Spp=="L",]$SRL))

hist(log(rootimage.[rootimage.$Spp=="L",]$Root.Length.Diameter.Range.1.mm))
qqPlot(log(rootimage.[rootimage.$Spp=="L",]$Root.Length.Diameter.Range.1.mm))

rootimage. %>% 
  filter(Number.of.Branch.Points > 5) %>% 
  filter(Spp=="V") %>% 
  dplyr::select(Number.of.Branch.Points) %>% 
  unlist() %>% 
  as.numeric() %>% 
  log() %>% 
  #hist(breaks = sqrt(nrow(LiCOR_df.)))
  qqPlot()
hist(log(rootimage.[rootimage.$Spp=="L",]$Number.of.Branch.Points))
qqPlot(log(rootimage.[rootimage.$Spp=="L",]$Number.of.Branch.Points))

# SIF.
hist(SIF.[SIF.$Spp=="V",]$d13C)
qqPlot(SIF.[SIF.$Spp=="V",]$d13C) # cute

library(lmerTest)  

# Step 2: for each of the four data types, and each response, go through this modeling protocol:
# test effect of CO2Tmt*H2OTmt on Plot means, for both species; then for each species
# test effect of CO2Tmt*H2OTmt on individual datapoints with (1|Plot), for both species; then for each species
# test effect of (rescaled)CO2*SWC on Plot means, for both species; then for each species
# test effect of (rescaled)CO2*SWC on individual datapoints with (1|Plot), for both species; then for each species
# ggeffects plot to visualize significant and/or interesting results

# models with transformed data
# go again; if intx is not significant, drop it and run the main effects

# Photo.y: no transformation, no outliers
m1V <- lmer(Photo.y ~ CO2*H2OTmt + (1|Plot), data = filter(LiCOR_df., Spp == "V"))
m1L <- lmer(Photo.y ~ CO2*H2OTmt + (1|Plot), data = filter(LiCOR_df., Spp == "L"))

# Cond.y: log transformations
m2V <- lmer(log(Cond.y) ~ CO2*H2OTmt + (1|Plot), data = filter(LiCOR_df., Spp == "V"))
m2L <- lmer(log(Cond.y) ~ CO2*H2OTmt + (1|Plot), data = filter(LiCOR_df., Spp == "L"))

# WUE.350: filter <550, sqrt
m3V <- lmer(WUE.350tfm ~ CO2*H2OTmt + (1|Plot), data = filter(LiCOR_df.tfm, Spp == "V"))
m3L <- lmer(WUE.350tfm ~ CO2*H2OTmt + (1|Plot), data = filter(LiCOR_df.tfm, Spp == "L"))

# totmass: sqrt
m4V <- lmer(sqrt(totmass) ~ CO2*H2OTmt + (1|Plot), data = filter(biomass2., Spp == "V"))
m4L <- lmer(sqrt(totmass) ~ CO2*H2OTmt + (1|Plot), data = filter(biomass2., Spp == "L"))

# rootshoot: filter >9
m5V <- lmer(rootshoot ~ CO2*H2OTmt + (1|Plot), data = filter(biomass2., Spp == "V" & rootshoot<9))
m5L <- lmer(rootshoot ~ CO2*H2OTmt + (1|Plot), data = filter(biomass2., Spp == "L" & rootshoot<9))

# lwc: ok
m6V <- lmer(lwc ~ CO2*H2OTmt + (1|Plot), data = filter(biomass2., Spp == "V")) 
m6L <- lmer(lwc ~ CO2*H2OTmt + (1|Plot), data = filter(biomass2., Spp == "L"))

# ratio_2to1: filter>0.01, log
m7V <- lmer(log(ratio_2to1) ~ CO2*H2OTmt + (1|Plot), data = filter(rootimage., Spp == "V" & ratio_2to1>0.01))
m7L <- lmer(log(ratio_2to1) ~ CO2*H2OTmt + (1|Plot), data = filter(rootimage., Spp == "L" & ratio_2to1>0.01))

## redo these with random effect!
# Root range 1: log
m8V <- lmer(log(Root.Length.Diameter.Range.1.mm) ~ CO2*H2OTmt + (1|Plot), data = filter(rootimage., Spp == "V"))
m8L <- lmer(log(Root.Length.Diameter.Range.1.mm) ~ CO2*H2OTmt + (1|Plot), data = filter(rootimage., Spp == "L"))

# # branch points: filter >5, log
m9V <- lmer(log(Number.of.Branch.Points) ~ CO2*H2OTmt + (1|Plot), data = filter(rootimage., Spp == "V" & Number.of.Branch.Points > 5))
m9L <- lmer(log(Number.of.Branch.Points) ~ CO2*H2OTmt + (1|Plot), data = filter(rootimage., Spp == "L" & Number.of.Branch.Points > 5))

# SIF: ok
m10V <- lmer(d13C ~ CO2*H2OTmt + (1|Plot), data = filter(SIF., Spp == "V"))
m10L <- lmer(d13C ~ CO2*H2OTmt + (1|Plot), data = filter(SIF., Spp == "L"))

# LiCOR_df. : Photo.y, Cond.y, WUE.350
LiCOR_df.pmeans <- LiCOR_df. %>% 
  dplyr::select(Code, Plot, Spp, CO2Tmt, H2OTmt, CO2, SWC, Photo.y, Cond.y, WUE.350) %>% 
  group_by(Plot, CO2Tmt, H2OTmt, Spp) %>% 
  summarise(across(c(CO2, SWC, Photo.y, Cond.y, WUE.350), ~ mean(.x, na.rm=TRUE)))
# Photo.y meets assumptions of normality (by spp)
summary(lm(Photo.y ~ CO2Tmt*H2OTmt, data = filter(LiCOR_df.pmeans, Spp == "V" | Spp == "L"))) # H2O p <.0003
summary(lm(Photo.y ~ CO2Tmt*H2OTmt, data = filter(LiCOR_df.pmeans, Spp == "L" | Spp == "L"))) # H2O p <.005 
summary(lm(Photo.y ~ CO2Tmt*H2OTmt, data = filter(LiCOR_df.pmeans, Spp == "V" | Spp == "V"))) # H2O p <.02
summary(lmer(Photo.y ~ CO2Tmt*H2OTmt + (1|Plot), data = filter(LiCOR_df., Spp == "V" | Spp == "L"))) # H2O p <.0004
summary(lmer(Photo.y ~ CO2Tmt*H2OTmt + (1|Plot), data = filter(LiCOR_df., Spp == "L" | Spp == "L"))) # H2O p <.008
summary(lmer(Photo.y ~ CO2Tmt*H2OTmt + (1|Plot), data = filter(LiCOR_df., Spp == "V" | Spp == "V"))) # H2O p <.015
summary(lm(Photo.y ~ rescale(CO2)*rescale(SWC), data = filter(LiCOR_df.pmeans, Spp == "L" | Spp == "V"))) # SWC p < .005
summary(lm(Photo.y ~ rescale(CO2)*rescale(SWC), data = filter(LiCOR_df.pmeans, Spp == "L" | Spp == "L"))) # SWC p < 0.03
summary(lm(Photo.y ~ rescale(CO2)*rescale(SWC), data = filter(LiCOR_df.pmeans, Spp == "V" | Spp == "V"))) # SWC p = 0.07
summary(lmer(Photo.y ~ rescale(CO2)*rescale(SWC) + (1|Plot), data = filter(LiCOR_df., Spp == "L" | Spp == "V"))) # SWC p<.0055
summary(lmer(Photo.y ~ rescale(CO2)*rescale(SWC) + (1|Plot), data = filter(LiCOR_df., Spp == "L" | Spp == "L"))) # SWC p<.05
summary(lmer(Photo.y ~ rescale(CO2)*rescale(SWC) + (1|Plot), data = filter(LiCOR_df., Spp == "V" | Spp == "V"))) # SWC p<.04
m1 <- lmer(Photo.y ~ CO2*H2OTmt + (1|Plot), data = filter(LiCOR_df., Spp == "V" | Spp == "V"))
ggpredict(m1L, terms=c("CO2","H2OTmt")) %>% 
  plot(add.data = TRUE, ci = FALSE)

cooksd1 <- cooks.distance(m1V)
sample_size <- nobs(m1V)
plot(cooksd1, pch="*", cex=2, main="Influential Obs by Cooks distance")  # plot cook's distance
abline(h = 4/sample_size, col="red")  # add cutoff line
text(x=1:length(cooksd1)+1, y=cooksd1, labels=ifelse(cooksd1>4/sample_size, names(cooksd1),""), col="red")  # add labels

influential1 <- as.numeric(names(cooksd1)[(cooksd1 > (4/sample_size))])
ggpredict(lmer(Photo.y ~ CO2*H2OTmt + (1|Plot), data = LiCOR_df. %>% 
                 filter(Spp == "V") %>% 
                 filter(!row_number() %in% influential1) ), terms=c("CO2","H2OTmt")) %>% plot(add.data = TRUE, ci = TRUE)
summary(lmer(Photo.y ~ CO2*H2OTmt + (1|Plot), data = LiCOR_df. %>% 
               filter(Spp == "V") %>% 
               filter(!row_number() %in% influential1) ))

# test model residuals
library(DHARMa)
plot(simulateResiduals(m1V))
plot(simulateResiduals(m1L))

# Cond.y: log
#summary(lm(Cond.y ~ CO2Tmt*H2OTmt, data = filter(LiCOR_df.pmeans, Spp == "V" | Spp == "L"))) # H2O p <.0015
summary(lm(log(Cond.y) ~ CO2Tmt*H2OTmt, data = filter(LiCOR_df.pmeans, Spp == "L" | Spp == "L"))) # H2O p <.002
summary(lm(log(Cond.y) ~ CO2Tmt*H2OTmt, data = filter(LiCOR_df.pmeans, Spp == "V" | Spp == "V"))) # H2O p <<<, CO2 <.01, intx<.025
#summary(lmer(Cond.y ~ CO2Tmt*H2OTmt + (1|Plot), data = filter(LiCOR_df., Spp == "V" | Spp == "L"))) # H2O p<.0006
summary(lmer(log(Cond.y) ~ CO2Tmt*H2OTmt + (1|Plot), data = filter(LiCOR_df., Spp == "L" | Spp == "L"))) # H2O p<.0003, isSing
summary(lmer(log(Cond.y) ~ CO2Tmt*H2OTmt + (1|Plot), data = filter(LiCOR_df., Spp == "V" | Spp == "V"))) # H2O p<.0002, CO2 <.015, int<.03
#summary(lm(Cond.y ~ rescale(CO2)*rescale(SWC), data = filter(LiCOR_df.pmeans, Spp == "L" | Spp == "V"))) # SWC p < .002
summary(lm(log(Cond.y) ~ rescale(CO2)*rescale(SWC), data = filter(LiCOR_df.pmeans, Spp == "L" | Spp == "L"))) # SWC p < 0.003
summary(lm(log(Cond.y) ~ rescale(CO2)*rescale(SWC), data = filter(LiCOR_df.pmeans, Spp == "V" | Spp == "V"))) # SWC p<<<, CO2<.0085, intx<.03
#summary(lmer(Cond.y ~ rescale(CO2)*rescale(SWC) + (1|Plot), data = filter(LiCOR_df., Spp == "L" | Spp == "V"))) # SWC p<<<.05
summary(lmer(log(Cond.y) ~ rescale(CO2)*rescale(SWC) + (1|Plot), data = filter(LiCOR_df., Spp == "L" | Spp == "L"))) # SWC p<<<, isSing
summary(lmer(log(Cond.y) ~ rescale(CO2)*rescale(SWC) + (1|Plot), data = filter(LiCOR_df., Spp == "V" | Spp == "V"))) # SWC p<<<, CO2<.02, int<.065
summary(lmer(log(Cond.y) ~ CO2*H2OTmt + (1|Plot), data = filter(LiCOR_df., Spp == "V" | Spp == "V"))) # H2) .003, CO2 .008, intx .02
m2 <- lmer(log(Cond.y) ~ CO2*H2OTmt + (1|Plot), data = filter(LiCOR_df., Spp == "V" | Spp == "V"))
ggpredict(m2, terms=c("CO2","H2OTmt")) %>% 
  plot(add.data = TRUE, ci = TRUE)
# Model has log-transformed response. Back-transforming predictions to original response scale. Standard errors are still on the log-scale.
# once again, why would eCO2 plants have *higher* gs when compared with their same-watering buddies ?

cooksd2 <- cooks.distance(m2L)
sample_size <- nobs(m2L)
plot(cooksd2, pch="*", cex=2, main="Influential Obs by Cooks distance")  # plot cook's distance
abline(h = 4/sample_size, col="red")  # add cutoff line
text(x=1:length(cooksd2)+1, y=cooksd2, labels=ifelse(cooksd2>4/sample_size, names(cooksd2),""), col="red")  # add labels

influential2 <- as.numeric(names(cooksd2)[(cooksd2 > (4/sample_size))]) # none are outliers! :)
ggpredict(lmer(log(Cond.y) ~ CO2*H2OTmt + (1|Plot), data = LiCOR_df. %>% 
                 filter(Spp == "L") %>% 
                 filter(!row_number() %in% influential2) ), terms=c("CO2","H2OTmt")) %>% plot(add.data = TRUE, ci = TRUE)
summary(lmer(log(Cond.y) ~ CO2*H2OTmt + (1|Plot), data = LiCOR_df. %>% 
               filter(Spp == "L") %>% 
               filter(!row_number() %in% influential2) )) 
plot(simulateResiduals(m2V))
plot(simulateResiduals(m2L))


LiCOR_df.tfm <- LiCOR_df. %>% 
  filter(WUE.350 < 550) %>% 
  mutate(WUE.350tfm = sqrt((WUE.350)))

LiCOR_df.tfm_pmeans <- LiCOR_df. %>% 
  filter(WUE.350 < 550) %>% 
  dplyr::select(Code, Plot, Spp, CO2Tmt, H2OTmt, CO2, SWC, Photo.y, Cond.y, WUE.350) %>% 
  group_by(Plot, CO2Tmt, H2OTmt, Spp) %>% 
  summarise(across(c(CO2, SWC, Photo.y, Cond.y, WUE.350), ~ mean(.x, na.rm=TRUE))) %>% 
  mutate(WUE.350tfm = sqrt(WUE.350))


#summary(lm(WUE.350 ~ CO2Tmt*H2OTmt, data = filter(LiCOR_df.pmeans, Spp == "V" | Spp == "L"))) 
summary(lm(WUE.350tfm ~ CO2Tmt*H2OTmt, data = filter(LiCOR_df.tfm_pmeans, Spp == "L" | Spp == "L"))) 
summary(lm(WUE.350tfm ~ CO2Tmt*H2OTmt, data = filter(LiCOR_df.tfm_pmeans, Spp == "V" | Spp == "V"))) 
#summary(lmer(WUE.350 ~ CO2Tmt*H2OTmt + (1|Plot), data = filter(LiCOR_df., Spp == "V" | Spp == "L"))) 
summary(lmer(WUE.350tfm ~ CO2Tmt*H2OTmt + (1|Plot), data = filter(LiCOR_df.tfm, Spp == "L" | Spp == "L")))
summary(lmer(WUE.350tfm ~ CO2Tmt*H2OTmt + (1|Plot), data = filter(LiCOR_df.tfm, Spp == "V" | Spp == "V"))) 
#summary(lm(WUE.350 ~ rescale(CO2)*rescale(SWC), data = filter(LiCOR_df.pmeans, Spp == "L" | Spp == "V"))) 
summary(lm(WUE.350tfm ~ rescale(CO2)*rescale(SWC), data = filter(LiCOR_df.tfm_pmeans, Spp == "L" | Spp == "L")))
summary(lm(WUE.350tfm ~ rescale(CO2)*rescale(SWC), data = filter(LiCOR_df.tfm_pmeans, Spp == "V" | Spp == "V"))) 
#summary(lmer(WUE.350 ~ rescale(CO2)*rescale(SWC) + (1|Plot), data = filter(LiCOR_df., Spp == "L" | Spp == "V")))
summary(lmer(WUE.350tfm ~ rescale(CO2)*rescale(SWC) + (1|Plot), data = filter(LiCOR_df.tfm, Spp == "L" | Spp == "L")))
summary(lmer(WUE.350tfm ~ rescale(CO2)*rescale(SWC) + (1|Plot), data = filter(LiCOR_df.tfm, Spp == "V" | Spp == "V")))
m3 <- lmer(WUE.350tfm ~ CO2*H2OTmt + (1|Plot), data = filter(LiCOR_df.tfm, Spp == "V" | Spp == "V"))
ggpredict(m3L, terms=c("CO2","H2OTmt")) %>% 
  plot(add.data = TRUE, ci = TRUE) # really nothing to see here

cooksd3 <- cooks.distance(m3L)
sample_size <- nobs(m3L)
plot(cooksd3, pch="*", cex=2, main="Influential Obs by Cooks distance")  # plot cook's distance
abline(h = 4/sample_size, col="red")  # add cutoff line
text(x=1:length(cooksd3)+1, y=cooksd3, labels=ifelse(cooksd3>4/sample_size, names(cooksd3),""), col="red")  # add labels

influential3 <- as.numeric(names(cooksd3)[(cooksd3 > (4/sample_size))])
ggpredict(lmer(WUE.350tfm ~ CO2*H2OTmt + (1|Plot), data = LiCOR_df.tfm %>% 
                 filter(Spp == "L") %>% 
                 filter(!row_number() %in% influential3) ), terms=c("CO2","H2OTmt")) %>% plot(add.data = TRUE, ci = TRUE)
summary(lmer(WUE.350tfm ~ CO2*H2OTmt + (1|Plot), data = LiCOR_df.tfm %>% 
               filter(Spp == "L") %>% 
               filter(!row_number() %in% influential3) ))

plot(simulateResiduals(m3V))
plot(simulateResiduals(m3L)) # quantile deviations detected (red curves) combined adjusted quantile test n.s.

# biomass2. : totmass, rootshoot, LWC
biomass2.pmeans <- biomass2. %>% 
  group_by(Plot, CO2Tmt, H2OTmt, Spp) %>% 
  summarise(across(c(CO2, meanSWC, totmass, rootshoot, lwc), ~ mean(.x, na.rm=TRUE)))

#summary(lm(totmass ~ CO2Tmt*H2OTmt, data = filter(biomass2.pmeans, Spp == "V" | Spp == "L"))) # H2O p <.03
summary(lm(sqrt(totmass) ~ CO2Tmt*H2OTmt, data = filter(biomass2.pmeans, Spp == "L" | Spp == "L"))) # H2O p<.065
summary(lm(sqrt(totmass) ~ CO2Tmt*H2OTmt, data = filter(biomass2.pmeans, Spp == "V" | Spp == "V"))) # H2O p <.007
#summary(lmer(totmass ~ CO2Tmt*H2OTmt + (1|Plot), data = filter(biomass2., Spp == "V" | Spp == "L")))
summary(lmer(sqrt(totmass) ~ CO2Tmt*H2OTmt + (1|Plot), data = filter(biomass2., Spp == "L" | Spp == "L"))) 
summary(lmer(sqrt(totmass) ~ CO2Tmt*H2OTmt + (1|Plot), data = filter(biomass2., Spp == "V" | Spp == "V"))) # H2O p<.001
#summary(lm(totmass ~ rescale(CO2)*rescale(meanSWC), data = filter(biomass2.pmeans, Spp == "L" | Spp == "V"))) # SWC p < .032
summary(lm(sqrt(totmass) ~ rescale(CO2)*rescale(meanSWC), data = filter(biomass2.pmeans, Spp == "L" | Spp == "L"))) # SWC p < 0.05
summary(lm(sqrt(totmass) ~ rescale(CO2)*rescale(meanSWC), data = filter(biomass2.pmeans, Spp == "V" | Spp == "V"))) # SWC p<.05
#summary(lmer(totmass ~ rescale(CO2)*rescale(meanSWC) + (1|Plot), data = filter(biomass2., Spp == "L" | Spp == "V"))) 
summary(lmer(sqrt(totmass) ~ rescale(CO2)*rescale(meanSWC) + (1|Plot), data = filter(biomass2., Spp == "L" | Spp == "L"))) # SWC < 0.1
summary(lmer(sqrt(totmass) ~ rescale(CO2)*rescale(meanSWC) + (1|Plot), data = filter(biomass2., Spp == "V" | Spp == "V"))) # SWC p<.055
m4 <- lmer(sqrt(totmass) ~ CO2*H2OTmt + (1|Plot), data = filter(biomass2., Spp == "V" | Spp == "V"))
ggpredict(m4, terms=c("CO2","H2OTmt")) %>% 
  plot(add.data = TRUE, ci = TRUE)

cooksd4 <- cooks.distance(m4L)
sample_size <- nobs(m4L)
plot(cooksd4, pch="*", cex=2, main="Influential Obs by Cooks distance")  # plot cook's distance
abline(h = 4/sample_size, col="red")  # add cutoff line
text(x=1:length(cooksd4)+1, y=cooksd4, labels=ifelse(cooksd4>4/sample_size, names(cooksd4),""), col="red")  # add labels

influential4 <- as.numeric(names(cooksd4)[(cooksd4 > (4/sample_size))])
ggpredict(lmer(sqrt(totmass) ~ CO2*H2OTmt + (1|Plot), data = biomass2. %>% 
                 filter(Spp == "V") %>% 
                 filter(!row_number() %in% influential4) ), terms=c("CO2","H2OTmt")) %>% plot(add.data = TRUE, ci = TRUE)
summary(lmer(sqrt(totmass) ~ CO2*H2OTmt + (1|Plot), data = biomass2. %>% 
               filter(Spp == "V") %>% 
               filter(!row_number() %in% influential4) )) # + with H2O, + with CO2, but not significant and no intxn

plot(simulateResiduals(m4V))
plot(simulateResiduals(m4L))

## leave out > 9 but otherwise leave alone
# rootshoot
#summary(lm(rootshoot ~ CO2Tmt*H2OTmt, data = filter(biomass2.pmeans, Spp == "V" | Spp == "L"))) 
summary(lm(rootshoot ~ CO2Tmt*H2OTmt, data = filter(biomass2.pmeans, Spp == "L" | Spp == "L" & rootshoot<9))) # H2O p < .009
summary(lm(rootshoot ~ CO2Tmt*H2OTmt, data = filter(biomass2.pmeans, Spp == "V" | Spp == "V" & rootshoot<9))) 
#summary(lmer(rootshoot ~ CO2Tmt*H2OTmt + (1|Plot), data = filter(biomass2., Spp == "V" | Spp == "L")))
summary(lmer(rootshoot ~ CO2Tmt*H2OTmt + (1|Plot), data = filter(biomass2., Spp == "L" | Spp == "L" & rootshoot<9))) # H2O p< .0021
summary(lmer(rootshoot ~ CO2Tmt*H2OTmt + (1|Plot), data = filter(biomass2., Spp == "V" | Spp == "V" & rootshoot<9)))
# summary(lm(rootshoot ~ rescale(CO2)*rescale(meanSWC), data = filter(biomass2.pmeans, Spp == "L" | Spp == "V"))) 
summary(lm(rootshoot ~ rescale(CO2)*rescale(meanSWC), data = filter(biomass2.pmeans, Spp == "L" | Spp == "L" & rootshoot<9))) # SWC p < 0.05
summary(lm(rootshoot ~ rescale(CO2)*rescale(meanSWC), data = filter(biomass2.pmeans, Spp == "V" | Spp == "V" & rootshoot<9)))  # CO2 < .085
# summary(lmer(rootshoot ~ rescale(CO2)*rescale(meanSWC) + (1|Plot), data = filter(biomass2., Spp == "L" | Spp == "V"))) 
summary(lmer(rootshoot ~ rescale(CO2)*rescale(meanSWC) + (1|Plot), data = filter(biomass2., Spp == "L" | Spp == "L" & rootshoot<9))) # SWC p<.075
summary(lmer(rootshoot ~ rescale(CO2)*rescale(meanSWC) + (1|Plot), data = filter(biomass2., Spp == "V" | Spp == "V" & rootshoot<9))) # CO2 p<.1
m5 <- lmer(rootshoot ~ CO2*H2OTmt + (1|Plot), data = filter(biomass2., Spp == "V" | Spp == "V" & rootshoot<9))
ggpredict(m5, terms=c("CO2","H2OTmt")) %>% 
  plot(add.data = TRUE, ci = TRUE)

cooksd5 <- cooks.distance(m5L)
sample_size <- nobs(m5L)
plot(cooksd5, pch="*", cex=2, main="Influential Obs by Cooks distance")  # plot cook's distance
abline(h = 4/sample_size, col="red")  # add cutoff line
text(x=1:length(cooksd5)+1, y=cooksd5, labels=ifelse(cooksd5>4/sample_size, names(cooksd5),""), col="red")  # add labels

influential5 <- as.numeric(names(sort(cooksd5, decreasing = TRUE)[1:2]))
ggpredict(lmer(rootshoot ~ CO2*H2OTmt + (1|Plot), data = biomass2. %>% 
                 filter(Spp == "L") %>% 
                 filter(rootshoot<9) %>% 
                 filter(!row_number() %in% influential5) ), terms=c("CO2","H2OTmt")) %>% plot(add.data = TRUE, ci = TRUE)
summary(lmer(rootshoot ~ CO2*H2OTmt + (1|Plot), data = biomass2. %>% 
               filter(Spp == "L") %>% 
               filter(rootshoot <9) %>% 
               filter(!row_number() %in% influential5) )) # nah

plot(simulateResiduals(m5V))
plot(simulateResiduals(m5L))

## come back to this one
# lwc
#summary(lm(lwc ~ CO2Tmt*H2OTmt, data = filter(biomass2.pmeans, Spp == "V" | Spp == "L"))) 
summary(lm(lwc ~ CO2Tmt*H2OTmt, data = filter(biomass2.pmeans, Spp == "L" | Spp == "L")))
summary(lm(lwc ~ CO2Tmt*H2OTmt, data = filter(biomass2.pmeans, Spp == "V" | Spp == "V"))) # H2O < .02, CO2 < .075
#summary(lmer(lwc ~ CO2Tmt*H2OTmt + (1|Plot), data = filter(biomass2., Spp == "V" | Spp == "L")))
summary(lmer(lwc ~ CO2Tmt*H2OTmt + (1|Plot), data = filter(biomass2., Spp == "L" | Spp == "L"))) 
summary(lmer(lwc ~ CO2Tmt*H2OTmt + (1|Plot), data = filter(biomass2., Spp == "V" | Spp == "V"))) # H2O < 0.025, CO2 < 0.1
#summary(lm(lwc ~ rescale(CO2)*rescale(meanSWC), data = filter(biomass2.pmeans, Spp == "L" | Spp == "V"))) 
summary(lm(lwc ~ rescale(CO2)*rescale(meanSWC), data = filter(biomass2.pmeans, Spp == "L" | Spp == "L"))) 
summary(lm(lwc ~ rescale(CO2)*rescale(meanSWC), data = filter(biomass2.pmeans, Spp == "V" | Spp == "V"))) # SWC < .075
#summary(lmer(lwc ~ rescale(CO2)*rescale(meanSWC) + (1|Plot), data = filter(biomass2., Spp == "L" | Spp == "V"))) 
summary(lmer(lwc ~ rescale(CO2)*rescale(meanSWC) + (1|Plot), data = filter(biomass2., Spp == "L" | Spp == "L"))) 
summary(lmer(lwc ~ rescale(CO2)*rescale(meanSWC) + (1|Plot), data = filter(biomass2., Spp == "V" | Spp == "V"))) # SWC < .07
m6 <- lmer(lwc ~ CO2+H2OTmt + (1|Plot), data = filter(biomass2., Spp == "V" | Spp == "V"))
ggpredict(m6, terms=c("CO2","H2OTmt")) %>% 
  plot(add.data = TRUE, ci = TRUE)

cooksd6 <- cooks.distance(m6L)
sample_size <- nobs(m6L)
plot(cooksd6, pch="*", cex=2, main="Influential Obs by Cooks distance")  # plot cook's distance
abline(h = 4/sample_size, col="red")  # add cutoff line
text(x=1:length(cooksd6)+1, y=cooksd6, labels=ifelse(cooksd6>4/sample_size, names(cooksd6),""), col="red")  # add labels

influential6 <- as.numeric(names(cooksd6)[(cooksd6 > (4/sample_size))])
ggpredict(lmer(lwc ~ CO2*H2OTmt + (1|Plot), data = biomass2. %>% 
                 filter(Spp == "L") %>% 
                 filter(!row_number() %in% influential6) ), terms=c("CO2","H2OTmt")) %>% plot(add.data = TRUE, ci = TRUE)
summary(lmer(lwc ~ CO2*H2OTmt + (1|Plot), data = biomass2. %>% 
               filter(Spp == "L") %>% 
               filter(!row_number() %in% influential6) )) # nada

plot(simulateResiduals(m6V))
plot(simulateResiduals(m6L)) # quantile deviations detected; combine adjusted quantile test significant

# rootimage. : ratio_2to1, dia_range_1 or w/e, # branchpoints
rootimage.pmeans <- rootimage. %>% 
  group_by(Plot, CO2Tmt, H2OTmt, Spp) %>% 
  summarise(across(c(CO2, meanSWC, ratio_2to1, Root.Length.Diameter.Range.1.mm, Number.of.Branch.Points), ~ mean(.x, na.rm=TRUE)))

#summary(lm(ratio_2to1 ~ CO2Tmt*H2OTmt, data = filter(rootimage.pmeans, Spp == "V" | Spp == "L"))) 
summary(lm(log(ratio_2to1) ~ CO2Tmt*H2OTmt, data = filter(rootimage.pmeans, Spp == "L" | Spp == "L" & ratio_2to1>0.01))) # CO2 <.07
summary(lm(log(ratio_2to1) ~ CO2Tmt*H2OTmt, data = filter(rootimage.pmeans, Spp == "V" | Spp == "V" & ratio_2to1>0.01))) 
#summary(lmer(ratio_2to1 ~ CO2Tmt*H2OTmt + (1|Plot), data = filter(rootimage., Spp == "V" | Spp == "L"))) 
summary(lmer(log(ratio_2to1) ~ CO2Tmt*H2OTmt + (1|Plot), data = filter(rootimage., Spp == "L" | Spp == "L" & ratio_2to1>0.01))) # CO2 < .09
summary(lmer(log(ratio_2to1) ~ CO2Tmt*H2OTmt + (1|Plot), data = filter(rootimage., Spp == "V" | Spp == "V" & ratio_2to1>0.01))) 
#summary(lm(ratio_2to1 ~ rescale(CO2)*rescale(meanSWC), data = filter(rootimage.pmeans, Spp == "L" | Spp == "V"))) 
summary(lm(log(ratio_2to1) ~ rescale(CO2)*rescale(meanSWC), data = filter(rootimage.pmeans, Spp == "L" | Spp == "L" & ratio_2to1>0.01))) # CO2 <.03, intx<.08
summary(lm(log(ratio_2to1) ~ rescale(CO2)*rescale(meanSWC), data = filter(rootimage.pmeans, Spp == "V" | Spp == "V" & ratio_2to1>0.01))) 
#summary(lmer(ratio_2to1 ~ rescale(CO2)*rescale(meanSWC) + (1|Plot), data = filter(rootimage., Spp == "L" | Spp == "V"))) 
summary(lmer(log(ratio_2to1) ~ rescale(CO2)*rescale(meanSWC) + (1|Plot), data = filter(rootimage., Spp == "L" | Spp == "L" & ratio_2to1>0.01))) # CO2 < .06
summary(lmer(log(ratio_2to1) ~ rescale(CO2)*rescale(meanSWC) + (1|Plot), data = filter(rootimage., Spp == "V" | Spp == "V" & ratio_2to1>0.01)))
m7 <- lmer(log(SRL) ~ CO2*H2OTmt + (1|Plot), data = filter(rootimage., Spp == "V" & SRL<1500))
ggpredict(m7, terms=c("CO2","H2OTmt")) %>% 
  plot(add.data = TRUE, ci = TRUE)

cooksd7 <- cooks.distance(m7)
sample_size <- nobs(m7)
plot(cooksd7, pch="*", cex=2, main="Influential Obs by Cooks distance")  # plot cook's distance
abline(h = 4/sample_size, col="red")  # add cutoff line
text(x=1:length(cooksd7)+1, y=cooksd7, labels=ifelse(cooksd7>4/sample_size, names(cooksd7),""), col="red")  # add labels

influential7 <- as.numeric(names(cooksd7)[(cooksd7 > (4/sample_size))])
ggpredict(lmer(log(SRL) ~ CO2*H2OTmt + (1|Plot), data = rootimage. %>% 
                 filter(Spp == "V" & SRL<1500) %>% 
                 filter(!row_number() %in% influential7) ), terms=c("CO2","H2OTmt")) %>% plot(add.data = TRUE, ci = TRUE)
summary(lmer(log(SRL) ~ CO2*H2OTmt + (1|Plot), data = rootimage. %>% 
               filter(Spp == "V" & SRL<1500) %>% 
               filter(!row_number() %in% influential7) )) # something is maybe up with the coarse:fine roots; maybe not

plot(simulateResiduals(m7V))
plot(simulateResiduals(m7L)) # quantile deviations detected (red curves) combined adjusted quantile test n.s.

#summary(lm(Root.Length.Diameter.Range.1.mm ~ CO2Tmt*H2OTmt, data = filter(rootimage.pmeans, Spp == "V" | Spp == "L"))) # H2O <.0024
summary(lm(log(Root.Length.Diameter.Range.1.mm) ~ CO2Tmt*H2OTmt, data = filter(rootimage.pmeans, Spp == "L" | Spp == "L"))) # H2O<.0085
summary(lm(log(Root.Length.Diameter.Range.1.mm) ~ CO2Tmt*H2OTmt, data = filter(rootimage.pmeans, Spp == "V" | Spp == "V"))) # H2O<.02
#summary(lmer(Root.Length.Diameter.Range.1.mm ~ CO2Tmt*H2OTmt + (1|Plot), data = filter(rootimage., Spp == "V" | Spp == "L"))) # H2O < .015
summary(lmer(log(Root.Length.Diameter.Range.1.mm) ~ CO2Tmt*H2OTmt + (1|Plot), data = filter(rootimage., Spp == "L" | Spp == "L"))) # H2O <.025
summary(lmer(log(Root.Length.Diameter.Range.1.mm) ~ CO2Tmt*H2OTmt + (1|Plot), data = filter(rootimage., Spp == "V" | Spp == "V"))) # H2O<.035
#summary(lm(Root.Length.Diameter.Range.1.mm ~ rescale(CO2)*rescale(meanSWC), data = filter(rootimage.pmeans, Spp == "L" | Spp == "V"))) # H2O < .005
summary(lm(log(Root.Length.Diameter.Range.1.mm) ~ rescale(CO2)*rescale(meanSWC), data = filter(rootimage.pmeans, Spp == "L" | Spp == "L"))) # H2O<.015
summary(lm(log(Root.Length.Diameter.Range.1.mm) ~ rescale(CO2)*rescale(meanSWC), data = filter(rootimage.pmeans, Spp == "V" | Spp == "V"))) 
#summary(lmer(Root.Length.Diameter.Range.1.mm ~ rescale(CO2)*rescale(meanSWC) + (1|Plot), data = filter(rootimage., Spp == "L" | Spp == "V"))) # H2O < .025
summary(lmer(log(Root.Length.Diameter.Range.1.mm) ~ rescale(CO2)*rescale(meanSWC) + (1|Plot), data = filter(rootimage., Spp == "L" | Spp == "L"))) # H2O < .02
summary(lmer(Root.Length.Diameter.Range.1.mm ~ rescale(CO2)*rescale(meanSWC) + (1|Plot), data = filter(rootimage., Spp == "V" | Spp == "V")))
m8 <- lm(log(Root.Length.Diameter.Range.1.mm) ~ CO2Tmt*H2OTmt, data = filter(rootimage.pmeans, Spp == "L" | Spp == "L"))
ggpredict(m8L, terms=c("CO2","H2OTmt")) %>% 
  plot(add.data = TRUE, ci = TRUE)

cooksd8 <- cooks.distance(m8L)
sample_size <- nobs(m8L)
plot(cooksd8, pch="*", cex=2, main="Influential Obs by Cooks distance")  # plot cook's distance
abline(h = 4/sample_size, col="red")  # add cutoff line
text(x=1:length(cooksd8)+1, y=cooksd8, labels=ifelse(cooksd8>4/sample_size, names(cooksd8),""), col="red")  # add labels

## go back and filter on spp for the cook's distance filtered plots
influential8 <- as.numeric(names(cooksd8)[(cooksd8 > (4/sample_size))])
ggpredict(lm(log(Root.Length.Diameter.Range.1.mm) ~ CO2*H2OTmt, data = rootimage. %>% 
               filter(Spp == "L") %>% 
               filter(!row_number() %in% influential8) ), terms=c("CO2","H2OTmt")) %>% plot(add.data = TRUE, ci = TRUE)
summary(lm(log(Root.Length.Diameter.Range.1.mm) ~ CO2*H2OTmt, data = rootimage. %>% 
             filter(Spp == "L") %>% 
             filter(!row_number() %in% influential8) )) # wet plants have more fine root length

plot(simulateResiduals(m8V))
plot(simulateResiduals(m8L)) # qu = 0.25, log(sigma) = -2.911984 : outer Newton did not converge fully.
# We had to increase `err` for some of the quantiles. See fit$calibr$err
# quantile deviations detected; combine adjusted quantile test significant

#summary(lm(Number.of.Branch.Points ~ CO2Tmt*H2OTmt, data = filter(rootimage.pmeans, Spp == "V" | Spp == "L"))) # H2O <.0065
summary(lm(Number.of.Branch.Points ~ CO2Tmt*H2OTmt, data = filter(rootimage.pmeans, Spp == "L" | Spp == "L"))) # H2O<.05
summary(lm(Number.of.Branch.Points ~ CO2Tmt*H2OTmt, data = filter(rootimage.pmeans, Spp == "V" | Spp == "V"))) # H2O<.055
#summary(lmer(Number.of.Branch.Points ~ CO2Tmt*H2OTmt + (1|Plot), data = filter(rootimage., Spp == "V" | Spp == "L"))) # H2O < .035
summary(lmer(Number.of.Branch.Points ~ CO2Tmt*H2OTmt + (1|Plot), data = filter(rootimage., Spp == "L" | Spp == "L"))) # H2O < .065
summary(lmer(Number.of.Branch.Points ~ CO2Tmt*H2OTmt + (1|Plot), data = filter(rootimage., Spp == "V" | Spp == "V"))) # H2O < .07
#summary(lm(Number.of.Branch.Points ~ rescale(CO2)*rescale(meanSWC), data = filter(rootimage.pmeans, Spp == "L" | Spp == "V"))) # H2O < .035
summary(lm(Number.of.Branch.Points ~ rescale(CO2)*rescale(meanSWC), data = filter(rootimage.pmeans, Spp == "L" | Spp == "L"))) # H2O < .035
summary(lm(Number.of.Branch.Points ~ rescale(CO2)*rescale(meanSWC), data = filter(rootimage.pmeans, Spp == "V" | Spp == "V"))) 
#summary(lmer(Number.of.Branch.Points ~ rescale(CO2)*rescale(meanSWC) + (1|Plot), data = filter(rootimage., Spp == "L" | Spp == "V"))) # H2O < .025
summary(lmer(Number.of.Branch.Points ~ rescale(CO2)*rescale(meanSWC) + (1|Plot), data = filter(rootimage., Spp == "L" | Spp == "L"))) # H2O < .045
summary(lmer(Number.of.Branch.Points ~ rescale(CO2)*rescale(meanSWC) + (1|Plot), data = filter(rootimage., Spp == "V" | Spp == "V")))
m9 <- lm(Number.of.Branch.Points ~ CO2*H2OTmt, data = filter(rootimage.pmeans, Spp == "V" | Spp == "L"))
ggpredict(m9L, terms=c("CO2","H2OTmt")) %>% 
  plot(add.data = TRUE, ci = FALSE)

cooksd9 <- cooks.distance(m9L)
sample_size <- nobs(m9L)
plot(cooksd9, pch="*", cex=2, main="Influential Obs by Cooks distance")  # plot cook's distance
abline(h = 4/sample_size, col="red")  # add cutoff line
text(x=1:length(cooksd9)+1, y=cooksd9, labels=ifelse(cooksd9>4/sample_size, names(cooksd9),""), col="red")  # add labels

influential9 <- as.numeric(names(cooksd9)[(cooksd9 > (4/sample_size))])
ggpredict(lmer(log(Number.of.Branch.Points) ~ CO2*H2OTmt + (1|Plot), data = rootimage. %>% 
                 filter(Spp == "L" & Number.of.Branch.Points > 5) %>% 
                 filter(!row_number() %in% influential9) ), terms=c("CO2","H2OTmt")) %>% plot(add.data = TRUE, ci = TRUE)
summary(lmer(log(Number.of.Branch.Points) ~ CO2*H2OTmt + (1|Plot), data = rootimage. %>% 
               filter(Spp == "L" & Number.of.Branch.Points > 5) %>% 
               filter(!row_number() %in% influential9) )) # wet plants have more branchy roots

plot(simulateResiduals(m9V))
plot(simulateResiduals(m9L))

# SIF. : d13C
SIF.pmeans <- SIF. %>% 
  group_by(Plot, CO2Tmt, H2OTmt, Spp) %>% 
  summarise(across(c(CO2, meanSWC, d13C), ~ mean(.x, na.rm=TRUE)))

#summary(lm(d13C ~ CO2Tmt*H2OTmt, data = filter(SIF.pmeans, Spp == "V" | Spp == "L"))) # H2O <.03, CO2<.035
summary(lm(d13C ~ CO2Tmt*H2OTmt, data = filter(SIF.pmeans, Spp == "L" | Spp == "L"))) 
summary(lm(d13C ~ CO2Tmt*H2OTmt, data = filter(SIF.pmeans, Spp == "V" | Spp == "V"))) # H2O<.07, CO2<.025
#summary(lmer(d13C ~ CO2Tmt*H2OTmt + (1|Plot), data = filter(SIF., Spp == "V" | Spp == "L"))) # H2O <.095, CO2<.075
summary(lmer(d13C ~ CO2Tmt*H2OTmt + (1|Plot), data = filter(SIF., Spp == "L" | Spp == "L"))) 
summary(lmer(d13C ~ CO2Tmt*H2OTmt + (1|Plot), data = filter(SIF., Spp == "V" | Spp == "V"))) # CO2 < .025, H2O<.07
#summary(lm(d13C ~ rescale(CO2)*rescale(meanSWC), data = filter(SIF.pmeans, Spp == "L" | Spp == "V"))) # SWC < .075
summary(lm(d13C ~ rescale(CO2)*rescale(meanSWC), data = filter(SIF.pmeans, Spp == "L" | Spp == "L"))) 
summary(lm(d13C ~ rescale(CO2)*rescale(meanSWC), data = filter(SIF.pmeans, Spp == "V" | Spp == "V"))) # CO2 < .085
#summary(lmer(d13C ~ rescale(CO2)*rescale(meanSWC) + (1|Plot), data = filter(SIF., Spp == "L" | Spp == "V"))) 
summary(lmer(d13C ~ rescale(CO2)*rescale(meanSWC) + (1|Plot), data = filter(SIF., Spp == "L" | Spp == "L"))) 
summary(lmer(d13C ~ rescale(CO2)*rescale(meanSWC) + (1|Plot), data = filter(SIF., Spp == "V" | Spp == "V"))) # CO2<.08
m10 <- lm(d13C ~ CO2*H2OTmt, data = filter(SIF.pmeans, Spp == "V" | Spp == "L"))
ggpredict(m10L, terms=c("CO2","H2OTmt")) %>% 
  plot(add.data = TRUE, ci = TRUE)

cooksd10 <- cooks.distance(m10L)
sample_size <- nobs(m10L)
plot(cooksd10, pch="*", cex=2, main="Influential Obs by Cooks distance")  # plot cook's distance
abline(h = 4/sample_size, col="red")  # add cutoff line
text(x=1:length(cooksd10)+1, y=cooksd10, labels=ifelse(cooksd10>4/sample_size, names(cooksd10),""), col="red")  # add labels

influential10 <- as.numeric(names(cooksd10)[(cooksd10 > (4/sample_size))])
ggpredict(lmer(d13C ~ CO2*H2OTmt + (1|Plot), data = SIF. %>% 
                 filter(Spp == "L") %>% 
                 filter(!row_number() %in% influential10) ), terms=c("CO2","H2OTmt")) %>% plot(add.data = TRUE, ci = TRUE)
summary(lmer(d13C ~ CO2*H2OTmt + (1|Plot), data = SIF. %>% 
               filter(Spp == "L") %>% 
               filter(!row_number() %in% influential10) )) # CO2 < .1; eCO2 less water stressed per d13C

plot(simulateResiduals(m10V))
plot(simulateResiduals(m10L))


# Pick PERMANOVA variables based on the above analysis
# Photo.y, totmass, rootshoot, lwc(?), ratio_2to1
library(tidyr)
# combine dataframes and rescale responses
permanova.df <- biomass2. %>% 
  dplyr::select(Plot, Spp, Code, LeafWet_expanded, StemWet_expanded, rootmass_g, totmass, rootshoot, lwc, CO2Tmt, H2OTmt, CO2, meanSWC) %>% 
  left_join(LiCOR_df.tfm[,c("Code","Photo.y","Cond.y", "WUE.350tfm")], by = "Code") %>% 
  left_join(rootimage.[,c("Code","ratio_2to1", "Root.Length.Diameter.Range.1.mm","Number.of.Branch.Points")], by = "Code") %>% 
  # filter(!is.na(Photo.y)) %>% 
  # filter(!is.na(rootshoot)) %>% 
  left_join(SIF.[,c("Code","d13C")]) %>% 
  # mutate(logCond.y = log(Cond.y), sqrt_totmass = sqrt(totmass), logRootRatio = log(ratio_2to1), logFineRoot = log(Root.Length.Diameter.Range.1.mm), logBranch=log(Number.of.Branch.Points)) %>% 
  mutate(across(where(is.numeric), ~ rescale(.x))) %>% 
  left_join(lookup, by = "Plot") 

nrow(biomass2.) # 127
nrow(LiCOR_df.tfm) # 73
nrow(rootimage.) # 127
nrow(SIF.) # 91

permanova.dfL <- permanova.df %>% 
  filter(Spp=="L")
permanova.dfV <- permanova.df %>% 
  filter(Spp=="V")

# select variables for response matrix
# all of them
response_matrix. <- permanova.df %>% dplyr::select(where(is.numeric)) %>% dplyr::select(!c(CO2, meanSWC)) # 127
response_matrix.L <- permanova.dfL %>% dplyr::select(where(is.numeric)) %>% dplyr::select(!c(CO2, meanSWC)) # 66
response_matrix.V <- permanova.dfV %>% dplyr::select(where(is.numeric)) %>% dplyr::select(!c(CO2, meanSWC)) # 61
colnames(response_matrix.)
# [1] "LeafWet_expanded"                "StemWet_expanded"                "rootmass_g"                      "totmass"                        
# [5] "rootshoot"                       "lwc"                             "Photo.y"                         "Cond.y"                         
# [9] "WUE.350tfm"                      "ratio_2to1"                      "Root.Length.Diameter.Range.1.mm" "Number.of.Branch.Points"  

adonis2(response_matrix.[complete.cases(response_matrix.),] ~ CO2*H2OTmt, permanova.df[complete.cases(response_matrix.),], na.rm=TRUE, method="mahalanobis")
adonis2(response_matrix.L[complete.cases(response_matrix.L),] ~ CO2*H2OTmt, permanova.dfL[complete.cases(response_matrix.L),], na.rm=TRUE, method="mahalanobis")
adonis2(response_matrix.V[complete.cases(response_matrix.V),] ~ CO2*H2OTmt, permanova.dfV[complete.cases(response_matrix.V),], na.rm=TRUE, method="mahalanobis")
colnames(response_matrix.)

adonis2(response_matrix.[complete.cases(response_matrix.[,-c(6,9:12)]),-c(6,9:12)] ~ CO2*H2OTmt, permanova.df[complete.cases(response_matrix.[,-c(6,9:12)]),], na.rm=TRUE, method="mahalanobis")
adonis2(response_matrix.L[complete.cases(response_matrix.L[,-c(6,9:12)]),-c(6,9:12)] ~ CO2*H2OTmt, permanova.dfL[complete.cases(response_matrix.L[,-c(6,9:12)]),], na.rm=TRUE, method="mahalanobis")
adonis2(response_matrix.V[complete.cases(response_matrix.V[,-c(6,9:12)]),-c(6,9:12)] ~ CO2*H2OTmt, permanova.dfV[complete.cases(response_matrix.V[,-c(6,9:12)]),], na.rm=TRUE, method="mahalanobis")
colnames(response_matrix.[,-c(6,9:12)])
# "LeafWet_expanded" "StemWet_expanded" "rootmass_g"       "totmass"          "rootshoot"        "Photo.y"          "Cond.y"   
# marginal

adonis2(response_matrix.[complete.cases(response_matrix.[,-c(1,2,6,9:12)]),-c(1,2,6,9:12)] ~ CO2*H2OTmt, permanova.df[complete.cases(response_matrix.[,-c(1,2,6,9:12)]),], na.rm=TRUE)
adonis2(response_matrix.L[complete.cases(response_matrix.L[,-c(1,2,6,9:12)]),-c(1,2,6,9:12)] ~ CO2*H2OTmt, permanova.dfL[complete.cases(response_matrix.L[,-c(1,2,6,9:12)]),], na.rm=TRUE)
adonis2(response_matrix.V[complete.cases(response_matrix.V[,-c(1,2,6,9:12)]),-c(1,2,6,9:12)] ~ CO2*H2OTmt, permanova.dfV[complete.cases(response_matrix.V[,-c(1,2,6,9:12)]),], na.rm=TRUE)
colnames(response_matrix.[,-c(1,2,6,9:12)])
# "rootmass_g" "totmass"    "rootshoot"  "Photo.y"    "Cond.y"   # was marginal

adonis2(response_matrix.[complete.cases(response_matrix.[,-c(1:3,6,9:12)]),-c(1:3,6,9:12)] ~ CO2*H2OTmt, permanova.df[complete.cases(response_matrix.[,-c(1:3,6,9:12)]),], na.rm=TRUE, method="mahalanobis")
adonis2(response_matrix.L[complete.cases(response_matrix.L[,-c(1:3,6,9:12)]),-c(1:3,6,9:12)] ~ CO2*H2OTmt, permanova.dfL[complete.cases(response_matrix.L[,-c(1:3,6,9:12)]),], na.rm=TRUE, method="mahalanobis")
adonis2(response_matrix.V[complete.cases(response_matrix.V[,-c(1:3,6,9:12)]),-c(1:3,6,9:12)] ~ CO2*H2OTmt, permanova.dfV[complete.cases(response_matrix.V[,-c(1:3,6,9:12)]),], na.rm=TRUE, method="mahalanobis")
colnames(response_matrix.[,-c(1:3,6,9:12)])
# "totmass"   "rootshoot" "Photo.y"   "Cond.y"   # was 0.45-0.6

adonis2(response_matrix.[complete.cases(response_matrix.[,-c(1,2,4,6,9:12)]),-c(1,2,4,6,9:12)] ~ CO2*H2OTmt, permanova.df[complete.cases(response_matrix.[,-c(1,2,4,6,9:12)]),], na.rm=TRUE, method="mahalanobis")
adonis2(response_matrix.L[complete.cases(response_matrix.L[,-c(1,2,4,6,9:12)]),-c(1,2,4,6,9:12)] ~ CO2*H2OTmt, permanova.dfL[complete.cases(response_matrix.L[,-c(1,2,4,6,9:12)]),], na.rm=TRUE, method="mahalanobis")
adonis2(response_matrix.V[complete.cases(response_matrix.V[,-c(1,2,4,6,9:12)]),-c(1,2,4,6,9:12)] ~ CO2*H2OTmt, permanova.dfV[complete.cases(response_matrix.V[,-c(1,2,4,6,9:12)]),], na.rm=TRUE, method="mahalanobis")
colnames(response_matrix.[,-c(1,2,4,6,9:12)])
# "rootmass_g" "rootshoot"  "Photo.y"    "Cond.y"  # was marginal

adonis2(response_matrix.[complete.cases(response_matrix.[,c(7,2,1,3,13)]),c(7,2,1,3,13)] ~ CO2*H2OTmt, permanova.df[complete.cases(response_matrix.[,c(7,2,1,3,13)]),], na.rm=TRUE, method="mahalanobis")
adonis2(response_matrix.L[complete.cases(response_matrix.L[,c(7,2,1,3,13)]),c(7,2,1,3,13)] ~ CO2*H2OTmt, permanova.dfL[complete.cases(response_matrix.L[,c(7,2,1,3,13)]),], na.rm=TRUE, method="mahalanobis")
adonis2(response_matrix.V[complete.cases(response_matrix.V[,c(7,2,1,3,13)]),c(7,2,1,3,13)] ~ CO2*H2OTmt, permanova.dfV[complete.cases(response_matrix.V[,c(7,2,1,3,13)]),], na.rm=TRUE, method="mahalanobis")
colnames(response_matrix.[,c(7,2,1,3,13)])

adonis2(response_matrix.[complete.cases(response_matrix.[,-c(9:12)]),-c(9:12)] ~ CO2*H2OTmt, permanova.df[complete.cases(response_matrix.[,-c(9:12)]),], na.rm=TRUE, method="mahalanobis")
adonis2(response_matrix.L[complete.cases(response_matrix.L[,-c(9:12)]),-c(9:12)] ~ CO2*H2OTmt, permanova.dfL[complete.cases(response_matrix.L[,-c(9:12)]),], na.rm=TRUE, method="mahalanobis")
adonis2(response_matrix.V[complete.cases(response_matrix.V[,-c(9:12)]),-c(9:12)] ~ CO2*H2OTmt, permanova.dfV[complete.cases(response_matrix.V[,-c(9:12)]),], na.rm=TRUE, method="mahalanobis")
colnames(response_matrix.[,-c(9:12)]) # intx .021
colnames(response_matrix.[,-c(9:11)]) # intx .05
# [1] "LeafWet_expanded" "StemWet_expanded" "rootmass_g"       "totmass"          "rootshoot"       
# [6] "lwc"              "Photo.y"          "Cond.y"  


# this one is significant (.05); n = 17
adonis2(response_matrix.V[complete.cases(response_matrix.V[,-c(9:11)]),-c(9:11)] ~ CO2Tmt*H2OTmt, permanova.dfV[complete.cases(response_matrix.V[,-c(9:11)]),], na.rm=TRUE, method="mahalanobis")
adonis2(response_matrix.V[complete.cases(response_matrix.V[,-c(9:11)]),-c(9:11)] ~ CO2*H2OTmt, permanova.dfV[complete.cases(response_matrix.V[,-c(9:11)]),], na.rm=TRUE, method="mahalanobis") # marginal

X = response_matrix.V[,-c(1:3,6,9:12)] 
X=PERMANOVA::IniTransform(X)
D = PERMANOVA::DistContinuous(X)
V.permanova = PERMANOVA::PERMANOVA(D,as.factor(permanova.dfV$Tmt), CoordPrinc=TRUE)
PERMANOVA::plot.PERMANOVA(V.permanova, ColorGroup = c("pink", "lightblue", "red", "blue"), VoronoiColor = c("pink", "lightblue", "red", "blue"))

### 
# This was regular manova without weights:

manova.dfL <- manova.df %>% 
  filter(Spp=="L") %>% 
  dplyr::select(Photo.y, logCond.y, sqrt_WUE.350, sqrt_totmass, rootshoot, lwc, logSRL, logFineRoot) %>% 
  mutate_at(vars("Photo.y", "logCond.y", "sqrt_WUE.350", "sqrt_totmass", "rootshoot", "lwc", "logSRL", "logFineRoot"), list(unlist)) 

manova.dfV <- manova.df %>% 
  filter(Spp=="V") %>% 
  dplyr::select(Photo.y, logCond.y, sqrt_WUE.350, sqrt_totmass, rootshoot, lwc, logSRL, logFineRoot) %>% 
  mutate_at(vars("Photo.y", "logCond.y", "sqrt_WUE.350", "sqrt_totmass", "rootshoot", "lwc", "logSRL", "logFineRoot"), list(unlist)) 

manova.cbind <- cbind(unlist(manova.df$Photo.y),unlist(manova.df$logCond.y),unlist(manova.df$sqrt_WUE.350),unlist(manova.df$sqrt_totmass),unlist(manova.df$rootshoot),unlist(manova.df$lwc),unlist(manova.df$logSRL),unlist(manova.df$logFineRoot))
manova.cbindL <- cbind(unlist(manova.dfL$Photo.y),unlist(manova.dfL$logCond.y),unlist(manova.dfL$sqrt_WUE.350),unlist(manova.dfL$sqrt_totmass),unlist(manova.dfL$rootshoot),unlist(manova.dfL$lwc),unlist(manova.dfL$logSRL),unlist(manova.dfL$logFineRoot))
manova.cbindV <- cbind(unlist(manova.dfV$Photo.y),unlist(manova.dfV$logCond.y),unlist(manova.dfV$sqrt_WUE.350),unlist(manova.dfV$sqrt_totmass),unlist(manova.dfV$rootshoot),unlist(manova.dfV$lwc),unlist(manova.dfV$logSRL),unlist(manova.dfV$logFineRoot))

# reasonable subset: Photo.y, logCond.y, sqrt_totmass, rootshoot
summary(manova(manova.cbind[,c(1,2,4,5)] ~ CO2Tmt*H2OTmt, manova.df, weights = manova.df[,c(3,4,6,7)]))
summary(manova(manova.cbindL[,c(1,2,4,5)] ~ CO2Tmt*H2OTmt, filter(manova.df, Spp=="L")))
summary(manova(manova.cbindV[,c(1,2,4,5)] ~ CO2Tmt*H2OTmt, filter(manova.df, Spp=="V")))

# ummm why is it not like this anymore
#  summary(manova(manova.cbind[,c(1,2,4,5)] ~ CO2Tmt*H2OTmt, manova.df))
#                 Df  Pillai approx F num Df den Df    Pr(>F)    
#   CO2Tmt         1 0.36876    3.067      4     21  0.038881 *  
#   H2OTmt         1 0.91197   54.391      4     21 8.765e-11 ***
#   CO2Tmt:H2OTmt  1 0.48919    5.028      4     21  0.005304 ** 
#   Residuals     24         

# kitchen sink oh wait this did not work bc too many variables
summary(manova(manova.cbind[,c(1,2,5,7)] ~ CO2Tmt*H2OTmt, manova.df))
summary(manova(manova.cbindL[,c(1,2,5,7)] ~ CO2Tmt*H2OTmt, filter(manova.df, Spp=="L")))
summary(manova(manova.cbindV[,c(1,2,5,7)] ~ CO2Tmt*H2OTmt, filter(manova.df, Spp=="V")))

## this time try it without taking plot means

manova.df2 <- biomass2. %>% 
  dplyr::select(Plot, Spp, Code, LeafWet_expanded, StemWet_expanded, rootmass_g, totmass, rootshoot, lwc, CO2Tmt, H2OTmt, CO2, meanSWC) %>% 
  left_join(LiCOR_df.[,c("Code","Photo.y","Cond.y", "WUE.350", "CO2Tmt", "H2OTmt")], by = "Code") %>% 
  left_join(rootimage.[,c("Code","SRL", "Root.Length.Diameter.Range.1.mm","Number.of.Branch.Points")], by = "Code") %>% 
  left_join(SIF.[,c("Code","d13C")]) %>% 
  mutate(logCond.y = log(Cond.y), sqrt_WUE.350 = sqrt(WUE.350), sqrt_totmass = sqrt(totmass), logSRL = log(SRL), logFineRoot = log(Root.Length.Diameter.Range.1.mm), logBranch=log(Number.of.Branch.Points)) %>% 
  left_join(lookup, by = "Plot") %>% 
  mutate(H2OTmt = substr(Tmt,2,2), CO2Tmt = substr(Tmt,1,1)) 
#write.csv(manova.df2, "QuailFACE_all.csv")

# check correlation between variables
cor(manova.df2[, c(4:9,14:20)], use="pairwise.complete.obs") %>% 
  ggcorrplot()


manova.df2L <- manova.df2 %>% 
  filter(Spp=="L") %>% 
  dplyr::select(Photo.y, logCond.y, sqrt_WUE.350, sqrt_totmass, rootshoot, lwc, logRootRatio, logFineRoot) %>% 
  mutate_at(vars("Photo.y", "logCond.y", "sqrt_WUE.350", "sqrt_totmass", "rootshoot", "lwc", "logRootRatio", "logFineRoot"), list(unlist)) 

manova.df2V <- manova.df2 %>% 
  filter(Spp=="V") %>% 
  dplyr::select(Photo.y, logCond.y, sqrt_WUE.350, sqrt_totmass, rootshoot, lwc, logRootRatio, logFineRoot) %>% 
  mutate_at(vars("Photo.y", "logCond.y", "sqrt_WUE.350", "sqrt_totmass", "rootshoot", "lwc", "logRootRatio", "logFineRoot"), list(unlist)) 

manova.cbind <- cbind(unlist(manova.df2$Photo.y),unlist(manova.df2$logCond.y),unlist(manova.df2$sqrt_WUE.350),unlist(manova.df2$sqrt_totmass),unlist(manova.df2$rootshoot),unlist(manova.df2$lwc),unlist(manova.df2$logRootRatio),unlist(manova.df2$logFineRoot))
manova.cbind2L <- cbind(unlist(manova.df2L$Photo.y),unlist(manova.df2L$logCond.y),unlist(manova.df2L$sqrt_WUE.350),unlist(manova.df2L$sqrt_totmass),unlist(manova.df2L$rootshoot),unlist(manova.df2L$lwc),unlist(manova.df2L$logRootRatio),unlist(manova.df2L$logFineRoot))
manova.cbind2V <- cbind(unlist(manova.df2V$Photo.y),unlist(manova.df2V$logCond.y),unlist(manova.df2V$sqrt_WUE.350),unlist(manova.df2V$sqrt_totmass),unlist(manova.df2V$rootshoot),unlist(manova.df2V$lwc),unlist(manova.df2V$logRootRatio),unlist(manova.df2V$logFineRoot))

# reasonable subset: Photo.y, logCond.y, sqrt_totmass, rootshoot
summary(manova(manova.cbind[,c(1,2,4,5)] ~ CO2Tmt*H2OTmt, manova.df2))
summary(manova(manova.cbind2L[,c(1,2,4,5)] ~ CO2Tmt*H2OTmt, filter(manova.df2, Spp=="L")))
summary(manova(manova.cbind2V[,c(1,2,4,5)] ~ CO2Tmt*H2OTmt, filter(manova.df2, Spp=="V")))

summary(manova(manova.cbind[,c(1,2,5,7)] ~ CO2Tmt*H2OTmt, manova.df2))
summary(manova(manova.cbind2L[,c(1,2,5,7)] ~ CO2Tmt*H2OTmt, filter(manova.df2, Spp=="L")))
summary(manova(manova.cbind2V[,c(1,2,5,7)] ~ CO2Tmt*H2OTmt, filter(manova.df2, Spp=="V")))

## time series extrapolation
# Let us try a linear extrapolation from the time series data in the style of stack overflow:
# first with one that has all 8 values and no herb
extpTest_df <- data.frame(unique(inv_all_nh$value), inv_all[which(inv_all$Code == "14L1"),]$ht_mm) # prep df
colnames(extpTest_df) <- c("Date","ht_mm")
extpTest_df$pred1 <- predict(lm(ht_mm ~ poly(Date,3), data=extpTest_df)) # add predicted values

pred <- data.frame(Date = c(extpTest_df$Date, ymd("2023-12-20"),ymd("2024-01-20"),ymd("2024-02-20")))
pred$ht_mm <- predict(lm(ht_mm ~ poly(Date,3), data = extpTest_df), newdata=pred)

ggplot(extpTest_df, aes(x = Date, y=ht_mm)) + # plot
  geom_line() +
  geom_point() +
  geom_hline(aes(yintercept=0)) +
  geom_line(aes(y = pred1), color = "red") +
  geom_point(color="blue", data=pred)

# 3rd degree is best for prediction past November
# now for one that has herb, so only 5 values; or you could fit the average curve for the whole nh set?
extpTest_df2 <- data.frame(unique(inv_all_nh[which(inv_all_nh$Code == "11V3"),]$value), inv_all_nh[which(inv_all_nh$Code == "11V3"),]$ht_mm) # prep df
colnames(extpTest_df2) <- c("Date","ht_mm")
extpTest_df2$pred1 <- predict(lm(ht_mm ~ poly(Date,3), data=extpTest_df2)) # add predicted values

pred2 <- data.frame(Date = c(unique(inv_all_nh$value)))
pred2$ht_mm <- predict(lm(ht_mm ~ poly(Date,3), data = extpTest_df2), newdata=pred2)

ggplot(extpTest_df2, aes(x = Date, y=ht_mm)) + # plot
  geom_line() +
  geom_point() +
  geom_hline(aes(yintercept=0)) +
  geom_point(color="blue", shape=15, data=pred2) +
  geom_line(color="blue", data=pred2)


# is there a relationship between Ht.mm..5, Leaf.Ct..5, or Max.Leaf..5 and final leaf count, leaf size, leaf total area?
left_join(biomass2_leaf, lai[,c("Code","avg_area","tot_area","SLA", "count")], by = "Code") %>% 
ggplot(aes(x=Leaf.Ct..5, y = tot_area)) + # plot real values (dots) vs predicted (crosses)
  geom_point(aes(color = H2OTmt)) + facet_grid( ~ Spp) + scale_y_continuous(limits=c(0,30000))

# data loading duplicate
## load the data fresh with "." suffix; do steps from Ch3_explore and Ch3_LiCOR. Examine colnames

# plot_CO2, plot_SWC
plot_SWC. <- LiCOR_df.[,c("Plot", "SWC")] %>% 
  group_by(Plot) %>% 
  summarise(meanSWC=mean(SWC)) %>% 
  as.data.frame()

plot_CO2. <- avg_between %>% # from CO2_Ch2.R
  dplyr::select(Plot, mDeltaTest) %>% 
  mutate(CO2 = 420+mDeltaTest) %>% 
  as.data.frame()

# LiCOR_df
# actually just load this one after running Ch3_LiCOR
LiCOR_df. <- LiCOR_df
LiCOR_df.[,80:94] # has SWC, Plot, Spp, Tmt, Photo.y, Cond.y, WUE.350, CO2Tmt and H2OTmt
# needs shortcode, CO2
LiCOR_df. <- LiCOR_df. %>% 
  mutate(Code = if_else(nchar(ID) == 4,substr(ID,1,3),substr(ID,1,4))) %>% 
  left_join(plot_CO2., by = "Plot")

# biomass2
# jeez too many steps. get this one after Ch3_explore, it is messy but w/e
## add leaf area columns to biomass2 in explore script ##
biomass2. <- biomass2
biomass2.[1,]
biomass2. <- biomass2. %>% 
  mutate(rootshoot = rootmass_g/(StemWet_expanded + LeafWet_expanded), totmass = rootmass_g + StemWet_expanded + LeafWet_expanded, lwc = (LeafWet_g-LeafDry_g)*100/LeafWet_g) %>% 
  left_join(plot_CO2., by = "Plot") %>% 
  left_join(plot_SWC., by = "Plot") 

# exclude rootshoot for 6V2, an outlier: was herbivory list but predicted values failed to adjust appropriately
biomass2.[which.max(biomass2.$rootshoot),"rootshoot"] <- NA

# now remove lwc for seedlings from herbivory list, likely to have no leaves 
biomass2.[which(biomass2.$Code %in% firstherb$Code),]$lwc <- NA 

# rootimage
rootimage. <- read.csv("/Users/paigekouba/Documents/UC_Davis/2021_Winter/Quals/Proposal/Chapter 1/TinyFACE/GitHub/QuailFACE/RawData/features_4.26.24_final.csv") %>% 
  mutate(Plot = as.character(Plot))
rootimage. <- left_join(lookup, rootimage., by = 'Plot')

rootimage. <- rootimage. %>% 
  mutate(Spp = substr(Code, nchar(Code)-2,nchar(Code)-2)) %>%
  filter(Code != "4V3c") %>% # prevent duplicate codes; 4V3c was planted late and misnamed
  filter(Code != "16V1a") %>% # thinned but grew back
  mutate(Code = if_else(nchar(Code) == 4,substr(Code,1,3),substr(Code,1,4)), CO2Tmt = substring(Tmt,1,1), H2OTmt = substring(Tmt,2,2)) %>% 
  left_join(biomass2.[,c("Code","rootmass_g")]) %>% 
  mutate(SRL = Total.Root.Length.mm/rootmass_g) %>% 
  left_join(plot_CO2., by = "Plot") %>% 
  left_join(plot_SWC., by = "Plot") 
rootimage.[1,]

# SIF
SIF_raw <- read.csv("/Users/paigekouba/Documents/UC_Davis/2021_Winter/Quals/Proposal/Chapter 1/TinyFACE/GitHub/QuailFACE/RawData/PVK_SIF.csv")
SIF_raw <- SIF_raw %>% 
  filter(!is.na(Internal.ID))
SIF. <- SIF_raw %>% 
  filter(Sample.ID != "4V3c") %>% 
  mutate(Spp = substr(Sample.ID, nchar(Sample.ID)-2,nchar(Sample.ID)-2)) %>% 
  mutate(Plot = if_else(nchar(Sample.ID) == 4, substr(Sample.ID,1,1), substr(Sample.ID,1,2))) %>% 
  dplyr::select(Sample.ID, δ13CVPDB...., Spp, Plot) 
colnames(SIF.) <- c("Code","d13C","Spp","Plot")
SIF. <- left_join(SIF., lookup, by = "Plot") %>% 
  mutate(Code=if_else(nchar(Code) == 4,substr(Code,1,3),substr(Code,1,4)), CO2Tmt = substring(Tmt,1,1), H2OTmt = substring(Tmt,2,2)) %>% 
  left_join(plot_CO2., by = "Plot") %>% 
  left_join(plot_SWC., by = "Plot") 
SIF.[1,]

### clean copy of manova and weighted models

# original version for arithmetic plot means
manova.df <- biomass2. %>% 
  dplyr::select(Plot, Spp, Code, LeafWet_expanded, StemWet_expanded, rootmass_g, totmass, rootshoot, lwc, CO2Tmt, H2OTmt, CO2, meanSWC) %>% 
  left_join(LiCOR_df.[,c("Code","Photo.y","Cond.y", "WUE.350")], by = "Code") %>% 
  left_join(rootimage.[,c("Code","SRL", "Root.Length.Diameter.Range.1.mm","Number.of.Branch.Points")], by = "Code") %>% 
  left_join(SIF.[,c("Code","d13C")]) %>% 
  group_by(Plot, Spp) %>% 
  summarise(across(where(is.numeric), ~ mean(.x, na.rm=TRUE))) %>% 
  ungroup() %>% 
  mutate(logCond.y = log(Cond.y), sqrt_WUE.350 = sqrt(WUE.350), sqrt_totmass = sqrt(totmass), logSRL = log(SRL), logFineRoot = log(Root.Length.Diameter.Range.1.mm), logBranch=log(Number.of.Branch.Points)) %>% 
  left_join(lookup, by = "Plot") %>% 
  mutate(H2OTmt = substr(Tmt,2,2), CO2Tmt = substr(Tmt,1,1)) 
#write.csv(manova.df, "QuailFACE_plotmeans.csv")

# I can write my own function
# function on a list of numbers that calculates their 1/sigma^2; then get column sum and divide each by that sum so they add up to one. Last, use these as the weights in univariate models. Still have to figure out what to do about MANOVA; can use just one variable's weights (Photo.y) or some kind of combined? Do this on a smaller dataset to keep things simple: Photo.y, Cond.y, rootshoot, logSRL
one_over_var <- function(x){
  1/(sd(x, na.rm=TRUE)^2)
}

manova.df.small0 <- biomass2. %>% 
  filter(Spp =="V") %>% 
  dplyr::select(Plot, Spp, Code, rootshoot, CO2, meanSWC) %>% 
  left_join(LiCOR_df.[,c("Code","Photo.y","Cond.y")], by = "Code") %>% 
  left_join(rootimage.[,c("Code","SRL")], by = "Code") %>% 
  mutate(logCond.y = log(Cond.y), logSRL = log(SRL)) %>%
  group_by(Plot, Spp) %>% 
  mutate(n = n()) %>% 
  mutate_at(c("Photo.y", "rootshoot", "logCond.y", "logSRL"), list(oov=one_over_var)) %>%
  summarise(across(where(is.numeric), ~ mean(.x, na.rm=TRUE))) %>% 
  ungroup() %>% 
  left_join(lookup, by = "Plot") %>% 
  mutate(H2OTmt = substr(Tmt,2,2), CO2Tmt = substr(Tmt,1,1)) 
# for plot 3, L has n = 1, but measurements for all data types
# assign it a weight based on the variance of all other points from its treatment group (AD):

biomass2. %>% 
  filter(Spp =="L") %>% 
  dplyr::select(Plot, Spp, Code, rootshoot, CO2, meanSWC) %>% 
  left_join(LiCOR_df.[,c("Code","Photo.y","Cond.y")], by = "Code") %>% 
  left_join(rootimage.[,c("Code","SRL")], by = "Code") %>% 
  mutate(logCond.y = log(Cond.y), logSRL = log(SRL)) %>%
  left_join(lookup, by = "Plot") %>% 
  filter(Tmt == "AD") %>% # get 1/var
  summarise_at(c("Photo.y", "rootshoot", "logCond.y", "logSRL"), ~ 1/sd(.x, na.rm = T)^2)
# add this to 1/var columns in row for plot 3, then do next step to scale 1/var and get weights

#manova.df.small0[manova.df.small0$Plot=="3",12:15] <- list(0.02501195,  30.50882,  1.673602, 4.632909)
# only do this for L !!

manova.df.small <- manova.df.small0 %>% 
  mutate(Photo.y_w = Photo.y_oov/(sum(manova.df.small0$Photo.y_oov, na.rm = T)),
         logCond.y_w = logCond.y_oov/(sum(manova.df.small0$logCond.y_oov, na.rm = T)),
         rootshoot_w = rootshoot_oov/(sum(manova.df.small0$rootshoot_oov, na.rm = T) ),
         logSRL_w = logSRL_oov/(sum(manova.df.small0$logSRL_oov, na.rm = T))) %>% 
  mutate(all_w = Photo.y_w+logCond.y_w+rootshoot_w+logSRL_w) %>% 
  mutate(all_w = all_w/sum(all_w, na.rm=T)) %>% 
  mutate(quad_w = sqrt((Photo.y_w^2)+(logCond.y_w^2)+(rootshoot_w^2)+(logSRL_w^2)) ) %>% 
  mutate(quad_w = quad_w/sum(quad_w, na.rm=T)) 

small.cbind <- cbind(unlist(manova.df.small$Photo.y_w),unlist(manova.df.small$logCond.y_w),unlist(manova.df.small$rootshoot),unlist(manova.df.small$logSRL_w))

# the weights come from inverse variance; check for outliers
manova.df.small00 <- biomass2. %>% 
  filter(Spp =="L") %>% 
  dplyr::select(Plot, Spp, Code, rootshoot, CO2, meanSWC) %>% 
  left_join(LiCOR_df.[,c("Code","Photo.y","Cond.y")], by = "Code") %>% 
  left_join(rootimage.[,c("Code","SRL")], by = "Code") %>% 
  mutate(logCond.y = log(Cond.y), logSRL = log(SRL)) %>% 
  left_join(lookup, by = "Plot") %>% 
  mutate(H2OTmt = substr(Tmt,2,2), CO2Tmt = substr(Tmt,1,1)) 
ggplot(manova.df.small00) +
  geom_point(aes(x=Plot, y=Photo.y, color=Tmt)) + 
  scale_color_manual(values = c("pink", "lightblue", "red", "blue"))

# first try the weights in an lm, with plot means, for V:
summary(lm(Photo.y ~ CO2*H2OTmt , data = manova.df.small, weights = Photo.y_w)) # ** for CO2, ** for water, . for intx
summary(lm(Photo.y ~ CO2*H2OTmt , data = manova.df.small)) # unweighted, NS
summary(lm(Photo.y ~ CO2*H2OTmt , data = manova.df.small, weights = all_w)) # combined weight, NS
summary(lm(Photo.y ~ CO2*H2OTmt , data = manova.df.small, weights = quad_w)) # quadrature NS

summary(lm(logCond.y ~ CO2*H2OTmt , data = manova.df.small, weights = logCond.y_w)) # NS
summary(lm(logCond.y ~ CO2*H2OTmt , data = manova.df.small)) # **, **, *
summary(lm(logCond.y ~ CO2*H2OTmt , data = manova.df.small, weights = Photo.y_w)) # CO2***, H2O***, intx*
summary(lm(logCond.y ~ CO2*H2OTmt , data = manova.df.small, weights = all_w)) # *, **, *
summary(lm(logCond.y ~ CO2*H2OTmt , data = manova.df.small, weights = quad_w)) # *, **, *

summary(lm(rootshoot ~ CO2*H2OTmt , data = manova.df.small, weights = rootshoot_w)) # CO2 .
summary(lm(rootshoot ~ CO2*H2OTmt , data = manova.df.small, weights = Photo.y_w)) # CO2 **
summary(lm(rootshoot ~ CO2*H2OTmt , data = manova.df.small, weights = all_w)) # CO2 .
summary(lm(rootshoot ~ CO2*H2OTmt , data = manova.df.small, weights = quad_w)) # CO2 *

summary(lm(logSRL ~ CO2*H2OTmt , data = manova.df.small, weights = logSRL_w)) # NS
summary(lm(logSRL ~ CO2*H2OTmt , data = manova.df.small, weights = Photo.y_w)) # NS
summary(lm(logSRL ~ CO2*H2OTmt , data = manova.df.small, weights = all_w)) # NS
summary(lm(logSRL ~ CO2*H2OTmt , data = manova.df.small, weights = quad_w)) # NS

# reasonable subset: Photo.y, logCond.y, sqrt_totmass, rootshoot
summary(manova(small.cbind ~ CO2Tmt*H2OTmt, manova.df.small, weights = Photo.y_w))
# with weights from just Photo.y_w, all p < .05
summary(manova(small.cbind ~ CO2Tmt*H2OTmt, manova.df.small, weights = all_w)) 
# with combined weights, * *** **
summary(manova(small.cbind ~ CO2Tmt*H2OTmt, manova.df.small, weights = quad_w)) # **, ***, ***

## now for fun, do ggpredict with weighted model, and with weighted model and outlier filter
ggpredict(lm(Photo.y ~ CO2*H2OTmt , data = manova.df.small, weights = Photo.y_w), terms=c("CO2","H2OTmt")) %>% 
  plot(add.data = TRUE, ci = TRUE)
ggpredict(lm(Photo.y ~ CO2*H2OTmt , data = manova.df.small, weights = quad_w), terms=c("CO2","H2OTmt")) %>% 
  plot(add.data = TRUE, ci = TRUE)


# now for L: (HAVE TO RE RUN manova.df.small)
summary(lm(Photo.y ~ CO2*H2OTmt , data = manova.df.small, weights = Photo.y_w)) # *** for CO2, ** for water, * for intx
summary(lm(Photo.y ~ CO2*H2OTmt , data = manova.df.small)) # unweighted, NS

summary(lm(logCond.y ~ CO2*H2OTmt , data = manova.df.small, weights = logCond.y_w)) # NS
summary(lm(logCond.y ~ CO2*H2OTmt , data = manova.df.small)) # NS
summary(lm(logCond.y ~ CO2*H2OTmt , data = manova.df.small, weights = Photo.y_w)) # CO2***, H2O*, intxNS

summary(lm(rootshoot ~ CO2*H2OTmt , data = manova.df.small, weights = rootshoot_w)) # CO2 *
summary(lm(rootshoot ~ CO2*H2OTmt , data = manova.df.small, weights = Photo.y_w)) # CO2 ***

summary(lm(logSRL ~ CO2*H2OTmt , data = manova.df.small, weights = logSRL_w)) # CO2 .
summary(lm(logSRL ~ CO2*H2OTmt , data = manova.df.small, weights = Photo.y_w)) # CO2***, H2O., intx**

# reasonable subset: Photo.y, logCond.y, sqrt_totmass, rootshoot
summary(manova(small.cbind ~ CO2Tmt*H2OTmt, manova.df.small, weights = Photo.y_w))
# with weights from just Photo.y_w, all p < .05

# comparison plots for different models
grid.arrange(
  +   ggpredict(lm(logCond.y ~ CO2*H2OTmt , data = trans_dfwV, weights = logCond.y_w), terms=c("CO2","H2OTmt")) %>% 
    +     plot(rawdata = TRUE, ci = TRUE),
  +   ggpredict(lm(logCond.y ~ CO2*H2OTmt , data = trans_dfwV, weights = all_w), terms=c("CO2","H2OTmt")) %>% 
    +     plot(rawdata = TRUE, ci = TRUE), ggpredict(lm(logCond.y ~ CO2*H2OTmt , data = trans_dfwV), terms=c("CO2","H2OTmt")) %>% 
    +       plot(rawdata = TRUE, ci = TRUE),
  +   ggplot(trans_dfwV, aes(x=CO2, y=logCond.y, group = H2OTmt)) + geom_point(aes(group = H2OTmt, color = H2OTmt)), nrow=4 )

> grid.arrange(
  +   ggpredict(lm(Photo.y ~ CO2*H2OTmt , data = trans_dfwV, weights = Photo.y_w), terms=c("CO2","H2OTmt")) %>% 
    +     plot(rawdata = TRUE, ci = TRUE),
  +   ggpredict(lm(Photo.y ~ CO2*H2OTmt , data = trans_dfwV, weights = all_w), terms=c("CO2","H2OTmt")) %>% 
    +     plot(rawdata = TRUE, ci = TRUE), ggpredict(lm(Photo.y ~ CO2*H2OTmt , data = trans_dfwV), terms=c("CO2","H2OTmt")) %>% 
    +       plot(rawdata = TRUE, ci = TRUE),
  +   ggplot(trans_dfwV, aes(x=CO2, y=logCond.y, group = H2OTmt)) + geom_point(aes(group = H2OTmt, color = H2OTmt)), nrow = 4 )

## clean copy of weighting calculation before trying pooled variance for LiCOR measurements by tmt group
### combine into one giant, all-inclusive, transformed dataset
# one_over_var <- function(x){
#   1/(sd(x, na.rm=TRUE)^2)
# }
one_over_se <- function(x){
  1/(sd(x, na.rm=TRUE)/sqrt(length(x[!is.na(x)])))
}

trans_df <- biomass2. %>% 
  dplyr::select(Plot, Spp, Code, totmass, rootshoot, lwc, CO2, meanSWC) %>% 
  left_join(LiCOR_df.[,c("Code","Photo.y","Cond.y", "WUE.350")], by = "Code") %>% 
  left_join(lai.[,c("Code","avg_area","perim_per_A","tot_area")], by = "Code") %>% 
  left_join(rootimage.[,c("Code","SRL")], by = "Code") %>% 
  left_join(SIF.[,c("Code","d13C")], by = "Code") %>% 
  mutate(logCond.y = log(Cond.y), sqrtWUE = sqrt(WUE.350), sqrtTotmass = sqrt(totmass), logAvg_area = log(avg_area), logPerimArea = log(perim_per_A), sqrtTotArea = sqrt(tot_area), logSRL = log(SRL)) %>%
  group_by(Plot, Spp) %>% 
  dplyr::mutate(n = n()) %>% 
  mutate_at(c("sqrtTotmass", "rootshoot", "lwc", "Photo.y", "logCond.y", "sqrtWUE", "logAvg_area", "logPerimArea", "sqrtTotArea", "logSRL", "d13C"), list(oov=one_over_se)) %>%
  dplyr::summarise(across(where(is.numeric), ~ mean(.x, na.rm=TRUE))) %>% 
  ungroup() %>% 
  left_join(lookup, by = "Plot") %>% 
  mutate(H2OTmt = substr(Tmt,2,2), CO2Tmt = substr(Tmt,1,1))
# for plot 3, L has n = 1, but measurements for all data types. Same for plot 16, V
# assign it a weight based on the variance of all other points from its treatment group (AD):

plot3L_vars <- biomass2. %>% 
  filter(Spp =="L") %>% 
  dplyr::select(Plot, Spp, Code, totmass, rootshoot, lwc, CO2, meanSWC) %>% 
  left_join(LiCOR_df.[,c("Code","Photo.y","Cond.y", "WUE.350")], by = "Code") %>% 
  left_join(lai.[,c("Code","avg_area","perim_per_A","tot_area")], by = "Code") %>% 
  left_join(rootimage.[,c("Code","SRL")], by = "Code") %>% 
  left_join(SIF.[,c("Code","d13C")], by = "Code") %>% 
  mutate(logCond.y = log(Cond.y), sqrtWUE = sqrt(WUE.350), sqrtTotmass = sqrt(totmass), logAvg_area = log(avg_area), logPerimArea = log(perim_per_A), sqrtTotArea = sqrt(tot_area), logSRL = log(SRL)) %>%
  group_by(Plot, Spp) %>% 
  mutate(n = n()) %>% 
  ungroup() %>% 
  left_join(lookup, by = "Plot") %>% 
  filter(Tmt == "AD") %>% # get 1/var
  summarise_at(c("sqrtTotmass", "rootshoot", "lwc", "Photo.y", "logCond.y", "sqrtWUE", "logAvg_area", "logPerimArea", "sqrtTotArea", "logSRL", "d13C"), list(one_over_se)) %>% 
  unlist()
# add this to 1/var columns in row for plot 3, then do next step to scale 1/var and get weights
# trans_df[trans_df$Plot=="3" & trans_df$Spp=="L",24:34] <- list(6.663002e-01, 2.655873e+01, 3.921653e-02, 3.963671e-02, 2.706536e+00, 9.376510e-02, 1.648682e+01, 3.025422e+01, 5.375108e-04, 4.618791e+00, 9.799790e+00)
trans_df[trans_df$Plot=="3" & trans_df$Spp=="L",24:34] <- list(3.65400837, 20.66696664,  0.97963889,  0.54785341,  4.48143137,  1.00380582,  8.66588962, 12.88699028,  0.09953649, 8.33628397,  3.89290435)

# same for plot 16, V
plot16V_vars <- biomass2. %>% 
  filter(Spp =="V") %>% 
  dplyr::select(Plot, Spp, Code, totmass, rootshoot, lwc, CO2, meanSWC) %>% 
  left_join(LiCOR_df.[,c("Code","Photo.y","Cond.y", "WUE.350")], by = "Code") %>% 
  left_join(lai.[,c("Code","avg_area","perim_per_A","tot_area")], by = "Code") %>% 
  left_join(rootimage.[,c("Code","SRL")], by = "Code") %>% 
  left_join(SIF.[,c("Code","d13C")], by = "Code") %>% 
  mutate(logCond.y = log(Cond.y), sqrtWUE = sqrt(WUE.350), sqrtTotmass = sqrt(totmass), logAvg_area = log(avg_area), logPerimArea = log(perim_per_A), sqrtTotArea = sqrt(tot_area), logSRL = log(SRL)) %>%
  group_by(Plot, Spp) %>% 
  mutate(n = n()) %>% 
  ungroup() %>% 
  left_join(lookup, by = "Plot") %>% 
  filter(Tmt == "AD") %>% # get 1/var
  summarise_at(c("sqrtTotmass", "rootshoot", "lwc", "Photo.y", "logCond.y", "sqrtWUE", "logAvg_area", "logPerimArea", "sqrtTotArea", "logSRL", "d13C"), list(one_over_se)) %>% 
  unlist()

# trans_df[trans_df$Plot=="16" & trans_df$Spp=="V",24:34] <- list(9.917998e+00, 8.742674e-01, 7.279417e-03, 7.063863e-02, 1.519038e+02, 1.883699e-01, 1.258911e+00, 5.279206e+00, 6.309727e-03, 1.165876e+02, 1.134142e+01)
trans_df[trans_df$Plot=="16" & trans_df$Spp=="V",24:34] <- list(9.5139324, 2.8393556, 0.2239555, 0.3804126, 7.7273218, 0.7254999, 2.3770735, 5.6281224, 0.1268094, 9.2005139, 4.2598168)

# need to calculate the weights AFTER filtering for species
# now add the weights; note that the combined weights will include variables not in the final model
## !!! trying 1/se to see if it fixes the weighting issue; "oov" is now "one over SE", will rename if it works
## addendum; 1/se is better but I also want to pool Tmt groups for LiCOR variables
trans_dfwV <- trans_df %>% 
  filter(Spp=="V") %>% 
  mutate(sqrtTotmass_w = sqrtTotmass_oov/(sum(trans_df$sqrtTotmass_oov, na.rm = T)),
         rootshoot_w = rootshoot_oov/(sum(trans_df$rootshoot_oov, na.rm = T)),
         lwc_w = lwc_oov/(sum(trans_df$lwc_oov, na.rm = T)),
         Photo.y_w = Photo.y_oov/(sum(trans_df$Photo.y_oov, na.rm = T)),
         logCond.y_w = logCond.y_oov/(sum(trans_df$logCond.y_oov, na.rm = T)),
         sqrtWUE_w = sqrtWUE_oov/(sum(trans_df$sqrtWUE_oov, na.rm =T)),
         logAvg_area_w = logAvg_area_oov/(sum(trans_df$logAvg_area_oov, na.rm = T)),
         logPerimArea_w = logPerimArea_oov/(sum(trans_df$logPerimArea_oov, na.rm = T)),
         sqrtTotArea_w = sqrtTotArea_oov/(sum(trans_df$sqrtTotArea_oov, na.rm = T)),
         logSRL_w = logSRL_oov/(sum(trans_df$logSRL_oov, na.rm = T)),
         d13C_w = d13C_oov/sum(trans_df$d13C_oov, na.rm = T)) %>% 
  mutate(all_w = sqrtTotmass_w+rootshoot_w+lwc_w+Photo.y_w+logCond.y_w+sqrtWUE_w+logAvg_area_w+logPerimArea_w+sqrtTotArea_w+logSRL_w+d13C_w) %>% 
  mutate(all_w = all_w/sum(all_w, na.rm=T)) %>% 
  mutate(quad_w = (sqrtTotmass_w^2)+(rootshoot_w^2)+(lwc_w^2)+(Photo.y_w^2)+(logCond.y_w^2)+(sqrtWUE_w^2)+(logAvg_area_w^2)+(logPerimArea_w^2)+(sqrtTotArea_w^2)+(logSRL_w^2)+(d13C_w^2)) %>% 
  mutate(quad_w = quad_w/sum(quad_w, na.rm=T)) 

trans_dfwL <- trans_df %>% 
  filter(Spp=="L") %>% 
  mutate(sqrtTotmass_w = sqrtTotmass_oov/(sum(trans_df$sqrtTotmass_oov, na.rm = T)),
         rootshoot_w = rootshoot_oov/(sum(trans_df$rootshoot_oov, na.rm = T)),
         lwc_w = lwc_oov/(sum(trans_df$lwc_oov, na.rm = T)),
         Photo.y_w = Photo.y_oov/(sum(trans_df$Photo.y_oov, na.rm = T)),
         logCond.y_w = logCond.y_oov/(sum(trans_df$logCond.y_oov, na.rm = T)),
         sqrtWUE_w = sqrtWUE_oov/(sum(trans_df$sqrtWUE_oov, na.rm =T)),
         logAvg_area_w = logAvg_area_oov/(sum(trans_df$logAvg_area_oov, na.rm = T)),
         logPerimArea_w = logPerimArea_oov/(sum(trans_df$logPerimArea_oov, na.rm = T)),
         sqrtTotArea_w = sqrtTotArea_oov/(sum(trans_df$sqrtTotArea_oov, na.rm = T)),
         logSRL_w = logSRL_oov/(sum(trans_df$logSRL_oov, na.rm = T)),
         d13C_w = d13C_oov/sum(trans_df$d13C_oov, na.rm = T)) %>% 
  mutate(all_w = sqrtTotmass_w+rootshoot_w+lwc_w+Photo.y_w+logCond.y_w+sqrtWUE_w+logAvg_area_w+logPerimArea_w+sqrtTotArea_w+logSRL_w+d13C_w) %>% 
  mutate(all_w = all_w/sum(all_w, na.rm=T)) %>% 
  mutate(quad_w = (sqrtTotmass_w^2)+(rootshoot_w^2)+(lwc_w^2)+(Photo.y_w^2)+(logCond.y_w^2)+(sqrtWUE_w^2)+(logAvg_area_w^2)+(logPerimArea_w^2)+(sqrtTotArea_w^2)+(logSRL_w^2)+(d13C_w^2)) %>% 
  mutate(quad_w = quad_w/sum(quad_w, na.rm=T)) 

# check correlation between variables
cor(trans_dfwV[, c("sqrtTotmass", "rootshoot", "lwc", "Photo.y", "logCond.y", "sqrtWUE", "logAvg_area", "logPerimArea", "sqrtTotArea", "logSRL", "d13C")], use="pairwise.complete.obs") %>% 
  ggcorrplot()

# I heard a MANOVA has more power, so long as your data is normally distributed, which I've already ensured
# now doing manova: transformed data, plot means
# 5/19 add normalized 1/variance term for weighted plot means
## filter out SRL > 1500: what is that data point doing?
## add n = to plot means, this may account for exceptionally low variance in cases where n = 1
## go undo seedlings you took off the herbivory list :| (see if there's still enough data left to work with)

# add the other seven variables in order here
trans_dfwV.cbind <- cbind(unlist(trans_dfwV$Photo.y_w),unlist(trans_dfwV$logCond.y_w),unlist(trans_dfwV$rootshoot),unlist(trans_dfwV$logSRL_w))

# the weights come from inverse variance; check for outliers # do this tomorrow
trans_df_full <- biomass2. %>% 
  #  filter(Spp =="V") %>% 
  dplyr::select(Plot, Spp, Code, totmass, rootshoot, lwc, CO2, meanSWC) %>% 
  left_join(LiCOR_df.[,c("Code","Photo.y","Cond.y", "WUE.350")], by = "Code") %>% 
  left_join(lai.[,c("Code","avg_area","perim_per_A","tot_area")], by = "Code") %>% 
  left_join(rootimage.[,c("Code","SRL")], by = "Code") %>% 
  left_join(SIF.[,c("Code","d13C")], by = "Code") %>% 
  mutate(logCond.y = log(Cond.y), sqrtWUE = sqrt(WUE.350), sqrtTotmass = sqrt(totmass), logAvg_area = log(avg_area), logPerimArea = log(perim_per_A), sqrtTotArea = sqrt(tot_area), logSRL = log(SRL)) %>%
  group_by(Plot, Spp) %>% 
  mutate(n = n()) %>% 
  left_join(lookup, by = "Plot") %>% 
  mutate(H2OTmt = substr(Tmt,2,2), CO2Tmt = substr(Tmt,1,1)) 
trans_df_full %>% filter(Spp=="V") %>% 
  ggplot() +
  geom_point(aes(x=Plot, y=logCond.y, color=Tmt)) + 
  scale_color_manual(values = c("pink", "lightblue", "red", "blue")) +
  labs(title = "Individual Responses per Plot: Stomatal Conductance")

# first try the weights in an lm, with plot means, for V:
# REDO the *s
summary(lmer(Photo.y ~ CO2*H2OTmt + (1|Plot), data = trans_dfwV, weights = Photo.y_w)) # ** for CO2, ** for water, . for intx
summary(lmer(Photo.y ~ CO2*H2OTmt + (1|Plot), data = trans_dfwV)) # unweighted, NS
summary(lmer(Photo.y ~ CO2*H2OTmt + (1|Plot), data = trans_dfwV, weights = all_w)) # combined weight, NS
summary(lmer(Photo.y ~ CO2*H2OTmt + (1|Plot), data = trans_dfwV, weights = quad_w)) # quadrature NS

summary(lm(logCond.y ~ CO2*H2OTmt , data = trans_dfwV, weights = logCond.y_w)) # NS
summary(lm(logCond.y ~ CO2*H2OTmt , data = trans_dfwV)) # **, **, *
summary(lm(logCond.y ~ CO2*H2OTmt , data = trans_dfwV, weights = Photo.y_w)) # CO2***, H2O***, intx*
summary(lm(logCond.y ~ CO2*H2OTmt , data = trans_dfwV, weights = all_w)) # *, **, *
summary(lm(logCond.y ~ CO2*H2OTmt , data = trans_dfwV, weights = quad_w)) # *, **, *

summary(lm(rootshoot ~ CO2*H2OTmt , data = trans_dfwV, weights = rootshoot_w)) # CO2 .
summary(lm(rootshoot ~ CO2*H2OTmt , data = trans_dfwV, weights = Photo.y_w)) # CO2 **
summary(lm(rootshoot ~ CO2*H2OTmt , data = trans_dfwV, weights = all_w)) # CO2 .
summary(lm(rootshoot ~ CO2*H2OTmt , data = trans_dfwV, weights = quad_w)) # CO2 *

summary(lm(logSRL ~ CO2*H2OTmt , data = trans_dfwV, weights = logSRL_w)) # NS
summary(lm(logSRL ~ CO2*H2OTmt , data = trans_dfwV, weights = Photo.y_w)) # NS
summary(lm(logSRL ~ CO2*H2OTmt , data = trans_dfwV, weights = all_w)) # NS
summary(lm(logSRL ~ CO2*H2OTmt , data = trans_dfwV, weights = quad_w)) # NS

# reasonable subset: Photo.y, logCond.y, sqrt_totmass, rootshoot
summary(manova(trans_dfwV.cbind ~ CO2Tmt*H2OTmt, trans_dfwV, weights = Photo.y_w))
# with weights from just Photo.y_w, all p < .05
summary(manova(trans_dfwV.cbind ~ CO2Tmt*H2OTmt, trans_dfwV, weights = all_w)) 
# with combined weights, * *** **
summary(manova(trans_dfwV.cbind ~ CO2Tmt*H2OTmt, trans_dfwV, weights = quad_w)) # **, ***, ***

## now for fun, do ggpredict with weighted model, and with weighted model and outlier filter
ggpredict(lm(Photo.y ~ CO2*H2OTmt, data = trans_dfwV, weights = Photo.y_w), terms=c("CO2","H2OTmt")) %>% 
  plot(add.data = TRUE, ci = TRUE)

grid.arrange(
  ggpredict(lm(Photo.y ~ CO2*H2OTmt , data = trans_dfwV, weights = Photo.y_w), terms=c("CO2","H2OTmt")) %>% 
    plot(rawdata = TRUE, ci = TRUE, colors=c("blue","red")) + labs(title = "A(net) weights"),
  ggpredict(lm(Photo.y ~ CO2*H2OTmt , data = trans_dfwV), terms=c("CO2","H2OTmt")) %>% 
    plot(rawdata = TRUE, ci = TRUE, colors=c("blue","red")) + labs(title = "Unweighted A(net)"),
  ggplot(trans_dfwV, aes(x=CO2, y=Photo.y, group = H2OTmt)) + geom_point(aes(group = H2OTmt, color = H2OTmt, size = Photo.y_w)) + scale_color_manual(values = c("red", "blue")) + labs(title = "A(net) Plot Means Sized by Inverse-SE Weight"), nrow=3 )

grid.arrange(
  ggpredict(lm(logCond.y ~ CO2*H2OTmt , data=trans_dfwV, weights=logCond.y_w), terms=c("CO2","H2OTmt")) %>% 
    plot(rawdata = TRUE, ci = TRUE, colors=c("blue","red")) + labs(title = "g(s) weights"),
  ggpredict(lm(logCond.y ~ CO2*H2OTmt , data = trans_dfwV), terms=c("CO2","H2OTmt")) %>% 
    plot(rawdata = TRUE, ci = TRUE, colors=c("blue","red"))+ labs(title = "Unweighted g(s)"),
  ggplot(trans_dfwV, aes(x=CO2, y=logCond.y, group = H2OTmt)) + geom_point(aes(group = H2OTmt, color = H2OTmt, size=logCond.y_w)) + scale_color_manual(values = c("red", "blue")) + labs(title = "g(s) Plot Means Sized by Inverse-SE Weight"), nrow=3 )

grid.arrange(
  ggpredict(lm(sqrtTotmass~CO2*H2OTmt , data=trans_dfwV, weights=sqrtTotmass_w), terms=c("CO2","H2OTmt"))%>% 
    plot(rawdata = TRUE, ci = TRUE, colors=c("blue","red")),
  ggpredict(lm(sqrtTotmass ~ CO2*H2OTmt , data = trans_dfwV), terms=c("CO2","H2OTmt")) %>% 
    plot(rawdata = TRUE, ci = TRUE, colors=c("blue","red")),
  ggplot(trans_dfwV, aes(x=CO2, y=sqrtTotmass, group = H2OTmt)) + geom_point(aes(group = H2OTmt, color = H2OTmt, size=sqrtTotmass_w)) + scale_color_manual(values = c("red", "blue")) , nrow=3 )



one_plot <- function(data1, thingy){
  #thingy <- eval(as.symbol(thingy))
  ggpredict(lm(eval(as.symbol(thingy)) ~ CO2*H2OTmt , data = data1), terms=c("CO2","H2OTmt")) %>% 
    plot(rawdata = TRUE, ci = TRUE, colors=c("blue","red")) + labs(title = paste0(thingy, " no weights"))
}
# one_plot <- function(data1, thingy){
#   #thingy <- eval(as.symbol(thingy))
#   plot(ggpredict(lm(eval(as.name(thingy)) ~ CO2*H2OTmt , data = data1), terms=c("CO2","H2OTmt")), rawdata = TRUE, ci = TRUE, colors=c("blue","red")) + labs(title = paste0(thingy, " no weights"))
# }
one_plot(trans_dfwV, "logCond.y")

# now for L: (HAVE TO RE RUN manova.df.small)
summary(lm(Photo.y ~ CO2*H2OTmt , data = manova.df.small, weights = Photo.y_w)) # *** for CO2, ** for water, * for intx
summary(lm(Photo.y ~ CO2*H2OTmt , data = manova.df.small)) # unweighted, NS

summary(lm(logCond.y ~ CO2*H2OTmt , data = manova.df.small, weights = logCond.y_w)) # NS
summary(lm(logCond.y ~ CO2*H2OTmt , data = manova.df.small)) # NS
summary(lm(logCond.y ~ CO2*H2OTmt , data = manova.df.small, weights = Photo.y_w)) # CO2***, H2O*, intxNS

summary(lm(rootshoot ~ CO2*H2OTmt , data = manova.df.small, weights = rootshoot_w)) # CO2 *
summary(lm(rootshoot ~ CO2*H2OTmt , data = manova.df.small, weights = Photo.y_w)) # CO2 ***

summary(lm(logSRL ~ CO2*H2OTmt , data = manova.df.small, weights = logSRL_w)) # CO2 .
summary(lm(logSRL ~ CO2*H2OTmt , data = manova.df.small, weights = Photo.y_w)) # CO2***, H2O., intx**

# first try the weights in an lm, with plot means, for V:
# REDO the *s
summary(lmer(Photo.y ~ CO2*H2OTmt + (1|Plot), data = trans_dfwV, weights = Photo.y_w)) # ** for CO2, ** for water, . for intx
summary(lmer(Photo.y ~ CO2*H2OTmt + (1|Plot), data = trans_dfwV)) # unweighted, NS
summary(lmer(Photo.y ~ CO2*H2OTmt + (1|Plot), data = trans_dfwV, weights = all_w)) # combined weight, NS
summary(lmer(Photo.y ~ CO2*H2OTmt + (1|Plot), data = trans_dfwV, weights = quad_w)) # quadrature NS

summary(lm(logCond.y ~ CO2*H2OTmt , data = trans_dfwV, weights = logCond.y_w)) # NS
summary(lm(logCond.y ~ CO2*H2OTmt , data = trans_dfwV)) # **, **, *
summary(lm(logCond.y ~ CO2*H2OTmt , data = trans_dfwV, weights = Photo.y_w)) # CO2***, H2O***, intx*
summary(lm(logCond.y ~ CO2*H2OTmt , data = trans_dfwV, weights = all_w)) # *, **, *
summary(lm(logCond.y ~ CO2*H2OTmt , data = trans_dfwV, weights = quad_w)) # *, **, *

summary(lm(rootshoot ~ CO2*H2OTmt , data = trans_dfwV, weights = rootshoot_w)) # CO2 .
summary(lm(rootshoot ~ CO2*H2OTmt , data = trans_dfwV, weights = Photo.y_w)) # CO2 **
summary(lm(rootshoot ~ CO2*H2OTmt , data = trans_dfwV, weights = all_w)) # CO2 .
summary(lm(rootshoot ~ CO2*H2OTmt , data = trans_dfwV, weights = quad_w)) # CO2 *

summary(lm(logSRL ~ CO2*H2OTmt , data = trans_dfwV, weights = logSRL_w)) # NS
summary(lm(logSRL ~ CO2*H2OTmt , data = trans_dfwV, weights = Photo.y_w)) # NS
summary(lm(logSRL ~ CO2*H2OTmt , data = trans_dfwV, weights = all_w)) # NS
summary(lm(logSRL ~ CO2*H2OTmt , data = trans_dfwV, weights = quad_w)) # NS

# reasonable subset: Photo.y, logCond.y, sqrt_totmass, rootshoot
summary(manova(trans_dfwV.cbind ~ CO2Tmt*H2OTmt, trans_dfwV, weights = Photo.y_w))
# with weights from just Photo.y_w, all p < .05
summary(manova(trans_dfwV.cbind ~ CO2Tmt*H2OTmt, trans_dfwV, weights = all_w)) 
# with combined weights, * *** **
summary(manova(trans_dfwV.cbind ~ CO2Tmt*H2OTmt, trans_dfwV, weights = quad_w)) # **, ***, ***

## now for fun, do ggpredict with weighted model, and with weighted model and outlier filter
ggpredict(lm(Photo.y ~ CO2*H2OTmt, data = trans_dfwV, weights = Photo.y_w), terms=c("CO2","H2OTmt")) %>% 
  plot(add.data = TRUE, ci = TRUE)

## here I am trying one (last?) attempt at significance, by using the continuous variables for both CO2 and water, and/or by using continuous water and categorical CO2
library(scales)
## Spp == V, both continuous
grid.arrange( # total mass
  ggpredict(lm(sqrtTotmass~rescale(CO2)*rescale(meanSWC) , data=trans_dfwV, weights=sqrtTotmass_w), terms=c("CO2","meanSWC [4,42]"))%>% 
    plot(rawdata = TRUE, ci = TRUE, colors=c("red","blue")),
  ggpredict(lm(sqrtTotmass ~ rescale(CO2)*rescale(meanSWC) , data = trans_dfwV), terms=c("CO2","meanSWC [4,42]")) %>% 
    plot(rawdata = TRUE, ci = TRUE, colors=c("red","blue")),
  ggplot(trans_dfwV, aes(x=rescale(CO2), y=sqrtTotmass, group = rescale(meanSWC))) + geom_point(aes(group = rescale(meanSWC), color = rescale(meanSWC), size=sqrtTotmass_w)) , nrow=3 )
summary(lm(sqrtTotmass~rescale(CO2)*rescale(meanSWC) , data=trans_dfwV, weights=sqrtTotmass_w)) # SWC . (.058)

grid.arrange( # root:shoot
  ggpredict(lm(rootshoot~rescale(CO2)*rescale(meanSWC) , data=trans_dfwV, weights=rootshoot_w), terms=c("CO2","meanSWC [4,42]"))%>% 
    plot(rawdata = TRUE, ci = TRUE, colors=c("red","blue")),
  ggpredict(lm(rootshoot ~ rescale(CO2)*rescale(meanSWC) , data = trans_dfwV), terms=c("CO2","meanSWC [4,42]")) %>% 
    plot(rawdata = TRUE, ci = TRUE, colors=c("red","blue")),
  ggplot(trans_dfwV, aes(x=rescale(CO2), y=rootshoot, group = rescale(meanSWC))) + geom_point(aes(group = rescale(meanSWC), color = rescale(meanSWC), size=rootshoot_w)) , nrow=3 )
summary(lm(rootshoot~rescale(CO2)*rescale(meanSWC) , data=trans_dfwV, weights=rootshoot_w)) # NS

grid.arrange( # lwc
  ggpredict(lm(lwc~rescale(CO2)*rescale(meanSWC) , data=trans_dfwV, weights=lwc_w), terms=c("CO2","meanSWC [4,42]"))%>% 
    plot(rawdata = TRUE, ci = TRUE, colors=c("red","blue")),
  ggpredict(lm(lwc ~ rescale(CO2)*rescale(meanSWC) , data = trans_dfwV), terms=c("CO2","meanSWC [4,42]")) %>% 
    plot(rawdata = TRUE, ci = TRUE, colors=c("red","blue")),
  ggplot(trans_dfwV, aes(x=rescale(CO2), y=lwc, group = rescale(meanSWC))) + geom_point(aes(group = rescale(meanSWC), color = rescale(meanSWC), size=lwc_w)) , nrow=3 )
summary(lm(lwc~rescale(CO2)*rescale(meanSWC) , data=trans_dfwV, weights=lwc_w)) # SWC . (.079)

grid.arrange( # logAvg_area
  ggpredict(lm(logAvg_area~rescale(CO2)*rescale(meanSWC) , data=trans_dfwV, weights=logAvg_area_w), terms=c("CO2","meanSWC [4,42]"))%>% 
    plot(rawdata = TRUE, ci = TRUE, colors=c("red","blue")),
  ggpredict(lm(logAvg_area ~ rescale(CO2)*rescale(meanSWC) , data = trans_dfwV), terms=c("CO2","meanSWC [4,42]")) %>% 
    plot(rawdata = TRUE, ci = TRUE, colors=c("red","blue")),
  ggplot(trans_dfwV, aes(x=rescale(CO2), y=logAvg_area, group = rescale(meanSWC))) + geom_point(aes(group = rescale(meanSWC), color = rescale(meanSWC), size=logAvg_area_w)) + geom_text(data = trans_dfwV, aes(x=rescale(CO2), y=logAvg_area+.2, label=n)), nrow=3 )
summary(lm(logAvg_area~rescale(CO2)*rescale(meanSWC) , data=trans_dfwV, weights=logAvg_area_w))
# log-transformed average leaf area: CO2*** (.0058), SWC* (.0128), intx* (.019)

grid.arrange( # logPerimArea
  ggpredict(lm(logPerimArea~rescale(CO2)*rescale(meanSWC) , data=trans_dfwV, weights=logPerimArea_w), terms=c("CO2","meanSWC [4,42]"))%>% 
    plot(rawdata = TRUE, ci = TRUE, colors=c("red","blue")),
  ggpredict(lm(logPerimArea ~ rescale(CO2)*rescale(meanSWC) , data = trans_dfwV), terms=c("CO2","meanSWC [4,42]")) %>% 
    plot(rawdata = TRUE, ci = TRUE, colors=c("red","blue")),
  ggplot(trans_dfwV, aes(x=rescale(CO2), y=logPerimArea, group = rescale(meanSWC))) + geom_point(aes(group = rescale(meanSWC), color = rescale(meanSWC), size=logPerimArea_w)) , nrow=3 )
summary(lm(logPerimArea~rescale(CO2)*rescale(meanSWC) , data=trans_dfwV, weights=logPerimArea_w))
# log-transformed perimeter-per-area: CO2 p < 0.0555

grid.arrange( # sqrtTotArea
  ggpredict(lm(sqrtTotArea~rescale(CO2)*rescale(meanSWC) , data=trans_dfwV, weights=sqrtTotArea_w), terms=c("CO2","meanSWC [4,42]"))%>% 
    plot(rawdata = TRUE, ci = TRUE, colors=c("red","blue")),
  ggpredict(lm(sqrtTotArea ~ rescale(CO2)*rescale(meanSWC) , data = trans_dfwV), terms=c("CO2","meanSWC [4,42]")) %>% 
    plot(rawdata = TRUE, ci = TRUE, colors=c("red","blue")),
  ggplot(trans_dfwV, aes(x=rescale(CO2), y=sqrtTotArea, group = rescale(meanSWC))) + geom_point(aes(group = rescale(meanSWC), color = rescale(meanSWC), size=sqrtTotArea_w)) , nrow=3 )
summary(lm(sqrtTotArea~rescale(CO2)*rescale(meanSWC) , data=trans_dfwV, weights=sqrtTotArea_w)) # NS

grid.arrange( # logSRL
  ggpredict(lm(logSRL~rescale(CO2)*rescale(meanSWC) , data=trans_dfwV, weights=logSRL_w), terms=c("CO2","meanSWC [4,42]"))%>% 
    plot(rawdata = TRUE, ci = TRUE, colors=c("red","blue")),
  ggpredict(lm(logSRL ~ rescale(CO2)*rescale(meanSWC) , data = trans_dfwV), terms=c("CO2","meanSWC [4,42]")) %>% 
    plot(rawdata = TRUE, ci = TRUE, colors=c("red","blue")),
  ggplot(trans_dfwV, aes(x=rescale(CO2), y=logSRL, group = rescale(meanSWC))) + geom_point(aes(group = rescale(meanSWC), color = rescale(meanSWC), size=logSRL_w)) , nrow=3 )
summary(lm(logSRL~rescale(CO2)*rescale(meanSWC) , data=trans_dfwV, weights=logSRL_w)) # NS

grid.arrange( # d13C
  ggpredict(lm(d13C~rescale(CO2)*rescale(meanSWC) , data=trans_dfwV, weights=d13C_w), terms=c("CO2","meanSWC [4,42]"))%>% 
    plot(rawdata = TRUE, ci = TRUE, colors=c("red","blue")),
  ggpredict(lm(d13C ~ rescale(CO2)*rescale(meanSWC) , data = trans_dfwV), terms=c("CO2","meanSWC [4,42]")) %>% 
    plot(rawdata = TRUE, ci = TRUE, colors=c("red","blue")),
  ggplot(trans_dfwV, aes(x=rescale(CO2), y=d13C, group = rescale(meanSWC))) + geom_point(aes(group = rescale(meanSWC), color = rescale(meanSWC), size=d13C_w)) , nrow=3 )
summary(lm(d13C~rescale(CO2)*rescale(meanSWC) , data=trans_dfwV, weights=d13C_w))
# d13C: CO2 * (.03), SWC . (.068)

grid.arrange( # Photo.y
  ggpredict(lm(Photo.y~rescale(CO2)*rescale(meanSWC) , data=trans_dfwV, weights=Photo.y_w), terms=c("CO2","meanSWC [4,42]"))%>% 
    plot(rawdata = TRUE, ci = TRUE, colors=c("red","blue")),
  ggpredict(lm(Photo.y ~ rescale(CO2)*rescale(meanSWC) , data = trans_dfwV), terms=c("CO2","meanSWC [4,42]")) %>% 
    plot(rawdata = TRUE, ci = TRUE, colors=c("red","blue")),
  ggplot(trans_dfwV, aes(x=rescale(CO2), y=Photo.y, group = rescale(meanSWC))) + geom_point(aes(group = rescale(meanSWC), color = rescale(meanSWC), size=Photo.y_w)) , nrow=3 )
summary(lm(Photo.y~rescale(CO2)*rescale(meanSWC) , data=trans_dfwV, weights=Photo.y_w))
# Photo.y: CO2 . (.068), H2O ** (.0023)

grid.arrange( # Cond.y
  ggpredict(lm(logCond.y~rescale(CO2)*rescale(meanSWC) , data=trans_dfwV, weights=logCond.y_w), terms=c("CO2","meanSWC [4,42]"))%>% 
    plot(rawdata = TRUE, ci = TRUE, colors=c("red","blue")),
  ggpredict(lm(logCond.y ~ rescale(CO2)*rescale(meanSWC) , data = trans_dfwV), terms=c("CO2","meanSWC [4,42]")) %>% 
    plot(rawdata = TRUE, ci = TRUE, colors=c("red","blue")),
  ggplot(trans_dfwV, aes(x=rescale(CO2), y=logCond.y, group = rescale(meanSWC))) + geom_point(aes(group = rescale(meanSWC), color = rescale(meanSWC), size=logCond.y_w)) , nrow=3 )
summary(lm(logCond.y~rescale(CO2)*rescale(meanSWC) , data=trans_dfwV, weights=logCond.y_w))
# log-transformed stomatal conductance: CO2 . (.0566), H2O *** (.00015), intx . (.059)

grid.arrange( # sqrtWUE
  ggpredict(lm(sqrtWUE~rescale(CO2)*rescale(meanSWC) , data=trans_dfwV, weights=sqrtWUE_w), terms=c("CO2","meanSWC [4,42]"))%>% 
    plot(rawdata = TRUE, ci = TRUE, colors=c("red","blue")),
  ggpredict(lm(sqrtWUE ~ rescale(CO2)*rescale(meanSWC) , data = trans_dfwV), terms=c("CO2","meanSWC [4,42]")) %>% 
    plot(rawdata = TRUE, ci = TRUE, colors=c("red","blue")),
  ggplot(trans_dfwV, aes(x=rescale(CO2), y=sqrtWUE, group = rescale(meanSWC))) + geom_point(aes(group = rescale(meanSWC), color = rescale(meanSWC), size=sqrtWUE_w)) , nrow=3 )
summary(lm(sqrtWUE~rescale(CO2)*rescale(meanSWC) , data=trans_dfwV, weights=sqrtWUE_w)) # NS

grid.arrange( # SLA
  ggpredict(lm(SLA~rescale(CO2)*rescale(meanSWC) , data=trans_dfwV, weights=SLA_w), terms=c("CO2","meanSWC [4,42]"))%>% 
    plot(rawdata = TRUE, ci = TRUE, colors=c("red","blue")),
  ggpredict(lm(SLA ~ rescale(CO2)*rescale(meanSWC) , data = trans_dfwV), terms=c("CO2","meanSWC [4,42]")) %>% 
    plot(rawdata = TRUE, ci = TRUE, colors=c("red","blue")),
  ggplot(trans_dfwV, aes(x=rescale(CO2), y=SLA, group = rescale(meanSWC))) + geom_point(aes(group = rescale(meanSWC), color = rescale(meanSWC), size=SLA_w)) , nrow=3 )
summary(lm(SLA~rescale(CO2)*rescale(meanSWC) , data=trans_dfwV, weights=SLA_w))

## Spp == L

grid.arrange( # total mass
    ggpredict(lm(sqrtTotmass~rescale(CO2)*rescale(meanSWC) , data=trans_dfwL, weights=sqrtTotmass_w), terms=c("CO2","meanSWC [4,42]"))%>% 
      plot(rawdata = TRUE, ci = TRUE, colors=c("red","blue")),
    ggpredict(lm(sqrtTotmass ~ rescale(CO2)*rescale(meanSWC) , data = trans_dfwL), terms=c("CO2","meanSWC [4,42]")) %>% 
      plot(rawdata = TRUE, ci = TRUE, colors=c("red","blue")),
    ggplot(trans_dfwL, aes(x=rescale(CO2), y=sqrtTotmass, group = rescale(meanSWC))) + geom_point(aes(group = rescale(meanSWC), color = rescale(meanSWC), size=sqrtTotmass_w)) , nrow=3 )
  summary(lm(sqrtTotmass~rescale(CO2)*rescale(meanSWC) , data=trans_dfwL, weights=sqrtTotmass_w))  
# SWC * (.039)

grid.arrange( # root:shoot
  ggpredict(lm(rootshoot~rescale(CO2)*rescale(meanSWC) , data=trans_dfwL, weights=rootshoot_w), terms=c("CO2","meanSWC [4,42]"))%>% 
    plot(rawdata = TRUE, ci = TRUE, colors=c("red","blue")),
  ggpredict(lm(rootshoot ~ rescale(CO2)*rescale(meanSWC) , data = trans_dfwL), terms=c("CO2","meanSWC [4,42]")) %>% 
    plot(rawdata = TRUE, ci = TRUE, colors=c("red","blue")),
  ggplot(trans_dfwL, aes(x=rescale(CO2), y=rootshoot, group = rescale(meanSWC))) + geom_point(aes(group = rescale(meanSWC), color = rescale(meanSWC), size=rootshoot_w)) , nrow=3 )
summary(lm(rootshoot~rescale(CO2)*rescale(meanSWC) , data=trans_dfwL, weights=rootshoot_w))  
# SWC . (.068)

grid.arrange( # lwc
  ggpredict(lm(lwc~rescale(CO2)*rescale(meanSWC) , data=trans_dfwL, weights=lwc_w), terms=c("CO2","meanSWC [4,42]"))%>% 
    plot(rawdata = TRUE, ci = TRUE, colors=c("red","blue")),
  ggpredict(lm(lwc ~ rescale(CO2)*rescale(meanSWC) , data = trans_dfwL), terms=c("CO2","meanSWC [4,42]")) %>% 
    plot(rawdata = TRUE, ci = TRUE, colors=c("red","blue")),
  ggplot(trans_dfwL, aes(x=rescale(CO2), y=lwc, group = rescale(meanSWC))) + geom_point(aes(group = rescale(meanSWC), color = rescale(meanSWC), size=lwc_w)) , nrow=3 )
summary(lm(lwc~rescale(CO2)*rescale(meanSWC) , data=trans_dfwL, weights=lwc_w)) 
# SWC . (.0895)

grid.arrange( # logAvg_area
  ggpredict(lm(logAvg_area~rescale(CO2)*rescale(meanSWC) , data=trans_dfwL, weights=logAvg_area_w), terms=c("CO2","meanSWC [4,42]"))%>% 
    plot(rawdata = TRUE, ci = TRUE, colors=c("red","blue")),
  ggpredict(lm(logAvg_area ~ rescale(CO2)*rescale(meanSWC) , data = trans_dfwL), terms=c("CO2","meanSWC [4,42]")) %>% 
    plot(rawdata = TRUE, ci = TRUE, colors=c("red","blue")),
  ggplot(trans_dfwL, aes(x=rescale(CO2), y=logAvg_area, group = rescale(meanSWC))) + geom_point(aes(group = rescale(meanSWC), color = rescale(meanSWC), size=logAvg_area_w)) , nrow=3 )
summary(lm(logAvg_area~rescale(CO2)*rescale(meanSWC) , data=trans_dfwL, weights=logAvg_area_w)) 
# log-transformed average leaf area: intx . (.0645)

grid.arrange( # logPerimArea
  ggpredict(lm(logPerimArea~rescale(CO2)*rescale(meanSWC) , data=trans_dfwL, weights=logPerimArea_w), terms=c("CO2","meanSWC [4,42]"))%>% 
    plot(rawdata = TRUE, ci = TRUE, colors=c("red","blue")),
  ggpredict(lm(logPerimArea ~ rescale(CO2)*rescale(meanSWC) , data = trans_dfwL), terms=c("CO2","meanSWC [4,42]")) %>% 
    plot(rawdata = TRUE, ci = TRUE, colors=c("red","blue")),
  ggplot(trans_dfwL, aes(x=rescale(CO2), y=logPerimArea, group = rescale(meanSWC))) + geom_point(aes(group = rescale(meanSWC), color = rescale(meanSWC), size=logPerimArea_w)) , nrow=3 )
summary(lm(logPerimArea~rescale(CO2)*rescale(meanSWC) , data=trans_dfwL, weights=logPerimArea_w))  # NS

grid.arrange( # sqrtTotArea
  ggpredict(lm(sqrtTotArea~rescale(CO2)*rescale(meanSWC) , data=trans_dfwL, weights=sqrtTotArea_w), terms=c("CO2","meanSWC [4,42]"))%>% 
    plot(rawdata = TRUE, ci = TRUE, colors=c("red","blue")),
  ggpredict(lm(sqrtTotArea ~ rescale(CO2)*rescale(meanSWC) , data = trans_dfwL), terms=c("CO2","meanSWC [4,42]")) %>% 
    plot(rawdata = TRUE, ci = TRUE, colors=c("red","blue")),
  ggplot(trans_dfwL, aes(x=rescale(CO2), y=sqrtTotArea, group = rescale(meanSWC))) + geom_point(aes(group = rescale(meanSWC), color = rescale(meanSWC), size=sqrtTotArea_w)) , nrow=3 )
summary(lm(sqrtTotArea~rescale(CO2)*rescale(meanSWC) , data=trans_dfwL, weights=sqrtTotArea_w))  
# SWC * (.022)

grid.arrange( # logSRL
  ggpredict(lm(logSRL~rescale(CO2)*rescale(meanSWC) , data=trans_dfwL, weights=logSRL_w), terms=c("CO2","meanSWC [4,42]"))%>% 
    plot(rawdata = TRUE, ci = TRUE, colors=c("red","blue")),
  ggpredict(lm(logSRL ~ rescale(CO2)*rescale(meanSWC) , data = trans_dfwL), terms=c("CO2","meanSWC [4,42]")) %>% 
    plot(rawdata = TRUE, ci = TRUE, colors=c("red","blue")),
  ggplot(trans_dfwL, aes(x=rescale(CO2), y=logSRL, group = rescale(meanSWC))) + geom_point(aes(group = rescale(meanSWC), color = rescale(meanSWC), size=logSRL_w)) , nrow=3 )
summary(lm(logSRL~rescale(CO2)*rescale(meanSWC) , data=trans_dfwL, weights=logSRL_w))   
# log-transformed specific root length: CO2 * .044

grid.arrange( # d13C
  ggpredict(lm(d13C~rescale(CO2)*rescale(meanSWC) , data=trans_dfwL, weights=d13C_w), terms=c("CO2","meanSWC [4,42]"))%>% 
    plot(rawdata = TRUE, ci = TRUE, colors=c("red","blue")),
  ggpredict(lm(d13C ~ rescale(CO2)*rescale(meanSWC) , data = trans_dfwL), terms=c("CO2","meanSWC [4,42]")) %>% 
    plot(rawdata = TRUE, ci = TRUE, colors=c("red","blue")),
  ggplot(trans_dfwL, aes(x=rescale(CO2), y=d13C, group = rescale(meanSWC))) + geom_point(aes(group = rescale(meanSWC), color = rescale(meanSWC), size=d13C_w)) , nrow=3 )
summary(lm(d13C~rescale(CO2)*rescale(meanSWC) , data=trans_dfwL, weights=d13C_w))  # NS

grid.arrange( # Photo.y
  ggpredict(lm(Photo.y~rescale(CO2)*rescale(meanSWC) , data=trans_dfwL, weights=Photo.y_w), terms=c("CO2","meanSWC [4,42]"))%>% 
    plot(rawdata = TRUE, ci = TRUE, colors=c("red","blue")),
  ggpredict(lm(Photo.y ~ rescale(CO2)*rescale(meanSWC) , data = trans_dfwL), terms=c("CO2","meanSWC [4,42]")) %>% 
    plot(rawdata = TRUE, ci = TRUE, colors=c("red","blue")),
  ggplot(trans_dfwL, aes(x=rescale(CO2), y=Photo.y, group = rescale(meanSWC))) + geom_point(aes(group = rescale(meanSWC), color = rescale(meanSWC), size=Photo.y_w)) , nrow=3 )
summary(lm(Photo.y~rescale(CO2)*rescale(meanSWC) , data=trans_dfwL, weights=Photo.y_w))  
# Photo.y: SWC ** (.0023)

grid.arrange( # Cond.y
  ggpredict(lm(logCond.y~rescale(CO2)*rescale(meanSWC) , data=trans_dfwL, weights=logCond.y_w), terms=c("CO2","meanSWC [4,42]"))%>% 
    plot(rawdata = TRUE, ci = TRUE, colors=c("red","blue")),
  ggpredict(lm(logCond.y ~ rescale(CO2)*rescale(meanSWC) , data = trans_dfwL), terms=c("CO2","meanSWC [4,42]")) %>% 
    plot(rawdata = TRUE, ci = TRUE, colors=c("red","blue")),
  ggplot(trans_dfwL, aes(x=rescale(CO2), y=logCond.y, group = rescale(meanSWC))) + geom_point(aes(group = rescale(meanSWC), color = rescale(meanSWC), size=logCond.y_w)) , nrow=3 )
summary(lm(logCond.y~rescale(CO2)*rescale(meanSWC) , data=trans_dfwL, weights=logCond.y_w))  
# log-transformed stomatal conductance: SWC ** (.00139)

grid.arrange( # sqrtWUE
  ggpredict(lm(sqrtWUE~rescale(CO2)*rescale(meanSWC) , data=trans_dfwL, weights=sqrtWUE_w), terms=c("CO2","meanSWC [4,42]"))%>% 
    plot(rawdata = TRUE, ci = TRUE, colors=c("red","blue")),
  ggpredict(lm(sqrtWUE ~ rescale(CO2)*rescale(meanSWC) , data = trans_dfwL), terms=c("CO2","meanSWC [4,42]")) %>% 
    plot(rawdata = TRUE, ci = TRUE, colors=c("red","blue")),
  ggplot(trans_dfwL, aes(x=rescale(CO2), y=sqrtWUE, group = rescale(meanSWC))) + geom_point(aes(group = rescale(meanSWC), color = rescale(meanSWC), size=sqrtWUE_w)) , nrow=3 )
summary(lm(sqrtWUE~rescale(CO2)*rescale(meanSWC) , data=trans_dfwL, weights=sqrtWUE_w))   # NS

grid.arrange( # SLA
  ggpredict(lm(SLA~rescale(CO2)*rescale(meanSWC) , data=trans_dfwL, weights=SLA_w), terms=c("CO2","meanSWC [4,42]"))%>% 
    plot(rawdata = TRUE, ci = TRUE, colors=c("red","blue")),
  ggpredict(lm(SLA ~ rescale(CO2)*rescale(meanSWC) , data = trans_dfwL), terms=c("CO2","meanSWC [4,42]")) %>% 
    plot(rawdata = TRUE, ci = TRUE, colors=c("red","blue")),
  ggplot(trans_dfwL, aes(x=rescale(CO2), y=SLA, group = rescale(meanSWC))) + geom_point(aes(group = rescale(meanSWC), color = rescale(meanSWC), size=SLA_w)) , nrow=3 )
summary(lm(SLA~rescale(CO2)*rescale(meanSWC) , data=trans_dfwL, weights=SLA_w))  


## Spp == V, CO2 categorical and SWC (continuous)
grid.arrange( # total mass
  ggpredict(lm(sqrtTotmass~CO2Tmt*meanSWC , data=trans_dfwV, weights=sqrtTotmass_w), terms=c("meanSWC", "CO2Tmt"))%>% 
    plot(rawdata = TRUE, ci = TRUE, colors=c("black","forestgreen")),
  ggpredict(lm(sqrtTotmass ~ CO2Tmt*meanSWC , data = trans_dfwV), terms=c("meanSWC", "CO2Tmt")) %>% 
    plot(rawdata = TRUE, ci = TRUE, colors=c("black","forestgreen")),
  ggplot(trans_dfwV, aes(x=meanSWC, y=sqrtTotmass, group = CO2Tmt)) + geom_point(aes(group = CO2Tmt, color = CO2Tmt, size=sqrtTotmass_w)) + scale_color_manual(values=c("black","forestgreen"))  , nrow=3)
summary(lm(sqrtTotmass~CO2Tmt*meanSWC , data=trans_dfwV, weights=sqrtTotmass_w)) # SWC 0.03

grid.arrange( # root:shoot
  ggpredict(lm(rootshoot~CO2Tmt*meanSWC , data=trans_dfwV, weights=rootshoot_w), terms=c("meanSWC", "CO2Tmt"))%>% 
    plot(rawdata = TRUE, ci = TRUE, colors=c("black","forestgreen")),
  ggpredict(lm(rootshoot ~ CO2Tmt*meanSWC , data = trans_dfwV), terms=c("meanSWC", "CO2Tmt")) %>% 
    plot(rawdata = TRUE, ci = TRUE, colors=c("black","forestgreen")),
  ggplot(trans_dfwV, aes(x=meanSWC, y=rootshoot, group = CO2Tmt)) + geom_point(aes(group = CO2Tmt, color = CO2Tmt, size=rootshoot_w)) + scale_color_manual(values=c("black","forestgreen"))  , nrow=3)
summary(lm(rootshoot~CO2Tmt*meanSWC , data=trans_dfwV, weights=rootshoot_w)) # NS

grid.arrange( # lwc
  ggpredict(lm(lwc~CO2Tmt*meanSWC , data=trans_dfwV, weights=lwc_w), terms=c("meanSWC", "CO2Tmt"))%>% 
    plot(rawdata = TRUE, ci = TRUE, colors=c("black","forestgreen")),
  ggpredict(lm(lwc ~ CO2Tmt*meanSWC , data = trans_dfwV), terms=c("meanSWC", "CO2Tmt")) %>% 
    plot(rawdata = TRUE, ci = TRUE, colors=c("black","forestgreen")),
  ggplot(trans_dfwV, aes(x=meanSWC, y=lwc, group = CO2Tmt)) + geom_point(aes(group = CO2Tmt, color = CO2Tmt, size=lwc_w)) + scale_color_manual(values=c("black","forestgreen"))  , nrow=3)
summary(lm(lwc~CO2Tmt*meanSWC , data=trans_dfwV, weights=lwc_w)) # SWC .0558

grid.arrange( # logAvg_area
  ggpredict(lm(logAvg_area~CO2Tmt*meanSWC , data=trans_dfwV, weights=logAvg_area_w), terms=c("meanSWC", "CO2Tmt"))%>% 
    plot(rawdata = TRUE, ci = TRUE, colors=c("black","forestgreen")),
  ggpredict(lm(logAvg_area ~ CO2Tmt*meanSWC , data = trans_dfwV), terms=c("meanSWC", "CO2Tmt")) %>% 
    plot(rawdata = TRUE, ci = TRUE, colors=c("black","forestgreen")),
  ggplot(trans_dfwV, aes(x=meanSWC, y=logAvg_area, group = CO2Tmt)) + geom_point(aes(group = CO2Tmt, color = CO2Tmt, size=logAvg_area_w)) + scale_color_manual(values=c("black","forestgreen"))  , nrow=3)
summary(lm(logAvg_area~CO2Tmt*meanSWC , data=trans_dfwV, weights=logAvg_area_w)) 
# CO2 .000628, SWC .00128, intx .025


grid.arrange( # logPerimArea
  ggpredict(lm(logPerimArea~CO2Tmt*meanSWC , data=trans_dfwV, weights=logPerimArea_w), terms=c("meanSWC", "CO2Tmt"))%>% 
    plot(rawdata = TRUE, ci = TRUE, colors=c("black","forestgreen")),
  ggpredict(lm(logPerimArea ~ CO2Tmt*meanSWC , data = trans_dfwV), terms=c("meanSWC", "CO2Tmt")) %>% 
    plot(rawdata = TRUE, ci = TRUE, colors=c("black","forestgreen")),
  ggplot(trans_dfwV, aes(x=meanSWC, y=logPerimArea, group = CO2Tmt)) + geom_point(aes(group = CO2Tmt, color = CO2Tmt, size=logPerimArea_w)) + scale_color_manual(values=c("black","forestgreen"))  , nrow=3)
summary(lm(logPerimArea~CO2Tmt*meanSWC , data=trans_dfwV, weights=logPerimArea_w)) 
# CO2 .0405

grid.arrange( # sqrtTotArea
  ggpredict(lm(sqrtTotArea~CO2Tmt*meanSWC , data=trans_dfwV, weights=sqrtTotArea_w), terms=c("meanSWC", "CO2Tmt"))%>% 
    plot(rawdata = TRUE, ci = TRUE, colors=c("black","forestgreen")),
  ggpredict(lm(sqrtTotArea ~ CO2Tmt*meanSWC , data = trans_dfwV), terms=c("meanSWC", "CO2Tmt")) %>% 
    plot(rawdata = TRUE, ci = TRUE, colors=c("black","forestgreen")),
  ggplot(trans_dfwV, aes(x=meanSWC, y=sqrtTotArea, group = CO2Tmt)) + geom_point(aes(group = CO2Tmt, color = CO2Tmt, size=sqrtTotArea_w)) + scale_color_manual(values=c("black","forestgreen"))  , nrow=3)
summary(lm(sqrtTotArea~CO2Tmt*meanSWC , data=trans_dfwV, weights=sqrtTotArea_w)) # NS

grid.arrange( # logSRL
  ggpredict(lm(logSRL~CO2Tmt*meanSWC , data=trans_dfwV, weights=logSRL_w), terms=c("meanSWC", "CO2Tmt"))%>% 
    plot(rawdata = TRUE, ci = TRUE, colors=c("black","forestgreen")),
  ggpredict(lm(logSRL ~ CO2Tmt*meanSWC , data = trans_dfwV), terms=c("meanSWC", "CO2Tmt")) %>% 
    plot(rawdata = TRUE, ci = TRUE, colors=c("black","forestgreen")),
  ggplot(trans_dfwV, aes(x=meanSWC, y=logSRL, group = CO2Tmt)) + geom_point(aes(group = CO2Tmt, color = CO2Tmt, size=logSRL_w)) + scale_color_manual(values=c("black","forestgreen"))  , nrow=3)
summary(lm(logSRL~CO2Tmt*meanSWC , data=trans_dfwV, weights=logSRL_w)) 

grid.arrange( # d13C
  ggpredict(lm(d13C~CO2Tmt*meanSWC , data=trans_dfwV, weights=d13C_w), terms=c("meanSWC", "CO2Tmt"))%>% 
    plot(rawdata = TRUE, ci = TRUE, colors=c("black","forestgreen")),
  ggpredict(lm(d13C ~ CO2Tmt*meanSWC , data = trans_dfwV), terms=c("meanSWC", "CO2Tmt")) %>% 
    plot(rawdata = TRUE, ci = TRUE, colors=c("black","forestgreen")),
  ggplot(trans_dfwV, aes(x=meanSWC, y=d13C, group = CO2Tmt)) + geom_point(aes(group = CO2Tmt, color = CO2Tmt, size=d13C_w)) + scale_color_manual(values=c("black","forestgreen"))  , nrow=3)
summary(lm(d13C~CO2Tmt*meanSWC , data=trans_dfwV, weights=d13C_w)) 
# CO2 .045, SWC .0804

grid.arrange( # Photo.y
  ggpredict(lm(Photo.y~CO2Tmt*meanSWC , data=trans_dfwV, weights=Photo.y_w), terms=c("meanSWC", "CO2Tmt"))%>% 
    plot(rawdata = TRUE, ci = TRUE, colors=c("black","forestgreen")),
  ggpredict(lm(Photo.y ~ CO2Tmt*meanSWC , data = trans_dfwV), terms=c("meanSWC", "CO2Tmt")) %>% 
    plot(rawdata = TRUE, ci = TRUE, colors=c("black","forestgreen")),
  ggplot(trans_dfwV, aes(x=meanSWC, y=Photo.y, group = CO2Tmt)) + geom_point(aes(group = CO2Tmt, color = CO2Tmt, size=Photo.y_w)) + scale_color_manual(values=c("black","forestgreen"))  , nrow=3)
summary(lm(Photo.y~CO2Tmt*meanSWC , data=trans_dfwV, weights=Photo.y_w)) 

grid.arrange( # Cond.y
  ggpredict(lm(logCond.y~CO2Tmt*meanSWC , data=trans_dfwV, weights=logCond.y_w), terms=c("meanSWC", "CO2Tmt"))%>% 
    plot(rawdata = TRUE, ci = TRUE, colors=c("black","forestgreen")),
  ggpredict(lm(logCond.y ~ CO2Tmt*meanSWC , data = trans_dfwV), terms=c("meanSWC", "CO2Tmt")) %>% 
    plot(rawdata = TRUE, ci = TRUE, colors=c("black","forestgreen")),
  ggplot(trans_dfwV, aes(x=meanSWC, y=logCond.y, group = CO2Tmt)) + geom_point(aes(group = CO2Tmt, color = CO2Tmt, size=logCond.y_w)) + scale_color_manual(values=c("black","forestgreen"))  , nrow=3)
summary(lm(logCond.y~CO2Tmt*meanSWC , data=trans_dfwV, weights=logCond.y_w)) 
# CO2 .053, SWC .000109, intx .0648

grid.arrange( # sqrtWUE
  ggpredict(lm(sqrtWUE~CO2Tmt*meanSWC , data=trans_dfwV, weights=sqrtWUE_w), terms=c("meanSWC", "CO2Tmt"))%>% 
    plot(rawdata = TRUE, ci = TRUE, colors=c("black","forestgreen")),
  ggpredict(lm(sqrtWUE ~ CO2Tmt*meanSWC , data = trans_dfwV), terms=c("meanSWC", "CO2Tmt")) %>% 
    plot(rawdata = TRUE, ci = TRUE, colors=c("black","forestgreen")),
  ggplot(trans_dfwV, aes(x=meanSWC, y=sqrtWUE, group = CO2Tmt)) + geom_point(aes(group = CO2Tmt, color = CO2Tmt, size=sqrtWUE_w)) + scale_color_manual(values=c("black","forestgreen"))  , nrow=3)
summary(lm(sqrtWUE~CO2Tmt*meanSWC , data=trans_dfwV, weights=sqrtWUE_w)) # NS

grid.arrange( # SLA
  ggpredict(lm(SLA~CO2Tmt*meanSWC , data=trans_dfwV, weights=SLA_w), terms=c("meanSWC", "CO2Tmt"))%>% 
    plot(rawdata = TRUE, ci = TRUE, colors=c("black","forestgreen")),
  ggpredict(lm(SLA ~ CO2Tmt*meanSWC , data = trans_dfwV), terms=c("meanSWC", "CO2Tmt")) %>% 
    plot(rawdata = TRUE, ci = TRUE, colors=c("black","forestgreen")),
  ggplot(trans_dfwV, aes(x=meanSWC, y=SLA, group = CO2Tmt)) + geom_point(aes(group = CO2Tmt, color = CO2Tmt, size=SLA_w)) + scale_color_manual(values=c("black","forestgreen"))  , nrow=3)
summary(lm(SLA~CO2Tmt*meanSWC , data=trans_dfwV, weights=SLA_w)) 

## Spp == L

grid.arrange( # total mass
    ggpredict(lm(sqrtTotmass~CO2Tmt*meanSWC , data=trans_dfwL, weights=sqrtTotmass_w), terms=c("meanSWC", "CO2Tmt"))%>% 
      plot(rawdata = TRUE, ci = TRUE, colors=c("black","forestgreen")),
    ggpredict(lm(sqrtTotmass ~ CO2Tmt*meanSWC , data = trans_dfwL), terms=c("meanSWC", "CO2Tmt")) %>% 
      plot(rawdata = TRUE, ci = TRUE, colors=c("black","forestgreen")),
    ggplot(trans_dfwL, aes(x=meanSWC, y=sqrtTotmass, group = CO2Tmt)) + geom_point(aes(group = CO2Tmt, color = CO2Tmt, size=sqrtTotmass_w)) + scale_color_manual(values=c("black","forestgreen"))  , nrow=3)
  summary(lm(sqrtTotmass~CO2Tmt*meanSWC , data=trans_dfwL, weights=sqrtTotmass_w)) 
  # SWC 0.0297

grid.arrange( # root:shoot
  ggpredict(lm(rootshoot~CO2Tmt*meanSWC , data=trans_dfwL, weights=rootshoot_w), terms=c("meanSWC", "CO2Tmt"))%>% 
    plot(rawdata = TRUE, ci = TRUE, colors=c("black","forestgreen")),
  ggpredict(lm(rootshoot ~ CO2Tmt*meanSWC , data = trans_dfwL), terms=c("meanSWC", "CO2Tmt")) %>% 
    plot(rawdata = TRUE, ci = TRUE, colors=c("black","forestgreen")),
  ggplot(trans_dfwL, aes(x=meanSWC, y=rootshoot, group = CO2Tmt)) + geom_point(aes(group = CO2Tmt, color = CO2Tmt, size=rootshoot_w)) + scale_color_manual(values=c("black","forestgreen"))  , nrow=3)
summary(lm(rootshoot~CO2Tmt*meanSWC , data=trans_dfwL, weights=rootshoot_w)) 
# SWC 0.0302

grid.arrange( # lwc
  ggpredict(lm(lwc~CO2Tmt*meanSWC , data=trans_dfwL, weights=lwc_w), terms=c("meanSWC", "CO2Tmt"))%>% 
    plot(rawdata = TRUE, ci = TRUE, colors=c("black","forestgreen")),
  ggpredict(lm(lwc ~ CO2Tmt*meanSWC , data = trans_dfwL), terms=c("meanSWC", "CO2Tmt")) %>% 
    plot(rawdata = TRUE, ci = TRUE, colors=c("black","forestgreen")),
  ggplot(trans_dfwL, aes(x=meanSWC, y=lwc, group = CO2Tmt)) + geom_point(aes(group = CO2Tmt, color = CO2Tmt, size=lwc_w)) + scale_color_manual(values=c("black","forestgreen"))  , nrow=3)
summary(lm(lwc~CO2Tmt*meanSWC , data=trans_dfwL, weights=lwc_w)) 
# SWC 0.0736

grid.arrange( # logAvg_area
  ggpredict(lm(logAvg_area~CO2Tmt*meanSWC , data=trans_dfwL, weights=logAvg_area_w), terms=c("meanSWC", "CO2Tmt"))%>% 
    plot(rawdata = TRUE, ci = TRUE, colors=c("black","forestgreen")),
  ggpredict(lm(logAvg_area ~ CO2Tmt*meanSWC , data = trans_dfwL), terms=c("meanSWC", "CO2Tmt")) %>% 
    plot(rawdata = TRUE, ci = TRUE, colors=c("black","forestgreen")),
  ggplot(trans_dfwL, aes(x=meanSWC, y=logAvg_area, group = CO2Tmt)) + geom_point(aes(group = CO2Tmt, color = CO2Tmt, size=logAvg_area_w)) + scale_color_manual(values=c("black","forestgreen"))  , nrow=3)
summary(lm(logAvg_area~CO2Tmt*meanSWC , data=trans_dfwL, weights=logAvg_area_w)) 
# intx 0.0579

grid.arrange( # logPerimArea
  ggpredict(lm(logPerimArea~CO2Tmt*meanSWC , data=trans_dfwL, weights=logPerimArea_w), terms=c("meanSWC", "CO2Tmt"))%>% 
    plot(rawdata = TRUE, ci = TRUE, colors=c("black","forestgreen")),
  ggpredict(lm(logPerimArea ~ CO2Tmt*meanSWC , data = trans_dfwL), terms=c("meanSWC", "CO2Tmt")) %>% 
    plot(rawdata = TRUE, ci = TRUE, colors=c("black","forestgreen")),
  ggplot(trans_dfwL, aes(x=meanSWC, y=logPerimArea, group = CO2Tmt)) + geom_point(aes(group = CO2Tmt, color = CO2Tmt, size=logPerimArea_w)) + scale_color_manual(values=c("black","forestgreen"))  , nrow=3)
summary(lm(logPerimArea~CO2Tmt*meanSWC , data=trans_dfwL, weights=logPerimArea_w)) # NS

grid.arrange( # sqrtTotArea
  ggpredict(lm(sqrtTotArea~CO2Tmt*meanSWC , data=trans_dfwL, weights=sqrtTotArea_w), terms=c("meanSWC", "CO2Tmt"))%>% 
    plot(rawdata = TRUE, ci = TRUE, colors=c("black","forestgreen")),
  ggpredict(lm(sqrtTotArea ~ CO2Tmt*meanSWC , data = trans_dfwL), terms=c("meanSWC", "CO2Tmt")) %>% 
    plot(rawdata = TRUE, ci = TRUE, colors=c("black","forestgreen")),
  ggplot(trans_dfwL, aes(x=meanSWC, y=sqrtTotArea, group = CO2Tmt)) + geom_point(aes(group = CO2Tmt, color = CO2Tmt, size=sqrtTotArea_w)) + scale_color_manual(values=c("black","forestgreen"))  , nrow=3)
summary(lm(sqrtTotArea~CO2Tmt*meanSWC , data=trans_dfwL, weights=sqrtTotArea_w)) 
# SWC 0.0161

grid.arrange( # logSRL
  ggpredict(lm(logSRL~CO2Tmt*meanSWC , data=trans_dfwL, weights=logSRL_w), terms=c("meanSWC", "CO2Tmt"))%>% 
    plot(rawdata = TRUE, ci = TRUE, colors=c("black","forestgreen")),
  ggpredict(lm(logSRL ~ CO2Tmt*meanSWC , data = trans_dfwL), terms=c("meanSWC", "CO2Tmt")) %>% 
    plot(rawdata = TRUE, ci = TRUE, colors=c("black","forestgreen")),
  ggplot(trans_dfwL, aes(x=meanSWC, y=logSRL, group = CO2Tmt)) + geom_point(aes(group = CO2Tmt, color = CO2Tmt, size=logSRL_w)) + scale_color_manual(values=c("black","forestgreen"))  , nrow=3)
summary(lm(logSRL~CO2Tmt*meanSWC , data=trans_dfwL, weights=logSRL_w)) 
# CO2 0.090

grid.arrange( # d13C
  ggpredict(lm(d13C~CO2Tmt*meanSWC , data=trans_dfwL, weights=d13C_w), terms=c("meanSWC", "CO2Tmt"))%>% 
    plot(rawdata = TRUE, ci = TRUE, colors=c("black","forestgreen")),
  ggpredict(lm(d13C ~ CO2Tmt*meanSWC , data = trans_dfwL), terms=c("meanSWC", "CO2Tmt")) %>% 
    plot(rawdata = TRUE, ci = TRUE, colors=c("black","forestgreen")),
  ggplot(trans_dfwL, aes(x=meanSWC, y=d13C, group = CO2Tmt)) + geom_point(aes(group = CO2Tmt, color = CO2Tmt, size=d13C_w)) + scale_color_manual(values=c("black","forestgreen"))  , nrow=3)
summary(lm(d13C~CO2Tmt*meanSWC , data=trans_dfwL, weights=d13C_w)) 
# intx 0.0812

grid.arrange( # Photo.y
  ggpredict(lm(Photo.y~CO2Tmt*meanSWC , data=trans_dfwL, weights=Photo.y_w), terms=c("meanSWC", "CO2Tmt"))%>% 
    plot(rawdata = TRUE, ci = TRUE, colors=c("black","forestgreen")),
  ggpredict(lm(Photo.y ~ CO2Tmt*meanSWC , data = trans_dfwL), terms=c("meanSWC", "CO2Tmt")) %>% 
    plot(rawdata = TRUE, ci = TRUE, colors=c("black","forestgreen")),
  ggplot(trans_dfwL, aes(x=meanSWC, y=Photo.y, group = CO2Tmt)) + geom_point(aes(group = CO2Tmt, color = CO2Tmt, size=Photo.y_w)) + scale_color_manual(values=c("black","forestgreen"))  , nrow=3)
summary(lm(Photo.y~CO2Tmt*meanSWC , data=trans_dfwL, weights=Photo.y_w)) 
# SWC 0.00118

grid.arrange( # Cond.y
  ggpredict(lm(logCond.y~CO2Tmt*meanSWC , data=trans_dfwL, weights=logCond.y_w), terms=c("meanSWC", "CO2Tmt"))%>% 
    plot(rawdata = TRUE, ci = TRUE, colors=c("black","forestgreen")),
  ggpredict(lm(logCond.y ~ CO2Tmt*meanSWC , data = trans_dfwL), terms=c("meanSWC", "CO2Tmt")) %>% 
    plot(rawdata = TRUE, ci = TRUE, colors=c("black","forestgreen")),
  ggplot(trans_dfwL, aes(x=meanSWC, y=logCond.y, group = CO2Tmt)) + geom_point(aes(group = CO2Tmt, color = CO2Tmt, size=logCond.y_w)) + scale_color_manual(values=c("black","forestgreen"))  , nrow=3)
summary(lm(logCond.y~CO2Tmt*meanSWC , data=trans_dfwL, weights=logCond.y_w)) 
# SWC 0.00685

grid.arrange( # sqrtWUE
  ggpredict(lm(sqrtWUE~CO2Tmt*meanSWC , data=trans_dfwL, weights=sqrtWUE_w), terms=c("meanSWC", "CO2Tmt"))%>% 
    plot(rawdata = TRUE, ci = TRUE, colors=c("black","forestgreen")),
  ggpredict(lm(sqrtWUE ~ CO2Tmt*meanSWC , data = trans_dfwL), terms=c("meanSWC", "CO2Tmt")) %>% 
    plot(rawdata = TRUE, ci = TRUE, colors=c("black","forestgreen")),
  ggplot(trans_dfwL, aes(x=meanSWC, y=sqrtWUE, group = CO2Tmt)) + geom_point(aes(group = CO2Tmt, color = CO2Tmt, size=sqrtWUE_w)) + scale_color_manual(values=c("black","forestgreen"))  , nrow=3)
summary(lm(sqrtWUE~CO2Tmt*meanSWC , data=trans_dfwL, weights=sqrtWUE_w)) # NS

grid.arrange( # SLA
  ggpredict(lm(SLA~CO2Tmt*meanSWC , data=trans_dfwL, weights=SLA_w), terms=c("meanSWC", "CO2Tmt"))%>% 
    plot(rawdata = TRUE, ci = TRUE, colors=c("black","forestgreen")),
  ggpredict(lm(SLA ~ CO2Tmt*meanSWC , data = trans_dfwL), terms=c("meanSWC", "CO2Tmt")) %>% 
    plot(rawdata = TRUE, ci = TRUE, colors=c("black","forestgreen")),
  ggplot(trans_dfwL, aes(x=meanSWC, y=SLA, group = CO2Tmt)) + geom_point(aes(group = CO2Tmt, color = CO2Tmt, size=SLA_w)) + scale_color_manual(values=c("black","forestgreen"))  , nrow=3)
summary(lm(SLA~CO2Tmt*meanSWC , data=trans_dfwL, weights=SLA_w)) 



## categorical variables for both
## Spp == V
grid.arrange( # total mass
  ggpredict(lm(sqrtTotmass~CO2Tmt*H2OTmt , data=trans_dfwV, weights=sqrtTotmass_w), terms=c("CO2Tmt", "H2OTmt"))%>% 
    plot(rawdata = TRUE, ci = TRUE, colors=c("blue","red")),
  ggpredict(lm(sqrtTotmass ~ CO2Tmt*H2OTmt , data = trans_dfwV), terms=c("CO2Tmt", "H2OTmt")) %>% 
    plot(rawdata = TRUE, ci = TRUE, colors=c("blue","red")),
  ggplot(trans_dfwV, aes(x=CO2, y=sqrtTotmass, group = H2OTmt)) + geom_point(aes(group = H2OTmt, color = H2OTmt, size=sqrtTotmass_w)) + scale_color_manual(values=c("red","blue"))  , nrow=3)
summary(lm(sqrtTotmass~CO2Tmt*H2OTmt , data=trans_dfwV, weights=sqrtTotmass_w)) 
# H2O 0.0171

grid.arrange( # root:shoot
  ggpredict(lm(rootshoot~CO2Tmt*H2OTmt , data=trans_dfwV, weights=rootshoot_w), terms=c("CO2Tmt", "H2OTmt"))%>% 
    plot(rawdata = TRUE, ci = TRUE, colors=c("blue","red")),
  ggpredict(lm(rootshoot ~ CO2Tmt*H2OTmt , data = trans_dfwV), terms=c("CO2Tmt", "H2OTmt")) %>% 
    plot(rawdata = TRUE, ci = TRUE, colors=c("blue","red")),
  ggplot(trans_dfwV, aes(x=CO2, y=rootshoot, group = H2OTmt)) + geom_point(aes(group = H2OTmt, color = H2OTmt, size=rootshoot_w)) + scale_color_manual(values=c("red","blue"))  , nrow=3)
summary(lm(rootshoot~CO2Tmt*H2OTmt , data=trans_dfwV, weights=rootshoot_w)) # NS

grid.arrange( # lwc
  ggpredict(lm(lwc~CO2Tmt*H2OTmt , data=trans_dfwV, weights=lwc_w), terms=c("CO2Tmt", "H2OTmt"))%>% 
    plot(rawdata = TRUE, ci = TRUE, colors=c("blue","red")),
  ggpredict(lm(lwc ~ CO2Tmt*H2OTmt , data = trans_dfwV), terms=c("CO2Tmt", "H2OTmt")) %>% 
    plot(rawdata = TRUE, ci = TRUE, colors=c("blue","red")),
  ggplot(trans_dfwV, aes(x=CO2, y=lwc, group = H2OTmt)) + geom_point(aes(group = H2OTmt, color = H2OTmt, size=lwc_w)) + scale_color_manual(values=c("red","blue"))  , nrow=3)
summary(lm(lwc~CO2Tmt*H2OTmt , data=trans_dfwV, weights=lwc_w)) 
# H2O .0314

grid.arrange( # logAvg_area
  ggpredict(lm(logAvg_area~CO2Tmt*H2OTmt , data=trans_dfwV, weights=logAvg_area_w), terms=c("CO2Tmt", "H2OTmt"))%>% 
    plot(rawdata = TRUE, ci = TRUE, colors=c("blue","red")),
  ggpredict(lm(logAvg_area ~ CO2Tmt*H2OTmt , data = trans_dfwV), terms=c("CO2Tmt", "H2OTmt")) %>% 
    plot(rawdata = TRUE, ci = TRUE, colors=c("blue","red")),
  ggplot(trans_dfwV, aes(x=CO2, y=logAvg_area, group = H2OTmt)) + geom_point(aes(group = H2OTmt, color = H2OTmt, size=logAvg_area_w)) + scale_color_manual(values=c("red","blue"))  , nrow=3)
summary(lm(logAvg_area~CO2Tmt*H2OTmt , data=trans_dfwV, weights=logAvg_area_w)) 
# .000361, .00264, 0.0126


grid.arrange( # logPerimArea
  ggpredict(lm(logPerimArea~CO2Tmt*H2OTmt , data=trans_dfwV, weights=logPerimArea_w), terms=c("CO2Tmt", "H2OTmt"))%>% 
    plot(rawdata = TRUE, ci = TRUE, colors=c("blue","red")),
  ggpredict(lm(logPerimArea ~ CO2Tmt*H2OTmt , data = trans_dfwV), terms=c("CO2Tmt", "H2OTmt")) %>% 
    plot(rawdata = TRUE, ci = TRUE, colors=c("blue","red")),
  ggplot(trans_dfwV, aes(x=CO2, y=logPerimArea, group = H2OTmt)) + geom_point(aes(group = H2OTmt, color = H2OTmt, size=logPerimArea_w)) + scale_color_manual(values=c("red","blue"))  , nrow=3)
summary(lm(logPerimArea~CO2Tmt*H2OTmt , data=trans_dfwV, weights=logPerimArea_w)) 
# CO2 .035

grid.arrange( # sqrtTotArea
  ggpredict(lm(sqrtTotArea~CO2Tmt*H2OTmt , data=trans_dfwV, weights=sqrtTotArea_w), terms=c("CO2Tmt", "H2OTmt"))%>% 
    plot(rawdata = TRUE, ci = TRUE, colors=c("blue","red")),
  ggpredict(lm(sqrtTotArea ~ CO2Tmt*H2OTmt , data = trans_dfwV), terms=c("CO2Tmt", "H2OTmt")) %>% 
    plot(rawdata = TRUE, ci = TRUE, colors=c("blue","red")),
  ggplot(trans_dfwV, aes(x=CO2, y=sqrtTotArea, group = H2OTmt)) + geom_point(aes(group = H2OTmt, color = H2OTmt, size=sqrtTotArea_w)) + scale_color_manual(values=c("red","blue"))  , nrow=3)
summary(lm(sqrtTotArea~CO2Tmt*H2OTmt , data=trans_dfwV, weights=sqrtTotArea_w)) # NS

grid.arrange( # logSRL
  ggpredict(lm(logSRL~CO2Tmt*H2OTmt , data=trans_dfwV, weights=logSRL_w), terms=c("CO2Tmt", "H2OTmt"))%>% 
    plot(rawdata = TRUE, ci = TRUE, colors=c("blue","red")),
  ggpredict(lm(logSRL ~ CO2Tmt*H2OTmt , data = trans_dfwV), terms=c("CO2Tmt", "H2OTmt")) %>% 
    plot(rawdata = TRUE, ci = TRUE, colors=c("blue","red")),
  ggplot(trans_dfwV, aes(x=CO2, y=logSRL, group = H2OTmt)) + geom_point(aes(group = H2OTmt, color = H2OTmt, size=logSRL_w)) + scale_color_manual(values=c("red","blue"))  , nrow=3)
summary(lm(logSRL~CO2Tmt*H2OTmt , data=trans_dfwV, weights=logSRL_w)) # NS

grid.arrange( # d13C
  ggpredict(lm(d13C~CO2Tmt*H2OTmt , data=trans_dfwV, weights=d13C_w), terms=c("CO2Tmt", "H2OTmt"))%>% 
    plot(rawdata = TRUE, ci = TRUE, colors=c("blue","red")),
  ggpredict(lm(d13C ~ CO2Tmt*H2OTmt , data = trans_dfwV), terms=c("CO2Tmt", "H2OTmt")) %>% 
    plot(rawdata = TRUE, ci = TRUE, colors=c("blue","red")),
  ggplot(trans_dfwV, aes(x=CO2, y=d13C, group = H2OTmt)) + geom_point(aes(group = H2OTmt, color = H2OTmt, size=d13C_w)) + scale_color_manual(values=c("red","blue"))  , nrow=3)
summary(lm(d13C~CO2Tmt*H2OTmt , data=trans_dfwV, weights=d13C_w)) 
# CO2 .00911, H2O .05316

grid.arrange( # Photo.y
  ggpredict(lm(Photo.y~CO2Tmt*H2OTmt , data=trans_dfwV, weights=Photo.y_w), terms=c("CO2Tmt", "H2OTmt"))%>% 
    plot(rawdata = TRUE, ci = TRUE, colors=c("blue","red")),
  ggpredict(lm(Photo.y ~ CO2Tmt*H2OTmt , data = trans_dfwV), terms=c("CO2Tmt", "H2OTmt")) %>% 
    plot(rawdata = TRUE, ci = TRUE, colors=c("blue","red")),
  ggplot(trans_dfwV, aes(x=CO2, y=Photo.y, group = H2OTmt)) + geom_point(aes(group = H2OTmt, color = H2OTmt, size=Photo.y_w)) + scale_color_manual(values=c("red","blue"))  , nrow=3)
summary(lm(Photo.y~CO2Tmt*H2OTmt , data=trans_dfwV, weights=Photo.y_w)) 
# CO2 .0388, H2O .000474

grid.arrange( # Cond.y
  ggpredict(lm(logCond.y~CO2Tmt*H2OTmt , data=trans_dfwV, weights=logCond.y_w), terms=c("CO2Tmt", "H2OTmt"))%>% 
    plot(rawdata = TRUE, ci = TRUE, colors=c("blue","red")),
  ggpredict(lm(logCond.y ~ CO2Tmt*H2OTmt , data = trans_dfwV), terms=c("CO2Tmt", "H2OTmt")) %>% 
    plot(rawdata = TRUE, ci = TRUE, colors=c("blue","red")),
  ggplot(trans_dfwV, aes(x=CO2, y=logCond.y, group = H2OTmt)) + geom_point(aes(group = H2OTmt, color = H2OTmt, size=logCond.y_w)) + scale_color_manual(values=c("red","blue"))  , nrow=3)
summary(lm(logCond.y~CO2Tmt*H2OTmt , data=trans_dfwV, weights=logCond.y_w)) 
# H2O .00437

grid.arrange( # sqrtWUE
  ggpredict(lm(sqrtWUE~CO2Tmt*H2OTmt , data=trans_dfwV, weights=sqrtWUE_w), terms=c("CO2Tmt", "H2OTmt"))%>% 
    plot(rawdata = TRUE, ci = TRUE, colors=c("blue","red")),
  ggpredict(lm(sqrtWUE ~ CO2Tmt*H2OTmt , data = trans_dfwV), terms=c("CO2Tmt", "H2OTmt")) %>% 
    plot(rawdata = TRUE, ci = TRUE, colors=c("blue","red")),
  ggplot(trans_dfwV, aes(x=CO2, y=sqrtWUE, group = H2OTmt)) + geom_point(aes(group = H2OTmt, color = H2OTmt, size=sqrtWUE_w)) + scale_color_manual(values=c("red","blue"))  , nrow=3)
summary(lm(sqrtWUE~CO2Tmt*H2OTmt , data=trans_dfwV, weights=sqrtWUE_w)) # NS

grid.arrange( # SLA
  ggpredict(lm(SLA~CO2Tmt*H2OTmt , data=trans_dfwV, weights=SLA_w), terms=c("CO2Tmt", "H2OTmt"))%>% 
    plot(rawdata = TRUE, ci = TRUE, colors=c("blue","red")),
  ggpredict(lm(SLA ~ CO2Tmt*H2OTmt , data = trans_dfwV), terms=c("CO2Tmt", "H2OTmt")) %>% 
    plot(rawdata = TRUE, ci = TRUE, colors=c("blue","red")),
  ggplot(trans_dfwV, aes(x=CO2, y=SLA, group = H2OTmt)) + geom_point(aes(group = H2OTmt, color = H2OTmt, size=SLA_w)) + scale_color_manual(values=c("red","blue"))  , nrow=3)
summary(lm(SLA~CO2Tmt*H2OTmt , data=trans_dfwV, weights=SLA_w)) 

## Spp == L

grid.arrange( # total mass
  ggpredict(lm(sqrtTotmass~CO2Tmt*H2OTmt , data=trans_dfwL, weights=sqrtTotmass_w), terms=c("CO2Tmt", "H2OTmt"))%>% 
    plot(rawdata = TRUE, ci = TRUE, colors=c("blue","red")),
  ggpredict(lm(sqrtTotmass ~ CO2Tmt*H2OTmt , data = trans_dfwL), terms=c("CO2Tmt", "H2OTmt")) %>% 
    plot(rawdata = TRUE, ci = TRUE, colors=c("blue","red")),
  ggplot(trans_dfwL, aes(x=CO2, y=sqrtTotmass, group = H2OTmt)) + geom_point(aes(group = H2OTmt, color = H2OTmt, size=sqrtTotmass_w)) + scale_color_manual(values=c("red","blue"))  , nrow=3)
summary(lm(sqrtTotmass~CO2Tmt*H2OTmt , data=trans_dfwL, weights=sqrtTotmass_w)) 
# H2O 0.0696

grid.arrange( # root:shoot
  ggpredict(lm(rootshoot~CO2Tmt*H2OTmt , data=trans_dfwL, weights=rootshoot_w), terms=c("CO2Tmt", "H2OTmt"))%>% 
    plot(rawdata = TRUE, ci = TRUE, colors=c("blue","red")),
  ggpredict(lm(rootshoot ~ CO2Tmt*H2OTmt , data = trans_dfwL), terms=c("CO2Tmt", "H2OTmt")) %>% 
    plot(rawdata = TRUE, ci = TRUE, colors=c("blue","red")),
  ggplot(trans_dfwL, aes(x=CO2, y=rootshoot, group = H2OTmt)) + geom_point(aes(group = H2OTmt, color = H2OTmt, size=rootshoot_w)) + scale_color_manual(values=c("red","blue"))  , nrow=3)
summary(lm(rootshoot~CO2Tmt*H2OTmt , data=trans_dfwL, weights=rootshoot_w)) 
# H2O .0138

grid.arrange( # lwc
  ggpredict(lm(lwc~CO2Tmt*H2OTmt , data=trans_dfwL, weights=lwc_w), terms=c("CO2Tmt", "H2OTmt"))%>% 
    plot(rawdata = TRUE, ci = TRUE, colors=c("blue","red")),
  ggpredict(lm(lwc ~ CO2Tmt*H2OTmt , data = trans_dfwL), terms=c("CO2Tmt", "H2OTmt")) %>% 
    plot(rawdata = TRUE, ci = TRUE, colors=c("blue","red")),
  ggplot(trans_dfwL, aes(x=CO2, y=lwc, group = H2OTmt)) + geom_point(aes(group = H2OTmt, color = H2OTmt, size=lwc_w)) + scale_color_manual(values=c("red","blue"))  , nrow=3)
summary(lm(lwc~CO2Tmt*H2OTmt , data=trans_dfwL, weights=lwc_w))  # NS

grid.arrange( # logAvg_area
  ggpredict(lm(sqrtTotmass~CO2Tmt*H2OTmt , data=trans_dfwL, weights=sqrtTotmass_w), terms=c("CO2Tmt", "H2OTmt"))%>% 
    plot(rawdata = TRUE, ci = TRUE, colors=c("blue","red")),
  ggpredict(lm(sqrtTotmass ~ CO2Tmt*H2OTmt , data = trans_dfwL), terms=c("CO2Tmt", "H2OTmt")) %>% 
    plot(rawdata = TRUE, ci = TRUE, colors=c("blue","red")),
  ggplot(trans_dfwL, aes(x=CO2, y=sqrtTotmass, group = H2OTmt)) + geom_point(aes(group = H2OTmt, color = H2OTmt, size=sqrtTotmass_w)) + scale_color_manual(values=c("red","blue"))  , nrow=3)
summary(lm(sqrtTotmass~CO2Tmt*H2OTmt , data=trans_dfwL, weights=sqrtTotmass_w)) 
# H2O .0696

grid.arrange( # logPerimArea
  ggpredict(lm(logPerimArea~CO2Tmt*H2OTmt , data=trans_dfwL, weights=logPerimArea_w), terms=c("CO2Tmt", "H2OTmt"))%>% 
    plot(rawdata = TRUE, ci = TRUE, colors=c("blue","red")),
  ggpredict(lm(logPerimArea ~ CO2Tmt*H2OTmt , data = trans_dfwL), terms=c("CO2Tmt", "H2OTmt")) %>% 
    plot(rawdata = TRUE, ci = TRUE, colors=c("blue","red")),
  ggplot(trans_dfwL, aes(x=CO2, y=logPerimArea, group = H2OTmt)) + geom_point(aes(group = H2OTmt, color = H2OTmt, size=logPerimArea_w)) + scale_color_manual(values=c("red","blue"))  , nrow=3)
summary(lm(logPerimArea~CO2Tmt*H2OTmt , data=trans_dfwL, weights=logPerimArea_w)) # NS

grid.arrange( # sqrtTotArea
  ggpredict(lm(sqrtTotArea~CO2Tmt*H2OTmt , data=trans_dfwL, weights=sqrtTotArea_w), terms=c("CO2Tmt", "H2OTmt"))%>% 
    plot(rawdata = TRUE, ci = TRUE, colors=c("blue","red")),
  ggpredict(lm(sqrtTotArea ~ CO2Tmt*H2OTmt , data = trans_dfwL), terms=c("CO2Tmt", "H2OTmt")) %>% 
    plot(rawdata = TRUE, ci = TRUE, colors=c("blue","red")),
  ggplot(trans_dfwL, aes(x=CO2, y=sqrtTotArea, group = H2OTmt)) + geom_point(aes(group = H2OTmt, color = H2OTmt, size=sqrtTotArea_w)) + scale_color_manual(values=c("red","blue"))  , nrow=3)
summary(lm(sqrtTotArea~CO2Tmt*H2OTmt , data=trans_dfwL, weights=sqrtTotArea_w)) 
# H2O .0206

grid.arrange( # logSRL
  ggpredict(lm(logSRL~CO2Tmt*H2OTmt , data=trans_dfwL, weights=logSRL_w), terms=c("CO2Tmt", "H2OTmt"))%>% 
    plot(rawdata = TRUE, ci = TRUE, colors=c("blue","red")),
  ggpredict(lm(logSRL ~ CO2Tmt*H2OTmt , data = trans_dfwL), terms=c("CO2Tmt", "H2OTmt")) %>% 
    plot(rawdata = TRUE, ci = TRUE, colors=c("blue","red")),
  ggplot(trans_dfwL, aes(x=CO2, y=logSRL, group = H2OTmt)) + geom_point(aes(group = H2OTmt, color = H2OTmt, size=logSRL_w)) + scale_color_manual(values=c("red","blue"))  , nrow=3)
summary(lm(logSRL~CO2Tmt*H2OTmt , data=trans_dfwL, weights=logSRL_w)) # NS

grid.arrange( # d13C
  ggpredict(lm(d13C~CO2Tmt*H2OTmt , data=trans_dfwL, weights=d13C_w), terms=c("CO2Tmt", "H2OTmt"))%>% 
    plot(rawdata = TRUE, ci = TRUE, colors=c("blue","red")),
  ggpredict(lm(d13C ~ CO2Tmt*H2OTmt , data = trans_dfwL), terms=c("CO2Tmt", "H2OTmt")) %>% 
    plot(rawdata = TRUE, ci = TRUE, colors=c("blue","red")),
  ggplot(trans_dfwL, aes(x=CO2, y=d13C, group = H2OTmt)) + geom_point(aes(group = H2OTmt, color = H2OTmt, size=d13C_w)) + scale_color_manual(values=c("red","blue"))  , nrow=3)
summary(lm(d13C~CO2Tmt*H2OTmt , data=trans_dfwL, weights=d13C_w)) # NS

grid.arrange( # Photo.y
  ggpredict(lm(Photo.y~CO2Tmt*H2OTmt , data=trans_dfwL, weights=Photo.y_w), terms=c("CO2Tmt", "H2OTmt"))%>% 
    plot(rawdata = TRUE, ci = TRUE, colors=c("blue","red")),
  ggpredict(lm(Photo.y ~ CO2Tmt*H2OTmt , data = trans_dfwL), terms=c("CO2Tmt", "H2OTmt")) %>% 
    plot(rawdata = TRUE, ci = TRUE, colors=c("blue","red")),
  ggplot(trans_dfwL, aes(x=CO2, y=Photo.y, group = H2OTmt)) + geom_point(aes(group = H2OTmt, color = H2OTmt, size=Photo.y_w)) + scale_color_manual(values=c("red","blue"))  , nrow=3)
summary(lm(Photo.y~CO2Tmt*H2OTmt , data=trans_dfwL, weights=Photo.y_w)) 
# H2O .00037

grid.arrange( # Cond.y
  ggpredict(lm(logCond.y~CO2Tmt*H2OTmt , data=trans_dfwL, weights=logCond.y_w), terms=c("CO2Tmt", "H2OTmt"))%>% 
    plot(rawdata = TRUE, ci = TRUE, colors=c("blue","red")),
  ggpredict(lm(logCond.y ~ CO2Tmt*H2OTmt , data = trans_dfwL), terms=c("CO2Tmt", "H2OTmt")) %>% 
    plot(rawdata = TRUE, ci = TRUE, colors=c("blue","red")),
  ggplot(trans_dfwL, aes(x=CO2, y=logCond.y, group = H2OTmt)) + geom_point(aes(group = H2OTmt, color = H2OTmt, size=logCond.y_w)) + scale_color_manual(values=c("red","blue"))  , nrow=3)
summary(lm(logCond.y~CO2Tmt*H2OTmt , data=trans_dfwL, weights=logCond.y_w)) 
# CO2 0.06711, H2O 0.00141

grid.arrange( # sqrtWUE
  ggpredict(lm(sqrtWUE~CO2Tmt*H2OTmt , data=trans_dfwL, weights=sqrtWUE_w), terms=c("CO2Tmt", "H2OTmt"))%>% 
    plot(rawdata = TRUE, ci = TRUE, colors=c("blue","red")),
  ggpredict(lm(sqrtWUE ~ CO2Tmt*H2OTmt , data = trans_dfwL), terms=c("CO2Tmt", "H2OTmt")) %>% 
    plot(rawdata = TRUE, ci = TRUE, colors=c("blue","red")),
  ggplot(trans_dfwL, aes(x=CO2, y=sqrtWUE, group = H2OTmt)) + geom_point(aes(group = H2OTmt, color = H2OTmt, size=sqrtWUE_w)) + scale_color_manual(values=c("red","blue"))  , nrow=3)
summary(lm(sqrtWUE~CO2Tmt*H2OTmt , data=trans_dfwL, weights=sqrtWUE_w)) # NS

grid.arrange( # SLA
  ggpredict(lm(SLA~CO2Tmt*H2OTmt , data=trans_dfwL, weights=SLA_w), terms=c("CO2Tmt", "H2OTmt"))%>% 
    plot(rawdata = TRUE, ci = TRUE, colors=c("blue","red")),
  ggpredict(lm(SLA ~ CO2Tmt*H2OTmt , data = trans_dfwL), terms=c("CO2Tmt", "H2OTmt")) %>% 
    plot(rawdata = TRUE, ci = TRUE, colors=c("blue","red")),
  ggplot(trans_dfwL, aes(x=CO2, y=SLA, group = H2OTmt)) + geom_point(aes(group = H2OTmt, color = H2OTmt, size=SLA_w)) + scale_color_manual(values=c("red","blue"))  , nrow=3)
summary(lm(SLA~CO2Tmt*H2OTmt , data=trans_dfwL, weights=SLA_w)) # NS


# manova time
# I heard a MANOVA has more power, so long as your data is normally distributed, which I've already ensured
# now doing manova: transformed data, plot means, with weights 

# add the other seven variables in order here
trans_dfwV.cbind <- cbind(unlist(trans_dfwV$sqrtTotmass), unlist(trans_dfwV$rootshoot), unlist(trans_dfwV$lwc), unlist(trans_dfwV$logAvg_area), unlist(trans_dfwV$logPerimArea), unlist(trans_dfwV$sqrtTotArea), unlist(trans_dfwV$logSRL), unlist(trans_dfwV$d13C),unlist(trans_dfwV$Photo.y),unlist(trans_dfwV$logCond.y),unlist(trans_dfwV$sqrtWUE))

trans_dfwL.cbind <- cbind(unlist(trans_dfwL$sqrtTotmass), unlist(trans_dfwL$rootshoot), unlist(trans_dfwL$lwc), unlist(trans_dfwL$logAvg_area), unlist(trans_dfwL$logPerimArea), unlist(trans_dfwL$sqrtTotArea), unlist(trans_dfwL$logSRL), unlist(trans_dfwL$d13C),unlist(trans_dfwL$Photo.y),unlist(trans_dfwL$logCond.y),unlist(trans_dfwL$sqrtWUE))

# doing this for now with the weights calculated from all 11. After selecting a subset, I'll recalculate grand weights with just those variables
# try these for V: logAvg_area, logPerimArea, Photo.y, Cond.y
summary(manova(trans_dfwV.cbind[,c(4,5,9,10)] ~ CO2Tmt*H2OTmt, trans_dfwV, weights = quad_w)) # CO2*, H2O*
summary(manova(trans_dfwV.cbind[,c(4,5,9,10)] ~ CO2Tmt*H2OTmt, trans_dfwV, weights = all_w)) # CO2*
summary(manova(trans_dfwV.cbind[,c(4,5,9,10)] ~ CO2Tmt*H2OTmt, trans_dfwV)) # H2O**
summary(manova(trans_dfwV.cbind[,c(5,9,10)] ~ CO2Tmt*H2OTmt, trans_dfwV, weights = quad_w)) # CO2**, H2O**
summary(manova(trans_dfwV.cbind[,c(4,5)] ~ CO2Tmt*H2OTmt, trans_dfwV, weights = quad_w))

# these for L: logAvg_area, logSRL, Photo.y, logCond.y
summary(manova(trans_dfwL.cbind[,c(4,7,9,10)] ~ CO2Tmt*H2OTmt, trans_dfwL, weights = quad_w)) # CO2*,H2O***,intx .
summary(manova(trans_dfwL.cbind[,c(4,7,9,10)] ~ CO2Tmt*H2OTmt, trans_dfwL, weights = all_w)) # H2O ***
summary(manova(trans_dfwL.cbind[,c(4,7,9,10)] ~ CO2Tmt*H2OTmt, trans_dfwL)) # H2O***
summary(manova(trans_dfwL.cbind[,c(5,9,10)] ~ CO2Tmt*H2OTmt, trans_dfwL, weights = quad_w)) # CO2** H2O***
summary(manova(trans_dfwL.cbind[,c(4,7)] ~ CO2Tmt*H2OTmt, trans_dfwL, weights = quad_w)) 




trans_dfVL <- biomass2. %>% 
  dplyr::select(Plot, Spp, Code, totmass, rootshoot, lwc, CO2, meanSWC) %>% 
  left_join(LiCOR_df.[,c("Code","Photo.y","Cond.y", "WUE.350")], by = "Code") %>% 
  left_join(lai.[,c("Code","avg_area","perim_per_A","tot_area", "SLA")], by = "Code") %>% 
  left_join(rootimage.[,c("Code","SRL")], by = "Code") %>% 
  left_join(SIF.[,c("Code","d13C")], by = "Code") %>% 
  mutate(logCond.y = log(Cond.y), sqrtWUE = sqrt(WUE.350), sqrtTotmass = sqrt(totmass), logAvg_area = log(avg_area), logPerimArea = log(perim_per_A), sqrtTotArea = sqrt(tot_area), logSRL = log(SRL)) %>%
  group_by(Plot) %>% 
  dplyr::mutate(n = n()) %>% 
  mutate_at(c("sqrtTotmass", "rootshoot", "lwc", "logAvg_area", "logPerimArea", "sqrtTotArea", "SLA", "logSRL", "d13C", "Photo.y", "logCond.y", "sqrtWUE"), list(oov=one_over_se))  %>% 
  dplyr::summarise(across(where(is.numeric), ~ mean(.x, na.rm=TRUE))) %>% 
  ungroup() %>% 
  left_join(lookup, by = "Plot") %>% 
  mutate(H2OTmt = substr(Tmt,2,2), CO2Tmt = substr(Tmt,1,1)) 

trans_dfVLw <- trans_dfVL %>% 
  mutate(sqrtTotmass_w = sqrtTotmass_oov/(sum(trans_df$sqrtTotmass_oov, na.rm = T)),
         rootshoot_w = rootshoot_oov/(sum(trans_df$rootshoot_oov, na.rm = T)),
         lwc_w = lwc_oov/(sum(trans_df$lwc_oov, na.rm = T)),
         logAvg_area_w = logAvg_area_oov/(sum(trans_df$logAvg_area_oov, na.rm = T)),
         logPerimArea_w = logPerimArea_oov/(sum(trans_df$logPerimArea_oov, na.rm = T)),
         sqrtTotArea_w = sqrtTotArea_oov/(sum(trans_df$sqrtTotArea_oov, na.rm = T)),
         SLA_w = SLA_oov/(sum(trans_df$SLA_oov, na.rm = T)),
         logSRL_w = logSRL_oov/(sum(trans_df$logSRL_oov, na.rm = T)),
         d13C_w = d13C_oov/sum(trans_df$d13C_oov, na.rm = T),
         Photo.y_w = Photo.y_oov/(sum(trans_df$Photo.y_oov, na.rm = T)),
         logCond.y_w = logCond.y_oov/(sum(trans_df$logCond.y_oov, na.rm = T)),
         sqrtWUE_w = sqrtWUE_oov/(sum(trans_df$sqrtWUE_oov, na.rm =T))) %>% 
  mutate(all_w = sqrtTotmass_w+rootshoot_w+lwc_w+Photo.y_w+logCond.y_w+sqrtWUE_w+logAvg_area_w+logPerimArea_w+sqrtTotArea_w+logSRL_w+d13C_w) %>% 
  #       mutate(all_w = logAvg_area_w+logSRL_w+Photo.y_w+logCond.y_w) %>% 
  mutate(all_w = all_w/sum(all_w, na.rm=T)) %>% 
  mutate(quad_w = (sqrtTotmass_w^2)+(rootshoot_w^2)+(lwc_w^2)+(Photo.y_w^2)+(logCond.y_w^2)+(sqrtWUE_w^2)+(logAvg_area_w^2)+(logPerimArea_w^2)+(sqrtTotArea_w^2)+(SLA_w^2)+(logSRL_w^2)+(d13C_w^2)) %>% 
  #mutate(quad_w = (Photo.y_w^2)+(logCond.y_w^2)+(logAvg_area_w^2)+(logSRL_w^2)) %>% 
  mutate(quad_w = quad_w/sum(quad_w, na.rm=T)) 

grid.arrange( # 
  ggpredict(lm(logCond.y~CO2*H2OTmt , data=trans_dfVw, weights=logCond.y_w), terms=c("CO2","H2OTmt"))%>% 
    plot(rawdata = TRUE, ci = TRUE, colors=c("blue","red")),
  ggpredict(lm(logAvg_area ~ CO2*H2OTmt , data = trans_dfVLw), terms=c("CO2","H2OTmt")) %>% 
    plot(rawdata = TRUE, ci = TRUE, colors=c("blue","red")),
  ggplot(trans_dfVLw, aes(x=CO2, y=logAvg_area, group = H2OTmt)) + geom_point(aes(group = H2OTmt, color = H2OTmt, size=logAvg_area_w)) + scale_color_manual(values = c("red", "blue")) , nrow=3 )
summary(lm(logAvg_area~CO2*H2OTmt , data=trans_dfVLw, weights=logAvg_area_w))


### complete pooling of the data (agnostic to species)

grid.arrange( # SLA
  ggpredict(lmer(SLA~CO2*H2OTmt + (1|Plot), data=trans_df_full), terms=c("CO2","H2OTmt"))%>% 
    plot(rawdata = TRUE, ci = TRUE, colors=c("red","blue")),
  ggpredict(lmer(SLA ~ CO2*H2OTmt + (1|Plot), data = trans_df_full), terms=c("CO2","H2OTmt")) %>% 
    plot(rawdata = TRUE, ci = TRUE, colors=c("red","blue")),
  ggplot(trans_df_full, aes(x=CO2, y=SLA, group = H2OTmt)) + geom_point(aes(group = H2OTmt, color = H2OTmt, shape=Spp)) + scale_color_manual(values = c("red", "blue")) , nrow=3 )
summary(lmer(SLA~CO2*H2OTmt + (1|Plot), data=trans_df_full))
# no dice

### partial pooling of the data (species as a random effect: intercept slopes for CO2 and H2O are assigned by species)

grid.arrange( # SLA
  ggpredict(lmer(SLA~CO2*H2OTmt + (1+CO2*H2OTmt|Spp) + (1|Plot), data=trans_df_full), terms=c("CO2","H2OTmt"))%>% 
    plot(rawdata = TRUE, ci = TRUE, colors=c("red","blue")),
  ggplot(trans_df_full, aes(x=CO2, y=SLA, group = H2OTmt)) + geom_point(aes(group = H2OTmt, color = H2OTmt, shape=Spp)) + scale_color_manual(values = c("red", "blue")) , nrow=2 )
summary(lmer(SLA~CO2*H2OTmt + (1+CO2*H2OTmt|Spp) + (1|Plot), data=trans_df_full))


### partial pooling of the data (species as a random effect: random intercepts but not slopes for species)

grid.arrange( # SLA
  ggpredict(lmer(logCond.y~CO2*H2OTmt + (1|Plot) + (1|Spp), data=trans_df_full), terms=c("CO2","H2OTmt"))%>% 
    plot(rawdata = TRUE, ci = TRUE, colors=c("red","blue")),
  ggplot(trans_df_full, aes(x=CO2, y=logCond.y, group = H2OTmt)) + geom_point(aes(group = H2OTmt, color = H2OTmt, shape=Spp)) + scale_color_manual(values = c("red", "blue")) , nrow=2 )
summary(lmer(logCond.y~CO2*H2OTmt + (1|Plot) + (1|Spp) + (1|Plot), data=trans_df_full))


# one more time and I will try the *unweighted* plot means this time

grid.arrange( # sqrtTotmass
  ggpredict(lm(sqrtTotmass~rescale(CO2)*rescale(meanSWC) , data=trans_dfwL, weights=sqrtTotmass_w), terms=c("CO2","meanSWC [4,42]"))%>% 
    plot(rawdata = TRUE, ci = TRUE, colors=c("red","blue")),
  ggpredict(lm(sqrtTotmass ~ rescale(CO2)*rescale(meanSWC) , data = trans_dfwL), terms=c("CO2","meanSWC [4,42]")) %>% 
    plot(rawdata = TRUE, ci = TRUE, colors=c("red","blue")),
  ggplot(trans_dfwL, aes(x=CO2, y=sqrtTotmass, group = H2OTmt)) + geom_point(aes(group = H2OTmt, color = H2OTmt, size=sqrtTotmass_w)) + scale_color_manual(values = c("red", "blue")) , nrow=3 )
summary(lm(sqrtTotmass~rescale(CO2)*rescale(meanSWC) , data=trans_dfwL))

# 11th hour; do it with pseudoreplication
grid.arrange( # rootshoot
  ggpredict(lm(rootshoot~CO2*H2OTmt, data=trans_df_full), terms=c("CO2","H2OTmt"))%>% 
    plot(rawdata = TRUE, ci = TRUE, colors=c("red","blue")),
  ggpredict(lm(rootshoot ~ CO2*H2OTmt, data = trans_df_full), terms=c("CO2","H2OTmt")) %>% 
    plot(rawdata = TRUE, ci = TRUE, colors=c("red","blue")),
  ggplot(trans_df_full, aes(x=CO2, y=rootshoot, group = H2OTmt)) + geom_point(aes(group = H2OTmt, color = H2OTmt, shape=Spp)) + scale_color_manual(values = c("red", "blue")) , nrow=3 )
summary(lm(rootshoot~CO2*H2OTmt, data=trans_df_full))

# CO2_total_corr <- CO2_total %>% 
#   mutate(brokenpump = ifelse( ((TIMESTAMP > "2023-02-15" & TIMESTAMP < "2023-04-07") | (TIMESTAMP > "2023-07-01" & TIMESTAMP < "2023-07-23")) , 1,0)) %>% 
#   mutate(DeltaObs_pred = predict(MFCmod, newdata=CO2_total)) %>% 
# #  ggplot() + geom_point(aes(x=DeltaObs, y=DeltaObs_pred))
#   mutate(DeltaObs_corr = case_when(brokenpump==0 ~ DeltaObs,
#                                    brokenpump==1 ~ DeltaObs_pred)) %>% 
#   mutate(CO2elev_pred = predict(CO2elevmod, newdata=CO2_total)) %>% 
#   mutate(CO2elev_corr = case_when(brokenpump==0 ~ CO2elev,
#                                   brokenpump==1 ~ CO2elev_pred)) %>% 
#   mutate(CO2ref_corr = case_when(brokenpump==0 ~ CO2ref,
#                                  brokenpump==1 ~ CO2ref_mean))

# clean copy of CO2 plot over time
CO2_total_corr %>% 
  group_by(month=cut(TIMESTAMP, breaks = "1 month")) %>% 
  mutate(month=ymd(month)) %>% 
  summarise(medianCO2ref_corr= median(CO2ref_corr), medianCO2elev_corr=median(CO2elev_corr)) %>% 
  ungroup() %>% 
  ggplot() +
  geom_point(aes(x=month, y=medianCO2ref_corr), color="darkgray") +
  geom_point(aes(x=month, y=medianCO2elev_corr), color="black") +
  geom_line(aes(x=month, y=medianCO2ref_corr), color="darkgray") +
  geom_line(aes(x=month, y=medianCO2elev_corr), color="black") +
  scale_x_date(date_breaks="1 month") +
  theme(axis.text.x = element_text(size = 12, angle = 45, hjust = 1), axis.text.y = element_text(size = 12)) +
  labs(title = "eCO2 and aCO2: Monthly Median Values") +
  ylab("CO2 Concentration (ppm)")

ggpredict(lm(Photo.x ~ CO2*SWC, data = filter(LiCOR_df., Spp=="L")), terms=c("CO2", "SWC [4,42]")) %>% 
plot(rawdata = TRUE, ci=TRUE, colors=c("blue","red"))
summary(lm(Photo.x ~ CO2*SWC, data = filter(LiCOR_df., Spp=="L")))

ggpredict(lm(Photo.y ~ CO2*SWC, data = filter(LiCOR_df., Spp=="L")), terms=c("CO2", "SWC [4,42]")) %>% 
  plot(rawdata = TRUE, ci=TRUE, colors=c("blue","red"))
summary(lm(Photo.y ~ CO2*SWC, data = filter(LiCOR_df., Spp=="L")))


# NEVER EVER GIVE UP
# NEVER SURRENDER