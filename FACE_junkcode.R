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
ggpredict(m1, terms=c("CO2","H2OTmt")) %>% 
  plot(add.data = TRUE, ci = FALSE)

cooksd1 <- cooks.distance(m1)
sample_size <- nobs(m1)
plot(cooksd1, pch="*", cex=2, main="Influential Obs by Cooks distance")  # plot cook's distance
abline(h = 4/sample_size, col="red")  # add cutoff line
text(x=1:length(cooksd1)+1, y=cooksd1, labels=ifelse(cooksd1>4/sample_size, names(cooksd1),""), col="red")  # add labels

influential1 <- as.numeric(names(cooksd1)[(cooksd1 > (4/sample_size))])
ggpredict(lmer(Photo.y ~ CO2*H2OTmt + (1|Plot), data = LiCOR_df. %>% 
                 filter(Spp == "V") %>% 
                 filter(!row_number() %in% influential1) ), terms=c("CO2","H2OTmt")) %>% plot(add.data = TRUE, ci = FALSE)
summary(lmer(Photo.y ~ CO2*H2OTmt + (1|Plot), data = LiCOR_df. %>% 
               filter(Spp == "V") %>% 
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

# NEVER EVER GIVE UP
# NEVER SURRENDER