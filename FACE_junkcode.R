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

# NEVER EVER GIVE UP
# NEVER SURRENDER