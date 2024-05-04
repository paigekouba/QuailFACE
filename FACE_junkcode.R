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

# NEVER EVER GIVE UP
# NEVER SURRENDER