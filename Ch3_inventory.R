# Chapter 3 Seedling Inventory: Cleaning and Plotting
# 10/12/24

inventory_raw <- read.csv("/Users/paigekouba/Documents/UC_Davis/2021_Winter/Quals/Proposal/Chapter 1/TinyFACE/GitHub/QuailFACE/RawData/Inventory8.csv")

# fix all the dates to the *earliest* day if multi-day inventory; also, fill in blanks
inventory_raw$Inv..1.Date <- "8/2/22"
inventory_raw$Inv..2.Date <- "10/21/22"
inventory_raw$Inv..3.Date <- "2/24/23"
inventory_raw$Inv..4.Date <- "4/21/23"
inventory_raw$Inv..5.Date <- "6/14/23"
inventory_raw$Inv..6.Date <- "8/26/23"
inventory_raw$Inv..7.Date <- "9/30/23"
inventory_raw$Inv..8.Date <- "11/20/23"

# fixing data entry errors
inventory_raw[inventory_raw$Code == "10V5b",25] <- 46 # 26; assume 46 based on previous and subsequent entries
inventory_raw[inventory_raw$Code == "13V5b",25] <- mean(c(43,39))# NA; assign average of adjacent hts
inventory_raw[inventory_raw$Code == "12V4b",25] <- 61.5 # NA; assign average of adjacent hts
inventory_raw[inventory_raw$Code == "4V1b",28] <- 50 # 15; assume "50" misheard
inventory_raw[inventory_raw$Code == "4V1b",49] <- mean(65,73) # NA; assign average of adjacent hts
inventory_raw[inventory_raw$Code == "9V2b",43] <- 116 # 65; assume 116, same as adjacent hts
inventory_raw[inventory_raw$Code == "9V5a",43] <- mean(c(101,84)) # NA; assign mean of adjacent hts
inventory_raw[inventory_raw$Code == "14L5a", 55] <- 450 # 45, assume entry error --> 450

# remove "4V3c" to avoid duplicate code
inventory_raw <- inventory_raw %>% 
  filter(Code!="4V3c") # now 384 rows

# adding to herbivory 
## 11V5 first herb is 8/25/23
## 9V3 first herb is 8/25/23
## 9V5 first herb is 8/25/23
## 7L6 first herb is 6/13/23
## 8L6 first herb is 8/25/23

# taking off herbivory list because their growth curves didn't look bad
# 10L6, 10V4, 11L7, 11L5, 11V1, 11V2, 14L4, 14V3, 15V1, 1V3, 1V5

# how many seedlings germinated per spp per tmt? how many were thinned? how many remained?
# need to associate "Tmt" with "Plot"
lookup <- data.frame(as.character(c(1:16)), c("AW","ED","AD","EW",
                                              "AW","ED","EW","AD",
                                              "ED","AW","EW","AD",
                                              "ED","AW","EW","AD"))
names(lookup) <- c("Plot","Tmt") # now "lookup" has the plots in order with their treatment codes

inventory_raw %>% # get germination rates and thinning counts
  mutate(Plot = as.character(Plot)) %>% 
  left_join(lookup, by = "Plot") %>% 
  group_by(Spp, Tmt) %>% 
  summarise(germts = sum(G. == "Y"), germrate = sum(G. == "Y")*100/48, thinned = sum(Thinned. == "8/26")) %>% 
  mutate(remain = germts - thinned) 
#   Spp   Tmt   germts germrate thinned remain
# 1 "L"   AD        31    64.6       10     21
# 2 "L"   AW        29    60.4       12     17
# 3 "L"   ED        33    68.8       12     21
# 4 "L"   EW        29    60.4       10     19
# 5 "V"   AD        13    27.1        0     13
# 6 "V"   AW        20    41.7        1     19
# 7 "V"   ED        16    33.3        1     15
# 8 "V"   EW        17    35.4        0     17
# Total:           189               46    143

# naming the seedlings things like "14L1a" and "14L1b" was a huge headache. Since each spot initially had two seeds, each seed got a code like: [Plot # 1-16][Spp L or V][location # 1-6][rep a or b]. During inventories we sometimes mistakenly wrote down an a as a b or vice versa. So this part of the code checks every "shortcode," i.e. the Plot+Spp+Location minus the a/b, and picks the value in each column with the higher # or the non-NA value, effectively collapsing the a/b data into one correct measurement series.
# I want to find the max value entered for each XYZ[a/b], since some data was mis-entered
inventory_thinned %>% 
  mutate(shortcode = if_else(nchar(Code)==4,substr(Code,1,3),substr(Code,1,4))) %>% 
  group_by(shortcode) %>% 
  filter(n()>1) %>% 
  nrow() # 384 duplicated seed codes

inventory_thinned <- inventory_thinned %>% # group_by shortcode and get max as summary
  mutate(shortcode = if_else(nchar(Code)==4,substr(Code,1,3),substr(Code,1,4))) %>% 
  group_by(shortcode) %>% 
  summarise_if(is.numeric, ~ max(.x, na.rm = TRUE)) # %>% # this results in 192 rows with the max of each shortcode
# mutate(across(c(ht_mm, dia, cond), na_if, -Inf))

colnames(inventory_thinned)[colnames(inventory_thinned)=="shortcode"] <- "Code"

# now I need to reassociate the date values from inventory_thinned
inventory_thinned$Inv..1.Date <- c(rep("8/2/22", nrow(inventory_thinned)))
inventory_thinned$Inv..2.Date <- c(rep("10/21/22", nrow(inventory_thinned)))
inventory_thinned$Inv..3.Date <- c(rep("2/24/23", nrow(inventory_thinned)))
inventory_thinned$Inv..4.Date <- c(rep("4/21/23", nrow(inventory_thinned)))
inventory_thinned$Inv..5.Date <- c(rep("6/14/23", nrow(inventory_thinned)))
inventory_thinned$Inv..6.Date <- c(rep("8/26/23", nrow(inventory_thinned)))
inventory_thinned$Inv..7.Date <- c(rep("9/30/23", nrow(inventory_thinned)))
inventory_thinned$Inv..8.Date <- c(rep("11/20/23", nrow(inventory_thinned)))

# here we go from wide format, with one column for each measurement x inventory date, to long format with columns for measurement types and unique rows for each seedling x inventory date
# # start with inventory date
inv_long1 <- inventory_thinned %>% 
  select(Code, Inv..1.Date, Inv..2.Date, Inv..3.Date, Inv..4.Date, Inv..5.Date, Inv..6.Date, Inv..7.Date, Inv..8.Date) #%>% 
#  mutate(Code = if_else(nchar(Code)==4,substr(Code,1,3),substr(Code,1,4))) # remove as and bs

inv_long <- inv_long1 %>%
  pivot_longer(!Code, names_to = "Date") %>%
  mutate(value = mdy(value))

# add in the reorganized ht, condition, and dia values
inv_ht <- inventory_thinned %>% # get long-form heights
  select(Code, paste0("Ht.mm..",c(1:8))) %>% 
  pivot_longer(!Code, names_to = "key", values_to = "ht_mm")

inv_cond <- inventory_thinned %>% # get long-form conditions (1-5 where 5=healthy and 1=dead)
  select(Code, paste0("Cond..",c(1:8))) %>% 
  pivot_longer(!Code, names_to = "key", values_to = "cond")

inv_dia <- inventory_thinned %>% # get long-form basal diameter
  select(Code, paste0("Dia.mm..",c(1:8))) %>% 
  pivot_longer(!Code, names_to = "key", values_to = "dia")

inv_leafct <- inventory_thinned %>% # long-form leaf count
  select(Code, c("Leaf.Ct.",paste0("Leaf.Ct..",c(1,5,6,7)))) %>% 
  pivot_longer(!Code, names_to = "key", values_to = "leafct")

inv_maxleaf <- inventory_thinned %>% # long-form max leaf length
  select(Code, c("Max.Leaf.Length.1",paste0("Max.Leaf.",c(5,6,7,8)))) %>% 
  pivot_longer(!Code, names_to = "key", values_to = "maxleaf")

inv_leafct <- inv_leafct %>% 
  mutate(Plot = if_else(nchar(Code)==3,substr(Code,1,1),substr(Code,1,2))) %>% 
  mutate(Spp = substr(Code, nchar(Code)-1,nchar(Code)-1)) %>% 
  left_join(lookup, by = "Plot") %>% 
  mutate(across(c(leafct), na_if, -Inf))

inv_leafct %>% # this was ugly don't do it again
  ggplot() +  geom_point(aes(x=factor(key, levels = unique(key)), y=leafct, color= Tmt, )) + 
  geom_line(aes(x=factor(key, levels = unique(key)), y=leafct, color= Tmt)) +
  facet_wrap(~ Code, scales = "free") 

inv_maxleaf <- inv_maxleaf %>% 
  mutate(Plot = if_else(nchar(Code)==3,substr(Code,1,1),substr(Code,1,2))) %>% 
  mutate(Spp = substr(Code, nchar(Code)-1,nchar(Code)-1)) %>% 
  left_join(lookup, by = "Plot") %>% 
  mutate(across(c(maxleaf), na_if, -Inf))

inv_maxleaf %>% # this was ugly don't do it again
  ggplot() +  geom_point(aes(x=factor(key, levels = unique(key)), y=,maxleaf, color= Tmt, )) + 
  facet_wrap(~ Code, scales = "free") 

inv_all <- cbind(inv_long, inv_ht[,3], inv_dia[,3], inv_cond[,3]) # combine code, date, ht, dia, cond

# NB I did not include leafct and maxleaf in this inv_all because they have a different # of survey dates represented (just 5 rather than 8) and I didn't bother to figure out how to fix that this time because from the plots just above this comment I determined they weren't very cool to look at.

inv_all[which.max(inv_all$dia),] # some outliers need adjusting
#    Code        Date      value ht_mm dia cond
# 776  2L1 Inv..8.Date 2023-11-20   156 389    5
inv_all[776,5] <- 3.89 # it's a decimal error
inv_all[which.max(inv_all$dia),]
#      Code        Date      value ht_mm  dia cond
# 934  3V3 Inv..6.Date 2023-08-26    96 54.9    5
inv_all[934,5] <- 2.69 # mean of Dia.mm..5 and Dia.mm..7
inv_all[which.max(inv_all$dia),]
#     Code        Date      value ht_mm  dia cond
# 1470  9L4 Inv..6.Date 2023-08-26  4.24 37.8    5
inv_all[1470,5] <- 4.24
inv_all[1470,4] <- 100.5 # mean of Ht.mm..5 and Ht.mm..7

# now add Plot and Spp
inv_all <- inv_all %>% 
  mutate(Plot = if_else(nchar(Code)==3,substr(Code,1,1),substr(Code,1,2))) %>% 
  mutate(Spp = substr(Code, nchar(Code)-1,nchar(Code)-1)) %>% 
  left_join(lookup, by = "Plot") %>% 
  mutate(across(c(ht_mm, dia, cond), na_if, -Inf))

# examine conditions and see how many get to 1
inv_all %>% 
  ggplot() +  geom_point(aes(x=value, y=cond, color= Tmt)) + facet_wrap(~ Code, scales = "fixed")

# # to accurately compare all the growth curves, need to account for herbivory, mostly starting 7/19/23, between Inv 5 and 6
# that also happens to be when the growth curves start to diverge (most likely due to the differential watering starting that summer but taking a while to show up due to a very wet winter/spring)
# (drip irrigation installed Sunday Aug 28, 2022, but got the same watering for the first summer. Would like to add layer of ppt + supplementary water)

# need to read in the long-form table of herbivory data
herbivory <- read.csv("/Users/paigekouba/Documents/UC_Davis/2021_Winter/Quals/Proposal/Chapter 1/TinyFACE/GitHub/QuailFACE/RawData/OakFACE Herbivory - Sheet1.csv")
herbivory$Date <- mdy(herbivory$Date)
# group by code, find min(date) for rows with no NA in [full / any]_herbivory
# Find all Code x date combos where date > date of first herbivory, exclude those

firstherb <- herbivory %>% # most conservative: first date *any* herbivory was observed
  group_by(Code) %>% 
  summarise(firstherb = min(Date)) %>% # after reviewing the initial plots, I found some seedlings in the "herbivory" subset that didn't lose enough height to matter, at least for stem mass, and some seedlings that evidently suffered stem herbivory (got shorter by like >10%) but I didn't notice in my surveys.
  # I checked this against leaf herbivory: compare ht.8 vs leaf mass and look for outliers. 1V6 was funky, added to herbivory list to be safe
  filter(!(Code %in% c("10L6","10V4","11L7","11L5","11V1","11V2","14L4","14V3","15V1","1V3","1V5"))) 
# add in these:
new_ones <- data.frame(c("11V5","9V3", "9V5","7L6","8L6"),c("8/25/23","8/25/23","8/25/23", "6/13/23", "8/25/23"))
colnames(new_ones) <- c("Code", "firstherb")
new_ones <- new_ones %>% 
  mutate(firstherb = mdy(firstherb))
firstherb <- rbind(firstherb, new_ones)

firstfullherb <- herbivory %>% # first date *full stem herbivory* (ie completely severed stem) was observed
  group_by(Code) %>% 
  filter(full_herb == "x") %>% 
  summarise(firstherb = min(Date)) %>% 
  filter(!(Code %in% c("10L6","10V4","11L7","11L5","11V1","11V2","14L4","14V3","15V1","1V3","1V5"))) 
firstfullherb <- rbind(firstfullherb, new_ones)

# also note that of 45 seedlings with evidence of herbivory, fully 34 also showed evidence of resprouting! Found no evidence that this was significantly related to Treatment group, but seems marginally associated with being a L vs a V.

# find Codes for seedlings with any herbivory and drop after the date of first herbivory
inv_all_nh0 <- right_join(firstherb, inv_all, by = "Code") 
inv_all_nfh0 <- right_join(firstfullherb, inv_all, by = "Code") 

inv_all_nh0[is.na(inv_all_nh0$firstherb),2] <- ymd("9999-09-09") # if not listed in firstherb (ie NA), assign a dummy date >> any in the surveys
inv_all_nfh0[is.na(inv_all_nfh0$firstherb),2] <- ymd("9999-09-09")

inv_all_nh <- inv_all_nh0 %>% 
  filter(firstherb > value)

inv_all_nfh <- inv_all_nfh0 %>% 
  filter(firstherb > value)
# this keeps all the data for the herbivorized seedlings *up until* they get eaten

# get counts per sampling date, species and tmt 
inv_nequals <- inv_all_nfh %>%
  filter(!is.na(dia)) %>% 
  group_by(Spp, Tmt, value) %>%
  tally()

inv_all_nfh %>% # for seedlings without *full* herbivory, how did height change over the course of the study?
  group_by(Spp, Tmt, value) %>% 
  summarise(mean_ht = mean(ht_mm, na.rm = TRUE),sd_ht = sd(ht_mm, na.rm = TRUE)) %>% 
  # summarise(mean_dia = mean(dia, na.rm = TRUE),sd_dia = sd(dia, na.rm = TRUE)) %>% 
  # summarise(mean_cond = mean(cond, na.rm = TRUE),sd_cond = sd(cond, na.rm = TRUE)) %>% 
  ggplot(aes(x=value, y = mean_ht, group = Tmt)) + 
  geom_line(aes(color = Tmt), position = position_dodge(20, preserve = "total"), linewidth = 1.2) +
  geom_pointrange(aes(ymin = mean_ht - sd_ht, ymax = mean_ht + sd_ht, color = Tmt, shape = Tmt), position = position_dodge(20, preserve = "total"), size = 1.5, alpha = 0.75) + 
  # geom_pointrange(aes(ymin = mean_dia - sd_dia, ymax = mean_dia + sd_dia, color = Tmt, shape = Tmt), position = position_dodge(20, preserve = "total"), size = 1.5, alpha = 0.75) + 
  # geom_pointrange(aes(ymin = mean_cond - sd_cond, ymax = mean_cond + sd_cond, color = Tmt, shape = Tmt), position = position_dodge(20, preserve = "total"), size = 1.5, alpha = 0.75) + 
  scale_color_manual(values = c("#e8665d", "#828cfa", "#850a01", "#010c85")) +
  scale_shape_manual(values = c(16,16,17,17)) +
  geom_text(data = inv_nequals, aes(x = value, y = as.numeric(as.factor(Tmt))*12-50, color = Tmt, label = paste0("N=",n))) +
  facet_grid(~ Spp)


## Back-filling biomass data
# let's look at the growth curves for this subset of seedlings

# view the growth curves (actual) for all seedlings with herbivory
inv_all[which(inv_all$Code %in% firstherb$Code),] %>% # (add ! after which( to get all without herb)
  na.omit() %>% 
  group_by(Spp, Tmt, value, Code) %>% 
  # summarise(mean_ht = mean(ht_mm, na.rm = TRUE),sd_ht = sd(ht_mm, na.rm = TRUE)) %>% 
  # ggplot(aes(x=value, y = mean_ht, group = Tmt)) + 
  # geom_pointrange(aes(ymin = mean_ht - sd_ht, ymax = mean_ht + sd_ht, color = Tmt, shape = Tmt), position = position_dodge(20, preserve = "total"), size = 1.5, alpha = 0.75) + 
  ggplot(aes(x=value, y=ht_mm,  group = Code, color = Tmt, shape = Tmt, size = 1.5, alpha = 0.75)) +
  geom_line(aes(x=value, y=ht_mm,  group = Code), position = position_dodge(20, preserve = "total"), linewidth = 1.2, na.rm = TRUE) +
  geom_point(position = position_dodge(20, preserve = "total")) +
  scale_color_manual(values = c("#e8665d", "#828cfa", "#850a01", "#010c85")) +
  scale_shape_manual(values = c(16,16,17,17)) +
  facet_grid(rows=vars(Spp), scales = "free")
# okay we have checked it out; they for sure get shorter

# In another script I have made some linear models using inventory heights before the vole onslaught (~ inventory 5) to predict stem and leaf mass for seedlings that got eaten; it's a little rough but probably better than nothing. I have not done the same for inventory heights, although that could also be interesting too. Here is a first stab at it

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
