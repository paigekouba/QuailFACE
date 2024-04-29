# 4/21/24
# Performance Testing for Ch 2
# Using 4/9-10 data

# load the data
testing <- read_csv("/Users/paigekouba/Documents/UC_Davis/2021_Winter/Quals/Proposal/Chapter 1/TinyFACE/CO2_control/CR3000_Data20s_4.19.24.csv", skip = 4, col_names = c("TIMESTAMP",	"RECORD",	"BattV_Avg",	"CO2signal",	"CO2ref",	"CO2elev",	"CO2test",	"DeltaObs",	"DeltaSet",	"LicorSig2",	"FlowMFC",	"AirTC",	"RH",	"WS_ms",	"PARuE",	"TurnCO2On",	"SpanObs",	"SpanAct",	"ZeroObs",	"ZeroAct"))

attr(testing$TIMESTAMP, "tzone") # starts out as UTC, though clock time is correct
# force_tz keeps the clock time but reassigns the tz to be accurate
testing$TIMESTAMP <- force_tz(testing$TIMESTAMP, "America/Los_Angeles")

# examine CO2 performance after 4.9.24
post4.9.24 <- testing %>% 
  filter(TIMESTAMP > "2024-04-09 08:10:00") %>%
  mutate(DeltaTest = CO2test - CO2ref) %>%
  distinct() # remove duplicate rows

post4.9.24_daytime %>% 
  select(TIMESTAMP, CO2elev, CO2ref) %>% 
  dygraph() %>% 
  dySeries(drawPoints=TRUE, strokeWidth=0) %>% 
  #dyOptions(drawPoints = TRUE) %>% 
  #dyRoller(rollPeriod = 180) %>% 
  dyRangeSelector()

# Fig 1
daytimeCO2_4.9to4.19 <- ggplot(post4.9.24_daytime, aes(x=DeltaObs))+
  geom_density()+
  geom_vline(aes(xintercept = mean(DeltaObs)),color = "red", linetype="dashed", size=1)+
  labs(title="Daytime CO2 Elevation: 4/9-4/19/24", 
       x= "∆ CO2 (ppm)",
       y= "Density")+
  theme_classic()

post4.9.24_daytime <- post4.9.24 %>% 
  filter(PARuE > 50) 
mean(post4.9.24_daytime$DeltaObs) # 196.8225
sd(post4.9.24_daytime$DeltaObs) # 63.57922
sum(post4.9.24_daytime$DeltaObs < 200*1.1 & post4.9.24_daytime$DeltaObs > 200*0.9)/nrow(post4.9.24_daytime) # 32.5% of the time within 10%
sum(post4.9.24_daytime$DeltaObs < 200*1.2 & post4.9.24_daytime$DeltaObs > 200*0.8)/nrow(post4.9.24_daytime) # 56.8% of the time within 20%
sum(post4.9.24_daytime$DeltaObs < 200*1.25 & post4.9.24_daytime$DeltaObs > 200*0.75)/nrow(post4.9.24_daytime) # 67.0% of the time within 25%
sum(post4.9.24_daytime$CO2elev < (post4.9.24_daytime$CO2ref+200)*1.2 & post4.9.24_daytime$CO2elev > (post4.9.24_daytime$CO2ref+200)*0.8)/nrow(post4.9.24_daytime) # 95.1% of the time within 20%

# what is the mathematical difference between l30 and l32?
# l30 has narrower bounds: focused only on ∆Obs, asking if it's within 160-240ppm (within 20% of 200)
# l32 looks at overall CO2 levels: asks if eCO2 is within 20% of [aCO2 ± 200], a range of about 582-873ppm
# * need to find out what other papers are using for this statistic *

# CO2 consumption per area, PAR > 50
20*mean(post4.9.24_daytime$FlowMFC)/5000 # 20L/min max * FlowMFC (mV) / 5000 mV max = 3.968844 L/min average
3.968844*1440 # L/min * min/d = 5715.135 L/d when PAR > 50
5715.135*0.001836 # L/d * kg/L = 10.49299 kg/d
10.49299/4 # kg/d / 4m2 plot area = 2.623248 kg/m2/d when PAR > 50
# cf Leadley, 6.10 kg/d/m2 (24h) ~3.05 kg/d/m2 during daylight
# nrow(post4.9.24_daytime)*20/60 # = 7146.7 min PAR > 50
# nrow(post4.9.24)*20/60 # = 14889.67 min total

# CO2 consumption per area, 24h
20*mean(post4.9.24$FlowMFC)/5000 # 20L/min max * FlowMFC (mV) / 5000 mV max = 2.479395 L/min average
2.479395*1440 # L/min * min/d = 3570.329 L/d 
3570.329*0.001836 # L/d * kg/L = 6.555124 kg/d
6.555124/4 # kg/d / 4m2 plot area = 1.638781 kg/m2/d 
# cf Leadley, 6.10 kg/d/m2 (24h) 


# Spatial Performance
testing <- testing %>% 
  filter(TIMESTAMP > "2024-04-09 08:10:00") %>% # subset to just the 2 days of quality testing in April 2024
  filter(TIMESTAMP < "2024-04-10 17:10:00")
nrow(testing) #6247
nrow(distinct(testing)) #5970
# why would there be duplicate rows? Is this a problem in the other CO2 data?

testing %>%
  # select(TIMESTAMP, CO2ref, CO2elev, CO2test, DeltaObs) %>% 
  mutate(DeltaTest = CO2test - CO2ref) %>% 
  select(TIMESTAMP, DeltaObs, DeltaTest) %>% 
  dygraph() %>% 
  dyRoller(rollPeriod = 180) %>% 
  dyRangeSelector()
testing <- distinct(testing)

# overall mean and sd for ∆Obs
testing %>% 
     filter(PARuE > 50) %>% 
     select(DeltaObs) %>%  
     colMeans()

# above-plot testing, MFCBase = 1100
# starting at 8:10 (10cm), until 11:10 (100cm) above, by 20m
pos_above <- c(paste0(seq(10,100,by=10),"cm"))
above_4.9.24 <- testing %>% 
  filter(TIMESTAMP < "2024-04-09 11:30:00") %>% 
  select(TIMESTAMP, CO2ref, CO2elev, CO2test, DeltaObs) %>% 
  mutate(DeltaTest = CO2test - CO2ref) %>% 
  mutate(timestep = floor_date(TIMESTAMP - 10*60, unit = "20 minutes"), 
        position = pos_above[factor(timestep)]) 
  #summarize(mDeltaTest = mean(DeltaTest), sd = sd(DeltaTest))
  #group_by(TIMESTAMP = cut(TIMESTAMP, breaks = "20 min")) %>% 
above_4.9.24$position <- factor(above_4.9.24$position, levels = unique(above_4.9.24$position))
mean(above_4.9.24[above_4.9.24$position == "10cm",]$DeltaTest) #  200.9993
sd(above_4.9.24[above_4.9.24$position == "10cm",]$DeltaTest) # 16.18553

above_means <- above_4.9.24 %>% 
  group_by(position) %>% 
  summarise(mean = mean(DeltaTest), sd = sd(DeltaTest)) %>% 
  ungroup()
colMeans(above_means[1:4,2]) # 198.3428  is the mean within the screen

above_4.9.24 %>%  # density curves for each timestep
  ggplot(aes(x=DeltaTest, group = position)) + geom_density(aes(color = position))

above_boxplots <- above_4.9.24 %>% # box plots
  ggplot(aes(x=position, y=DeltaTest)) + geom_boxplot() + coord_flip() + 
  annotate("rect", xmin=0, xmax=4.3, ymin=0, ymax=Inf, alpha=0.5) +
  geom_hline(yintercept=200,color = "red", linetype="dashed", size=1) +
  ylab("∆CO2") + xlab("Height Above Plot Center")

# within-plot testing, MFCBase = 1000
# starting at 11:40, changing every 20m
# spots in this order: A, B, F, G, K, L, C, D, H, I, E, J
position <- c("A", "B", "F", "G", "K", "L", "C", "D", "H", "I", "E", "J")
x_within <-  c(-13, 13, -13, 13, -13, 13, -26, 0, -26, 0, 26, 26)
y_within <- c(26, 26, 0, 0, -26, -26, 13, 13, -13, -13, 13, -13)
points_within <- data.frame(x_within, y_within, position)
ggplot(points_within, aes(x=x_within, y=y_within, label = position)) + geom_text()

within_4.9.24 <- testing %>% 
  filter(TIMESTAMP > "2024-04-09 11:40:00") %>% 
  filter(TIMESTAMP < "2024-04-09 15:40:00") %>% 
  select(TIMESTAMP, CO2ref, CO2elev, CO2test, DeltaObs) %>% 
  mutate(DeltaTest = CO2test - CO2ref) %>% 
  mutate(timestep = floor_date(TIMESTAMP, unit = "20 minutes"), # assigns a time-group ID
         position = pos_within[factor(timestep)]) # turns time-group into corresponding position ID
#summarize(mDeltaTest = mean(DeltaTest), sd = sd(DeltaTest))
#group_by(TIMESTAMP = cut(TIMESTAMP, breaks = "20 min")) %>% 
mean(within_4.9.24$DeltaTest) # 198.9011
sd(within_4.9.24$DeltaTest) # 53.36563

within_4.9.24$position <- factor(within_4.9.24$position, levels = position)

within_4.9.24 %>%  # density curves for each timestep
  ggplot(aes(x=DeltaTest, group = position)) + geom_density(aes(color = position))
within_4.9.24 %>%  # density curve over all timesteps
  ggplot(aes(x=DeltaTest)) + geom_density()

within_4.9.24 %>% # box plots
  ggplot(aes(x=position, y=DeltaTest)) + geom_boxplot()

# assign spots to points in a grid, then try "interp" package to make isolines!!
# get x and y associated with CO2 table
within_4.9.24 <- left_join(within_4.9.24, points_within, by = "position")
within_4.9.24$position <- factor(within_4.9.24$position, levels = position)
library(interp)
library(akima)
library(purrr)

means_within <- within_4.9.24 %>% 
  group_by(position) %>% 
  summarise(mean = mean(DeltaTest), sd = sd(DeltaTest)) 
means_within <- left_join(means_within, points_within, by = "position")

# add one more point 10cm above the center
mean(above_4.9.24[above_4.9.24$position == "10cm",]$DeltaTest) #  200.9993
means_within <- rbind(means_within,c(0,as.numeric(mean(above_4.9.24[above_4.9.24$position == "10cm",]$DeltaTest)),sd(above_4.9.24[above_4.9.24$position == "10cm",]$DeltaTest), as.numeric(0), as.numeric(0)))

# find out average difference from sampling point to center
# do sampling point (by row) - same row's value for eCO2
within_4.9.24 %>% 
  mutate(diff_center = (CO2test - CO2elev)*100/CO2elev) %>% 
  group_by(position) %>% 
  summarize(mean = mean(diff_center)) %>% 
  summarise(sd(mean)) 

# gridded bicubic spline interpolation from akima
interpolated <- interp(x = means_within$x_within, y = means_within$y_within, z = means_within$mean, linear=FALSE, extrap = FALSE)
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
  geom_point(data = interp_df, aes(x = x, y = y, color = DeltaTest), size=4.3, shape=15) +
  scale_color_continuous(type = "viridis") + theme_classic() +
  geom_text(data = means_within, aes(x=x_within, y=y_within, label = round(mean, 1)), size = 5)
  #geom_text(data = summarise(group_by(within_4.9.24, position), mean=round(mean(DeltaTest),1)), aes(x=x_within, y=y_within, label = mean), size=5)

ggplot(data = interp_df, aes(x = x, y = y)) +
  geom_contour(aes(z = DeltaTest, colour = stat(level)), size = 1.2) +
  metR::geom_text_contour(aes(z=DeltaTest), stroke = 0.15) +
  coord_equal()

# also, mean and sd at different spots in the grid
within_4.9.24 %>% 
  group_by(as.factor(position)) %>% 
  summarise(mean = round(mean(DeltaTest), 1), sd = round(sd(DeltaTest), 1)) %>% 
  ggplot(aes(x=x_within, y=y_within, label = mean)) + geom_text(aes(color=mean), size = 5)

ggplot(means_within, aes(x=x_within, y=y_within, label = round(mean,1))) + geom_text(aes(color=mean), size = 5) 

# compass points in 10cm increments, 20m timesteps, starting at 15:40
# N, E, S (replace), W, N+20, E+20, S+20, W+20(replace), 
# @ 18:24 N+30, E+30, S+30, W+30, S redo, W+20 redo
# 4/10, 14:25 @ 25s, N+40, E+40, S+40, W+40; 15:50 N+50, W+50, S+50, E+50

# adding outside-screen points to inside-screen points: (1) 15:40-18:00, (2) 18:24-2024 on 4/9; 
# (3) 14:25-15:45, (4) 15:50-17:10 on 4/10

# developing list of position IDs in the order they were measured
c(paste0(rep(c("No","Ea","So","We"),5), "+", rep(seq(10,50,10), each=4)))
pos_out <- c("No+10", "Ea+10", "So+10", "We+10", "No+20", "Ea+20", "So+20", "No+30", "Ea+30", "So+30", "We+30", "So+10.1","We+20","No+40", "Ea+40", "So+40", "We+40", "No+50", "Ea+50", "So+50", "We+50")
pos_out <- factor(pos_out, levels = unique(pos_out))

# get x and y coordinates for outside-screen positions
x_out <- vector()
for(i in 1:5){
  x_i <- c(0,39+(i*10),0,-39-(i*10))
  x_out <- append(x_out, x_i)
}
x_out <- c(0,  49,   0, -49,   0,  59,   0,   0,  69,   0, -69, 0, -59,   0,  79,   0, -79,   0,  89,   0, -89)
y_out <- vector()
for(i in 1:5){
  y_i <- c(39+(i*10),0,-39-(i*10),0)
  y_out <- append(y_out, y_i)
}
y_out <- c(49,   0, -49,   0,  59,   0, -59, 69,   0, -69,   0, -49, 0,  79,   0, -79,   0,  89,   0, -89,   0)
points_out <- data.frame(x_out, y_out, pos_out)
points_out$pos_out <- factor(pos_out, levels = unique(pos_out))
ggplot(points_out, aes(x=x_out, y=y_out, label = pos_out)) + geom_text() #+coord_flip()

# broken into four chunks due to timestep errors or dropped points
CO2_out1 <- testing %>% # first 7
  filter(TIMESTAMP > "2024-04-09 15:40:00") %>% 
  filter(TIMESTAMP < "2024-04-09 18:00:00") %>% 
  select(TIMESTAMP, CO2ref, CO2elev, CO2test, DeltaObs) %>% 
  mutate(DeltaTest = CO2test - CO2ref) %>% 
  mutate(timestep = floor_date(TIMESTAMP, unit = "20 minutes"), 
          position = pos_out[factor(timestep)])

CO2_out2 <- testing %>% # 8-13
  filter(TIMESTAMP > "2024-04-09 18:24:00") %>% 
  filter(TIMESTAMP < "2024-04-09 20:24:00") %>% 
  select(TIMESTAMP, CO2ref, CO2elev, CO2test, DeltaObs) %>% 
  mutate(DeltaTest = CO2test - CO2ref) %>% 
  mutate(timestep = floor_date(TIMESTAMP-(4*60), unit = "20 minutes"), 
         position = pos_out[8:13][factor(timestep)]) 

CO2_out3 <- testing %>% # 14-17
  filter(TIMESTAMP > "2024-04-10 14:25:00") %>% 
  filter(TIMESTAMP < "2024-04-10 15:45:00") %>% 
  select(TIMESTAMP, CO2ref, CO2elev, CO2test, DeltaObs) %>% 
  mutate(DeltaTest = CO2test - CO2ref) %>% 
  mutate(timestep = floor_date(TIMESTAMP-(5*60), unit = "20 minutes"), 
         position = pos_out[14:17][factor(timestep)]) 

CO2_out4 <- testing %>% # 18-21
  filter(TIMESTAMP > "2024-04-10 15:50:00") %>% 
  filter(TIMESTAMP < "2024-04-10 17:10:00") %>% 
  select(TIMESTAMP, CO2ref, CO2elev, CO2test, DeltaObs) %>% 
  mutate(DeltaTest = CO2test - CO2ref) %>% 
  mutate(timestep = floor_date(TIMESTAMP+(10*60), unit = "20 minutes"), 
         position = pos_out[18:21][factor(timestep)]) 

CO2_out <- rbind(CO2_out1, CO2_out2, CO2_out3, CO2_out4)
# filter out S+10, and corresponding point, since PAR dropped below threshold during that step
CO2_out <- CO2_out %>% 
  filter(position != "So+10")
points_out <- points_out[-3,]

# get x y data into CO2 df
CO2_out <- left_join(CO2_out, points_out, by="position")

# average CO2 level for each distance from screen for CO2_out
CO2_out %>% 
  rowwise() %>% 
  mutate(distance = max(abs(x),abs(y))) %>% 
  mutate(diff_center = (CO2test - CO2elev)*100/CO2elev) %>% View()
  summarize(mean = mean(CO2test)) %>% 

CO2_out %>% # box plots
  ggplot(aes(x=position, y=DeltaTest)) + geom_boxplot()
CO2_out %>%  # density curves for each timestep
  ggplot(aes(x=DeltaTest, group = position)) + geom_density(aes(color = position))

CO2_out %>% 
  group_by(position) %>% 
  summarise(mean = mean(DeltaTest), sd = sd(DeltaTest)) %>% 
  left_join(points_out, join_by(position == pos_out)) %>% 
  ggplot(aes(x=x_out, y=y_out, label = round(mean,1))) + geom_text(aes(color=sd), size = 5) + coord_flip()

# join outside screen data with inside screen data
names(within_4.9.24)
names(CO2_out)
names(within_4.9.24)[9] <- "x"
names(within_4.9.24)[10] <- "y"
names(CO2_out)[9] <- "x"
names(CO2_out)[10] <- "y"

in.out_4.9.24 <- rbind(within_4.9.24, CO2_out)

# need same colnames for rbind
names(points_out) <- c("x", "y", "position")
names(points_within) <- c("x", "y", "position")
allpoints <- rbind(points_out, points_within) 

in.out_4.9.24 %>% # means and sd for inside and outside points
   group_by(position) %>% 
   summarise(mean = mean(DeltaTest), sd = sd(DeltaTest)) %>% 
   left_join(allpoints, by = "position") %>% 
   ggplot(aes(x=x, y=y, label = round(mean,1))) + geom_text(aes(color=sd), size = 5) 

in.out_means <- in.out_4.9.24 %>% # df with means already in it
  group_by(position) %>% 
  summarise(mean = mean(DeltaTest), sd = sd(DeltaTest)) 
in.out_means <- left_join(in.out_means, allpoints, by = "position")

# interpolation function for data including outside-screen points
interpolated2 <- interp(x = in.out_means$x, y = in.out_means$y, z = in.out_means$mean, linear=TRUE, extrap = TRUE, remove=FALSE)
#convert this to a long form dataframe
interp_df2 <- expand_grid(i = seq_along(interpolated2$x), 
                         j = seq_along(interpolated2$y)) %>% 
  mutate(x = interpolated2$x[i],
         y = interpolated2$y[j],
         DeltaTest = map2_dbl(i, j, ~interpolated2$z[.x,.y])) %>% 
  select(-i, -j)

# visualize all interpolated points
ggplot(data = interp_df2, aes(x = x, y = y, color = DeltaTest)) + geom_point(size=6) + scale_color_continuous(type = "viridis") + theme_classic()

interpolated_CO2 <- ggplot() + # interpolated points plus measured values  (means) plus circle showing screen
  geom_point(data = interp_df2, aes(x = x, y = y, color = DeltaTest), size=5, shape=15) +
  scale_color_continuous(type = "viridis") + theme_classic() +
  geom_text(data = in.out_means, aes(x=x, y=y, label = round(mean, 1)), size = 5) +
  ggforce::geom_circle(aes(x0=0,y0=0,r=39))

ggplot(data = interp_df2, aes(x = x, y = y)) + # contour map, kinda cool
  geom_contour(aes(z = DeltaTest, colour = stat(level)), size = 1.2) +
  metR::geom_text_contour(aes(z=DeltaTest), stroke = 0.15) +
  scale_color_continuous(type = "viridis") +
    coord_equal() +
  ggforce::geom_circle(aes(x0=0,y0=0,r=39))


# between-plot testing
# at 7:25, 20m timesteps, plot 1-16

between_4.10.24 <- testing %>%
  filter(TIMESTAMP > "2024-04-10 07:25:00") %>%
  filter(TIMESTAMP < "2024-04-10 12:45:00") %>%
  select(TIMESTAMP, CO2ref, CO2elev, CO2test, DeltaObs) %>%
  mutate(DeltaTest = CO2test - CO2ref) %>%
  mutate(timestep = floor_date(TIMESTAMP - 5*60, unit = "20 minutes"),
         position = factor(c(1:16)[factor(timestep)])) %>% 
  mutate(eCO2 = as.numeric(position %in% c(2,3,6,7,9,11,13,15))) 

between_4.10.24 %>% 
  group_by(position) %>% 
  summarise(mean = mean(DeltaTest), sd = sd(DeltaTest)) %>% 
  ungroup() %>% View()

# NB I think plots 3 and 4 got swapped when I rebuilt the thing!! 3 is E now and 4 is A

mean(between_4.10.24[between_4.10.24$position %in% c(1,4,5,8,10,12,14,16),]$DeltaTest) # 13.0398
sd(between_4.10.24[between_4.10.24$position %in% c(1,4,5,8,10,12,14,16),]$DeltaTest) # 47.83779

mean(between_4.10.24[between_4.10.24$position %in% c(2,3,6,7,9,11,13,15),]$DeltaTest) # 209.8614
sd(between_4.10.24[between_4.10.24$position %in% c(2,3,6,7,9,11,13,15),]$DeltaTest) # 56.70083

#sum(between_4.10.24$CO2elev < (between_4.10.24$CO2ref+200)*1.2 & between_4.10.24$CO2elev > (between_4.10.24$CO2ref+200)*0.8)/nrow(between_4.10.24) # 99.4%

between_4.10.24 %>% 
  filter(eCO2 == 1) %>% 
  mutate(within10 = (CO2elev < (CO2ref+200)*1.1 & CO2elev > (CO2ref+200)*0.9)) %>% 
  mutate(within20 = (CO2elev < (CO2ref+200)*1.2 & CO2elev > (CO2ref+200)*0.8)) %>% 
#  summarise(sum20 = sum(within20)) 
  summarise(sum10 = sum(within10)) 
465/nrow(between_4.10.24[between_4.10.24$eCO2==1,]) # 96.9% within 10%; 100% within 20%

between_4.10.24 %>%  # density curves for each timestep
  ggplot(aes(x=DeltaTest, group = factor(position))) + geom_density(aes(color = factor(eCO2, levels=c("1","0")))) + labs(colour = "eCO2") + xlab("∆CO2")

between_boxplots <- between_4.10.24 %>% # box plots
  ggplot(aes(x=factor(position), y=DeltaTest)) + geom_boxplot(aes(color=factor(eCO2, levels=c("1","0")))) +
  labs(colour = "eCO2") + xlab("Plot") + ylab("∆CO2") +
  geom_hline(yintercept=200,color = "red", linetype="dashed", size=1)

# 12:45 short veg, canopy height
# 13:05 medium veg, canopy height
# 13:25 tall veg, canopy height
# 13:45 medium, 10cm
# 14:05 tall, 10cm

veg_4.10.24 <- testing %>%
  filter(TIMESTAMP > "2024-04-10 12:45:00") %>%
  filter(TIMESTAMP < "2024-04-10 14:25:00") %>%
  select(TIMESTAMP, CO2ref, CO2elev, CO2test, DeltaObs) %>%
  mutate(DeltaTest = CO2test - CO2ref) %>%
  mutate(timestep = floor_date(TIMESTAMP - 5*60, unit = "20 minutes"),
         position = c("short_10cm@canopy","med_25-35cm@canopy","tall_50-60cm@canopy","med_@10cm","tall_@10cm")[factor(timestep)])

veg_4.10.24$position <- factor(veg_4.10.24$position, levels = unique(veg_4.10.24$position))

veg_4.10.24 %>% 
  group_by(position) %>% 
  summarise(mean = mean(DeltaTest), sd = sd(DeltaTest)) %>% 
  ungroup() %>% View()

veg_4.10.24 %>%  # density curves for each timestep
  ggplot(aes(x=DeltaTest, group = factor(position))) + geom_density(aes(color = position))

veg_boxplot <- veg_4.10.24 %>% # box plots
  ggplot(aes(x=position, y=DeltaTest)) + geom_boxplot() +
  geom_hline(yintercept=200,color = "red", linetype="dashed", size=1) +
  xlab("Vegetation Height & Sampling Height") + ylab("∆CO2")
