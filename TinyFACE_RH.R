# RH variation within and outside a plot
# not including a buffer period this time since all measurements are within one eCO2 screen

library(tidyverse)
library(interp)

# Load the data: CO2 performance, light penetration data, temperature data
testing <- read_csv("/Users/paigekouba/Documents/UC_Davis/2021_Winter/Quals/Proposal/Chapter 1/TinyFACE/GitHub/QuailFACE/TinyFACE_performance.csv")
PAR_df <- read.csv("/Users/paigekouba/Documents/UC_Davis/2021_Winter/Quals/Proposal/Chapter 1/TinyFACE/GitHub/QuailFACE/RawData/tinyFACE_PAR.csv")
temp_df <- read.csv("/Users/paigekouba/Documents/UC_Davis/2021_Winter/Quals/Proposal/Chapter 1/TinyFACE/GitHub/QuailFACE/RawData/tinyFACE_T.csv")

attr(testing$TIMESTAMP, "tzone") # starts out as UTC, though clock time is correct
# force_tz keeps the clock time but reassigns the tz to be accurate
testing$TIMESTAMP <- force_tz(testing$TIMESTAMP, "America/Los_Angeles")


# within-plot testing, MFCBase = 1000
# starting at 11:40, changing every 20m
# spots in this order: A, B, F, G, K, L, C, D, H, I, E, J
position <- c("A", "B", "F", "G", "K", "L", "C", "D", "H", "I", "E", "J")
x_within <-  c(-13, 13, -13, 13, -13, 13, -26, 0, -26, 0, 26, 26)
y_within <- c(26, 26, 0, 0, -26, -26, 13, 13, -13, -13, 13, -13)
points_within <- data.frame(x_within, y_within, position)
#ggplot(points_within, aes(x=x_within, y=y_within, label = position)) + geom_text()
#(shows the layout of sampling points)

within_4.9.24 <- testing %>% 
  filter(TIMESTAMP > "2024-04-09 11:40:00") %>% 
  filter(TIMESTAMP < "2024-04-09 15:40:00") %>% # subset to within-plot spatial test on 4/9/24
  select(TIMESTAMP, CO2ref, CO2elev, CO2test, DeltaObs, RH) %>% 
  mutate(DeltaTest = CO2test - CO2ref) %>% 
  mutate(RH = -1*RH) %>% 
  mutate(timestep = floor_date(TIMESTAMP, unit = "20 minutes"), # assigns a time-group ID
         position = position[factor(timestep)]) # turns time-group into corresponding position ID
#summarize(mDeltaTest = mean(DeltaTest), sd = sd(DeltaTest))
#group_by(TIMESTAMP = cut(TIMESTAMP, breaks = "20 min")) %>% 
mean(within_4.9.24$DeltaTest) # 198.9011
sd(within_4.9.24$DeltaTest) # 53.36563

within_4.9.24$position <- factor(within_4.9.24$position, levels = position)

# assign spots to points in a grid, then use "interp" package to make figure
# get x and y associated with CO2 table
within_4.9.24 <- left_join(within_4.9.24, points_within, by = "position")
within_4.9.24$position <- factor(within_4.9.24$position, levels = position)

means_within <- within_4.9.24 %>% 
  group_by(position) %>% 
  summarise(mean = mean(RH), sd = sd(RH)) 
means_within <- left_join(means_within, points_within, by = "position")

min(means_within$mean) # 160.9 ppm is min avg for a single sampling point
max(means_within$mean) # 256.1 ppm is max avg for a single sampling point

# RH levels outside the screen; see below (l. 262)

# gridded bicubic spline interpolation from akima
interpolated <- interp(x = means_within$x_within, y = means_within$y_within, z = means_within$mean, linear=FALSE, extrap = FALSE)
#convert this to a long form dataframe
interp_df <- expand_grid(i = seq_along(interpolated$x), 
                         j = seq_along(interpolated$y)) %>% 
  mutate(x = interpolated$x[i],
         y = interpolated$y[j],
         DeltaTest = map2_dbl(i, j, ~interpolated$z[.x,.y])) %>% 
  select(-i, -j)

# Next, tested points outside the screen boundary, to test CO2 attenuation:
# compass points in 10cm increments, 20m timesteps, starting at 15:40
# N, E, S (replace), W, N+20, E+20, S+20, W+20(replace), 
# @ 18:24 N+30, E+30, S+30, W+30, S redo, W+20 redo
# 4/10, 14:25 @ 25s, N+40, E+40, S+40, W+40; 15:50 N+50, W+50, S+50, E+50

# adding outside-screen points to inside-screen points: (1) 15:40-18:00, (2) 18:24-2024 on 4/9; 
# (3) 14:25-15:45, (4) 15:50-17:10 on 4/10

# developing list of position IDs in the order they were measured
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

# broken into four chunks due to timestep errors or dropped points
CO2_out1 <- testing %>% # first 7
  filter(TIMESTAMP > "2024-04-09 15:40:00") %>% 
  filter(TIMESTAMP < "2024-04-09 18:00:00") %>% 
  select(TIMESTAMP, CO2ref, CO2elev, CO2test, DeltaObs, RH) %>% 
  mutate(DeltaTest = CO2test - CO2ref) %>% 
  mutate(timestep = floor_date(TIMESTAMP, unit = "20 minutes"), 
         position = pos_out[factor(timestep)])

CO2_out2 <- testing %>% # 8-13
  filter(TIMESTAMP > "2024-04-09 18:24:00") %>% 
  filter(TIMESTAMP < "2024-04-09 20:24:00") %>% 
  select(TIMESTAMP, CO2ref, CO2elev, CO2test, DeltaObs, RH) %>% 
  mutate(DeltaTest = CO2test - CO2ref) %>% 
  mutate(timestep = floor_date(TIMESTAMP-(4*60), unit = "20 minutes"), 
         position = pos_out[8:13][factor(timestep)]) 

CO2_out3 <- testing %>% # 14-17
  filter(TIMESTAMP > "2024-04-10 14:25:00") %>% 
  filter(TIMESTAMP < "2024-04-10 15:45:00") %>% 
  select(TIMESTAMP, CO2ref, CO2elev, CO2test, DeltaObs, RH) %>% 
  mutate(DeltaTest = CO2test - CO2ref) %>% 
  mutate(timestep = floor_date(TIMESTAMP-(5*60), unit = "20 minutes"), 
         position = pos_out[14:17][factor(timestep)]) 

CO2_out4 <- testing %>% # 18-21
  filter(TIMESTAMP > "2024-04-10 15:50:00") %>% 
  filter(TIMESTAMP < "2024-04-10 17:10:00") %>% 
  select(TIMESTAMP, CO2ref, CO2elev, CO2test, DeltaObs, RH) %>% 
  mutate(DeltaTest = CO2test - CO2ref) %>% 
  mutate(timestep = floor_date(TIMESTAMP+(10*60), unit = "20 minutes"), 
         position = pos_out[18:21][factor(timestep)]) 

CO2_out <- rbind(CO2_out1, CO2_out2, CO2_out3, CO2_out4)
# filter out S+10, and corresponding point, since PAR dropped below threshold during that step
CO2_out <- CO2_out %>% 
  filter(position != "So+10") %>% 
  mutate(RH = -1*RH)
points_out <- points_out[-3,]

# get x y data into CO2 df
# CO2_out <- left_join(CO2_out, points_out, by="position")
CO2_out <- left_join(CO2_out, points_out, by=c("position"="pos_out"))

# join outside screen data with inside screen data
# first fix naming conventions
names(within_4.9.24)[10] <- "x"
names(within_4.9.24)[11] <- "y"
names(CO2_out)[10] <- "x"
names(CO2_out)[11] <- "y"

# join inside-screen and outside-screen data:
in.out_4.9.24 <- rbind(within_4.9.24, CO2_out) # this is the measured CO2 values over several hours

# need same colnames for rbind
names(points_out) <- c("x", "y", "position")
names(points_within) <- c("x", "y", "position")
allpoints <- rbind(points_out, points_within) # this is the name and x,y coord of each of the 12 points measured

in.out_means <- in.out_4.9.24 %>% # df with per-point means
  group_by(position) %>% 
  summarise(mean = mean(abs(RH), na.rm=T), sd = sd(RH)) 
in.out_means <- left_join(in.out_means, allpoints, by = "position") # adds position name and coords

# interpolation function for data including outside-screen points
interpolated2 <- interp(x = in.out_means$x, y = in.out_means$y, z = in.out_means$mean, linear=TRUE, extrap = TRUE)
#convert this to a long form dataframe
interp_df2 <- expand_grid(i = seq_along(interpolated2$x), 
                          j = seq_along(interpolated2$y)) %>% 
  mutate(x = interpolated2$x[i],
         y = interpolated2$y[j],
         RH = map2_dbl(i, j, ~interpolated2$z[.x,.y])) %>% 
  select(-i, -j)

# Measured and interpolated RH values for a single plot, inside and outside the screen
interpolated_RH <- ggplot() + # interpolated points plus measured values  (means) plus circle showing screen
  geom_point(data = interp_df2, aes(x = x, y = y, color = RH), size=5, shape=15) +
  scale_color_continuous(type = "viridis") + theme_classic() +
  geom_text(data = in.out_means, aes(x=x, y=y, label = round(mean)), size = 5) +
  ggforce::geom_circle(aes(x0=0,y0=0,r=39)) +
  scale_y_continuous(expand = c(0,0)) +
  scale_x_continuous(expand = c(0,0)) +
  labs(color="RH")

plot(in.out_4.9.24$TIMESTAMP, in.out_4.9.24$RH)