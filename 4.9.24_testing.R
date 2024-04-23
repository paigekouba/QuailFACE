# 4/21/24
# Performance Testing for Ch 2
# Using 4/9-10 data

# load the data
testing <- read_csv("/Users/paigekouba/Documents/UC_Davis/2021_Winter/Quals/Proposal/Chapter 1/TinyFACE/CO2_control/CR3000_Data20s_4.19.24.csv", skip = 4, col_names = c("TIMESTAMP",	"RECORD",	"BattV_Avg",	"CO2signal",	"CO2ref",	"CO2elev",	"CO2test",	"DeltaObs",	"DeltaSet",	"LicorSig2",	"FlowMFC",	"AirTC",	"RH",	"WS_ms",	"PARuE",	"TurnCO2On",	"SpanObs",	"SpanAct",	"ZeroObs",	"ZeroAct"))

attr(testing$TIMESTAMP, "tzone") # starts out as UTC, though clock time is correct
# force_tz keeps the clock time but reassigns the tz to be accurate
testing$TIMESTAMP <- force_tz(testing$TIMESTAMP, "America/Los_Angeles")

testing <- testing %>% 
  filter(TIMESTAMP > "2024-04-09 08:10:00") %>% 
  filter(TIMESTAMP < "2024-04-10 17:10:00")

testing %>%
  select(TIMESTAMP, CO2ref, CO2elev, CO2test, DeltaObs) %>% 
  mutate(DeltaTest = CO2test - CO2ref) %>% 
  dygraph() %>% 
  dyRangeSelector()

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

above_4.9.24 %>%  # density curves for each timestep
  ggplot(aes(x=DeltaTest, group = position)) + geom_density(aes(color = position))

above_4.9.24 %>% # box plots
  ggplot(aes(x=position, y=DeltaTest)) + geom_boxplot()

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
  mutate(timestep = floor_date(TIMESTAMP, unit = "20 minutes"), 
         position = pos_within[factor(timestep)])
#summarize(mDeltaTest = mean(DeltaTest), sd = sd(DeltaTest))
#group_by(TIMESTAMP = cut(TIMESTAMP, breaks = "20 min")) %>% 
mean(within_4.9.24$DeltaTest) # 198.9011

within_4.9.24 %>%  # density curves for each timestep
  ggplot(aes(x=DeltaTest, group = position)) + geom_density(aes(color = position))

within_4.9.24 %>% # box plots
  ggplot(aes(x=position, y=DeltaTest)) + geom_boxplot()

# assign spots to points in a grid, then try "interp" package to make isolines!!
# get x and y associated with CO2 table
within_4.9.24 <- left_join(within_4.9.24, points_within, by = "position")
library(interp)
interpolated <- interp(within_4.9.24$x_within, within_4.9.24$y_within, within_4.9.24$DeltaTest, duplicate = "mean", output = "grid")
#convert this to a long form dataframe
interp_df <- expand_grid(i = seq_along(interpolated$x), 
                         j = seq_along(interpolated$y)) %>% 
  mutate(x = interpolated$x[i],
         y = interpolated$y[j],
         DeltaTest = map2_dbl(i, j, ~interpolated$z[.x,.y])) %>% 
  select(-i, -j)

ggplot(data = interp_df, aes(x = x, y = y)) +
  geom_contour(aes(z = DeltaTest, colour = stat(level)), size = 1.2) +
  metR::geom_text_contour(aes(z=DeltaTest), stroke = 0.15) +
  coord_equal()


# compass points in 10cm increments, 20m timesteps, starting at 15:40
# N, E, S (replace), W, N+20, E+20, S+20, W+20(replace), 
# @ 18:24 N+30, E+30, S+30, W+30, S redo, W+20 redo
# 4/10, 14:25 @ 25s, N+40, E+40, S+40, W+40; 15:50 N+50, W+50, S+50, E+50

# between-plot testing
# at 7:25, 20m timesteps, plot 1-16

# 12:45 short veg, canopy height
# 13:05 medium veg, canopy height
# 13:25 tall veg, canopy height
# 13:45 medium, 10cm
# 14:05 tall, 10cm

