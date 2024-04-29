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

# NEVER EVER GIVE UP
# NEVER SURRENDER