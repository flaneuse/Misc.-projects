# Fragile States Index visualization -----------------------------------------
#
# fragilestates_overlap.R: script to visualize three classifications of fragile states
#
# Data are from 
#
# Laura Hughes, lhughes@usaid.gov, 6 October 2016
# with Aaron Roesch (aroesch@usaid.gov) and Nada Petrovic (npetrovic@usaid.gov)
#
# Copyright 2016 by Laura Hughes via MIT License



# Setup vars and packages -------------------------------------------------
base_dir = '~/Documents/USAID/mini projects/Fragile States - (Aaron Roesch)/'

library(readxl)
library(ggplot2)
library(packcircles)
library(dplyr)

limits = c(-100, 100)

color_afr = '#fff2ae'
color_asia = '#b3e2cd'
color_lac = '#fdcdac'
color_me = '#f1e2cc'
color_ee = '#cbd5e8'
color_else = '#cccccc'

# Import data -------------------------------------------------------------
df = read_excel(paste0(base_dir, 'SBU_fragility_lists.xlsx')) 

df = df %>% 
  mutate(fill_color = case_when(df$region == 'AFR' ~ color_afr,
                                df$region == 'ASIA' ~ color_asia,
                                df$region == 'LAC' ~ color_lac,
                                df$region == 'ME' ~ color_me,
                                df$region == 'E&E' ~ color_ee,
                                TRUE ~ color_else))

x = df %>% filter(no_lists == 0) %>% 
  arrange(desc(region), desc(pop)) %>% 
  mutate(r = pop/1e6,
         area = pi * r^2)


total_area = sum(x$area) * 1.25
array_length = ceiling(sqrt(nrow(x)))

limits = c(-sqrt(total_area)/2, sqrt(total_area)/2)

coord_loc = seq(by = sqrt(total_area)/array_length, from = limits[1]/1.25, length.out = array_length)

y = x %>%
  mutate(x = rep(coord_loc, array_length)[1:157],
         y = rep(coord_loc, each = array_length)[1:157]) %>% 
  select(x, y, r)

z = circleLayout(y , xlim = limits, ylim = limits,
                 weights = 1, wrap = TRUE)




# Convert centers, radii into cartesian coordinates -----------------------
# Though ggplot has a mechanism for mapping a variable to the size of geom_point,
# the size is in units of mm, NOT the cartesian axis.  As a result, you have to convert
# the center and radii into coordinates that can be used with geom_polygon.
circle_coords = circlePlotData(z$layout, npoints = 50)
# circle_coords = circlePlotData(y, npoints = 50)

circle_coords = bind_cols(z$layout, x)

# ggplot(circle_coords, aes(x = x, y = y, group = id,
  #                    fill = id)) +
  # geom_polygon() +
  # # coord_equal(xlim = limits, ylim = limits) +
  # # scale_fill_gradientn(colours = RColorBrewer::brewer.pal(9, 'YlGnBu')) +
  # scale_size_identity()


ggplot(y, aes(x = x, y = y, 
                          size = r,
                          fill = fill_color,
                          alpha = coverage)) +
  # geom_polygon(alpha = 0.4) +
  geom_point(shape = 21) +
  scale_fill_identity() +
  coord_equal(xlim = limits, ylim = limits) +
  scale_alpha_discrete(range = c(0.2, 0.5)) +
  scale_size_identity()

x = df %>% filter(no_lists == 0) %>% 
  arrange(desc(region))

bubbles(value = x$pop/1e7, label = x$country, color = x$fill_color)

