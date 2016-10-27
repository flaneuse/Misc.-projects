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
library(bubbles)
library(llamar)

limits = c(-350, 300)

color_afr = '#fff2ae'
color_asia = '#b3e2cd'
color_lac = '#fdcdac'
color_me = '#f1e2cc'
color_ee = '#cbd5e8'
color_else = '#cccccc'

# Import data -------------------------------------------------------------
df = read_excel(paste0(base_dir, 'SBU_fragility_lists.xlsx')) 


# d3-based visualiztion ---------------------------------------------------
# Uses package 'bubbles' and d3-based packing optimization.
# optimization is great, but fewer params to control, esp. size scaling (on same axes?)
# also more annoying to export to vector-graphic.
x = df %>% filter(no_lists == 0) %>% 
  arrange(desc(region))

bubbles(value = x$pop/1e7, label = paste0(x$country), color = x$fill_color)

rm(x)

# calculate starting coordinates ------------------------------------------

df = df %>% 
  mutate(fill_color = case_when(df$region == 'AFR' ~ color_afr,
                                df$region == 'ASIA' ~ color_asia,
                                df$region == 'LAC' ~ color_lac,
                                df$region == 'ME' ~ color_me,
                                df$region == 'E&E' ~ color_ee,
                                TRUE ~ color_else)) %>% 
  group_by(no_lists) %>% 
  arrange(desc(region), desc(pop)) %>% 
  mutate(r = pop/1e7,
         area = pi * r^2,
         r2 = lead(pop/1e6)) %>% 
  mutate(x = cumsum(r + r2))



calc_packing = function(df,
                        filename, 
                        height_plot = 10,
                        width_plot = 10){
  # Find the average radius for each group
  avg_r = mean(df$r)
  
  init_coords = 
    df %>% 
    arrange(desc(region), coverage) %>% 
    mutate(grp_num = dense_rank(region),
           id = row_number(region)) %>% 
    group_by(region) %>% 
    mutate(
      grp_rank = row_number(region),
      num_across = ceiling(sqrt(max(grp_rank))),
      x = (grp_rank + num_across - 1) %% num_across,
      y = ceiling(grp_rank / num_across))
  # x = rep(seq(from = 0, by = avg_r, length.out = 5), times = 5),
  # y = rep(seq(from = 0, by = avg_r, length.out = 5), each = 5)) %>% 
  # bind_cols(df, data.frame(y = 1:nrow(df))) %>%
  
  # how much to offset each group, to clump them together
  grp_offset = init_coords %>% 
    ungroup() %>% 
    select(region, num_across, grp_num) %>% 
    distinct() %>% 
    arrange(grp_num) %>% 
    mutate(prev_circles = cumsum(lag(num_across, default = 0)))
  
  init_coords = full_join(init_coords, grp_offset)
  
  init_coords = init_coords %>% 
    mutate(x = (x + prev_circles)  * avg_r,
           y = y * avg_r) %>%
    ungroup() %>% 
    select(x, y, r)
  
  opt_layout = circleLayout(init_coords , xlim = limits, ylim = limits,
                            weights = 1, wrap = TRUE)
  
  circle_centroids = bind_cols(opt_layout$layout, data.frame(id = 1:nrow(df)))
  
  
  # Convert centers, radii into cartesian coordinates -----------------------
  # Though ggplot has a mechanism for mapping a variable to the size of geom_point,
  # the size is in units of mm, NOT the cartesian axis.  As a result, you have to convert
  # the center and radii into coordinates that can be used with geom_polygon.
  circle_coords = circlePlotData(circle_centroids, npoints = 100)
  
  full_data = bind_cols(df  %>% arrange(desc(region), coverage), data.frame(id = 1:nrow(df))) %>% 
    select(id, country, region, coverage, pop, budget, fill_color, r, no_lists)
  
  # Bind original data
  circle_coords = full_join(circle_coords, full_data, by = c("id"))
  
  circle_centroids = full_join(circle_centroids, full_data, by = c("id", "r"))
  
  p = ggplot(circle_coords, aes(x = x, y = y, group = id,
                                colour = fill_color,
                                fill = fill_color)) +
    geom_polygon(aes(alpha = factor(coverage)),
                 size = 0) +
    geom_path(size = 0.25) +
    geom_text(aes(label = country),
              family = 'Lato Light',
              size = 3,
              colour = grey75K,
              data = circle_centroids %>% 
                filter(coverage == 'Coverage' |
                         pop > 100e6)) + 
    coord_equal(xlim = limits, ylim = limits) +
    scale_colour_identity() +
    scale_fill_identity() +
    scale_alpha_manual(values = c('0' = 0.2, 'Coverage' = 0.75)) +
    # facet_wrap(~no_lists, ncol = 1) +
    theme_blank()
  
  save_plot(filename, width = width_plot, height = height_plot)
  
  return(p)
}

# plot --------------------------------------------------------------------
df_plot = df %>% filter(no_lists == 1)
calc_packing(df_plot, '~/Documents/USAID/mini projects/Fragile States - (Aaron Roesch)/fragile_circles1.pdf')

df_plot = df %>% filter(no_lists == 2)
calc_packing(df_plot, '~/Documents/USAID/mini projects/Fragile States - (Aaron Roesch)/fragile_circles2.pdf')

df_plot = df %>% filter(no_lists == 3)
calc_packing(df_plot, '~/Documents/USAID/mini projects/Fragile States - (Aaron Roesch)/fragile_circles3.pdf')

regions = unique(df$region)

for(i in seq_along(regions)){
  
  df_plot = df %>% filter(no_lists == 0, region == regions[i])
  
  p=calc_packing(df_plot, paste0('~/Documents/USAID/mini projects/Fragile States - (Aaron Roesch)/fragile_circles0_',
                                 regions[i], '.pdf'))
}


