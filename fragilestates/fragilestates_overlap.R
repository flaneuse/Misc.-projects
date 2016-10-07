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

# Import data -------------------------------------------------------------
df = read_excel(paste0(base_dir, 'SBU_fragility_lists.xlsx'))

x = df %>% filter(coverage == 'Coverage') %>% slice(1:20)

y = data.frame(x = 1:20, y = 1:20, r = x$pop/1e6)

z = circleLayout(y, xlim = 100, ylim = 100)

dat = circlePlotData(z$layout)

ggplot(dat, aes(x = x, y = y, group = id,
                     fill = id)) +
  geom_polygon(alpha = 0.4) +
  scale_fill_gradientn(colours = RColorBrewer::brewer.pal(9, 'YlGnBu')) +
  scale_size_identity()


ggplot(res$layout, aes(x = x, y = y, size = r*2.25, fill = r)) +
  geom_point(shape = 21, alpha = 0.3) +
  coord_equal(xlim = limits, ylim = limits) +
  scale_fill_gradientn(colours = RColorBrewer::brewer.pal(9, 'YlGnBu')) +
  scale_size_identity()



ggplot(dat) + 
  geom_polygon(aes(x, y, group=id), colour="brown", fill="burlywood", alpha=0.3) +
  coord_equal(xlim=limits, ylim=limits) +
  theme_bw() 