# Zambia Feed the Future Midline data -----------------------------------------
#
# ZMB_geo.R: script to map districts in Zambia
#
# Data are from GADM
#
# Laura Hughes, lhughes@usaid.gov, 6 October 2016
#
# Copyright 2016 by Laura Hughes via MIT License


# setup -------------------------------------------------------------------

library(ggplot2)
library(maptools)
library(llamar)
library(RColorBrewer)
library(dplyr)

ftf_prov = 'Eastern'
ftf_dist = c('Chipata', 'Katete', 'Lundazi', 'Nyimba', 'Petauke')

adm0_fill = grey10K
adm0_stroke = grey40K
adm1_fill = grey40K
adm1_stroke = grey75K
adm2_fill = brewer.pal(11, 'RdYlBu')[2]
adm2_stroke = brewer.pal(11, 'RdYlBu')[1]
stroke_size = 0.1

# import data -------------------------------------------------------------

adm0 = readRDS('~/Documents/USAID/mini projects/Zambia FtF changes - (BFS)/data/ZMB_adm0.rds')
adm1 = readRDS('~/Documents/USAID/mini projects/Zambia FtF changes - (BFS)/data/ZMB_adm1.rds')
adm2 = readRDS('~/Documents/USAID/mini projects/Zambia FtF changes - (BFS)/data/ZMB_adm2.rds')



# shp2df ------------------------------------------------------------------


gadm2df = function(sp_df){
  sp_df@data$id = rownames(sp_df@data)
  
  # Convert the shape polygons into a series of lat/lon coordinates.
  poly_points = ggplot2::fortify(sp_df, region = "id")
  
  # Merge the polygon lat/lon points with the original data
  df = dplyr::left_join(poly_points, sp_df@data, by = "id")
}

adm0_df = gadm2df(adm0)
adm1_df = gadm2df(adm1)
adm2_df = gadm2df(adm2)


# Quick Ftf map -----------------------------------------------------------
adm1_df = adm1_df %>% filter(NAME_1 == ftf_prov)
adm2_df = adm2_df %>% filter(NAME_2 %in% ftf_dist)

ggplot(adm0_df, aes(x = long, y = lat,
                    group = group,
                    order = order)) +
  geom_polygon(fill = adm0_fill) +
  geom_path(colour = adm0_stroke,
            size = stroke_size) +
  
  geom_polygon(fill = adm1_fill,
               data = adm1_df) +
  geom_path(colour = adm1_stroke,
            size = stroke_size,
            data = adm1_df) +
  
  geom_polygon(fill = adm2_fill,
               data = adm2_df) +
  geom_path(colour = adm2_stroke,
            size = stroke_size,
            data = adm2_df) +
  coord_equal() +
  theme_void() +
  theme(legend.position = 'none')

save_plot('~/Creative Cloud Files/MAV/Projects/ZMB_FTFmidline_2016-10/ZMB_FtFzones.pdf', width = 3, height = 2)

