# Fragile States Index visualization -----------------------------------------
#
# fragilestates_overlap.R: script to visualize three classifications of fragile states
#
# Geo data from Natural Earth: http://www.naturalearthdata.com/downloads/10m-cultural-vectors/
#
# Laura Hughes, lhughes@usaid.gov, 6 October 2016
# with Aaron Roesch (aroesch@usaid.gov) and Nada Petrovic (npetrovic@usaid.gov)
#
# Copyright 2016 by Laura Hughes via MIT License

# Maps to show, for most recent data:
# * USAID fragility score
# * Any crisis in the past 5 years
# * major conflict (battle deaths > 1000)
# * major violent extremism (terrorism > 500 deaths)
# * attempted coup
# * adverse regime change
# * mass atrocity

 #  anybroad_last5: any of the 
 # confvemin_last5: minor conflict or minor violent extremism
 #        al_bicat: alert list == 1
 #       any_last5: any severe violence or instability
 #  climrisk_last5: climate risk
 # econshock_last5: major economic shock
 #     crime_last5: high violent crime
 #   confmaj_last5: major confligt
 #     vemaj_last5: major violent extremism 
 #   rgmchng_last5: adverse regime change 
 #      coup_last5: coup d'etat
 #  disaster_last5: natural disaster
 # massatroc_last5: mass atrocities


# setup params ------------------------------------------------------------
# Run fragilestates_overlap.R first.

bg_fill = '#f6f8fb'

lac_xlim = c(-120, -35)
lac_ylim = c(-35, 30)

afr_xlim = c(-15, 60)
afr_ylim = c(-36, 39)

ee_xlim = c(15, 52)
ee_ylim = c(35, 57)

asia_xlim = c(60, 145)
asia_ylim = c(-13, 55)

# libraries to load -------------------------------------------------------
library(devtools)
install_github('flaneuse/frontier')

pkgs = c('llamar', 'dplyr', 'tidyr', 
         'frontier', 'maptools',
         'RColorBrewer', 'extrafont', 'readxl')

# Install and load packages -----------------------------------------------

# Check if packages are installed
alreadyInstalled = installed.packages()[, "Package"]

toInstall = pkgs[!pkgs %in% alreadyInstalled]

# Install anything that isn't already installed.
if (length(toInstall > 0)) {
  print(paste0("Installing these packages: ", paste0(toInstall, collapse = ", ")))
  
  install.packages(toInstall)
}


# Load packages
for (i in seq_along(pkgs)) {
  library(pkgs[i], character.only = TRUE, quietly = TRUE)
}

loadfonts(quiet = TRUE)



# import data -------------------------------------------------------------

# fragile scores and threats
frag_breakdown = read_excel('~/Documents/USAID/mini projects/Fragile States - (Aaron Roesch)/fragility_maps_data.xlsx')
#SBU_fragility_threats.xlsx')

# merge with frag_overlap to get the regional codes
frag_breakdown = left_join(frag_breakdown, 
                           frag_overlap %>% select(-r)
)

# frag_breakdown2015 = frag_breakdown %>% 
# filter(year == 2015) 


# import geo data ---------------------------------------------------------

# import spatial data

geo = frontier::shp2df(baseDir = '~/Documents/USAID/geodata/ne_10m_admin_0_countries_10pctsimpl/',
                                   layerName = 'ne_10m_admin_0_countries', getCentroids = FALSE,
                       reproject = TRUE,
                       projection = '+proj=wintri +lon_0=0 +x_0=0 +y_0=0 +ellps=WGS84 +datum=WGS84 +no_defs')

# land mass basemap
land = frontier::shp2df(baseDir = '~/Documents/USAID/geodata/ne_10m_land_2pctsimpl/',
                                    layerName = 'ne_10m_land', getCentroids = FALSE,
                        reproject = TRUE,
                        projection = '+proj=wintri +lon_0=0 +x_0=0 +y_0=0 +ellps=WGS84 +datum=WGS84 +no_defs') 

# find which code contains the correct iso code. options:
# ADM0_A3_US
# ADM0_A3_IS
# ADM0_A3_UN
# ADM0_A3_WB
# WB_A3
# UN_A3
# ISO_A3
# BRK_A3
# GU_A3
# SU_A3
# SOV_A3
# ADM0_A3

# Trying first WB, assuming World Bank
all(geo$ADM0_A3_WB == geo$WB_A3)
all(geo$ADM0_A3_UN == geo$UN_A3)

codes = data.frame(code = unique(geo$WB_A3),
                   in_ne = TRUE)

# test merge
x = full_join(frag_breakdown, codes, by = c("code"))

View(x %>% filter(is.na(in_ne)))

# merges everything, aside from Norway-- which is -99 in WB_A3

# Fix Norway
geo = geo %>% 
  mutate(code = ifelse(NAME == 'Norway', 'NOR', as.character(WB_A3)))

codes = data.frame(code = unique(geo$code),
                   in_ne = TRUE)
# test merge
x = full_join(frag_breakdown, codes, by = c("code"))

View(x %>% filter(is.na(in_ne)))


# Filter out extraneous geo data ------------------------------------------

geo = geo %>% 
  filter(NAME != 'Antarctica') %>% 
  select(code, lat, long, group, order, id, REGION_WB, CONTINENT)

# merge fragile data w/ geo data ------------------------------------------
frag_breakdown_geo = full_join(frag_breakdown, geo, by = c("code"))


# choropleth function --------------------------------------------------------------
plot_choro = function(fill_var, 
                      fill_colour = brewer.pal(11, 'Spectral')[2]) {
  ggplot(frag_breakdown_geo, 
         aes_string(x = 'long', y = 'lat',
                    group = 'group', order = 'order')) +
    geom_path(data = land, fill = grey15K) +
    geom_path(data = land, colour = '#89a3d1', size = 1.5, fill = NA) +
    geom_polygon(aes(alpha = factor(usaidcov)),
                 fill = grey30K) +
    geom_polygon(aes(fill = fill_color, alpha = factor(usaidcov)),
                 data = frag_breakdown_geo %>% filter_(paste0(fill_var,' == 1'))) +
    # geom_polygon(aes_string(fill = paste0('factor(', fill_var, ')'),
                                          # alpha = 'factor(usaidcov)')) +
    geom_path(colour = 'white', size = 0.06, fill = NA) +
    coord_equal() +
    
    scale_fill_identity() +
    # scale_fill_manual(values = c('0' = grey25K, '1' = fill_colour)) + 
    scale_alpha_manual(values = c('0' = 0.25, '1' = 0.75)) +
    theme_void() +
    theme(legend.position = 'none',
          rect = element_rect(fill = '#ffffff', colour = '#ffffff', size = 0, linetype = 1),
          panel.background = element_rect(fill = bg_fill))
}



# bar plot ----------------------------------------------------------------
frag_breakdown_sum = frag_breakdown %>% 
  select(-no_lists, -contains('frag'), -budget, -pop, -contains('any'), -al_bicat) %>% 
  gather(event, occurred, -country, -code, -usaidcov, -region, -fill_color, -coverage) %>% 
  group_by(region, event, usaidcov, fill_color) %>% 
  summarise(total = mean(occurred), n = n()) %>% 
  arrange(desc(total))

frag_breakdown_untidy = frag_breakdown_sum %>% 
  select(-n) %>% 
  spread(usaidcov, total)

# Relevel
# Event totals:
# 1  confvemin_last5    53
# 2   climrisk_last5    21
# 3  econshock_last5    21
# 4      crime_last5    19
# 5    confmaj_last5    16
# 6      vemaj_last5    15
# 7    rgmchng_last5    13
# 8       coup_last5    12
# 9   disaster_last5     9
# 10 massatroc_last5     3

event_order = frag_breakdown %>% 
  select(-fill_color, -no_lists, -contains('frag'), -budget, -pop, -contains('any'), -al_bicat) %>% 
  gather(event, occurred, -country, -code, -usaidcov, -region, -coverage) %>% 
  group_by(event) %>% 
  summarise(total = sum(occurred)) %>% 
  arrange(total)

region_order = frag_breakdown %>% 
  select(-fill_color, -no_lists, -contains('frag'), -budget, -pop, -contains('any'), -al_bicat) %>% 
  gather(event, occurred, -country, -code, -usaidcov, -region, -coverage) %>% 
  group_by(region) %>% 
  summarise(total = sum(occurred)) %>% 
  arrange(desc(total))

frag_breakdown_sum$region = factor(frag_breakdown_sum$region,
                                    levels = region_order$region)
frag_breakdown_untidy$region = factor(frag_breakdown_untidy$region,
                                   levels = region_order$region)

frag_breakdown_sum$event = factor(frag_breakdown_sum$event,
                                    levels = event_order$event)
frag_breakdown_untidy$event = factor(frag_breakdown_untidy$event,
                                     levels = event_order$event)


ggplot(frag_breakdown_sum %>% filter(!is.na(region)), aes(y =  event, 
                                                          x = total, 
                                                          fill = fill_color,
                                                          colour = fill_color,
                                                          size = n,
                                                          alpha = factor(usaidcov))) +
  geom_segment(aes(x = `0`, xend = `1`,
                   y = event, yend = event),
               size = 0.5, colour = grey60K,
               alpha = 1,
               data = frag_breakdown_untidy %>% filter(!is.na(region))) +
  geom_point(stroke = 0.05, colour = grey90K, shape = 21) + 
  geom_point(stroke = 0.25, alpha = 1,
             data = frag_breakdown_sum %>% filter(usaidcov == 0, !is.na(region)),
             shape = 21, fill = 'white') + 
  facet_wrap(~region, ncol = 2) +
  scale_alpha_manual(values = c('0' = 0., '1' = 1)) +
  scale_size(range = c(1, 8)) +
  scale_fill_identity() +
  scale_colour_identity() +
  scale_x_continuous(labels = scales::percent, breaks = c(0, 0.25, 0.5, 0.75)) +
  theme_xgrid()

# choros of binary vars ---------------------------------------------------


plot_choro('al_bicat') + coord_equal(xlim = afr_xlim, ylim = afr_ylim)
plot_choro('any_last5')  + coord_equal(xlim = afr_xlim, ylim = afr_ylim)
plot_choro('disaster_last5') + coord_equal(xlim = afr_xlim, ylim = afr_ylim)
# scale_fill_gradientn(colours = rev(brewer.pal(10, 'RdYlBu')), 
# limits = c(-max_val, max_val),
# na.value = grey15K) +
# max_val = max(abs(range(frag_breakdown$al, na.rm = TRUE)))




# basic map of regions ----------------------------------------------------


p = ggplot(frag_breakdown_geo, 
             aes_string(x = 'long', y = 'lat',
                        group = 'group', order = 'order')) +
  geom_path(data = land, colour = '#89a3d1', size = 1.5) +
  geom_path(data = land, fill = grey15K, colour = NA, alpha = 1, size = 0) +
  geom_polygon(aes_string(fill = 'fill_color', alpha = 'coverage'), size = 0, colour = NA) +
  # geom_polygon(aes_string(fill = 'fill_color')) +
  geom_path(colour = 'white', size = 0.06, fill = NA) +
  coord_equal() +
  
  theme_void() +
  theme(legend.position = 'none',
        rect = element_rect(fill = '#ffffff', colour = '#ffffff', size = 0, linetype = 1),
        panel.background = element_rect(fill = bg_fill)) +
  scale_fill_identity() +
  scale_alpha_manual(values = c('0' = 0.5, 'Coverage' = 1)) 


save_plot('~/Documents/USAID/mini projects/Fragile States - (Aaron Roesch)/map_region.pdf',
          width = 7, height = 4)
