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
#   confmaj_last5: major conflict
#     vemaj_last5: major violent extremism 
#   rgmchng_last5: adverse regime change 
#      coup_last5: coup d'etat
#  disaster_last5: natural disaster
# massatroc_last5: mass atrocities


# setup params ------------------------------------------------------------
# Run fragilestates_overlap.R first.

bg_fill = '#f6f8fb'

limits = data.frame(region = sort(unique(frag_overlap$region)),
                    xmin = c(
                      -0.2e7,  
                      0.35e7,   
                      -0.1e7,  
                      -1e7,    
                      -0.2e7,  
                      -1e7),
                    xmax = c(0.5e7, # Africa
                             1.35e7, # Asia,
                             0.45e7, # E&E 
                             -0.3e7, # LAC
                             0.6e7, # ME
                             1e7),
                    
                    ymin = c(
                      -3.8e6 ,
                      -2e6 ,
                      3.5e6  ,
                      -3.8e6 ,
                      1.2e6  ,
                      -1e7
                    ),
                    ymax = c(4e6, # Africa
                             6e6, # Asia
                             6.5e6, # E&E
                             3.9e6, # LAC
                             4.8e6, # ME
                             1e7 # NA
                    ))
# lat/long
# lac_xlim = c(-120, -35)
# lac_ylim = c(-35, 30)
# 
# afr_xlim = c(-15, 60)
# afr_ylim = c(-36, 39)
# 
# ee_xlim = c(15, 52)
# ee_ylim = c(35, 57)
# 
# asia_xlim = c(60, 145)
# asia_ylim = c(-13, 55)

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
# frag_breakdown = read_excel('~/Documents/USAID/mini projects/Fragile States - (Aaron Roesch)/fragility_maps_data.xlsx')
#SBU_fragility_threats.xlsx')
frag_breakdown = read_excel('~/Documents/USAID/mini projects/Fragile States - (Aaron Roesch)/fragility_maps_data_10y.xlsx')

# merge with frag_overlap to get the regional codes
frag_breakdown = left_join(frag_breakdown, 
                           frag_overlap %>% select(-r)
)

# frag_breakdown2015 = frag_breakdown %>% 
# filter(year == 2015) 


# import geo data ---------------------------------------------------------

# import spatial data

geo = frontier::shp2df(baseDir = '~/Documents/USAID/geodata/ne_10m_admin_0_countries_10pctsimpl/',
                       layerName = 'ne_10m_admin_0_countries', 
                       getCentroids = TRUE,
                       labelVar = 'WB_A3',
                       reproject = TRUE,
                       projection = '+proj=wintri +lon_0=0 +x_0=0 +y_0=0 +ellps=WGS84 +datum=WGS84 +no_defs')

geo_centroids = geo$centroids
geo = geo$df

# land mass basemap
land_outline = frontier::shp2df(baseDir = '~/Documents/USAID/geodata/ne_10m_land_2pctsimpl/',
                                layerName = 'ne_10m_land', getCentroids = FALSE,
                                reproject = TRUE,
                                projection = '+proj=wintri +lon_0=0 +x_0=0 +y_0=0 +ellps=WGS84 +datum=WGS84 +no_defs') 

land = frontier::shp2df(baseDir = '~/Documents/USAID/geodata/ne_10m_land_10pctsimpl/',
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

# View(x %>% filter(is.na(in_ne)))

# merges everything, aside from Norway-- which is -99 in WB_A3

# Fix Norway
geo = geo %>% 
  mutate(code = ifelse(NAME == 'Norway', 'NOR', as.character(WB_A3)))

codes = data.frame(code = unique(geo$code),
                   in_ne = TRUE)
# test merge
x = full_join(frag_breakdown, codes, by = c("code"))

# View(x %>% filter(is.na(in_ne)))


# Filter out extraneous geo data ------------------------------------------

geo = geo %>% 
  filter(NAME != 'Antarctica') %>% 
  select(code, lat, long, group, order, id, REGION_WB, CONTINENT)

# merge fragile data w/ geo data ------------------------------------------
frag_breakdown_geo = full_join(frag_breakdown, geo, by = c("code"))

# -- centers for labelling -- 
geo_centroids = full_join(frag_breakdown, geo_centroids, by = c("code" = "label"))

geo_centroids = geo_centroids %>% 
  filter(usaidcov == 1)


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
# Event totals: (last 5 y)
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
               size = 0.35, colour = grey30K,
               alpha = 1,
               data = frag_breakdown_untidy %>% filter(!is.na(region))) +
  geom_point(stroke = 0.05, colour = grey90K, shape = 21) + 
  geom_point(stroke = 0.25, alpha = 1,
             data = frag_breakdown_sum %>% filter(usaidcov == 0, !is.na(region)),
             shape = 21, fill = 'white') + 
  facet_wrap(~region, ncol = 1) +
  scale_alpha_manual(values = c('0' = 0., '1' = 1)) +
  scale_size(range = c(1, 8)) +
  scale_fill_identity() +
  scale_colour_identity() +
  scale_x_continuous(labels = scales::percent, breaks = c(0, 0.25, 0.5, 0.75, 1)) +
  theme_xgrid()

save_plot('~/Documents/USAID/mini projects/Fragile States - (Aaron Roesch)/event_breakdown.pdf',
          width = 4, height = 16)


# bar plot -- only USAID ----------------------------------------------------------------
frag_breakdown_sum$event = factor(frag_breakdown_sum$event,
                                  levels = c("massatroc_last10", 
                                             "rgmchng_last10",
                                             "vemaj_last10", "confmaj_last10", "coup_last10",
                                             "climrisk_last10", "disaster_last10",
                                             "crime_last10", "econshock_last5", "confvemin_last10"),
                                  labels = c('mass atrocity', 'adverse regime change',
                                             'major violent extremism', 'major conflict',
                                             "attempted coup d'état", 'high climate change risk',
                                             'major natural disaster', 'high violent crime',
                                             'major economic shock', 'minor conflict/violent extremism')
)

frag_breakdown_sum = frag_breakdown_sum %>% 
  filter(!is.na(event))

for(i in seq_along(regions)){
  df = frag_breakdown_sum %>% filter(!is.na(region), 
                                     region == regions[i], usaidcov == 1)
  
  ggplot(df, 
         aes(y =  fct_reorder(event, total), 
             x = total, 
             fill = fill_color,
             colour = fill_color,
             size = n)) +
    geom_segment(aes(x = 0, xend = total,
                     y = fct_reorder(event, total),
                     yend = fct_reorder(event, total)),
                 size = 0.35, colour = grey30K) +
    geom_point(stroke = 0.05, colour = grey90K, shape = 21) + 
    geom_text(aes(label = percent(total, 0)),
              hjust = 0,
              family = 'Lato Light',
              nudge_x = 0.025,
              colour = grey60K,
              size = 4) +
    scale_size(range = c(1, 5), limits = c(1, 47)) +
    scale_fill_identity() +
    scale_colour_identity() +
    scale_x_continuous(labels = scales::percent, 
                       limits = c(0, 1),
                       breaks = c(0, 0.25, 0.5, 0.75, 1)) +
    theme_xgrid() +
    theme(axis.title.x = element_blank(),
          axis.text.y = element_text(size = 13))
  
  save_plot(paste0('~/Documents/USAID/mini projects/Fragile States - (Aaron Roesch)/event_breakdown-',
                   regions[i], '.pdf'),
            # width = 5.5, height = 2.15)
            width = 5.5, height = 4)
}

# choropleth function --------------------------------------------------------------
plot_choro = function(df = frag_breakdown_geo,
                      basemap = land,
                      ocean = land_outline,
                      cntry_labels = geo_centroids,
                      fill_var,
                      limits,
                      bg_fill = "#f6f8fb",
                      region_name) {
  
  # Filter out the relevant data
  current_df = df %>% 
    filter(region == region_name)
  
  current_limits = limits %>% 
    filter(region == region_name)
  
  current_centroids = cntry_labels %>% 
    filter(region == region_name)
  
  # Find the correct color for the label
  text_colour = case_when(region_name == 'AFR' ~ '#ad6c68',
                          region_name == 'ASIA' ~ '#688197',
                          region_name == 'E&E' ~ '#7d9973',
                          region_name == 'LAC' ~ '#b0ad7e',
                          region_name == 'ME' ~ '#b19062',
                          TRUE ~ grey60K)
  
  ggplot(current_df, 
         aes_string(x = 'long', y = 'lat',
                    group = 'group', order = 'order')) +
    
    # -- gaussian blur fade --
    geom_path(data = ocean, colour = '#89a3d1', size = 0.5) +
    
    # -- fill under entire area -- 
    geom_polygon(data = df, fill = grey5K) +
    # -- continent outline --
    geom_path(data = basemap, colour = grey75K, size = 0.05) +
    
    # -- fill just region for a shadow -- 
    geom_polygon(data = current_df, fill = 'white') +
    
    # -- choropleth areas NOT experiencing event grey -- 
    geom_polygon(aes(alpha = factor(usaidcov)),
                 fill = grey25K, 
                 data = current_df %>% filter_(paste0(fill_var,' == 0'))) +
    
    # -- choropleth areas experiencing event in Pastel1 --
    geom_polygon(aes(fill = fill_color, alpha = factor(usaidcov)),
                 data = current_df %>% filter_(paste0(fill_var,' == 1'))) +
    
    # -- label USAID countries --
    geom_text(aes_string(group = '1', order = '1', 
                         label = 'stringr::str_to_upper(country)',
                         colour = paste0('factor(',
                                         fill_var, ')')),
              size = 2, 
              family = 'Lato Light',
              data = current_centroids) +
    
    # -- stroke regional country outlines --
    geom_path(colour = grey75K, size = 0.06) +
    
    # -- crop area of interest --
    coord_equal(xlim = c(current_limits$xmin, current_limits$xmax), 
                ylim = c(current_limits$ymin, current_limits$ymax)) +
    
    # -- scales --
    scale_fill_identity() +
    scale_colour_manual(values = c('0' = grey60K, '1' = text_colour)) + 
    scale_alpha_manual(values = c('0' = 0.25, '1' = 1)) +
    
    # -- themes --
    theme_void() +
    theme(legend.position = 'none',
          rect = element_rect(fill = '#ffffff', colour = '#ffffff', size = 0, linetype = 1),
          panel.background = element_rect(fill = bg_fill))
}


# loop over choros --------------------------------------------------------
regions = unique(frag_overlap$region)
regions = setdiff(regions, 'NA') # ignore North America

vars = c('any', 'anybroad_last10', 'any_last10', 
         'econshock_last5', 'coup_last10', 'confvemin_last10',
         'crime_last10', 'disaster_last10')
# vars = c('al_bicat', 'any_last5', 'confvemin_last5', 'econshock_last5', 'crime_last5', 'coup_last5')

for (i in seq_along(regions)) {
  for(j in seq_along(vars)) {
    plot_choro(fill_var = vars[j], region = regions[i], limits = limits) 
    
    save_plot(filename = paste0('~/Documents/USAID/mini projects/Fragile States - (Aaron Roesch)/',
                                regions[i], '-', vars[j], '.pdf'),
              width = 4)
  }
}

# basic map of regions ----------------------------------------------------


p = ggplot(frag_breakdown_geo, 
           aes_string(x = 'long', y = 'lat',
                      group = 'group', order = 'order')) +
  geom_path(data = land, colour = '#89a3d1', size = 1.5) +
  geom_polygon(data = land, fill = 'white', colour = NA, alpha = 1, size = 0) +
  geom_polygon(aes_string(fill = 'fill_color', alpha = 'coverage'), size = 0, colour = NA) +
  # geom_polygon(aes_string(fill = 'fill_color')) +
  geom_path(colour = grey75K, size = 0.06) +
  coord_equal() +
  
  theme_void() +
  theme(legend.position = 'none',
        rect = element_rect(fill = '#ffffff', colour = '#ffffff', size = 0, linetype = 1),
        panel.background = element_rect(fill = bg_fill)) +
  scale_fill_identity() +
  scale_alpha_manual(values = c('0' = 0.5, 'Coverage' = 1)) 


save_plot('~/Documents/USAID/mini projects/Fragile States - (Aaron Roesch)/map_region.pdf',
          width = 7, height = 4)
