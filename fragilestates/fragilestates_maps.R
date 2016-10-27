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


# setup params ------------------------------------------------------------
bg_fill = '#f6f8fb'

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
df = read_excel('~/Documents/USAID/mini projects/Fragile States - (Aaron Roesch)/SBU_fragility_threats.xlsx')

df2015 = df %>% 
  filter(year == 2015) 


# import geo data ---------------------------------------------------------

# import spatial data

geo = frontier::shp2df(baseDir = '~/Documents/USAID/geodata/ne_10m_admin_0_countries_10pctsimpl/',
                 layerName = 'ne_10m_admin_0_countries', getCentroids = FALSE)

# land mass basemap
land = frontier::shp2df(baseDir = '~/Documents/USAID/geodata/ne_10m_land_10pctsimpl/',
                       layerName = 'ne_10m_land', getCentroids = FALSE) 

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
x = full_join(df, codes, by = c("code"))

View(x %>% filter(is.na(in_ne)))

# merges everything, aside from Norway-- which is -99 in WB_A3

# Fix Norway
geo = geo %>% 
  mutate(code = ifelse(NAME == 'Norway', 'NOR', as.character(WB_A3)))

codes = data.frame(code = unique(geo$code),
                   in_ne = TRUE)
# test merge
x = full_join(df, codes, by = c("code"))

View(x %>% filter(is.na(in_ne)))


# Filter out extraneous geo data ------------------------------------------

geo = geo %>% 
  filter(NAME != 'Antarctica') %>% 
  select(code, lat, long, group, order, id, REGION_WB, CONTINENT)

# merge fragile data w/ geo data ------------------------------------------
df_geo = full_join(df2015, geo, by = c("code"))


# choropleth --------------------------------------------------------------

max_val = max(abs(range(df2015$al, na.rm = TRUE)))

p = ggplot(df_geo %>% filter(CONTINENT == 'Africa'), aes_string(x = 'long', y = 'lat',
                           group = 'group', order = 'order')) +
  geom_path(data = land, fill = grey15K) +
  geom_path(data = land, colour = '#89a3d1', size = 2) +
  geom_polygon(aes(fill = al)) +
  geom_path(colour = 'white', size = 0.15) +
  coord_equal() +

  scale_fill_gradientn(colours = rev(brewer.pal(10, 'RdYlBu')), 
                       limits = c(-max_val, max_val),
                       na.value = grey15K) +
  theme_void() +
  theme(legend.position = 'none',
        rect = element_rect(fill = '#ffffff', colour = '#ffffff', size = 0, linetype = 1),
              panel.background = element_rect(fill = bg_fill))

save_plot('~/Documents/USAID/mini projects/Fragile States - (Aaron Roesch)/al_score.pdf',
          width = 7, height = 4)

p = frontier::plot_map(df_geo, fill_var = 'factor(any_last5)') +
  scale_fill_manual(values = c('0' = grey15K, '1' = brewer.pal(11, 'Spectral')[2]))

save_plot('~/Documents/USAID/mini projects/Fragile States - (Aaron Roesch)/any_events_5y.pdf',
          width = 7, height = 4)


p = frontier::plot_map(df_geo, fill_var = 'confdeaths') +
  scale_fill_gradientn(colours = brewer.pal(9, 'BuPu'),
                       na.value = grey15K)

save_plot('~/Documents/USAID/mini projects/Fragile States - (Aaron Roesch)/confdeaths.pdf',
          width = 7, height = 4)

range(df$vedeaths, na.rm =  TRUE)

p = frontier::plot_map(df_geo, fill_var = 'vedeaths') +
  scale_fill_gradientn(colours = brewer.pal(9, 'BuPu'),
                       breaks = 
                       na.value = grey15K)


save_plot('~/Documents/USAID/mini projects/Fragile States - (Aaron Roesch)/vedeaths.pdf',
          width = 7, height = 4)

p = frontier::plot_map(df_geo, fill_var = 'coups') +
  scale_fill_gradientn(colours = brewer.pal(9, 'BuPu'),
                       na.value = grey15K)


save_plot('~/Documents/USAID/mini projects/Fragile States - (Aaron Roesch)/coups.pdf',
          width = 7, height = 4)

p = frontier::plot_map(df_geo, fill_var = 'rgmchngmag') +
  scale_fill_gradientn(colours = brewer.pal(9, 'BuPu'),
                       na.value = grey15K)


save_plot('~/Documents/USAID/mini projects/Fragile States - (Aaron Roesch)/rgmchngmag.pdf',
          width = 7, height = 4)

p = frontier::plot_map(df_geo, fill_var = 'massatrocmag') +
  scale_fill_gradientn(colours = brewer.pal(9, 'BuPu'),
                       na.value = grey15K)


save_plot('~/Documents/USAID/mini projects/Fragile States - (Aaron Roesch)/massatrocmag.pdf',
          width = 7, height = 4)


ggplot(geo, aes_string(x = 'long', y = 'lat',
                      group = 'group', order = 'order')) +

  coord_equal() +
  theme_void() +
  theme(legend.position = 'none')
