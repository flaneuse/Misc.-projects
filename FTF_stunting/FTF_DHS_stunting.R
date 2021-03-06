
# Pull latest DHS stats on stunting for FTF countries ---------------------
# Laura Hughes, GeoCenter, lhughes@usaid.gov
# 15 December 2016

# load libraries ----------------------------------------------------------
library(llamar)
library(geocenter)
library(dplyr)
library(ggplot2)
library(RColorBrewer)
library(maptools)

# select countries --------------------------------------------------------

ftf = c('Bangladesh',
        'Cambodia',
        'Ethiopia',
        'Ghana',
        'Guatemala',
        'Haiti',
        'Honduras',
        'Kenya',
        'Liberia',
        'Malawi',
        'Mali',
        'Mozambique',
        'Nepal',
        'Rwanda',
        'Senegal',
        'Tajikistan',
        'Tanzania',
        'Uganda',
        'Zambia')

ftf_addit = c('India', 'Guinea', 'Burma', 'Congo Dem', 'Sierra Leone', 'Nigeria')
ftf_codes = llamar::getDHScountry(c(ftf, ftf_addit))

# import DHS data ---------------------------------------------------------
stunting = llamar::loadDHS(breakdown = 'subnational', indicators = 'CN_NUTS_C_HA2',
                           countries = ftf_codes, apiKey = 'USAAID-405632', numResults = 5000)
# shapefile from DHS website should have only the good names of the Admin1.
# Since Admin1's occasionally break up, for continuity, DHS reports on old and new names.
stunting = read_shp('~/Creative Cloud Files/MAV/Projects/FTF/DHS_stunting_shp/shps', 
                    layerName = 'sdr_subnational_data')
stunting = stunting@data

stunting = stunting %>% 
  rename(SurveyYear = SVYYEAR,
         CountryName = CNTRYNAMEE,
         CharacteristicLabel = DHSREGEN,
         Value = CNNUTSCHA2)


natl = llamar::loadDHS(breakdown = 'national', indicators = 'CN_NUTS_C_HA2',
                       countries = ftf_codes, apiKey = 'USAAID-405632', numResults = 5000)

# find most recent year ---------------------------------------------------
recent_year = natl %>% 
  group_by(CountryName) %>% 
  summarise(SurveyYear = max(SurveyYear))



recent_data = left_join(recent_year, natl) %>% 
  arrange(desc(Value)) %>% 
  mutate(country = paste0(CountryName, ' (', SurveyYear, ')'))

recent_sub = left_join(recent_year, stunting) %>% 
  filter(SurveyYear > 2006) %>% 
  arrange(desc(Value)) %>% 
  mutate(country = paste0(CountryName, ' (', SurveyYear, ')'))


# refactor ----------------------------------------------------------------

# reorder, based on highest nat'l stunting in most recent year.
stunting$CountryName = factor(stunting$CountryName, levels = recent_data$CountryName)
natl$CountryName = factor(natl$CountryName, levels = recent_data$CountryName)
recent_data$CountryName = factor(recent_data$CountryName, levels = recent_data$CountryName)
recent_sub$country = factor(recent_sub$country, levels = recent_data$country)


# trends over time --------------------------------------------------------
alpha_bg = 0.3

pal = brewer.pal(11, 'Spectral')

rect = data.frame(xmin = rep(1995,3), xmax = rep(2016,3),
                  ymin = c(0, 20, 40),
                  ymax = c(20, 40, 60),
                  fill = c(pal[5], pal[4], pal[3]))

ggplot(natl) +
  geom_line(aes(x = SurveyYear, y = Value, group = CountryName),
            colour = grey90K) +
  geom_point(aes(x = SurveyYear, y = Value, group = CountryName),
             size = 1.5, stroke = 0.1,
             colour = grey90K, fill = grey30K, shape = 21) +
  geom_point(aes(x = SurveyYear, y = Value, group = CountryName),
             size = 2.5, data = recent_data,
             stroke = 0.1, shape = 21,
             fill = grey90K, 
             colour = grey90K) +
  geom_text(aes(x = SurveyYear, y = Value, 
                label = paste0(round(Value), '%'), 
                group = CountryName),
            family = 'Lato Light',
            data = recent_data,
            size = 4, colour = grey90K, nudge_y = 10) +
  geom_rect(aes(xmin=xmin, xmax=xmax, 
                ymin=ymin,ymax=ymax,
                fill = fill),
            data = rect,
            alpha = alpha_bg) +
  facet_wrap(~CountryName) +
  ggtitle('Percent of stunted children under 5 in FTF countries',
          subtitle = 'Source: Demographic and Health Surveys') +
  scale_fill_identity() +
  scale_x_continuous(limits = c(1995, 2016),
                     breaks = seq(1995, 2016, by = 10),
                     minor_breaks = seq(1995, 2016, by = 5)) +
  scale_y_continuous(breaks = seq(0, 60, by = 20), limits = c(0, 62)) + 
  theme_xygrid() +
  theme(panel.grid.minor.x = element_line(colour = grey60K, size = 0.1),
        panel.spacing = unit(1, 'lines'),
        plot.subtitle = element_text(family = 'Lato Light', size = 12))

save_plot('~/Creative Cloud Files/MAV/Projects/FTF/FTF_natl_stuntingDHS', saveBoth = TRUE, 
          width = 10, height = 10)

# geographic variation ----------------------------------------------------

p = ggplot(recent_sub) +
  geom_tile(aes(y = forcats::fct_reorder(CharacteristicLabel, Value, .desc=FALSE),
                x = 1, fill = Value),
            colour = 'white', size = 0.25) +
  facet_wrap(~country, scales = 'free') +
  scale_fill_gradientn(colours = rev(pal[1:5])) +
  theme_xylab(legend.position = c(0.9, 0.1),
              legend.direction = 'vertical') +
  ggtitle('Percent of stunted children under 5 within FTF countries, with most recent data',
          subtitle = 'Source: Demographic and Health Surveys') +
  theme(axis.text.x = element_blank(),
        panel.spacing = unit(1, 'lines'),
        axis.text.y = element_text(size = 7),
        strip.text = element_text(size = 9)
  )

save_plot('~/Creative Cloud Files/MAV/Projects/FTF/FTF_subnatl_stuntingDHS', saveBoth = TRUE, 
          width = 14, height = 10)

recent_sub=recent_sub %>% filter(!is.na(CharacteristicLabel))

for(i in unique(recent_sub$CountryName)){
  data2plot = recent_sub %>% filter(CountryName == i)
  
  num_regions = length(unique(data2plot$CharacteristicLabel))
  
  plot_dot(data2plot, value_var =  'Value', label_digits = 0, value_label_offset = 7,
           by_var = 'forcats::fct_reorder(CharacteristicLabel, Value, .desc=FALSE)', 
           sort_by = 'Value', scales = 'free_y', grey_background = TRUE,
           facet_var = 'country', lollipop = T, dot_fill_cont = rev(pal[1:5])) +
    scale_x_continuous(limits = c(0, 60)) +
    scale_fill_gradientn(colours = rev(pal[1:5]), limits = c(6.6, 55.3))
    # ggtitle('Stunting prevalence varies across the countries') +
  
  save_plot(paste0('~/Creative Cloud Files/MAV/Projects/FTF/lollipops/FTF_subnatlLolli_stuntingDHS_', i, '.png'), 
          width = 3, height = num_regions/2.5)
}

# DHS shapefiles w/ data: ------------------------------------------------- 
# Downloaded from http://spatialdata.dhsprogram.com/data/#/single/surveys/indicators/download

dhs_shp = shp2df('~/Creative Cloud Files/MAV/Projects/FTF/DHS_stunting_shp/shps', 
                 layerName = 'sdr_subnational_data', getCentroids = F)

p2 = plot_map(dhs_shp, fill_var = 'CNNUTSCHA2') +
  scale_fill_gradientn(colours = rev(pal[1:5])) +
  facet_wrap(~CNTRYNAMEE, scales = 'free') +
  coord_equal() +
  theme_void()

save_plot('~/Creative Cloud Files/MAV/Projects/FTF/FTF_map_stuntingDHS.pdf', 
          width = 14, height = 10)

p2 = plot_map(dhs_shp, fill_var = 'CNNUTSCHA2', stroke_size = 0) +
  scale_fill_gradientn(colours = rev(pal[1:5])) +
  coord_equal() +
  theme_void()

save_plot('~/Creative Cloud Files/MAV/Projects/FTF/FTF_world_stuntingDHS.pdf', 
          width = 14, height = 10)
