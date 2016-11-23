# Zambia Feed the Future Midline data -----------------------------------------
#
# ZMB_povertycomp.R: script to visualize Zambia's percentage of people in poverty
# by district
#
# Data are from three sources:
# 1. The 2010 and 2015 Living Conditions Monitoring Survey
# 2. The 2012 and 2015 Rural Agricultural Livelihoods Survey
# 3. A 2015 fitted model from the World Bank using the LCMS and a 2010 Census
#
# Laura Hughes, lhughes@usaid.gov, 6 October 2016
#
# Copyright 2016 by Laura Hughes via MIT License


# import data -------------------------------------------------------------

# -- 2010 and 2015 LCMS --
# Source: http://catalog.ihsn.org/index.php/catalog/2597 (2010)
# Source: http://www.zamstats.gov.zm/report/Lcms/LCMS%202015%20Summary%20Report.pdf (2015; can only see cached)
# From the National Statistics Bureau
# Figure 32: Headcount Poverty by Province (2015 report)
# Figure 12.5: Poverty Changes by Province, 2006 - 2010 and Figure 12.2: Changes in Poverty levels by residence, 2006 - 2010 (2010 report)
# http://eaz.org.zm/wp-content/uploads/2016/04/12a-Poverty-26_04_16v2.pdf (2015)

# Disclaimer from the Key Stats report:
# There have been a number of improvements in the method used to measure poverty during the 2015 poverty analysis. Therefore, users should take into consideration these methodological improvements when explaining the change in the level of poverty between 2010 and 2015.

lcms = data.frame(province = c('Central',
                          'Copperbelt',
                          'Eastern',
                          'Luapula',
                          'Lusaka',
                          'Muchinga', # Didn't exist in 2010
                          'Northern',
                          'NorthWestern',
                          'Southern',
                          'Western',
                          'RuralZambia'),
                  pov2006 = c(70.7, 37.3, 78.5, 73.9, 24.7, NA, 78.5, 70.7, 73, 83.3, 80.3),
                  pov2010 = c(60.9, 34.3, 77.9, 80.5, 24.4, NA, 75, 67, 67.9, 80.4, 77.9),
                  pov2015 = c(56.2, 30.8, 70, 81.1, 20.2, 69.3, 79.7, 66.4, 57.6, 82.2, 76.6)
) 

lcms = lcms %>% 
  mutate(chg_LCMS = (pov2015 - pov2010)/ pov2010)

# -- 2012 and 2015 RALS --
# Source: 
# Funded by USAID, Sida, FAO
rals = data.frame(province = c('Central',
                               'Copperbelt',
                               'Eastern',
                               'Luapula',
                               'Lusaka',
                               'Muchinga',
                               'Northern',
                               'NorthWestern',
                               'Southern',
                               'Western',
                               'RuralZambia'),
                  pov2012 = c(76.0,
                             75.7,
                             80.6,
                             71.3,
                             60.1,
                             74.8,
                             68.8,
                             76.8,
                             76.6,
                             85.1,
                             76.0),
                  pov2015 = c(69.5,
                             68.0,
                             84.3,
                             78.5,
                             58.4,
                             83.1,
                             81.8,
                             74.7,
                             75.8,
                             83.3,
                             78.0)
)

rals = rals %>% 
  mutate(chg_RALS = (pov2015 - pov2012)/pov2012)

# -- World Bank Poverty Mapping --
# Source: http://documents.worldbank.org/curated/en/766931468137977527/pdf/952760WP0Mappi0mbia0Report00PUBLIC0.pdf

wb = data.frame(province = c(
  'Central',   
  'Copperbelt',  
  'Eastern',     
  'Luapula',     
  'Lusaka',      
  'Muchinga',    
  'Northern',    
  'NorthWestern',
  'Southern',    
  'Western',     
  'National'  ),  
  combSurveys2010 = c(0.61,
                      0.34,
                      0.79,
                      0.80,
                      0.24,
                      0.78,
                      0.73,
                      0.67,
                      0.68,
                      0.80,
                      0.60),
  combSurveys_lb = c(0.56,
                     0.29,
                     0.75,
                     0.76,
                     0.19,
                     0.71,
                     0.68,
                     0.60,
                     0.63,
                     0.76,
                     0.58),
  combSurveys_ub = c(0.66,
                     0.40,
                     0.82,
                     0.85,
                     0.30,
                     0.84,
                     0.78,
                     0.74,
                     0.73,
                     0.85,
                     0.62),
  combSurveys_se = c(0.03,
                     0.03,
                     0.02,
                     0.02,
                     0.03,
                     0.03,
                     0.03,
                     0.04,
                     0.03,
                     0.02,
                     0.01),
  povmap = c(0.65,
             0.37,
             0.80,
             0.79,
             0.25,
             0.77,
             0.76,
             0.64,
             0.68,
             0.84,
             0.60),
  povmap_se = c( 0.026,
                 0.018,
                 0.022,
                 0.022,
                 0.031,
                 0.027,
                 0.024,
                 0.033,
                 0.016,
                 0.013,
                 0.01)
)


# Join data ---------------------------------------------------------------

comb = full_join(rals, lcms, by = 'province')


# x-y scatter -------------------------------------------------------------


ggplot(data = comb, aes(x = chg_RALS, y = chg_LCMS)) +
  geom_abline(colour = 'red', slope = 1, intercept = 0) + 
  geom_vline(colour = 'black', xintercept = 0) +
  annotate(geom = 'rect', xmin = -0.2, xmax = 0, ymin = -0.2, ymax = 0, 
            fill = 'red',
            alpha = 0.05) +
  # annotate(geom = 'rect', xmin = -0.2, xmax = 0, ymin = 0.2, ymax = 0, 
            # fill = 'yellow',
            # alpha = 0.2) +
  # annotate(geom = 'rect', xmin = 0.2, xmax = 0, ymin = -0.2, ymax = 0, 
            # fill = 'yellow',
            # alpha = 0.2) +
  annotate(geom = 'rect', xmin = 0.2, xmax = 0, ymin = 0.2, ymax = 0, 
            fill = 'blue',
            alpha = 0.05) +
  geom_point(aes(fill = chg_RALS), 
             stroke = 0.125,
             shape = 21,
             colour = grey90K,
             size = 5, alpha = 0.3) +
  coord_equal(ratio = 1, ylim = c(-0.2, 0.2),  xlim = c(-0.2, 0.2)) +
  scale_fill_gradientn(colours = brewer.pal(11, 'RdYlBu')) +
  theme_xygrid()



# difference dot plot -----------------------------------------------------

ggplot(comb, aes(y = forcats::fct_reorder(province, chg_LCMS))) +
  geom_segment(aes(x = chg_RALS, xend = chg_LCMS, yend = forcats::fct_reorder(province, chg_LCMS)),
             size = 0.35,
             colour = grey70K) +
    geom_point(aes(x = chg_RALS, fill = chg_RALS),
             size = 5,
             shape = 21,
             colour = grey90K, 
             stroke = 0.125) +
  geom_point(aes(x = chg_LCMS, fill = chg_LCMS),
             size = 5,
             shape = 24,
             colour = grey90K, 
             stroke = 0.125) +
  scale_fill_gradientn(colours = rev(brewer.pal(11, 'RdYlBu')),
                       limits = c(-0.2, 0.2)) +
  scale_x_reverse(labels = scales::percent, limits = c(0.2, -0.2)) +
  theme_xgrid() +
  theme(legend.position = c(0.7, 0.7),
        legend.direction = 'horizontal')


save_plot('~/Creative Cloud Files/MAV/Projects/ZMB_FTFmidline_2016-10/ZMB_povertyChg.pdf', width = 8, height = 7)


# RALS slope plot ---------------------------------------------------------
eastern_rals  = rals %>% 
  filter(province == 'Eastern') %>% 
  gather(year, est, -province, -chg_RALS) %>% 
  mutate(year = as.numeric(str_replace(year, 'pov', '')),
         est = est/100)

ggplot(eastern_rals, aes(x = year, y = est, fill = chg_RALS)) +
  geom_area(aes(y = 1), fill = grey20K) +
  geom_area(stat = 'identity', 
            alpha = 1) +
  geom_hline(yintercept = eastern_rals[1, 'est'],
             size = 0.325,
             colour = grey75K) +
  geom_point(size = 5,
             fill = NA,
             stroke = stroke_dot,
             colour = grey90K,
             shape = 24,
             alpha = 1, 
             data = eastern_lcms) +
  geom_point(size = 10,
             stroke = stroke_dot,
             colour = grey90K,
             shape = 21,
             alpha = 1) +
  geom_text(aes(x = year, y = est,
                label = percent(est, 0)),
            size = 3.25, family = 'Lato',
            colour = grey75K) +
  scale_x_continuous(limits = c(2010, 2015),
    breaks = c(2012, 2015), position = 'top'
  ) +
  scale_y_continuous(labels = scales::percent) +
  scale_fill_gradientn(colours = rev(brewer.pal(11, 'RdYlBu')),
                       limits = c(-0.2, 0.2)) +
  ggtitle('percent of households living in poverty') +
  theme_ygrid() +
  theme(panel.ontop = TRUE,
        axis.title.y = element_blank())

save_plot('~/Creative Cloud Files/MAV/Projects/ZMB_FTFmidline_2016-10/ZMB_povertyChg_RALS.pdf', width = 3.5, height = 7)

# LCMS slope plot ---------------------------------------------------------
eastern_lcms  = lcms %>% 
  filter(province == 'Eastern') %>% 
  gather(year, est, -province, -chg_LCMS) %>% 
  mutate(year = as.numeric(str_replace(year, 'pov', '')),
         est = est/100)

ggplot(eastern_lcms, aes(x = year, y = est, fill = chg_LCMS)) +
  geom_area(aes(y = 1), fill = grey20K) +
  geom_area(stat = 'identity', 
            alpha = 1) +
  geom_hline(yintercept = eastern_lcms[1, 'est'],
             size = 0.325,
             colour = grey75K) +
  geom_point(size = 5,
             fill = NA,
             stroke = stroke_dot,
             colour = grey90K,
             shape = 21,
             alpha = 1, 
             data = eastern_rals) +
    geom_point(size = 10,
             stroke = stroke_dot,
             colour = grey90K,
             shape = 24,
             alpha = 1) +
  geom_text(aes(x = year, y = est,
                label = percent(est, 0)),
            size = 3.25, family = 'Lato',
            colour = grey75K) +
  scale_x_continuous(limits = c(2010, 2015),
                     breaks = c(2010, 2015), position = 'top'
  ) +
  scale_y_continuous(labels = scales::percent) +
  scale_fill_gradientn(colours = rev(brewer.pal(11, 'RdYlBu')),
                       limits = c(-0.2, 0.2)) +
  ggtitle('percent of households living in poverty') +
  theme_ygrid() +
  theme(panel.ontop = TRUE,
        axis.title.y = element_blank())

save_plot('~/Creative Cloud Files/MAV/Projects/ZMB_FTFmidline_2016-10/ZMB_povertyChg_LCMS.pdf', width = 3.5, height = 7)
