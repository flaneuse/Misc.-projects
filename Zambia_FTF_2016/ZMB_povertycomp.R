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

ggplot(data = comb, aes(x = chg_RALS, y = chg_LCMS)) +
  geom_abline(colour = 'red', slope = 1, intercept = 0) + 
  geom_point(size = 5, alpha = 0.3) +
  geom_point(size = 5, alpha = 0.3, colour = 'dodgerblue', data = comb %>% filter(province == 'Eastern')) +
  coord_equal(ratio = 1, ylim = c(-0.2, 0.2),  xlim = c(-0.2, 0.2))
