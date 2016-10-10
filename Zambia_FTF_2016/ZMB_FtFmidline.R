# Zambia Feed the Future Midline data -----------------------------------------
#
# ZMB_FtFmidline.R: script to visualize Zambia's baseline and midline data from
# the 2015 Zone of Influence Interim Assessment
#
# Data are from USAID's Bureau for Food Security from the draft of the interim report
#
# Laura Hughes, lhughes@usaid.gov, 6 October 2016
#
# Copyright 2016 by Laura Hughes via MIT License


# setup, load packages ----------------------------------------------------
base_dir = '~/Documents/USAID/mini projects/Zambia FtF changes - (BFS)/'

colorFemale = "#9483BD"
colorMale = "#27aae1"
colorTarget = '#fdae61'
colorWEIA = 'BuPu'

size_dot = 5
size_dot_weia = 8
stroke_dot = 0.2
stroke_line = 0.75
alpha_ci = 0.25
width_ci = 2

library(readxl)
library(dplyr)
library(tidyr)
library(stringr)
library(data.table)
library(ggplot2)
library(llamar)
library(extrafont)
library(RColorBrewer)

loadfonts()

# import data -------------------------------------------------------------

df = read_excel(paste0(base_dir, 'Zambia_Interim_Country_Report_9.26.16 Exec Sum table.xlsx'), 
                na = 'NA')

dhs = read_excel(paste0(base_dir, 'ZMB_DHS_malnutrition.xlsx'))

# clean FTF data ----------------------------------------------------------

# tidy data frame

est = df %>% 
  select(indicator, disaggregation, baseline_est, interim_est) %>% 
  gather(year_est, est, c(baseline_est, interim_est)) %>% 
  mutate(year = ifelse(year_est %like% 'baseline', 2012,
                       ifelse(year_est %like% 'interim', 2015, NA)))

ci = df %>% 
  select(indicator, disaggregation, baseline_CI, interim_CI) %>% 
  gather(year_ci, CI, c(baseline_CI, interim_CI)) %>% 
  mutate(year = ifelse(year_ci %like% 'baseline', 2012,
                       ifelse(year_ci %like% 'interim', 2015, NA)))

n = df %>% 
  select(indicator, disaggregation, baseline_n, interim_n) %>% 
  gather(year_n, n, c(baseline_n, interim_n)) %>% 
  mutate(year = ifelse(year_n %like% 'baseline', 2012,
                       ifelse(year_n %like% 'interim', 2015, NA)))

tidy = left_join(est, ci, by = c('year', 'indicator', 'disaggregation'))
tidy = left_join(tidy, n, by = c('year', 'indicator', 'disaggregation'))

# split CIs into two columns
untidy = df %>% 
  mutate(base_CI = baseline_CI,
         mid_CI = interim_CI) %>% 
  rowwise() %>% 
  separate(baseline_CI, into = c('baseline_lb', 'baseline_ub'), sep = ' – ') %>% 
  separate(interim_CI, into = c('interim_lb', 'interim_ub'), sep = ' – ') %>% 
  mutate(baseline_lb= as.numeric(baseline_lb),
         baseline_ub= as.numeric(baseline_ub),
         interim_lb = as.numeric(interim_lb),
         interim_ub = as.numeric(interim_ub))

tidy = tidy %>% 
  mutate(ci_cpy = CI) %>% 
  rowwise() %>% 
  separate(CI, into = c('lb', 'ub'), sep = ' – ') %>% 
  mutate(lb = as.numeric(lb),
         ub = as.numeric(ub))

untidy = untidy %>% 
  ungroup() %>% 
  mutate(sex = case_when(untidy$disaggregation %like% 'All children' ~ 'total',
                         untidy$disaggregation %like% 'Female' ~ 'female',
                         untidy$disaggregation %like% 'Male' ~ 'male',
                         TRUE ~ NA_character_))

tidy = tidy %>% 
  ungroup() %>% 
  mutate(sex = case_when(tidy$disaggregation %like% 'All children' ~ 'total',
                         tidy$disaggregation %like% 'Female' ~ 'female',
                         tidy$disaggregation %like% 'Male' ~ 'male',
                         TRUE ~ NA_character_))

# clean dhs ---------------------------------------------------------------
dhs = dhs %>% 
  mutate(est = Value/100,
         sex = case_when(dhs$`Characteristic Label` %like% 'Total' ~ 'total',
                         dhs$`Characteristic Label` %like% 'Female' ~ 'female',
                         dhs$`Characteristic Label` %like% 'Male' ~ 'male',
                         TRUE ~ NA_character_))


# stunting plots ----------------------------------------------------------
stunting_target = .376 # from FTF-MS


st_tidy = tidy %>% filter(indicator %like% 'stunt') %>% 
  select(year, est, lb, ub, n, sex) %>% 
  # percent-ize
  mutate(est = est/100,
         lb = lb/100,
         ub = ub/100,
         group = 'FTF')

st_dhs = dhs %>% filter(Indicator %like% 'stunt') %>% 
  select(year = `Survey Year`, est, sex) %>% 
  mutate(group = 'DHS', lb = NA, ub = NA)

st_tidy = bind_rows(st_tidy, st_dhs)

st_untidy = untidy %>% filter(indicator %like% 'stunt') %>% 
  # percent-ize
  mutate(baseline_est = baseline_est/100,
         interim_est = interim_est/100)


ggplot(st_tidy, aes(fill = sex, colour = sex, 
                    alpha = group, size = group)) +
  
  # -- target --
  geom_hline(colour = colorTarget, yintercept = stunting_target, size = 0.25) + 
  
  # -- CIs --
  # geom_segment(aes(x = year, xend = year, y = lb, yend = ub),
  # alpha = alpha_ci, size = width_ci*100) +
  geom_segment(aes(x = year, xend = year, y = lb, yend = ub, group = group),
               alpha = alpha_ci, size = width_ci) +
  
  # -- connector --
  geom_line(aes(x = year, y = est, group = group, 
                linetype = group),
            size = stroke_line) +
  
  # -- point estimate -- 
  geom_point(aes(x = year, y = est,
                 shape = sex, group = group),
             stroke = stroke_dot,
             colour = grey90K) +
  geom_text(aes(x = year, y = est,
                label = round(est*100,0), group = group),
            size = 2, family = 'Lato',
            colour = 'white') +
  
  # -- scales --
  scale_y_continuous(labels = scales::percent) +
  scale_x_continuous(breaks = c(1992, 2002, 2012, 2015), position = 'top') +
  scale_colour_manual(values = c('total' = grey50K, 'female' = colorFemale, 'male' = colorMale)) +
  scale_fill_manual(values = c('total' = grey50K, 'female' = colorFemale, 'male' = colorMale)) +
  scale_alpha_discrete(range = c(0.25, 1)) + 
  scale_size_discrete(range = c(size_dot/2, size_dot)) +
  scale_shape_manual(values = c('total' = 21, 'female' = 22, 'male' = 23)) +
  scale_linetype_manual(values = c('DHS' = 2, 'FTF' = 1)) +
  
  # -- themes --
  theme_ygrid() +
  theme(rect = element_rect(fill = grey10K, colour = grey25K, size = 0, linetype = 1),
        panel.background = element_rect(fill = 'white'),
        axis.title.y = element_blank(),
        axis.title.x = element_text(size = 12),
        strip.text = element_text(family = 'Lato Light', colour = grey90K, size = 14)) +
  # -- annotation --
  annotate(geom = 'text', label = 'DHS', x = 1992, y = 0.5, family = 'Lato Light', size = 5) +
  annotate(geom = 'text', label = 'FTF ZOI', x = 2012, y = 0.5, family = 'Lato Light', size = 5) +
  ggtitle('Stunting prevalence decreased in the ZOI, mirroring a national trend',
          subtitle = 'percent of stunted children under 5 years of age')  +
  facet_wrap(~ sex)

save_plot('~/Creative Cloud Files/MAV/Projects/ZMB_FTFmidline_2016-10/ZMB_stunting.pdf', width = 8, height = 4.5)

# wasting plots ----------------------------------------------------------
wasting_target = 0.025 # from FTF-MS


wast_tidy = tidy %>% filter(indicator %like% 'wast') %>% 
  select(year, est, lb, ub, n, sex) %>% 
  # percent-ize
  mutate(est = est/100,
         lb = lb/100,
         ub = ub/100,
         group = 'FTF')

wast_dhs = dhs %>% filter(Indicator %like% 'wast') %>% 
  select(year = `Survey Year`, est, sex) %>% 
  mutate(group = 'DHS', lb = NA, ub = NA)

wast_tidy = bind_rows(wast_tidy, wast_dhs)


ggplot(wast_tidy, aes(fill = sex, colour = sex, 
                      alpha = group, size = group)) +
  
  # -- target --
  geom_hline(colour = colorTarget, yintercept = wasting_target, size = 0.25) + 
  
  # -- CIs --
  # geom_segment(aes(x = year, xend = year, y = lb, yend = ub),
  # alpha = alpha_ci, size = width_ci*100) +
  geom_segment(aes(x = year, xend = year, y = lb, yend = ub, group = group),
               alpha = alpha_ci, size = width_ci) +
  
  # -- connector --
  geom_line(aes(x = year, y = est, group = group, 
                linetype = group),
            size = stroke_line) +
  
  # -- point estimate -- 
  geom_point(aes(x = year, y = est,
                 shape = sex, group = group),
             stroke = stroke_dot,
             colour = grey90K) +
  geom_text(aes(x = year, y = est,
                label = round(est*100,0), group = group),
            size = 2, family = 'Lato',
            colour = 'white') +
  
  # -- scales --
  scale_y_continuous(labels = scales::percent) +
  scale_x_continuous(breaks = c(1992, 2002, 2012, 2015), position = 'top') +
  scale_colour_manual(values = c('total' = grey50K, 'female' = colorFemale, 'male' = colorMale)) +
  scale_fill_manual(values = c('total' = grey50K, 'female' = colorFemale, 'male' = colorMale)) +
  scale_alpha_discrete(range = c(0.25, 1)) + 
  scale_size_discrete(range = c(size_dot/2, size_dot)) +
  scale_shape_manual(values = c('total' = 21, 'female' = 22, 'male' = 23)) +
  scale_linetype_manual(values = c('DHS' = 2, 'FTF' = 1)) +
  
  # -- themes --
  theme_ygrid() +
  theme(rect = element_rect(fill = grey10K, colour = grey25K, size = 0, linetype = 1),
        panel.background = element_rect(fill = 'white'),
        axis.title.y = element_blank(),
        axis.title.x = element_text(size = 12),
        strip.text = element_text(family = 'Lato Light', colour = grey90K, size = 14)) +
  # -- annotation --
  annotate(geom = 'text', label = 'national (DHS)', x = 1992, y = 0.1, family = 'Lato Light', size = 5) +
  annotate(geom = 'text', label = 'FTF ZOI', x = 2012, y = 0.1, family = 'Lato Light', size = 5) +
  ggtitle('While nationally wasting has remained around 6% for the past 2 decades, it is lower in the ZOI',
          subtitle = 'percent of wasted children under 5 years of age')  +
  facet_wrap(~ sex)

save_plot('~/Creative Cloud Files/MAV/Projects/ZMB_FTFmidline_2016-10/ZMB_wasting.pdf', width = 8, height = 4.5)

# underweight plots ----------------------------------------------------------
underwt_target = 0.12 # from FTF-MS


under_tidy = tidy %>% filter(indicator %like% 'underweight child') %>% 
  select(year, est, lb, ub, n, sex) %>% 
  # percent-ize
  mutate(est = est/100,
         lb = lb/100,
         ub = ub/100,
         group = 'FTF')

under_dhs = dhs %>% filter(Indicator %like% 'under') %>% 
  select(year = `Survey Year`, est, sex) %>% 
  mutate(group = 'DHS', lb = NA, ub = NA)

under_tidy = bind_rows(under_tidy, under_dhs)


ggplot(under_tidy, aes(fill = sex, colour = sex, 
                       alpha = group, size = group)) +
  
  # -- target --
  geom_hline(colour = colorTarget, yintercept = underwt_target, size = 0.25) + 
  
  # -- CIs --
  # geom_segment(aes(x = year, xend = year, y = lb, yend = ub),
  # alpha = alpha_ci, size = width_ci*100) +
  geom_segment(aes(x = year, xend = year, y = lb, yend = ub, group = group),
               alpha = alpha_ci, size = width_ci) +
  
  # -- connector --
  geom_line(aes(x = year, y = est, group = group, 
                linetype = group),
            size = stroke_line) +
  
  # -- point estimate -- 
  geom_point(aes(x = year, y = est,
                 shape = sex, group = group),
             stroke = stroke_dot,
             colour = grey90K) +
  geom_text(aes(x = year, y = est,
                label = round(est*100,0), group = group),
            size = 2, family = 'Lato',
            colour = 'white') +
  
  # -- scales --
  scale_y_continuous(labels = scales::percent) +
  scale_x_continuous(breaks = c(1992, 2002, 2012, 2015), position = 'top') +
  scale_colour_manual(values = c('total' = grey50K, 'female' = colorFemale, 'male' = colorMale)) +
  scale_fill_manual(values = c('total' = grey50K, 'female' = colorFemale, 'male' = colorMale)) +
  scale_alpha_discrete(range = c(0.25, 1)) + 
  scale_size_discrete(range = c(size_dot/2, size_dot)) +
  scale_shape_manual(values = c('total' = 21, 'female' = 22, 'male' = 23)) +
  scale_linetype_manual(values = c('DHS' = 2, 'FTF' = 1)) +
  
  # -- themes --
  theme_ygrid() +
  theme(rect = element_rect(fill = grey10K, colour = grey25K, size = 0, linetype = 1),
        panel.background = element_rect(fill = 'white'),
        axis.title.y = element_blank(),
        axis.title.x = element_text(size = 12),
        strip.text = element_text(family = 'Lato Light', colour = grey90K, size = 14)) +
  # -- annotation --
  annotate(geom = 'text', label = 'national (DHS)', x = 1992, y = 0.1, family = 'Lato Light', size = 5) +
  annotate(geom = 'text', label = 'FTF ZOI', x = 2012, y = 0.1, family = 'Lato Light', size = 5) +
  ggtitle('Underweight CIs are huge.',
          subtitle = 'percent of underweight children under 5 years of age')  +
  facet_wrap(~ sex)

save_plot('~/Creative Cloud Files/MAV/Projects/ZMB_FTFmidline_2016-10/ZMB_underweight.pdf', width = 8, height = 4.5)

# WEIA --------------------------------------------------------------------
weia = untidy %>% 
  filter(indicator %like% 'Empowerment') %>% 
  mutate(baseline_est = baseline_est/100,
         interim_est = interim_est/100,
         baseline_lb = baseline_lb/100,
         baseline_ub = baseline_ub/100,
         interim_lb = interim_lb/100,
         interim_ub = interim_ub/100) # percentize

weia$disaggregation = factor(weia$disaggregation,
                             levels = rev(c("Control over use of income",
                                        "Ownership of assets", 
                                        "Leisure",  
                                        "Input in productive decisions",
                                        "Autonomy in production",
                                        "Speaking in public", 
                                        "Group member",                        
                                        "Purchase, sale or transfer of assets",
                                        "Workload",                            
                                        "Access to and decisions on credit"   
                                        )))


ggplot(weia, aes(y = disaggregation)) +
  # # -- CIs --
  # geom_segment(aes(x = baseline_lb,
  #                  xend = baseline_ub, 
  #                  y = forcats::fct_reorder(disaggregation, interim_est), 
  #                  yend = forcats::fct_reorder(disaggregation, interim_est)),
  #              colour = grey60K,
  #              size = 1.5,
  #              alpha = 0.3) +
  # geom_segment(aes(x = interim_lb,
  #                  xend = interim_ub, 
  #                  y = forcats::fct_reorder(disaggregation, interim_est), 
  #                  yend = forcats::fct_reorder(disaggregation, interim_est)),
  #              colour = grey60K,
  #              size = 1.5,
  #              alpha = 0.3) +
  
  # -- lollipop stick --
  geom_segment(aes(x = baseline_est,
                   xend = interim_est, 
                   y = disaggregation, 
                   yend = disaggregation),
               colour = grey60K, 
               arrow = arrow(length = unit(0.1, 'inches'),
                             ends = 'last'),
               size = 0.25
               ) +
  # -- points --
  geom_point(aes(x = baseline_est),
             size = size_dot_weia, 
             shape = 21,
             fill = 'white',
             stroke = 0,
             colour = grey90K) +
  
  geom_point(aes(x = baseline_est, 
                 fill = baseline_est),
             alpha = 0.2,
             size = size_dot_weia, 
             shape = 21,
             stroke = 0.125,
             colour = grey90K) +
  geom_point(aes(x = interim_est, fill = interim_est),
             size = size_dot_weia, 
             shape = 22,
             stroke = 0.125,
             colour = grey90K) +

  
  # -- scales --
  scale_x_continuous(limits = c(0,1), labels = scales::percent) +
  scale_fill_gradientn(colours = brewer.pal(9, colorWEIA),
                       limits = c(0,1)) +
  scale_colour_gradientn(colours = brewer.pal(9, colorWEIA),
                       limits = c(0,1)) +
  # scale_color_text(weia$est) +
  
  # -- annotations --
  ggtitle("Workload was the largest gain in women's empowerment between 2012 - 2015", 
subtitle = "percent of women achieving adequacy on Women’s Empowerment in Agriculture Index
indicators") +
  
  geom_text(aes(x = baseline_est, 
                colour = baseline_est,
                label = round(baseline_est*100,0)),
            size = 3, family = 'Lato') +
  geom_text(aes(x = interim_est,
                label = round(interim_est*100,0)),
            colour = 'white',
            size = 3, family = 'Lato') +
  # -- themes --
  theme_xgrid() +
  theme(rect = element_rect(fill = grey10K, colour = grey25K, size = 0, linetype = 1),
        panel.background = element_rect(fill = 'white'),
        axis.title.y = element_blank(),
        axis.text.y = element_text(size = 14),
        axis.title.x = element_blank(),
        strip.text = element_text(family = 'Lato Light', colour = grey90K, size = 14))
  

save_plot('~/Creative Cloud Files/MAV/Projects/ZMB_FTFmidline_2016-10/ZMB_WEIA.pdf', width = 8, height = 6)

# Prevalence of poverty ---------------------------------------------------
pov_target = 0.7018 # from FTFMS

ggplot() +
  geom_point() +
  ggtitle('Though poverty prevalence has significantly declined, the ZOI still lags relative to the rest of Zambia',
          subtitle = 'percent of people living on less than $1.25/day (2005 PPP)')

# hunger ---------------------------------------------------------

hunger = tidy %>% 
  filter(indicator %like% 'hunger',
         is.na(sex)) %>% 
  mutate(est = est/100,
         lb = lb/100,
         ub = ub/100)

ggplot(hunger, aes(y = est, x = year)) +
  geom_bar(aes(y = 1), stat = 'identity',
           fill = grey20K) +
  geom_bar(stat = 'identity', fill = brewer.pal(11, 'Spectral')[2],
           alpha = 0.75) +
  
  geom_text(aes(label = paste0(round(lb*100, 0), ' - ', percent(ub,0), ' households with moderate or severe hunger')),
            colour = brewer.pal(11, 'Spectral')[2], hjust = 1,
            size = 4, 
            family = 'Lato Light') +

  geom_text(aes(y = 1,
    label = paste0(round((1 - ub) * 100, 0), ' - ', percent(1 - lb,0), ' households without moderate or severe hunger')),
            colour = grey75K, hjust = 1,
            size = 4, 
            family = 'Lato Light') +
  coord_flip() +
  scale_x_continuous(breaks = c(2012, 2015)) +
  scale_y_continuous(labels = scales::percent) +
  ggtitle('Though gains were made in children eating acceptable diets and dietary diversity of women, household hunger increased significantly') +
  theme_xylab()

save_plot('~/Creative Cloud Files/MAV/Projects/ZMB_FTFmidline_2016-10/ZMB_hungry.pdf', 
          width = 7, height = 2.5)


# women’s dietary diversity -----------------------------------------------

dd = tidy %>% 
  filter(indicator %like% 'Diet')

ggplot(dd, aes(x = year, y = est)) +
  geom_area(aes(y = 9), fill = grey20K) +
  geom_area(stat = 'identity', fill = brewer.pal(11, 'Spectral')[10], 
            alpha = 0.75) +
  geom_point(fill = brewer.pal(11, 'Spectral')[10],
             size = 8,
             stroke = stroke_dot,
             colour = grey90K,
             shape = 21,
             alpha = 1) +
  geom_text(aes(x = year, y = est,
                label = round(est, 1)),
            size = 4, family = 'Lato',
            colour = 'white') +
  scale_x_continuous(breaks = c(2012, 2015), position = 'top'
                    ) +
  scale_y_continuous(breaks = c(0, 3, 6, 9),
                     labels = c(0, 3, 6, '9 food groups')) +
  ggtitle('mean food groups consumed by women of reproductive age') +
  theme_ygrid() +
  theme(panel.ontop = TRUE,
        axis.title.y = element_blank())

save_plot('~/Creative Cloud Files/MAV/Projects/ZMB_FTFmidline_2016-10/ZMB_womenDD.pdf', width = 5, height = 7)


# women’s dietary diversity -----------------------------------------------

mad = tidy %>% 
  filter(indicator %like% 'min',
         sex == 'total') %>% 
  mutate(est = est/100)

ggplot(mad, aes(x = year, y = est)) +
  geom_area(aes(y = 1), fill = grey20K) +
  geom_area(stat = 'identity', fill = brewer.pal(11, 'Spectral')[10], 
            alpha = 0.75) +
  geom_point(fill = brewer.pal(11, 'Spectral')[10],
             size = 8,
             stroke = stroke_dot,
             colour = grey90K,
             shape = 21,
             alpha = 1) +
  geom_text(aes(x = year, y = est,
                label = percent(est, 0)),
            size = 3.25, family = 'Lato',
            colour = 'white') +
  scale_x_continuous(breaks = c(2012, 2015), position = 'top'
  ) +
  scale_y_continuous(labels = scales::percent) +
  ggtitle('percent of children 6-23 months receiving a minimum acceptable diet') +
  theme_ygrid() +
  theme(panel.ontop = TRUE,
        axis.title.y = element_blank())

save_plot('~/Creative Cloud Files/MAV/Projects/ZMB_FTFmidline_2016-10/ZMB_kidsMAD.pdf', width = 5, height = 7)
