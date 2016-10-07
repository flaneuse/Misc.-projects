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

size_dot = 5
alpha_ci = 0.25

library(readxl)
library(dplyr)
library(tidyr)
library(stringr)
library(data.table)
library(ggplot2)
library(llamar)

# import data -------------------------------------------------------------

df = read_excel(paste0(base_dir, 'Zambia_Interim_Country_Report_9.26.16 Exec Sum table.xlsx'), 
                na = 'NA')

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


# stunting plots ----------------------------------------------------------
stunting_target = .376 # from FTF-MS


st_tidy = tidy %>% filter(indicator %like% 'stunt') %>% 
  select(-year_est) %>% 
  # percent-ize
  mutate(est = est/100,
         lb = lb/100,
         ub = ub/100)

ggplot(st_tidy, aes(fill = disaggregation)) +
  
  # -- CIs --
  geom_segment(aes(x = year, xend = year, y = lb, yend = ub),
               alpha = alpha_ci) +
    # -- point estimate -- 
  geom_point(aes(x = year, y = est,
                 shape = disaggregation),
             stroke = 0.1,
             colour = grey90K,
             size = size_dot) +
  
  # -- scales --
  scale_y_continuous(labels = scales::percent) +
  scale_x_continuous(breaks = c(2012, 2015), position = 'top') +
  scale_fill_manual(values = c('All children' = grey50K, 'Female children' = colorFemale, 'Male children' = colorMale)) +
  scale_shape_manual(values = c('All children' = 21, 'Female children' = 22, 'Male children' = 23)) +
  
  # -- themes --
  theme_ygrid() +
  theme(axis.title.y = element_blank()) +
  ggtitle('Stunting prevalence significantly decreased in the ZOI',
          subtitle = 'percent of stunted children under 5 years of age')

