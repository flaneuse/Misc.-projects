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

library(readxl)
library(dplyr)
library(tidyr)
library(stringr)
library(data.table)
library(ggplot2)

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
  separate(interim_CI, into = c('interim_lb', 'interim_ub'), sep = ' – ')


# stunting plots ----------------------------------------------------------
stunting = est %>% filter(indicator %like% 'stunt', !disaggregation %like% 'All') %>% 
  select(-year_est) %>% 
  spread(year, est)

ggplot(stunting, aes(colour = disaggregation)) +
  geom_segment(aes(x = 2012, xend = 2015, y = `2012`, yend = `2015`)) +
  geom_point(aes(x = 2012, y = `2012`), size = 5) +
  geom_point(aes(x = 2015, y = `2015`), size = 5) +
  theme_bw() +
  ylab('stunting percentage')
  
ggplot(stunting, aes(colour = disaggregation)) +
  geom_segment(aes(y = 2012, yend = 2012,x = 40.8, xend = 51.2)) +
  geom_segment(aes(y = 2015, yend = 2015,x = 34.3, xend = 41.4)) +
  geom_segment(aes(y = 2012, yend = 2015,x = 51.2, xend = 41.4), colour = 'grey', linetype = 2) +
  geom_segment(aes(y = 2012, yend = 2015,x = 40.8, xend = 34.3), colour = 'grey', linetype = 2) +
  geom_point(aes(y = 2012, x = `2012`), size = 5) +
  geom_point(aes(y = 2015, x = `2015`), size = 5) +
  theme_bw() +
  scale_y_reverse() +
  ylab('year') +
  xlab('stunting percentage')
