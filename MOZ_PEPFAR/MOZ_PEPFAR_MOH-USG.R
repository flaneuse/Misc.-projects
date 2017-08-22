
# Explore Mozambique PEPFAR data ------------------------------------------
# Laura Hughes, lhugehs@usaid.gov, 21 August 2017

# load libraries ----------------------------------------------------------
library(tidyverse)
library(readxl)
library(stringr)

# import data -------------------------------------------------------------
curr = read_excel('~/Documents/Mini Projects/2017-08_MOZ_PEPFAR/MISAU vs PEPFAR Data Concordance ART Dec16 and March17 FY17 Updated Aug15.xlsx', sheet = 3)

new = read_excel('~/Documents/Mini Projects/2017-08_MOZ_PEPFAR/MISAU vs PEPFAR Data Concordance ART Dec16 and March17 FY17 Updated Aug15.xlsx', sheet = 4)

# TX_CURR should be TX_CURR(last quarter) + TX_NEW - drop outs


# clean and merge the data ------------------------------------------------
# `Health Facilities` + `Health Facility MOH_Name` seems to be unique.  Not Location_USG or iPSL_USG
new = new %>% 
  select(reporting_period = `Periodo de reportagem`,
         province = PROVINCE_USG, 
         district = DISTRICT_USG,
         Location_USG,
         facility_id = `ID da US_MISAU`,
         facility_name = `Unidade Sanitária`,
         tx_new_MOH = `Novos Inícios TARV_MISAU`,
         tx_new_USG = `Novos Inícios TARV__USG`) 
  

curr = curr %>% 
  select(reporting_period = `Reporting Period`,
         province = PROVINCE_USG, 
         district = DISTRICT_USG,
         Location_USG,
         facility_id = `Health Facility MOH_Name`,
         facility_name = `Health Facilities`,
         tx_curr_MOH = TX_CURR_MOH,
         tx_curr_USG = TX_CURR_USG)

# join together, create ID
df = full_join(new, curr) %>% 
  mutate(id = dense_rank(paste0(facility_name, facility_id, Location_USG)),
         diff_curr = tx_curr_USG - tx_curr_MOH,
         diff_new = tx_new_USG - tx_new_MOH,
         pct_curr = diff_curr / tx_curr_USG,
         pct_new = diff_new / tx_new_USG) %>% 
  separate(reporting_period, into = c('quarter', 'year'), sep = '_', remove = FALSE) %>% 
  mutate(quarter = ifelse(quarter == 'Out-Dez', 4,
                          ifelse(quarter == 'Jan-Mar', 1, NA))) %>% 
  group_by(id) %>% 
  arrange(year, quarter) %>%
  mutate(tx_lastQ_MOH = lag(tx_curr_MOH),
         tx_lastQ_USG = lag(tx_curr_USG),
         tx_lastQ_diff = tx_lastQ_USG - tx_lastQ_MOH,
         dropouts_MOH = tx_lastQ_MOH + tx_new_MOH - tx_curr_MOH,
         dropouts_USG = tx_lastQ_USG + tx_new_USG - tx_curr_USG)

# basic plots -------------------------------------------------------------

ggplot(df, aes(x = diff_curr, diff_new)) +
  geom_point(size = 4, alpha = 0.2)

ggplot(df, aes(x = pct_curr, pct_new)) +
  geom_point(size = 4, alpha = 0.2) +
  scale_x_continuous(labels = scales::percent) +
  scale_y_continuous(labels = scales::percent)

ggplot(df %>% filter(pct_curr < 1, pct_new < 1, pct_curr > -1, pct_new > -1), 
       aes(x = pct_curr, pct_new, color = province)) +
  geom_point(size = 4, alpha = 0.3) +
  scale_x_continuous(labels = scales::percent) +
  scale_y_continuous(labels = scales::percent) +
  xlab('percent difference between USG and MOH TX_CURR') +
  ylab('percent difference between USG and MOH TX_NEW') +
  coord_equal() +
  theme_minimal()


ggplot(df, aes(x = dropouts_MOH)) + geom_histogram(binwidth = 50)
ggplot(df, aes(x = dropouts_USG)) + geom_histogram(binwidth = 50)

ggplot(df, aes(x = dropouts_MOH, dropouts_USG)) +
  geom_point(size = 4, alpha = 0.2)


ggplot(df, aes(x = diff_curr, tx_lastQ_diff)) +
  geom_point(size = 4, alpha = 0.2) +
  coord_equal() +
  geom_abline(slope = 1, intercept = 0, color = 'red')
