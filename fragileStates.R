
# Load custom plot themes and useful libraries ----------------------------

source('~/GitHub/Rplots/themes_ldh.r')

# Import data -------------------------------------------------------------



fragileFile = '~/GitHub/StataTraining/Data/fragilestatesindex-2006to2014.xlsx'

# Seed w/ 2014 and 2015 data.
fragile = read_excel(fragileFile, sheet = 1)  %>%
  mutate(year = 2014,
         country = `Fragile States Index 2014`) %>% 
  select(-contains('Fragile'))

fragile2015 = read_excel('~/GitHub/StataTraining/Data/fragilestatesindex-2015.xlsx') %>% 
  mutate(year = 2015,
         country = `Fragile States Index 2015`) %>% 
  select(-contains('Fragile'))


fragile = rbind(fragile, fragile2015)

for (i in 2:9) {
  temp = read_excel(fragileFile, sheet = i) 
  
  weirdCol = colnames(temp)[2]
  
  yearNum = paste0(str_extract_all(weirdCol, "[0-9]",
                            simplify = TRUE), collapse =  "")
  
  temp = temp %>% 
    mutate(year = yearNum) %>% 
    mutate_(country = paste0('`', weirdCol, '`')) %>% 
    select(-contains('Failed'))
  
  fragile = rbind(temp, fragile)
  }


# Country by year trends --------------------------------------------------


