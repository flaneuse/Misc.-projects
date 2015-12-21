
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


# calc lag ----------------------------------------------------------------

fragile = fragile %>% 
  group_by(country, year) %>% 
  mutate(y = lag(Total))

fragile$id = 1:nrow(fragile)

# Country by year trends --------------------------------------------------
fragile = fragile %>% 
  filter(!is.na(Total)) %>% 
  mutate(x = ntile(country, 4))

ggplot(fragile, aes(x = year, y = Total, group = country)) +
  geom_line(size = 2, color = grey50K, alpha = 0.3) +
  facet_wrap(~x) +
  theme_yGrid()

# What's happened to the average?
ggplot(fragile, aes(x = year, y = Total, group = 1)) +
  stat_summary(geom = 'line', fun.y = mean, 
               size = 2, color = 'dodgerblue') + 
  theme_jointplot() +
  coord_cartesian(ylim = c(0, 115))


fragile %>% 
  mjs_plot(x=year, y=Total) %>%
  mjs_point()


all_values <- function(x) {
  if(is.null(x)) return(NULL)
  paste0(names(x), ": ", format(x), collapse = "<br />")
  }

fragile %>% 
  group_by(country) %>% 
  ggvis(~year, ~Total) %>% 
  layer_paths() %>% 
  add_tooltip(all_values, 'hover')

