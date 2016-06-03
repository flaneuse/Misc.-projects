# import cleaned data from .xlsx

library(llamar)
loadPkgs()

ffp = read_excel('~/Documents/USAID/Kyla - FFP reshaping/Tech sector analysis_052316.xlsx', sheet = 2)


ffp = ffp %>% 
  filter(ARRyear == 2015, 
         pgrmType == 'Development')

ffpActive = ffp %>% 
  filter(isActive == 1) 

ffpPct = ffpActive %>% 
  group_by(Country, category1) %>% 
  summarise(num = n()) %>% 
  ungroup() %>% 
  group_by(Country) %>% 
  mutate(pct = num/sum(num),
         categ = ifelse(category1 == 'Maternal Child Health & Nutrition', 'MCH',
                        ifelse(category1 == 'Agriculture & Livelihoods', 'Agriculture & Livelihoods', 
                               ifelse(category1 == 'Program Approaches', 'Program Approaches',
                                      ifelse(category1 == 'Markets, Inputs & Financial Access', 'Markets, Inputs & Financial Access',
                                             ifelse(category1 == 'Water, Sanitation, and Hygiene (WASH)', 'WASH', 'other')
                                      )))))

x = ffpActive %>% 
  group_by(category1) %>% 
  summarise(tot = n()) %>% 
  arrange(desc(tot))

# ffpPct$category1 = factor(ffpPct$category1,
#                              levels = c(
#                             "Maternal Child Health & Nutrition",    
#                             "Program Approaches",                   
#                             "Markets, Inputs & Financial Access",   
#                             "Water, Sanitation, and Hygiene (WASH)",
#                             "Integrated activities/sectors",        
#                             "Sustainability",                       
#                             "Disaster Risk Reduction (DRR)",        
#                             "Infrastructure & Community Assets",    
#                             "Types of Interventions",               
#                             "Uses of Food",                         
#                             "Organization capacity building",       
#                             "HIV",
#                             "Agriculture & Livelihoods"
#                              ))

ffpPct$category1 = factor(ffpPct$category1,
                      levels = c('Maternal Child Health & Nutrition', 'Agriculture & Livelihoods', 
                                 'Program Approaches', 'Markets, Inputs & Financial Access',
                                 'WASH'))

# ffpPct$Country = factor(ffpPct$Country, levels = rev(ffpPct$Country))
ffpPct$Country = factor(ffpPct$Country,
                          levels=  totOrder$Country)

ggplot(ffpPct %>% filter(category1 %in% c('Maternal Child Health & Nutrition', 'Agriculture & Livelihoods')), 
       aes(fill = category1, 
           order = factor(category1),
           x = Country, y = pct, 
           label = percent(pct, 0))) +
  geom_bar(stat = 'identity', position = 'stack', alpha = 0.7) +
  geom_text(colour = 'white', family = 'Segoe UI', size = 4,
            nudge_y = -0.05) +
  theme_xgrid() + 
  coord_flip() +
  scale_y_continuous(labels = scales::percent,
                     name = 'percent of all activities') +
  scale_fill_manual(values  = c('#79857E', '#AB4432')) +
  facet_wrap(~ category1)

write.csv(ffpPct, '~/Documents/USAID/Kyla - FFP reshaping/ffpPct.csv')


# Basic bar plot: # pgrms ------------------------------------------------

totOrder =  ffpActive %>% 
  group_by(Country) %>% 
  summarise(num = n()) %>% 
  arrange(num)

totOrder$Country = factor(totOrder$Country,
                           levels=  totOrder$Country)

ggplot(totOrder, aes(x = Country,
                     y = num,
                     label = num)) +
  geom_bar(stat = 'identity', fill = '#AB4432') +
  geom_text(colour = 'white', family = 'Segoe UI', 
            size = 5, nudge_y = -5,
            hjust = 1) +
  scale_y_continuous(name = 'number of activities',
                     breaks = seq(0, 250, by = 50)) +
  coord_flip() +
  theme_xgrid() 


# WASH analysis -----------------------------------------------------------
wash = ffpActive %>% 
  filter(category1 %like% 'WASH') %>% 
  group_by(Awardee, subcategory) %>% 
  summarise(num = n()) %>% 
  ungroup() %>% 
  group_by(Awardee) %>% 
  mutate(pct = num/sum(num))

wash$subcategory = factor(wash$subcategory, 
                          levels = rev(c('Water Supply',
                                                       'Sanitation',
                                                       'WASH Governance & Capacity',
                                                       'Hygiene')))

ggplot(wash, aes(x = subcategory, 
                 y = pct, 
                fill = subcategory)) +
  geom_bar(stat = 'identity', alpha = 0.7) +
  scale_y_continuous(labels = scales::percent, 
                     name = 'percent of activities') +
  facet_wrap(~Awardee) +
  theme_xgrid() +
  coord_flip() +
  scale_fill_manual(values = c('#AB4432','#6C6947', '#62585C', '#C0953E')) +
  theme(rect = element_rect(fill = '#EFEEED', colour = NA, 
                            size = 0, linetype = 1),
        panel.background = element_rect(fill = 'white'))
  


# Wash-iest ---------------------------------------------------------------
wash2 = ffpActive %>% 
  filter(category1 %like% 'WASH') %>% 
  group_by(Awardee, Country) %>% 
  summarise(num = n()) %>% 
  arrange(num)

washiest = wash2 %>% 
  ungroup() %>% 
  group_by(Awardee) %>% 
  summarise(tot = mean(num)) %>% 
  arrange(desc(tot))

wash2$Awardee = factor(wash2$Awardee, levels = washiest$Awardee)

wash2$Country = factor(wash2$Country, levels = wash2$Country)

ggplot(wash2, aes(y = Country, x = num, 
                  yend = Country, xend = 0, colour = num)) +
  geom_segment() +
  geom_point(size = 4) +
  facet_wrap(~Awardee, scales = 'free_y') +
  xlab('number of activities') +
  scale_colour_gradientn(colours = brewer.pal(9, 'YlGnBu')[2:9]) +
  theme_xgrid()+
  theme(axis.text.y = element_text(size = 10, colour = grey80K),
        rect = element_rect(fill = '#EFEEED', colour = NA, 
                            size = 0, linetype = 1),
        panel.background = element_rect(fill = 'white'),
        strip.text = element_text(size = 10))


ggsave('~/Documents/USAID/Kyla - FFP reshaping/washiest.pdf',
       width = 10, height = 4.5,
       bg = 'transparent',
       paper = 'special',
       units = 'in',
       useDingbats=FALSE,
       compress = FALSE,
       dpi = 300)


# Ethiopia ----------------------------------------------------------------

eth = ffpActive %>% 
  filter(Country == 'Ethiopia')

eth %>% group_by(pgrmNum) %>% summarise(n())

eth %>% group_by(Awardee, category1) %>% summarise(num = n()) %>% arrange(desc(num)) %>% ungroup() %>% 
  group_by(Awardee) %>% 
  mutate(pct = percent(num/sum(num))) %>% 
  filter(category1 %in% c('Maternal Child Health & Nutrition', 'Water, Sanitation, and Hygiene (WASH)', 'Agriculture & Livelihoods'))
