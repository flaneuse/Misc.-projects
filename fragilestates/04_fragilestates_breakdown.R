vars = c('any', 'anybroad_last10', 'any_last10')

for(var in vars){
  region_sum = frag_breakdown %>% 
    filter(region != 'NA', !is.na(region),
           usaidcov == 1) %>% 
    group_by(region) %>%
    summarise_(.dots = list(n = 'n()', 
                            avg = paste0('mean(', var, ')'))) %>% 
    arrange((n), (region)) 
  
  region_sum = region_sum %>% 
    mutate(region_name = case_when(region_sum$region == 'AFR' ~ 'Africa',
                                   region_sum$region == 'ASIA' ~ 'Asia',
                                   region_sum$region == 'ME' ~ 'Middle East',
                                   region_sum$region == 'LAC' ~ 'Latin Amer./Caribbean',
                                   region_sum$region == 'E&E' ~ 'Europe/Eurasia',
                                   TRUE ~ NA_character_), 
           outline_colour = case_when(region_sum$region == 'AFR' ~ '#ad6c68',
                                      region_sum$region == 'ASIA' ~ '#688197',
                                      region_sum$region == 'E&E' ~ '#7d9973',
                                      region_sum$region == 'LAC' ~ '#b0ad7e',
                                      region_sum$region == 'ME' ~ '#b19062',
                                      TRUE ~ grey60K))
  
  any10 = frag_breakdown %>% 
    filter(region != 'NA', !is.na(region), 
           usaidcov == 1)
  
  any10 = any10 %>% 
    mutate(outline_colour = case_when(any10$region == 'AFR' ~ '#ad6c68',
                                      any10$region == 'ASIA' ~ '#688197',
                                      any10$region == 'E&E' ~ '#7d9973',
                                      any10$region == 'LAC' ~ '#b0ad7e',
                                      any10$region == 'ME' ~ '#b19062',
                                      TRUE ~ grey60K))
  
  any10$region = factor(any10$region, 
                        levels = region_sum$region,
                        labels = region_sum$region_name)
  
  region_sum$region = factor(region_sum$region, 
                             levels = region_sum$region,
                             labels = region_sum$region_name)
  
  
  
  
  # horiz version ----------------------------------------------------------
  
  ggplot(any10, aes(x = region)) +
    coord_flip() +
    # facet_wrap(~region, scales = 'free_y', ncol = 1) +
    
    # -- stacked bar --
    geom_bar(aes_string(fill = 'fill_color', 
                        alpha = paste0('factor(', var, ')')), 
             position = 'stack', stat = 'count') + 
    # -- bar outline --
    geom_bar(aes_string(fill = 'fill_color', 
                        colour = 'outline_colour',
                        alpha = paste0('factor(', var, ')')), 
             # alpha = 1,
             size = 0.25,
             fill = NA, 
             position = 'stack', stat = 'count') + 
    
    # -- % labels --
    geom_text(aes(label = percent(avg, 0),
                  x = region,
                  y = avg,
                  colour = outline_colour),
              data = region_sum, family = 'Lato',
              hjust = 1,
              size = 4) +
    
    # -- scales --
    scale_fill_identity() +
    scale_alpha_discrete(range = c(0.2, 1)) +
    scale_colour_identity() +
    scale_y_reverse() +
    
    # -- themes --
    theme_ylab()
  
  save_plot(paste0('~/Documents/USAID/mini projects/Fragile States - (Aaron Roesch)/pctbyregion_', 
                   var,
                   '.pdf'), width = 3.5, height = 1.5)
  
}

# eep! stacked bar --------------------------------------------------------

ggplot(any10, aes(x = region)) +
  
  # -- stacked bar --
  geom_bar(aes(fill = fill_color, alpha = factor(anybroad_last10)), 
           position = 'stack', stat = 'count') + 
  # -- bar outline --
  geom_bar(aes(colour = outline_colour, alpha = factor(anybroad_last10)), 
           # alpha = 1,
           size = 0.25,
           fill = NA, 
           position = 'stack', stat = 'count') + 
  
  # -- % labels --
  geom_text(aes(label = percent(avg, 0), 
                x = region_name,
                y = 5,
                colour = outline_colour),
            data = region_sum, family = 'Lato',
            size = 4) +
  
  # -- scales --
  scale_fill_identity() +
  scale_alpha_discrete(range = c(0.2, 1)) +
  scale_colour_identity() +
  
  # -- themes --
  theme_ygrid() +
  theme(axis.title.y = element_blank())


# mini version ------------------------------------------------------------

df = frag_breakdown %>% 
  filter(usaidcov == 1) %>% 
  summarise(any = mean(any), any10 = mean(any_last10), anyminor = mean(anybroad_last10), n = n())

df %>% mutate_all(funs((. * 4)))
