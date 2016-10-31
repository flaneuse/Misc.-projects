library(tidyr)
library(forcats)
library(RColorBrewer)
grey5K = '#F1F2F2'

# relevel -----------------------------------------------------------------


# Create an ordering variable, based on: 
df_tidy = df %>%
  mutate(order = 
           ifelse(no_lists == 3, 1, 
                  ifelse(frag_usaid == 'Fragile' & frag_fsi == 'Fragile', 2,
                         ifelse(frag_usaid == 'Fragile' & frag_wb == 'Fragile', 3,
                                ifelse(frag_fsi == 'Fragile' & frag_wb == 'Fragile', 4,
                                       ifelse(frag_usaid == 'Fragile', 5,
                                              ifelse(frag_fsi == 'Fragile', 6,
                                                     ifelse(frag_wb == 'Fragile', 7, 8))))))))
#   mutate(order = case_when(df$no_lists == 3 ~ 1,
#                            df$frag_usaid == 'Fragile' & df$frag_fsi == 'Fragile' ~ 2,
#                            df$frag_usaid == 'Fragile' & df$frag_wb == 'Fragile' ~ 3,
#                            df$frag_usaid == 'Fragile'~ 4,
#                            df$frag_fsi == 'Fragile' ~ 5,
#                            df$frag_wb == 'Fragile' ~ 6,
#                            TRUE ~ 0))
df_tidy = df_tidy %>% 
  arrange(desc(order), budget, coverage)

df_tidy$country = factor(df_tidy$country,
                         levels = df_tidy$country)


# tidy-ize ----------------------------------------------------------------

df_tidy =  df_tidy %>% 
  mutate(coverage2 = coverage) %>% 
  select(country, region, contains('frag'), fill_color, no_lists, order, coverage, budget) %>% 
  gather(list_name, is_fragile, -country, -fill_color, -no_lists, -region, -order, -budget)


# create budget label -----------------------------------------------------

df_tidy = df_tidy %>% 
  mutate(budget_label = ifelse(budget > 99e6, paste0(round(budget / 1e9, digits = 1), 'B'),
                  paste0(round(budget / 1e6, digits = 0), 'M')))

# relevel fragile variables -----------------------------------------------
df_tidy$list_name = factor(df_tidy$list_name, 
                           levels = c('frag_usaid', 'frag_fsi', 'frag_wb', 'coverage'),
                           labels = c('USAID', 'FSI', 'World Bank', 'USAID presence'))

# plot ---------------------------------------------------------------------

dot_matrix = function(df,
                      x_var, 
                      y_var,
                      fill_var,
                      # alpha_var,
                      dot_size = 4,
                      stroke_size = 0.1,
                      stroke_colour = grey90K,
                      no_colour = grey5K,
                      yes_colour = grey50K, #brewer.pal(11, 'Spectral')[2],
                      usaid_colour = '#4575b4'
) {
  ggplot(df, aes_string(x = x_var, 
                        y = y_var,
                        fill = fill_var)) +
    geom_point(size = dot_size, shape = 21,
               stroke = stroke_size,
               colour = stroke_colour) +
    scale_x_discrete(position = 'top') +
    scale_fill_manual(values = c('0' = no_colour, 'Fragile' = yes_colour, 'Coverage' = usaid_colour)) +
    # scale_alpha_manual(values = c('0' = 0.2, 'Coverage' = 0.75)) +
    theme_xylab()
}




dot_matrix(df_tidy, 'list_name', 'country', 'is_fragile') 
#+ facet_wrap(~no_lists, scales = 'free_y')


save_plot('~/Documents/USAID/mini projects/Fragile States - (Aaron Roesch)/fragile_overlap.pdf',
          width = 4, height = 28)

# plot: color regions ---------------------------------------------------------------------

dot_matrix3 = function(df,
                      x_var, 
                      y_var,
                      fill_var,
                      # alpha_var,
                      dot_size = 4,
                      stroke_size = 0.1,
                      stroke_colour = grey90K,
                      no_colour = grey5K,
                      yes_colour = grey50K, #brewer.pal(11, 'Spectral')[2],
                      usaid_colour = '#4575b4'
) {
  ggplot(df, aes_string(x = x_var, 
                        y = y_var,
                        fill = fill_var)) +
    geom_point(size = dot_size, shape = 21,
               stroke = stroke_size,
               colour = stroke_colour) +
    scale_x_discrete(position = 'top') +
    scale_fill_identity() +
    # scale_fill_manual(values = c('0' = no_colour, 'Fragile' = yes_colour, 'Coverage' = usaid_colour)) +
    # scale_alpha_manual(values = c('0' = 0.2, 'Coverage' = 0.75)) +
    theme_xylab()
}




dot_matrix3(df_tidy, 'list_name', 'country', fill_var = 'fill_color') 
#+ facet_wrap(~no_lists, scales = 'free_y')


save_plot('~/Documents/USAID/mini projects/Fragile States - (Aaron Roesch)/fragile_overlap_region.pdf',
          width = 4, height = 28)

# plot budgets ------------------------------------------------------------


dot_matrix2 = function(df,
                       x_var, 
                       y_var,
                       fill_var,
                       # alpha_var,
                       dot_size = 4,
                       stroke_size = 0.1,
                       stroke_colour = grey90K,
                       no_colour = grey5K,
                       yes_colour = brewer.pal(11, 'Spectral')[2],
                       usaid_colour = '#4575b4'
) {
  ggplot(df_tidy, aes_string(x = 'list_name', 
                             y = 'country',
                             # fill = 'budget')) +
                             fill = 'log10(budget)')) +
    geom_point(size = dot_size, shape = 21,
               stroke = stroke_size,
               colour = stroke_colour) +
    # geom_text(aes(label = round(budget / 1e6)),
    geom_text(aes(colour = budget, label = budget_label),
              # colour = grey60K,
              size = 2,  hjust = 0.5) +
    scale_x_discrete(position = 'top') +
    scale_fill_gradientn(colours = brewer.pal(9, 'Greys'), 
                         na.value = grey15K) +
    # limits = c(0, log10(max(df$budget)))) +
    scale_color_text(df_tidy$budget, mapping_range = c(0, 0.3)) +
    # scale_fill_manual(values = c('0' = no_colour, 'Fragile' = yes_colour, 'Coverage' = usaid_colour)) +
    # scale_alpha_manual(values = c('0' = 0.2, 'Coverage' = 0.75)) +
    theme_xylab()
}


dot_matrix2(df_tidy, 'list_name', 'country', 'is_fragile') 
#+    scale_color_text(df_tidy$budget, mapping_range = c(0, 0.4))+ facet_wrap(~no_lists, scale = 'free_y')

save_plot('~/Documents/USAID/mini projects/Fragile States - (Aaron Roesch)/fragile_overlap_budgets.pdf',
          width = 4, height = 28)
