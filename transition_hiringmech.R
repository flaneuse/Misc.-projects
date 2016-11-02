library(readxl)
library(waffle)
library(dplyr)
library(tidyr)
library(forcats)
library(extrafont)
library(stringr)
library(llamar)

loadfonts()
df = read_excel('~/Documents/USAID/mini projects/Tranistion team HCTM - (Nick Vivio)/HCTM HR Flat File.xlsx', 
                 # skip = 3, sheet = 1)
                sheet = 2)

df = df %>% 
  select(-`WORKFORCE BACKSTOP TITLE`, -`Congress Report Name`) %>% 
  gather(hiring_mech, n, -Bureau, -Region, -Country) %>%
  group_by(hiring_mech) %>% 
  summarise(n = sum(n, na.rm = TRUE)) %>% 
  arrange(desc(n))

# Legacy: from Excel pivot table from Nick that excl. CS and AD.
# df = df %>% 
#   filter(`Row Labels` == 'Grand Total') %>% 
#   select(-`0`) %>% 
#   gather(hiring_mech, n, -`Row Labels`) %>% 
#   mutate(mech = str_replace_all(hiring_mech, 'Sum of ', '')) %>% 
#   arrange(desc(n))

ggplot(df, aes(x = fct_reorder(hiring_mech, n), y = n)) +
  geom_point()

df$hiring_mech = factor(df$hiring_mech,
                        levels = df$hiring_mech)

waffle(df$n/10,  use_glyph = 'user', glyph_size = 7, rows = 25) + 
  theme(legend.position = 'none') +
  # scale_color_brewer('Paired') +
  scale_color_manual(values = c(brewer.pal(8, 'Set1'), '#a6761d', '#999999')) +
  geom_text(aes(x = 10, y = 1, label = paste0(hiring_mech, ': ', n)), 
            data = df, size = 7, family = 'Lato Light')


ggsave('~/Documents/USAID/mini projects/Tranistion team HCTM - (Nick Vivio)/hctm_isotype.svg', 
       width = 12, height = 20,
       units = 'in')
