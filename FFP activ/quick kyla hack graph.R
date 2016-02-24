library(ggplot2)
library(dplyr)
library(knitr)
library(stringr)
activ = read.csv('~/Documents/USAID/Kyla - FFP reshaping/Development_activities_2016-02-19.csv') %>%
filter(pgrmType == 'Development',
isActive == 1,
ARRyear == 2015)
allActiv = read.csv('~/Documents/USAID/Kyla - FFP reshaping/allFFP_activities_2016-02-19.csv') %>%
filter(pgrmType == 'Development',
isActive == 1,
ARRyear == 2015)
View(allActiv)
activ  %>% filter(str_detect(category1, 'WASH'), ARRyear == 2015)  %>% group_by(Awardee, subcategory) %>% summarise(x=n())
x = activ  %>% filter(str_detect(category1, 'WASH'), ARRyear == 2015)  %>% group_by(Awardee, subcategory) %>% summarise(x=n())
ggplot(x, aes(x = subcategory, y = x, colour = subcategory)) + geom_bar(stat = 'identity') + facet_wrap(Awardee)
View(x)
ggplot(x, aes(x = subcategory, y = x, colour = subcategory)) + geom_bar(stat = 'identity') + facet_wrap(~Awardee)
ggplot(x, aes(x = subcategory, y = x, fill = subcategory)) + geom_bar(stat = 'identity') + facet_wrap(~Awardee)
library(llamar)
ggplot(x, aes(x = subcategory, y = x, fill = subcategory)) + geom_bar(stat = 'identity') + facet_wrap(~Awardee)+ coord_flip()
ggplot(x, aes(x = subcategory, y = x, fill = subcategory)) + geom_bar(stat = 'identity') + facet_wrap(~Awardee)+ coord_flip()+theme_xgrid()
ggplot(x, aes(x = subcategory, y = x, fill = subcategory)) + geom_bar(stat = 'identity') + facet_wrap(~Awardee)+ coord_flip()+theme_xgrid()+theme(strip.text= element_text(size = 12))
ggplot(x, aes(x = subcategory, y = x, fill = subcategory)) + geom_bar(stat = 'identity') + facet_wrap(~Awardee)+ coord_flip()+theme_xgrid()+theme(strip.text= element_text(size = 10))
ggplot(x, aes(x = Awardee, y = x, fill = subcategory)) + geom_bar(stat = 'identity') + facet_wrap(~subcategory)+ coord_flip()+theme_xgrid()+theme(strip.text= element_text(size = 10))
x$subcategory = factor(x$subcategory, c('Water Supply', 'Sanitation', 'WASH Governance & Capacity', 'Hygiene'))
ggplot(x, aes(x = Awardee, y = x, fill = subcategory)) + geom_bar(stat = 'identity') + facet_wrap(~subcategory)+ coord_flip()+theme_xgrid()+theme(strip.text= element_text(size = 10))
x$Awardee = factor(x$Awardee, c('Catholic Relief Services', 'ACDI/VOCA (PCI, JSI & MCI)', 'Save the Children', 'Food for the Hungry', 'Mercy Corps', 'CNFA', 'Adventist Relief and Development Agency', 'OICI', 'World Vision', 'Project Concern International', 'CARE'))
ggplot(x, aes(x = Awardee, y = x, fill = subcategory)) + geom_bar(stat = 'identity') + facet_wrap(~subcategory)+ coord_flip()+theme_xgrid()+theme(strip.text= element_text(size = 10))
x$Awardee = factor(x$Awardee, rev(c('Catholic Relief Services', 'ACDI/VOCA (PCI, JSI & MCI)', 'Save the Children', 'Food for the Hungry', 'Mercy Corps', 'CNFA', 'Adventist Relief and Development Agency', 'OICI', 'World Vision', 'Project Concern International', 'CARE'))
)
ggplot(x, aes(x = Awardee, y = x, fill = subcategory)) + geom_bar(stat = 'identity') + facet_wrap(~subcategory)+ coord_flip()+theme_xgrid()+theme(strip.text= element_text(size = 10))
ggplot(x, aes(x = Awardee, y = x, fill = subcategory)) + geom_bar(stat = 'identity') + facet_wrap(~subcategory, ncol = 4)+ coord_flip()+theme_xgrid()+theme(strip.text= element_text(size = 10))
ggplot(x, aes(x = Awardee, y = x, fill = subcategory)) + geom_bar(stat = 'identity') + facet_wrap(~subcategory, ncol = 4)+ coord_flip()+theme_xgrid()+theme(strip.text= element_text(size = 10))


ggsave('subcat.pdf')


y = x %>% group_by(Awardee) %>% summarise(summed = sum(x))

ggplot(y, aes(x = 1, y = Awardee, colour = summed, label = summed)) +
  geom_point(size = 8) +
  geom_text(colour = grey80K) +
  geom_text(colour = grey15K, data = y %>% filter(str_detect(Awardee, "Cath"))) +
  scale_colour_gradient(low = grey20K, high = grey70K) +
  theme_xygrid() 
