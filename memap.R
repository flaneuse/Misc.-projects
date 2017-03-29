library(ggmap)
library(dplyr)
library(llamar)

df = NULL
df = bind_rows(df, geocode('Prairie Village, KS'))
df = bind_rows(df, geocode('Northwestern University Evanston, IL'))
df = bind_rows(df, geocode('Cambridge University Cambridge United Kingdom'))
df = bind_rows(df, geocode('Stanford University Stanford CA'))
df = bind_rows(df, geocode('Washington DC'))

df = df %>% mutate(id = 1:nrow(df)) %>% 
  bind_cols(data.frame(loc = c('Kansas', 'Northwestern', 'Cambridge', 'Stanford', 'D.C.')))

map_loc = get_map(location = 'St. Louis, MO', 
                  zoom = 2,
                  source='stamen', maptype = 'watercolor')

p = ggmap(map_loc, extent = 'device', maprange = TRUE) +
  geom_path(aes(x = lon, y = lat), 
            data = df, size = 0.1, colour = grey90K) +
  
  geom_point(aes(x = lon, y = lat, fill = id), 
             data = df, size = 3.5, 
             shape = 21,
             stroke = 0.1,
             colour = grey90K) +
  scale_fill_gradientn(colours = c(grey10K, grey60K)) +

  # ylim(c(22,60)) +
  xlim(c(-130, 10)) +
  geom_text(aes(x = lon, y = lat,
                label = loc),
            colour = grey75K,
            family = 'Lato Light',
            size = 3,
            hjust = 0.5,
            data = df) +
  theme_blank()

save_plot(p, filename = '~/Desktop/memap.pdf', width = 12)
