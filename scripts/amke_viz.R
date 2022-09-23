#------------------------------------------------#
####               Setting up                 ####
#------------------------------------------------#

# Packages
library(tidyverse)
library(lubridate)
library(leaflet)
library(scales)
library(htmlwidgets)
library(webshot2)



#------------------------------------------------#
####        Get data and manipulate           ####
#------------------------------------------------#

# Read in data and clean
amke <- tibble(read.delim("./data/ebd/ebd_US-ME-009_amekes_relJul-2022.txt", header = T, quote = "")) %>% 
  select(c('OBSERVATION.DATE', 'COMMON.NAME', 'SCIENTIFIC.NAME', 'OBSERVATION.COUNT', 
           'LOCALITY', 'LATITUDE', 'LONGITUDE')) %>% 
  rename('obs.date'='OBSERVATION.DATE', 'common.name'='COMMON.NAME', 
         'scientific.name'='SCIENTIFIC.NAME', 'count'='OBSERVATION.COUNT', 'locality'='LOCALITY', 
         'latitude'='LATITUDE', 'longitude'='LONGITUDE') %>% 
  filter(year(obs.date) %in% 2002:2021) %>% 
  mutate(latitude = jitter(latitude, factor = 15),
         longitude = jitter(longitude, factor = 15))

  

ssha <- tibble(read.delim("./data/ebd/ebd_US-ME-009_shshaw_relJul-2022.txt", header = T, quote = "")) %>% 
  select(c('OBSERVATION.DATE', 'COMMON.NAME', 'SCIENTIFIC.NAME', 'OBSERVATION.COUNT', 
           'LOCALITY', 'LATITUDE', 'LONGITUDE')) %>% 
  rename('obs.date'='OBSERVATION.DATE', 'common.name'='COMMON.NAME', 
         'scientific.name'='SCIENTIFIC.NAME', 'count'='OBSERVATION.COUNT', 'locality'='LOCALITY', 
         'latitude'='LATITUDE', 'longitude'='LONGITUDE') %>% 
  filter(year(obs.date) %in% 2002:2021) %>% 
  mutate(latitude = jitter(latitude, factor = 15),
         longitude = jitter(longitude, factor = 15))


  # mutate(obs.date = as.Date(obs.date)) %>% 
  # filter(year(obs.date) %in% 2007:2021) %>% 
  # group_by(year(obs.date)) %>% 
  # tally()


maxLong = max(amke$longitude)
maxLat = max(amke$latitude)
minLong = min(amke$longitude)
minLat = min(amke$latitude)

septmap <- leaflet(options = leafletOptions(zoomControl = FALSE)) %>% 
  addProviderTiles(providers$Esri.WorldImagery) %>% 
  addProviderTiles(providers$Stamen.TerrainLabels) %>% 
  addCircleMarkers(amke$longitude, amke$latitude, color = "#E1723A", radius = 2, opacity = 1) %>% 
  addCircleMarkers(ssha$longitude, ssha$latitude, color = "#4E74AE", radius = 2, opacity = 1) %>% 
  fitBounds(minLong, 44.28, maxLong, 44.5)
             
saveWidget(septmap, "temp.html", selfcontained = TRUE)
webshot2::webshot("temp.html", file = "outputs/september_raptor_map.png", vwidth = 1100, vheight = 775)

