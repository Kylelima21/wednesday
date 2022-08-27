#------------------------------------------------#
####               Setting up                 ####
#------------------------------------------------#

# Packages
library(tidyverse)
library(lubridate)
library(leaflet)
library(gganimate)
library(scales)



#------------------------------------------------#
####        Get data and manipulate           ####
#------------------------------------------------#

# Read in data and clean
rawdata <- tibble(read.delim("./data/ebd/ebd_US-ME-009_lesyel_relJul-2022.txt", header = T, quote = "")) %>% 
  select(c('OBSERVATION.DATE', 'COMMON.NAME', 'SCIENTIFIC.NAME', 'OBSERVATION.COUNT', 
               'LOCALITY', 'LATITUDE', 'LONGITUDE')) %>% 
  rename('obs.date'='OBSERVATION.DATE', 'common.name'='COMMON.NAME', 
         'scientific.name'='SCIENTIFIC.NAME', 'count'='OBSERVATION.COUNT', 'locality'='LOCALITY', 
         'latitude'='LATITUDE', 'longitude'='LONGITUDE')

# Create leaflet map
map <- leaflet() %>% 
  addProviderTiles(providers$Esri.WorldImagery) %>% 
  addProviderTiles(providers$Stamen.TerrainLabels) %>% 
  addMarkers(rawdata$longitude, rawdata$latitude, clusterOptions = markerClusterOptions())

# Manipulate for ggplot
first_obs <- rawdata %>% 
  mutate(year = str_extract(obs.date, "^\\d\\d\\d\\d"),
         obs.date = ymd(obs.date)) %>% 
  arrange(obs.date) %>% 
  group_by(year)

# Get arrival dates and clean for ggplot
arrival_date <- first_obs[!duplicated(first_obs$year),] %>% 
  ungroup() %>%
  filter(year > 2001) %>% 
  mutate(ex.date = ymd(str_replace(obs.date, "^\\d*", "2000")),
         obs.date = ymd(str_replace(obs.date, "^\\d*", "2000"))) %>% 
  mutate(meanday = mean.Date(ex.date),
         dist = meanday - obs.date,
         dist = as.numeric(str_replace(dist, " days", "")),
         y = 1)

# Get date for ggplot
anno <- arrival_date$meanday[1]

# Creat base ggplot
p <- arrival_date %>% 
  ggplot(aes(x = obs.date, y = y, group = year, color = dist)) + 
  geom_point(shape = 19, size = 4.7) +
  geom_vline(xintercept = ymd(anno), linetype = "dashed", color = "gray57") +
  geom_text(aes(x = ymd(anno), y = 1), vjust = -0.8, hjust = -0.2, size = 3.2,
            label = "Mean arrival date", color = "gray57", angle = 90) +
  scale_y_continuous(breaks = seq(0.8, 1, 1.2)) +
  scale_color_gradient2(low = "#C17E44", mid = "wheat", high = "springgreen4",
                        midpoint = 0, guide = "none") +
  scale_x_date(breaks = date_breaks("months"),
               labels = date_format("%b"),
               limits = c(as.Date("2000-01-01"), as.Date("2000-12-01"))) +
  labs(x = NULL, y = NULL,
       title = "Lesser Yellowlegs arrival dates",
       subtitle = "(Hancock County, Maine, 2002 - 2022)") +
  theme(panel.background = element_blank(),
    panel.grid.minor = element_blank(),
    panel.grid.major.x = element_blank(),
    panel.grid.major.y = element_blank(),
    plot.title.position = "plot",
    plot.title = element_text(face = "bold"),
    plot.subtitle = element_text(color = "gray18", size = 10),
    plot.margin = margin(10, 15, 10, 10),
    axis.ticks.y = element_blank(),
    axis.text.y = element_blank(),
    axis.title.y = element_blank(),
    axis.text.x = element_text(color = "black", size = 10))

# Add changing label
a <- p + 
  geom_label(aes(x = ymd(anno), y = 1, label = year), fontface = "bold",  label.size = 0,
             vjust = 2) +
  transition_manual(year, cumulative = T)

# Create GIF and save
animate(a, width = 7, height = 3.45, unit = "in", res = 300)
anim_save("outputs/leye_dates.gif")




