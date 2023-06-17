#------------------------------------------------#
####               Setting up                 ####
#------------------------------------------------#

# Packages
library(tidyverse)
library(lubridate)
library(ggplot2)
library(sf)
library(viridis)
library(scales)


filter_acad <- function(dat, lat, long) {
  
  sf::sf_use_s2(FALSE)
  
  
  acad.bounds <- sf::read_sf("data/acad_boundary/ACAD_ParkBoundary_PY_202004.shp") %>% 
    st_transform(4326)
  
  
  dat2 <- dat %>% 
    rename(x = paste(long), y = paste(lat)) %>% 
    mutate(longitude.keep = x,
           latitude.keep = y) %>% 
    sf::st_as_sf(., coords = c("x","y"), crs = sf::st_crs(acad.bounds))
  
  
  dat2 %>% 
    mutate(intersect = as.integer(st_intersects(geometry, acad.bounds))) %>% 
    filter(!is.na(intersect))
  
  
  output <- sf::st_join(dat2, acad.bounds, left = F) %>% 
    st_set_geometry(., NULL) %>% 
    select(1:latitude.keep) %>% 
    rename(latitude = latitude.keep, longitude = longitude.keep)
  
  
  return(output)
  
}

#------------------------------------------------#
####        Get data and manipulate           ####
#------------------------------------------------#

dat <- tibble(read.delim("./data/ebd_US-ME_amtspa_relFeb-2023.txt", header = T, quote = "")) %>% 
  select(c('OBSERVATION.DATE', 'COMMON.NAME', 'SCIENTIFIC.NAME', 'OBSERVATION.COUNT', 
           'LOCALITY', 'LATITUDE', 'LONGITUDE')) %>% 
  rename('obs.date'='OBSERVATION.DATE', 'common.name'='COMMON.NAME', 
         'scientific.name'='SCIENTIFIC.NAME', 'count'='OBSERVATION.COUNT', 'locality'='LOCALITY', 
         'latitude'='LATITUDE', 'longitude'='LONGITUDE') %>% 
  mutate(count = str_replace(count, "X", "1"),
         count = as.numeric(count),
         obs.date = ymd(obs.date),
         month = month(obs.date),
         year = year(obs.date)) %>% 
  filter_acad(., lat = "latitude", long = "longitude")




mon <- tibble(month = rep(1:12, 4)) %>% 
  arrange(month) %>% 
  mutate(timestep = rep(c(0, 25, 50, 75), 12),
         timestep = paste(month, timestep, sep = "."))
         

abundance <- tibble(abd = c(0.0480979, 0.0468541, 0.0700361, 0.0619048, 0.0588697, 0.0663082, 0.0872622, 0.0428157, 
               0.0443131, 0.0418919, 0.0336012, 0.0297137, 0.0342972, 0.0250875, 0.0077427, 0.0063752,	
               4.666E-4, 0.0,	0.0, 0.0,	0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0,
               0.0, 0.0, 0.0, 0.0, 5.333E-4, 0.0014184, 0.0096372, 0.0164251, 0.0197711, 0.0360767, 
               0.0232558, 0.0167264, 0.031941, 0.0331633, 0.036031))


bc_dat <- cbind(mon, abundance) %>% 
  mutate(timestep = as.numeric(timestep),
         calc = abd/2,
         above = calc, 
         below = -calc) %>% 
  select(-c(abd, calc)) %>% 
  pivot_longer(cols = c(above, below)) %>% 
  rename(abd = value) %>% 
  mutate(abd = ifelse(abd < 0.001 & abd > 0, 0.1, abd),
         abd = ifelse(abd > -0.001 & abd < 0, -0.1, abd),
         abd = ifelse(abd < 0.005 & abd > 0.001, 0.5, abd),
         abd = ifelse(abd > -0.005 & abd < -0.001, -0.5, abd),
         abd = ifelse(abd < 0.025 & abd > 0.005, 1, abd),
         abd = ifelse(abd > -0.025 & abd < -0.005, -1, abd),
         abd = ifelse(abd < 0.05 & abd > 0.025, 1.5, abd),
         abd = ifelse(abd > -0.05 & abd < -0.025, -1.5, abd))
  

background <- data.frame(xstart = seq(1,12,1), xend = seq(1.95,12.95,1), 
                         col = rep(c("white", "gray90"), 6))  


ggplot() + 
  geom_rect(data = background, aes(ymin = -2, ymax = 2, xmin = xstart,
                                   xmax = xend, fill = col), alpha = 0.5) +
  geom_bar(data = bc_dat, aes(x = timestep, y = abd), stat = "identity", 
           position = position_nudge(x = 0.1), fill = "#0B9F00") +
  scale_x_continuous(breaks = 1:12,
                     labels = c("Jan", "Feb", "Mar", "Apr", "May", "Jun",
                                "Jul", "Aug", "Sep", "Oct", "Nov", "Dec"),
                     expand = c(0,0)) +
  scale_y_continuous(limits = c(-3, 3)) +
  theme(plot.title = element_text(color = "black", hjust = 0.5, vjust = 0.8, size = rel(2)),
        plot.background = element_rect(fill = "white", color = "white"),
        plot.margin = unit(c(14.5, 18.5, 5.5, 5.5), "points"),
        panel.background = element_rect(fill = "white"),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        axis.line = element_blank(),
        axis.ticks = element_blank(), 
        axis.text = element_text(color = "black", size = rel(1.1)),
        axis.text.x = element_text(hjust = -0.2),
        axis.text.y  = element_blank(),
        axis.title = element_blank(),
        legend.position = "none") + 
  scale_fill_manual(values = alpha(c("white", "gray90"), 0.2))

ggsave("outputs/atsp_barchart.png", dpi = 350, height = 1.1, width = 7, units = "in")
  
  
  
  
  
  
         