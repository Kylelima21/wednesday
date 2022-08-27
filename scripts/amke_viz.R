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
rawdata <- tibble(read.delim("./data/ebd/ebd_US-ME-009_amekes_relJul-2022.txt", header = T, quote = "")) %>% 
  select(c('OBSERVATION.DATE', 'COMMON.NAME', 'SCIENTIFIC.NAME', 'OBSERVATION.COUNT', 
           'LOCALITY', 'LATITUDE', 'LONGITUDE')) %>% 
  rename('obs.date'='OBSERVATION.DATE', 'common.name'='COMMON.NAME', 
         'scientific.name'='SCIENTIFIC.NAME', 'count'='OBSERVATION.COUNT', 'locality'='LOCALITY', 
         'latitude'='LATITUDE', 'longitude'='LONGITUDE')


