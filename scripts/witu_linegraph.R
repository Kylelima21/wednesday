#------------------------------------------------#
####               Setting up                 ####
#------------------------------------------------#

# Packages
library(tidyverse)
library(lubridate)
library(ggplot2)
library(rphylopic)



#------------------------------------------------#
####          Read in data and plot           ####
#------------------------------------------------#

witu <- tibble(read.csv("data/witu_linegraph_data.csv")) %>% 
  mutate(week = as.Date(week, format = "%m/%d/%Y"))

get_uuid("wild turkey", n = 2)

witu %>% 
  ggplot(aes(x = week, y = freq)) +
  geom_line(linewidth = 0.6) +
  add_phylopic(uuid = "101ad9ab-dae8-442f-b102-8109b02b2a69",
               x = as.Date("01-Nov-0023", format = "%d-%b-%Y"),
               y = 8, ysize = 50,
               color = "#C67334") +
  labs(title = "Annual variation in wild turkey frequency", 
       subtitle = "Hancock County, Maine",
       caption = "Data provided by ebird.org",
       y = "Frequency",
       x = "Month") +
  scale_x_date(date_labels = "%b", 
               expand = c(0,0),
               date_breaks = "1 month") +
  theme_classic() +
  theme(axis.title = element_text(color = "black", size = 13),
        axis.text = element_text(color = "black", size = 12),
        plot.title = element_text(size = 18, face = "bold"),
        axis.title.y = element_text(margin = margin(0,.5,0,0, unit = "cm")),
        axis.title.x = element_text(margin = margin(.5,0,0,0, unit = "cm")),
        plot.margin = margin(0.5,0.5,0.5,0.5, unit = "cm"),
        plot.subtitle = element_text(margin = margin(0,0,0.5,0, unit = "cm")))

ggsave("outputs/witu_freq.png", height = 6, width = 8.5, dpi = 350)
