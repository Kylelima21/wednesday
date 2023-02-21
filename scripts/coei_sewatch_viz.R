#------------------------------------------------#
####               Setting up                 ####
#------------------------------------------------#

# Packages
library(tidyverse)
library(lubridate)
library(ggplot2)
library(ggimage)
library(jpeg)



#------------------------------------------------#
####        Get data and manipulate           ####
#------------------------------------------------#

img <- readJPEG("data/test.jpeg")

read.csv("data/coei_totals_2016_to_2022.csv") %>% 
  mutate(cph = coei_count/hours) %>% 
  ggplot(aes(x = year, y = cph)) +
  geom_line(size = 0.9, color = "white") +
  labs(title = "Common Eider", subtitle = "Schoodic Point Sea Watch data (2016 - 2022)",
       x = "Year", y = "Average count/hour") +
  theme_classic() +
  theme(plot.title.position = "plot",
        plot.title = element_text(face = "bold", size = 18, color = "white"),
        plot.subtitle = element_text(color = "gray90", size = 13),
        axis.text = element_text(color = "white", size = 13),
        axis.title = element_text(color = "white", size = 13),
        axis.title.x = element_text(margin = margin(0.6, 0, 0, 0, "cm")),
        axis.title.y = element_text(margin = margin(0, 0.5, 0, 0, "cm")),
        axis.line = element_line(color = "white"),
        axis.ticks = element_line(color = "white"),
        plot.background = element_blank(),
        panel.background = element_blank(), 
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(), 
        #panel.border = element_rect(color = 'black', fill = NA, size = 1),
        legend.position = "none")

ggsave("outputs/eider_seawatch_trends.png", width = 5.28, height = 3.75)

