#Tidytuesday project for 2020-07-14
# Astronaut database

# Data Set sorce (Mariya Stavnichuk, Mariya Stavnichuk)
# https://data.mendeley.com/datasets/86tsnnbv2w/1

# Data preparation by Georgios Karamanis
# https://twitter.com/geokaramanis

# INITIAL SETTING ---------------------------------------------------------

# libraries
library(tidytuesdayR)
library(tidyverse)
library(extrafont)



# data load
tuesdata <- tidytuesdayR::tt_load('2020-07-14')

# data frame astronauts
astronauts <- tuesdata$astronauts


# WRANGLING ---------------------------------------------------------------

## data frame for star placement in the background ##

set.seed(140) # random state for star placement

n_stars = 75 # number of stars 
  
# stars data.frame
stars <- data.frame(
  
  # random placement
  x = runif(n_stars, -20, 50), 
  y = runif(n_stars, -2020, -1960), 
  
  # setting different degrees of luminosity for each star
  alpha = sample(c(0.3, 0.5, 0.8), n_stars, replace = TRUE))


## wrangling astronauts data frame ##

astronauts <- astronauts %>% 
    
    # filtering nationalities
    filter(nationality %in% c("U.S.", "U.S.S.R/Russia")) %>%
    
    # count astronauts names (only to get unique names)
    count(year_of_mission, nationality, military_civilian, name) %>%  
    
    # cumulative count of astronauts by year 
    group_by(year_of_mission, nationality) %>% 
    mutate(index = row_number() + 6, # "+ 6" is in order to leave a center space for the timeline
    military_civilian = str_to_sentence(military_civilian)) %>% # capitalize nationality
    ungroup() 
    


# VISUALIZATION -----------------------------------------------------------

# Note: you'll have to uncomment the two lines in the next paragraph if you use extrafont library 
# for the first time, in order for the code to detect specific fonts employed in the 
# nationality titles. please note that fonts are loaded from Windows operating system

# font_import()
# loadfonts(device = "win")

astronauts %>% 
  
  # axis coordinates, dot orientation an dot color
  ggplot(aes(x = ifelse(nationality == "U.S.", index, -index), 
             y = -year_of_mission, color = military_civilian)) + 
  
  # stars placement in the background
  geom_point(data = stars, 
             aes(x = x, y = y, alpha = alpha), 
             size = 0.5, 
             color = "white", 
             show.legend = F) +
  
  # dots for astronauts
  geom_point(size = 3) + 
  
  # timeline: years, text annotations and vertical lines to enclose it
  geom_text(data = data.frame(year_of_mission = seq(1960, 2020, by = 5)), 
            aes(label = year_of_mission, x = 0), 
            color = "lightblue", 
            size = 3) + 
  
  geom_segment(data = data.frame(x = c(-5.5, 5.5), 
                                 xend = c(-5.5, 5.5), 
                                 y = rep(-2020, 2),
                                 yend = rep(-1960, 2)),
               aes(x = x, y = y, xend = xend, yend = yend),
               color = "lightblue") +

  annotate(geom = "text", x = 0, y =-1962, 
           label = "Yuri Gagarin, first human\n in outer space", color = "#e6c15c",
           size = 2.5) +
  
  annotate(geom = "text", x = 0, y =-1968.5, 
           label = "First man on The Moon", color = "#e6c15c",
           size = 2.5) +
  
  annotate(geom = "text", x = 0, y =-1982, 
           label = "Space shuttle\n program beggins", color = "#e6c15c",
           size = 2.5) +
  
  annotate(geom = "text", x = 0, y =-1986.5, 
           label = "MIR in orbit", color = "#e6c15c",
           size = 2.5) +
  
  annotate(geom = "text", x = 0, y =-2012, 
           label = "Space shuttle\n program ends", color = "#e6c15c",
           size = 2.5) +
  
  annotate(geom = "text", x = 0, y =-1998, 
           label = "ISS in orbit", color = "#e6c15c",
           size = 2.5) +
  
  # titles with nationalities
  annotate(geom = "text", x = -12, y = -1959, label = "U.S.S.R / Russia", color = "red", 
           family = "Verdana Pro") +
  
  annotate(geom = "text", x = 9, y = -1959, label = "U.S.A", color = "#4287f5", 
           family = "Verdana Pro") + 
  
  # dot colors
  scale_color_manual(values = c("#7792bf", "#e6622e")) +
  
  # limits for time scale: we want extra space for annotations
  scale_y_continuous(limits = c(-2021, -1957)) + 
  
  # title and caption
  labs(title = "Númber of  U.S.A and U.S.S.R / Russian astronauts in space missions", 
       caption = "Data comes from Mariya Stavnichuk and Tatsuya Corlett, cleaned by Georgios Karamanis. Visualization by Martín Pons | @MartinPonsM") + 
  
  # plot theme  
  theme(panel.background = element_rect(fill = "black"),
        panel.grid = element_blank(), 
        axis.title = element_blank(), 
        axis.text = element_blank(), 
        axis.ticks = element_blank(), 
        legend.position = c(0.9, 0.9), 
        legend.title = element_blank(),
        legend.background = element_blank(),
        legend.text = element_text(color = "lightblue", size = 10),
        legend.key = element_blank(),
        plot.title = element_text(hjust = 0.5, vjust = -7, color = "lightblue"),
        plot.caption = element_text(color = "lightblue", vjust = 17, hjust = 0.5)) 
    


    
  