
# Tidytuesday project for 2020 week 45: Historical Phone Usage
# https://github.com/rfordatascience/tidytuesday/blob/master/data/2020/2020-11-10/readme.md

# Data comes from Our World in Data

# librarires
library(tidyverse)
library(gganimate)
library(Cairo)
library(extrafont)

# load data
tuesdata <- tidytuesdayR::tt_load(2020, week = 46)

mobile <- tuesdata$mobile
landline <- tuesdata$landline

# Prevention of aliaed visualization in windows
CairoWin()

# WRANGLING ---------------------------------------------------------------

# join of mobile and landline data.frames
lines <- mobile %>% 
  full_join(
    landline %>% 
      select(code, year, landline_subs),
    by = c("code", "year")
  )


# computation of relative mobile lines by continent. We have to weight by population since
# our measure is a ratio
lines_continent <- lines %>% 
  group_by(continent, year) %>% 
  summarise(rel_mobile_subs = sum(mobile_subs * total_pop, na.rm = T) / sum(landline_subs * total_pop, na.rm = T),
            gdp_per_cap = sum(gdp_per_cap * total_pop, na.rm = T) / sum(total_pop, na.rm = T)) %>% 
  mutate(year = as.integer(year))


# population only until 2013. We have to filter out the following years
lines_continent_filtered <-  lines_continent %>% 
  filter(year < 2014, !is.na(continent))

# VISUALIZATION -----------------------------------------------------------


palette1 <- c("#6d0903", "#ba7361", "#01356d", "#7685b4", "grey45")

theme_set(theme_bw())

l_plot <- lines_continent_filtered %>% 
 
  # base
  ggplot(aes(y = rel_mobile_subs, color = continent)) + 
  
  # layers
  # the offset in x allows to generate the enlonging lines at the beginning 
  # and also allows for the labels to fit inside the plot area
  geom_path(aes(x = year - 2, group = continent), size = 1, show.legend = FALSE) + 
  geom_point(aes(x = year - 2), size = 4, show.legend = FALSE) +
  geom_text(aes(x = year - 1, label = continent), family = "Candara", show.legend = FALSE, size = 5) +
  
  
  # labs
  labs(
    y = "Mobile lines relative to landline",
    title = "Number mobile lines for each landline. 1990 - 2013",
    subtitle = 'Year: {frame_along}', 
    caption = "Data comes from Our World in Data. Visualization by Martín Pons | @MartinPonsM") + 
  
  # scales
  scale_color_manual(values = palette1) +
  
  #theme
  theme(text = element_text(color = "#4d433d", size = 17, family = "Candara"),
        plot.title = element_text(hjust = 0.5, color = "#443c36"),
        plot.subtitle = element_text(vjust = -12, hjust = 0.5, size = 22), 
        plot.caption = element_text(),
        panel.grid.minor = element_blank(),
        legend.position = "top", 
        axis.title.x = element_blank(), 
        axis.ticks.x = element_blank(), 
        axis.text.x = element_blank(),
        axis.text.y = element_text(size = 15),
        panel.border = element_blank(),
        axis.line = element_line(color = "#4d433d"),
        axis.line.x = element_blank()
        ) + 
  
  # animation features
  transition_reveal(along = year) + 
  view_follow() 

# render animation
animate(l_plot, nframes = 150, height = 500, width = 800, type = 'cairo')
 
# save animation  
anim_save(here::here("path.gif"))


