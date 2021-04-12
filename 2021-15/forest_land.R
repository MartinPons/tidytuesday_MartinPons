
## Tidytuesday: 2021 week 15: Global deforestation
# https://github.com/rfordatascience/tidytuesday/blob/master/data/2021/2021-04-06/readme.md

# Data comes from Our World in Data
# https://ourworldindata.org/forests-and-deforestation

# And the world bank for the total land
# https://data.worldbank.org/indicator/AG.LND.TOTL.K2


# INITIAL SETTING ---------------------------------------------------------

# libraries
library(tidyverse)
library(rnaturalearth)
library(extrafont)
library(tmap)
library(Cairo)



# LOAD DATA ---------------------------------------------------------------

tuesdata <- tidytuesdayR::tt_load(2021, week = 15)
forest_area <- forest_area <- tuesdata$forest_area

# world map from rnaturalearth
world_map <- ne_download(scale = "medium", category = "cultural")

# total land area
land_area <- read_csv(here::here("data", "countries_land_area.csv"), 
                      skip = 3) %>% 
  
  # selection of last year with data
  select(`Country Code`, `2018`) %>% 
  set_names("code", "land") 


# DATA WRANGLING ----------------------------------------------------------

# forest land data
forest_land <- forest_area %>% 
  
  filter(year == 2018, !is.na(code), code != "OWID_WRL") %>% # only countries, not other entities, in 2018
  
  inner_join(land_area, by = "code") %>% 
  
  mutate(p_land = land / sum(land) * 100, # rescale to make the data comparable
         forest_index = forest_area / p_land) # density index

# basic map
world_map <- world_map[world_map$SOVEREIGNT != "Antarctica", ] 
world_map[world_map$SOVEREIGNT == "Norway", "ISO_A3_EH"] <- "NOR" # Iso3 for Norway is missing for some reason

# merging map and forest data
countries_spdf_merge <- sp::merge(world_map,
                                  forest_land, by.x = "ISO_A3_EH", by.y = "code")


# PLOT --------------------------------------------------------------------

# basic choropleth map
tm_shape(countries_spdf_merge) + 
  tm_borders() + 
  tm_fill(col = "forest_index", midpoint = 1, style = "cont", palette = "div", legend.is.portrait = FALSE, 
              title = "") + 
  
  # style
  tm_style("white") +
  tm_layout(main.title = "F O R E S T   A R E A   D E N S I T Y", 
            main.title.fontfamily = "Candara", 
            main.title.size = 1.3,
            main.title.position = "center", 
            title = "Forest area relative to the total land area in each country",
            title.size = 1.1,
            title.position = c(0.3, 0.95),
            title.fontfamily = "Candara",
            legend.title.color = "white", 
            legend.position = c(0.15, 0.16),
            legend.just = c("center", "bottom"),
            legend.height = 0.08,
            legend.width = 0.23,
            bg.color = "#E6F2F8", 
            outer.bg.color = "white", 
            frame = T, 
            frame.double.line = T, 
            asp = 2.1) + 
  tm_credits(text = "A value of one means that the country\nforest area relative to the World\nis proportional to the country total\nland area relative to the World.", 
             position = c(0.15, 0.225), 
             size = 0.65, 
             just = "center", 
             fontfamily = "Candara")

# save map
tmap_save(filename = "forest_density_map.png", type = "cairo", dpi = 100, 
          width = 25, 
          height = 13, 
          units = "cm")


