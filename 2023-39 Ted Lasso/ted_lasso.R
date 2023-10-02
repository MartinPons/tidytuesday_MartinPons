# Visualization for #tidytuesday, 2023-09-26; Roy Kent F**K count. by Martín Pons | @MartinPonsM
# The data has been obtained by Deepsha Menghani, who watched every Ted Lasso episode

# Fonts from the family GraphicusDT have to bee installed. Please refer to the visualization, at the end of this document

# INITIAL SETTING ---------------------------------------------------------

# libraries
library(tidyverse)
library(extrafont)
library(ggtext)
library(Cairo)

# load data
tuesdata <- tidytuesdayR::tt_load('2023-09-26')


richmondway <- tuesdata$richmondway

richmondway <- richmondway |> 
  mutate(season_ep_cleanded = as.numeric(str_remove(Season_Episode, "^S[0-9]_e")))


# colors
richmond_blue <- "#0176ea"
desaturated_richmond_red <- "#e78888" 
lasso_moustache <- "#594745"



# DATA WRANGLING ----------------------------------------------------------


build_area_coords <- function(n, rows, cols){
  
  "creates a dataframe of coordinates (row and column)  where each coordinate 
  is a point. Beggining by 1, 1, for each row of the data.frame increases by one
  unit the number of rows or columns. The purpose of this data.frame is to be the source data
  for a grid chard build with geom_tile
  
  INPUTS
  # n (integer): total count
  # rows (integer): number of rows
  # cols (integer): number of columns
  
  RETURNS
  # a dataframe with rows equal to the total count provided in n, with row an column coordinates
  "

 # data.frame with coordinates delimited by rows and columns provided
 # the number of rows is rows * cols
 dat <- expand_grid(cols = 1:cols, rows = 1:rows)
 
 # the actual number of rows to match the total count 
 # is obtained from removing the remaining rows when subtracting rows * cols from n
 coords_to_remove <- rows * cols - n
 if (coords_to_remove < 0) stop ("rows * cols must be equal or greater than n")
 
 # if the total count can be shaped as a rectangle according to rows and cols we don't remove any row from the data.frame
 if (coords_to_remove == 0) {
   
   return(dat)
   
  # if total counts can't be shaped as a perfect rectangle, we remove the remaining rows
  } else {
    rows_to_remove <-  (nrow(dat) - coords_to_remove + 1):nrow(dat)
  }
 
 dat[-rows_to_remove, ]
  
}


# Obtaining the total fuck counts in each season
rich_total_season <- richmondway |> 
  group_by(Season) |> filter(cum_total_season == last(cum_total_season)) |> 
  select(Season, cum_rk_season, cum_total_season) |> 
  ungroup() |> 
  mutate(rows = 15, 
         cols = c(10, 15, 26))


# creating the data frame which will be used as a data source for the geom_tile
grid_data <- pmap_df(list(rk = rich_total_season$cum_rk_season, 
             total = rich_total_season$cum_total_season, 
             rows = rich_total_season$rows, 
             cols = rich_total_season$cols, 
             season = paste("SEASON", 1:3)), 
        
        function(rk, total, rows, cols, season) {
          
          build_area_coords(total, rows, cols) |> 
            mutate(rk = c(rep(TRUE, rk), rep(FALSE, total - rk)), 
                   season = season)
        })

# VISUALIZATION ----------------------------------------------------------

CairoWin()
grid_data |> 
  ggplot(aes(rows, cols)) + 
  geom_tile(color = richmond_blue, size = 0.7, aes(fill = rk), show.legend = FALSE) + 
  coord_fixed(ratio = 1) + 
  facet_wrap(~season, ncol = 3) +
  
  labs(title = "F**K COUNT ALONG THE THREE SEASONS OF TED LASSO",
       subtitle = "The number of times <span style = 'color:#594745;'>Roy Kent</span> or
       <span style = 'color:#e78888;'>any other character</span> say the word Fuck", 
       caption = "Data comes from Deepsha Menghani. Visualization by Martín Pons | @MartinPonsM") +
  
  scale_fill_manual(values = c(desaturated_richmond_red, lasso_moustache)) + 
  
  theme_void() + 
  theme(
        plot.title = element_text(family = "GraphicusDT-BoldOblique", color = "white", size = 32),
        plot.subtitle = element_textbox_simple(family = "GraphicusDT-Demi", color = "white", size = 20),
        plot.caption = element_text(family = "GraphicusDT-Book", color = "white", size = 16),
        panel.background = element_rect(fill = richmond_blue, color = richmond_blue),
        plot.background = element_rect(fill = richmond_blue, color = richmond_blue),
        panel.spacing=unit(8,"lines"),
        panel.border = element_blank(),
        strip.text = element_text(family = "GraphicusDT-BoldOblique", 
                                  size = 24, 
                                  color = "white",
                                  margin = margin(b = 30, t = 30))
        )
  


