
# Tidytuesday from 2021, week 33
# https://github.com/rfordatascience/tidytuesday/blob/master/data/2021/2021-08-10/readme.md

# Data and additional info comes from "Measuring Infrastructure in the Bureau of Economic Analysis National Economic Accounts"
# By Jennifer Bennett, Robert Kornfeld, Daniel Sichel and David Wasshausen
# And publised by the Bureau of Economic Analysis 

# INITIAL SETTING ---------------------------------------------------------

library(tidyverse)
library(forcats)
library(ggthemes)
library(Cairo)
library(extrafont)
library(scales)
library(ggtext)

# graphical theme
theme_set(theme_tufte())

# data
tuesdata <- tidytuesdayR::tt_load(2021, week = 33)

chain_investment <-tuesdata$chain_investment #  readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2021/2021-08-10/chain_investment.csv')
investment <- tuesdata$investment


# function to uppercase and create a blank space between a string character. Used in plot title and in main category labels
insert_blank_between <- function(string) {

  map_chr(string, ~paste0(unlist(str_split(str_to_upper(.x), "")), collapse = " "))
  
}


# DATA WRANGLIND ----------------------------------------------------------

# Agregation of basic infrastructure types by year
chi_year <- chain_investment %>% 
  filter(category %in% c("Total basic infrastructure", 
                         "Total social infrastructure", 
                         "Total digital infrastructure")) %>% 
  group_by(year, category) %>% 
  summarise(gi = sum(gross_inv_chain)) %>% 
  
  # Format infrastructure category names
  mutate(category = str_trim(str_to_upper(str_match(category, "([aA-zZ]+) ([aA-zZ]+) ([aA-zZ]+)")[ ,3])), 
         category = fct_relevel(paste(category, "INFRASTRUCTURE"), paste(c("BASIC", "SOCIAL", "DIGITAL"), "INFRASTRUCTURE"))) 

# Data set used for category title placement inside the plot and as reference for segments in annotations
labels_dataset <-  
  chi_year %>%  
  ungroup() %>% 
  filter(year %in% c(min(year), max(year))) %>% 
  group_by(year) %>% 
  arrange(desc(category))%>%
  mutate(f_sum = gi / sum(gi), 
         F_sum = cumsum(f_sum), 
         f_middle = F_sum + (lag(F_sum) - F_sum) / 2, 
         f_middle = ifelse(category == "DIGITAL INFRASTRUCTURE", f_sum / 2, 
                           f_middle))




# point coordinates as a reference for placing the segments of the main annotations
text_base_heigth <- 1- labels_dataset$f_sum[6]
text_base_heigth_right <- 1- labels_dataset$f_sum[6]



# VISUALIZATION -----------------------------------------------------------


g <- chi_year %>% 
  
  ggplot(aes(year, gi)) + 
  
  # Area
  geom_area(aes(fill = category), 
            position = "fill", 
            color = "white", 
            show.legend = F) + 
  
  # Title labels
  geom_text(data =labels_dataset %>% 
              filter(year == 2017), 
            aes(x = c(1998, 1993, 1988), 
                y = c(0.08, 0.35, 0.75), 
                label = insert_blank_between(category)), 
            hjust = "left",
            color = "white", 
            size = 3) + 

  
  annotate(geom = "text", 
           x = 1988, 
           y = 0.73, 
           hjust = "left", 
           color = "white", #"#3a4535", 
           size = 2.4,
           label = "Includes things like transportation or water supply", 
           lineheight = 0.8) +
  
  annotate(geom = "text", 
           x = 1993, 
           y = 0.33, 
           hjust = "left", 
           color = "white", #"#162c2b", 
           size = 2.4,
           label = "Includes things like schools and hospitals", 
           lineheight = 0.8) +
  
  annotate(geom = "text", 
           x = 1998, 
           y = 0.05, 
           hjust = "left", 
           color = "white", #"#c0d3dd", # "#bacfd9"
           size = 2.4,
           label = "Enables the storage and exchange of data through a\ncentralized communication system", 
           lineheight = 0.8) +
  
  # x axis with year labels
  geom_text(data = data.frame(gi = 0.01, 
                              year = seq(1950, 2010, by = 10)), 
            aes(label = year), 
            color = "white", 
            size = 2.5) +
  
  
  # Basic infrastructure text labels in 2017 
  geom_segment(data = labels_dataset %>% 
                 filter(str_detect(category, "BASIC"), year == 2017), 
               aes(x = year, xend = year - 11, y = text_base_heigth_right, yend = 1 - f_sum),
               color = "white", 
               size = 0.3) + 
  
  
  geom_point(data = labels_dataset %>% 
               filter(str_detect(category, "BASIC"), year == 2017), 
             aes(x = year - 11, 
                 y = text_base_heigth_right), 
             color = "white", 
             size = 2) + 
  
  
  geom_point(data = labels_dataset %>% 
               filter(str_detect(category, "BASIC"), year == 2017), 
             aes(x = year - 11, 
                 y = text_base_heigth_right), 
             color = "#3a4535", 
             size = 1.5)  + 
  
  
  annotate(geom = "text", x = 2007, y =  text_base_heigth_right + 0.045, 
           label = "By 2017", 
           hjust = "left",
           vjust = "bottom", 
           lineheight = 0.8, 
           size = 3,
           color = "white") +
  
  annotate(geom = "text", x = 2007, y =  text_base_heigth_right + 0.01, 
           label = percent(labels_dataset$f_sum[6], accuracy = 1),
           hjust = "left",
           vjust = "bottom", 
           lineheight = 0.8, 
           size = 5,
           color = "#3a4535") + 
  
  annotate(geom = "richtext", x = 2010.2, y =  text_base_heigth_right, 
           label = "Of the total <br>remained as <br><span style='color:#3a4535;'>basic expenditure</span>", 
           hjust = "left",
           vjust = "bottom", 
           lineheight = 1.2, 
           size = 2.3,
           color = "white", 
           fill = NA, 
           label.color = NA) +
  
  
  # Basic infrastructure text labels in 1947
  geom_segment(data = labels_dataset %>%
                 filter(str_detect(category, "BASIC"), year == 1947),
               aes(x = year, xend = year + 1, y = 1 - f_sum, yend = 1 - f_sum),
               color = "white",
               size = 0.3,
               linejoin = "round",
               lineend = "round") +
  
  geom_segment(data = labels_dataset %>% 
                 filter(str_detect(category, "BASIC"), year == 1947), 
               aes(x = year + 1, xend = year + 1, y = 1 - f_sum, yend = text_base_heigth),
               color = "white", 
               size = 0.3, 
               linejoin = "round", 
               lineend = "round") + 
  
  geom_segment(data = labels_dataset %>% 
                 filter(str_detect(category, "BASIC"), year == 1947), 
               aes(x = year + 1, xend = year + 11.5, y = text_base_heigth, yend = text_base_heigth),
               color = "white", 
               size = 0.3, 
               linejoin = "round", 
               lineend = "round") + 
  
  geom_point(data = labels_dataset %>% 
               filter(str_detect(category, "BASIC"), year == 1947), 
             aes(x = year + 11.5, 
             y = text_base_heigth), 
             color = "white", 
             size = 2) + 
  
  
  geom_point(data = labels_dataset %>% 
               filter(str_detect(category, "BASIC"), year == 1947), 
             aes(x = year + 11.5, 
                 y = text_base_heigth), 
             color = "#3a4535", 
             size = 1.5)  + 
  
  
  annotate(geom = "text", x = 1947 + 1, y =  text_base_heigth + 0.045, 
           label = "In 1947", 
           hjust = "left",
           vjust = "bottom", 
           lineheight = 0.8, 
           size = 3,
           color = "white") + 

  annotate(geom = "text", x = 1947 + 1, y =  text_base_heigth + 0.01, 
           label = percent(labels_dataset$f_sum[5], accuracy = 1),
           hjust = "left",
           vjust = "bottom", 
           lineheight = 0.8, 
           size = 5,
           color = "#3a4535") + 
  
  annotate(geom = "richtext", x = 1947 + 4.2, y =  text_base_heigth, 
           label = "of the total expenditure<br>was placed in<br><span style='color:#3a4535;'>basic infrastructure</span>",
           hjust = "left",
           vjust = "bottom", 
           lineheight = 1.2, 
           size = 2.3,
           color = "white", 
           fill = NA, 
           label.color = NA) + 

   
  # Context text below plot title
  annotate(geom = "richtext", x = 1947.5, y = 0.95, 
           label = "
As the <span style='color:#1d465c;'>digital</span> era changes the way we look at the world, we might also have to rethink the concept of infrastructure.<br>
The word that once was reserved for transportation, energy or things like schools and hospitals<br>
now has to  make room for <span style='color:#1d465c;'>communication structures, cell towers, computers and software</span>.<br>
This also brings with it a change in ownership: an important share<br>
of this new <span style='color:#1d465c;'>digital infrastructure</span> is privately owned." ,
           color = "grey95", 
           fill = NA, 
           label.color = NA, 
           hjust = "left", 
           vjust = "top",
           size = 2.7, 
           family = "Bahnschrift", 
           lineheight = 1.5) + 

  # labs
  labs(title = insert_blank_between("THE DIGITAL SHIFT IN THE U.S INFRASTRUCTURE"),
       caption = "All data and aditional info comes form the Bureau of Economic Analysis. Visualization by Martín Pons | @MartinPonsM") + 
  
  # sclaes
  scale_fill_manual(values = c("#96a88f", "#48908d", "#1d465c")) + 
  scale_y_continuous(expand = c(0,0)) +
  scale_x_continuous(expand = c(0, 0)) +
  
  #theme
  theme(axis.text = element_blank(), 
        axis.title = element_blank(),
        axis.ticks.y = element_blank(), 
        axis.ticks.x = element_blank(), 
        plot.title = element_text(family = "Candara", hjust = 0.03, vjust = -6, color = "grey20", size = 16), 
        plot.caption = element_text(family = "Candara", size = 7, 
                                    hjust = 1, 
                                    margin = margin(t = 0, r = 40, b = 10, l = 0)), 
        plot.caption.position = "plot"
        ) 




ggsave(plot = g, "C:/Users/marti/OneDrive/Favoritos/Documentos/tidy_tuesday_projects/expenditures.png", type = "cairo-png", dpi = 300, 
       width = 11, 
       height = 7)


