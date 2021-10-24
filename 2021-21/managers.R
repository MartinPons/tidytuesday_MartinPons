

# tidytuesday from 2021, week 21: Aks A Manager
# https://github.com/rfordatascience/tidytuesday/blob/master/data/2021/2021-05-18/readme.md

# Data comes from the Ask a Manager Survey


library(tidyverse)
library(lubridate)
library(ggthemes)
library(Cairo)
library(scales)
library(extrafont)
library(cowplot)
library(ggtext)

tuesdata <- tidytuesdayR::tt_load(2021, week = 21)
survey <- tuesdata$survey



# DATA WRANGLING ----------------------------------------------------------


# set the order of education levels
education_order <- c("High School", 
                     "Some college", 
                     "College degree", 
                     "Professional degree (MD, JD, etc.)",
                     "Master's degree",
                     "PhD"
                     )

survey <- survey %>% 
  
  # reorder education
  mutate(highest_level_of_education_completed = fct_relevel(highest_level_of_education_completed,
                                                            education_order), 
         # format timestamp
         timestamp = mdy_hms(timestamp))





## getting data related jobs and additional filtering ##
dj <- survey %>% 
  
  # removing oultiers, posible errors
  filter(annual_salary > 300,
         
         # filtering for data related jobs
         str_detect(job_title, "Data"),
         
         # filtering for US
         str_detect(str_to_upper(country), "^((UNITED STATES(OF AMERICA)?)|(USA)|(U(\\.)?S(\\.)?))")) %>% 
  
  mutate( industry_grouped = fct_lump(industry, 9))



## summarise salaries by industry ##

# this is to plot segments an values

industry_avgs <- dj  %>% 
group_by(industry_grouped) %>% 
  summarise(across(annual_salary, .fns = list(q1 = ~quantile(.x, 0.25), 
                                              min = min,
                                              max = max,
                                              median = median,
                                              mean = mean,
                                              q3 = ~quantile(.x, 0.75)))) %>% 
  
  mutate(industry_grouped = fct_reorder(industry_grouped, annual_salary_median)) %>% 
  ungroup() %>% 
  
  # I had to use numeric coordinates for the y axis in order to nudge the way that I wanted
  mutate(y_position = as.numeric(industry_grouped),
         
         # just uppercase and separate by letters industry labels to get a beautiful format
         industry_grouped = map_chr(industry_grouped, 
                                    ~paste0(unlist(str_split(str_to_upper(.x), "")),
                                   collapse = " ")))



# VISUALIZATION -----------------------------------------------------------

theme_set(theme_tufte())

CairoWin()

palette <- c("#4d2a0e", 
             "#6f4a2e",
             "#926b50",
             "#b58f74",
             "#dab49a",
             "#ffdbc2")


line_color <- "#4e778d"
text_color <- "#030c12"
bg_color <- "#f2efee" #"#f3efec"
median_line_color <- "#7a929f"
text_annotation_color <- "grey40" 

# constant for manual nudging
nudge <- 0.33

# manin visualization
g_main <- dj %>% 
  
  # applying same formats as aggregated data to raw data
  mutate(industry_grouped = fct_reorder(industry_grouped, annual_salary, median), 
         y_position = as.numeric(industry_grouped)) %>% 
  
  
  ggplot(aes(annual_salary, y_position)) +
  
  # dots
  geom_jitter(aes(fill = highest_level_of_education_completed), 
              color = bg_color, 
              pch = 21, 
              size = 2.5, 
              stroke = 0.7,
              show.legend = T,
              height = 0.15, 
              alpha = 0.8) + 
  
  # industry labels
  geom_text(data = industry_avgs,
            aes(annual_salary_median, y_position - nudge, 
                label = industry_grouped), 
            nudge_x = -2000,
            nudge_y = -0.15, 
            hjust = "right", 
            size = 4.5, 
            color = text_color,
            family = "Gadugi") +
  
  # median salary labels
  geom_text(data = industry_avgs, 
            aes(x = annual_salary_median, 
                y = y_position - nudge, 
                label = number(annual_salary_median / 1000,
                               prefix = "$ ", 
                               suffix = "k",
                               accuracy = 1)), 
            hjust = "left", 
            nudge_x = 2000,
            nudge_y = -0.15, 
            size = 5, 
            color = "#152733", 
            family = "Gadugi") +
  
  # interquartile segment
  geom_segment(data = industry_avgs, 
               aes(x = annual_salary_q1, 
                   xend = annual_salary_q3,
                   y = y_position - nudge, 
                   yend = y_position - nudge), 
               color = line_color, 
               size = 0.75) +
  
  # median segment
  geom_segment(data = industry_avgs, 
               aes(x = annual_salary_median, 
                   xend = annual_salary_median, 
                   y = y_position - nudge - 0.05, 
                   yend = y_position - 0.58), 
               color = median_line_color, 
               size = 0.75) +
  
  labs(title = "A N N U A L   S A L A R Y   F O R   M A N A G E R S   I N  D A T A   R E L A T E D   J O B S", 
       subtitle = "Based on the responses on the *Ask a Manager Survey* among the blog readers from the US, conducted in 2021. Each dot represents a respondent",
       fill = "Highest level of education completed", 
       caption = "Please note that the data doesn't necessary reflect the general population. 
       Data comes from the *Ask a Manager Survey*. Visualization by Martín Pons | @MartinPonsM") +
  
  scale_fill_manual(values = palette[6:1]) + 
  scale_x_continuous(labels = number_format(scale = 0.001, suffix = "k", prefix ="$"), 
                     limits = c(0, 4e5)) +
  
  theme(text = element_text(family = "Gadugi", color = text_color), 
        plot.title = element_text(hjust = 0.5, size = 22),
        plot.subtitle = element_textbox(hjust = 0.5, size = 15),
        plot.caption = element_textbox(hjust = 1, size= 11),
        plot.background = element_rect(fill = bg_color, 
                                       color = bg_color),
        
        legend.title = element_text(size = 12), 
        legend.text = element_text(size = 11),
        legend.position = "top",
        legend.box = "horizontal",
        
        axis.title = element_blank(),
        axis.text.x = element_text(size = 13),
        axis.text.y = element_blank(), 
        axis.ticks = element_blank())

# annotation plot explaing the segments
g_anot <- ggplot() + 
  annotate(geom = "segment", 
           x = -0.9, xend = 0.9, 
           y = 0, yend = 0, 
           size = 0.8, 
           color = line_color, 
           ) + 
  
  annotate(geom = "segment", 
           x = 0, xend = 0, 
           y = -0.04, yend = -0.22, 
           size = 0.8, 
           color = median_line_color, 
  ) + 
  
  annotate(geom = "text", 
           x = 0, 
           y = 0.28, 
           label = 
           "Interquartile range: 50 % of respondents
reported an annual salary that lies
between the limits of the segment", 
           family = "Gadugi", 
           color = "grey40") +
  
  annotate(geom = "text", 
           x = 0, 
           y = -0.4, 
           label = 
             "Median salary\nwithin the group", 
family = "Gadugi", 
color = "grey40") +
  
  xlim(-1.2, 1.2) + 
  ylim(-1, 1) +
  theme_void()

# draw plots
g_main + draw_grob(as_grob(g_anot), 
                   x = 3e5, 
                   y = 1, 
                   width = 80000, 
                   height = 3)
 




