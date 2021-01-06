# transit_cost.R

# Tidytuesday dataset for week 2021-2: Transit Costs Project
# More info at https://transitcosts.com/


# INITIAL SETTING --------------------------------------------------------------------

# libraries
library(tidyverse)
library(glue)
library(Cairo)
library(scales)
library(ggtext)

# data
transit_cost <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2021/2021-01-05/transit_cost.csv')

# WRANGLING ---------------------------------------------------------------


transit_cost <- transit_cost %>% 
  filter(!is.na(e))

# parse numeric variables interpreted as character
transit_cost <- transit_cost %>% 
  mutate(tunnel_per = replace_na(parse_number(tunnel_per), 0),
         real_cost = parse_number(real_cost),
         across(start_year:end_year, as.numeric)) %>% 
  
  # remove outliers_length
  filter(length <= 80)

# fit linear regression cost ~ length
reg <- lm(real_cost ~ length, data = transit_cost)

transit_cost <- 
  transit_cost %>% 
  mutate(fit = predict(reg, transit_cost), 
         error = real_cost - fit)

# dataset for highlighted projects
outliers_error <- 
  transit_cost %>% 
  filter(error < quantile(error, 0.005) | error > quantile(error, 0.995)) %>%
  mutate(years_to_complete = replace_na(end_year - start_year, "Unspecified")) %>% 
  select(e, country, city, line, start_year, years_to_complete, tunnel, real_cost, length, fit)

# addition of formatted text for tags
outliers_error <- outliers_error %>% 
  mutate(text = glue('Project: {line}\nCity: {city} ({country})\nTotal cost: {comma(real_cost, prefix = "$", suffix =  "M")}\nStart: {start_year}\nYears to complete: {years_to_complete}\nDeviation from predicted: {comma(real_cost - fit, prefix = "$", suffix =  "M")}'), 
         hjust = c("right", "right", "right", "right", "left", "left")) 


# dataframe for segements accompaining text for highlighted projects
segments_df <- tribble(
  ~x, ~xend, ~y_shift,
  30, 30, -4000,
  20, 20, 0,
  50, 50, 0,
  72, 72, 4000,
  67, 67, -2600,
  60, 60, -1000
) %>% 
  mutate(e = outliers_error$e)

## creation of path lines connecting hightligthed points to tag text ##

# aesthetics needed for geom_path
outliers_error <- outliers_error %>% 
  left_join(segments_df, by = "e") %>% 
  mutate(y = real_cost - 2300 + y_shift, 
         yend = real_cost + 2300 + y_shift, 
         y2 = real_cost - 300 + y_shift, 
         yend2 = real_cost + 300 + y_shift, 
         x2 = ifelse(hjust == "right", x + 0.3, x - 0.3), 
         x_beggin = ifelse(hjust == "right", length - 0.7, length + 0.7))

# data.frame in format suitable for geom_path
paths_df <- outliers_error %>% 
  mutate(x_middle1 = (length + x) / 2, 
         x_middle2 = (length + x) / 2, 
         y_middle1 = real_cost, 
         y_middle2 = (y + yend) / 2,
         y_end = (y + yend) / 2
         ) %>% 
  select(e, x_beggin, x_middle1, x_middle2, x2, real_cost, y_middle1, y_middle2, y_end) %>% 
  rename(x_end = x2, y_beggin = real_cost) %>% 
  pivot_longer(names_to = c(".value", "point"), names_sep = "_", cols = 2:ncol(.))


# VISUALIZATION -----------------------------------------------------------

# CairoWin()

transit_cost %>% 
  ggplot(aes(length, real_cost)) + 
  geom_point(color = "grey65", alpha = 0.6) + 
  
  # segments connecting fitted line to highlited points
  geom_segment(data = outliers_error, aes(x = length, xend = length,
                                    y= fit, yend = real_cost - 350),
               color = "grey25") +
  
  # highglighted points
  geom_point(data = outliers_error, color = "#059fff", size = 5, shape = 1, stroke = 1) +
  geom_point(data = outliers_error, color = "white", size = 2.2) +
  
  
  # text and dashed segments indicating the increasing in cost ofr every 10 Km
  annotate(geom = "segment", x = 60, xend = 60,
           y = predict(reg, data.frame(length = 60)),
           yend = predict(reg, data.frame(length = 70)), 
           color = "#bbd1f0", lty = "dashed") +
  
  
  annotate(geom = "segment", x = 60, xend = 70,
           y = predict(reg, data.frame(length = 70)),
           yend = predict(reg, data.frame(length = 70)), 
           color = "#bbd1f0", lty = "dashed") +
  
  annotate(geom = "text", x = 65, y = predict(reg, data.frame(length = 70)) + 900,
           label = glue("Every 10 Km of road increases \n the cost in {comma(reg$coefficients[2], , prefix = '$', suffix =  'M')} on average"),
           color = "#bbd1f0", 
           size = 2.9) +

  # fitted line
  geom_smooth(method = "lm", se = F, color = "#385ee8") +
  
  # blue vertical segments next to tag text
  geom_segment(data = outliers_error, aes(x = x , xend = xend, y = y, yend = yend), color = "#059fff") +
  geom_segment(data = outliers_error, aes(x = x2, 
                                          xend = x2,
                                          y = y2, 
                                          yend = yend2), color = "#059fff") +
  
  # lines connecting highlighted points to vertigal segments
  geom_path(data = paths_df, 
            aes(x, y, group = e), color = "white", 
            linejoin = "bevel", 
            linemitre = 1) +
  
  # tagged text
  geom_text(data = outliers_error, 
            aes(ifelse(hjust == "right",x - 0.5, x + 0.5), (y + yend) / 2, label = text, hjust = hjust), 
            color = "white", 
            size = 3) +

  labs(x = "Length of the line (Km)", 
       y = "Real cost of the project (millions of $)", 
       title = "THE MOST AND LEAST COSTLY TRANSIT-INFRASTRUCTURE PROJECTS AROUND THE WORLD", 
       subtitle = "Every point represents a project. Highligthed projects are below the 0.5 percentile or above the 99.5 percentile of the  <span style='color:#385ee8'>predicted cost</span>", 
       caption = "Projects are limitied to 80 Km of road or less\nData comes from the Transit Costs Project. Visualization by Martín Pons | @MartinPonsM") +
  
  scale_x_continuous(breaks = seq(0, 80, by = 10)) +
  scale_y_continuous(labels = comma) +

  theme(
    text = element_text(family = "Candara", color = "#9dc6e0"),
    plot.background = element_rect(fill = "grey15"),
    panel.background = element_rect(fill = "grey15"), 
    panel.grid = element_blank(), 
    axis.text = element_text(size = 13, color = "#9dc6e0"), 
    axis.title = element_text(size = 13, color = "#9dc6e0"),
    plot.title = element_text(color = "#ced8f2", size = 21),
    plot.subtitle = element_markdown(color = "#9bb0c9", size = 13),
    plot.caption = element_text(color = "#9bb0c9", size = 10)
) 

ggsave(here::here("transit_cost2.png"), type = "cairo-png", dpi = 400)
