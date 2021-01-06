# transit_cost.R



# INITIAL SETTING --------------------------------------------------------------------

# libraries
library(tidyverse)
library(skimr)
library(glue)
library(Cairo)
library(scales)

# data
# tuesdata <- tidytuesdayR::tt_load(2021, week = 2)
# transit_cost <- tuesdata$transit_cost

transit_cost <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2021/2021-01-05/transit_cost.csv')

glimpse(transit_cost)
skim(transit_cost)


# WRANGLING ---------------------------------------------------------------

# eliminamos NA en la id. Son filas que parecen más de agregaciones o metadatos

transit_cost <- transit_cost %>% 
  filter(!is.na(e))

#hay que pasar variables definidas como categoricas, a numericas
transit_cost <- transit_cost %>% 
  mutate(tunnel_per = parse_number(tunnel_per),
         real_cost = parse_number(real_cost),
         across(start_year:end_year, as.numeric)) %>% 
  
  # remove outliers_length
  filter(length <= 80)

skim(transit_cost)
# HACER ALGUN TIPO DE DASHBOARD CON LAS TOP DE COSTE???


# hay claros outliers en todas las numericas. A ver a quien pertenecen
outliers <- transit_cost %>% 
  filter(real_cost > quantile(real_cost, 0.985)) %>% 
  select(real_cost, length)

reg <- lm(real_cost ~ length, data = transit_cost)$coefficients
outliers <- 
  outliers %>% 
  mutate(fit = reg[1] + reg[2] * length)

transit_cost <- 
  transit_cost %>% 
  mutate(fit = reg[1] + reg[2] * length, 
         error = real_cost - fit)

outliers_error <- 
  transit_cost %>% 
  filter(error < quantile(error, 0.005) | error > quantile(error, 0.995)) %>%
  mutate(years_to_complete = end_year - start_year) %>% 
  select(e, country, city, line, start_year, years_to_complete, tunnel, real_cost, length, fit)

segments_df <- tribble(
  ~x, ~xend, ~y_shift,
  30, 30, -1000,
  20, 20, 0,
  50, 50, 0,
  72, 72, 3500,
  67, 67, -1000,
  60, 60, -1000
) %>% 
  mutate(e = outliers_error$e)

# paths_df <- data.frame(e = rep(outliers_error$e, each = 4),
#                        length = c(41.5, 35, 35, 30, 43, 30, 30, 20, )
                       
                       
outliers_error <- outliers_error %>% 
  mutate(text = glue('Project: {line}\nCity: {city} ({country})\nStart: {start_year}\nYears to complete: {years_to_complete}\n% of lenght is tunnel: {tunnel}\nDeviation from predicted: {comma(real_cost - fit, prefix = "$", suffix =  "M")}'), 
         hjust = c("right", "right", "right", "right", "left", "left")) 

outliers_error <- outliers_error %>% 
  left_join(segments_df, by = "e") %>% 
  mutate(y = real_cost - 1800 + y_shift, 
         yend = real_cost + 1800 + y_shift, 
         y2 = real_cost - 300 + y_shift, 
         yend2 = real_cost + 300 + y_shift, 
         x2 = ifelse(hjust == "right", x + 0.3, x - 0.3))

paths_df <- outliers_error %>% 
  mutate(x_middle1 = (length + x) / 2, 
         x_middle2 = (length + x) / 2, 
         y_middle1 = real_cost, 
         y_middle2 = (y + yend) / 2,
         y_end = (y + yend) / 2
         ) %>% 
  select(e, length,  x_middle1, x_middle2, x2, real_cost, y_middle1, y_middle2, y_end) %>% 
  rename(x_beggin = length, x_end = x2, y_beggin = real_cost) %>% 
  pivot_longer(names_to = c(".value", "point"), names_sep = "_", cols = 2:ncol(.))


 # mutate(x = ifelse(hjust == "rigth", x - 1, x + 1))


# imagino que coste en dolares se relaciona con la longitud

# podemos hacer scatterplot y resaltar puntos que se han destivado demasiado del coste medio
CairoWin()

transit_cost %>% 
  ggplot(aes(length, real_cost)) + 
  geom_point(color = "grey55", alpha = 0.6) + 
  geom_segment(data = outliers_error, aes(x = length, xend = length,
                                    y= fit, yend = real_cost - 350),
               color = "grey25") +
  geom_point(data = outliers_error, color = "#059fff", size = 5, shape = 1, stroke = 1) +
  geom_point(data = outliers_error, color = "white", size = 2.2) +

  geom_smooth(method = "lm", se = F) +
  
  geom_segment(data = outliers_error, aes(x = x , xend = xend, y = y, yend = yend), color = "white") +
  geom_segment(data = outliers_error, aes(x = x2, 
                                          xend = x2,
                                          y = y2, 
                                          yend = yend2), color = "white") +
  
  geom_path(data = paths_df, 
            aes(x, y, group = e), color = "white", 
            linejoin = "bevel", 
            linemitre = 1) +
  
  geom_text(data = outliers_error, 
            aes(ifelse(hjust == "right",x - 0.5, x + 0.5), (y + yend) / 2, label = text, hjust = hjust), 
            color = "white", 
            size = 3) +

  theme(panel.background = element_rect(fill = "grey15"), 
        panel.grid = element_blank()) 

ggsave(here::here("transit_cost.png"), type = "cairo-png")