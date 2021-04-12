

library(tidyverse)
library(skimr)
library(ggthemes)
library(extrafont)
library(lubridate)
library(zoo)
library(Cairo)
library(glue)

tuesdata <- tidytuesdayR::tt_load(2021, week = 9)

employed <- tuesdata$employed
earn <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2021/2021-02-23/earn.csv')


skim(employed)

palette_single3 <- c("#6d5e45", "#f6e0bd", "#af9c7e")

# eliminacion del total en race render

employed <- employed %>% 
  filter(!(industry %in% c(NA, "Women", "White", "Men", "Black or African American", 
                           "Asian"))) %>%  
         mutate(minor_occupation = str_replace(minor_occupation, 
                                        "Manage-ment", 
                                        "Management"))

employed_total <- employed %>% 
  filter(race_gender == "TOTAL")

employed_gender <- employed %>% 
  filter(race_gender %in% c("Men", "White"))

employed_race <- employed %>% 
  filter(!(race_gender %in% c("TOTAL", "Women", "Men")))


skim(employed)

employed_total %>% 
  
  filter(year != 2020) %>% 
  
  group_by(year, major_occupation) %>% 
  summarise(employ_n = sum(employ_n)) %>%
  
  group_by(major_occupation) %>% 
  mutate(index_employ_n = employ_n / first(employ_n)) %>% 
  
  ggplot(aes(year, index_employ_n)) +
  geom_line(aes(color = major_occupation))

employed_total %>% 
  filter(year!= 2020, str_detect(minor_occupation, "Sales")) %>% 
  group_by(year, minor_occupation) %>% 
  summarise(employ_n = sum(employ_n)) %>%
  group_by(minor_occupation) %>% 
  mutate(index_employ_n = employ_n / first(employ_n)) %>% 
  ggplot(aes(year, index_employ_n)) +
  geom_line(aes(color = minor_occupation))
  
filter(employed, industry == "Women") %>% View()

# que es lo que ha crecido tanto en Management
employed_total %>% 
  filter(year != 2020, str_detect(major_occupation, "Management")) %>% 
  group_by(year, minor_occupation) %>% 
  summarise(employ_n = sum(employ_n)) %>%
  group_by(minor_occupation) %>% 
  mutate(index_employ_n = employ_n / first(employ_n)) %>% 
  ggplot(aes(year, index_employ_n)) +
  geom_line(aes(color = minor_occupation))



employed %>% 
  filter(year %in% c(2019)) %>% 
  group_by(year, major_occupation) %>% 
  summarise(employ_n = sum(employ_n)) %>% 
  ggplot(aes(employ_n, fct_reorder(major_occupation, employ_n))) + 
  geom_point(pch = "|", size = 10) + 
  geom_text(aes(label = major_occupation), family = "Georgia", hjust = "right") +
  scale_x_continuous(limits = c(0, 1.6e+08)) +
  theme(text = element_text(family = "Georgia")) 
        )

employed_race %>% 
  group_by(year, major_occupation, race_gender) %>% 
  summarise(employ_n = sum(employ_n)) %>% 
  ggplot(aes(year, employ_n)) + 
  geom_col(aes(fill = race_gender), position = "fill") + 
  facet_wrap(~race_gender)

# women men ominor occupacionts




# EARNINGS ----------------------------------------------------------------
CairoWin()
skim(earn)

earn %>% 
  filter(sex != "Both Sexes", race != "All Races", ethnic_origin == "All Origins") %>% 
  
  group_by(year, quarter, sex, race) %>% 
  summarise(median_weekly_earn = mean(median_weekly_earn)) %>% 
  
  mutate(date = parse_date_time(glue("{year}Q{quarter}"), "%Y%q")) %>% 
  
  pivot_wider(names_from = c(sex, race), values_from = median_weekly_earn) %>% 
  
  mutate(diff_white = Women_White / Men_White, 
         diff_black = `Women_Black or African American` / `Men_Black or African American`, 
         diff_asian = Women_Asian / Men_Asian) %>% 
  
  ungroup() %>% 
  
  select(date, diff_white, diff_black, diff_asian) %>% 
  pivot_longer(names_to = "group", values_to = "difference", 2:4) %>% 
  
  filter(year(date) == 2019)
  
  ggplot(aes(date, difference)) + 
  geom_line(aes(color = group))
  
  
  
CairoWin()
  earn %>% 
    filter(sex != "Both Sexes", race != "All Races", ethnic_origin == "All Origins") %>% 
    
    group_by(year, sex, race) %>% 
    summarise(median_weekly_earn = sum(median_weekly_earn *n_persons) / sum(n_persons)) %>% 
    
    pivot_wider(names_from = c(sex, race), values_from = median_weekly_earn) %>% 
    
    mutate(diff_white = Women_White / Men_White, 
           diff_black = `Women_Black or African American` / `Men_Black or African American`, 
           diff_asian = Women_Asian / Men_Asian) %>% 
    
    ungroup() %>% 
    
    select(year, diff_white, diff_black, diff_asian) %>% 
    pivot_longer(names_to = "group", values_to = "difference", 2:4) %>% 
    
    filter(year == 2019) %>% 
    
    ggplot(aes(difference, fct_reorder(group, -difference))) + 
    geom_col(aes(fill = group), width = 0.85) + 
    
    geom_text(aes(label = round(difference * 100), x = difference + 0.01),
              hjust = "left", 
              size = 5,
              family = "georgia", 
              nudge_y = 0.2) + 
    
    coord_polar() + 
    
    scale_x_continuous(limits = c(0, 1)) + 
    scale_fill_manual(values = palette_single3) +
    theme_void() + 
    theme(
          legend.position = "top")
  
  # Dos piecharts mas. Cada uno comparando uno de los dos sexos por razas
  
  earn %>% 
    filter(race == "All Races", 
           sex == "Both Sexes",
           ethnic_origin == "All Origins") %>% 
    
    mutate(date = parse_date_time(glue("{year}Q{quarter}"), "%Y%q")) %>% 
    
    group_by(date) %>% 
    summarise(median_weekly_earn = sum(median_weekly_earn * n_persons) / sum(n_persons)) %>% 
    
    ggplot(aes(date, median_weekly_earn)) + 
    geom_line()
  
