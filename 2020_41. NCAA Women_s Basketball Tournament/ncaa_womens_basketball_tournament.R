# Tidytuesday project for 2020-10-06
# NCAA Women's Basketball Tournament

# Data source: FiveThirtyEight
# https://fivethirtyeight.com/features/louisiana-tech-was-the-uconn-of-the-80s/

# libraries
library(tidyverse)
library(skimr)
library(ggthemes)
library(extrafont)
library(grid)
library(ggplotify)

# get the data
tuesdata <- tidytuesdayR::tt_load(2020, week = 41)
tournament <- tuesdata$tournament


# CLEANING ----------------------------------------------------------------

tournament <- tournament %>% 
  filter(year >= 1994)


# order of tournament finish and change labels of playing home / away
order_finish <- c("1st", "2nd", "RSF", "RF", "NSF", "N2nd", "Champ")

tournament <- tournament %>% 
  mutate(tourney_finish = fct_relevel(tourney_finish, order_finish),
         x1st_game_at_home = ifelse(x1st_game_at_home == "Y", "First game at home", "First game away"))



# CREATION OF ADDITIONAL DATASETS ----------------------------------------

# average percent victories accross years and rounds
average_percent_year <- tournament %>% 
  group_by(year, tourney_finish) %>% 
  summarise(reg_percent = mean(reg_percent, na.rm = TRUE))

# average percent victories by rounds
average_percent_finish <- tournament %>% 
  group_by(tourney_finish) %>% 
  summarise(reg_percent = mean(reg_percent, na.rm = TRUE))

average_percent_finish <- average_percent_finish %>% 
  bind_rows(average_percent_finish) %>% 
  mutate(year = rep(c(1994, 2018), 7))


# VISUALIZATION ------------------------------------------------------------


# palette
points <- c("#eb7200", "#359be0")
lines_color <- "#5996a1"

# labels for rounds
label_names <- c(`1st` = "1st round", 
                 `2nd` = "2nd round",
                 RSF = "Sweet 16", 
                 RF = "Elite Eight", 
                 NSF = "Final Four", 
                 N2nd = "Runner-up", 
                 Champ = "Champion")


theme_set(theme_tufte())

tournament %>% 
  ggplot(aes(x = year, y = reg_percent)) + 
  
  geom_point(aes(color = x1st_game_at_home, 
                 alpha = tourney_finish), 
             pch = 15, 
             size = 2.4) + 
  
  # line trend
  geom_smooth(se = F, 
              color = lines_color, 
              size = 1.2) + 
  
  # average percentage of winnings across years
  geom_hline(data = average_percent_finish, 
             aes(yintercept = reg_percent),
             color = lines_color, 
             size = 1) + 

  facet_wrap(~tourney_finish, 
             ncol = 7,
             labeller = labeller(tourney_finish = label_names), 
             strip.position = "top") + 
  
  
  # year labels on top of lines
  geom_text(data = average_percent_finish, 
            aes(x = ifelse(year == 1994, year + 0.8, year - 0.8), 
                label = year), 
                family = "Georgia", 
                color = "black", 
            nudge_y = 1, 
            size = 3.2) +
  
# Annotations
  geom_text(data = tournament %>% 
              filter(tourney_finish == "Champ"), 
            aes(x = 2005, y = 62, 
            label = 
            "Tennessee was the\n champion in 1997\n having only won\n 67 % of games\n in the regular season"), 
            size = 3, 
            color = "grey50", 
            family = "Georgia") + 
  
  geom_curve(data = tournament %>% 
               filter(tourney_finish == "Champ"), 
             x = 2006, 
             y = 66, 
             xend = 1998, 
             yend = 69.7, 
             arrow = arrow(length = unit(0.1, "inches")), 
             color = "grey50") + 
  
  geom_text(data = tournament %>% 
              filter(tourney_finish == "1st"), 
            aes(
            x = 2009.5, y = 101, 
            label = "Liberty school: out in\n 1st round in 1998 after \nwinning every game  in\nthe regular season"),
            size = 3, 
            color = "grey50", 
            family = "Georgia") +
  
  geom_curve(data = tournament %>% 
              filter(tourney_finish == '1st'), 
            x = 2006, 
            xend = 1998, 
            y = 104.5, 
            yend = 101, 
            arrow = arrow(length = unit(0.1, "inches")), 
            curvature = 0.6,
            color = "grey50") +
  
  scale_alpha_manual(values = seq(0.3, 9, by = 0.1)) +
  scale_color_manual(values = points) +
  scale_y_continuous(limits = c(40, 105)) +
  
  guides(alpha = F) + 
  
  labs(x = NULL,
       y = "Win percentage in regular season", 
       color = NULL, 
       title = "The importance of not playing the first game away in NCAA Women's Basketball Tournament", 
       subtitle = "Percentage of victories in regular-season from 1994 to 2008 and round of the final game for each team",
       caption = "Each dot represents the percentage of victories in the regular season for one team in a specific year. Curves\nrepresent the trend across years and the straight horizontal lines represent the average score for each round.
       \nData comes from FiveThirtyEight. Visualization by Martín Pons | @MartinPonsM") +
  
  theme(text = element_text(family = "Georgia"),
        axis.text.x = element_blank(), 
        axis.ticks.x = element_blank(), 
        strip.text = element_text(size = 15, family = "Georgia", color = "#4b7e87"),
        plot.title = element_text(size = 18, color = "grey25"),
        plot.subtitle = element_text(size = 15, color = "grey45"),
        plot.caption = element_text(size = 10, hjust = 0, color = "grey25"),
        legend.position = "top", 
        legend.direction = "horizontal", 
        legend.key.size = unit(10, "mm"), 
        legend.text = element_text(size = 11, color = "grey25"))
