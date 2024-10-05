# Visualization for #tidytuesday 2024-10-01; Chess Game Dataset (Lichess) by Martín Pons | @MartinPonsM 
# The data comes from Kaggle / Mitchell J. It can be found here
# https://www.kaggle.com/datasets/datasnaek/chess/data

# libraries
library(tidyverse)
library(Cairo)
library(ggthemes)
library(cowplot)
library(forcats)
library(ggtext)
library(extrafont)


# load data
tuesdata <- tidytuesdayR::tt_load('2024-10-01')
chess <- tuesdata$chess

# VISUAL CONSTANTS --------------------------------------------------------

# colors
checkmate_color <- "#3e3e3e"
other_outcomes_color <- "#eccfa2"
main_text_color <- "grey10"
detail_text_color <- "#634d3b"
background_color <- "#e0ecf0"



# fonts
# This script uses fonts from the Roboto family, that maybe are not accessible in some systems. 
# They can be downloaded here
# # https://fonts.google.com/specimen/Roboto

main_title_font <- "Roboto Black"
subtitle_axis_font <- "Roboto Medium"
caption_font <- "Segoe UI"


# WRANGLING ---------------------------------------------------------------

# removing duplicated game ids
chess <- chess[!duplicated(chess$game_id), ]

# getting data.frame of games from active players (the ones that both players have at least 5 games)
df_players <- data.frame(player = c(chess$white_id, chess$black_id)) |> 
  count(player)

active_players <- df_players |> filter(n >= 5) |> pull(player)

chess_active <- chess |> 
  filter(white_id %in% active_players &
         black_id %in% active_players)


# VISUALIZATION  ----------------------------------------------

# Main plot. Stacked squares
main_plot <- chess_active |> 
  filter(turns> 2) |> # counting as chess games the ones with more than 2 turns
  
  ggplot(aes(turns)) +
  geom_bar(color = background_color, 
           aes(y = 1, 
               fill = as.factor(victory_status == "mate")), 
           size = 0.3,
           stat = "identity",
           show.legend = F) + 
  
  coord_equal() + 
  
  scale_y_continuous(limits = c(0, 50)) +
  scale_fill_manual(values = c(other_outcomes_color, checkmate_color)) +
  
  
  labs(title = "Most online chess games end in resignation",
       subtitle = "Formal resignations, disconnections, trolling, rage quits... Also draws and time forfeits. A high percentage of games don't end in checkmate", 
       caption = "Data comes from a sample of games played in lichess.com. Only games from players with at least five games were included. Visualization by Martín Pons | @MartinPonsM",
       x = "Game length in turns", 
       y = "Number of individual games") + 
  
  theme_tufte() + 
  theme(axis.ticks = element_blank(), 
        plot.background = element_rect(fill = background_color, color = background_color), 
        panel.background = element_rect(fill = background_color, color = background_color),
        plot.title = element_textbox(family = main_title_font, size = 16, color = main_text_color, hjust = 0.067, halign = 0),
        plot.subtitle = element_textbox(family = subtitle_axis_font, size = 12, color = checkmate_color, hjust = 0.15, halign = 0), 
        plot.caption = element_text(family = "Segoe UI", size = 10),
        axis.title = element_text(color = main_text_color, family = subtitle_axis_font, size = 12),
        axis.title.x = element_text(hjust = 0.058),
        axis.text.x = element_text(color = main_text_color, family = subtitle_axis_font, size = 11),
        axis.text.y = element_blank()) + 
  
  
  annotate("richtext", x = 0.5,  y = 43,
           label = "Each square represents an individual chess game played in **lichess.com**,<br>
                    concluding in **<span style='color:#3e3e3e'>checkmate</span>** or **<span style='color:#eccfa2'>other possible outcomes</span>**.<br>
                    Only active players were included, <br>
                    totalling 1,353, games", 
           hjust = "left", 
           fill = NA, 
           size = 3.7,
           text.color = detail_text_color,
           label.color = NA, 
           lineheight = 1.5) 


# Subplot. Bar plot.
subplot <- chess_active |> 
  
  count(victory_status) |> 
  
  # Reformating text labels
  mutate(f = n / sum(n), 
         victory_status = str_to_sentence(victory_status), 
         victory_status = case_when(victory_status == "Mate" ~ "Checkmate", 
                                    victory_status == "Outoftime" ~ "Out of time", 
                                    TRUE ~ victory_status)) |> 
  
  ggplot(aes(f, fct_reorder(victory_status, n))) + 
  geom_col(aes(fill = victory_status == "Checkmate"), width = 0.75, show.legend = F) + 
  geom_text(aes(label = scales::percent(f, accuracy = 1)),
            hjust = "left", 
            nudge_x = 0.03,
            size = 3.4, 
            color = detail_text_color)  +
  
  labs(title = "Game endings") + 
  
  scale_fill_manual(values = c(other_outcomes_color, checkmate_color)) +
  scale_x_continuous(limits = c(0, 0.7)) +
  
  theme_void() + 
  theme(axis.text.y = element_text(hjust = 0, size = 9, color = detail_text_color), 
        plot.title = element_text(family = subtitle_axis_font, hjust = 0.1, size = 11))
  

# Drawing the two plots
ggdraw(main_plot) + 
  draw_plot(subplot, x = 0.685, y = 0.51, width = 0.14, height = 0.32)



# saving plot
ggsave(here::here("chess.png"),
       device = png, 
       type = "cairo",
       dpi = 300, 
       width = 40, 
       height = 12.9, 
       units = "cm")
