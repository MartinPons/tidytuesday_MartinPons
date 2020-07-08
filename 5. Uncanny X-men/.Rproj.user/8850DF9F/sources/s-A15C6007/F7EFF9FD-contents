# tidytuesday project for 2020-06-30: Claremont Run of X-Men
# https://github.com/rfordatascience/tidytuesday/blob/master/data/2020/2020-06-30/readme.md

# Data comes from http://www.claremontrun.com/ and https://twitter.com/malco_barrett


# INITIAL SETTING ---------------------------------------------------------

# libraries
library(tidytuesdayR)
library(tidyverse)
library(skimr)
library(tidygraph)
library(ggraph)
library(extrafont)
library(forcats)
library(ggrepel)
library(gridExtra)

# load data
tuesdata <- tidytuesdayR::tt_load('2020-06-30')

# helpler function to create tbl_graphs
create_xmen_graph <- function(dat, variable, weighted = TRUE, dir = FALSE) {
  
  # data.frame, character, logical, logical -> tbl_graph
  
  # creates a table graph given a data.frame ("dat") and a "variable" which will be used for
  # the "to" edge side
  # Cleans all character names in the process
  # ASSUME: data.frame has a column named "character" which will be used for 
  # the "from edge side
  
  # column selection
  dat <- dat[, c("character", variable)]  
  
  # column name change
  dat <- setNames(dat, c("from", "to"))
  
  # separate rows by comma
  dat <- dat %>% 
    separate_rows(to, sep = ", ")
  
  # removing missing
  dat <- dat %>% 
    filter(!is.na(to))
  
  # clean names from column to (remove *)
  dat <- dat %>% 
    mutate(to = str_remove(to, "\\*{1,4}"))
  
  # grouping to avoid repetition and to create weights
  dat <- dat %>% 
    count(from, to)
  
  
  ## creation of net ##
  
  # nodes
  nodes <- unique(c(dat$from, dat$to))
  
  # tbl_graph
  char_graph <- tbl_graph(edges = dat,
                          node_key = "character", 
                          nodes = data.frame(character = nodes),
                          directed = dir)
  
  # compute degree and betweenness if the network is weighted
  if (weighted) {
    char_graph <- char_graph %>% 
      mutate(degree = centrality_degree(weights = n),
             betweenness = centrality_betweenness(weights = n))
  }
  
  # return tbl_graph
  char_graph
  
}


# CLEANING ----------------------------------------------------------------

# data.frame assignation
characters <- tuesdata$characters
vis <- tuesdata$character_visualization

# name cleaning (removing alias)
characters <- characters %>% 
  mutate(character = str_extract(character, "\\w+(([ /])?\\w+){1,2}"),
         character = str_remove(character, "\\*"))

# name cleaning (removing alias)
vis <- vis %>% 
  mutate(character = str_extract(character, "\\w+(([ /])?\\w+){1,2}"),
         character = str_remove(character, "\\*"))


# SPEECH BY ISSUE PLOT ---------------------------------------------------------

# vis wrangling
vis_cum <- vis %>% 
  
  # sum character values in each issue
  group_by(issue, character) %>%
  summarise(across(2:5, sum)) %>% 
  ungroup() %>% 
  
  # compute cumulative sum of speech bubbles for each character
  group_by(character) %>% 
  mutate(cum_speech = cumsum(speech))
  
  ## visualization ##
  vis_cum %>% 
  ggplot(aes(issue, cum_speech)) + 
    
  # lines
  geom_step(aes(group = character,
                color = character == "Wolverine", 
                alpha = character == "Wolverine"), size = 1, 
            show.legend = F) + 
    
  # character labels
  geom_text(data = filter(vis_cum, issue == max(issue)), 
            aes(x = issue, y = cum_speech, label = character, color = character == "Wolverine"),
            show.legend = F, vjust = 1, family = "Agency FB", size = 5) + 
    
  
  # line colors
  scale_color_manual(values = c("grey75", "#F8CC35")) + 
  
  # transparency for non wolverine characters
  scale_alpha_manual(values = c(0.4, 1)) +
  labs(title = "Wolverine is an INSUFFERABLE BORE who just won't shut up",
       subtitle = "Cumulative speech bubbles for X-MEN characters. Issues 97 - 278", 
       x = "Issue", 
       y = "Cumulative speech Bubbles", 
       caption = "Data comes from Claremont Run Project and Malcom Barret | Visualization by @MartinPonsM") +
  
  theme_bw() + 
  theme(text = element_text(family = "Agency FB", color = "#465C95"),
        panel.border = element_blank(),
        plot.title = element_text(size = 24, hjust = 0.5),
        plot.subtitle = element_text(vjust = -3, size = 19),
        plot.caption = element_text(color = "grey45", size = 9, family = "Georgia"),
        axis.title = element_text(size = 16))


# MEMBERS WITH MOST HUMAN KILLINGS ------------------------------------------------

characters %>% 
  
  # count human killings for each character
  group_by(character) %>% 
  summarise(kills = sum(number_of_kills_humans, na.rm = T)) %>% 
  arrange(desc(kills)) %>% 
  
  # only characters which have killed some human
  filter(kills > 0) %>% 
  
  ## visualization ##
  
  # x and y axis
  ggplot(aes(kills, fct_reorder(character, kills))) + 
  
  # X in the background
  geom_text(x = 3, y = 11 / 2, label = "X", size = 700, 
            family = "Bahnschrift", color = "#DAEAF4", alpha = 1) + 
  
  # point. "|" symbol
  geom_point(aes(color = character == "Wolverine"), pch = "|", show.legend = F, size = 8) +
  
  # number of kills label
  geom_text(aes(label = kills, color = character == "Wolverine"),
            hjust = 1.5, size = 7, family = "Agency FB", show.legend = F) +
  
  # character label
  geom_text(aes(label = character, color = character == "Wolverine"),
            hjust = -0.1, family = "Agency FB", size = 7, show.legend = F) +
  
  # scales
  scale_color_manual(values = c("steelblue", "#F8CC35")) + 
  
  # titles
  labs(title = "Wolverine is a PSYCHOTIC SERIAL KILLER\n (Ok, maybe Psylocke has some issues too)",
       subtitle = "Nº of human killings by X-MEN characters. Issues 97 - 278",
       caption = "Data comes from Claremont Run Project and Malcom Barret | Visualization by @MartinPonsM") +
  
  # scales
  scale_x_continuous(limits = c(0, 6), breaks = 0:6) +
  
  # theme
  theme_bw() + 
  theme(
    text = element_text(family = "Agency FB", color = "#465C95"),
    axis.text = element_blank(), 
    axis.ticks = element_blank(), 
    axis.title = element_blank(),
    axis.line.y = element_line(color = "grey65"),
    panel.border = element_blank(),
    panel.grid = element_blank(),
    plot.title = element_text(size = 22, hjust = 0.5, vjust = 0),
    plot.subtitle = element_text(size = 18, vjust = -7, hjust = 0.02),
    plot.caption = element_text(color = "grey45", size = 9, family = "Georgia"),
    panel.background = element_rect()
  ) 



# CARRYING MEMBERS GRAPH --------------------------------------------------

# graph creation
carrying_graph <- create_xmen_graph(characters, "carrying_with_which_character", dir = T)

# random state
set.seed(111)

## visualization ##
carrying_graph %>% 
  
  # only characters which have carried anoter characters
  activate(nodes) %>% 
  filter(degree > 0) %>% 
  
  # we are interested in knowing if the character who is being carried is Wolverine
  activate(edges) %>% 
  mutate(carrying_who = ifelse(to == 22, "Carrying Wolverine", "Carrying other character")) %>% 
  
  ## visualization ##
  
  # layout
  ggraph(layout = "fr") +
  
  # edges
  geom_edge_link(aes(alpha = n / max(n), color = carrying_who), width = 1,
                 show.legend = T, arrow = arrow(type = "closed",
                                                angle = 20,
                                                length = unit(0.14, "inches"))) + 
  
  # nodes
  geom_node_point(aes(size = degree, fill = character == "Wolverine"), pch = 21) + 
  
  # labels
  geom_node_text(aes(label = character), repel = F, family = "Agency FB",
                 color = "#465C95", vjust = 2, size = 5) + 
  
  # labs
  labs(title = "Wolverine is a LITERAL DEAD WEIGHT member",
       subtitle = "X-MEN characters carrying each other. Issues 97 - 278",
       caption = "Data comes from Claremont Run Project and Malcom Barret | Visualization by @MartinPonsM") + 
  
  # scales
  scale_fill_manual(values = c("steelblue", "#F8CC35")) + 
  scale_edge_alpha(guide = "none") + 
  guides(fill = FALSE, size = F, colour = F, alpha = F) +
  
  # theme
  theme_graph(base_family = "Agency FB", 
              title_colour = "#465C95", 
              title_face = "plain",
              title_size = 22,
              subtitle_size = 17, 
              subtitle_colour = "#465C95",
              caption_family = "Georgia", 
              caption_face = "plain",
              caption_size = 9, 
              caption_colour = "grey45") +
  
  theme(legend.title = element_blank(),
        plot.title = element_text(hjus = 0.5),

        legend.position = "top",
        legend.text = element_text(size = 14, color = "grey35")) 






# CHARACTER CAPTURED -----------------------------------------------------------------

characters %>% 
    
  # counting number of times each character has been captured
  group_by(character) %>% 
  summarise(captured = sum(captured)) %>% 
  arrange(desc(captured)) %>% 
    
  # only characters who have been captured
  filter(captured > 0) %>% 
    
  ## visualization ##
    
  # axis
  ggplot(aes(captured, fct_reorder(character, captured))) + 
    
  # X in the background
  geom_text(x = 11, y = 21 / 2, label = "X", size = 700, 
            family = "Bahnschrift", color = "#DAEAF4", alpha = 1) + 
    
  # point. "|" symbol
  geom_point(aes(color = character == "Wolverine"), pch = "|", show.legend = F, size = 7) + 
  
  # character labels
  geom_text(aes(label = captured, color = character == "Wolverine"),
            hjust = 1.5, size = 6, family = "Agency FB", show.legend = F) + 
    
  # number of killings
  geom_text(aes(label = character, color = character == "Wolverine"), 
            hjust = -0.1, family = "Agency FB", size = 6, show.legend = F) +
    
 
  # titles
  labs(title = "Wolverine is pretty much USELESS and a PAIN IN THE NECK for the rest of the group",
       subtitle = "Nº of times an X-MEN character is captured. Issues 97 - 278",
       caption = "Data comes from Claremont Run Project and Malcom Barret | Visualization by @MartinPonsM") +
    
  # scales
  scale_color_manual(values = c("steelblue", "#EDCA7A")) +
  
  # theme
  theme_bw() + 
  theme(
    text = element_text(family = "Agency FB", color = "#465C95"),
    axis.text = element_blank(), 
    axis.ticks = element_blank(), 
    axis.title = element_blank(),
    axis.line.y = element_line(color = "grey65"),
    panel.border = element_blank(),
    panel.grid = element_blank(),
    plot.title = element_text(size = 22, hjust = 0.5, vjust = 0),
    plot.subtitle = element_text(size = 18, vjust = -7, hjust = 0.02),
    plot.caption = element_text(color = "grey45", size = 9, family = "Georgia"),
    panel.background = element_rect()
  ) + 
  scale_x_continuous(limits = c(0, 22), breaks = 0:6) 




