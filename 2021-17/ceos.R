
# Tidytuesday 2021-17. CEO departures
# data comes from Gentry et al. by way of DatalsPlural


library(tidyverse)
library(ggthemes)
library(ggtext)
library(ggiraph)
library(glue)
library(ggtext)
library(extrafont)

tuesdata <- tidytuesdayR::tt_load(2021, week = 18)

departures <- tuesdata$departures


# DATA WRANGLING ----------------------------------------------------------

departures <- departures %>% 
  mutate(coname = str_remove(coname, " +INC|CO?(RP)$"), 
         motive = case_when(departure_code == 1 ~ "Death", 
                            departure_code == 2 ~ "Health Concerns", 
                            departure_code == 3 ~ "Job performance", 
                            departure_code == 4 ~ "Policy related problems", 
                            departure_code == 5 ~ "Voluntary turnover",
                            departure_code == 6 ~ "When to work in other company", 
                            departure_code == 7 ~ "Departure following a marger adquisition", 
                            departure_code %in% 8:9 ~ "Unknown"))


#  top firms
top_departure_firms_df <- departures %>% 
  drop_na(departure_code) %>% 
  count(coname) %>% 
  arrange(desc(n)) %>% 
  slice_max(n, 
            n = 20, 
            with_ties = F) 
  
top_departure_firms <- top_departure_firms_df$coname

# get number of voluntary and involuntary departures
departure_firms_main_cause <- departures %>% 
  
  filter(coname %in% top_departure_firms) %>% 
  count(coname, ceo_dismissal) %>% 
  mutate(main_cause = case_when(ceo_dismissal == 0 ~ "voluntary", 
                                ceo_dismissal == 1 ~ "involuntary", 
                                TRUE ~ "unknown")) %>% 
  
  select(-ceo_dismissal) %>% 
  
  pivot_wider(names_from = main_cause, values_from = n, 
              values_fill = 0)




# VISUALIZATION -------------------------------------------------

palette <- c("#894843", "#887d74")
bg_color <- "#d7e0da"


g_bar <- 
  
  # aditional wrangling
  departures %>% 
  drop_na(ceo_dismissal) %>% 
  filter(coname %in% top_departure_firms) %>% 
  left_join(departure_firms_main_cause, by = "coname") %>% # to get nº of vol and invol. dep. in main data layer
  
  ggplot(aes(fyear)) + 
  
  # bars
  geom_bar_interactive(aes(y = 1, 
                           fill = as.factor(ceo_dismissal), 
                           tooltip = glue("Firm: {coname}\nCEO: {exec_fullname}\nYear: {fyear}\nMotive: {motive}"), 
                           data_id = coname), 
                       color = bg_color, 
                       stat = "identity", 
                       size = 1,
                       show.legend = F) + 
  # firm name text
  geom_text_interactive(aes(1993, 9.2, 
                            label = glue("Firm: {coname}"),
                            data_id = coname), 
                        color = bg_color, 
                        size = 2.5, 
                        hjust = "left", 
                        alpha = 0, 
                        family = "Georgia") + 
  
  # firm vol. and invol. departures text
  geom_text_interactive(
    aes(1993, 8.35, 
        label = glue("Voluntary departures: {voluntary}
                     Involuntary departures: {involuntary}"),
        data_id = coname),
    color = bg_color, 
    size = 2, 
    hjust = "left",
    alpha = 0, 
    family = "Georgia", 
    lineheight = 1) +

  
  labs(title = paste("CEO", "DEPARTURES", sep = "\t"), 
       subtitle = "CEO **<span style = 'color:#894843'>voluntary</span>** and 
       **<span style= 'color:#887d74'>involuntary</span>** departures 
       in the 20 *S&P 1500* firms with most CEO rotation between 1993 and 2018", 
       caption = "Data comes from Gentry et al. Facilitated by DatalsPlural. Visualization by Martín Pons | @MartinPonsM") + 
  
  scale_fill_manual(values = palette) + 
  scale_x_continuous(limits = c(1992, 2019), labels = c(2000, 2010), breaks = c(2000, 2010)) +
  
  theme_minimal_hgrid(12) +
  
  theme(
    text = element_text(color = "#1f3225", family = "Candara"),
    plot.title = element_text(hjust = 0.5),
    plot.subtitle = element_textbox(family = "Candara", size = 8),
    plot.caption = element_text(size = 6),
    plot.background = element_rect(fill = bg_color, color = bg_color),
    panel.background = element_rect(fill = bg_color, color = bg_color),
    axis.title = element_blank(),
    axis.text.y = element_blank(), 
    axis.ticks = element_blank(), 
    legend.position = "top") + 
  
  coord_equal()


# INTERACTIVITY  ----------------------------------------------------------


g_inter <- girafe(ggobj = g_bar)
  
g_inter %>% 
  girafe_options(opts_tooltip(opacity = 0.8, 
                              use_fill = T, 
                              use_stroke = F, 
                              css = "font-family: Candara;color:white"),
                 opts_hover_inv(css = "opacity:0.5"), 
                 opts_hover(css = "fill:#4c6061;"))


