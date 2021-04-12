# big_mac.R

library(tidyverse)
library(lubridate)
library(ggbeeswarm)
library(Cairo)
library(ggthemes)
library(ggtext)

tuesdata <- tidytuesdayR::tt_load(2020, week = 52)

# dollar_ex no es el tipo de cambio. Es el mismo índice

big_mac <- tuesdata$`big-mac`


big_mac %>% 
  ggplot(aes(dollar_ex, local_price)) + 
  geom_point()


big_mac %>% 
  filter(dollar_ex < 100, year(date) == 2015) %>% 
  ggplot(aes(dollar_ex, usd_raw)) + 
  geom_point()

View(big_mac)

CairoWin()

theme_set(theme_tufte())
big_mac %>% 
  filter(date == "2020-07-01") %>% 
  ggplot(aes(x = usd_raw, y = 0)) + 
  geom_vline(xintercept = 0, color = "#655a4d") + 
  geom_quasirandom(size = 3.8, aes(color = usd_raw > 0), 
                   groupOnX = FALSE, show.legend = F) + 
  annotate("text", label = "Undervalued",
           x = -0.01, 
           y = 0.49, 
           hjust = "right", 
           color = "#255f8f", 
           size = 5) + 
  annotate("text", label = "Overvalued",
           x = 0.01, 
           y = 0.49, 
           hjust = "left", 
           color = "#de425b", 
           size = 5) + 
  
  labs(title = "The euro <span style='color:#255f8f'>is 16 % undervalued</span> against the US dollar") +

  scale_y_continuous(limits = c(-0.8, 0.5)) + 
  scale_x_continuous(limits = c(-0.4, 0.4)) + 
  scale_color_manual(values = c("#255f8f", "#de425b")) + 
  theme(axis.ticks.y = element_blank(), 
        axis.title.y = element_blank(), 
        axis.text.y = element_blank(), 
        plot.title = element_markdown())

# versión animada del grafico de the economist???


# SCATTER -----------------------------------------------------------------


big_mac %>% 
  ggplot(aes(gdp_dollar, usd_raw)) + 
  geom_point(size = 0.8)
  
  

# EVOLUCION DE EURO -------------------------------------------------------

theme_set(theme_bw())
big_mac %>% 
  filter(iso_a3 %in% c("EUZ", "GBR")) %>%  
  ggplot(aes(date, usd_raw)) + 
  geom_line(aes(group = iso_a3)) + 
  theme(panel.border = element_blank(), 
        axis.line = element_line())

# podemos juntarlo con barras de crecimiento de gdp per cáipta??

