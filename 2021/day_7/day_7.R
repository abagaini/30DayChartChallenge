
# 30DayChartChallenge: Day 7

# LIBRARIES ---------------------------------------------------------------

library(tidyverse)

# DATA --------------------------------------------------------------------

data <- read.csv("day7_mountains.csv") # data from Wikipedia https://en.wikipedia.org/wiki/List_of_mountains_by_elevation
data$metres <- as.numeric(sub(",", "", data$metres, fixed = TRUE))

# PLOT --------------------------------------------------------------------

theme_set(theme_void(base_family = "Avenir Next"))

p <- ggplot(data = data, aes(x=metres)) + 
  geom_area(stat = "bin", color = "#1f7a8c", fill = "#F0F7F4", bins = 30, size = 0.5) +
  labs(title = "Distribution of the Height of Mountains \n from the Wikipedia page: *List of Mountains by Elevation*", 
       x = "Height (metres)", y = "Count",
       caption = "Data from Wikipedia | 1613 mountains | @a_bagaini") +
  scale_x_continuous(breaks = seq(0,8850, by = 2000)) + 
  theme(text = element_text(colour = "#1f7a8c"),
        axis.title.x = element_text(face = "italic", size = "10", margin = margin(t = 20, r = 0, b = 40, l = 0)),
        # axis.title.y = element_text(face = "italic", size = "10", margin = margin(t = 0, r = 20, b = 0, l = 20), angle = 90), # optional
        axis.text.x = element_text(face = "italic", size = "8"),
        # axis.text.y = element_text(face = "italic", size = "8"), # optional
        plot.caption = element_text(face = "italic", hjust = 0.95,size = "7",  margin = margin(t = 0, r = 0, b = 10,l = 0)),
        plot.title = ggtext::element_markdown(hjust = 0.5,size = "18", margin = margin(t = 50, r = 0, b = 50, l = 0)),
        panel.background = element_rect(fill = "#F0F7F4", colour = "#F0F7F4"),
        plot.background = element_rect(fill = "#F0F7F4", colour = "#F0F7F4")) 



ggsave(p, file="plot.png", device="png",dpi = 300, width = 33, height = 15, units = "cm") # can adjust resolution


