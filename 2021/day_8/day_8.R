# 30DayChartChallenge: Day 8

# LIBRARIES ---------------------------------------------------------------

library(tidyverse)


# DATA --------------------------------------------------------------------

data <- read.csv("day8_lepidosaur.csv") # renamed some columns but original dataset from https://onlinelibrary.wiley.com/doi/full/10.1111/geb.12398
data$mass <- log(as.numeric(data$mass)) # transform
data <- data %>% select(mass, Family)  # select relevant variables
data$rand <- "rand"
data <- data %>% filter(!is.na(mass))


# PLOT --------------------------------------------------------------------

theme_set(theme_void(base_family = "Avenir Next"))

p <- ggplot(data, aes(y=rand, x=mass, fill = Family)) + 
  geom_jitter(shape = 23, size = 3, height = 0.5, alpha = 0.75, color = "black", stroke = .75) +
  theme(legend.position = "bottom") + 
  labs(x = "log(mass in grams)", title = "Body Mass Distribution of Lepidosauria",
       caption = "Data from Feldman et al., 2015 doi.org/10.1111/geb.12398 | 9'805 species | @a_bagaini") +
  theme(text = element_text(colour = "#013220"),
        axis.title.x = element_text(face = "italic", size = 10, margin = margin(t = 20, r = 0, b = 20, l = 0)),
        axis.text.x = element_text(face = "italic", size = 8, margin = margin(t = 10, r = 0, b = 0, l = 0)),
        panel.grid.major.x = element_line(color = "light grey", size = 0.5),
        panel.grid.minor.x = element_line(color = "light grey", size = 0.25),
        legend.title  = element_text(face = "bold", margin = margin(t = 0, r = 0, b = 10, l = 0)),
        plot.caption = element_text(face = "italic", hjust = 0.95,size = 7,  margin = margin(t = 0, r = 0, b = 10,l = 0)),
        plot.title = ggtext::element_markdown(hjust = 0.5,size = "18", margin = margin(t = 70, r = 0, b = 70, l = 0)),
        panel.background = element_rect(fill = "#fdf8e1", colour = "#fdf8e1"),
        plot.background = element_rect(fill = "#fdf8e1", colour = "#fdf8e1"),
        legend.box.margin = margin(t = 20, r = 0, b = 50, l = 0),
        legend.title.align= 0.5,
        legend.text = element_text(size = 8),
        legend.spacing.x = unit(0.35, 'cm'),
        legend.key.size = unit(0.15,"line")) +
  scico::scale_fill_scico_d(palette = 'bamako') +
  scale_x_continuous(breaks = c(-2,0,2,4,6,8,10,12)) +
  guides(fill = guide_legend(nrow = 6, title.position = "top"))



p

ggsave(p, file="test.png", device="png",dpi = 300, width = 50, height = 20, units = "cm") # can adjust resolution
