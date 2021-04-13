
# 30DayCharChallenge: Day 10 & 11



# LIBRARIES ---------------------------------------------------------------

library("tidyverse")
library("ggrepel")


# DATA --------------------------------------------------------------------


options(scipen=999)
data <- read.csv("vHoneyNeonic_v03.csv") # from https://www.kaggle.com/jessicali9530/honey-production


data <- data %>% filter(year == 2017) 

data$rand_x = runif(nrow(data),0,0.5)
data$rand_y = runif(nrow(data),0,0.5)

# PLOT --------------------------------------------------------------------


theme_set(theme_void(base_family = "Avenir Next"))

p <- data %>%  ggplot(aes(x=rand_x, y = rand_y, size = numcol, alpha = yieldpercol))+ geom_point(shape = 21, fill = "black", color = "black") + 
 scale_alpha_continuous(range = c(0.5, 1))+
  labs( title =  "Honey Production in the USA in 2017",
   subtitle= "Showing data from 40 states in the USA. One circle corresponds to one state.\nSize of the circle corresponds to the number of honey producing colonies.\nThe level of transparency corresponds to the honey yield (in pounds) per colony.",
    size = "Number of honey producing colonies", alpha = "Honey yield (pounds) per colony",
   caption = "Data from kaggle / National Agricultural Statistics Service (NASS) | Postion of circles randomly generated | @a_bagaini") +
  theme(plot.background = element_rect("#fdc500"),
        plot.margin = unit(c(t = 2,b = 2, r = 2,l = 2), "cm"),
        plot.title = element_text(size = 40, face = "bold", margin = margin(b = 20)),
        plot.subtitle = element_text(lineheight = 0.95, size = 18, margin = margin(b = 50)),
        plot.caption = element_text(margin = margin(b = 20), hjust = 0.95,size = 11, color = "black"),
        legend.position = "bottom",
        legend.title.align =0.5,
        legend.margin = margin(t = 50, b = 10, l = 70, r = 70),
        legend.spacing.x = unit(0.5, 'cm'),
        legend.text = element_text(size = 10),
        legend.title = element_text(hjust = 0.5, face = "bold", size = 10, margin = margin(b = 0, t = 0))) +
  guides(alpha = guide_legend(override.aes = list(size = 20), title.position = "top", label.position = "bottom", nrow = 1),
         size = guide_legend(title.position = "top", label.position = "bottom", nrow = 1, override.aes = list(fill = NA, stroke = 1.5)))+
  ylim(0,0.5) + xlim(0, 0.5) +
  scale_size_area(max_size = 35) +
 geom_text_repel(aes(label = state),
         size = 4.5, show.legend = F) 

# save
ggsave(p, file="day10_11.png", device="png",dpi = 600, width = 30, height = 30, units = "cm") # can adjust resolution
