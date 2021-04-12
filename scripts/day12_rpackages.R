
# 30DayChartChallenge: Day 12

# LIBRARIES ---------------------------------------------------------------

library(tidyverse)
library(ggtext)

# DATA --------------------------------------------------------------------

cran_code <- readr::read_csv("https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2019/2019-11-12/loc_cran_packages.csv") # data 
cran_code_g <- cran_code %>% filter(language =="R" & code > 100) %>% 
                                    group_by(code, pkg_name) %>% mutate(Frequency = n()) %>% ungroup() %>% distinct(code, Frequency,pkg_name)
cran_code_g <- rbind(cran_code_g,cran_code_g,cran_code_g,cran_code_g) # had to replicate data as lines are otherwise not very visible in the plot

# PLOT --------------------------------------------------------------------

theme_set(theme_void(base_family = "Helvetica"))

p <- ggplot(cran_code_g,
       aes(x = code, y = 1, fill = Frequency))+
  geom_tile()  + scale_colour_grey() +
  labs(x = "# of lines of R code", 
       title = "Number of Lines of R Code in R Packages*", 
       subtitle =  "Number of packages: 13'267    ||   Total number of lines of R code: 22'747'653    ||   Median number of lines of R code: 745",
       caption = "*selected packages with at least 100 lines of code \n Data from Philippe Massicotte & TidyTuesday | 30DayChartChallenge | @a_bagaini") +
  theme( legend.position = "none",
    axis.text.x = element_text(margin = margin(t = 5)),
    axis.title.x = element_text(face = "bold",margin = margin(t = 20, b = 40)),
    plot.caption = element_text(margin = margin(b = 20), hjust = 0.95,size = 8),
    plot.subtitle = element_text(face = "bold",margin = margin(t = 40, b = 40), hjust = 0.5,  color = "dark grey", size = 18),
    plot.title = element_text(face = "bold",margin = margin(t = 60), hjust = 0.5,  size = 24)) + 
  scale_fill_gradient(low = "black", high = "black") +
  scale_x_continuous(breaks = c(100,5000,10000,20000,50000,75000, 94000)) +
  
  # annotations
  annotate(geom = "text", x = 2400, y = 1.25, label = "tidyr (2'464)", hjust = "center", angle = 90, color = "light grey", fontface = 2) +
  annotate(geom = "text", x = 500, y = 0.85, label = "magrittr (390)", hjust = "center", angle = 270, color = "light grey", fontface = 2) + # needed to slight move label, otherwise it would not print well
  annotate(geom = "text", x = 26358, y = 0.95, label = "brms (26'358)", hjust = "center", angle = 90, color = "black") +
  annotate(geom = "text", x = 24856, y = 1.4, label = "metafor (24'856)", hjust = "center", angle = 90, color = "black") +
  annotate(geom = "text", x = 41225, y = 0.75, label = "lavaan (41'225)", hjust = "center", angle = 270, color = "black") +
  annotate(geom = "text", x = 74734, y = 1.2, label = "VGAM (74'734)", hjust = "center", angle = 270, color = "black") +
  
  annotate(
    geom = "curve", x = 19829, y = 1.5, xend = 21000, yend = 1.55, 
    curvature = .3, arrow = arrow(length = unit(2, "mm"))
  ) +  annotate(geom = "text", x = 19000, y = 1.57, label = "ggplot2 (19'829)", hjust = "left") +
  
  annotate(
    geom = "curve", x = 93275, y = 1.5, xend = 90000, yend = 1.55, 
    curvature = .1, arrow = arrow(length = unit(2, "mm"))
  ) + annotate(geom = "text", x = 86000, y = 1.57, label = "spatstat \n (93'275)", hjust = "left") +

  annotate(
    geom = "curve", x = 15387, y = 1, xend = 16000, yend = 1.02, 
    curvature = .3, arrow = arrow(length = unit(2, "mm"))
  ) +  annotate(geom = "text", x = 16500, y = 1.05, label = "dplyr \n (15'387)", hjust = "center") +
  
  annotate(
    geom = "curve", x = 4461, y = 1.5, xend = 4200, yend = 1.53, 
    curvature = .1, arrow = arrow(length = unit(2, "mm"))
  ) +   annotate(geom = "text", x = 4261, y = 1.55, label = "purrr (4'461)", hjust = "center", angle = 0, color = "black") +
  
   annotate(
    geom = "segment", x = 12949, y = 1.3, xend = 14000, yend = 1.35, arrow = arrow(length = unit(2, "mm"))
  ) +  annotate(geom = "text", x = 14000, y = 1.37, label = "lme4 (12'949)", hjust = "center")  +
  
  annotate(
    geom = "curve", x = 58247, y = 0.95, xend = 57000, yend = 1, 
    curvature = .3, arrow = arrow(length = unit(2, "mm"))
  ) + annotate(geom = "text", x = 54500, y = 0.99, label = "OpenMx \n (58'247)", hjust = "center") 
  
  # save
ggsave(p, file="day12.png", device="png",dpi = 600, width = 40, height = 28 , units = "cm") # can adjust resolution
  

