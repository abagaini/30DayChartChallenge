
# day 19: Global Change

# PACKAGES ---------------------------------------------------------------

library(tidyverse)
library(cowplot)



# DATA --------------------------------------------------------------------

data_energy <- read_csv("energy-consumption-by-source-and-region.csv") # from https://ourworldindata.org/energy-production-consumption
data_energy_w <- data_energy %>% filter(Entity == "World") 
colnames(data_energy_w) <- c("Region", "Code", "year", "Oil", "Gas", "Coal", "Solar", "Hydro", "Nuclear", "Wind", "Biomass", "Biofuel")
data_energy_w <- data_energy_w %>% pivot_longer(cols = Oil:Biofuel, names_to = "source", values_to = "twh" ) %>% 
  mutate(twh = twh/1000)

data_energy_w_fossil <- data_energy_w %>% filter(source %in% c("Oil", "Gas", "Coal")) %>% filter(!is.na(twh) & year > 1964)
data_energy_w_renew <- data_energy_w %>% filter(source %in% c("Solar", "Hydro", "Wind", "Biomass", "Biofuel")) %>% filter(!is.na(twh) & year > 1964)



# PLOT --------------------------------------------------------------------

# upper plot
a <- ggplot(data=data_energy_w_renew, aes(x= year, y=twh, group = source))  +
  geom_area(size=1, colour="#343a40", fill = "#f2e9e4") + 
  theme_void()+
  labs(y = "terawatt-hour")+
  theme(plot.background = element_rect(fill = "#f2e9e4"),
        title = element_text(colour = "#343a40"),
        text = element_text(colour = "#343a40"),
        axis.line=element_blank(),
        axis.text = element_blank(),
        axis.ticks=element_blank(),
        axis.title = element_blank(),
        panel.border=element_blank(), 
        panel.spacing = unit(0, "cm"),
        panel.grid.major=element_blank(),
        panel.grid.minor=element_blank(),
        plot.margin = margin(0, 0, 0, 0, "cm"),
        # axis.text.x =  element_text(size = 30, family = "Bebas Neue", vjust = 80),
        axis.title.y =  element_text(size = 35, family = "Bebas Neue", angle = 90, hjust = 0.01, margin = margin(r = 10, l = 10)),
        axis.text.y =  element_text(size = 25, family = "Bebas Neue"))+
  scale_x_continuous(breaks = seq(1960, 2020, by = 10)) +
  scale_y_continuous(breaks = seq(20, 140, by = 20), limits = c(0,140),expand = expansion(mult = c(0, 0))) +
  guides(fill = guide_legend(title.position = "top",  label.position = "bottom", title.hjust = 0.5)) +
  
 
  annotate(geom = "text", x = 1965, y = 115, label = 
             "Global energy
consumption", hjust = "left", color = "#343a40", lineheight = 0.65,
           family = "Bebas Neue", size = 65, fontface = "bold") +
  annotate(geom = "text", x = 1965, y = 75, label = 
             "Renewables vs. 
Fossil Fuels", hjust = "left", color = "#343a40",lineheight = 0.65,
           family = "Bebas Neue", size = 20, fontface = "italic") +
  annotate(geom = "text", x = 1965, y = 65, label = "(1965-2019)", hjust = "left", color = "#343a40",lineheight = 0.55,
           family = "Bebas Neue", size = 15, fontface = "italic") +
  annotate(geom = "text", x = 1985, y = 87,
           label =
             "In 2019, 84.3% of global primary energy
came  from fossil fuels (i.e., coal, gas, and oil),
11.4% came from renewables   (i.e., solar, 
hydropower, wind, biofuels, and biomass) and 
4.3% from nuclear.  These  differences are 
alarming as the burning of  fossil fuels 
for  energy produces 3/4 of the world's
greenhouse  gas emissions. To  reduce this 
consumption, we need to decarbonize and 
switch to cleaner  sources  of energy. Over
the  years we've   been burning  even more
fossil fuels, but  energy from renewable 
sources is slowly  rising. These two plots
show   global energy consumption  by  source
(in thousand  terawatt-hour) between 1965 and
2019, and compares renewables (up) and 
fossil fuels (down).", hjust = "left", vjust = "top",color = "#343a40", lineheight = 0.85,
           family = "Montserrat Italic", size = 5.8) +
  annotate(x = 2016, y = 15, xend = 2014, yend = 18, size = 0.5, colour = "#343a40",geom = "segment") +
  annotate(geom = "text", x = 2014, y = 19.5, label = "Biofuel", hjust = "centre", color = "#343a40",
           family = "Bebas Neue", size = 5, fontface = "bold") +
  annotate(x = 2012, y = 11.5, xend = 2010, yend = 15.5, size = 0.5, colour = "#343a40",geom = "segment") +
  annotate(geom = "text", x = 2008, y = 17, label = "Biomass & Other", hjust = "left", color = "#343a40",
           family = "Bebas Neue", size = 5, fontface = "bold") +
  annotate(geom = "text", x = 2002, y = 4, label = "Hydropower", hjust = "left", color = "#343a40",
           family = "Bebas Neue", size = 15, fontface = "bold") +
  annotate(x = 2018, y = 3.8, xend = 2017, yend = 20.5, size = 0.5, colour = "#343a40",geom = "segment") +
  annotate(geom = "text", x = 2017, y = 22, label = "Solar", hjust = "right", color = "#343a40",
           family = "Bebas Neue", size = 5, fontface = "bold") +
  annotate(geom = "text", x = 2019, y = 1.25, label = "Wind", hjust = "right", color = "#343a40",
           family = "Bebas Neue", size = 5, fontface = "bold")


#lower plot
b <- ggplot(data=data_energy_w_fossil, aes(x= year, y=twh, group = source))  +
  geom_area(size=1, colour="#f2e9e4", fill = "#343a40") + #6c757d
  theme_void()+
  labs(y = "Thousand",
       caption = "Text adapted from ourworldindata.org | Data from ourworldindata.org | #30DayChartChallenge | @a_bagaini")+
  theme(plot.background = element_rect(fill = "#343a40"),
        title = element_text(colour = "#f2e9e4"),
        text = element_text(colour = "#f2e9e4"),
        plot.caption =  element_text(size = 15, family = "Bebas Neue", hjust = 0.98, margin = margin(t = 10, b = 10), colour = "#f2e9e4"),
        axis.title.y =  element_text(size = 35, family = "Bebas Neue", angle = 90, hjust = 0.93, margin = margin(r = 10, l = 10)),
        axis.text.y =  element_text(size = 25, family = "Bebas Neue"),
        axis.line=element_blank(),
        axis.text = element_blank(),
        axis.ticks=element_blank(),
        axis.title = element_blank(),
        panel.border=element_blank(), 
        panel.spacing = unit(0, "cm"),
        panel.grid.major=element_blank(),
        panel.grid.minor=element_blank(),
        plot.margin = margin(-1.35, 0,0, 0, "cm"),
        legend.position = "right",
        legend.margin = margin(r = 10, l = 10),
        legend.spacing.x = unit(2.0, 'cm'))+
  scale_x_continuous(breaks = seq(1960, 2020, by = 10), position = "top") +
  scale_y_reverse(breaks = seq(20, 140, by = 20)) +
  guides(fill = guide_legend(title.position = "top",  label.position = "bottom", title.hjust = 0.5)) +
  annotate(geom = "text", x = 2018, y = 105, label = "Coal", hjust = "right", color = "#f2e9e4",
           family = "Bebas Neue", size = 50, fontface = "bold") +
  annotate(geom = "text", x = 2018, y = 67, label = "Gas", hjust = "right", color = "#f2e9e4",
           family = "Bebas Neue", size = 55, fontface = "bold") +
  annotate(geom = "text", x = 2018, y = 25, label = "Oil", hjust = "right", color = "#f2e9e4",
           family = "Bebas Neue", size = 60, fontface = "bold") +
  
  # adding manually x axis (some issues using a shifted x.axis)
  annotate(geom = "text", x = 1970, y = 5, label = 
             "1970", hjust = "center", color = "#f2e9e4", lineheight = 0.65,
           family = "Bebas Neue", size = 9, fontface = "bold") +
  annotate(geom = "text", x = 1990, y = 5, label = 
             "1990", hjust = "center", color = "#f2e9e4", lineheight = 0.65,
           family = "Bebas Neue", size = 9, fontface = "bold") +
  annotate(geom = "text", x = 2010, y = 5, label = 
             "2010", hjust = "center", color = "#f2e9e4", lineheight = 0.65,
           family = "Bebas Neue", size = 9, fontface = "bold") 

# merge
c <- cowplot::plot_grid(a,b, ncol = 1)

# save
ggsave(c, file="day_19.png", device="png",dpi = 400, width = 40, height = 60, units = "cm") # can adjust resolution




