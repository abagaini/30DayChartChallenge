
#30DayChartChallenge: Tiles


# LIBRARIES ---------------------------------------------------------------

library(tidyverse)

# DATA --------------------------------------------------------------------

data <- read.csv("share-of-the-population-with-access-to-electricity.csv") # from https://ourworldindata.org/energy-access

# remove world regions and just keep countries that have data from 1990-2016
data_c <- data  %>% rename(access = Access.to.electricity....of.population., country = Entity)  %>% 
  filter(!(Code %in% c("", "OWID_WRL", "OWID_CIS", "OWID_KOS"))) %>%
  group_by(Code) %>% mutate(n_data = n(), mean_access = mean(access)) %>%
  ungroup() %>% filter(n_data == 27) 

# there are 201 countries, but to have a 10 x 20 display I remove one, "Micronesia (country)"  
data_c <- data_c  %>% filter(country !="Micronesia (country)") %>%  
  # make names shorter otherwise cannot print well
  mutate(country = case_when(country == "United States Virgin Islands" ~ "US. Virgin Islands",
                             country == "Saint Kitts and Nevis" ~ "St.Kitts & Nevis", 
                             country ==  "Saint Martin (French part)"~ "St.Martin (FR part)",
                             country ==  "Sint Maarten (Dutch part)" ~ "St.Maarten (NL part)",
                             country ==  "Saint Vincent and the Grenadines"~ "St.Vincent & the Grenadines",
                             country == "Democratic Republic of Congo" ~ "Dem. Rep. of Congo",
                             TRUE ~country))

# world data
data_w <- data  %>% filter(Code == "OWID_WRL") %>% 
  rename(access = Access.to.electricity....of.population., country = Entity) 

# PLOTS -------------------------------------------------------------------

 
yellow = "#ffea00"


# countries sorted alphabetically
p1 <- data_c   %>%  
  ggplot(aes(y=country, x=Year, fill=access, group = country)) + 
  geom_tile(color="grey15", size=0.15) + 
  labs(caption = "Text adapted from ourworldindata.org | Data from ourworldindata.org | #30DayChartChallenge | @a_bagaini",
    title =  "COULD SOMEONE PLEASE TURN ON THE LIGHTS?", fill = "% electricity access",
       subtitle = 
"These tile plots show the percentage of the population with access to electricity between 1990 & 2016 (data from 200 countries)\n
At a global level, the percentage of people with access to electricity has been steadily increasing over the last few decades. In 1990, around 71% of the world's population had
access; this has increased to 87% in 2016. In 1990, for quite a few countries, close to 100% of the population had access to electricity (e.g., European countries). However, looking
at 2016, this was still far from being the case for certain countries, such as in Sub-Saharan Africa. For countries, like Nepal or Bhutan, access to electricity has considerably 
increased over the years.\n
INTERPRETING THE PLOTS: Each tile plot shows starting from 1990 (leftmost stripe) to 2016 (rightmost stripe), how the access to electricity in a country has changed.") +
  theme_void () +
  theme(plot.background = element_rect(fill = "grey15"),
        # legend.position = "none",
        legend.position = "top",
        plot.margin = margin(r = 20, l = 20),
        legend.justification = "center",
        plot.caption =  element_text(size = 16, family = "Montserrat Bold", hjust = 0.98, margin = margin(t = 10, b = 10), colour = yellow),
        legend.box.margin = margin( t = 5, b = 25),
        panel.spacing.y=unit(1.1,"lines"),
        plot.title  = element_text(size = 74, color = yellow, family = "Montserrat Black", margin = margin(t = 20, b = 20)),
        legend.text =  element_text(size = 20, color = yellow, family = "Montserrat Medium"),
        legend.title =  element_text(size = 20, color = yellow, family = "Montserrat Bold"),
        plot.subtitle  = element_text(size = 22, color = yellow, family = "Montserrat Medium", margin = margin(t = 0, b = 20), lineheight = 0.9),
        strip.text.x = element_text(size = 14, color = yellow, family = "Montserrat Bold"))+
  facet_wrap(country~., scales = "free", nrow = 20) +
 guides(fill = guide_colorbar(title.position = "top", title.hjust = 0.5, barwidth = 20)) +
  scale_fill_gradient(low = "grey15", high = yellow, limits = c(0,100))

ggsave(p1, file="chartchall_d23_1.png", device="png",dpi = 250, width = 70, height = 85, units = "cm") # can adjust resolution



# gradient

# order country factors according to mean access
data_c$country <- factor(data_c$country)
data_c$country <- fct_reorder(data_c$country, data_c$mean_access, min)

p2 <- data_c   %>%  
  ggplot(aes(y=country, x=Year, fill=access, group = country)) + 
  geom_tile(color="grey15", size=0.15) + 
  labs(caption = "Text adapted from ourworldindata.org | Data from ourworldindata.org | #30DayChartChallenge | @a_bagaini",
       title =  "COULD SOMEONE PLEASE TURN ON THE LIGHTS?", fill = "% electricity access",
       subtitle = 
         "These tile plots show the percentage of the population with access to electricity between 1990 & 2016 (data from 200 countries)\n
At a global level, the percentage of people with access to electricity has been steadily increasing over the last few decades. In 1990, around 71% of the world's population had
access; this has increased to 87% in 2016. In 1990, for quite a few countries, close to 100% of the population had access to electricity (e.g., European countries). However, looking
at 2016, this was still far from being the case for certain countries, such as in Sub-Saharan Africa. For countries, like Nepal or Bhutan, access to electricity has considerably 
increased over the years.\n
INTERPRETING THE PLOTS: Each tile plot shows starting from 1990 (leftmost stripe) to 2016 (rightmost stripe), how the access to electricity in a country has changed.") +
  theme_void () +
  theme(plot.background = element_rect(fill = "grey15"),
        # legend.position = "none",
        legend.position = "top",
        plot.margin = margin(r = 20, l = 20),
        legend.justification = "center",
        plot.caption =  element_text(size = 16, family = "Montserrat Bold", hjust = 0.98, margin = margin(t = 10, b = 10), colour = yellow),
        legend.box.margin = margin( t = 5, b = 25),
        panel.spacing.y=unit(1.1,"lines"),
        plot.title  = element_text(size = 74, color = yellow, family = "Montserrat Black", margin = margin(t = 20, b = 20)),
        legend.text =  element_text(size = 20, color = yellow, family = "Montserrat Medium"),
        legend.title =  element_text(size = 20, color = yellow, family = "Montserrat Bold"),
        plot.subtitle  = element_text(size = 22, color = yellow, family = "Montserrat Medium", margin = margin(t = 0, b = 20), lineheight = 0.9),
        strip.text.x = element_text(size = 14, color = yellow, family = "Montserrat Bold"))+
  facet_wrap(country~., scales = "free", nrow = 20) +
  guides(fill = guide_colorbar(title.position = "top", title.hjust = 0.5, barwidth = 20)) +
  scale_fill_gradient(low = "grey15", high = yellow, limits = c(0,100))

ggsave(p2, file="chartchall_d23_2.png", device="png",dpi = 250, width = 70, height = 85, units = "cm") # can adjust resolution



# tile close up
p3 <- data_w   %>%  
  ggplot(aes(y=country, x=Year, fill=access)) + 
  geom_tile(color="grey15", size=0.75) + 
  labs(caption = "Data from ourworldindata.org | #30DayChartChallenge | @a_bagaini",
       title =  
"ACCESS TO ELECTRICITY AT THE
GLOBAL LEVEL", fill = "% electricity access",
       subtitle = "(1990 - 2016)") +
  theme_void () +
  theme(plot.background = element_rect(fill = "grey15"),
        legend.position = "top",
        plot.margin = margin(r = 40, l = 40),
        legend.justification = "center",
        plot.caption =  element_text(size = 8, family = "Montserrat Medium", hjust = 0.98, margin = margin(t = 30, b = 10), colour = yellow),
        legend.box.margin = margin( t = 5, b = 15),
        plot.title  = element_text(size = 35, color = yellow, family = "Montserrat Black", margin = margin(t = 20, b = 20)),
        legend.text =  element_text(size = 10, color = yellow, family = "Montserrat Medium"),
        legend.title =  element_text(size = 10, color = yellow, family = "Montserrat Bold"),
        axis.text.x  =  element_text(size = 12, color = yellow, family = "Montserrat Bold"),
        plot.subtitle  = element_text(size = 18, color = yellow, family = "Montserrat", margin = margin(t = 0, b = 30), lineheight = 0.8))+
  guides(fill = guide_colorbar(title.position = "top", title.hjust = 0.5, barwidth = 20)) +
  scale_x_continuous(breaks = seq(1990,2016, by = 2))+
  annotate(geom = "text", x = 1988, y = 1.5, label = "71.4% in 1990", size = 5, color = yellow, family = "Montserrat", vjust = "bottom", hjust = "left") +
  annotate(geom = "text", x = 2018, y = 1.5, label = "87.4% in 2016", size = 5, color = yellow, family = "Montserrat", vjust = "bottom", hjust = "right") +
  scale_fill_gradient(low = "grey15", high = yellow, limits = c(0,100))

ggsave(p3, file="chartchall_d23_3.png", device="png",dpi = 400, width = 25, height = 15, units = "cm") # can adjust resolution


