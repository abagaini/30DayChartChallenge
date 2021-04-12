
# 30DayCharChallenge: Day 9

# LIBRARIES ---------------------------------------------------------------


library(tidyverse)


# DATA --------------------------------------------------------------------

data <- read.csv("day9_cranpkg.csv")
data <- data %>%  filter(!is.na(package))
data$description <- str_to_lower(data$description)
data$package <- str_to_lower(data$package)

pat <-c("statistic")
r <-grep(pat, data$description)
data_c <- data[r,]
names <- paste(data_c$package, collapse = "")
character <- strsplit(names, "")
dt <- as.data.frame(table(character))
 
 
 

# PLOT --------------------------------------------------------------------

theme_set(theme_void(base_family = "Avenir Next", base_size = 24))
p <- dt %>% ggplot(aes(x= character, y = 1, shape = character, size = Freq))+ geom_point(color = "white") + 
                scale_shape_manual(values=c("." = 46, "0" = 48, "1" = 49, "2" = 50,"3" = 51,"4" = 52,
                                            "5" = 53, "6" = 54, "7" = 55, "8" = 56, "9" = 57,
                                            "a" = 97, "b" = 98, "c" = 99, "d" = 100, "e" = 101,
                                            "f" = 102, "g" = 103, "h" = 104, "i" = 105, "j" = 106,
                                            "k" = 107, "l" = 108, "m" = 109, "n" = 110, "o" = 111, "p" = 112,
                                            "q" = 113, "r" = 114, "s" = 115, "t" = 116, "u" = 117, "v" = 118, "w" = 119, "x" = 120,
                                            "y" = 121, "z" = 122)) +
  labs(title = "**Which characters appear the most often in the names of R packages?**",
       subtitle = "Focus on packages with the word *statistic* mentioned in their description", 
       caption = "Data from CRAN | 584 packages, 4585 characters | @a_bagaini") +
  theme(plot.background = element_rect(fill = "black"), 
        legend.text = element_text(colour = "white", size = 14),
       legend.margin = margin(b = 40, t = 50),
      plot.title = ggtext::element_markdown(margin = margin(t = 60, b = 20), colour = "white", hjust = 0.5),
       plot.caption = element_text(margin = margin(b = 20), hjust = 0.95,size = 12, color = "white"),
        plot.subtitle = ggtext::element_markdown(margin = margin(b = 50),colour = "white", hjust = 0.5),
      legend.position = "bottom") + guides(shape = F, size = guide_legend(nrow = 1, label.position = "bottom")) +
  scale_size_area(max_size = 25,breaks= c(seq(0,100, by = 25), seq(200,500, by = 100)))


# save
ggsave(p, file="day9.png", device="png",dpi = 600, width = 40, height = 25, units = "cm") # can adjust resolution
