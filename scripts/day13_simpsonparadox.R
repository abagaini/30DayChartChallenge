
#3ODayChartChallenge Day 13: Correlations
#@a_bagaini


# LIBRARIES ---------------------------------------------------------------
library(ggplot2)
library(correlation)
library(patchwork)
library(png)
library(cowplot)

#font from:https://fonts.google.com/specimen/Pacifico


# DATA --------------------------------------------------------------------

data <- simulate_simpson(n = 250, groups = 5, r = -0.45, difference = 2)

# PLOT --------------------------------------------------------------------

# create the plot with the gathered groups
a <- ggplot(data, aes(x = V1, y = V2)) +
  geom_point(fill = "#D6E69F", shape = 21, size = 2.75, alpha = 0.95, colour = "black", stroke = 1.5) +
  geom_smooth(method = "lm", color = "black", size = 2) +
  theme_void() +
  labs( x = "Variable A", y = "Variable B") +
  theme(axis.line = element_line(color = "black", size = 1.5)) +
  theme(plot.margin = unit(c(t = 2,b = 2, r = 2,l = 2), "cm"),
        axis.title.x = element_text(hjust = 0.5,size = 18, color = "black", family = "Pacifico Regular", margin = margin(t = 20)),
        axis.title.y = element_text(hjust = 0.5,size = 18, angle = 90, color = "black", family = "Pacifico Regular", margin = margin(r = 20))) +
  annotate(
    geom = "curve", x = 3, y = 6, xend = 8, yend = 16, size = 1.15, colour = "black",
    curvature = -.3, arrow = arrow(length = unit(2, "mm"))
  ) +  annotate(geom = "text", x = 8.1, y = 16.1, label = "Need to be careful when\nignoring subgroups of data!", hjust = "left", family = "Pacifico Regular", size = 5) 

a

# create plot with the separated groups
b <- ggplot(data, aes(x = V1, y = V2)) +
  geom_point(aes(fill = Group), shape = 21, size = 2.75, alpha = 0.85, colour = "black", stroke = 1.5) +
  geom_smooth(aes(color = Group), method = "lm") +
  theme_void() +
  labs( x = "Variable A", y = "Variable B") +
  theme(axis.line = element_line(color = "black", size = 1.5)) +
  theme(plot.margin = unit(c(t = 2,b = 2, r = 2,l = 2), "cm"),
        axis.title.x = element_text(hjust = 0.5,size = 18, color = "black", family = "Pacifico Regular", margin = margin(t = 20)),
        axis.title.y = element_text(hjust = 0.5,size = 18, angle = 90, color = "black", family = "Pacifico Regular", margin = margin(r = 20))) +
  scale_colour_manual(values = c("#FED41D", "#009DDC", "#F14E28", "#00947E", "#ff499e")) +
  scale_fill_manual(values = c("#FED41D", "#009DDC", "#F14E28", "#00947E", "#ff499e")) +
  guides(fill = guide_legend(override.aes = list(size = 5)))+
  guides(fill=guide_legend(override.aes=list(fill=NA)))
        
b
                

#save the legend
legend <- get_legend(
  # create some space to the left of the legend
  b + theme(legend.box.margin = margin(0, 0, 0, 12)))


# create a composite plot with the legend
 c <- plot_grid(a, b + theme(legend.position = "none", plot.title = element_blank(), plot.subtitle = element_blank()), align = "v")
 c <- plot_grid(c, legend, rel_widths = c(3, .4))

#load images
img <- readPNG("marge.png", native = T) # from https://www.pngitem.com/middle/mRwRbo_marge-simpson-homer-simpson-bart-simpson-lisa-simpson/
img2 <- readPNG("donut.png", native = T) # from https://e7.pngegg.com/pngimages/107/184/png-clipart-donuts-homer-simpson-frosting-icing-coffee-and-doughnuts-cartoon-unicorn-donut-purple-face.png
img3 <- readPNG("lisa.png", native = T) # from


# adding titles to the composite plot
patchwork <- c
d <- patchwork + plot_annotation(
  title = "Simpson's Paradox",
  subtitle = "Simpson’s Paradox is a statistical phenomenon where an association between two variables in a population emerges, disappears or reverses when the population is divided into subpopulations.
  For instance, two variables may be positively associated in a population, but be independent or even negatively associated in all subpopulations. Edward H. Simpson first addressed this
  phenomenon in a  technical paper  in 1951, but Karl Pearson et al. in 1899 and Udny Yule in 1903, had mentioned  a similar effect earlier.",
  caption = "Text: plato.stanford.edu and Pearl, 2013 |Images: pngimage.net, pngegg.com and pngimage.com | Data generated using {correlation} | #30DayChartChallenge | @a_bagaini",
  theme = theme(plot.background = element_rect("#ffd90f"))) & 
  theme(plot.title = element_text(size = 85, face = "bold", margin = margin(b = 20, t = 30), hjust = 0.5, family = "Pacifico Regular", color = "#107DC0"),
         plot.subtitle = element_text(lineheight = 1.25, size = 15, margin = margin(b = 20, t = 20),hjust = 0.5, family = "Pacifico Regular", colour = "black"),
        plot.caption = element_text(size = 9, hjust = 0.99, margin = margin(b =10), family = "Pacifico Regular", colour = "black"))

# adding images to the composite plot
p <- ggdraw() + 
  draw_plot(d) +
  draw_image(
    img, x = 1, y = 1, hjust = 3, vjust = 1.3, halign = 0.25, valign = 0.5,
    width = 0.22
  ) + # marge
  draw_image(
    img2, x = 1, y = 1, hjust = 0.5, vjust = 0.95, halign = 1, valign = 1,
    width = 0.2
  )+ # donut
  draw_image(
    img3,x = 1, y = 1, hjust = 10, vjust = 0.65, halign = 0.25, valign = 0.5,
    width = 0.1
  ) # lisa


p

# save
# ggsave(p, file="chartchall_d13.png", device="png",dpi = 500, width = 50, height = 30, units = "cm") # can adjust resolution
