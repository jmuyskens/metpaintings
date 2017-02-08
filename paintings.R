library(ggplot2)
library(dplyr)
libyary(tidyr)
library(grid)
library(stringr)
library(png)

download.file(url="https://media.githubusercontent.com/media/metmuseum/openaccess/master/MetObjects.csv", destfile="MetObjects.csv", method="curl")

standing <- readPNG("standing.png")

met <- read.csv("MetObjects.csv")

paintings <-
  MetObjects %>%
  filter(Classification == "Paintings") %>%
  mutate(dim=gsub("\\(|\\)| cm", "", str_extract(Dimensions, "\\((.+) cm\\)"))) %>% 
  separate(dim, c("dim_h", "dim_w"), sep = " x ") %>%
  mutate(
    dim_h = as.numeric(dim_h), 
    dim_w = as.numeric(dim_w),
    area = dim_h * dim_w) %>%
  arrange(desc(area))

g <- ggplot(paintings, aes(xmin=0, xmax=dim_w, ymin=0, ymax=dim_h)) + 
  geom_rect(fill = "maroon", color = "white", alpha = 0.1, size=0.1) + 
  theme_minimal() + 
  coord_fixed(ratio = 1) + 
  labs(x="Width in cm.", y="Height in cm.") + 
  ggtitle("The paintings of the Met") + 
  annotate("text", x = 40, y = 640, hjust = 0, label="Japanese handscroll") + 
  annotate("text", x = 330, y = 550, hjust = 0, label = "The Triumph of Marius") + 
  annotate("text", x = 390, y = 440, hjust = 0, label = "The Battle of Vercellae, The Capture of Carthage") + 
  annotate("text", x = 660, y = 365, hjust = 0, label = "Washington Crossing the Delaware") + 
  annotate("text", x = 945, y = 260, hjust = 0, label = "Taxi Cab III") + 
  annotate("text", x = 2000, y= 150, hjust = 1, label = "Chinese and Japanese handscrolls") + 
  annotation_raster(standing, ymin= 0 , ymax=177, xmin=1100, xmax=1156, interpolate = TRUE) + 
  ylim(0, 750) +
  theme(plot.margin = unit(c(1,1,3,1), "cm"))

t <- grid.text("“standing” icon by Nicole Yip, from thenounproject.com.", x = unit(.1, "npc"), y = unit(.1, "npc"), just = c("left", "bottom"), gp=gpar(fontsize=10, col="#777777"))

png("metpaintings.png", width = 900, height = 450)
grid.draw(g)
grid.draw(t)
dev.off()
