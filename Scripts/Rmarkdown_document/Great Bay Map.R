library(sf)
library(ggplot2)
library(lubridate)
library(ggplot2)
library(dplyr)
library(data.table)
library(ggrepel)
library(tidyverse)
library(ggmap)
library(grid)
library(ggsn)
library(raster)
library(maps)
library(rgdal)
library(dplyr)
library(ggpattern)
# remotes::install_github("coolbutuseless/ggpattern")
# Need to install ggpattern for geom_polygon_pattern
library(patchwork)
library(gridExtra)
library(cowplot)
library(ggspatial)
library(here)



m <- get_stamenmap(bbox = c(-70.921099,43.045919,-70.829538, 43.127930), maptype = "toner-lite", zoom = 13, scale = "auto")

#shape file for oyster natural reefs
shapefile1 <- readOGR(file.path(here("Data", "2020 Great Bay Reefs", "2020 Great Bay natural reefs.shp")))
shapefile_df1 <- fortify(shapefile1)

#greabay
gb<-ggmap(m)+
  geom_polygon_pattern(data=shapefile_df1,
                       aes(x = long, y = lat, group = group),#oyster reef natural
                       fill    = 'grey33',
                       colour  = 'grey33',
                       pattern = "none",pattern_density = 0.01, pattern_spacing=0.013, pattern_colour = "grey33", pattern_alpha = 0.8, alpha = 0.65, pattern_angle = 45)+
  geom_point(aes(x = -70.86365, y = 43.06843), data = NULL, 
             alpha = 0.5, color = "black", size = 4, pch= 18)+
  geom_point(aes(x = -70.86365, y = 43.06843), data = NULL, 
             alpha = 0.5, color = "black", size = 4, pch= 18)+
  annotate('text', x = -70.859, y = 43.0686, label = 'WP', 
           colour = I("black"), size = 3)+
  geom_point(aes(x = -70.86208, y = 43.06987), data = NULL, 
             alpha = 0.5, color = "black", size = 4, pch= 18)+
  annotate('text', x = -70.8659, y = 43.0693, label = 'NI', 
           colour = I("black"), size = 3)+
  geom_point(aes(x = -70.86609, y = 43.08920), data = NULL, 
             alpha = 0.5, color = "black", size = 4, pch= 18)+
  annotate('text', x = -70.86725, y = 43.0874, label = "AP", 
           colour = I("black"), size = 3)+
  geom_point(aes(x = -70.85570, y = 43.11592), data = NULL, 
             alpha = 0.5, color = "black", size = 4, pch= 18)+
    annotate('text', x = -70.85578, y = 43.114, label = 'OF', 
           colour = I("black"), size = 3)+
  annotate('text', x = -70.9140, y = 43.049, label = 'SR', 
           colour = I("black"), size = 4, angle = 70, fontface = "italic")+
  annotate('text', x = -70.913, y = 43.0656, label = 'LR', 
           colour = I("black"), size = 4, fontface = "italic")+
  annotate('text', x = -70.872, y = 43.123, label = 'OR', 
           colour = I("black"), size = 4, angle = -15, fontface = "italic")+
  annotate('text', x = -70.862, y = 43.105, label = 'Little Bay', 
           colour = I("black"), size = 4.5, fontface = "italic")+
  annotate('text', x = -70.889, y = 43.07, label = 'Great Bay', 
           colour = I("black"), size = 4.5, fontface = "italic")+
  annotate('text', x = -70.84, y = 43.12, label = 'PR', 
           colour = I("black"), size = 4, angle = -15, fontface = "italic")+
  annotate('text', x = -70.8467, y = 43.0497, label = 'WR', 
           colour = I("black"), size = 4, angle = -40)+
  labs(x = 'Longitude', y = 'Latitude')+   
  ggsn::scalebar(x.min = -70.893, x.max = -70.868, 
                 y.min = 43.049, y.max = 43.06, dist_unit = "km", 
                 dist = 0.5, transform = TRUE, 
                 model = "WGS84", height = 0.1, 
                 st.dist = 0.1, st.size = 3, border.size = 0.3)+#scale bar
  labs(x = 'Longitude', y = 'Latitude')+
  theme(axis.text = element_text(size = 15, color = "black"), axis.title=element_text(size = 17), 
        axis.ticks = element_line (colour = "black", size = 1),  
        axis.line = element_line(color = "black", size = 1))

gb
#map of east coast
states<-map_data("state")

east_coast <- subset(states, region %in% c("new hampshire", 'maine','massachusetts'))



nh2<-ggplot(data = east_coast) + 
  geom_polygon(aes(x = long, y = lat, group = group), fill = "white", color = "black") + 
  geom_point(aes(x =-70.876526, y =  43.077958), color = 'red', size = 3, pch = 18)+
  annotate('rect', xmin=-72.75, ymin=42.4, xmax=-70.47, ymax=45.7, col = "black", alpha = 0.2, size = 1)+
  coord_fixed(1.5)+
    theme_void() +
  xlab("") + ylab("")


nh2

#inset - final product
gb+
  inset(ggplotGrob(nh2), xmin =-70.97, xmax= -70.835, ymin = 43.096, ymax= 43.13)


ggsn::north2(m, x = 0.5683, y = 0.17 ,scale = 0.08, symbol = 1) # add north compass
# you'll need to adjust this










