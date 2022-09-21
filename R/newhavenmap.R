# load package and data
library(GISTools)
data(newhaven)
proj4string(roads) <- proj4string(blocks)
# plot spatial data
#pdf(file='map.pdf') #sets all the next maps to be within "map.pdf"
png(file='map.png')
#or use the png command to save as a png

tm_shape(blocks) +
  tm_borders(lwd = 3) + #lwd is line width
  tm_shape(roads) +
  tm_lines(col = "blue", lwd = 2) +
  # embellish the map
  tm_scale_bar(width = 0.22) +
  tm_compass(position = c(0.8, 0.07)) +
  tm_layout(frame = F, title = "New Haven, CT", 
            title.size = 1.5, 
            title.position = c(0.55, "top"), 
            legend.outside = T) 
dev.off() #turns off the map command so that no more maps are added to the file
#these maps are now saved collectively/together in one pdf doc
