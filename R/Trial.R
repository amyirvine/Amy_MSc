#TRIAL CODE FOR GABRIEL's DATA

#cleaning
remove(list = ls())

#libraries
library(sf)
library(rnaturalearth)
library(dplyr)
library(tidyverse)
library(spData)
library(raster)
library(rnaturalearthdata)
library(stars)
library(mapview)
library(magrittr)
library(rgdal)

#setwd("C:/Users/Kristina at work/Dropbox/FOME_DATA")
setwd("~/Dropbox/FOME_DATA")

#BASIC loading to get all the info ----
load("./FISH_TST/META_MASTER_TAX_SPID_FISH_2FEB2021.Rdata") # META of all fish

#import geographical database layers
geo <- read.csv("~/Dropbox/FOME_DATA/GEOGRAPHICAL_LAYER/GEO_SPATIAL_META_MOL720GRID_V5.csv", header=TRUE)
View(geo)

#mapping shapefile
#maritime_area <- readOGR("~/Documents/MSC_Thesis/ScotianShelf_BayOfFundy_Bioregion/MaritimesPlanningArea.shp")
#GEO <- readOGR("~/Dropbox/FOME_DATA/720 x 228 grid GIS INFO/720x228global.shp") %>%
#  st_as_sf(GEO, coords = c("longitude","latitude"), crs = "+proj=longlat +datum=WGS84 +no_defs", remove = FALSE)

grid <- st_read("~/Dropbox/FOME_DATA/720 x 228 grid GIS INFO/720x228global.shp") %>% #grid data
  st_as_sf(grid, coords = c("longitude","latitude"), crs = "+proj=longlat +datum=WGS84 +no_defs", remove = FALSE)
#reads as 164160 observations of 19 variables

world <- ne_countries(scale = "medium", returnclass = "sf") %>%
  st_transform(crs = "+proj=longlat +datum=WGS84 +no_defs") #world data

maritime_bounds <- st_read("~/Documents/MSC_Thesis/ScotianShelf_BayOfFundy_Bioregion/MaritimesPlanningArea.shp")
#equivalent to Kristina's bounding box
maritime_bounds_WGS84 <- st_transform(maritime_bounds,"+proj=longlat +datum=WGS84 +no_defs")

MaritimeGrid <- st_intersection(grid, maritime_bounds_WGS84)


#PLOT maritime area and grid cells that overlap with it
ggplot() + geom_sf(data = maritime_bounds_WGS84, colour = "black", fill = NA) + 
 geom_point(data = MaritimeGrid, aes(x = X_COORD, y = Y_COORD), colour = "red", fill = NA)+
  #add axes info
  theme_minimal()+
  labs(title = "Cod") +
  ggeasy::easy_center_title()+
  xlab("Longitude") + # for the x axis label
  ylab("Latitude")

  labs(title = "Maritime Grid")+
  ggeasy::easy_center_title()+
  xlab("Longitude")+
  ylab("Latitude")

#create an empty list of grid cells * species ID and read in the SDM then get rid of it
#Trying to save the grid cells that fit within the maritime region as a function
###PRACTICE BUT NOT USING YET###
spatial_isolation_function <- function(MaritimeGrid) {
  grid <- st_read("~/Dropbox/FOME_DATA/720 x 228 grid GIS INFO/720x228global.shp") #grid data
  maritime_bounds <- st_read("~/Documents/MSC_Thesis/ScotianShelf_BayOfFundy_Bioregion/MaritimesPlanningArea.shp")
  #equivalent to Kristina's bounding box
  maritime_bounds_WGS84 <- st_transform(maritime_bounds,"+proj=longlat +datum=WGS84 +no_defs")
  
  MaritimeGrid <- st_intersection(grid, maritime_bounds_WGS84)
  MaritimeGrid_map <- ggplot() + geom_polygon(data = maritime_area, aes(x = long, y = lat, group = group), colour = "black", fill = NA) + 
    geom_point(data = MaritimeGrid, aes(x = X_COORD, y = Y_COORD), colour = "red", fill = NA)
  return(MaritimeGrid_map)
}


#Reading species presence into isolated area of Maritime Region ----
#KRISTINA CODE - import list of species
file <- "~/Dropbox/FOME_DATA/FINAL_SDM_RANGE_V4.zip"
file_names <- unzip(file, list=TRUE) # list zip archive to check on file names if needed

#DEREK CODE - Different way to read in file paths 
file_paths <- fs::dir_ls("~/Dropbox/FOME_DATA/FISH_TST/FINAL_SDM_RANGE_V4")
file_paths

#using readr unzip and set header and separator
#REPLACE SPID NUMBER AT END OF FILE NAME
#Here I import cod & tuna
AtlanticCod <- read.csv(unzip("~/Dropbox/FOME_DATA/FINAL_SDM_RANGE_V4.zip", "FINAL_SDM_RANGE_V4/FINAL_SDM_RANGE_V4_SPID_2113.csv"), header = TRUE, sep=";")
AtlanticTuna <- read.csv(unzip("~/Dropbox/FOME_DATA/FINAL_SDM_RANGE_V4.zip", "FINAL_SDM_RANGE_V4/FINAL_SDM_RANGE_V4_SPID_11180.csv"), header = TRUE, sep=";")

#Only looking at Binary so need to make that column categorical
AtlanticCod$BINARY <- ordered(AtlanticCod$BINARY, levels = 0:1)
str(AtlanticCod)

AtlanticTuna$BINARY <- ordered(AtlanticTuna$BINARY, levels = 0:1)
str(AtlanticTuna)

#combine Atlantic Cod and Tuna with geo file
combined_AtCod <- merge(AtlanticCod, geo, by.x="INDEX", by.y="row.names")
combined_AtTun <- merge(AtlanticTuna, geo, by.x="INDEX", by.y="row.names")

AtCodcoords <- st_as_sf(combined_AtCod, coords = c("longitude","latitude"), crs = "+proj=longlat +datum=WGS84 +no_defs", remove = FALSE)
AtTuncoords <- st_as_sf(combined_AtTun, coords = c("longitude","latitude"), crs = "+proj=longlat +datum=WGS84 +no_defs", remove = FALSE)
#don't join the species but rather see if they are present and if present, add to a list

combo <- st_join(grid, AtCodcoords) #both need to be sf format as join is performed spatially

MaritimeCod <- st_intersection( combo, maritime_bounds_WGS84) #causes R program to reboot
class(MaritimeCod)

ggplot() + geom_sf(data = maritime_bounds_WGS84, colour = "black", fill = NA) + 
  geom_point(data = MaritimeCod, aes(x = X_COORD, y = Y_COORD, colour = BINARY)) +
  labs(title = "Maritime Cod")+
  scale_color_manual(name = "Presence/ Absence",
                                                  values = c("1" = "#bf0584",
                                                             "0" = "blue")) +
  ggeasy::easy_center_title()+
  xlab("Longitude")+
  ylab("Latitude")

###PRACTICING WITH A SPECIES NOT IN NW ATLANTIC TO SEE IF IT WORKS####
## Rainbow parrotfish = Scarus guacamaia = SPID 10817
RainbowParrot <- read.csv(unzip("~/Dropbox/FOME_DATA/FINAL_SDM_RANGE_V4.zip", "FINAL_SDM_RANGE_V4/FINAL_SDM_RANGE_V4_SPID_10817.csv"), header = TRUE, sep=";")


#Only looking at Binary so need to make that column categorical
#RainbowParrot$BINARY <- ordered(RainbowParrot$BINARY, levels = 0:1)
#str(RainbowParrot)

#combine Atlantic Cod and Tuna with geo file
combined_RainbowParrot <- merge(RainbowParrot, geo, by.x="INDEX", by.y="row.names")

RbParrotcoords <- st_as_sf(combined_RainbowParrot, coords = c("longitude","latitude"), crs = "+proj=longlat +datum=WGS84 +no_defs", remove = FALSE)
#don't join the species but rather see if they are present and if present, add to a list

comboRbParrot <- st_join(grid, RbParrotcoords) #both need to be sf format as join is performed spatially
#rast2 <- st_rasterize(comboRbParrot %>% dplyr::select(BINARY, geometry)) #rasterize for easier plotting

MaritimeRbParrot <- st_intersection(comboRbParrot, maritime_bounds_WGS84) #causes R program to reboot
class(MaritimeRbParrot)

#is this right? looking at only the 1 value
#MaritimeRbParrot_present <- combo[MaritimeRbParrot[[1]],]
#class(MaritimeRbParrot_present)

ggplot() + geom_sf(data = maritime_bounds_WGS84, colour = "black", fill = NA) + 
  theme_minimal()+
  geom_point(data = MaritimeRbParrot, aes(x = X_COORD, y = Y_COORD, colour = BINARY)) +
  scale_color_manual(name = "Presence",
                     values = c("1" = "#bf0584",
                                "0" = "blue")) +
  labs(title = "Maritime Rainbow Parrot")+
  ggeasy::easy_center_title() +
  xlab("Longitude")+
  ylab("Latitude")



#Practice
combo$BINARY <- (na.omit(combo$BINARY))



mydata$y = ifelse(mydata$x3 %in% c("A","B") ,mydata$x1*2,
                  ifelse(mydata$x3 %in% c("C","D"), mydata$x1*3,
                         mydata$x1*4))



ifelse(combo$BINARY %in% c("0","1"), transmute(combo, species_pres = 2113))
transmute(combo, species_pres = ifelse(BINARY %in% c("0","1"), "Present", )))


ggplot() + geom_sf(data = maritime_bounds_WGS84, colour = "black", fill = NA) + 
  geom_point(data = MaritimeCod, aes(x = X_COORD, y = Y_COORD), colour = "red", fill = NA)


#Matthew GGplot - Richness ----
#data("World") #grab data set

rast <- st_rasterize(combo %>% dplyr::select(BINARY, geometry)) #rasterize for easier plotting

library(cmocean)
world <- ne_countries(scale = "medium", returnclass = "sf") %>% #world data
  st_transform(crs = "+proj=longlat +datum=WGS84 +no_defs")
ggplot() +
  theme_minimal()+
  #add axes info
  geom_stars(data = rast)+
  scale_fill_cmocean("Binary", name = "dense", na.value="white", discrete = TRUE) +
  geom_sf(data = world, fill = "coral",
          colour = "white", size = 0.2)+
  labs(title = "Cod") +
  ggeasy::easy_center_title()+
  xlab("Longitude") + # for the x axis label
  ylab("Latitude")


#Kristina tmap plotting ----
#plot using the tmap package
tmap_mode("plot") #set to plotting
data("World") #grab data set
map <- tm_shape(World) + "bbox = maritime_bounds_WGS84" +
  tm_grid(lines=FALSE) + #include ticks but no grid
  tm_polygons() +
  tm_borders("grey") +
  tm_layout(inner.margins = 0) + #remove margins in frame
  tm_shape(rast) +
  tm_raster(col="HSI",
            n=10, #number of breaks for legend
            title="HSI", 
            palette = "Blues") +
  tm_layout(main.title="Thunnus albacares",
            legend.outside = TRUE, #place legend outside of map
            legend.outside.position=c("right", "top"),
            legend.outside.size = 0.2) #remove some white space next to legend
map

#MG - GGplot 

ggplot() +
  theme_minimal()+
  #add axes info
  
  geom_stars(data = rast)+
  scale_fill_cmocean("HSI", name = "matter", na.value="white") +
  geom_sf(data = World, fill = "coral",
          colour = "white", size = 0.2)+ 
  
  
  labs(title = "Thunnus albacares") +
  ggeasy::easy_center_title()+
  xlab("Longitude") + # for the x axis label
  ylab("Latitude")



# 8. SAVE map ----
tmap_save(map, "Gadus morhua.png", asp=0)

# save as PDF
filename <- "./Figure_X.pdf"
ggsave(plot = map, filename = filename, width = 10, height = 10)





#EXTRA
#Mapping the world
worldmap <- ne_countries(scale = 'medium', type = 'map_units',
                         returnclass = 'sf')
ggplot() + geom_sf(data = worldmap) + theme_bw()

#Mapping Canada
canada <- worldmap[worldmap$name == 'Canada',]
ggplot() + geom_sf(data = canada) + theme_bw()