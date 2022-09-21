#TRIAL CODE FOR GABRIEL's DATA

#cleaning
remove(list = ls())

#FIXED THE non-ZERO exit message
Sys.getenv("UDUNITS2_LIBS")
Sys.setenv("UDUNITS2_LIBS" = "/opt/homebrew/Cellar/udunits/2.2.28/lib")

Sys.getenv("UDUNITS2_INCLUDE")
Sys.setenv("UDUNITS2_INCLUDE" = "/opt/homebrew/Cellar/udunits/2.2.28/include")

install.packages("sf", type = "source",
                 configure.args = c("--with-udunits2-include=UDUNITS2_INCLUDE", "--with-udunits2-lib=UDUNITS2_LIBS",
                                    "--with-sqlite3-lib=/opt/homebrew/Cellar/sqlite/3.39.2/lib", "--with-proj-lib=/opt/homebrew/Cellar/proj/9.0.1/lib"))


#libraries
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
library(ggplot2)

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


#PLOT maritime area and grid cells that overlap with it - hollow grid
ggplot() + geom_sf(data = maritime_bounds_WGS84, colour = "black", fill = NA) + 
 geom_tile(data = MaritimeGrid, aes(x = X_COORD, y = Y_COORD), colour = "blue", fill = NA)+
  #add axes info
  theme_minimal()+
  labs(title = "Maritime Grid")+
  ggeasy::easy_center_title()+
  xlab("Longitude")+
  ylab("Latitude")

#trying to create a neutral code so that I do not have to copy paste so much

dropbox_directory = "/Users/amyirvine/Dropbox/"

#Species of Interest

#Atlantic Cod
#species_of_interest <- read.csv("/Users/amyirvine/Dropbox/FOME_DATA/FISH_TST/FINAL_SDM_RANGE_V4/FINAL_SDM_RANGE_V4_SPID_2113.csv", header = TRUE, sep=";")

#Atlantic Tuna
#species_of_interest <- read.csv("FOME_DATA/FISH_TST/FINAL_SDM_RANGE_V4/FINAL_SDM_RANGE_V4/FINAL_SDM_RANGE_V4_SPID_11180.csv", header = TRUE, sep=";")

## Rainbow parrotfish = Scarus guacamaia = SPID 10817
#species_of_interest <- read.csv("/Users/amyirvine/Dropbox/FOME_DATA/FISH_TST/FINAL_SDM_RANGE_V4/FINAL_SDM_RANGE_V4_SPID_10817.csv", header = TRUE, sep=";")

##Trying for a species that is only present in the deepwater shelf of maritime region##
#deep-water bristlemouth = Cyclothone atraria = SPID 15300
species_of_interest <- read.csv("/Users/amyirvine/Dropbox/FOME_DATA/FISH_TST/FINAL_SDM_RANGE_V4/FINAL_SDM_RANGE_V4_SPID_15300.csv", header = TRUE, sep=";")


#Combining species of interest with the geo
combined_species_of_interest <- merge(species_of_interest, geo, by.x="INDEX", by.y="row.names")

#converting with right projections
Species_geo_coordinates <- st_as_sf(combined_species_of_interest, coords = c("longitude","latitude"), crs = "+proj=longlat +datum=WGS84 +no_defs", remove = FALSE)

#combining gridcodes with species that now have coordinates
combo <- st_join(grid, Species_geo_coordinates)

Maritime_Species <- st_intersection(combo, maritime_bounds_WGS84)

ggplot() + geom_tile(data = Maritime_Species, aes(x = X_COORD, y = Y_COORD, fill = BINARY)) +
  geom_sf(data = maritime_bounds_WGS84, colour = "black", fill = NA) +
  labs(title = "Species of Interest")+
  theme_minimal()+
  scale_color_manual(name = "Presence/ Absence",
                     values = c("1" = "#bf0584",
                                "NA" = "blue")) +
  ggeasy::easy_center_title()+
  xlab("Longitude")+
  ylab("Latitude")

#Reading species presence into isolated area of Maritime Region ----
#KRISTINA CODE - import list of species
file <- "~/Dropbox/FOME_DATA/FINAL_SDM_RANGE_V4.zip"
file_names <- unzip(file, list=TRUE) # list zip archive to check on file names if needed

#DEREK CODE - Different way to read in file paths 
file_paths <- fs::dir_ls("~/Dropbox/FOME_DATA/FISH_TST/FINAL_SDM_RANGE_V4")
file_paths

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
  scale_fill_cmocean("Binary", name = "dense", na.value="white") +
  geom_sf(data = world, fill = "coral", colour = "white", size = 0.2)+
  labs(title = "Species of Interest") +
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


