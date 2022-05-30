#TRIAL CODE FOR GABRIEL's DATA

#cleaning
remove(list = ls())

#libraries
library(sf)
library(rnaturalearth)
library(dplyr)
library(tidyverse)
library(spData)
library(tmap)
library(raster)
library(rnaturalearthdata)
library(stars)
library(mapview)
library(magrittr)
library(rgdal)

#import geographical database layers
geo <- read.csv("~/Dropbox/FOME_DATA/GEOGRAPHICAL_LAYER/GEO_SPATIAL_META_MOL720GRID_V5.csv", header=TRUE)
View(geo)

#KRISTINA CODE - import list of species 
file <- "~/Dropbox/FOME_DATA/FINAL_SDM_RANGE_V4.zip"
file_names <- unzip(file, list=TRUE) # list zip archive to check on file names if needed

#DEREK CODE - Different way to read in file paths 
file_paths <- fs::dir_ls("~/Dropbox/FOME_DATA/FISH_TST/FINAL_SDM_RANGE_V4")
file_paths

#using readr unzip and set header and separator
#REPLACE SPID NUMBER AT END OF FILE NAME
#Here I import cod
AtlanticCod <- read.csv(unzip("~/Dropbox/FOME_DATA/FINAL_SDM_RANGE_V4.zip", "FINAL_SDM_RANGE_V4/FINAL_SDM_RANGE_V4_SPID_2113.csv"), header = TRUE, sep=";")
AtlanticTuna <- read.csv(unzip("~/Dropbox/FOME_DATA/FINAL_SDM_RANGE_V4.zip", "FINAL_SDM_RANGE_V4/FINAL_SDM_RANGE_V4_SPID_11180.csv"), header = TRUE, sep=";")

#combine Atlantic Cod and Tuna with geo file
combined_AtCod <- merge(AtlanticCod, geo, by.x="INDEX", by.y="row.names")
combined_AtTun <- merge(AtlanticTuna, geo, by.x="INDEX", by.y="row.names")

AtCodcoords <- st_as_sf(combined_AtCod, coords = c("longitude","latitude"), crs = 4326, remove = FALSE)
AtTuncoords <- st_as_sf(combined_AtTun, coords = c("longitude","latitude"), crs = 4326, remove = FALSE)

CodTuna <- st_join(AtCodcoords, AtTuncoords)
#Derek says use st_intersects()
#this seems wrong because the output is off

#mapping shapefile
maritime_area <- readOGR("~/Documents/MSC_Thesis/ScotianShelf_BayOfFundy_Bioregion/MaritimesPlanningArea.shp")
GEO <- readOGR("~/Dropbox/FOME_DATA/720 x 228 grid GIS INFO/720x228global.shp")
#reads as spatial polygon - do we need to readOGR first?

grid <- st_read("~/Dropbox/FOME_DATA/720 x 228 grid GIS INFO/720x228global.shp") #grid data
#reads as 164160 observations of 19 variables

world <- ne_countries(scale = "medium", returnclass = "sf") %>% #world data

  maritime_bounds <- st_read("~/Documents/MSC_Thesis/ScotianShelf_BayOfFundy_Bioregion/MaritimesPlanningArea.shp")
#equivalent to Kristina's bounding box
maritime_bounds_WGS84 <- st_transform(maritime_bounds,"+proj=longlat +datum=WGS84 +no_defs")
GEO2 <- st_as_sf(GEO, coords = c("longitude","latitude"), crs = "+proj=longlat +datum=WGS84 +no_defs", remove = FALSE)

intersection1 <- st_intersection(grid, maritime_bounds_WGS84)


#PLOT
ggplot() + geom_polygon(data = maritime_area, aes(x = long, y = lat, group = group), colour = "black", fill = NA) + 
 geom_point(data = intersection1, aes(x = X_COORD, y = Y_COORD), colour = "red", fill = NA)


combo <- st_join(grid, CodTuna) #both need to be sf format as join is performed spatially
rast <- st_rasterize(combo %>% dplyr::select(HSI.x, geometry)) #rasterize for easier plotting



#KRISTINA'S CODE

id <- META %>%
  filter(AcceptedSciName == "Thunnus albacares") %>%
  select("SPID") 
id

#EXTRACT file with the SPID number from FINAL_SDM_RANGE_V4.zip (DOWNLOADED FOR EASIER ACCESS) ----
#get file from local location and unzip
file <- ("~/Dropbox/FOME_DATA/FINAL_SDM_RANGE_V4.zip")
file_names <- unzip(file, list=TRUE) # list zip archive to check on file names if needed
#using readr unzip and set header and separator
#REPLACE SPID AT END OF FILE NAME
species <- read.csv(unzip("FINAL_SDM_RANGE_V4.zip", "FINAL_SDM_RANGE_V4/FINAL_SDM_RANGE_V4_SPID_11180.csv"), header = TRUE, sep=";")

# 4. MATCH to GEO_SPATIAL_META ----
#INDEX in species file corresponds to line number of the Geographical MASTER data
#merge files by INDEX and row.names
combined_df <- merge(species, geo, by.x="INDEX", by.y="row.names")

#convert row names to column as row.names=1 doesn't work for some shitty reason
#geo$INDEX_geo <- row.names(geo)
#combined_df <- merge(species, geo, by.x="INDEX", by.y="INDEX_geo")

# 5. WRITE output as csv ----
write.csv(combined_df, "Thunnus albacares.csv", row.names=FALSE)




#EXTRA
#Mapping the world
worldmap <- ne_countries(scale = 'medium', type = 'map_units',
                         returnclass = 'sf')
ggplot() + geom_sf(data = worldmap) + theme_bw()

#Mapping Canada
canada <- worldmap[worldmap$name == 'Canada',]
ggplot() + geom_sf(data = canada) + theme_bw()