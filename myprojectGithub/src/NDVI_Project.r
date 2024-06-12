#remotes::install_github("wmgeolab/rgeoboundaries")

library(rgeoboundaries)
library(sf)
library(raster)
library(here) #facilitate file path management in projects
library(ggplot2)
library(viridis)
#library(rgdal)

# Downloading the country boundary of Sweden
#map_boundary <- geoboundaries("Sweden") #extract the map of Sweden 
#dir.create("./data/modis", recursive = TRUE) #temporary directory to our folder. 

# Downloading the country boundary of Norway
#map_boundary <- geoboundaries("Norway") #extract the map of Norway 
#dir.create("./data/modis", recursive = TRUE) #temporary directory to our folder. 

# Defining filepath to save downloaded spatial file
#spatial_filepath <- "./data/modis/sweden.shp"
#spatial_filepath <- "./data/modis/norway.shp"

# Saving downloaded spatial file on to our computer
#st_write(map_boundary, paste0(spatial_filepath))


#library(MODIStsp)
#### check available data
#MODIStsp_get_prodlayers("M*D13Q1") #layer for vegetation 

#MODIStsp(
  #gui = FALSE,
  #out_folder = "./data/modis",
  #out_folder_mod = "./data/modis",
  #selprod = "Vegetation Indexes_16Days_250m (M*D13Q1)",
  #bandsel = "NDVI",
  #user = "mstp_test",
  #password = "MSTP_test_01",
  #start_date = "2020.06.01",
  #end_date = "2020.06.01",
  #verbose = FALSE,
  #spatmeth = "file",
  #spafile = spatial_filepath,
  #out_format = "GTiff"
#)



###################################################################################

# Reading the downloaded NDVI raster data
NDVI_raster_sw <- raster("./data/MYD13Q1_NDVI_2020_153_Sweden.tif")
NDVI_raster_nw <- raster("./data/MYD13Q1_NDVI_2020_153_Norway.tif")
NDVI_raster <- merge(NDVI_raster_sw, NDVI_raster_nw)

# Transforming the data
NDVI_raster <- projectRaster(NDVI_raster, crs = "+proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs")
plot(NDVI_raster)

# Cropping the data
list_countries <- c("sweden", "norway")
map_boundary <- geoboundaries(list_countries)
NDVI_raster <- raster::mask(NDVI_raster, as_Spatial(map_boundary))
plot(NDVI_raster)

# Dividing values by 10000 to have NDVI values between -1 and 1
#gain(NDVI_raster) <- 0.0001 #this function did not convert my data between -1 and 1, so I did it manually.
NDVI_raster <- NDVI_raster * 0.0001

# Matrix_full_eco_elev_clim is my data frame with latitude and longitude columns
spatial_points <- SpatialPoints(coords = matrix_full_eco_elev_clim[, c("longitude","latitude")], proj4string = CRS("+proj=longlat +datum=WGS84"))
plot(spatial_points,add=T)
####################################
####################################
#####################################

# Extract values
NDVI <- raster::extract(NDVI_raster, spatial_points)


###################
#################
#create the matrix_full_eco_elev_clim_sat with the NDVI data
matrix_full_eco_elev_clim_sat <- data.frame(matrix_full_eco_elev_clim, NDVI )


#visualise the data
p5 <- ggplot(matrix_full_eco_elev_clim_sat, aes(x = NDVI, fill = species)) +
  geom_density(alpha = 0.5,adjust=3) +
  labs(title = "Density of NDVI by Species",
       x = "NDVI value [-1 to 1, with -1 less vegetated areas]", y = "Density") +
  theme_minimal() 
print(p5)
#Vulpes lagopus occurs in less vegetated areas typical of tundra ecosystems. 