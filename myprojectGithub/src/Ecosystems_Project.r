
##### https://esri.maps.arcgis.com/home/item.html?id=3bfa1aa4cd9844d5a0922540210da25b

# Load the librairies
library(raster)
library(ggplot2)

# Set the file path to the GeoTIFF
file_path <- "./data/WorldEcosystem.tif"

# Read the raster GeoTIFF
ecosystem_raster <- raster(file_path)

#Use SweNor to have only Sweden and Norway without Svalbard. See the script of Import_data to have the whole code
#SweNor_notfiltered <- ne_countries(scale = "medium", returnclass = "sf",country = c("Sweden", "Norway") )
#SweNor <- SweNor_notfiltered %>%
#  st_make_valid() %>%
#  st_crop(bbox_sf)


## crop and mask
r2 <- crop(ecosystem_raster, extent(SweNor))
ecosystem_swenor <- mask(r2, SweNor)
plot(ecosystem_swenor)


# Matrix_full is the data frame with latitude and longitude columns
spatial_points <- SpatialPoints(coords = matrix_full[, c("longitude","latitude")], proj4string = CRS("+proj=longlat +datum=WGS84"))
plot(spatial_points,add=T,pch=16,cex=2) 

# Extract values: to give a value to each point
eco_values <- raster::extract(ecosystem_swenor, spatial_points)

# Print the extracted values to visualize dataset
print(eco_values)

###########Combine two data frames into one
matrix_full_eco <- data.frame(matrix_full, eco_values)

## Remove the NA
unique(matrix_full_eco$eco_values) #we can see all the modalities of the vector
matrix_full_eco <- matrix_full_eco[!is.na(matrix_full_eco$eco_values),] # Remove rows where eco_values column has NA values


####################################################################################################3
######### metadata: 

#Read metadata from a tab-delimited file
metadat_eco <- read.delim("./data/WorldEcosystem.metadata.tsv")

################################################
#Merge matrix_full_eco and metadat_eco data frames by matching values in eco_values and Value columns
matrix_full_eco <- merge(matrix_full_eco, metadat_eco, by.x="eco_values", by.y="Value", all.x=T)


### Plot to verify
p2 <- ggplot(matrix_full_eco, aes(x = Climate_Re)) +
  geom_bar(aes(fill = species), position = "dodge") +
  theme_minimal() +
  labs(title = "Count of Observations of Each Species by Climate",
       x = "Climate",
       y = "count of observations")
print(p2)
#Red fox have more occurences in Cool Temperate Moist Climate
#Arctic fox have more occurences in Borel Moist Climate
