##needed packages
#install.packages("elevatr")
library(sf)
library(elevatr) #used for accessing elevation data
library(raster)

# Disable use of S2 geometry library in the sf package
sf_use_s2(FALSE)

#Use SweNor to have only Sweden and Norway without Svalbard. See the script of Import_data to have the whole code
#SweNor_notfiltered <- ne_countries(scale = "medium", returnclass = "sf",country = c("Sweden", "Norway") )
#SweNor <- SweNor_notfiltered %>%
#  st_make_valid() %>%
#  st_crop(bbox_sf)


# Retrieve elevation data for Sweden and Norway
elevation <- get_elev_raster(SweNor, z = 5)
plot(elevation)

## crop and mask
r2 <- crop(elevation, extent(SweNor))
elevation <- mask(r2, SweNor)
plot(elevation)


############################### add points 
latitude <- matrix_full_eco$latitude
longitude <- matrix_full_eco$longitude
 
# Create a data frame 
coord <- data.frame(longitude,latitude)

# Extract elevation values at Vulpes vulpes and Vulpes Lagopus occurrences
ll_prj <- "EPSG:4326" 
points <- sp::SpatialPoints(coord, 
                            proj4string = sp::CRS(SRS_string = ll_prj))
elevation_points <- extract(elevation, points, method='bilinear')
elevation_df <- data.frame(elevation = elevation_points)

#Add to matrix_full_eco
matrix_full_eco_elev <- cbind(matrix_full_eco, elevation_df)

# Plot density of elevation data for Vulpes vulpes and Vulpes lagopus occurrences
p3 <- ggplot(matrix_full_eco_elev, aes(x = elevation, fill = species)) +
  geom_density(alpha = 0.5,adjust=3) +
  labs(title = "Density of Elevation by Species",
       x = "Elevation", y = "Density") +
  theme_minimal() 
print(p3)

#Vulpes lagopus occurences are at higher altitudes than Vulpes vulpes occurences.