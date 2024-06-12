#install.packages("geodata")
#install.packages("sp")
library(geodata) #tools for importing, manipulating, and visualizing spatial data
library(sp)     #methods for representing and manipulating spatial data objects
library(ggplot2)


# Matrix_full_eci_elev is my data frame with latitude and longitude columns
spatial_points <- SpatialPoints(coords = matrix_full_eco_elev[, c("longitude","latitude")], proj4string = CRS("+proj=longlat +datum=WGS84"))
plot(spatial_points)

###################################################################
###################################################################
# Get temperature data for Sweden and Norway
list_countries <- c("sweden", "norway")

clim_tempsw <- worldclim_country(list_countries[1], var = "tavg", path = tempdir()) 
clim_tempnw <- worldclim_country(list_countries[2], var = "tavg", path = tempdir())
clim_temp_tot <- merge(clim_tempsw, clim_tempnw)
clim_temp_br <- brick(clim_temp_tot)
plot(clim_temp_tot)

################################################################
# create matrix that will be added to the matrix_full_eco_elev
matrix_temp = NULL
vec_colnames = NULL

# LOOP
# Create a loop to have mean temperature data from january to december over 20 years
for (i in 1:12) {
raster_temp_avg <- as(clim_temp_br[[i]], "Raster")
vec_colnames <- c(vec_colnames, names(raster_temp_avg))

temp <- raster::extract(raster_temp_avg, spatial_points, method = 'bilinear')
matrix_temp <- cbind(matrix_temp, temp) 
}

# Add the names of the column in the matrix that are in the vectore names
colnames(matrix_temp) <- vec_colnames

# Make the mean of these annual value and store it into the matrix
vec_mean_temp <- as.vector(rowMeans(matrix_temp))
matrix_temp <- data.frame(matrix_temp, vec_mean_temp)

################################
#################################


# Plot density of temperature data for my species occurences
ggplot(matrix_temp, aes(x = vec_mean_temp)) +
  geom_density(color = "darkblue", fill = "lightblue", adjust = 3) +
  theme_bw()
#we see the range of temperatures in my data

###################################################################
###################################################################
###################################################################
# Retrieve precipitation data for Sweden and Norway

clim_precsw <- worldclim_country(list_countries[1], var = "prec", path = tempdir()) 
clim_precnw <- worldclim_country(list_countries[2], var = "prec", path = tempdir())
clim_prec_tot <- merge(clim_precsw, clim_precnw)
clim_prec_br <- brick(clim_prec_tot)
plot(clim_prec_tot)

# create matrix that will be added to the matrix full
matrix_prec = NULL
vec_colnames1 = NULL

# LOOP
# Create a loop to have the mean precipitation data from january to december over 20 years
for (i in 1:12) {
raster_prec_avg <- as(clim_prec_br[[i]], "Raster")
vec_colnames1 <- c(vec_colnames1, names(raster_prec_avg))

temp <- raster::extract(raster_prec_avg, spatial_points, method = 'bilinear')
matrix_prec <- cbind(matrix_prec, temp) 
}

# Add the names of the column in the matrix that are in the vectore names
colnames(matrix_prec) <- vec_colnames1

# Make the mean of these annual value and store it into the matrix
vec_mean_prec <- as.vector(rowMeans(matrix_prec))
matrix_prec <- data.frame(matrix_prec,vec_mean_prec)


# Plot density of precipitation data my species occurrences
ggplot(matrix_prec, aes(x = vec_mean_prec)) +
  geom_density(color = "black", fill = "darkgreen", adjust = 2) +
  theme_bw()
#we see the range of precipitations values in my dataset


###################################
#combine the 3 dataframes into one:
matrix_full_eco_elev_clim <- data.frame(matrix_full_eco_elev,matrix_prec,matrix_temp)


# Create the plot to visualize 
p4 <- ggplot(matrix_full_eco_elev_clim, aes(x = vec_mean_prec,y=vec_mean_temp, color = species)) +
  geom_point() +
  theme_minimal()

print(p4)
#Vulpes vulpes has more occurences in higher mean temperatures and less mean precipitations

