#install.packages("rayshader")
library(rayshader)  #used for creating 2D and 3D visualizations

###### 3d plot v1 
elmat <- raster_to_matrix(elevation)
attr(elmat, "extent") <- extent(elevation)

#simple version
#elmat %>% 
  #sphere_shade(texture = "bw") %>%
  #plot_map()

###### 3d map version
elmat %>% 
  sphere_shade(texture = "bw") %>%
  add_shadow(ray_shade(elmat, zscale = 3), 0.4) %>%
  add_water(detect_water(elmat,cutoff = 0.5), color = "desert") %>%
plot_3d(elmat, zscale = 150, fov = 0, theta = 135, zoom = 0.75, 
        phi = 45, windowsize = c(1500, 800)) 


# Render points on the 3D elevation map with two colors for each species
vec <- c(1:nrow(matrix_full_eco_elev)) #alternative rep("1",nrow(matrix_full_eco_elev))
vec[matrix_full_eco_elev$species =="Vulpes vulpes"] <- "darkred"
vec[matrix_full_eco_elev$species =="Vulpes lagopus"] <- "blue"

render_points(
  extent = extent(SweNor), size = 10,
  lat = matrix_full_eco_elev$latitude, long = matrix_full_eco_elev$longitude,
  altitude = elevation_points + 100, zscale = 150, color = vec
)

# Render label on the 3D elevation map for Stockholm
render_label(heightmap = elmat, extent = extent(SweNor),
             long = 18.0685808, lat = 59.3293235, altitude = 50000, 
             linewidth = 2, zscale = 1000, text = "Stockholm", textsize = 1.5)

# Render label on the 3D elevation map for Oslo
render_label(heightmap = elmat, extent = extent(SweNor),
             long = 10.7522454, lat = 59.9138688, altitude = 50000, 
             linewidth = 2, zscale = 1000, text = "Oslo", textsize = 1.5)



#################
#################Kernel

#install needed packages: 
#install.packages("tidyverse")
#install.packages("RColorBrewer")
#install.packages("eks")
library(sf)
library(elevatr)
library(raster)
library(tidyverse) #for data manipulation and visualization
library(RColorBrewer) #provides color palettes
library(rayshader)
library(eks) #provides functions for fitting statistical models

# Disable use of S2 geometry library in the sf package
sf_use_s2(FALSE)

#takes latitude and longitude values from the data frame, converts them into a spatial points object
sf_points <- data.frame(
    lat = matrix_full_eco_elev$latitude,
    lon = matrix_full_eco_elev$longitude
  ) |>
    st_as_sf(coords = c("lon", "lat"), crs = 4326)
plot(sf_points)

#Calculate the kernel density estimate from the spatial points sf_points using a grid size of 100x100
skde1 <- st_kde(sf_points, gridsize = c(100, 100))
plot(skde1)
#generate contour lines
dataxx = st_get_contour(skde1, cont = c(seq(1, 99, 5)), disjoint = FALSE)

# Create a function to generate the color palette
color_palette <- colorRampPalette(c("darkolivegreen4","darkolivegreen3","darkseagreen1","yellow","orange","red","darkred"))
# Define the number of colors in the palette
num_colors <- 20  # Adjust as needed
# Generate the color palette
palette <- color_palette(num_colors)

##plot 2d kernel 
elmat <- raster_to_matrix(elevation)

elmat %>%
 sphere_shade(texture = "desert") %>%
 add_overlay(generate_polygon_overlay(dataxx, 
                        palette = palette, linewidth=0,
                        extent = extent(elevation), heightmap = elmat),
                        alphalayer=0.7)  %>%
plot_map()
#the red areas show higher densities of points. 

##plot 3d kernel (nicer)
elmat %>%
 sphere_shade(texture = "bw") %>%
 add_overlay(generate_polygon_overlay(dataxx, 
                        palette = palette, linewidth=0,
                        extent = extent(elevation), heightmap = elmat),
                        alphalayer=0.7)  %>%
plot_3d(elmat, zscale = 150, fov = 0, theta = 135, zoom = 0.75, 
        phi = 45, windowsize = c(1500, 800))



