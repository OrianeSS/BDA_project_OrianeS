#install.packages("rgbif")
#install.packages("rnaturalearth")
#install.packages("ggplot2")
#install.packages("rinat")
#install.packages("raster")
#install.packages("sf")
#install.packages("viridis")
#install.packages("rgeoboundaries")
#install.packages("dlpyr")
library(rgbif)         # For accessing GBIF occurrence data
library(rnaturalearth) # For obtaining spatial data
library(ggplot2)       # For data visualization
library(rinat)         # For accessing iNaturalist occurrence data
library(raster)        # For spatial operations
library(sf)            # for vector spatial data
library(viridis)       # provides color palettes
library(rgeoboundaries) # access spatial data from the GeoBoundaries 
library(dplyr)         # provides functions for data manipulation, particularly for data frames

#Species list
myspecies1 <- c("Vulpes vulpes") #red fox
myspecies2 <- c("Vulpes lagopus") #arctic fox

# get GBIF data 
# Get occurrence data for myspecies1 from GBIF for Norway (NO)
gbif_data1N <- occ_data(scientificName = myspecies1, hasCoordinate = TRUE, limit = 1000, country = "NO")  
# Get occurrence data for myspecies1 from GBIF for Sweden (SE)
gbif_data1S <- occ_data(scientificName = myspecies1, hasCoordinate = TRUE, limit = 1000, country = "SE")  
# Get occurrence data for myspecies2 from GBIF for Norway (NO)
gbif_data2N <- occ_data(scientificName = myspecies2, hasCoordinate = TRUE, limit = 1000, country = "NO")  
# Get occurrence data for myspecies2 from GBIF for Sweden (SE)
gbif_data2S <- occ_data(scientificName = myspecies2, hasCoordinate = TRUE, limit = 1000, country = "SE")  

# Extract occurrence data for the two species from the two countries
occur1N  <- gbif_data1N$data
occur1S <- gbif_data1S$data
occur2N <- gbif_data2N$data
occur2S <- gbif_data2S$data

#convert the occurence data in dataframe to better visualise
occur1BN <- data.frame(gbif_data1N$data) #to better visualise
occur1BS <- data.frame(gbif_data1S$data) #to better visualise
occur2BN <- data.frame(gbif_data2N$data) #to better visualise
occur2BS <- data.frame(gbif_data2S$data) #to better visualise

## Filter occurrence data for each species in each country
gbif_data_sweden1 <- occur1S[occur1S$country == "Sweden",]
gbif_data_norway1 <- occur1N[occur1N$country == "Norway",] 

gbif_data_sweden2 <- occur2S[occur2S$country == "Sweden",] 
#remove specimen of museum for the arctic fox verified with: unique(gbif_data_sweden2$basisOfRecord)
gbif_data_sweden2 <- gbif_data_sweden2[gbif_data_sweden2$basisOfRecord =="HUMAN_OBSERVATION",]
dim(gbif_data_sweden2) #to make sure the data are present

gbif_data_norway2 <- occur2N[occur2N$country == "Norway",] 

###################################################################
####Creation of a raster with the two countries Sweden and Norway
SweNor_notfiltered <- ne_countries(scale = "medium", returnclass = "sf",country = c("Sweden", "Norway") )
plot(SweNor_notfiltered)
plot(SweNor_notfiltered["geometry"])

# Define a box to exclude Svalbard because no data there (~74° N)
bbox <- st_bbox(c(xmin = 0, xmax = 60, ymin = 35, ymax = 74), crs = st_crs (SweNor_notfiltered))

# Convert the bbox in an object sf
bbox_sf <- st_as_sfc(bbox, crs = st_crs(SweNor_notfiltered))
print(bbox_sf)

# Filter the data to exclude the zone
SweNor <- SweNor_notfiltered %>%
  st_make_valid() %>%
  st_crop(bbox_sf)

# Confirm with a plot
plot(SweNor["geometry"])


#Make plot for species 1, Red fox in Norway 
ggplot(data = SweNor) + 
  geom_sf()   +
  geom_point(data = gbif_data_norway1, aes(x = decimalLongitude, y = decimalLatitude), size = 4, 
             shape = 23, fill = "darkgreen") + geom_point(data = gbif_data_sweden1, aes(x = decimalLongitude, y = decimalLatitude), size = 4, 
             shape = 23, fill = "darkgreen") + theme_classic()  


#For species 2, Arctic fox in Norway
ggplot(data = SweNor) + 
  geom_sf()   +
  geom_point(data = gbif_data_norway2, aes(x = decimalLongitude, y = decimalLatitude), size = 4, 
             shape = 23, fill = "red") + geom_point(data = gbif_data_sweden2, aes(x = decimalLongitude, y = decimalLatitude), size = 4, 
             shape = 23, fill = "red") + theme_classic() 

#make the same plots for Sweden if you want. 
##################################################

# Extract relevant data from GBIF occurrences for Red fox in Norway
species <- gbif_data_norway1$species
latitude <- gbif_data_norway1$decimalLatitude
longitude <- gbif_data_norway1$decimalLongitude
source <- rep("gbif", length(species))

# Create a data frame for GBIF data
data_gbif1 <- data.frame(species, latitude, longitude, source)

# Extract relevant data from GBIF occurrences for Red fox in Sweden
species <- gbif_data_sweden1$species
latitude <- gbif_data_sweden1$decimalLatitude
longitude <- gbif_data_sweden1$decimalLongitude
source <- rep("gbif", length(species))

# Create a data frame for GBIF data 
data_gbif2 <- data.frame(species, latitude, longitude, source)

# Extract relevant data from GBIF occurrences for Arctic fox in Norway
species <- gbif_data_norway2$species
latitude <- gbif_data_norway2$decimalLatitude
longitude <- gbif_data_norway2$decimalLongitude
source <- rep("gbif", length(species))

# Create a data frame for GBIF data
data_gbif3 <- data.frame(species, latitude, longitude, source)

# Extract relevant data from GBIF occurrences for Arctic fox in Sweden
species <- gbif_data_sweden2$species
latitude <- gbif_data_sweden2$decimalLatitude
longitude <- gbif_data_sweden2$decimalLongitude
source <- rep("gbif", length(species))

# Create a data frame for GBIF data
data_gbif4 <- data.frame(species, latitude, longitude, source)


###############################################################################
###############################################################################
###############################################################################

# Get iNaturalist occurrence data for the different species in Sweden and Norway
vulpes_vulpesS <- get_inat_obs(query = "Vulpes vulpes",place_id = "sweden") 
vulpes_vulpesN <- get_inat_obs(query = "Vulpes vulpes",place_id = "norway") 

vulpes_lagopusS <- get_inat_obs(query = "Vulpes lagopus",place_id = "sweden") 
vulpes_lagopusN <- get_inat_obs(query = "Vulpes lagopus",place_id = "norway") 


# Plot iNaturalist occurrences on a map of Sweden and Norway
ggplot(data = SweNor) +
  geom_sf() +
  geom_point(data = vulpes_vulpesS, aes(x = longitude, y = latitude), size = 4, 
             shape = 23, fill = "darkred") + theme_classic()
#The plot was only for Red fox in Sweden, make also with the other data if wanted, just to verify. 

##################################################
#Extract the data for Red fox in Sweden
species <- vulpes_vulpesS$scientific_name
latitude <- vulpes_vulpesS$latitude
longitude <- vulpes_vulpesS$longitude
source <- rep("inat", length(species))

# Create a data frame for iNaturalist data
data_inat1 <- data.frame(species, latitude, longitude, source)

#Extract the data for Red fox in Norway
species <- vulpes_vulpesN$scientific_name
latitude <- vulpes_vulpesN$latitude
longitude <- vulpes_vulpesN$longitude
source <- rep("inat", length(species))

# Create a data frame for iNaturalist data
data_inat2 <- data.frame(species, latitude, longitude, source)

#Extract the data for Arctic fox in Sweden
species <- vulpes_lagopusS$scientific_name
latitude <- vulpes_lagopusS$latitude
longitude <- vulpes_lagopusS$longitude
source <- rep("inat", length(species))

# Create a data frame for iNaturalist data
data_inat3 <- data.frame(species, latitude, longitude, source)


#Extract the data for Arctic fox in Norway
species <- vulpes_lagopusN$scientific_name
latitude <- vulpes_lagopusN$latitude
longitude <- vulpes_lagopusN$longitude
source <- rep("inat", length(species))

# Create a data frame for iNaturalist data
data_inat4 <- data.frame(species, latitude, longitude, source)



#############################################################################
#############################################################################

# Combine the different GBIF and iNat data frames
matrix_full <- rbind(data_gbif1, data_gbif2, data_gbif3, data_gbif4, data_inat1, data_inat2, data_inat3, data_inat4)
#The iNat data have several names for the Red Fox: Vulpes, Vulpes vulpes and Vulpes vulpes vulpes
unique(matrix_full$species)
#Remove Vulpes vulpes vulpes and Vulpes from column species: 
matrix_full <- matrix_full[-grep("Vulpes vulpes vulpes",matrix_full$species),]
matrix_full <- matrix_full[!(matrix_full$species == "Vulpes"),]

# Plot combined data on a map of Norway and Sweden
p1 <- ggplot(data = SweNor) +
  geom_sf() +
  geom_point(data = matrix_full, aes(x = longitude, y = latitude, fill = species), size = 2, 
             shape = 23) + theme_classic()
 
print(p1)

#Arctic fox are not present in the southern parts, but Red Fox are present everywhere on the map