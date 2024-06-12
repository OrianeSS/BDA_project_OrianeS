#install.packages("ggplot2") #creation of graphs and plots
#install.packages("ggcorrplot") #creation of correlation matrix
#install.packages("ggfortify") #for plotting PCA
#install.packages("corrplot")  # for correlation
#install.packages("pheatmap")  #for the heatmap
#install.packages("plotly")     #to create interactive plots
#install.packages("randomcoloR") #for the heatmap 
#install.packages("emmeans")     #calcules estimated margine means useful for post-hoc test
#install.packages("ggridges")    #for the creation of ridgeline plots
#install.packages("fmsb")       #for the creation of spider plots


# Load necessary libraries for the script
library(ggplot2)
library(ggcorrplot)
library(ggfortify)
library(corrplot)
library(pheatmap)
library(plotly)
library(randomcoloR)
library(emmeans)
library(ggridges)
library(fmsb)

####################################################################################
################## Data Exploration 

# Load and clean the dataset
df <- matrix_full_eco_elev_clim_sat
df <- na.omit(df)  # Remove rows with missing values

# Separate continuous and discrete variables
df_continous <- df[, colnames(df) %in% c("vec_mean_temp", "elevation", "NDVI", "vec_mean_prec")]
df_discrete <- df[, !(colnames(df) %in% c("vec_mean_temp", "elevation", "NDVI", "vec_mean_prec"))] #taking all the columns except the continous variables indicated by "!". 

####################################################################################
##### Correlation Matrix and Corplot

### Compute the correlation matrix for continuous variables
mydata.cor <- cor(df_continous)
print(mydata.cor)

### Plot the correlation matrix with hierarchical clustering
my_corplot <- corrplot(mydata.cor, order = 'hclust', addrect = 3) #the biggest and darkest circles have the strongest correlation
#the strongest positive correlation is between NDVI and temperature.
#the strongest negative correlation is between elevation and temperature and elevation dans NDVI.

####################################################################################
##### Annotated Heatmap 

### Simple heat map using only numeric values 
# Prepare data for heatmap
data <- df_continous
row.names(data) <- c(1:nrow(data)) #create row names for continous data

### Generate a basic heatmap
#heatmap(scale(data)) 

### Advanced heat map with annotation 
## Factor for annotation
my_group <- df_discrete[c("Landcover", "species")]  #select the columns of the wanted discrete data
row.names(my_group) <- c(1:nrow(my_group)) #create row names

## Generate an advanced heatmap with annotations
#pheatmap(scale(data),
#        annotation_row = my_group)

## Customize the heatmap
# Define custom colors for the heatmap
data_col <- grDevices::colorRampPalette(c("black", "darkblue", "white", "darkred"))

# Make the customized heatmap
ht <- pheatmap(scale(data),
         annotation_row = my_group,
         cutree_rows = 2,
         cutree_cols = 2,
         cellwidth = 100,
         cellheight = 0.2,
         color = data_col(10))
print(ht)
#vec_mean_temp and NDVI are more closely related, as they are clustered together.
#elevation and vec_mean_prec are also clustered and more related. 
#The color bars for species and landcover indicate how these categorical variables are distributed among the clusters.
#Darker blue colors means lower values and darker red means higher values


####################################################################################
####################################################################################
#### Basic Statistics 

### Correlation between continuous variables 

# Prepare data for statistical analysis
data_stat1 <- df_continous

# Create a scatter plot with a linear regression line
P <- ggplot(data = data_stat1, mapping = aes(x = vec_mean_temp, y = vec_mean_prec))
P + geom_point(shape = 18) + 
  geom_smooth(method = "lm", se = FALSE) + 
  theme_classic()
#we can see that higher temperature have slighlty less precipitations. 

# Perform Pearson correlation test
cor.test(data_stat1$vec_mean_temp, data_stat1$vec_mean_prec) # there is a significant negative correlation between temperature and precipitation. Higher temperatures have less precipitation. 
cor.test(data_stat1$elevation, data_stat1$vec_mean_prec) # there is a significant positive correlation between elevation and precipitations, indicating higher elevations have more precipitation.
cor.test(data_stat1$NDVI, data_stat1$vec_mean_prec) # there is a significant negative correlation between NDVI and precipitations, indicating higher vegetation densities have less precipitation.
cor.test(data_stat1$elevation, data_stat1$vec_mean_temp) #there is a significant strong negative correlation between elevation and temperature. This is absolutely normal. 
cor.test(data_stat1$NDVI, data_stat1$vec_mean_temp) #there is a significant strong positive correlation between NDVI and temperature. The higher the temperature the more vegetation densities. 
cor.test(data_stat1$NDVI, data_stat1$elevation) #there is a significant strong negative correlation between NDVI and elevation. The higher the elevation the less vegetation densities. 

# Fit a linear model for mean temperatures and mean precipitations
linear_model <- lm(vec_mean_temp ~ vec_mean_prec, data = data_stat1)
summary(linear_model)
anova(linear_model) #p-value < 0,05 : the model including precipitation as a predictor is statistically significant.


####################################################################################
####################################################################################
#### Factor Analysis

# Use the original dataset for factor analysis
data_stat2 <- matrix_full_eco_elev_clim_sat

# Create a boxplot for temperature by species
P_fact1 <- ggplot(data = data_stat2, mapping = aes(x = species, y = vec_mean_temp, fill = species))

P_fact1 <- P_fact1 + geom_boxplot(varwidth = TRUE, outlier.shape = NA) +  # Change boxplot width 
  geom_jitter(alpha = 0.2, size = 2, width = 0.1) +  # Add points and spread them
  stat_summary(fun = mean, shape = 13, size = 1, colour = "darkgreen") +  # Add mean 
  theme_classic()

print(P_fact1)

linear_model1 <- lm(vec_mean_temp ~ species, data = data_stat2)
anova(linear_model1) # p-value < 0,05: indicates that temperature has a significant effect on the species. 



# Create a boxplot for temperature by landcover type
P_fact2 <- ggplot(data = data_stat2, mapping = aes(x = Landcover, y = vec_mean_temp, fill = Landcover))

P_fact2 <- P_fact2 + geom_boxplot(varwidth = TRUE, outlier.shape = NA) +  # Change boxplot width 
  geom_jitter(alpha = 0.2, size = 2, width = 0.1) +  # Add points and spread them
  stat_summary(fun = mean, shape = 13, size = 1, colour = "darkgreen") +  # Add mean 
  theme_classic()

print(P_fact2)
#the highest temperatures are in grassland and the lowest in non-vegetated

#create an interactive plot 
ggplotly(P_fact2) #ggplotly is an interactive plot. 

# Fit a linear model with landcover as a factor
linear_model2 <- lm(vec_mean_temp ~ Landcover, data = data_stat2)
# Perform ANOVA on the linear model
anova(linear_model2) # p-value < 0,05: indicates that the Landcover variable has a significant effect on the response variable mean temperatures.


####################################################################################
### Post-hoc test

# Conduct post-hoc tests with Tukey adjustment for Landcover
library(emmeans)
em2 <- emmeans(linear_model2, list(pairwise ~ Landcover), adjust = "tukey")
print(em2) #Results show how different Landcover types relate to mean temperature. 
#There are only 3 non-signficant results: Cropland-Grassland, Cropland-Settlement and Grassland-settlement. 
#Indicating a statistically non-significant mean temperature difference between these two Landcover types.


# Conduct post-hoc tests with Tukey adjustment for species
library(emmeans)
em1 <- emmeans(linear_model1, list(pairwise ~ species), adjust = "tukey")
print(em1) 
#Vulpes lagopus has an estimated marginal mean temperature (emmean) of −1.04
#Vulpes vulpes has an estimated marginal mean temperature of 5.11
#The estimated difference in mean temperatures between Vulpes lagopus and Vulpes vulpes is −6.15
#This negative value indicates that Vulpes lagopus has a lower mean temperature compared to Vulpes vulpes.
#The p-value < 0,05 indicates high significance


####################################################################################
#### Multivariate Plot

###PCA
df_continous2 <- apply(df_continous,2,as.numeric)

pca_res <- prcomp(df_continous2, scale. = TRUE)

PCA <- autoplot(pca_res, data = df_discrete, colour = 'species',
         loadings = TRUE, loadings.colour = 'black',
         loadings.label = TRUE, loadings.label.size = 3, frame = TRUE, frame.type = 'norm') + ggtitle("my project") + theme_classic()
PCA
#There is minimal niche overlap between the two species as the two ellipses only overlap a little. This suggests that the two species have their own niches and some specific environmental conditions are overlapping. 
#Vulpes lagopus (red) is mostly clustered to the left side of the plot, suggesting it is associated with lower values of PC1, which might correspond to lower NDVI and mean temperature, but higher elevations and precipitations.
#Vulpes vulpes (blue) is clustered more towards the right, indicating higher values of PC1, potentially higher NDVI and mean temperature, but lower elevations and precipitations.


##interactive PCA
library(plotly)
intercative_pca <- ggplotly(PCA)
intercative_pca 



############################################################################################# 
#####################PLOTS

# basic plot
ggplot(matrix_full_eco_elev_clim_sat, aes(x = vec_mean_temp, y = species, fill = species)) +
  geom_density_ridges() +
  theme_ridges() + 
  theme(legend.position = "none")
#We can see that the arctic fox is associated with lower temperatures than the red fox and there are some temperature where both species are present. 


###############################################
################ More PLOTS

#Use the whole dataset
data_stat2 <- matrix_full_eco_elev_clim_sat

# Aggregate the data by species
aggregated_data_species <- aggregate(cbind(elevation, vec_mean_prec, vec_mean_temp, NDVI) ~ species, 
  data = data_stat2, 
  FUN = mean
)
# show the results
print(aggregated_data_species)


min1 <- c(160.6350,66.92227 , -1.036564, -0.03030266)
max2 <- c(apply(aggregated_data_species[,2:5],2,max))
sp1 <- aggregated_data_species[1,2:5]
sp2 <- aggregated_data_species[2,2:5]
row_id <- c(1,2,aggregated_data_species$species)
 
aggregated_data_species <- rbind(min1,max2, sp1, sp2)
row.names(aggregated_data_species) <- row_id
#row.names(aggregated_data_species) <- c("min", "max", "Vulpes lagopus", "Vulpes vulpes")

print(aggregated_data_species)

# Add a colors of legend and plots
colors_in <- c(rgb(0.8, 0.2, 0.5, 0.4), rgb(0.2, 0.5, 0.5, 0.4), rgb(0.7, 0.5, 0.1, 0.4))
colors_border <- c(rgb(0.8, 0.2, 0.5, 0.9), rgb(0.2, 0.5, 0.5, 0.9), rgb(0.7, 0.5, 0.1, 0.9))

#make the radarchart
radarchart(aggregated_data_species, pcol=colors_border , pfcol=colors_in , plwd=4 , plty=1,)
#add legends
legend(x = 0.7, y = 1, legend = row.names(aggregated_data_species[-c(1, 2), ]), bty = "n", pch = 20, col = c("lightblue", "pink"), text.col = "grey", cex = 1.2, pt.cex = 3)

#There is some overlap between the two species in the central area of the radarchart, indicating that there are some common environmental conditions.
#Vulpes lagopus is associated with higher elevations and precipitations 
#Vulpes vulpes is associated with higher NDVI and temperatures



#################### FINAL COMMENTS ###############################
#With all these plots we can see that as Vulpes vulpes is associated with higher temperatures, it could be more and more adapted to higher latitudes due to climate change.
#There is already a slight niche overlap, but Red foxes could cross more and more the niche of Arctic foxes due to temperature rise.
#Arctic foxes are associated with lower temperatures, so with climate change they will have to move upper and upper in elevation but also latitudes. 















###############################################################################33
####################################################################################
######### For information Data Aggregation and Formatting for Plots (not used here)

# Use the original dataset for data aggregation
#data_stat <- matrix_full_eco_elev_clim_sat

# Aggregate data by W_Ecosystm
#aggregated_data <- aggregate(
#  cbind(elevation, vec_mean_prec, vec_mean_temp, NDVI) ~ W_Ecosystm, 
#  data = data_stat, 
#  FUN = mean
#)

# Print the aggregated data
#print(aggregated_data)

############## Add factor value: 

# Extract species and W_Ecosystm columns
#data_stat_discrete <- data_stat[c("species", "W_Ecosystm")]

# Merge aggregated data with discrete data by W_Ecosystm
#aggregated_data_final_species <- merge(aggregated_data, data_stat_discrete, by = "W_Ecosystm")

# Load dplyr for data manipulation
#library(dplyr)

# Ensure unique rows in the final aggregated data
#aggregated_data_final_species <- aggregated_data_final_species %>% distinct()