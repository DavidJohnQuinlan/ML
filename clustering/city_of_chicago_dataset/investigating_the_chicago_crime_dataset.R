###########################################################################
#                                                                         #
#                Investigating the Chicago Crime Dataset                  # 
#                                                                         #
###########################################################################

#  ------------------------------------------------------------------------
# Import the Chicago crime dataset ----------------------------------------
#  ------------------------------------------------------------------------

# set the current directory
setwd(dirname(rstudioapi::getActiveDocumentContext()$path))

# read in the crime dataset
crime <- read.csv("./data/crime_data.csv")

# examine the crime dataset 
head(crime)
tail(crime)
str(crime)
summary(crime)
dim(crime)


#  ------------------------------------------------------------------------
# Subset the dataset ------------------------------------------------------
#  ------------------------------------------------------------------------

# subset the data for the year 2005 
crime_2005 <- subset(crime, crime$Year == 2005)

# Find the crime totals ---------------------------------------------------

# examine the counts of the total numbers of offenses and convert to a dataframe 
crime_totals <- data.frame(apply(crime_2005[,c(-1,-2,-3,-4)],2,sum))

# convert to another dataframe to include the rownames as a column 
crime_totals_df <- data.frame("Offences" = rownames(crime_totals), "Total" = crime_totals)

# reset the rownames to numbers 
row.names(crime_totals_df) <- NULL

# change the column names to meaningful values 
colnames(crime_totals_df) <- c("Offence", "Total")

# put this dataframe in order 
crime_totals_df[order(crime_totals_df$Total, decreasing = T),]

# choose 3 variables of my choice 
# I chose the variables theft, narcotics and homicide 
my_crime <- crime_2005[,c("Number_Community", "Name_Community", "Population", 
                          "THEFT", "NARCOTICS", "HOMICIDE")]

# add the row names to the dataset that are the community areas 
rownames(my_crime) <- my_crime$Name_Community


#  ------------------------------------------------------------------------
# Find the row totals  ----------------------------------------------------
#  ------------------------------------------------------------------------

# find the row totals of the number of crimes committed within each community 
crime_2005$Crime_Total <- apply(crime_2005[c(-1,-2,-3,-4,-5)], 1, sum)

# and divide my the population size to find an estimate total crime rate for each community 
crime_2005$Total_Crime_Rate <- crime_2005$Crime_Total/crime_2005$Population

#  ------------------------------------------------------------------------
# Examine the my_crime dataset --------------------------------------------
#  ------------------------------------------------------------------------

# examine these this new dataset in more detail 
head(my_crime)
tail(my_crime)
str(my_crime)
summary(my_crime)


#  ------------------------------------------------------------------------
# Initial Exploratory analysis --------------------------------------------
#  ------------------------------------------------------------------------

#  ------------------------------------------------------------------------
# Create some plots of the data -------------------------------------------
#  ------------------------------------------------------------------------

#  ------------------------------------------------------------------------
# Create some boxplots ----------------------------------------------------
#  ------------------------------------------------------------------------

# create some simple boxplots 
par(mfrow = c(1,3), mar = c(2,3,2,0.25))
boxplot(my_crime$THEFT, main = "Theft")
boxplot(my_crime$NARCOTICS, main = "Narcotics")
boxplot(my_crime$HOMICIDE, main = "Homocide")
par(mfrow = c(1,1))

# create a nicer boxplot using the libraries reshape and ggplot 
# load the necessary libraries 
library(reshape)
library(ggplot2)
library(ggpubr, warn.conflicts = F)

# create a theme for the boxplots 
boxplot_theme <- theme(axis.text.x=element_text(size=15, face = "bold"),
                       axis.title.x=element_blank(), 
                       axis.title.y = element_blank(),
                       plot.title = element_text(hjust = 0.5, size = 18, face = "bold"), 
                       legend.position="none",
                       axis.title=element_text(size=25,face="bold"),
                       text = element_text(size=20))

# create boxplots of the different variables and then put them together 
Theft_boxplot <- ggplot(my_crime, aes(x="Theft", y=THEFT)) + geom_boxplot(fill = "#F8766D") + boxplot_theme
Narcotics_boxplot <- ggplot(my_crime, aes(x="Narcotics", y=NARCOTICS)) + geom_boxplot(fill = "#9EB9D4") + boxplot_theme
Homicide_boxplot <- ggplot(my_crime, aes(x="Homicide", y=HOMICIDE)) + geom_boxplot(fill = "Yellow") + boxplot_theme

# arrange the different boxplots into one page 
ggarrange(Theft_boxplot, Narcotics_boxplot, Homicide_boxplot, ncol = 3, nrow = 1)


#  ------------------------------------------------------------------------
# Create some histograms --------------------------------------------------
#  ------------------------------------------------------------------------

# plot the histogram of each variable 
par(mfrow =c(1,3))
hist((my_crime$THEFT), cex.main=2.5, 
     main = expression(paste("Theft, ", mu, "= 1112.66, ", sigma, "=966.57")),
     xlab = "Theft", col = "#F8766D", cex.lab = 1.5, cex.axis = 1.5)
hist(my_crime$NARCOTICS, cex.main=2.5, 
     main = expression(paste("Narcotics, ", mu, "=730.23, ", sigma, "=1117.09")),
     xlab = "Narcotics", col = "#9EB9D4", cex.lab = 1.5, cex.axis = 1.5)
hist(my_crime$HOMICIDE, cex.main=2.5, 
     main = expression(paste("Homicide, ", mu, "=5.88, ", sigma, "=6.97")),
     xlab = "Homicide", col = "Yellow", cex.lab = 1.5, cex.axis = 1.5)
par(mfrow = c(1,1))


# Find the summary statistics for Theft, Homicide and Narcotics ---------------------

# find the means of both variables 
mean(my_crime$THEFT)
mean(my_crime$NARCOTICS)
mean(my_crime$HOMICIDE)

# find the standard devation of both variables 
sd(my_crime$THEFT)
sd(my_crime$NARCOTICS)
sd(my_crime$HOMICIDE)


#  ------------------------------------------------------------------------
# Create a correlation matrix ---------------------------------------------
#  ------------------------------------------------------------------------

# create a correlation matrix 
pairs(my_crime[,c(4:6)], cex.labels = 2.5)

# calculate the correlation matrix 
cor(my_crime[,c(4:6)])


#  ------------------------------------------------------------------------
# Conduct some clustering analysis ----------------------------------------
#  ------------------------------------------------------------------------

#  ------------------------------------------------------------------------
# Find the crime rate per community ---------------------------------------
#  ------------------------------------------------------------------------

# divide the crime counts by the population for the year 2005 
# I feel it is better to compare similar rates of crime against each other 
my_crime$THEFT_Rate <- my_crime$THEFT/my_crime$Population
my_crime$NARCOTICS_Rate <- my_crime$NARCOTICS/my_crime$Population
my_crime$HOMICIDE_Rate <- my_crime$HOMICIDE/my_crime$Population


#  ------------------------------------------------------------------------
# Scale the data ----------------------------------------------------------
#  ------------------------------------------------------------------------

# so as to not place any additional weight on certain variables over others 
# it was decided to scale the data 
my_crime$THEFT_Scaled <- scale(my_crime$THEFT_Rate)
my_crime$NARCOTICS_Scaled <- scale(my_crime$NARCOTICS_Rate)
my_crime$HOMICIDE_Scaled <- scale(my_crime$HOMICIDE_Rate)


#  ------------------------------------------------------------------------
# K-means -----------------------------------------------------------------
#  ------------------------------------------------------------------------

# cluster the dataset using k-means clustering 
# run a cluster analysis for the number of clusters 1 to 15 
K <- 15

# create an empty vector to store the WCSS values 
WCSS <- rep(NA, K)

# for loop that calculates the WCSS for each cluster of sizes 1 to 15 
for (k in 1:15){
  WCSS[k] <- kmeans(my_crime[,c(10:12)], centers = k, nstart = 20)$tot.withinss
}

# plot the within cluster sum of squares against k 
plot(WCSS, type = "b", xlab = "Number of clusters, k", ylab = "Within cluster sum of squares",
     main = "Plot of the within cluster sum of squares against the number of clusters, k",
     cex.lab = 1.5, cex.main = 2)
# there does not appear to be a clear and obvious elbow present within the plot 
# however there does appear to be a slight elbow at k = 3 or 4.
# this means that 3/4 clusters appear to be the optimal number of clusters
# present within the dataset.

# fit the best cluster model 
my_crime_kmeans_optim <- kmeans(my_crime[,c(10:12)], centers = 4, nstart = 1000)

# plot the scatterplot of the optimal clustering solution 
plot(my_crime[,c(10:12)], col = my_crime_kmeans_optim$cluster, pch = 16, cex.labels = 2.5)

# append cluster assignment 
my_crime_kcluster <- data.frame(my_crime, my_crime_kmeans_optim$cluster)

# identify the community which has the very large theft rate 
my_crime[my_crime$THEFT_Scaled > 4,]
# the Loop community has the largest scaled theft rate 


#  ------------------------------------------------------------------------
# Clusplot ----------------------------------------------------------------
#  ------------------------------------------------------------------------

# load the cluster library 
library(cluster) 

# examine the cluster results of the k-means in more detail 
clusplot(my_crime_kcluster, my_crime_kmeans_optim$cluster, color=TRUE, shade=TRUE, 
         labels=2, lines=0, main = "Clusplot of the 3 cluster k-means model")
# there appears to be 4 well formed groups according to the clusplot 


#  ------------------------------------------------------------------------
# Examine some hierarchical clustering ------------------------------------
#  ------------------------------------------------------------------------

#  ------------------------------------------------------------------------
# Complete Linkage --------------------------------------------------------
#  ------------------------------------------------------------------------

# examine the cluster similarities using euclidean distance and complete linkage 
my_crime_CL = hclust(dist(my_crime[,c(10:12)]), method = "complete")

# plot the complete linkage dendrogram 
plot(as.dendrogram(my_crime_CL, hang = 0.01), ylab="Complete Linkage")
# poor structure


#  ------------------------------------------------------------------------
# Single Linkage ----------------------------------------------------------
#  ------------------------------------------------------------------------

# examine the cluster similarities using euclidean distance and single linkage 
my_crime_SL = hclust(dist(my_crime[,c(10:12)]), method = "single")

# plot the dendrogram 
plot(as.dendrogram(my_crime_SL,hang = 0.01), ylab="Single Linkage")
# not a very good structure present 


#  ------------------------------------------------------------------------
# Average Linkage ---------------------------------------------------------
#  ------------------------------------------------------------------------

# examine the cluster similarities using euclidean distance and average linkage 
my_crime_AL = hclust(dist(my_crime[,c(10:12)]), method = "average")

# plot the dendrogram 
plot(as.dendrogram(my_crime_AL, hang = 0.01), ylab="Average Linkage")
# also not a very good structure, I may need to examine other forms of linkage method 


#  ------------------------------------------------------------------------
# Ward.2D Linkage ---------------------------------------------------------
#  ------------------------------------------------------------------------

# Ward's linkage method examines cluster analysis problems as analysis of variance problems. 
# therefore, clusters/groups are merged when the error sum of squares is minimised.

# examine the cluster similarities using euclidean distance and the ward.D2 linkage 
my_crime_WD2L = hclust(dist(my_crime[,c(10:12)]), method = "ward.D2")

# plot the dendrogram 
plot(as.dendrogram(my_crime_WD2L), ylab="Ward.D2 Linkage", main = "Dendrogram created using hierarchical clustering algorithm with euclidean distance measurement and Wards linkage method")
# 4 clusters appear to be the most likely from the dendrogram 


#  ------------------------------------------------------------------------
# Using Mclsut to find the clusters  --------------------------------------
#  ------------------------------------------------------------------------

# load the mclust library 
library(mclust)

# fit an mclust model to the data where the number of mixture components ranges 
# from 2 to 10 
my_crime_mclust <- Mclust(my_crime[,c(10:12)], G=2:10) 
my_crime_mclust$class

# find the best model based by BIC 
my_crime_mclust$BIC
# the best models according to BIC are VEV,3 VVI,3 or VEE,3. 
# therefore the mclust best models would suggest that a 3 cluster model would 
# be optimal 


#  ------------------------------------------------------------------------
# Examine the silhouette plots best models --------------------------------
#  ------------------------------------------------------------------------

# load the cluster library 
library(cluster)


#  ------------------------------------------------------------------------
# Silhouette plots for k-means --------------------------------------------
#  ------------------------------------------------------------------------

# examining the silhouette plots for different size of k 
# for k = 4 
my_crime_kmeans_4 <- kmeans(my_crime[,c(10:12)], centers = 4, nstart = 1000)

# calculate the silhouette distances using the optimal model found above 
sil_k4 <- silhouette(my_crime_kmeans_optim$cluster, dist(my_crime[,c(10:12)]))

# plot the silhouette plot 
plot(sil_k4, col = c("#F8766D", "Yellow", "#9EB9D4","Yellow"), main = "Silhouette plot for k-means where k = 4")
# average silhouette width of 0.47 

# examine the silhouette plot for k=3 
my_crime_kmeans_3 <- kmeans(my_crime[,c(10:12)], centers = 3, nstart = 1000)

# calculate the silhouette distances using the optimal model found above 
sil_k3 <- silhouette(my_crime_kmeans_3$cluster, dist(my_crime[,c(10:12)]))

# plot the silhouette plot 
plot(sil_k3, main = "Silhouette plot for k-means where k = 3") # with cluster-wise coloring
# average silhouette width of 0.56

# it is found that the highest silhouette width is found for k = 3


#  ------------------------------------------------------------------------
# Silhouette plots for mclust ---------------------------------------------
#  ------------------------------------------------------------------------

# examine the silhouette plot for the best mclust model 
sil_m <- silhouette(my_crime_mclust$class, dist(my_crime[,c(10:12)]))

# plot the silhouette plot for the mclust model 
plot(sil_m)
# the mixture model does not appear to be a good fit the dataset, when 
# examining the silhouette plot 


#  ------------------------------------------------------------------------
# Examine the silhouette plots for hierarchical clustering ----------------
#  ------------------------------------------------------------------------

# examine a silhouette plot for the average linkage 
sil_AL <- silhouette(cutree(my_crime_AL,4), dist(my_crime[,c(10:12)]))

# plot the dendrogram 
plot(sil_AL, main = "Silhouette plot of the 4 cluster average linkage hierarchical clustering solution")


#  ------------------------------------------------------------------------
# Silhouette plot for wards linkage ---------------------------------------
#  ------------------------------------------------------------------------

# examine a silhouette plot for the ward.D2 linkage 
sil_WD2 <- silhouette(cutree(my_crime_WD2L,4), dist(my_crime[,c(10:12)]))

# plot the dendrogram 
plot(sil_WD2, main = "Silhouette plot of the 4 cluster average linkage hierarchical clustering solution")
# the highest silhouette width is achieved for 3/4 clusters
# however the 4 cluster model has less negative values then the 3 cluster model 


#  ------------------------------------------------------------------------
# Compare the cluster solutions -------------------------------------------
#  ------------------------------------------------------------------------

#  ------------------------------------------------------------------------
# Adjusted Rand Index -----------------------------------------------------
#  ------------------------------------------------------------------------

# use the adjusted rand index to compare the cluster solutions 
# compare the 3 k-means and the 4 hierarchical clustering cluster solutions 
adjustedRandIndex(my_crime_kmeans_3$cluster, cutree(my_crime_WD2L, 4))

# compare the k-means where k = 4 and the wards linkage clustering solutions 
adjustedRandIndex(my_crime_kmeans_4$cluster, cutree(my_crime_WD2L, 4))


#  ------------------------------------------------------------------------
# Rand Index --------------------------------------------------------------
#  ------------------------------------------------------------------------

# load the fossil library 
library(fossil)

# compare the 3 k-means and the wards linkage clustering solutions 
rand.index(my_crime_kmeans_3$cluster, cutree(my_crime_WD2L, 4))

# compare the 4 k-means and the wards linkage clustering solutions 
rand.index(my_crime_kmeans_4$cluster,cutree(my_crime_WD2L,4))


#  ------------------------------------------------------------------------
# Create a table of the k-means against HC clusters -----------------------
#  ------------------------------------------------------------------------

# create a table of the 4 cluster solutions for k-means and HC against each other 
tab_clust <- table(my_crime_kmeans_4$cluster, cutree(my_crime_WD2L, 4))

# load the xtable library 
library(xtable)

# create a table for latex 
xtable(tab_clust, type = "latex")


#  ------------------------------------------------------------------------
# Mapping -----------------------------------------------------------------
#  ------------------------------------------------------------------------

# create a map of the 77 communities present within Chicago and then use 
# colours to indicate which neighborhoods show similar outcomes according 
# to the different clustering solutions 

# load the ggplot2 and maptools libraries 
library(ggplot2)
library(maptools) 
library(RColorBrewer)

# load the Chicago shapefile 
chicago <- readShapePoly("./data/chicago_shapefile/geo_export_c4b95e57-70e1-4051-891f-d73b0e62dba5")

# organise the data into a data frame 
chicago <- fortify(chicago) 

# create a plot of the Chicago area 
ggplot(chicago, aes(x=long, y=lat, group=group)) + geom_polygon(colour = "black")

# fix the crime data to allow it to be plotted onto the map 
# create a new crime dataframe 
crime_2005_new <- crime_2005

# add the k-means, hierarchical clustering and mclust clustering solutions to the 
# crime_2005_new dataset 
crime_2005_new$k1cluster <- my_crime_kmeans_3$cluster
crime_2005_new$kcluster <- as.factor(my_crime_kmeans_4$cluster)
crime_2005_new$mcluster <- my_crime_mclust$class
crime_2005_new$hcluster <- as.factor(cutree(my_crime_WD2L, 4))

# find the total number of crimes across thief, homicide and narcotics 
# find the rowsums of my_crime dataset 
# append the community id's on to the dataset first 
my_crime1 <- data.frame(crime_2005$Number_Community, my_crime)
crime_2005_new$My_crime_Total <- apply(my_crime1[,c(5:7)], 1, sum)

# change the column name in crime_2005_new from Number_Community to id 
colnames(crime_2005_new)[1] <- "id"

# merge these values with the Chicago shapefile dataframe by id 
chicago_data <- merge(chicago, crime_2005_new, by = "id")

# use RColorBrewer to identify four colours which will be useful to help 
# identify certain similar clusters within the different maps 
display.brewer.pal(n = 4, name = 'Blues')
brewer.pal(n = 4, name = 'Blues')

# find the totals for each community 
my_crime_totals_2005 <- apply(my_crime[,c(4:6)], 1, sum)

# find the rates for each community 
my_crime_totals_2005_rates <- my_crime_totals_2005/my_crime$Population

# I also need to identify which clusters have more crimes then the others 
aggregate(my_crime_totals_2005_rates, list(my_crime_kmeans_4$cluster), sum)
# therefore the cluster with the worst crime rate is as follows: 3,1,2,4 


# Create the maps ---------------------------------------------------------

# now create the different maps for the different cluster solutions 
# k-means cluster solution 
cols_k <- c("3" = "#F8766D", "1" =  "chocolate1", "2" = "Yellow", "4" = "#2171B5")
kcluster_map <- ggplot(chicago_data, aes(x=long, y=lat, group=group, fill = kcluster)) + 
  geom_polygon(colour = "black") + scale_fill_manual(values = cols_k) +
  ggtitle("4 Cluster K-means")
# it is clear that there are certain areas which are more similar then others 

# Ward's linkage hierarchical clustering solution 
cols <- c("4" = "#F8766D", "3" =  "chocolate1", "1" = "Yellow", "2" = "#2171B5")
hcluster_map <- ggplot(chicago_data, aes(x=long, y=lat, group=group, fill = hcluster)) +
  geom_polygon(colour = "black") + scale_fill_manual(values = cols) +
  ggtitle("4 Cluster Hierarchical Clustering")
# all of these areas seem to identify those areas which are more similar then others

# create a map showing the crime rates for each neighborhood 
total_crime_rate_map <- ggplot(chicago_data, aes(x=long, y=lat, group=group, fill = Total_Crime_Rate)) + 
  geom_polygon(colour = "black") + 
  scale_fill_distiller(type = "seq", palette = "YlOrRd", direction = 1) +
  ggtitle("Total Crime Rates For 2005")

# place all the maps on the same plot 
ggarrange(kcluster_map ,hcluster_map, total_crime_rate_map, ncol = 3, nrow = 1)
