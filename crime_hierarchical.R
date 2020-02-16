library(dplyr)

## read clean data
clean_crime <- read.csv("1Percent_Clean_Chicago_Crime_Dataset.csv")

## outliers
out1 <- boxplot(clean_crime$Latitude,plot=FALSE)
out2 <- boxplot(clean_crime$Longitude,plot=FALSE)

clean_crime <- clean_crime[!(clean_crime$Latitude %in% out1$out | clean_crime$Longitude %in% out2$out),]   #60830 rows



## Transfer time to number

clean_crime$case_date_num <- as.numeric(difftime(as.Date(clean_crime$Case_Date), min(as.Date(clean_crime$Case_Date)),units=c("days"))) + 1

clean_crime$updated_date_num <- as.numeric(difftime(as.Date(clean_crime$Updated_Date), min(as.Date(clean_crime$Updated_Date)),units=c("days"))) + 1

clean_crime$case_time_num <- as.numeric(difftime(strptime(clean_crime$Case_Time,format='%H:%M:%S'), min(strptime(clean_crime$Case_Time,format='%H:%M:%S')),units=c("mins"))) + 1

clean_crime$updated_time_num <- as.numeric(difftime(strptime(clean_crime$Updated_Time,format='%H:%M:%S'), min(strptime(clean_crime$Updated_Time,format='%H:%M:%S')),units=c("mins"))) + 1




# time dimension
crime_time <- clean_crime[,c("IUCR","Arrest","case_date_num","case_time_num","updated_date_num","updated_time_num","Unsolved_Case_Elapsed_Time","Solved_Case_Elapsed_Time")]


#15435 rows
crime_time_T <- crime_time[which(crime_time$Arrest==TRUE), -7] 
#45638 rows
crime_time_F <- crime_time[which(crime_time$Arrest==FALSE), -8]

set.seed(123)
crime_time_F <- sample_n(crime_time_F, nrow(crime_time_F)*0.5)


# location dimension
crime_loc <- clean_crime[,c("IUCR","Arrest","Latitude","Longitude","Far_North_Side","Northwest_Side","North_Side","Central","West_Side","Southwest_Side","South_Side","Far_Southwest_Side","Far_Southeast_Side")]

crime_loc_T <- crime_loc[which(crime_loc$Arrest==TRUE), ]
crime_loc_F <- crime_loc[which(crime_loc$Arrest==FALSE), ]

set.seed(123)
crime_loc_F <- sample_n(crime_loc_F, nrow(crime_loc_F)*0.5)


# remove useless 
crime_time_T <- crime_time_T[, c(-2,-5,-6)]
crime_time_F <- crime_time_F[, c(-2,-5,-6)]
crime_loc_T <- crime_loc_T[,-2]
crime_loc_F <- crime_loc_F[,-2]


crime_time_F <- read.csv("crime_time_F_half_sample.csv")
row.names(crime_time_F) <- crime_time_F$X
crime_time_F <- crime_time_F[,-1]

## Standardization
normalize <- function(x) {
  return ((x - mean(x)) / sd(x))
}

# location dimension
crime_loc_T$IUCR <- normalize(crime_loc_T$IUCR)
crime_loc_T$Latitude <- normalize(crime_loc_T$Latitude)
crime_loc_T$Longitude <- normalize(crime_loc_T$Longitude)

crime_loc_F$IUCR <- normalize(crime_loc_F$IUCR)
crime_loc_F$Latitude <- normalize(crime_loc_F$Latitude)
crime_loc_F$Longitude <- normalize(crime_loc_F$Longitude)



# time dimension
crime_time_T$IUCR <- normalize(crime_time_T$IUCR)
crime_time_T$case_date_num <- normalize(crime_time_T$case_date_num)
crime_time_T$case_time_num <- normalize(crime_time_T$case_time_num)
crime_time_T$Solved_Case_Elapsed_Time <- normalize(crime_time_T$Solved_Case_Elapsed_Time)

crime_time_F$IUCR <- normalize(crime_time_F$IUCR)
crime_time_F$case_date_num <- normalize(crime_time_F$case_date_num)
crime_time_F$case_time_num <- normalize(crime_time_F$case_time_num)
crime_time_F$Unsolved_Case_Elapsed_Time <- normalize(crime_time_F$Unsolved_Case_Elapsed_Time)




library(tidyverse)  # data manipulation
library(cluster)    # clustering algorithms
library(factoextra) # clustering visualization
library(dendextend)

### crime_time_T
## Dissimilarity matrix
crime_time_T_d <- dist(crime_time_T, method = "euclidean")

#agglomeration method to be used (i.e. “complete”, “average”, “single”, “ward.D”)
# Hierarchical clustering using Complete Linkage
hc1 <- hclust(crime_time_T_d, method = "complete" )

res.hc <- eclust(crime_time_T, "hclust", k = 8,
                method = "complete", graph = FALSE) 

# Plot the obtained dendrogram
dend <- as.dendrogram(hc1)
dend <- color_branches(dend, k=8)
dend <- hang.dendrogram(dend,hang_height=0.1)
nodePar <- list(lab.cex = 0.6, pch = c(NA, 19), 
                cex = 0.7, col = "blue")
plot(dend, 
      main = "Clustered Crime data set
 (when arrest = TRUE)", ylab = "Height", leaflab = "none")


# Ward's method
hc5 <- hclust(crime_time_T_d, method = "ward.D2" )

# Cut tree into 4 groups (clusters);
sub_grp <- cutree(hc5, k = 7)

table(sub_grp)

#cutree output to add the the cluster each observation belongs to to our original data;
crime_time_T$cluster <- sub_grp 

## visualization
# Add borders to 4 clusters in dendrogram;
plot(hc5, cex = 0.6)
rect.hclust(hc5, k = 7, border = 2:5)


library(plot3D)

# x, y and z coordinates
x <- crime_time_T$case_date_num
y <- crime_time_T$case_time_num
z <- crime_time_T$IUCR
t <- crime_time_T$Solved_Case_Elapsed_Time
cluster <- as.numeric(crime_time_T$cluster)

scatter3D(x, y, z, colvar=cluster,xlab = "Case Date", ylab ="Case Time", zlab = "IUCR",theta = 60, phi = 20)

scatter3D(x, t, z, colvar=cluster,xlab = "Case Date", ylab = "Solving Duration", zlab ="IUCR",theta = 60, phi = 20)


library(animation)

saveGIF({
  for(i in 1:150){
    x <- crime_time_T$case_date_num
    y <- crime_time_T$case_time_num
    #f <- function(x, y) { sin(x) + cos(y) }
    z <- crime_time_T$IUCR
    t <- crime_time_T$Solved_Case_Elapsed_Time
    cluster <- as.numeric(crime_time_T$cluster)
    scatter3D(x, y, z, colvar=cluster,xlab = "Case Date", ylab ="Case Time", zlab = "IUCR", theta = 45 + (i * 0.5), phi = 35)
  }
}, interval = 0.1, ani.width = 550, ani.height = 550)





### crime_time_F
## Dissimilarity matrix

crime_time_F_d <- dist(crime_time_F, method = "euclidean")

#agglomeration method to be used (i.e. “complete”, “average”, “single”, “ward.D”)
# Hierarchical clustering using Complete Linkage
hc1 <- hclust(crime_time_F_d, method = "complete" )

res.hc <- eclust(crime_time_F, "hclust", k = 6,
                method = "complete", graph = FALSE) 


# Plot the obtained dendrogram
dend <- as.dendrogram(hc1)
dend <- color_branches(dend, k=6)
dend <- hang.dendrogram(dend,hang_height=-1)
nodePar <- list(lab.cex = 0.6, pch = c(NA, 19), 
                cex = 0.7, col = "blue")
plot(dend, 
      main = "Clustered Crime data set
 (when arrest = FALSE)", ylab = "Height", leaflab = "none")


# Ward's method
#hc5 <- hclust(crime_time_F_d, method = "ward.D2" )

# Cut tree into 4 groups (clusters);
sub_grp <- cutree(hc1, k = 6)

table(sub_grp)

#cutree output to add the the cluster each observation belongs to to our original data;
crime_time_F$cluster <- sub_grp 

## visualization
# Add borders to 4 clusters in dendrogram;
plot(hc1, cex = 0.6)
rect.hclust(hc1, k = 6, border = 2:5)



library(plot3D)

# x, y and z coordinates
x <- crime_time_F$case_date_num
y <- crime_time_F$case_time_num
z <- crime_time_F$IUCR
t <- crime_time_F$Solved_Case_Elapsed_Time
cluster <- as.numeric(crime_time_F$cluster)

scatter3D(x, y, z, colvar=cluster,xlab = "Case Date", ylab ="Case Time", zlab = "IUCR",theta = 60, phi = 20)

scatter3D(x, t, z, colvar=cluster,xlab = "Case Date", ylab = "Solving Duration", zlab ="IUCR",theta = 60, phi = 20)


library(animation)

saveGIF({
  for(i in 1:150){
    x <- crime_time_F$case_date_num
    y <- crime_time_F$case_time_num
    #f <- function(x, y) { sin(x) + cos(y) }
    z <- crime_time_F$IUCR
    t <- crime_time_F$Solved_Case_Elapsed_Time
    cluster <- as.numeric(crime_time_F$cluster)
    scatter3D(x, y, z, colvar=cluster,xlab = "Case Date", ylab ="Case Time", zlab = "IUCR", theta = 45 + (i * 0.5), phi = 35)
  }
}, interval = 0.1, ani.width = 550, ani.height = 550)





### crime_loc_T
## Dissimilarity matrix
crime_loc_T_d <- dist(crime_loc_T, method = "euclidean")

#agglomeration method to be used (i.e. “complete”, “average”, “single”, “ward.D”)
# Hierarchical clustering using Complete Linkage
hc1 <- hclust(crime_loc_T_d, method = "complete" )

# Plot the obtained dendrogram
plot(hc1, cex = 0.6, hang = -1)


# Ward's method
hc5 <- hclust(crime_time_T_d, method = "ward.D2" )

# Cut tree into 4 groups (clusters);
sub_grp <- cutree(hc1, k = 9)

table(sub_grp)

#cutree output to add the the cluster each observation belongs to to our original data;
crime_loc_T$cluster <- sub_grp 

## visualization
# Add borders to 4 clusters in dendrogram;
plot(hc1, cex = 0.6)
rect.hclust(hc1, k = 9, border = 2:5)


library(plot3D)

# x, y and z coordinates
x <- crime_loc_T$Latitude
y <- crime_loc_T$Longitude
z <- crime_loc_T$IUCR
cluster <- as.numeric(crime_loc_T$cluster)
sides <- as.numeric(crime_loc_T$Sides)


crime_loc_T$Sides <- factor(apply(crime_loc_T[,c(-1,-2,-3,-13)], 1, function(x) which(x == 1)), labels = colnames(crime_loc_T[,c(-1,-2,-3,-13)]))



scatter3D(x, y, z, colvar=cluster,xlab = "Latitude", ylab ="Longitude", zlab = "IUCR",theta = 60, phi = 20)

#scatter3D(x, y, z, colvar=sides,xlab = "Latitude", ylab ="Longitude", zlab = "IUCR",theta = 45, phi = 90, pch = shapes)


library(animation)

saveGIF({
  for(i in 1:150){
  	x <- crime_loc_T$Latitude
	y <- crime_loc_T$Longitude
	z <- crime_loc_T$IUCR
    #f <- function(x, y) { sin(x) + cos(y) }
    cluster <- as.numeric(crime_loc_T$cluster)
    scatter3D(x, y, z, colvar=cluster,xlab = "Latitude", ylab ="Longitude", zlab = "IUCR", theta = 45 + (i * 0.5), phi = 35)
  }
}, interval = 0.1, ani.width = 550, ani.height = 550)





### crime_loc_F
## Dissimilarity matrix
crime_loc_F_d <- dist(crime_loc_F, method = "euclidean")

#agglomeration method to be used (i.e. “complete”, “average”, “single”, “ward.D”)
# Hierarchical clustering using Complete Linkage
hc1 <- hclust(crime_loc_F_d, method = "complete" )

# Plot the obtained dendrogram
plot(hc1, cex = 0.6, hang = -1)


# Ward's method
hc5 <- hclust(crime_time_T_d, method = "ward.D2" )

# Cut tree into 8 groups (clusters);
sub_grp <- cutree(hc1, k = 8)

table(sub_grp)

#cutree output to add the the cluster each observation belongs to to our original data;
crime_loc_F$cluster <- sub_grp 

## visualization
# Add borders to 4 clusters in dendrogram;
plot(hc1, cex = 0.6)
rect.hclust(hc1, k = 9, border = 2:5)


library(plot3D)

# x, y and z coordinates
x <- crime_loc_F$Latitude
y <- crime_loc_F$Longitude
z <- crime_loc_F$IUCR
cluster <- as.numeric(crime_loc_F$cluster)
sides <- as.numeric(crime_loc_F$Sides)


crime_loc_F$Sides <- factor(apply(crime_loc_F[,c(-1,-2,-3,-13)], 1, function(x) which(x == 1)), labels = colnames(crime_loc_F[,c(-1,-2,-3,-13)]))



scatter3D(x, y, z, colvar=cluster,xlab = "Latitude", ylab ="Longitude", zlab = "IUCR",theta = 60, phi = 20)

#scatter3D(x, y, z, colvar=sides,xlab = "Latitude", ylab ="Longitude", zlab = "IUCR",theta = 45, phi = 90, pch = shapes)


library(animation)

saveGIF({
  for(i in 1:150){
  	x <- crime_loc_F$Latitude
	y <- crime_loc_F$Longitude
	z <- crime_loc_F$IUCR
    #f <- function(x, y) { sin(x) + cos(y) }
    cluster <- as.numeric(crime_loc_F$cluster)
    scatter3D(x, y, z, colvar=cluster,xlab = "Latitude", ylab ="Longitude", zlab = "IUCR", theta = 45 + (i * 0.5), phi = 35)
  }
}, interval = 0.1, ani.width = 550, ani.height = 550)










