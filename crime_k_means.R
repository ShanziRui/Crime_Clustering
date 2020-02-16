library(cluster.datasets)
library(tidyverse)
library(gridExtra)
library(dplyr)


## read clean data
clean_crime <- read.csv("1Percent_Clean_Chicago_Crime_Dataset.csv")


#Load the libraries

library(data.table)

library(dplyr)

library(formattable)

library(tidyr)

#Set a few color variables to make our table more visually appealing

customGreen0 = "#DeF7E9"

customGreen = "#71CA97"

customRed = "#ff7f7f"

customRed0 = "#ff9898"

customBlue = "#7f7fff"

customBlue0 = "#4c4cff"

customOrange = "#ffc04c"

customOrange0 = "#ffa500"


attach(clean_crime)

formattable(head(clean_crime))

#2) Add the color mapping for all attributes.
formattable(head(clean_crime[,c("ID","Case.Number","Date","Updated.On","Block","District","Ward","Community.Area","Latitude","Longitude","IUCR","Primary.Type","FBI.Code")]), align =c("l","c","c","c","c", "c", "c", "c", "r"), list(
  `Indicator Name` = formatter("span", style = ~ style(color = "grey",font.weight = "bold")), 
  `ID`= color_tile(customGreen, customGreen0),
  `Case.Number`= color_tile(customGreen, customGreen0),
  `Date`= color_tile(customRed, customRed0),
  `Updated.On`= color_tile(customRed, customRed0),
  `Block`= color_tile(customBlue, customBlue0),
  `District`= color_tile(customBlue, customBlue0),
  `Ward`= color_tile(customBlue, customBlue0),
  `Community.Area`= color_tile(customBlue, customBlue0),
  `Latitude`= color_tile(customBlue, customBlue0),
  `Longitude`= color_tile(customBlue, customBlue0),
  `IUCR`= color_tile(customOrange, customOrange0),
  `Primary.Type`= color_tile(customOrange, customOrange0),
  `FBI.Code`= color_tile(customOrange, customOrange0)
))


#3) Add the color mapping for time-related attributes.
formattable(head(clean_crime[,c("Date","Case_Date","Case_Time","Updated.On","Updated_Date","Updated_Time","Arrest","Unsolved_Case_Elapsed_Time","Solved_Case_Elapsed_Time")],10), align =c("l","c","c","c","c", "c", "c", "c", "r"), list(
  `Indicator Name` = formatter("span", style = ~ style(color = "grey",font.weight = "bold")), 
  `Date`= color_tile(customGreen, customGreen0),
  `Case_Date`= color_tile(customGreen, customGreen0),
  `Case_Time`= color_tile(customGreen, customGreen0),
  `Updated.On`= color_tile(customRed, customRed0),
  `Updated_Date`= color_tile(customRed, customRed0),
  `Updated_Time`= color_tile(customRed, customRed0),
  `Arrest`= color_tile(customBlue, customBlue0),
  `Unsolved_Case_Elapsed_Time`= color_tile(customBlue, customBlue0),
  `Solved_Case_Elapsed_Time`= color_tile(customBlue, customBlue0)
))




## outliers
out1 <- boxplot(clean_crime$Latitude,plot=FALSE)
out2 <- boxplot(clean_crime$Longitude,plot=FALSE)

clean_crime <- clean_crime[!(clean_crime$Latitude %in% out1$out | clean_crime$Longitude %in% out2$out),]   #60242 rows


p<-ggplot(clean_crime, aes(x=Arrest, y=Latitude, color=Arrest)) +
  geom_boxplot()

q<-ggplot(clean_crime, aes(x=Arrest, y=Longitude, color=Arrest)) +
  geom_boxplot()





## Transfer time to number

clean_crime$case_date_num <- as.numeric(difftime(as.Date(clean_crime$Case_Date), min(as.Date(clean_crime$Case_Date)),units=c("days"))) + 1

clean_crime$updated_date_num <- as.numeric(difftime(as.Date(clean_crime$Updated_Date), min(as.Date(clean_crime$Updated_Date)),units=c("days"))) + 1

clean_crime$case_time_num <- as.numeric(difftime(strptime(clean_crime$Case_Time,format='%H:%M:%S'), min(strptime(clean_crime$Case_Time,format='%H:%M:%S')),units=c("mins"))) + 1

clean_crime$updated_time_num <- as.integer(difftime(strptime(clean_crime$Updated_Time,format='%H:%M:%S'), min(strptime(clean_crime$Updated_Time,format='%H:%M:%S')),units=c("mins"))) + 1




# time dimension
crime_time <- clean_crime[,c("IUCR","Arrest","case_date_num","case_time_num","updated_date_num","updated_time_num","Unsolved_Case_Elapsed_Time","Solved_Case_Elapsed_Time")]


#16834 rows
crime_time_T <- crime_time[which(crime_time$Arrest==TRUE), -7] 
#43408 rows
crime_time_F <- crime_time[which(crime_time$Arrest==FALSE), -8]

set.seed(123)
index <- row.names(crime_time_F)
sample_index <- sample(index,size=22734,replace=FALSE)
crime_time_F <- crime_time_F[sample_index,]


# location dimension
crime_loc <- clean_crime[,c("IUCR","Arrest","Latitude","Longitude","Far_North_Side","Northwest_Side","North_Side","Central","West_Side","Southwest_Side","South_Side","Far_Southwest_Side","Far_Southeast_Side")]

crime_loc_T <- crime_loc[which(crime_loc$Arrest==TRUE), ]
crime_loc_F <- crime_loc[which(crime_loc$Arrest==FALSE), ]

set.seed(123)
index <- row.names(crime_loc_F)
sample_index <- sample(index,size=22734,replace=FALSE)
crime_loc_F <- crime_loc_F[sample_index,]




### See distribution of each attributes so determine whether it should be put in for distance
## Time dimension when Arrest = TRUE
# Solved_Case_Elapsed_Time
plot1 <- crime_time_T %>% 
  ggplot(aes(x = "Arrest Crime", y = Solved_Case_Elapsed_Time)) + 
  geom_jitter(width = .25, height = 10, size = 1.5, alpha = .5, color = "blue") +
  labs(x = "", y="Solving Duration")

grid.arrange(plot1)


# IUCR
plot2 <-  crime_time_T %>%
  ggplot(aes(x = "Arrest Crime", y = IUCR)) + 
  geom_jitter(width = .30, height = 20, size = 1, alpha = .6,  color = "orange") +
  labs(x = "", y="IUCR")

grid.arrange(plot2)



# Case_Date
plot3 <-  crime_time_T %>%
  ggplot(aes(x = "Arrest Crime", y = as.numeric(case_date_num))) + 
  geom_jitter(width = .2, height = 1, size = 1.5, alpha = .6,  color = "green") +
  labs(x = "", y="Case_Date")


grid.arrange(plot3)



# Case_Time
plot4 <-  crime_time_T %>%
  ggplot(aes(x = "Arrest Crime", y = as.numeric(case_time_num))) + 
  geom_jitter(width = .30, height = 50, size = 0.7, alpha = .6,  color = "red") +
  labs(x = "", y="Case_Time")

grid.arrange(plot4)


# Updated_Date  -> Remove
plot5 <-  crime_time_T %>%
  ggplot(aes(x = "Arrest Crime", y = as.numeric(updated_date_num))) + 
  geom_jitter(width = .20, height = 1, size = 2, alpha = .6,  color = "violet") +
  labs(x = "", y="Updated_Date")

grid.arrange(plot5)


# Updated_Time -> Remove
plot6 <-  crime_time_T %>%
  ggplot(aes(x = "Arrest Crime", y = as.numeric(updated_time_num))) + 
  geom_jitter(width = .30, height = 50, size = 1, alpha = .6,  color = "yellow") +
  labs(x = "", y="Updated_Time")

grid.arrange(plot6)


crime_time_T <- crime_time_T[, c(-2,-5,-6)]



grid.arrange(plot1,plot2,plot3,plot4,plot5,plot6,nrow=2, ncol=3)

## Time dimension when Arrest = FALSE
# Unsolved_Case_Elapsed_Time
plot1 <- crime_time_F %>% 
  ggplot(aes(x = "Unarrest Crime", y = Unsolved_Case_Elapsed_Time)) + 
  geom_jitter(width = .20, height = 10, size = 1.0, alpha = .5, color = "blue") +
  labs(x = "", y="Unsolving Duration")

grid.arrange(plot1)


# IUCR
plot2 <-  crime_time_F %>%
  ggplot(aes(x = "Unarrest Crime", y = IUCR)) + 
  geom_jitter(width = .20, height = 20, size = 1, alpha = .6,  color = "orange") +
  labs(x = "", y="IUCR")

grid.arrange(plot2)



# Case_Date
plot3 <-  crime_time_F %>%
  ggplot(aes(x = "Unarrest Crime", y = as.numeric(case_date_num))) + 
  geom_jitter(width = .20, height = 50, size = 1.0, alpha = .6,  color = "green") +
  labs(x = "", y="Case_Date")


grid.arrange(plot3)



# Case_Time
plot4 <-  crime_time_F %>%
  ggplot(aes(x = "Unarrest Crime", y = as.numeric(case_time_num))) + 
  geom_jitter(width = .30, height = 100, size = 0.5, alpha = .6,  color = "red") +
  labs(x = "", y="Case_Time")

grid.arrange(plot4)


# Updated_Date  -> Remove
plot5 <-  crime_time_F %>%
  ggplot(aes(x = "Unarrest Crime", y = as.numeric(updated_date_num))) + 
  geom_jitter(width = .10, height = 1, size = 2, alpha = .6,  color = "violet") +
  labs(x = "", y="Updated_Date")

grid.arrange(plot5)


# Updated_Time -> Remove
plot6 <-  crime_time_F %>%
  ggplot(aes(x = "Unarrest Crime", y = as.numeric(updated_time_num))) + 
  geom_jitter(width = .20, height = 50, size = 1, alpha = .6,  color = "yellow") +
  labs(x = "", y="Updated_Time")

grid.arrange(plot6)



crime_time_F <- crime_time_F[, c(-2,-5,-6)]

write.csv(crime_time_F,"crime_time_F_half_sample.csv")
crime_time_F <- read.csv("crime_time_F_half_sample.csv")
crime_time_F <- crime_time_F[,-1]

## Location dimension when Arrest = TRUE
# Solved_Case_Elapsed_Time
plot1 <- crime_loc_T %>% 
  ggplot(aes(x = "Arrest Crime", y = Latitude)) + 
  geom_jitter(width = .10, height = 5, size = 0.5, alpha = .5, color = "blue") +
  labs(x = "", y="Latitude")

grid.arrange(plot1)


plot2 <- crime_loc_T %>% 
  ggplot(aes(x = "Arrest Crime", y = Longitude)) + 
  geom_jitter(width = .20, height = 10, size = 1.0, alpha = .5, color = "blue") +
  labs(x = "", y="Longitude")

grid.arrange(plot2)


plot3 <- crime_loc_T %>% 
  ggplot(aes(x = "Arrest Crime", y = IUCR)) + 
  geom_jitter(width = .20, height = 70, size = 1.5, alpha = .5, color = "blue") +
  labs(x = "", y="IUCR")

grid.arrange(plot3)


crime_loc_T <- crime_loc_T[,-2]

## Location dimension when Arrest = FALSE
# Solved_Case_Elapsed_Time
plot1 <- crime_loc_F %>% 
  ggplot(aes(x = "Arrest Crime", y = Latitude)) + 
  geom_jitter(width = .30, height = 700, size = 0.5, alpha = .5, color = "blue") +
  labs(x = "", y="Latitude")

grid.arrange(plot1)


plot2 <- crime_loc_F %>% 
  ggplot(aes(x = "Arrest Crime", y = Longitude)) + 
  geom_jitter(width = .20, height = 700, size = 0.5, alpha = .5, color = "blue") +
  labs(x = "", y="Longitude")

grid.arrange(plot2)


plot3 <- crime_loc_F %>% 
  ggplot(aes(x = "Arrest Crime", y = IUCR)) + 
  geom_jitter(width = .20, height = 40, size = 1.0, alpha = .5, color = "blue") +
  labs(x = "", y="IUCR")

grid.arrange(plot3)

crime_loc_F <- crime_loc_F[,-2]
write.csv(crime_loc_F,"crime_loc_F_half_sample.csv")
crime_loc_F <- read.csv("crime_loc_F_half_sample.csv")
crime_loc_F <- crime_loc_F[,-1]


## Standardization
normalize <- function(x) {
  return ((x - mean(x)) / sd(x))
}

# location dimension
crime_loc_T$IUCR <- normalize(crime_loc_T$IUCR)
crime_loc_T$Latitude <- normalize(crime_loc_T$Latitude)
crime_loc_T$Longitude <- normalize(crime_loc_T$Longitude)
crime_loc_T$Far_North_Side <- normalize(crime_loc_T$Far_North_Side)
crime_loc_T$Northwest_Side <- normalize(crime_loc_T$Northwest_Side)
crime_loc_T$North_Side <- normalize(crime_loc_T$North_Side)
crime_loc_T$Central <- normalize(crime_loc_T$Central)
crime_loc_T$West_Side <- normalize(crime_loc_T$West_Side)
crime_loc_T$Southwest_Side <- normalize(crime_loc_T$Southwest_Side)
crime_loc_T$South_Side <- normalize(crime_loc_T$South_Side)
crime_loc_T$Far_Southwest_Side <- normalize(crime_loc_T$Far_Southwest_Side)
crime_loc_T$Far_Southeast_Side <- normalize(crime_loc_T$Far_Southeast_Side)



crime_loc_F$IUCR <- normalize(crime_loc_F$IUCR)
crime_loc_F$Latitude <- normalize(crime_loc_F$Latitude)
crime_loc_F$Longitude <- normalize(crime_loc_F$Longitude)
crime_loc_F$Far_North_Side <- normalize(crime_loc_F$Far_North_Side)
crime_loc_F$Northwest_Side <- normalize(crime_loc_F$Northwest_Side)
crime_loc_F$North_Side <- normalize(crime_loc_F$North_Side)
crime_loc_F$Central <- normalize(crime_loc_F$Central)
crime_loc_F$West_Side <- normalize(crime_loc_F$West_Side)
crime_loc_F$Southwest_Side <- normalize(crime_loc_F$Southwest_Side)
crime_loc_F$South_Side <- normalize(crime_loc_F$South_Side)
crime_loc_F$Far_Southwest_Side <- normalize(crime_loc_F$Far_Southwest_Side)
crime_loc_F$Far_Southeast_Side <- normalize(crime_loc_F$Far_Southeast_Side)




# time dimension
crime_time_T$IUCR <- normalize(crime_time_T$IUCR)
crime_time_T$case_date_num <- normalize(crime_time_T$case_date_num)
crime_time_T$case_time_num <- normalize(crime_time_T$case_time_num)
crime_time_T$Solved_Case_Elapsed_Time <- normalize(crime_time_T$Solved_Case_Elapsed_Time)

crime_time_F$IUCR <- normalize(crime_time_F$IUCR)
crime_time_F$case_date_num <- normalize(crime_time_F$case_date_num)
crime_time_F$case_time_num <- normalize(crime_time_F$case_time_num)
crime_time_F$Unsolved_Case_Elapsed_Time <- normalize(crime_time_F$Unsolved_Case_Elapsed_Time)


# write four data set
write.csv(crime_time_T,"crime_time_T.csv")
write.csv(crime_time_F,"crime_time_F.csv")
write.csv(crime_loc_T,"crime_loc_T.csv")
write.csv(crime_loc_F,"crime_loc_F.csv")


library(factoextra)
library(NbClust)

# Elbow method
fviz_nbclust(crime_time_T, kmeans, method = "wss") +
    geom_vline(xintercept = 3, linetype = 2)+
  labs(subtitle = "Elbow method")


fviz_nbclust(crime_loc_T, kmeans, method = "wss",k.max=15) +
    geom_vline(xintercept = 9, linetype = 2)+
  labs(subtitle = "Elbow method")
  

crime_time_F <- crime_time_F[sample(nrow(crime_time_F), 15361), ]

fviz_nbclust(crime_time_F, kmeans, method = "wss") +
    geom_vline(xintercept = 4, linetype = 2)+
  labs(subtitle = "Elbow method")
  
  
crime_loc_F <- crime_loc_F[sample(nrow(crime_loc_F), 15361), ]
fviz_nbclust(crime_loc_F, kmeans, method = "wss",k.max=15) +
    geom_vline(xintercept = 12, linetype = 2)+
  labs(subtitle = "Elbow method")




### crime_time_T
## define the best k
wssplot <- function(data, nc=15, seed=123){
  wss <- (nrow(data)-1)*sum(apply(data,2,var))
  for (i in 2:nc){
    set.seed(seed)
    wss[i] <- sum(kmeans(data, centers=i)$withinss)
    }
    return (wss)}
    #wss <- as.data.frame(wss)
    #wss$K <- row.names(wss)
    #wss$withinss <- wss$wss

  #ggplot(wss, aes(x=K,y=withinss)) + geom_line() + geom_point()
  #type="b", xlab="Number of groups",
       #ylab="Sum of squares within a group")
       

test = wssplot(crime_time_T, nc = 20)




#compare 6 7 8  -> 8
set.seed(123)
clustering_6 <- kmeans(crime_time_T, centers = 6, nstart = 20) #81.5%
clustering_7 <- kmeans(crime_time_T, centers = 7, nstart = 20) #84.2%
clustering_8 <- kmeans(crime_time_T, centers = 8, nstart = 20) #86.3%





## validation
library(cluster)
library(factoextra)

sil <- silhouette(clustering_8$cluster, dist(crime_time_T))
fviz_silhouette(sil)


## visualization
library(GGally)
library(plotly)

crime_time_T$cluster <- as.factor(clustering_8$cluster)

# write csv the result
crime_time_T_cluster <- crime_time[which(crime_time$Arrest==TRUE), -7] 
crime_time_T_cluster$cluster <- as.factor(clustering_8$cluster)
crime_time_T_cluster <- crime_time_T_cluster[,c(-2,-5,-6)]
write.csv(crime_time_T_cluster,"crime_time_T_cluster.csv")


# visualization 1
p <- ggparcoord(data = crime_time_T, groupColumn = "cluster", scale = "std") + labs(x = "time dimension", y = "value (in standard-deviation units)", title = "Clustering")
ggplotly(p)



# 3D plot
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



saveGIF({
  for(i in 1:150){
    x <- crime_time_T$case_date_num
    y <- crime_time_T$case_time_num
    #f <- function(x, y) { sin(x) + cos(y) }
    z <- crime_time_T$IUCR
    t <- crime_time_T$Solved_Case_Elapsed_Time
    cluster <- as.numeric(crime_time_T$cluster)
    scatter3D(x, t, z, colvar=cluster,xlab = "Case Date", ylab ="Solved_Case_Elapsed_Time", zlab = "IUCR", theta = 45 + (i * 0.5), phi = 35)
  }
}, interval = 0.1, ani.width = 550, ani.height = 550)



### crime_time_F
## define the best k
wssplot <- function(data, nc=15, seed=123){
  wss <- (nrow(data)-1)*sum(apply(data,2,var))
  for (i in 2:nc){
    set.seed(seed)
    wss[i] <- sum(kmeans(data, centers=i)$withinss)}
  plot(1:nc, wss, type="b", xlab="Number of groups",
       ylab="Sum of squares within a group")}

wssplot(crime_time_T, nc = 20)

#compare 5 6  -> 6
set.seed(123)
clustering_5 <- kmeans(crime_time_F, centers = 5, nstart = 20) #78.1%
clustering_6 <- kmeans(crime_time_F, centers = 6, nstart = 20) #81.1%



## validation
library(cluster)
library(factoextra)

sil <- silhouette(clustering_6$cluster, dist(crime_time_F))
fviz_silhouette(sil)


## visualization
library(GGally)
library(plotly)

crime_time_F$cluster <- as.factor(clustering_6$cluster)
index <- row.names(crime_time_F)

# write csv the result
crime_time_F_cluster <- crime_time[index, c(-2,-5,-6,-8)]
crime_time_F_cluster$cluster <- as.factor(clustering_6$cluster)
write.csv(crime_time_F_cluster,"crime_time_F_cluster.csv")


library(plot3D)

# x, y and z coordinates
x <- crime_time_F$case_date_num
y <- crime_time_F$case_time_num
z <- crime_time_F$IUCR
t <- crime_time_F$Unsolved_Case_Elapsed_Time
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
    t <- crime_time_F$Unsolved_Case_Elapsed_Time
    cluster <- as.numeric(crime_time_F$cluster)
    scatter3D(x, y, z, colvar=cluster,xlab = "Case Date", ylab ="Case Time", zlab = "IUCR", theta = 45 + (i * 0.5), phi = 35)
  }
}, interval = 0.1, ani.width = 550, ani.height = 550)




### crime_loc_T
## define the best k
wssplot <- function(data, nc=15, seed=123){
  wss <- (nrow(data)-1)*sum(apply(data,2,var))
  for (i in 2:nc){
    set.seed(seed)
    wss[i] <- sum(kmeans(data, centers=i)$withinss)}
  plot(1:nc, wss, type="b", xlab="Number of groups",
       ylab="Sum of squares within a group")}

wssplot(crime_loc_T, nc = 20)

#compare 8 9 -> 9
set.seed(123)
clustering_8 <- kmeans(crime_loc_T, centers = 8, nstart = 20) #74.0%
clustering_9 <- kmeans(crime_loc_T, centers = 9, nstart = 20) #76.6%




## validation
library(cluster)
library(factoextra)

sil <- silhouette(clustering_9$cluster, dist(crime_loc_T))
fviz_silhouette(sil)


## visualization
library(GGally)
library(plotly)

crime_loc_T$cluster <- as.factor(clustering_9$cluster)


# write csv the result
crime_loc_T_cluster <- crime_loc[which(crime_time$Arrest==TRUE), -2] 
crime_loc_T_cluster$cluster <- as.factor(clustering_9$cluster)
crime_loc_T_cluster$Sides <- factor(apply(crime_loc_T_cluster[,c(-1,-2,-3,-13)], 1, function(x) which(x == 1)), labels = colnames(crime_loc_T_cluster[,c(-1,-2,-3,-13)]))
write.csv(crime_loc_T_cluster,"crime_loc_T_cluster.csv")




library(plot3D)

# x, y and z coordinates
x <- crime_loc_T$Latitude
y <- crime_loc_T$Longitude
z <- crime_loc_T$IUCR
cluster <- as.numeric(crime_loc_T$cluster)
sides <- as.numeric(crime_loc_T$Sides)


library(dplyr)
library(tidyr)



crime_loc_T$Sides <- factor(apply(crime_loc_T[,c(-1,-2,-3,-13)], 1, function(x) which(x == 1)), labels = colnames(crime_loc_T[,c(-1,-2,-3,-13)]))



#shapes = c(1:9) 
#shapes <- shapes[as.numeric(crime_loc_T$Sides)]

#shapes = c(1:8) 
#shapes <- shapes[cluster]


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
## define the best k
wssplot <- function(data, nc=15, seed=123){
  wss <- (nrow(data)-1)*sum(apply(data,2,var))
  for (i in 2:nc){
    set.seed(seed)
    wss[i] <- sum(kmeans(data, centers=i)$withinss)}
  plot(1:nc, wss, type="b", xlab="Number of groups",
       ylab="Sum of squares within a group")}

wssplot(crime_loc_F, nc = 20)

#compare 8 9 -> 9
set.seed(123)
clustering_8 <- kmeans(crime_loc_F, centers = 8, nstart = 20) #73.5%
clustering_9 <- kmeans(crime_loc_F, centers = 9, nstart = 20) #76.5%



## validation
library(cluster)
library(factoextra)

sil <- silhouette(clustering_8$cluster, dist(crime_loc_F))
fviz_silhouette(sil)


## visualization
library(GGally)
library(plotly)

crime_loc_F$cluster <- as.factor(clustering_9$cluster)
index <- row.names(crime_loc_F)

# write csv the result
crime_loc_F_cluster <- crime_loc[index, -2]
 
crime_loc_F_cluster$cluster <- as.factor(clustering_9$cluster)
crime_loc_F_cluster$Sides <- factor(apply(crime_loc_F_cluster[,c(-1,-2,-3,-13)], 1, function(x) which(x == 1)), labels = colnames(crime_loc_F_cluster[,c(-1,-2,-3,-13)]))
write.csv(crime_loc_F_cluster,"crime_loc_F_cluster.csv")


library(plot3D)

# x, y and z coordinates
x <- crime_loc_F$Latitude
y <- crime_loc_F$Longitude
z <- crime_loc_F$IUCR
cluster <- as.numeric(crime_loc_F$cluster)
#sides <- as.numeric(crime_loc_F$Sides)


library(dplyr)
library(tidyr)



#crime_loc_F$Sides <- factor(apply(crime_loc_F[,c(-1,-2,-3,-13)], 1, function(x) which(x == 1)), labels = colnames(crime_loc_F[,c(-1,-2,-3,-13)]))



#shapes = c(1:9) 
#shapes <- shapes[as.numeric(crime_loc_T$Sides)]

#shapes = c(1:8) 
#shapes <- shapes[cluster]


scatter3D(x, y, z, colvar=cluster,xlab = "Latitude", ylab ="Longitude", zlab = "IUCR",theta = 45, phi = 15)



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







