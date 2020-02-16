crime <- read.csv("1Percent_Clean_Chicago_Crime_Dataset.csv")


## crime_loc_F_cluster
crime_loc_F <- read.csv("crime_loc_F_cluster.csv")
row.names(crime_loc_F) <- crime_loc_F$X
crime_loc_F <- crime_loc_F[,-1]


## 9 clusters
crime_loc_F_1 <- crime_loc_F[which(crime_loc_F$cluster==1),]
crime_loc_F_2 <- crime_loc_F[which(crime_loc_F$cluster==2),]
crime_loc_F_3 <- crime_loc_F[which(crime_loc_F$cluster==3),]
crime_loc_F_4 <- crime_loc_F[which(crime_loc_F$cluster==4),]
crime_loc_F_5 <- crime_loc_F[which(crime_loc_F$cluster==5),]
crime_loc_F_6 <- crime_loc_F[which(crime_loc_F$cluster==6),]
crime_loc_F_7 <- crime_loc_F[which(crime_loc_F$cluster==7),]
crime_loc_F_8 <- crime_loc_F[which(crime_loc_F$cluster==8),]
crime_loc_F_9 <- crime_loc_F[which(crime_loc_F$cluster==9),]


## cluster 1
hist(crime_loc_F_1$IUCR) #400 to 650 -> BATTERY,RITUALISM,ASSAULT,STALKING ; 800 to 1000 -> THEFT,MOTOR VEHICLE THEFT ; around 1300 -> CRIMINAL TRESPASS,CRIMINAL DAMAGE
summary(crime_loc_F_1$Sides)  #South_Side 2948



## cluster 2
hist(crime_loc_F_2$IUCR) #around 400 -> BATTERY; around 1300 -> CRIMINAL TRESPASS,CRIMINAL DAMAGE
summary(crime_loc_F_2$Sides)  #West_Side 4985


## cluster 3
hist(crime_loc_F_3$IUCR) #around 500 -> ASSAULT; around 1000 -> ARSON; around 1200 -> DECEPTIVE PRACTICE
summary(crime_loc_F_3$Sides)  #Far_Southeast_Side 2456


## cluster 4
hist(crime_loc_F_4$IUCR) #around 500 -> ASSAULT; around 1000 -> ARSON
summary(crime_loc_F_4$Sides)  #Far_Southwest_Side  1533


## cluster 5
hist(crime_loc_F_5$IUCR) #around 500 -> ASSAULT; around 1000 -> ARSON; around 1200 -> DECEPTIVE PRACTICE
summary(crime_loc_F_5$Sides)  #Northwest_Side  1222


## cluster 6
hist(crime_loc_F_6$IUCR) #around 3000 -> PUBLIC PEACE VIOLATION,INTERFERENCE WITH PUBLIC OFFICER
summary(crime_loc_F_6$Sides)  #disperse everywhere, West_Side > Southwest_Side > South_Side > Far_Southeast_Sid  (central, North_Side, Far_North_Side, Far_Southwest_Side, Northwest_Side doesn't have much records)


## cluster 7
hist(crime_loc_F_7$IUCR) #around 500 -> ASSAULT; around 1000 -> ARSON
summary(crime_loc_F_7$Sides)  #Far_North_Sid  1578



## cluster 8
hist(crime_loc_F_8$IUCR) #400 to 500 -> BATTERY,RITUALISM; 800 to 900 -> THEFT,MOTOR VEHICLE THEFT; 1300 to 1400 -> CRIMINAL DAMAGE,CRIMINAL TRESPASS
summary(crime_loc_F_8$Sides)  #Southwest_Side  3112


## cluster 9
hist(crime_loc_F_9$IUCR) #900 to 1000 -> MOTOR VEHICLE THEFT
summary(crime_loc_F_9$Sides)  #Central 1535





















