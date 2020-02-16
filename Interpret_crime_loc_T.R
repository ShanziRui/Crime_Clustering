crime <- read.csv("1Percent_Clean_Chicago_Crime_Dataset.csv")


## crime_loc_T_cluster
crime_loc_T <- read.csv("crime_loc_T_cluster.csv")
row.names(crime_loc_T) <- crime_loc_T$X
crime_loc_T <- crime_loc_T[,-1]


## 9 clusters
crime_loc_T_1 <- crime_loc_T[which(crime_loc_T$cluster==1),]
crime_loc_T_2 <- crime_loc_T[which(crime_loc_T$cluster==2),]
crime_loc_T_3 <- crime_loc_T[which(crime_loc_T$cluster==3),]
crime_loc_T_4 <- crime_loc_T[which(crime_loc_T$cluster==4),]
crime_loc_T_5 <- crime_loc_T[which(crime_loc_T$cluster==5),]
crime_loc_T_6 <- crime_loc_T[which(crime_loc_T$cluster==6),]
crime_loc_T_7 <- crime_loc_T[which(crime_loc_T$cluster==7),]
crime_loc_T_8 <- crime_loc_T[which(crime_loc_T$cluster==8),]
crime_loc_T_9 <- crime_loc_T[which(crime_loc_T$cluster==9),]


## cluster 1
hist(crime_loc_T_1$IUCR) #around 500 -> ASSAULT ; around 900 -> 900; around 1200 -> DECEPTIVE PRACTICE
summary(crime_loc_T_1$Sides)  #Central 890



## cluster 2
hist(crime_loc_T_2$IUCR) #1900 to 2100 -> NARCOTICS
summary(crime_loc_T_2$Sides)  #West_Side 2754


## cluster 3
hist(crime_loc_T_3$IUCR) #0 to 500 -> HOMICIDE;CRIM SEXUAL ASSAULT;ROBBERY;BATTERY
summary(crime_loc_T_3$Sides)  #Far_Southeast_Side 1649


## cluster 4
hist(crime_loc_T_4$IUCR) #0 to 1000 -> HOMICIDE;CRIM SEXUAL ASSAULT;ROBBERY;BATTERY;ASSAULT;THEFT
summary(crime_loc_T_4$Sides)  #Northwest_Side  648


## cluster 5
hist(crime_loc_T_5$IUCR) #around 500 -> ASSAULT; around 2000 -> NARCOTICS
summary(crime_loc_T_5$Sides)  #Southwest_Side  2351


## cluster 6
hist(crime_loc_T_6$IUCR) #0 to 1000 -> HOMICIDE;CRIM SEXUAL ASSAULT;ROBBERY;BATTERY;ASSAULT;THEFT
summary(crime_loc_T_6$Sides)  #Far_North_Side  907


## cluster 7
hist(crime_loc_T_7$IUCR) #400 to 500 -> BATTERY;RITUALISM
summary(crime_loc_T_7$Sides)  #West_Side  1732



## cluster 8
hist(crime_loc_T_8$IUCR) #around 500 -> ASSAULT; around 2000 -> NARCOTICS
summary(crime_loc_T_8$Sides)  #South_Side  1608


## cluster 9
hist(crime_loc_T_9$IUCR) #around 4600-5000 -> OTHER OFFENSE
summary(crime_loc_T_9$Sides)  #disperse every where; West_Side > Southwest_Side > South_Side > Far_Southeast_Sid > Far_Southwest_Side (central, North_Side, Far_North_Side, Northwest_Side doesn't have much records)






















