crime <- read.csv("1Percent_Clean_Chicago_Crime_Dataset.csv")

## crime_time_F_cluster
crime_time_F <- read.csv("k-means_crime_time_F.csv")
row.names(crime_time_F) <- crime_time_F$X
crime_time_F <- crime_time_F[,-1]
index <- row.names(crime_time_F)
crime_time_F$Case_Date <- crime[index,"Case_Date"]
crime_time_F$Case_Time <- crime[index,"Case_Time"]
crime_time_F$IUCR <- crime[index,"IUCR"]
crime_time_F$Unsolved_Case_Elapsed_Time <- crime[index,"Unsolved_Case_Elapsed_Time"]
crime_time_F$case_date_num <- as.numeric(difftime(as.Date(crime_time_F$Case_Date), min(as.Date(crime_time_F$Case_Date)),units=c("days"))) + 1
crime_time_F$case_time_num <- as.integer(difftime(strptime(crime_time_F$Case_Time,format='%H:%M:%S'), min(strptime(crime_time_F$Case_Time,format='%H:%M:%S')),units=c("mins"))) + 1



## 4 clusters
crime_time_F_1 <- crime_time_F[which(crime_time_F$cluster==1),]
crime_time_F_2 <- crime_time_F[which(crime_time_F$cluster==2),]
crime_time_F_3 <- crime_time_F[which(crime_time_F$cluster==3),]
crime_time_F_4 <- crime_time_F[which(crime_time_F$cluster==4),]


## cluster 1   "2002-01-12"
summary(crime_time_T_1)
hist(crime_time_F_1$IUCR) # around 500 and 800 to 1000 -> ASSAULT, THEFT, MOTOR VEHICLE THEFT
hist(crime_time_F_1$case_date_num)  #between 1000 to 3000 -> September 30, 2004 to March 23, 2010
hist(crime_time_F_1$case_time_num) # 450 to 650 -> 7am to 11am
hist(crime_time_F_1$Unsolved_Case_Elapsed_Time)  #3000 to 5500 days


## cluster 2   "2002-03-12"
summary(crime_time_T_2)
hist(crime_time_F_2$IUCR) # 3000 -> PUBLIC PEACE VIOLATION, WEAPONS VIOLATION, INTERFERENCE WITH PUBLIC OFFICER
hist(crime_time_F_2$case_date_num)  #before 3000 -> before March 23, 2010
hist(crime_time_F_2$case_time_num) # 600 to 1000 -> 10am to 4pm
hist(crime_time_F_2$Unsolved_Case_Elapsed_Time)  #4000 to 5500 days


## cluster 3   "2002-03-12"
summary(crime_time_F_3)
hist(crime_time_F_3$IUCR) # around 500 -> ASSAULT; around 900 -> MOTOR VEHICLE THEFT
hist(crime_time_F_3$case_date_num)  #between 50 to 3000 -> before March 23, 2010
hist(crime_time_F_3$case_time_num) # 900 to 1400 -> 3pm to 11pm
hist(crime_time_F_3$Unsolved_Case_Elapsed_Time)  #3500 to 5900 days



## cluster 4   "2002-03-12"
summary(crime_time_F_4)
hist(crime_time_F_4$IUCR) # around 500 -> ASSAULT; around 900 -> MOTOR VEHICLE THEFT; around 1400 -> CRIMINAL DAMAGE,WEAPONS VIOLATION
hist(crime_time_F_4$case_date_num)  #between 3500 to 6500 -> August 5, 2011 to October 22, 2019
hist(crime_time_F_4$case_time_num) # 700 to 1100 -> 11am to 6pm
hist(crime_time_F_4$Unsolved_Case_Elapsed_Time)  #less than 200 days






