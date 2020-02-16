crime <- read.csv("1Percent_Clean_Chicago_Crime_Dataset.csv")

## crime_time_T_cluster
crime_time_T <- read.csv("k-means_crime_time_T.csv")
row.names(crime_time_T) <- crime_time_T$X
crime_time_T <- crime_time_T[,-1]
index <- row.names(crime_time_T)
crime_time_T$Case_Date <- crime[index,"Case_Date"]
crime_time_T$Case_Time <- crime[index,"Case_Time"]
crime_time_T$IUCR <- crime[index,"IUCR"]
crime_time_T$Solved_Case_Elapsed_Time <- crime[index,"Solved_Case_Elapsed_Time"]
crime_time_T$case_date_num <- as.numeric(difftime(as.Date(crime_time_T$Case_Date), min(as.Date(crime_time_T$Case_Date)),units=c("days"))) + 1
crime_time_T$case_time_num <- as.integer(difftime(strptime(crime_time_T$Case_Time,format='%H:%M:%S'), min(strptime(crime_time_T$Case_Time,format='%H:%M:%S')),units=c("mins"))) + 1


## 3 clusters
crime_time_T_1 <- crime_time_T[which(crime_time_T$cluster==1),]
crime_time_T_2 <- crime_time_T[which(crime_time_T$cluster==2),]
crime_time_T_3 <- crime_time_T[which(crime_time_T$cluster==3),]


## cluster 1   "2002-01-04"
summary(crime_time_T_1)
hist(crime_time_T_1$IUCR) # from 1500 to 2500 -> PROSTITUTION,SEX OFFENSE,GAMBLING, OFFENSE INVOLVING CHILDREN, NARCOTICS
hist(crime_time_T_1$case_date_num)  #between 200 to 1400 then drop -> July 23, 2002 to Nov.5 2005
hist(crime_time_T_1$case_time_num) # 1100 to 1300 -> 6pm to 10 pm
hist(crime_time_T_1$Solved_Case_Elapsed_Time)  #4600 to 5800 days


## cluster 2   "2002-01-04"
summary(crime_time_T_2)
hist(crime_time_T_2$IUCR) # 0 to 500 -> HOMICIDE, ROBBERY, BATTERY, CRIM SEXUAL ASSAULT

hist(crime_time_T_2$case_date_num)  #between 1000 to 2500 -> September 30, 2004 to November 8, 2008
hist(crime_time_T_2$case_time_num) # o to 200 and 500 to 600 -> midnight before 3am and 8 to 10 am
hist(crime_time_T_2$Solved_Case_Elapsed_Time)  #3500 to 5000 days


## cluster 3   "2002-01-04"
summary(crime_time_T_3)
hist(crime_time_T_3$IUCR) # 1500 to 2500 -> PROSTITUTION,SEX OFFENSE,GAMBLING, OFFENSE INVOLVING CHILDREN, NARCOTICS
hist(crime_time_T_3$case_date_num)  #between 3000 to 5000 -> March 23, 2010 to September 13, 2015
hist(crime_time_T_3$case_time_num) # 1100 to 1400 -> 6pm to 11pm
hist(crime_time_T_3$Solved_Case_Elapsed_Time)  #500 to 2500 days


"Rplot_crime_loc_T_1$IUCR"

p<-ggplot(data=crime_time_T_8, aes(x=IUCR, y=count(cluster), color = IUCR)) + geom_bar(stat="identity", fill='white')

p+scale_fill_manual(values=c("#999999", "#E69F00", "#56B4E9","#999999", "#E69F00"))


ggplot(data=crime_time_T_8, aes(x=Solved_Case_Elapsed_Time)) + 
  geom_histogram( 
                 col="red", 
                 aes(fill=..count..)) +
  scale_fill_gradient("Count", low="green", high="red")
  

  scale_fill_manual(values=c("#999999", "#E69F00", "#56B4E9","#999999", "#E69F00","#56B4E9","#999999", "#E69F00", "#56B4E9"))








