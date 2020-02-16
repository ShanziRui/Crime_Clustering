library(dplyr)

crime <- read.csv("Crimes_-_2001_to_present.csv")

### Describe how you handle missing values
## remove empty columns
nrow(crime)
ncol(crime)
col.na <- sapply(crime, function(x)all(is.na(x)))
index <- which(col.na == TRUE)
index  #named integer(0), no empty columns


## remove rows with missing values
raw_crime <- na.omit(crime)
nrow(raw_crime)


### Describe how you handle data inconsistency
## invalid IUCR code
IUCR_code <- read.csv("Chicago_Police_Department_-_Illinois_Uniform_Crime_Reporting__IUCR__Codes.csv")
nrow(IUCR_code)
valid_code <- as.character(IUCR_code$IUCR)
valid_code <- sprintf("%04s",valid_code)
index.invalid <- which(is.element(raw_crime$IUCR,valid_code)==FALSE)

valid_crime <- raw_crime[-index.invalid,]
nrow(valid_crime)


## invalid community code
Community <- read.csv("CommAreas.csv")
valid_num <- Community$AREA_NUMBE
index.invalid <- which(is.element(valid_crime$Community.Area,valid_num)==FALSE)

valid_crime <- valid_crime[-index.invalid,]

valid_crime$IUCR <- gsub("[A-Z]","",valid_crime$IUCR)
valid_crime$IUCR <- as.numeric(valid_crime$IUCR)
nrow(valid_crime)



## incomplete records
year_freq <- data.frame(table(valid_crime$Year))
barplot(year_freq$Freq)  #2001 and 2019 not complete

index.invalid <- which(valid_crime$Year=='2001' | valid_crime$Year=='2019')
valid_crime <- valid_crime[-index.invalid,]
nrow(valid_crime)


### Show at least 2 instances of data transformation 
## split “Date” attribute into “Case_Date” and “Case_Time”
library(lubridate)

Format_Date <- mdy_hms(valid_crime$Date)
yr <- as.character(year(Format_Date))
mon <- sprintf("%02s",as.character(month(Format_Date)))
day <- sprintf("%02s",as.character(day(Format_Date)))

hr <- sprintf("%02s",as.character(hour(Format_Date)))
min <- sprintf("%02s",as.character(minute(Format_Date)))
sec <- sprintf("%02s",as.character(second(Format_Date)))

Case_Date <- paste(yr,mon,day,sep='-')
Case_Time <- paste(hr,min,sec,sep=':')

valid_crime$Case_Date <- Case_Date
valid_crime$Case_Time <- Case_Time


## split “Updated On” into “Updated_Date” and “Updated_Time”

Format_Update_Date <- mdy_hms(valid_crime$Updated.On)
yr <- as.character(year(Format_Update_Date))
mon <- sprintf("%02s",as.character(month(Format_Update_Date)))
day <- sprintf("%02s",as.character(day(Format_Update_Date)))

hr <- sprintf("%02s",as.character(hour(Format_Update_Date)))
min <- sprintf("%02s",as.character(minute(Format_Update_Date)))
sec <- sprintf("%02s",as.character(second(Format_Update_Date)))

Updated_Date <- paste(yr,mon,day,sep='-')
Updated_Time <- paste(hr,min,sec,sep=':')

valid_crime$Updated_Date <- Updated_Date
valid_crime$Updated_Time <- Updated_Time




## Convert 77 community areas into 9 "sides" with binary variables
central <- c(8,32:33)
north.side <- c(5:7,21:22)
far.north.side <- c(1:4,9:14,76:77)
northwest.side <- c(15:20)
west.side <- c(23:31)
south.side <- c(34:43,60,69)
southwest.side <- c(56:59,61:68)
far.southeast.side <- c(44:55)
far.southwest.side <- c(70:75)

valid_crime$Central <- 0
valid_crime$North_Side <- 0
valid_crime$Far_North_Side <- 0
valid_crime$Northwest_Side <- 0
valid_crime$West_Side <- 0
valid_crime$South_Side <- 0
valid_crime$Southwest_Side <- 0
valid_crime$Far_Southeast_Side <- 0
valid_crime$Far_Southwest_Side <- 0


index_central <- which(is.element(valid_crime$Community.Area,central)==TRUE)
index_north.side <- which(is.element(valid_crime$Community.Area,north.side)==TRUE)
index_far.north.side <- which(is.element(valid_crime$Community.Area,far.north.side)==TRUE)
index_northwest.side <- which(is.element(valid_crime$Community.Area,northwest.side)==TRUE)
index_west.side <- which(is.element(valid_crime$Community.Area,west.side)==TRUE)
index_south.side <- which(is.element(valid_crime$Community.Area,south.side)==TRUE)
index_southwest.side <- which(is.element(valid_crime$Community.Area,southwest.side)==TRUE)
index_far.southeast.side <- which(is.element(valid_crime$Community.Area,far.southeast.side)==TRUE)
index_far.southwest.side <- which(is.element(valid_crime$Community.Area,far.southwest.side)==TRUE)

valid_crime[index_central,c('Central')] <- 1
valid_crime[index_north.side,c('North_Side')] <- 1
valid_crime[index_far.north.side,c('Far_North_Side')] <- 1
valid_crime[index_northwest.side,c('Northwest_Side')] <- 1
valid_crime[index_west.side,c('West_Side')] <- 1
valid_crime[index_south.side,c('South_Side')] <- 1
valid_crime[index_southwest.side,c('Southwest_Side')] <- 1
valid_crime[index_far.southeast.side,c('Far_Southeast_Side')] <- 1
valid_crime[index_far.southwest.side,c('Far_Southwest_Side')] <- 1

head(valid_crime)


### Create at least 2 new variables using a combination of at least 2 variables
## Create a new variable called “Unsolved_Case_Elapsed_Days” = Updated_Date - Case_Date when values in Arrest column are false.
valid_crime$Arrest <- as.character(valid_crime$Arrest)

valid_crime$Arrest <- ifelse(valid_crime$Arrest=="true",TRUE,FALSE)


valid_crime$Unsolved_Case_Elapsed_Time <- ifelse(valid_crime$Arrest==FALSE,as.numeric(difftime(as.Date(valid_crime$Updated_Date),as.Date(valid_crime$Case_Date))),NA)

invalid_days <- which(valid_crime$Unsolved_Case_Elapsed_Time<=0)
length(invalid_days)  #no negtive durations


## Create a new variable called “Solved_Case_Elapsed_Days” = Updated_Date - Case_Date when values in Arrest column are true. 
valid_crime$Solved_Case_Elapsed_Time <- ifelse(valid_crime$Arrest==TRUE,as.numeric(difftime(as.Date(valid_crime$Updated_Date),as.Date(valid_crime$Case_Date))),NA)

invalid_days <- which(valid_crime$Solved_Case_Elapsed_Time<=0)
length(invalid_days)  #no negtive durations



## write clean crime data
write.csv(valid_crime,"clean_crime_before_sample.csv")

head(valid_crime)

##random sampling
set.seed(123)
smaller_clean <- sample_n(valid_crime, nrow(valid_crime)*0.01)

write.csv(smaller_clean,"1Percent_Clean_Chicago_Crime_Dataset.csv")


# model * 4

# arrest: T F

# time: case_date; case_time; update_date; update_time; IUCR

# location: nine sides; lon; lat;





