##Getting and Cleaning Data - Course Project peer assessment
#

setwd("C:/Users/Chandru/cleaningdata/UCI HAR Dataset")
file <- unzip("getdata-projectfiles-UCI HAR Dataset.zip")

#READ DATA
 x_test <- read.table("./test/X_test.txt")
 y_test <- read.table("./test/y_test.txt")
 subject_test <- read.table("./test/subject_test.txt")
 x_train <- read.table("./train/X_train.txt")
 y_train <- read.table("./train/y_train.txt")
 subject_train <- read.table("./train/subject_train.txt")

 features <- read.table("features.txt")
 ff <- as.character(features$V2)
 colnames(x_file) <- c(ff)

 Activity_Labels <- read.table("activity_labels.txt")
 Act_Label <- as.character(Activity_Labels$V2)

#MERGE DATA
 x_file <- rbind(x_test, x_train)
 y_file <- rbind(y_test, y_train)
 subject_file <- rbind(subject_test, subject_train)

 
 selCol1 <- features$V2[grep("-std()",features$V2)]
 selCol2 <- features$V2[grep("mean()",features$V2)]
 selCol1 <- as.character(selCol1)
 selCol2 <- as.character(selCol2)
 selCol <- c(selCol1, selCol2)
 Datafile <- subset(x_file , select = selCol)

 

#Merges the training and the test sets to create one data set.
 
Datafile <- cbind(subject_file, y_file, Datafile)
colnames(Datafile)[1:2] <- c("Subject", "Activity")
Datafile$Activity <-  as.character(as.numeric(Datafile$Activity))


for(i in 1:10299) {
     j <- as.numeric(Datafile$Activity[i])
     Datafile$Activity[i] <- as.character(Act_Label[j])
 
 }


#


selCol[1:2] <- c("Subject", "Activity")

for (i in 3:81) {

    str <- colnames(Datafile[i])

	 
	if(substr(str,1,1) == 't')
	{	
		returnStr <- "Time domain "
	}	

	if(substr(str,1,1) == 'f')
	{
		returnStr <- "Frequency domain  "
	}

	if(length(grep("BodyAccJerkMag",str)) > 0)
	{
		returnStr <- paste(returnStr,"Jerk_ Magnitude of the Body Acceleration ")
	}
	else if(length(grep("BodyGyroJerkMag",str)) > 0)
	{
		returnStr <- paste(returnStr,"Jerk magnitude of the Body Angular velocity ")
	}
	else if(length(grep("BodyAccJerk",str)) > 0)
	{
		returnStr <- paste(returnStr,"Jerk of the Body Acceleration ")
	}
	else if(length(grep("GravityAccMag",str)) > 0)
	{
		returnStr <- paste(returnStr,"Gravity Acceleration Magnitude ")
	}
	else if(length(grep("GravityAcc",str)) > 0)
	{
		returnStr <- paste(returnStr,"Gravity Acceleration ")
	}
	else if(length(grep("BodyAccMag",str)) > 0)
	{
		returnStr <- paste(returnStr,"Body Acceleration magnitude ")
	}
	else if(length(grep("BodyGyroJerk",str)) > 0)
	{
		returnStr <- paste(returnStr,"Jerk of the Body Angular velocity ")
	}
	else if(length(grep("BodyGyro",str)) > 0)
	{
		returnStr <- paste(returnStr,"Gravity Acceleration Magnitude ")
	}
	else if(length(grep("BodyAcc",str)) > 0)
	{
		returnStr <- paste(returnStr,"Body Acceleration ")
	}



	if(length(grep("meanFreq",str)) > 0)
	{
		returnStr <- paste(returnStr,"Average frequency ")
	}
	else if(length(grep("mean",str)) > 0)
	{
		returnStr <- paste(returnStr,"Average ")
	}

	if(length(grep("std",str)) > 0)
	{
		returnStr <- paste(returnStr,"Standard Deviation ")
	}

	if(length(grep("-X",str)) > 0)
	{
		returnStr <- paste(returnStr,"in X axis ")
	}

	if(length(grep("-Y",str)) > 0)
	{
		returnStr <- paste(returnStr,"in Y axis ")
	}
	
	if(length(grep("-Z",str)) > 0)
	{
		returnStr <- paste(returnStr,"in Z axis ")
	}
    selCol[i] <- as.character(returnStr)

}

colnames(Datafile) <- c(selCol)

library(plyr)
finalDF <- ddply(Datafile, .(Subject, Activity), colwise(mean))

write.table(finalDF, file = "tidydataset.txt", row.names = FALSE)
