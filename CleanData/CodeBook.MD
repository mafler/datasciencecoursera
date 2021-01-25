---
title: "Cleaning up the UCI HAR Dataset"
author: "Mafler"
date: "25/01/2021"
output: html_document
---
## Aim 
The purpose of this project is to demonstrate your ability to collect, work with, and clean a data set

You should create one R script called run_analysis.R that does the following. 

1. Merges the training and the test sets to create one data set.
2. Extracts only the measurements on the mean and standard deviation for each measurement. 
3. Uses descriptive activity names to name the activities in the data set
4. Appropriately labels the data set with descriptive variable names. 
5. From the data set in step 4, creates a second, independent tidy data set with the average of each variable for each activity and each subject.

## Data 
* [Human Activity Recognition Using smartphone Data Set](https://archive.ics.uci.edu/ml/datasets/Human+Activity+Recognition+Using+Smartphones)

* [Download Data](https://d396qusza40orc.cloudfront.net/getdata%2Fprojectfiles%2FUCI%20HAR%20Dataset.zip)

## Variables 

### Feature Selection 

The features selected for this database come from the accelerometer and gyroscope 3-axial raw signals tAcc-XYZ and tGyro-XYZ. These time domain signals (prefix 't' to denote time) were captured at a constant rate of 50 Hz. Then they were filtered using a median filter and a 3rd order low pass Butterworth filter with a corner frequency of 20 Hz to remove noise. Similarly, the acceleration signal was then separated into body and gravity acceleration signals (tBodyAcc-XYZ and tGravityAcc-XYZ) using another low pass Butterworth filter with a corner frequency of 0.3 Hz. 

Subsequently, the body linear acceleration and angular velocity were derived in time to obtain Jerk signals (tBodyAccJerk-XYZ and tBodyGyroJerk-XYZ). Also the magnitude of these three-dimensional signals were calculated using the Euclidean norm (tBodyAccMag, tGravityAccMag, tBodyAccJerkMag, tBodyGyroMag, tBodyGyroJerkMag). 

Finally a Fast Fourier Transform (FFT) was applied to some of these signals producing fBodyAcc-XYZ, fBodyAccJerk-XYZ, fBodyGyro-XYZ, fBodyAccJerkMag, fBodyGyroMag, fBodyGyroJerkMag. (Note the 'f' to indicate frequency domain signals). 

These signals were used to estimate variables of the feature vector for each pattern:  
'-XYZ' is used to denote 3-axial signals in the X, Y and Z directions.

* tBodyAcc-XYZ
* tGravityAcc-XYZ
* tBodyAccJerk-XYZ
* tBodyGyro-XYZ
* tBodyGyroJerk-XYZ
* tBodyAccMag
* tGravityAccMag
* tBodyAccJerkMag
* tBodyGyroMag
* tBodyGyroJerkMag
* fBodyAcc-XYZ
* fBodyAccJerk-XYZ
* fBodyGyro-XYZ
* fBodyAccMag
* fBodyAccJerkMag
* fBodyGyroMag
* fBodyGyroJerkMag

The full list of features can be found in the data set under features_info.txt

For this project I have filtered the original list of features to only include ones titled: 

* mean(): Mean value
* std(): Standard deviation

## Code Explanation 

### Packages and Data download. 

Packages: 

* data.table 
* reshape2

Sets working directory then checks to see if data is already there by checking if the folder UCI HAR Dataset exists
if not then downloads the data. 

### Loads names of features and activites  

Activity contains the file activity_labels.txt this is a list of the activities performed

features contains the file features.txt this is a list of names of the features 

### Load and organise train and test data

* subject_train contains train/subject_train.txt which is the order of the subjects who were observed 

* train_x contains train/X_train.txt and label with the features names this is the recorded data from the Smartphone 

* train_y contains train/y_train.txt this is the corresponding order of the activities 

* subject_test contains test/subject_test.txt which is the order of the subjects who were observed this only included 9/30 being observed

* test_x contains test/X_test.txt and label with the features names this is the recorded data from the Smartphone 

* train_y contains test/y_test.txt this is the corresponding order of the activities 


### Combine train and test data 

Merged the test and train data using cbind() function stored in mergedDT

### Find columns only with mean and standard deviation 

Using grep function search and filter the features for those containing only mean and std.

Then remove meanFreq columns 

### Filter merged data

Filter the data to remove the unwanted columns and then remove the brackets from column names 
and store in data table filteredDT

### Rename columns with descriptive variable names
Using gsub() to replace abbreviated names 

1. Acc replacement = Accelerometer
2. Gyro, replacement = Gyroscope
3. BodyBody, replacement = Body 
4. Mag, replacement = Magnitude 
5. f, replacement = Frequency
6. t, replacement = Time

### Create independent tidy data set with average of each variable for each activity and subject

Using reshape2 package

Use the function melt() to create a molten data frame around Subject and Activity and then dcast() to restructure the data and calculate the mean based on each variable for Subject and Activity


### Write tidy information to text file. 
Write the newly created tidy data to data.txt

## Code 
```
##Cleaning some data 
library(data.table)
library(reshape2)

##Option to download data
##Windows machines option 
directory <- choose.dir()
setwd(directory)

if(!dir.exists("UCI HAR Dataset")){
    ##Get data
  fileURL <- "https://d396qusza40orc.cloudfront.net/getdata%2Fprojectfiles%2FUCI%20HAR%20Dataset.zip"
  download.file(fileURL, "dataFile.zip")
  unzip("dataFile.zip")
  setwd("UCI HAR Dataset")
  } else{
  setwd("UCI HAR Dataset")
  message("Working Directory: ", getwd())
}
## Ensure that working directory is set to UCI HAR Dataset 
## Part one features/labels

features <- fread("features.txt", col.names = c("index", "activityMeasures"))
activity <- fread("activity_labels.txt", col.names = c("activityIndex", "activityNames"))


### load the train data and name the columns
subject_train <- fread("train/subject_train.txt", col.names = "Subject")
train_x <- fread("train/X_train.txt" , col.names = features$activityMeasures)
train_y <- fread("train/y_train.txt", col.names = "Activity")
##combine train data
train <- cbind(subject_train,train_y, train_x)

### load the  test data and name the columns 

subject_test <- fread("test/subject_test.txt", col.names = "Subject")
test_x <- fread("test/X_test.txt" , col.names=features$activityMeasures )
test_y <- fread("test/y_test.txt", col.names = "Activity")
##combine test data
test <- cbind(subject_test,test_y, test_x)

## combine the test and train data 

mergedDT <- rbind(test,train)
## find names of columns with only mean and standard deviation 
req_features <-grep("std|mean", features$activityMeasures, value = TRUE)
r <- grep("meanFreq", x= req_features)
req_features <- req_features[-r]

## filter the merged tables to only include
## Subject, Activity and required mean/standard deviation columns 

filteredDT <- mergedDT[, c("Subject","Activity", req_features), with =FALSE]
req_features <- gsub("[()]","", req_features)
names(filteredDT) <- c("Subject","Activity", req_features)
## Set Actvity as factors and rename
filteredDT[,Activity:= as.factor(Activity)]
levels(filteredDT$Activity) <- activity$activityNames

##Rename column names 
names(filteredDT) <- gsub("Acc", replacement = "-Accelerometer", x= names(filteredDT))
names(filteredDT) <- gsub("Gyro", replacement = "-Gyroscope", x= names(filteredDT))
names(filteredDT) <- gsub("BodyBody", replacement = "Body", x= names(filteredDT))
names(filteredDT) <- gsub("Mag", replacement = "-Magnitude", x= names(filteredDT))
names(filteredDT) <- gsub("^f", replacement = "Frequency-", x= names(filteredDT))
names(filteredDT) <- gsub("^t", replacement = "Time-", x= names(filteredDT))

## Set Subject as factors
filteredDT[,Subject:= as.factor(Subject)]

##Using reShape2 melt the data frame 
filteredDT <- melt(filteredDT, id=c("Subject", "Activity"))

##reshape the data using dcast to average each variable based on 
##Subject and Activity
filteredDT <- dcast(filteredDT, Subject + Activity ~ variable, fun.aggregate = mean)


write.table(filteredDT,"data.txt" ,row.names = FALSE)


```





