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

##reshape the data using Dcast to average each variable based on 
##Subject and Activity
filteredDT <- dcast(filteredDT, Subject + Activity ~ variable, fun.aggregate = mean)


write.table(filteredDT,"data.txt" ,row.names = FALSE)
