# Getting and Cleaning Data Course Project

# Instructions
# The purpose of this project is to demonstrate your ability to collect, work with, and clean a data set.
# Review criterialess 
    # The submitted data set is tidy.
    # The Github repo contains the required scripts.
    # GitHub contains a code book that modifies and updates the available codebooks with the data to indicate all the variables and summaries calculated, along with units, and any other relevant information.
    # The README that explains the analysis files is clear and understandable.
    # The work submitted for this project is the work of the student who submitted it.

# One of the most exciting areas in all of data science right now is wearable computing - see for example this article . Companies like Fitbit, Nike, and Jawbone Up are racing to develop the most advanced algorithms to attract new users. The data linked to from the course website represent data collected from the accelerometers from the Samsung Galaxy S smartphone. A full description is available at the site where the data was obtained:
    #   http://archive.ics.uci.edu/ml/datasets/Human+Activity+Recognition+Using+Smartphones

# Here are the data for the project:
    #   https://d396qusza40orc.cloudfront.net/getdata%2Fprojectfiles%2FUCI%20HAR%20Dataset.zip

# You should create one R script called run_analysis.R that does the following.
    # 1.Merges the training and the test sets to create one data set.
    # 2.Extracts only the measurements on the mean and standard deviation for each measurement.
    # 3.Uses descriptive activity names to name the activities in the data set
    # 4.Appropriately labels the data set with descriptive variable names.
    # 5.From the data set in step 4, creates a second, independent tidy data set with the average of each variable for each activity and each subject.


rm(list=ls())
if(!file.exists("./data")){dir.create("./data")}
fileUrl <- "https://d396qusza40orc.cloudfront.net/getdata%2Fprojectfiles%2FUCI%20HAR%20Dataset.zip"
download.file(fileUrl,destfile="./data/Dataset.zip",method="curl")

unzip(zipfile="./data/Dataset.zip",exdir="./data")

# Based on the info from README.txt:
      # feature.txt - 561 feature names
      # activity_label.txt - 6 activity names
      # Train set: 
          # subject_train - volunteers 1-30  
          # x_train - measurement set
          # y_train - activities 1-6
      # Test set:
          # subject_test - volunteers 1-30
          # x_test - measurement set
          # y_test - activities 1-6

      #-----------------------------------------------------------   
      # activity 1-6 | subject      | features 1-561 (2x561)
      #-------------------------------------------------------
      # y_train      | subject_train|  x_train
      # (7352x1)     | (7352x1)     |  (7352x561)
      #----------------------------------------------------------
      # y_test       | subject_test |  x_test
      # (2947x1)     | (2947x1)     |  (2947x561)
      
      
      
setwd('/Users/anna/data/UCI HAR Dataset/') 

# Reading tables
features <- read.table('./features.txt', header = FALSE)
activity_labels <- read.table('./activity_labels.txt', header = FALSE)

subject_train <- read.table('./train/subject_train.txt', header = FALSE)
x_train <- read.table('./train/x_train.txt', header = FALSE)
y_train <- read.table('./train/y_train.txt', header = FALSE)

subject_test <- read.table('./test/subject_test.txt', header = FALSE)
x_test <- read.table('./test/x_test.txt', header = FALSE)
y_test <- read.table('./test/y_test.txt', header = FALSE)


# Assigning column names
colnames(activity_labels) <- c('activityId', 'activityType')

colnames(subject_train) <- "subjectId"
colnames(x_train) <- features[ , 2]  # 2nd col only
colnames(y_train) <-"activityId"

colnames(subject_test) <- "subjectId"
colnames(x_test) <- features[ , 2] 
colnames(y_test) <- "activityId"


# 1.Merges the training and the test sets to create one data set
mergedTrain <- cbind(subject_train, x_train, y_train)
mergedTest <- cbind(subject_test, x_test, y_test)
mergedData <- rbind(mergedTrain, mergedTest)


# 2.Extracts only the measurements on the mean and standard deviation for each measurement
colNames <- colnames(mergedData)
extractMeanStd <- (grepl("activityId", colNames) | 
                   grepl("subjectId", colNames) | 
                   grepl("mean\\(\\)", colNames) |
                   grepl("std\\(\\)", colNames))

meanStdData <- mergedData[extractMeanStd == TRUE]


# 3.Use descriptive activity names to name the activities in the data set (meanStdData + activity names)
finalds <- merge(meanStdData, activity_labels, by = 'activityId', all.x = TRUE)


# 4.Appropriately labels the data set with descriptive variable names
# From features_info.txt:  
    # tBodyAcc-XYZ
    # tGravityAcc-XYZ
    # tBodyAccJerk-XYZ
    # tBodyGyro-XYZ
    # tBodyGyroJerk-XYZ
    # tBodyAccMag
    # tGravityAccMag
    # tBodyAccJerkMag
    # tBodyGyroMag
    # tBodyGyroJerkMag
    # fBodyAcc-XYZ
    # fBodyAccJerk-XYZ
    # fBodyGyro-XYZ
    # fBodyAccMag
    # fBodyAccJerkMag
    # fBodyGyroMag
    # fBodyGyroJerkMag
    
    # prefix't' - time
    # prefix'f' - frequency
    # Acc - Acceleration signals
    # Gyro - Gyroscope
    # Mag - Magnitude
    # There're also "fBodyBody...." so fix it

# gsub(pattern, replacement, x, ignore.case = FALSE, perl = FALSE, fixed = FALSE, useBytes = FALSE)
names(finalds)<-gsub("^t", "time", names(finalds))
names(finalds)<-gsub("^f", "frequency", names(finalds))
names(finalds)<-gsub("Acc", "Accelerometer", names(finalds))
names(finalds)<-gsub("Gyro", "Gyroscope", names(finalds))
names(finalds)<-gsub("Mag", "Magnitude", names(finalds))
names(finalds)<-gsub("BodyBody", "Body", names(finalds))


# 5.From the data set in step 4, creates a second, independent tidy data set with the average of 
#   each variable for each activity and each subject.

# aggregate() splits the data into subsets, computes summary statistics for each, and returns the result in a convenient form.
library(plyr)
finaldsWithAvg <- aggregate(. ~subjectId + activityId, finalds, mean)
# Reorder by subjectId
finaldsWithAvg <- finaldsWithAvg[order(finaldsWithAvg$subjectId,finaldsWithAvg$activityId),]
write.table(finaldsWithAvg, file = "finaldsWithAvg.txt",row.name=FALSE)
 

