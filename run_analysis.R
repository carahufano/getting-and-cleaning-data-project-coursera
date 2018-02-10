# Instructions for project
# The purpose of this project is to demonstrate your ability to collect, work with, and clean a data set. The goal is to prepare tidy data that can be used for later analysis. You will be graded by your peers on a series of yes/no questions related to the project. You will be required to submit: 1) a tidy data set as described below, 2) a link to a Github repository with your script for performing the analysis, and 3) a code book that describes the variables, the data, and any transformations or work that you performed to clean up the data called CodeBook.md. You should also include a README.md in the repo with your scripts. This repo explains how all of the scripts work and how they are connected.
# 
# One of the most exciting areas in all of data science right now is wearable computing - see for example this article . Companies like Fitbit, Nike, and Jawbone Up are racing to develop the most advanced algorithms to attract new users. The data linked to from the course website represent data collected from the accelerometers from the Samsung Galaxy S smartphone. A full description is available at the site where the data was obtained:
# 
#         http://archive.ics.uci.edu/ml/datasets/Human+Activity+Recognition+Using+Smartphones
# 
# Here are the data for the project:
# 
#         https://d396qusza40orc.cloudfront.net/getdata%2Fprojectfiles%2FUCI%20HAR%20Dataset.zip
# 
# You should create one R script called run_analysis.R that does the following.
# 
# Merges the training and the test sets to create one data set.
# Extracts only the measurements on the mean and standard deviation for each measurement.
# Uses descriptive activity names to name the activities in the data set
# Appropriately labels the data set with descriptive variable names.
# From the data set in step 4, creates a second, independent tidy data set with the average of each variable for each activity and each subject.

## STEP 1: GET THE DATA
        
        ## download data to dir name "data_mod3_project"

if (!file.exists("./data_mod3_project")){
        dir.create("./data_mod3_project")
        fileUrl <- "https://d396qusza40orc.cloudfront.net/getdata%2Fprojectfiles%2FUCI%20HAR%20Dataset.zip"
        download.file(fileUrl,destfile="./data_mod3_project/Dataset.zip")
        }

        ## Unzip the file if data does not exist
if(!file.exists("Dataset.zip")){
        unzip(zipfile="./data_mod3_project/Dataset.zip",exdir="./data_mod3_project")}

## STEP 2: READ THE DATA

dataPath <- file.path("./data_mod3_project/UCI HAR Dataset")

        ## read test data
trainSubjects <- read.table(file.path(dataPath, "train", "subject_train.txt"),header = FALSE)
trainActivity <- read.table(file.path(dataPath, "train", "y_train.txt"),header = FALSE)
trainValues <- read.table(file.path(dataPath, "train", "X_train.txt"),header = FALSE)

        ## read train data
testSubjects <-read.table(file.path(dataPath, "test","subject_test.txt"),header = FALSE)
testActivity <-read.table(file.path(dataPath, "test","y_test.txt"),header = FALSE)
testValues <-read.table(file.path(dataPath, "test","X_test.txt"),header = FALSE)

        ## read features
features <- read.table(file.path(dataPath, "features.txt"),as.is = TRUE) 

        ## read activity labels
activities <- read.table(file.path(dataPath, "activity_labels.txt"), header = FALSE)

## STEP 3: MERGE DATA

        ## Merge Data of individual tables
combData<- rbind(cbind(trainSubjects,trainValues,trainActivity),cbind(testSubjects,testValues,testActivity))

        ## Removing individual data tables to 
rm(testActivity,testSubjects,testValues,trainActivity,trainSubjects,trainValues)

        ## Assign Column Names
colnames(combData) <- c("subject",features[,2],"activity")

## STEP 4:Extracting the measurements on the mean and standard deviation for each measurement

        ## mean = mean, standard deviation = std, get column names with the ff keywords
keepColNames <-grepl("subject|activity|mean|std",colnames(combData))

        ## subsetting
combData<-combData[,keepColNames]

## STEP 5: Replacing Activity names 1,2,3...6(y data) to WALKING, WALKING_UPSTAIRS,... etc.(activity labels txt)

combData$activity <- factor(combData$activity, 
                                levels = activities[, 1], labels = activities[, 2])

## STEP 6: Replacing labels names with readable variable names

        ## obtaining column names from combined data set and storing to combDataNames
combDataNames<- colnames(combData)

        ## removing special characters from the column names
combDataNames <- gsub("[\\(\\)-]", "", combDataNames)

        ## replacing shortcuts to expanded names
combDataNames <- gsub("^f", "frequencyDomain", combDataNames)
combDataNames <- gsub("^t", "timeDomain", combDataNames)
combDataNames <- gsub("Acc", "Accelerometer", combDataNames)
combDataNames <- gsub("Gyro", "Gyroscope", combDataNames)
combDataNames <- gsub("Mag", "Magnitude", combDataNames)
combDataNames <- gsub("Freq", "Frequency", combDataNames)
combDataNames <- gsub("mean", "Mean", combDataNames)
combDataNames <- gsub("std", "StandardDeviation", combDataNames)
combDataNames <- gsub("BodyBody", "Body", combDataNames)

        ## combDataNames to replace column names of combData
colnames(combData) <- combDataNames

## STEP 7: Producing output text file "tidy_data.txt"

        ## grouping by subject and activity and getting the mean

library(plyr)

dataMean<-aggregate(. ~subject + activity, combData, mean)
dataMean<-dataMean[order(dataMean$subject,dataMean$activity),]

        
        ## generation of output means
write.table(dataMean, "tidy_data.txt", row.names = FALSE, quote = FALSE)

