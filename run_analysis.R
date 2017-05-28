##########################################################################################################

## Coursera Getting and Cleaning Data Course Project
## Akshat S Prakash
## 2017-05-28

# runAnalysis.R File Description:

# This script will perform the following steps on the UCI HAR Dataset downloaded from 
# https://d396qusza40orc.cloudfront.net/getdata%2Fprojectfiles%2FUCI%20HAR%20Dataset.zip 
# 1. Merge the training and the test sets to create one data set.
# 2. Extract only the measurements on the mean and standard deviation for each measurement. 
# 3. Use descriptive activity names to name the activities in the data set
# 4. Appropriately label the data set with descriptive activity names. 
# 5. Creates a second, independent tidy data set with the average of each variable for each activity and each subject. 

##########################################################################################################


## Step 0: Reading download txt files to load the data sets
rm(list = ls() )
require(dplyr)
# where the data was unzipped
setwd("~/Downloads/UCI HAR Dataset") 

features <- read.table("features.txt")
activity_labels <- read.table("activity_labels.txt")

setwd("~/Downloads/UCI HAR Dataset/test")
subject_test <- read.table("subject_test.txt")
X_test <- read.table("X_test.txt")
Y_test <- read.table("Y_test.txt")

# setwd("~/Downloads/UCI HAR Dataset/test/Inertial Signals")
# body_acc_x_test <- read.table("body_acc_x_test.txt")
# body_acc_y_test <- read.table("body_acc_y_test.txt")
# body_acc_z_test <- read.table("body_acc_z_test.txt")
# body_gyro_x_test <- read.table("body_gyro_x_test.txt")
# body_gyro_y_test <- read.table("body_gyro_y_test.txt")
# body_gyro_z_test <- read.table("body_gyro_z_test.txt")
# total_acc_x_test <- read.table("total_acc_x_test.txt")
# total_acc_y_test <- read.table("total_acc_y_test.txt")
# total_acc_z_test <- read.table("total_acc_z_test.txt")


setwd("~/Downloads/UCI HAR Dataset/train")
subject_train <- read.table("subject_train.txt")
X_train <- read.table("X_train.txt")
Y_train <- read.table("Y_train.txt")

# setwd("~/Downloads/UCI HAR Dataset/train/Inertial Signals")
# body_acc_x_train <- read.table("body_acc_x_train.txt")
# body_acc_y_train <- read.table("body_acc_y_train.txt")
# body_acc_z_train <- read.table("body_acc_z_train.txt")
# body_gyro_x_train <- read.table("body_gyro_x_train.txt")
# body_gyro_y_train <- read.table("body_gyro_y_train.txt")
# body_gyro_z_train <- read.table("body_gyro_z_train.txt")
# total_acc_x_train <- read.table("total_acc_x_train.txt")
# total_acc_y_train <- read.table("total_acc_y_train.txt")
# total_acc_z_train <- read.table("total_acc_z_train.txt")


## Step 1: Merges the training and the test sets to create one data set.
X <- rbind(X_test, X_train)
Y <- rbind(Y_test, Y_train)
subject <- rbind(subject_test, subject_train)

# body_acc_x <- rbind(body_acc_x_test, body_acc_x_train)
# body_acc_y <- rbind(body_acc_y_test, body_acc_y_train)
# body_acc_z <- rbind(body_acc_z_test, body_acc_z_train)
# 
# body_gyro_x <- rbind(body_gyro_x_test, body_gyro_x_train)
# body_gyro_y <- rbind(body_gyro_y_test, body_gyro_y_train)
# body_gyro_z <- rbind(body_gyro_z_test, body_gyro_z_train)
# 
# total_acc_x <- rbind(total_acc_x_test, total_acc_x_train)
# total_acc_y <- rbind(total_acc_y_test, total_acc_y_train)
# total_acc_z <- rbind(total_acc_z_test, total_acc_z_train)

# release the memory as the data have been merged and copied
rm(list = ls(pattern = "test"))
rm(list = ls(pattern = "train"))

# give headers to merged data set
names(X) <- features[,2]
names(Y) <- "activityId"
names(subject) <- "subjectId"

final_data <- cbind(subject, Y, X)

## Step 2: Extracts only the measurements on the mean and standard deviation for each measurement.
col_names <- names(final_data)[grep("subjectId|activityId|mean..$|std..$",names(final_data))]
final_data <- final_data[col_names]

## Step 3: Uses descriptive activity names to name the activities in the data set
names(activity_labels) <- c("activityId", "activityName")
final_data <- merge(final_data,activity_labels,"activityId")

## Step 4: Appropriately labels the data set with descriptive variable names.
# label activities
final_data$activityId <- final_data$activityName
final_data <- select(final_data, -activityName)
final_data <- rename(final_data, activityName = activityId)

# manipulate the column names
col_names <- names(final_data)
cleanCol <- function(x) {
  x <- gsub('^(t)', 'time', x)
  x <- gsub('^(f)', 'freq', x)
  x <- gsub('-mean\\()$', 'Mean', x)
  x <- gsub('-std\\()$', 'StdDev', x)
  x <- gsub('BodyBody', 'Body', x)
  x <- gsub('Mag', 'Magnitude', x)
}
names(final_data) <- sapply(col_names, cleanCol)

## Step 5: Create a second, independent tidy data set with the average of each variable for each activity and each subject.
final_tidy <- final_data %>%
  group_by(activityName, subjectId) %>%
  summarise_each(funs(mean(., na.rm=TRUE)))
# sort by subject
final_tidy <- arrange(final_tidy, subjectId)

# write output and clean env
write.table(final_tidy, '~/Downloads/UCI HAR Dataset/tidy_data.txt',row.names=FALSE,sep=' ');
# rm(list=ls())