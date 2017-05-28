## Getting and Cleaning Data Project

by Akshat S Prakash

### Description
Additional information about the variables, data and transformations used in the course project for the Johns Hopkins Getting and Cleaning Data course.

### Source Data
A full description of the data used in this project can be found at [The UCI Machine Learning Repository](http://archive.ics.uci.edu/ml/datasets/Human+Activity+Recognition+Using+Smartphones)

[The source data for this project can be found here.](https://d396qusza40orc.cloudfront.net/getdata%2Fprojectfiles%2FUCI%20HAR%20Dataset.zip)

### Data Set Information
The experiments have been carried out with a group of 30 volunteers within an age bracket of 19-48 years. Each person performed six activities (WALKING, WALKING_UPSTAIRS, WALKING_DOWNSTAIRS, SITTING, STANDING, LAYING) wearing a smartphone (Samsung Galaxy S II) on the waist. Using its embedded accelerometer and gyroscope, we captured 3-axial linear acceleration and 3-axial angular velocity at a constant rate of 50Hz. The experiments have been video-recorded to label the data manually. The obtained dataset has been randomly partitioned into two sets, where 70% of the volunteers was selected for generating the training data and 30% the test data. 

The sensor signals (accelerometer and gyroscope) were pre-processed by applying noise filters and then sampled in fixed-width sliding windows of 2.56 sec and 50% overlap (128 readings/window). The sensor acceleration signal, which has gravitational and body motion components, was separated using a Butterworth low-pass filter into body acceleration and gravity. The gravitational force is assumed to have only low frequency components, therefore a filter with 0.3 Hz cutoff frequency was used. From each window, a vector of features was obtained by calculating variables from the time and frequency domain.

### Identifier Information
activityName - The type of activity performed when the corresponding measurements were taken (WALKING, WALKING_UPSTAIRS, WALKING_DOWNSTAIRS, SITTING, STANDING, LAYING)
subjectId - The ID of the test subject (1 to 30)

### Measurement Information
For each observations in the dataset it is provided for: 
- Triaxial acceleration from the accelerometer (total acceleration) and the estimated body acceleration.
- Triaxial Angular velocity from the gyroscope.
- A 561-feature vector with time and frequency domain variables.

Following steps have been performed on the raw data to arrive at the tidy data:
* For each subject the data for training and testing has been merged
* All measurements except mean and standard deviation of the data have been filtered out
* mean for each subject and activity has been taken

Mean of the following variables for each subject for their respective activity is recorded:
- timeBodyAccMagnitudeMean
- timeBodyAccMagnitudeStdDev
- timeGravityAccMagnitudeMean
- timeGravityAccMagnitudeStdDev
- timeBodyAccJerkMagnitudeMean
- timeBodyAccJerkMagnitudeStdDev
- timeBodyGyroMagnitudeMean
- timeBodyGyroMagnitudeStdDev"    
- timeBodyGyroJerkMagnitudeMean
- timeBodyGyroJerkMagnitudeStdDev"
- freqBodyAccMagnitudeMean
- freqBodyAccMagnitudeStdDev"     
- freqBodyAccJerkMagnitudeMean
- freqBodyAccJerkMagnitudeStdDev" 
- freqBodyGyroMagnitudeMean
- freqBodyGyroMagnitudeStdDev"    
- freqBodyGyroJerkMagnitudeMean
- freqBodyGyroJerkMagnitudeStdDev"
