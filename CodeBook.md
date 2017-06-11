## Codebook
This code book describes the variables, the data, and any transformations or work performed to clean up the data

## Variables
*timeBodyAccelerometerMeanX
timeBodyAccelerometerMeanY
timeBodyAccelerometerMeanZ
timeBodyAccelerometerStdX
timeBodyAccelerometerStdY
timeBodyAccelerometerStdZ
timeGravityAccelerometerMeanX
timeGravityAccelerometerMeanY
timeGravityAccelerometerMeanZ
timeGravityAccelerometerStdX
timeGravityAccelerometerStdY
timeGravityAccelerometerStdZ
timeBodyAccelerometerJerkMeanX
timeBodyAccelerometerJerkMeanY
timeBodyAccelerometerJerkMeanZ
timeBodyAccelerometerJerkStdX
timeBodyAccelerometerJerkStdY
timeBodyAccelerometerJerkStdZ
timeBodyGyroscopeMeanX
timeBodyGyroscopeMeanY
timeBodyGyroscopeMeanZ
timeBodyGyroscopeStdX
timeBodyGyroscopeStdY
timeBodyGyroscopeStdZ
timeBodyGyroscopeJerkMeanX
timeBodyGyroscopeJerkMeanY
timeBodyGyroscopeJerkMeanZ
timeBodyGyroscopeJerkStdX
timeBodyGyroscopeJerkStdY
timeBodyGyroscopeJerkStdZ
timeBodyAccelerometerMagnitudeMean
timeBodyAccelerometerMagnitudeStd
timeGravityAccelerometerMagnitudeMean
timeGravityAccelerometerMagnitudeStd
timeBodyAccelerometerJerkMagnitudeMean
timeBodyAccelerometerJerkMagnitudeStd
timeBodyGyroscopeMagnitudeMean
timeBodyGyroscopeMagnitudeStd
timeBodyGyroscopeJerkMagnitudeMean
timeBodyGyroscopeJerkMagnitudeStd
frequencyBodyAccelerometerMeanX
frequencyBodyAccelerometerMeanY
frequencyBodyAccelerometerMeanZ
frequencyBodyAccelerometerStdX
frequencyBodyAccelerometerStdY
frequencyBodyAccelerometerStdZ
frequencyBodyAccelerometerJerkMeanX
frequencyBodyAccelerometerJerkMeanY
frequencyBodyAccelerometerJerkMeanZ
frequencyBodyAccelerometerJerkStdX
frequencyBodyAccelerometerJerkStdY
frequencyBodyAccelerometerJerkStdZ
frequencyBodyGyroscopeMeanX
frequencyBodyGyroscopeMeanY
frequencyBodyGyroscopeMeanZ
frequencyBodyGyroscopeStdX
frequencyBodyGyroscopeStdY
frequencyBodyGyroscopeStdZ
frequencyBodyAccelerometerMagnitudeMean
frequencyBodyAccelerometerMagnitudeStd
frequencyBodyAccelerometerJerkMagnitudeMean
frequencyBodyAccelerometerJerkMagnitudeStd
frequencyBodyGyroscopeMagnitudeMean
frequencyBodyGyroscopeMagnitudeStd
frequencyBodyGyroscopeJerkMagnitudeMean
frequencyBodyGyroscopeJerkMagnitudeStd
subject
activity

## The data
Represents data collected from the accelerometers from the Samsung Galaxy S smartphone.

The experiments have been carried out with a group of 30 volunteers within an age bracket of 19-48 years. Each person performed six activities (WALKING, WALKING_UPSTAIRS, WALKING_DOWNSTAIRS, SITTING, STANDING, LAYING) wearing a smartphone (Samsung Galaxy S II) on the waist. Using its embedded accelerometer and gyroscope, we captured 3-axial linear acceleration and 3-axial angular velocity at a constant rate of 50Hz. The experiments have been video-recorded to label the data manually. The obtained dataset has been randomly partitioned into two sets, where 70% of the volunteers was selected for generating the training data and 30% the test data. 

The sensor signals (accelerometer and gyroscope) were pre-processed by applying noise filters and then sampled in fixed-width sliding windows of 2.56 sec and 50% overlap (128 readings/window). The sensor acceleration signal, which has gravitational and body motion components, was separated using a Butterworth low-pass filter into body acceleration and gravity. The gravitational force is assumed to have only low frequency components, therefore a filter with 0.3 Hz cutoff frequency was used. From each window, a vector of features was obtained by calculating variables from the time and frequency domain.

## Steps performed to clean up data
1. Merge the training and the test sets to create one data set.
2. Extract only the measurements on the mean and standard deviation for each measurement.
3. Use descriptive activity names to name the activities in the data set
4. Appropriately label the data set with descriptive variable names.
5. From the data set in step 4, create a second, independent tidy data set with the average of each variable for each activity and each subject.
