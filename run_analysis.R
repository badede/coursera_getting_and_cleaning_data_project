library(data.table)
library(plyr)

# Download data
fileUrl <- "https://d396qusza40orc.cloudfront.net/getdata%2Fprojectfiles%2FUCI%20HAR%20Dataset.zip"
download.file(fileUrl, destfile = "course4_project.zip")

# Unzip
mydata <- "course4_project.zip"
unzip(mydata)

# Read training and test datasets into R
features_train <- read.table("UCI HAR Dataset/train/X_train.txt")
subject_train <- read.table("UCI HAR Dataset/train/subject_train.txt")
activity_train <- read.table("UCI HAR Dataset/train/y_train.txt")

features_test <- read.table("UCI HAR Dataset/test/X_test.txt")
subject_test <- read.table("UCI HAR Dataset/test/subject_test.txt")
activity_test <- read.table("UCI HAR Dataset/test/y_test.txt")

# Concatenate rows for each column
features <- rbind(features_train,features_test)
subject <- rbind(subject_train,subject_test)
activity <- rbind(activity_train,activity_test)

names(subject) <- c("subject")
names(activity) <- c("activity")

# Read in features names
features_names <- read.table("UCI HAR Dataset/features.txt")

names(features) <- features_names$V2

# Combine/merge the training and test sets to create one dataset
data_all <- cbind(features, subject, activity)

# Extract only the measurements on the mean and standard deviation for each measurement
mean_std <- features_names$V2[grep("mean\\(\\)|std\\(\\)", features_names$V2)]

data_all <- subset(data_all,select=c(as.character(mean_std),"subject","activity"))

# Use descriptive activity names to name the activities in the data set
activity_labels <- read.table("UCI HAR Dataset/activity_labels.txt")

activity_labels[,2] <- as.character(activity_labels[,2])

data_all$activity <- factor(data_all$activity, levels = activity_labels[,1], labels = activity_labels[,2])
data_all$subject <- as.factor(data_all$subject)

# Label the data set with descriptive variable names
column_names <- colnames(data_all)

for (i in 1:length(column_names)){
  column_names[i] = gsub("^t", "time",column_names[i])
  column_names[i] = gsub("^f", "frequency",column_names[i])
  column_names[i] = gsub("Acc", "Accelerometer",column_names[i])
  column_names[i] = gsub("Gyro", "Gyroscope",column_names[i])
  column_names[i] = gsub("Mag", "Magnitude",column_names[i])
  column_names[i] = gsub("BodyBody", "Body",column_names[i])
  column_names[i] = gsub("\\()","",column_names[i])
  column_names[i] = gsub("\\-","",column_names[i])
  column_names[i] = gsub("mean", "Mean",column_names[i])
  column_names[i] = gsub("std", "Std",column_names[i])
}

colnames(data_all) <- column_names

# Create a second, independent tidy data set with the average of each variable 
# for each activity and each subject
forTidy<-aggregate(. ~subject + activity, data_all, mean)
forTidy<-forTidy[order(forTidy$subject,forTidy$activity),]
write.table(forTidy, file = "tidydata.txt",row.name=FALSE)

