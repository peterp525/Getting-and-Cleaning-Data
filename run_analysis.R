library(data.table)
library(dplyr)

#read in features and labels
features_names <- read.table("C:/Users/Peter/Documents/Coursera/JH_DS/Getting Data/week4/UCI HAR Dataset/features.txt")
activity_labels <- read.table("C:/Users/Peter/Documents/Coursera/JH_DS/Getting Data/week4/UCI HAR Dataset/activity_labels.txt", header = FALSE)

#read in train data
subject_train <- read.table("C:/Users/Peter/Documents/Coursera/JH_DS/Getting Data/week4/UCI HAR Dataset/train/subject_train.txt", header = FALSE)
y_train <- read.table("C:/Users/Peter/Documents/Coursera/JH_DS/Getting Data/week4/UCI HAR Dataset/train/y_train.txt", header = FALSE)
x_train <- read.table("C:/Users/Peter/Documents/Coursera/JH_DS/Getting Data/week4/UCI HAR Dataset/train/X_train.txt", header = FALSE)

#read in test data
subject_test <- read.table("C:/Users/Peter/Documents/Coursera/JH_DS/Getting Data/week4/UCI HAR Dataset/test/subject_test.txt", header = FALSE)
y_test <- read.table("C:/Users/Peter/Documents/Coursera/JH_DS/Getting Data/week4/UCI HAR Dataset/test/y_test.txt", header = FALSE)
x_test <- read.table("C:/Users/Peter/Documents/Coursera/JH_DS/Getting Data/week4/UCI HAR Dataset/test/X_test.txt", header = FALSE)

#1.merge training and test data sets
subject <- rbind(subject_train, subject_test)
activity <- rbind(y_train, y_test)
features <- rbind(x_train, x_test)
colnames(features) <- t(features_names[2])
colnames(activity) <- "Activity"
colnames(subject) <- "Subject"
completeData <- cbind(features,activity,subject)

#2 Extracts only the measurements on the mean and standard deviation for each measurement
columnsWithMeanSTD <- grep(".*Mean.*|.*Std.*", names(completeData), ignore.case=TRUE)
requiredColumns <- c(columnsWithMeanSTD, 562, 563)
dim(completeData)
extractedData <- completeData[,requiredColumns]
dim(extractedData)

#3 Uses descriptive activity names to name the activities in the data set
extractedData$Activity <- as.character(extractedData$Activity)
for (i in 1:6){
  extractedData$Activity[extractedData$Activity == i] <- as.character(activity_labels[i,2])
}
extractedData$Activity <- as.factor(extractedData$Activity)

#4 Appropriately labels the data set with descriptive variable names
names(extractedData)
names(extractedData)<-gsub("Acc", "Accelerometer", names(extractedData))
names(extractedData)<-gsub("Gyro", "Gyroscope", names(extractedData))
names(extractedData)<-gsub("BodyBody", "Body", names(extractedData))
names(extractedData)<-gsub("Mag", "Magnitude", names(extractedData))
names(extractedData)<-gsub("^t", "Time", names(extractedData))
names(extractedData)<-gsub("^f", "Frequency", names(extractedData))
names(extractedData)<-gsub("tBody", "TimeBody", names(extractedData))
names(extractedData)<-gsub("-mean()", "Mean", names(extractedData), ignore.case = TRUE)
names(extractedData)<-gsub("-std()", "STD", names(extractedData), ignore.case = TRUE)
names(extractedData)<-gsub("-freq()", "Frequency", names(extractedData), ignore.case = TRUE)
names(extractedData)<-gsub("angle", "Angle", names(extractedData))
names(extractedData)<-gsub("gravity", "Gravity", names(extractedData))
names(extractedData)

#5 From the data set in step 4, creates a second, independent tidy data set with the average of each variable for each activity and each subject
extractedData$Subject <- as.factor(extractedData$Subject)
extractedData <- data.table(extractedData)
tidyData <- aggregate(. ~Subject + Activity, extractedData, mean)
tidyData <- tidyData[order(tidyData$Subject,tidyData$Activity),]
write.table(tidyData, file = "C:/Users/Peter/Documents/Coursera/JH_DS/Getting Data/week4/UCI HAR Dataset/Tidy.txt", row.names = FALSE)