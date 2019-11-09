# Get the Data
filename <- "./getdata_projectfiles_UCI HAR Dataset.zip"
if(!file.exists(filename)){
  fileUrl <- "https://d396qusza40orc.cloudfront.net/getdata%2Fprojectfiles%2FUCI%20HAR%20Dataset.zip"
  download.file(fileUrl,destfile = filename, method = "curl")
  unzip(filename)
}
unzip(filename)

## Merge the training and the test datasets to create one dataset

### Read the train data
trainData <- read.table("./UCI HAR Dataset/train/X_train.txt")
trainLabels <- read.table("./UCI HAR Dataset/train/y_train.txt")
trainSubjects <- read.table("./UCI HAR Dataset/train/subject_train.txt")

### Examine train data
str(trainData)
table(trainLabels)
table(trainSubjects)

### Read the test data
testData <- read.table("./UCI HAR Dataset/test/X_test.txt")
testLabels <- read.table("./UCI HAR Dataset/test/y_test.txt")
testSubjects <- read.table("./UCI HAR Dataset/test/subject_test.txt")

### Examine test data
str(testData)
table(testLabels)
table(testSubjects)

### Join both datasets
data <- rbind(trainData, testData)
labels <- rbind(trainLabels, testLabels)
subjects <- rbind(trainSubjects, testSubjects)

### Examine joined data
str(data)
table(labels)
table(subjects)

## Extract only the measurements on the mean and standard deviation for each measurement 

### Get the features data
features <- read.table("./UCI HAR Dataset/features.txt")

### Examine features
str(features)

### Set the measurements value by looking for "mean" or "std"
measurements <- grep("(mean|std)\\(\\)", features[, 2])

### Get the measurements on the mean and standard deviation
data <- data[, measurements]

### Examine data
str(data)

### Remove "()" and "-" from column names
names(data) <- gsub("\\(\\)", "", features[measurements, 2])
names(data) <- gsub("-", "", names(data))

## Uses descriptive activity names to name the activities in the data set

### Get the activity labels data
activity_labels <- read.table("./UCI HAR Dataset/activity_labels.txt")

### Examine activity labels
str(activity_labels)

### Set all to lower case and remove "_"
activity_labels[, 2] <- tolower(gsub("_", "", activity_labels[, 2]))

### Merge data
labels[, 1] <- activity_labels[labels[, 1], 2]

## Appropriately label the data set with descriptive variable names

### Set the column name
names(subjects) <- "subject"
names(labels) <- "activity"

## Creates a second, independent tidy data set with the average of 
## each variable for each activity and each subject

### Create a combined dataset
combined <- cbind(subjects, labels, data)

### Examine data
str(combined)

### Load required library
require(reshape2)

### Melt the dataset
newData <- reshape2::melt(combined, id = c("subject", "activity"))

### Examine data
str(newData)

### Create the final dataset
tidyData <- reshape2::dcast(newData, subject + activity ~ variable, mean)

### Examine final dataset
View(tidyData)

### Export dataset to file
write.table(tidyData, "./tidyData.txt", quote = FALSE)
