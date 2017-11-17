fn <- function(fileName) {
        paste(".", datadir, fileName, sep = "/")
}

## Merges the training and the test sets to create one data set.
datadir <- "UCI HAR Dataset"
# Names of columns from features.txt
fnF <- fn("features.txt")
features_raw <- read.table(fnF, sep = " ", stringsAsFactors = FALSE)
features <- features_raw[, 2]

fnXtr <- fn("train/X_train.txt")
X_train <- read.fwf(fnXtr, rep(16, 561), sep="", header = FALSE, n=7352, row.names = NULL,
                   col.names = features, colClasses = rep("numeric", 561), buffersize = 100)
fnXts<- fn("test/X_test.txt")
X_test <- read.fwf(fnXts, rep(16, 561), sep="", header = FALSE, n=2947, row.names = NULL,
                   col.names = features, colClasses = rep("numeric", 561), buffersize = 100)
X <- rbind(X_train, X_test)

## Extracts only the measurements on the mean and standard deviation for each measurement
X_mean_std <- X[, grep("(.*)(tBodyAcc\\.|tGravityAcc\\.|tBodyGyro\\.)(mean|std)(.*)",
                          names(X), value = TRUE)]

## Uses descriptive activity names to name the activities in the data set
fnActLbl <- fn("activity_labels.txt")
activityLabels <- read.table(fnActLbl, sep = " ", stringsAsFactors = FALSE,
                             col.names = c("numAct", "libAct"))

fnActivities_train <- fn("train/y_train.txt")
activities_train <- read.table(fnActivities_train, stringsAsFactors = FALSE, col.names = c("numAct"))
fnActivities_test <- fn("test/y_test.txt")
activities_test <- read.table(fnActivities_test, stringsAsFactors = FALSE, col.names = c("numAct"))
activities <- rbind(activities_train, activities_test)

X_mean_std_actnum <- cbind(activities, X_mean_std)

library(plyr)
mergedData <- join(activityLabels, X_mean_std_actnum)

## Appropriately labels the data set with descriptive variable names
names(mergedData) <- c("activitynumber", "activitylabel",
                       "bodyaccelerationmeanalongx", "bodyaccelerationmeanalongy", "bodyaccelerationmeanalongz",
                       "bodyaccelerationstandarddeviationalongx", "bodyaccelerationstandarddeviationalongy",
                       "bodyaccelerationstandarddeviationalongz",
                       "gravityaccelerationmeanalongx", "gravityaccelerationmeanalongy", "gravityaccelerationmeanalongz",
                       "gravityaccelerationstandarddeviationalongx", "gravityaccelerationstandarddeviationalongy",
                       "gravityaccelerationstandarddeviationalongz",
                       "bodyangularvelocitymeanalongx", "bodyangularvelocitymeanalongy",
                       "bodyangularvelocitymeanalongz",
                       "bodyangularvelocitystandarddeviationalongx", "bodyangularvelocitystandarddeviationalongy",
                       "bodyangularvelocitystandarddeviationalongz")

## From the data set in step 4, creates a second, independent tidy data set
## with the average of each variable for each activity and each subject
fnSubjects_train <- fn("train/subject_train.txt")
subjects_train <- read.table(fnSubjects_train, stringsAsFactors = FALSE,
                             col.names = c("subjectid"), colClasses = c("integer"))
fnSubjects_test <- fn("test/subject_test.txt")
subjects_test <- read.table(fnSubjects_test, stringsAsFactors = FALSE,
                            col.names = c("subjectid"), colClasses = c("integer"))
subjects <- rbind(subjects_train, subjects_test)

mm <- cbind(subjects, mergedData)

library(reshape2)

id_vars <- list(mm$subjectid, mm$activitylabel)

agg <- aggregate.data.frame(mm[, 4:21], id_vars, mean, drop = TRUE)
names(agg)[1:2] <- c("subjectid", "activitylabel")

fnResult <- fn("result_average-mean-std.txt")
write.table(agg, fnResult, row.names = FALSE)
