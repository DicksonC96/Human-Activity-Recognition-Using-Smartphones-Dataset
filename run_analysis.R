### Reading data sets from text files
# Inertial signals
xtrain <- read.table("./data/UCI HAR Dataset/train/X_train.txt")
xtest <- read.table("./data/UCI HAR Dataset/test/X_test.txt")
# Activities
ytrain <- read.table("./data/UCI HAR Dataset/train/y_train.txt")
ytest <- read.table("./data/UCI HAR Dataset/test/y_test.txt")
# Subject id
trainsubj <- read.table(file("./data/UCI HAR Dataset/train/subject_train.txt"))
testsubj <- read.table(file("./data/UCI HAR Dataset/test/subject_test.txt"))

### Merging all data sets (subjects > signals > activities)
train <- cbind(trainsubj, xtrain, ytrain)
test <- cbind(testsubj, xtest, ytest)
data <- rbind(train, test)


### Parsing features.txt to filter out mean and std variable names
file <- file("./data/UCI HAR Dataset/features.txt")
features <- readLines(file)
close(file)
target <- features[grepl("mean[(]|std[(]", features)]

### Extracting the mean and std variables from the data set
library(readr)
index <- parse_number(target)+1 #considering the first ID column
msdata <- data[, c(1, index, ncol(data))]

### Renaming the variables (Subject_ID, variables, Activity)
variables <- gsub("[-0-9() ]", "", target)
variables <- gsub("mean", "MEAN", variables)
variables <- gsub("std", "STD", variables)
names(msdata) <- c("Subject_ID", variables, "Activity")

### Renaming the descriptive activity names (6 levels of activities)
labelfile <- readLines("./data/UCI HAR Dataset/activity_labels.txt")
labels <- sub("^[0-9] ", "", labelfile)
msdata$Activity <- factor(msdata$Activity, levels = c(1:6), labels = labels)


### Second independent tidy data set with the average of each variable 
### for each activity and each subject
library(dplyr)
df <- tbl_df(msdata)
result <- df %>%
          group_by(Activity, Subject_ID) %>%
          summarize(across(tBodyAccMEANX:fBodyBodyGyroJerkMagSTD, mean)) %>%
          print
View(result)

### Optional
write.table(result, "./meanbyActivitySubject.txt", row.names = FALSE)