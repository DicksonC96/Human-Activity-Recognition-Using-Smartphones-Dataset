Human Activity Recognition Using Smartphones Dataset
====================================================

This dataset was adapted from the following publication:  
Davide Anguita, Alessandro Ghio, Luca Oneto, Xavier Parra and Jorge L.
Reyes-Ortiz. Human Activity Recognition on Smartphones using a
Multiclass Hardware-Friendly Support Vector Machine. International
Workshop of Ambient Assisted Living (IWAAL 2012). Vitoria-Gasteiz,
Spain. Dec 2012

The experiments have been carried out with a group of 30 volunteers
within an age bracket of 19-48 years. Each person performed six
activities (WALKING, WALKING\_UPSTAIRS, WALKING\_DOWNSTAIRS, SITTING,
STANDING, LAYING) wearing a smartphone (Samsung Galaxy S II) on the
waist. Measurements obtained were recorded into datasets.

This study summarizes the means and standard deviations (std) of each
variables according to each activity and each subject prior to identify
the most significant variable for predicting human activity.

Procedure:
----------

1.  Training data and testing data for both signals(x) and
    activities(y), along with the subject labels were merged.

``` r
xtrain <- read.table("./data/UCI HAR Dataset/train/X_train.txt")
xtest <- read.table("./data/UCI HAR Dataset/test/X_test.txt")
ytrain <- read.table("./data/UCI HAR Dataset/train/y_train.txt")
ytest <- read.table("./data/UCI HAR Dataset/test/y_test.txt")
trainsubj <- read.table(file("./data/UCI HAR Dataset/train/subject_train.txt"))
testsubj <- read.table(file("./data/UCI HAR Dataset/test/subject_test.txt"))
train <- cbind(trainsubj, xtrain, ytrain)
test <- cbind(testsubj, xtest, ytest)
data <- rbind(train, test)
dim(data)
```

    ## [1] 10299   563

1.  features.txt was parsed to get the variable names of mean and std.

``` r
file <- file("./data/UCI HAR Dataset/features.txt")
features <- readLines(file)
close(file)
length(features)
```

    ## [1] 561

1.  mean and std variables extracted from the data set.

``` r
target <- features[grepl("mean[(]|std[(]", features)]
length(target)
```

    ## [1] 66

1.  Signal variables were renamed and improved based on features.txt

``` r
library(readr)
index <- parse_number(target)+1
msdata <- data[, c(1, index, ncol(data))]
variables <- gsub("[-0-9() ]", "", target)
variables <- gsub("mean", "MEAN", variables)
variables <- gsub("std", "STD", variables)
names(msdata) <- c("Subject_ID", variables, "Activity")
names(msdata)
```

    ##  [1] "Subject_ID"               "tBodyAccMEANX"           
    ##  [3] "tBodyAccMEANY"            "tBodyAccMEANZ"           
    ##  [5] "tBodyAccSTDX"             "tBodyAccSTDY"            
    ##  [7] "tBodyAccSTDZ"             "tGravityAccMEANX"        
    ##  [9] "tGravityAccMEANY"         "tGravityAccMEANZ"        
    ## [11] "tGravityAccSTDX"          "tGravityAccSTDY"         
    ## [13] "tGravityAccSTDZ"          "tBodyAccJerkMEANX"       
    ## [15] "tBodyAccJerkMEANY"        "tBodyAccJerkMEANZ"       
    ## [17] "tBodyAccJerkSTDX"         "tBodyAccJerkSTDY"        
    ## [19] "tBodyAccJerkSTDZ"         "tBodyGyroMEANX"          
    ## [21] "tBodyGyroMEANY"           "tBodyGyroMEANZ"          
    ## [23] "tBodyGyroSTDX"            "tBodyGyroSTDY"           
    ## [25] "tBodyGyroSTDZ"            "tBodyGyroJerkMEANX"      
    ## [27] "tBodyGyroJerkMEANY"       "tBodyGyroJerkMEANZ"      
    ## [29] "tBodyGyroJerkSTDX"        "tBodyGyroJerkSTDY"       
    ## [31] "tBodyGyroJerkSTDZ"        "tBodyAccMagMEAN"         
    ## [33] "tBodyAccMagSTD"           "tGravityAccMagMEAN"      
    ## [35] "tGravityAccMagSTD"        "tBodyAccJerkMagMEAN"     
    ## [37] "tBodyAccJerkMagSTD"       "tBodyGyroMagMEAN"        
    ## [39] "tBodyGyroMagSTD"          "tBodyGyroJerkMagMEAN"    
    ## [41] "tBodyGyroJerkMagSTD"      "fBodyAccMEANX"           
    ## [43] "fBodyAccMEANY"            "fBodyAccMEANZ"           
    ## [45] "fBodyAccSTDX"             "fBodyAccSTDY"            
    ## [47] "fBodyAccSTDZ"             "fBodyAccJerkMEANX"       
    ## [49] "fBodyAccJerkMEANY"        "fBodyAccJerkMEANZ"       
    ## [51] "fBodyAccJerkSTDX"         "fBodyAccJerkSTDY"        
    ## [53] "fBodyAccJerkSTDZ"         "fBodyGyroMEANX"          
    ## [55] "fBodyGyroMEANY"           "fBodyGyroMEANZ"          
    ## [57] "fBodyGyroSTDX"            "fBodyGyroSTDY"           
    ## [59] "fBodyGyroSTDZ"            "fBodyAccMagMEAN"         
    ## [61] "fBodyAccMagSTD"           "fBodyBodyAccJerkMagMEAN" 
    ## [63] "fBodyBodyAccJerkMagSTD"   "fBodyBodyGyroMagMEAN"    
    ## [65] "fBodyBodyGyroMagSTD"      "fBodyBodyGyroJerkMagMEAN"
    ## [67] "fBodyBodyGyroJerkMagSTD"  "Activity"

1.  Renaming the descriptive activity names (6 levels of activities)

``` r
labelfile <- readLines("./data/UCI HAR Dataset/activity_labels.txt")
labels <- sub("^[0-9] ", "", labelfile)
msdata$Activity <- factor(msdata$Activity, levels = c(1:6), labels = labels)
levels(msdata$Activity)
```

    ## [1] "WALKING"            "WALKING_UPSTAIRS"   "WALKING_DOWNSTAIRS"
    ## [4] "SITTING"            "STANDING"           "LAYING"

1.  Summarizing dataset with the average of each variable for each
    activity and each subject

``` r
library(dplyr)
```

    ## 
    ## Attaching package: 'dplyr'

    ## The following objects are masked from 'package:stats':
    ## 
    ##     filter, lag

    ## The following objects are masked from 'package:base':
    ## 
    ##     intersect, setdiff, setequal, union

``` r
df <- tbl_df(msdata)
```

    ## Warning: `tbl_df()` is deprecated as of dplyr 1.0.0.
    ## Please use `tibble::as_tibble()` instead.
    ## This warning is displayed once every 8 hours.
    ## Call `lifecycle::last_warnings()` to see where this warning was generated.

``` r
result <- df %>%
          group_by(Activity, Subject_ID) %>%
          summarize(across(tBodyAccMEANX:fBodyBodyGyroJerkMagSTD, mean)) %>%
          print
```

    ## `summarise()` regrouping output by 'Activity' (override with `.groups` argument)

    ## # A tibble: 180 x 68
    ## # Groups:   Activity [6]
    ##    Activity Subject_ID tBodyAccMEANX tBodyAccMEANY tBodyAccMEANZ tBodyAccSTDX
    ##    <fct>         <int>         <dbl>         <dbl>         <dbl>        <dbl>
    ##  1 WALKING           1         0.277       -0.0174        -0.111       -0.284
    ##  2 WALKING           2         0.276       -0.0186        -0.106       -0.424
    ##  3 WALKING           3         0.276       -0.0172        -0.113       -0.360
    ##  4 WALKING           4         0.279       -0.0148        -0.111       -0.441
    ##  5 WALKING           5         0.278       -0.0173        -0.108       -0.294
    ##  6 WALKING           6         0.284       -0.0169        -0.110       -0.297
    ##  7 WALKING           7         0.276       -0.0187        -0.111       -0.327
    ##  8 WALKING           8         0.275       -0.0187        -0.107       -0.174
    ##  9 WALKING           9         0.279       -0.0181        -0.111       -0.238
    ## 10 WALKING          10         0.279       -0.0170        -0.109       -0.179
    ## # ... with 170 more rows, and 62 more variables: tBodyAccSTDY <dbl>,
    ## #   tBodyAccSTDZ <dbl>, tGravityAccMEANX <dbl>, tGravityAccMEANY <dbl>,
    ## #   tGravityAccMEANZ <dbl>, tGravityAccSTDX <dbl>, tGravityAccSTDY <dbl>,
    ## #   tGravityAccSTDZ <dbl>, tBodyAccJerkMEANX <dbl>, tBodyAccJerkMEANY <dbl>,
    ## #   tBodyAccJerkMEANZ <dbl>, tBodyAccJerkSTDX <dbl>, tBodyAccJerkSTDY <dbl>,
    ## #   tBodyAccJerkSTDZ <dbl>, tBodyGyroMEANX <dbl>, tBodyGyroMEANY <dbl>,
    ## #   tBodyGyroMEANZ <dbl>, tBodyGyroSTDX <dbl>, tBodyGyroSTDY <dbl>,
    ## #   tBodyGyroSTDZ <dbl>, tBodyGyroJerkMEANX <dbl>, tBodyGyroJerkMEANY <dbl>,
    ## #   tBodyGyroJerkMEANZ <dbl>, tBodyGyroJerkSTDX <dbl>, tBodyGyroJerkSTDY <dbl>,
    ## #   tBodyGyroJerkSTDZ <dbl>, tBodyAccMagMEAN <dbl>, tBodyAccMagSTD <dbl>,
    ## #   tGravityAccMagMEAN <dbl>, tGravityAccMagSTD <dbl>,
    ## #   tBodyAccJerkMagMEAN <dbl>, tBodyAccJerkMagSTD <dbl>,
    ## #   tBodyGyroMagMEAN <dbl>, tBodyGyroMagSTD <dbl>, tBodyGyroJerkMagMEAN <dbl>,
    ## #   tBodyGyroJerkMagSTD <dbl>, fBodyAccMEANX <dbl>, fBodyAccMEANY <dbl>,
    ## #   fBodyAccMEANZ <dbl>, fBodyAccSTDX <dbl>, fBodyAccSTDY <dbl>,
    ## #   fBodyAccSTDZ <dbl>, fBodyAccJerkMEANX <dbl>, fBodyAccJerkMEANY <dbl>,
    ## #   fBodyAccJerkMEANZ <dbl>, fBodyAccJerkSTDX <dbl>, fBodyAccJerkSTDY <dbl>,
    ## #   fBodyAccJerkSTDZ <dbl>, fBodyGyroMEANX <dbl>, fBodyGyroMEANY <dbl>,
    ## #   fBodyGyroMEANZ <dbl>, fBodyGyroSTDX <dbl>, fBodyGyroSTDY <dbl>,
    ## #   fBodyGyroSTDZ <dbl>, fBodyAccMagMEAN <dbl>, fBodyAccMagSTD <dbl>,
    ## #   fBodyBodyAccJerkMagMEAN <dbl>, fBodyBodyAccJerkMagSTD <dbl>,
    ## #   fBodyBodyGyroMagMEAN <dbl>, fBodyBodyGyroMagSTD <dbl>,
    ## #   fBodyBodyGyroJerkMagMEAN <dbl>, fBodyBodyGyroJerkMagSTD <dbl>

==================================================================

This project includes the following files:  
- ‘README.md’  
- ‘CodeBook.md’ - Shows information about the variables used.  
- ‘run\_analysis.R’ - Script that can reproduce the data cleaning
process. - ‘meanbyActivitySubject.txt’ - Output table

==================================================================

The detailed information of the original project are as followed:

==================================================================

Human Activity Recognition Using Smartphones Dataset Version 1.0

==================================================================

Jorge L. Reyes-Ortiz, Davide Anguita, Alessandro Ghio, Luca Oneto.
Smartlab - Non Linear Complex Systems Laboratory DITEN - UniversitÃ 
degli Studi di Genova. Via Opera Pia 11A, I-16145, Genoa, Italy.
<a href="mailto:activityrecognition@smartlab.ws" class="email">activityrecognition@smartlab.ws</a>
www.smartlab.ws

==================================================================

The experiments have been carried out with a group of 30 volunteers
within an age bracket of 19-48 years. Each person performed six
activities (WALKING, WALKING\_UPSTAIRS, WALKING\_DOWNSTAIRS, SITTING,
STANDING, LAYING) wearing a smartphone (Samsung Galaxy S II) on the
waist. Using its embedded accelerometer and gyroscope, we captured
3-axial linear acceleration and 3-axial angular velocity at a constant
rate of 50Hz. The experiments have been video-recorded to label the data
manually. The obtained dataset has been randomly partitioned into two
sets, where 70% of the volunteers was selected for generating the
training data and 30% the test data.

The sensor signals (accelerometer and gyroscope) were pre-processed by
applying noise filters and then sampled in fixed-width sliding windows
of 2.56 sec and 50% overlap (128 readings/window). The sensor
acceleration signal, which has gravitational and body motion components,
was separated using a Butterworth low-pass filter into body acceleration
and gravity. The gravitational force is assumed to have only low
frequency components, therefore a filter with 0.3 Hz cutoff frequency
was used. From each window, a vector of features was obtained by
calculating variables from the time and frequency domain. See
‘features\_info.txt’ for more details.

For each record it is provided:

-   Triaxial acceleration from the accelerometer (total acceleration)
    and the estimated body acceleration.
-   Triaxial Angular velocity from the gyroscope.
-   A 561-feature vector with time and frequency domain variables.
-   Its activity label.
-   An identifier of the subject who carried out the experiment.

License: Use of this dataset in publications must be acknowledged by
referencing the following publication \[1\]

\[1\] Davide Anguita, Alessandro Ghio, Luca Oneto, Xavier Parra and
Jorge L. Reyes-Ortiz. Human Activity Recognition on Smartphones using a
Multiclass Hardware-Friendly Support Vector Machine. International
Workshop of Ambient Assisted Living (IWAAL 2012). Vitoria-Gasteiz,
Spain. Dec 2012

This dataset is distributed AS-IS and no responsibility implied or
explicit can be addressed to the authors or their institutions for its
use or misuse. Any commercial use is prohibited.

Jorge L. Reyes-Ortiz, Alessandro Ghio, Luca Oneto, Davide Anguita.
November 2012.
