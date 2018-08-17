# =======================
# NAME: run_analysis.R script 
# =======================
# OBJECTIVE:
# Create a tidy dataset based on the "Human Activity Recognition Using Smartphones" experiment data.

# INSTRUCTIONS SUMMARY
# 1. Merge training and testing subsets from the original data into one dataset
# 2. Extract only measurements on the mean and standard deviation for each measurement
# 3. Use descriptive activity names to name the activities in the dataset
# 4. Label all variables applicable appropriately
# 5. Create a second (independent) dataset with the average of each variable for each (subject,activity) pair

# INSTRUCTIONS EXPANDED
# 1. Merge training and testing subsets from the original data into one dataset:
#       1.1. Column-bind "subject_test" + "y_test" + "x_test" (i.e. bind together the 'subject ID' dataframe, the 'activity ID' dataframe, and the 'values' dataframe for the test subsets)
#       1.2. Same with "subject_train" + "y_train" + "x_train" (i.e. the training subsets)
#       1.3. Row-bind the previous two (1.1 + 1.2) into one single dataset

library(data.table)

# TEST SET
subject_test <- read.table("UCI HAR Dataset/test/subject_test.txt")
y_test <- read.table("UCI HAR Dataset/test/y_test.txt")
x_test <- read.table("UCI HAR Dataset/test/X_test.txt")

test_set <- cbind(subject_test,y_test,x_test)

# TRAINING SET
subject_train <- read.table("UCI HAR Dataset/train/subject_train.txt")
y_train <- read.table("UCI HAR Dataset/train/y_train.txt")
x_train <- read.table("UCI HAR Dataset/train/X_train.txt")

train_set <- cbind(subject_train,y_train,x_train)

# MERGED SET
MergedDataset <- rbind(test_set,train_set)

#---------------------------------------------------------------------------------------------------------------------------------------
# This 1st step is done by examining how the subsets should be bound together (by looking at their dimensions as well as their content).
# The idea is to have a dataset organized by subject (1st column), activity (2nd column), and all variables (3rd column onwards)
#---------------------------------------------------------------------------------------------------------------------------------------

# 2. Extract only measurements on the mean and standard deviation for each measurement:
#       2.1. Determine which variables of the merged set are applicable (i.e. only means and standard deviation)  
#               This is done by way of examining both the "features_info.txt" and the contents of the "features.txt" file
#               The "features_info.txt" file outlines the following data-obtention sequence: (1) raw signals --> (2) converted signals --> (3) calculated variables
#                      (a) Raw signals - signals obtained directly from experiments (via accelorometer + gyroscope in smartphones worn by experiment subjects) 
#                      (b) Converted signals - details in the file are as follows:
#                               - Acceleration signal split into 'body' and 'gravity acceleration' signals (tBodyAcc-XYZ, tGravityAcc-XYZ)
#                               - Body-linear acceleration and angular velocity derived in 'time' to obtain Jerk signals (tBodyAccJerk-XYZ, tBodyGyroJerk-XYZ)
#                               - Magnitude of the aforementioned calculated via Euclidean norm (tBodyAccMag, tGravityAccMag, tBodyAccJerkMag, tBodyGyroMag, tBodyGyroJerkMag)
#                               - Fast Fourier Transform applied to some of the previously mentioned to obtain: fBodyAcc-XYZ, fBodyAccJerk-XYZ, fBodyGyro-XYZ, fBodyAccJerkMag, fBodyGyroMag, fBodyGyroJerkMag
#                      (c) Calculated variables - these were estimated based on the previously converted signals, by using the following calculations:
#                          Mean, standard deviation, median absolute deviation, largest value, smallest value, signal magnitude area, among several others.

#               In summary, there are 561 variables which stem directly from step (c) in this explanation (i.e., for every converted signal, multiple calculations were applied). 
#               Of those, this project is only interested in those obtained from 'mean' and 'standard deviation' (this excludes some seemingly ambiguous ones such as 'angle(Z,gravityMean)')
#               The "features.txt" file provides a full list of the variables, from which the desired variables (according to numeration in the "features.txt" file) would be: 
#                      features <- read.table("UCI HAR Dataset/features.txt")
#                      desiredvariables <- features[c(1,2,3,4,5,6,41,42,43,44,45,46,81,82,83,84,85,86,121,122,123,124,125,126,161,162,163,164,165,166,201,202,214,215,227,228,240,241,253,254,266,267,268,269,270,271,345,346,347,348,349,350,424,425,426,427,428,429,503,504,516,517,529,530,542,543) ,2]
#               
#               When extracting this subset from the "MergedDataset" in step 1.3, it is important to contemplate that two columns were added to the far-left (subject IDs + activity IDs)
#               To account for this, add "+2" (desiredvariables + 2) to the previously detailed numeration BUT ALSO RE-ADD THE FIRST TWO COLUMNS (subjectID and activityID):
#                      numeration <- c(1,2,3,4,5,6,7,8,43,44,45,46,47,48,83,84,85,86,87,88,123,124,125,126,127,128,163,164,165,166,167,168,203,204,216,217,229,230,242,243,255,256,268,269,270,271,272,273,347,348,349,350,351,352,426,427,428,429,430,431,505,506,518,519,531,532,544,545)

#               Therefore, the desired subset will be obtained via subsetting => MergedDataset[ , numeration]

# MERGED SET SUBSET OF ONLY mean() AND sd()
numeration <- c(1,2,3,4,5,6,7,8,43,44,45,46,47,48,83,84,85,86,87,88,123,124,125,126,127,128,163,164,165,166,167,168,203,204,216,217,229,230,242,243,255,256,268,269,270,271,272,273,347,348,349,350,351,352,426,427,428,429,430,431,505,506,518,519,531,532,544,545)
MergedDataset <- MergedDataset[ ,numeration]

#---------------------------------------------------------------------------------------------------------------------------------------

# 3. Use descriptive activity names to name the activities in the dataset:
#       For this item, the «dplyr» package will be used. Namely, the mutate() + recode() functions, to redefine the values in the "V1.1" column,
#       from {1,2,3,4,5,6} to {WALKING, WALKING_UPSTAIRS, WALKING_DOWNSTAIRS, SITTING, STANDING, LAYING}, respectively.

# RE-NAME ACTIVITIES IN 'MergedDataset' (second column of the dataset)
library(dplyr)
MergedDataset  <- mutate(MergedDataset, V1.1 = recode(MergedDataset$V1.1,"1"= "WALKING", "2"="WALKING_UPSTAIRS", "3"="WALKING_DOWNSTAIRS", "4"="SITTING", "5"="STANDING", "6"="LAYING"))

#---------------------------------------------------------------------------------------------------------------------------------------

# 4. Label all variables applicable appropriately:
#       At this stage, we have a dataset consisting of only the sixty-six "mean()" and "sd()" variables (plus "subjectID" and "activityID")
#       In order to rename them, the following criteria will be applied:

#               OLD NOMENCLATURE -> [t/f][converted signal]-[calculation] -(X/Y/Z)
#                      1. "[]" symbols indicate a text string which applies for all variables
#                      2. "()" symbols indicate a text string which only applies for some variables
#                      3. "/" indicates "OR", meaning that one of the options between "[]" or "()" will apply
#                      4. "-" is meant to separate the "converted signal" being used (see «Instructions Expanded» 2.1.b) from the "calculation" being applied (see «Instructions Expanded» 2.1.c), 
#                         or it can also indicate the axis (X/Y/Z) in a 3-axial variable (if applicable)
#                      5. Examples:
#                               -> tBodyAcc-mean()-X
#                               -> tBodyAcc-std()-X
#                               -> fBodyAccMag-mean()
#                               -> fBodyAccMag-std()

#               NEW NOMENCLATURE -> [time/freq].[converted signal].(AxisX/AxisY/AxisZ).[calculation]
#                      1. "[]" symbols indicate a text string that appears for all variables
#                      2. "()" symbols indicate a text string which only applies for some variables
#                      3. "/" indicates "OR", meaning that one of the options between "[]" or "()" will apply 
#                      4. "." separates text strings
#                      5. Examples (same as those of old nomenclature above):
#                               -> Time.BodyAcc.AxisX.Mean
#                               -> Time.BodyAcc.AxisX.Std
#                               -> Freq.BodyAccMag.Mean
#                               -> Freq.BodyAccMag.Std

# RE-NAME VARIABLES IN 'MergedDataset'
newvariablenames <- c("subjectID","activityID","Time.BodyAcc.AxisX.Mean","Time.BodyAcc.AxisY.Mean","Time.BodyAcc.AxisZ.Mean","Time.BodyAcc.AxisX.Std","Time.BodyAcc.AxisY.Std","Time.BodyAcc.AxisZ.Std","Time.GravityAcc.AxisX.Mean","Time.GravityAcc.AxisY.Mean","Time.GravityAcc.AxisZ.Mean","Time.GravityAcc.AxisX.Std","Time.GravityAcc.AxisY.Std","Time.GravityAcc.AxisZ.Std","Time.BodyAccJerk.AxisX.Mean","Time.BodyAccJerk.AxisY.Mean","Time.BodyAccJerk.AxisZ.Mean","Time.BodyAccJerk.AxisX.Std","Time.BodyAccJerk.AxisY.Std","Time.BodyAccJerk.AxisZ.Std","Time.BodyGyro.AxisX.Mean","Time.BodyGyro.AxisY.Mean","Time.BodyGyro.AxisZ.Mean","Time.BodyGyro.AxisX.Std","Time.BodyGyro.AxisY.Std","Time.BodyGyro.AxisZ.Std","Time.BodyGyroJerk.AxisX.Mean","Time.BodyGyroJerk.AxisY.Mean","Time.BodyGyroJerk.AxisZ.Mean","Time.BodyGyroJerk.AxisX.Std","Time.BodyGyroJerk.AxisY.Std","Time.BodyGyroJerk.AxisZ.Std","Time.BodyAccMag.Mean","Time.BodyAccMag.Std","Time.GravityAccMag.Mean","Time.GravityAccMag.Std","Time.BodyAccJerkMag.Mean","Time.BodyAccJerkMag.Std","Time.BodyGyroMag.Mean","Time.BodyGyroMag.Std","Time.BodyGyroJerkMag.Mean","Time.BodyGyroJerkMag.Std","Freq.BodyAccAxisX.Mean","Freq.BodyAccAxisY.Mean","Freq.BodyAccAxisZ.Mean","Freq.BodyAccAxisX.Std","Freq.BodyAccAxisY.Std","Freq.BodyAccAxisZ.Std","Freq.BodyAccJerkAxisX.Mean","Freq.BodyAccJerkAxisY.Mean","Freq.BodyAccJerkAxisZ.Mean","Freq.BodyAccJerkAxisX.Std","Freq.BodyAccJerkAxisY.Std","Freq.BodyAccJerkAxisZ.Std","Freq.BodyGyroAxisX.Mean","Freq.BodyGyroAxisY.Mean","Freq.BodyGyroAxisZ.Mean","Freq.BodyGyroAxisX.Std","Freq.BodyGyroAxisY.Std","Freq.BodyGyroAxisZ.Std","Freq.BodyAccMag.Mean","Freq.BodyAccMag.Std","Freq.BodyAccJerkMag.Mean","Freq.BodyAccJerkMag.Std","Freq.BodyGyroMag.Mean","Freq.BodyGyroMag.Std","Freq.BodyGyroJerkMag.Mean","Freq.BodyGyroJerkMag.Std")
colnames(MergedDataset) <- newvariablenames

#---------------------------------------------------------------------------------------------------------------------------------------

# 5. Create a second (independent) dataset with the average of each variable for each (subject,activity) pair
#       5.1. Firstly an auxiliary dataset is created by grouping on "subjectID" and "activityID"
#       5.2. Secondly, this auxiliary dataset is summarized utilizing mean() for every subject/activity pair (therefore collapsing all rows with matching subject/activity into one row with an average value)
#       5.3. Columns are then renamed. The only change (applicable to all columns except "subjectID" and "activityID") is the addition of ".SummaryAvg" to all variables

# CREATE SECOND DATASET WITH THE AVERAGE OF EVERY SUBJECT/ACTIVITY PAIR:
AuxiliarySet <- dplyr::group_by(MergedDataset, subjectID, activityID)
TidyDataset <- dplyr::summarize_all(AuxiliarySet, mean)
TidyDatasetRenamedColumns  <- c("subjectID","activityID","Time.BodyAcc.AxisX.Mean.SummaryAvg","Time.BodyAcc.AxisY.Mean.SummaryAvg","Time.BodyAcc.AxisZ.Mean.SummaryAvg","Time.BodyAcc.AxisX.Std.SummaryAvg","Time.BodyAcc.AxisY.Std.SummaryAvg","Time.BodyAcc.AxisZ.Std.SummaryAvg","Time.GravityAcc.AxisX.Mean.SummaryAvg","Time.GravityAcc.AxisY.Mean.SummaryAvg","Time.GravityAcc.AxisZ.Mean.SummaryAvg","Time.GravityAcc.AxisX.Std.SummaryAvg","Time.GravityAcc.AxisY.Std.SummaryAvg","Time.GravityAcc.AxisZ.Std.SummaryAvg","Time.BodyAccJerk.AxisX.Mean.SummaryAvg","Time.BodyAccJerk.AxisY.Mean.SummaryAvg","Time.BodyAccJerk.AxisZ.Mean.SummaryAvg","Time.BodyAccJerk.AxisX.Std.SummaryAvg","Time.BodyAccJerk.AxisY.Std.SummaryAvg","Time.BodyAccJerk.AxisZ.Std.SummaryAvg","Time.BodyGyro.AxisX.Mean.SummaryAvg","Time.BodyGyro.AxisY.Mean.SummaryAvg","Time.BodyGyro.AxisZ.Mean.SummaryAvg","Time.BodyGyro.AxisX.Std.SummaryAvg","Time.BodyGyro.AxisY.Std.SummaryAvg","Time.BodyGyro.AxisZ.Std.SummaryAvg","Time.BodyGyroJerk.AxisX.Mean.SummaryAvg","Time.BodyGyroJerk.AxisY.Mean.SummaryAvg","Time.BodyGyroJerk.AxisZ.Mean.SummaryAvg","Time.BodyGyroJerk.AxisX.Std.SummaryAvg","Time.BodyGyroJerk.AxisY.Std.SummaryAvg","Time.BodyGyroJerk.AxisZ.Std.SummaryAvg","Time.BodyAccMag.Mean.SummaryAvg","Time.BodyAccMag.Std.SummaryAvg","Time.GravityAccMag.Mean.SummaryAvg","Time.GravityAccMag.Std.SummaryAvg","Time.BodyAccJerkMag.Mean.SummaryAvg","Time.BodyAccJerkMag.Std.SummaryAvg","Time.BodyGyroMag.Mean.SummaryAvg","Time.BodyGyroMag.Std.SummaryAvg","Time.BodyGyroJerkMag.Mean.SummaryAvg","Time.BodyGyroJerkMag.Std.SummaryAvg","Freq.BodyAccAxisX.Mean.SummaryAvg","Freq.BodyAccAxisY.Mean.SummaryAvg","Freq.BodyAccAxisZ.Mean.SummaryAvg","Freq.BodyAccAxisX.Std.SummaryAvg","Freq.BodyAccAxisY.Std.SummaryAvg","Freq.BodyAccAxisZ.Std.SummaryAvg","Freq.BodyAccJerkAxisX.Mean.SummaryAvg","Freq.BodyAccJerkAxisY.Mean.SummaryAvg","Freq.BodyAccJerkAxisZ.Mean.SummaryAvg","Freq.BodyAccJerkAxisX.Std.SummaryAvg","Freq.BodyAccJerkAxisY.Std.SummaryAvg","Freq.BodyAccJerkAxisZ.Std.SummaryAvg","Freq.BodyGyroAxisX.Mean.SummaryAvg","Freq.BodyGyroAxisY.Mean.SummaryAvg","Freq.BodyGyroAxisZ.Mean.SummaryAvg","Freq.BodyGyroAxisX.Std.SummaryAvg","Freq.BodyGyroAxisY.Std.SummaryAvg","Freq.BodyGyroAxisZ.Std.SummaryAvg","Freq.BodyAccMag.Mean.SummaryAvg","Freq.BodyAccMag.Std.SummaryAvg","Freq.BodyAccJerkMag.Mean.SummaryAvg","Freq.BodyAccJerkMag.Std.SummaryAvg","Freq.BodyGyroMag.Mean.SummaryAvg","Freq.BodyGyroMag.Std.SummaryAvg","Freq.BodyGyroJerkMag.Mean.SummaryAvg","Freq.BodyGyroJerkMag.Std.SummaryAvg")
colnames(TidyDataset)  <- TidyDatasetRenamedColumns

View(TidyDataset)
#---------------------------------------------------------------------------------------------------------------------------------------
# ///////////////////// END OF SCRIPT /////////////////////
#---------------------------------------------------------------------------------------------------------------------------------------
