###########################################################################################################################################
#0.Preliminary work, including loading packages, setting working path,  downloading datafile, unzipping file.
###########################################################################################################################################
# Setting working directory, then downloading data file only if the datafile doesn't exists.
#if(!file.exists("./getdata009pr")){dir.create("./getdata009pr")}
setwd("./getdata009pr")
fileurl <- "https://d396qusza40orc.cloudfront.net/getdata%2Fprojectfiles%2FUCI%20HAR%20Dataset.zip"
if(!file.exists("Dataset.zip")){download.file(fileurl, destfile = "Dataset.zip", mode = "wb")}
# Unzipping data file in working path by calling the winRAR.exe command, then listing the unzipped files.(platform: Win8.1, 64bit)
executable <- file.path("D:", "software/Totalcmd/Tools", "WinRAR", "WinRAR.exe")
parameters <- "x"
cmd <- paste(paste0("\"", executable, "\""), parameters, paste0("\"", file.path(getwd(), "Dataset.zip"), "\""))
system(cmd)

###########################################################################################################################################
#1.Merges the training and the test sets to create one data set.
###########################################################################################################################################
pathIn <- file.path(getwd(), "UCI HAR Dataset"); list.files(pathIn, recursive = TRUE)

#Read train files 
xtrain <- read.table(file.path(pathIn, "train", "X_train.txt"));              dim(xtrain)        #7352  561
subjectTrain <- read.table(file.path(pathIn, "train", "subject_train.txt"));  dim(subjectTrain)  #7352    1
ytrain <- read.table(file.path(pathIn, "train", "Y_train.txt"));              dim(ytrain)        #7352    1

#Read test files
xtest <- read.table(file.path(pathIn, "test", "X_test.txt"));                  dim(xtest)        #2947  561
subjectTest <- read.table(file.path(pathIn, "test", "subject_test.txt"));      dim(subjectTest)  #2947    1
ytest <- read.table(file.path(pathIn, "test", "Y_test.txt"));                  dim(ytest)        #2947    1

#Merging data into one dataset
X <- rbind(xtrain, xtest);dim(X)                           #10299   561
y <- rbind(ytrain, ytest); dim(y)                          #10299     1 
subjects <- rbind(subjectTrain, subjectTest);dim(subjects) #10299     1

###########################################################################################################################################
#2.Extracts only the measurements on the mean and standard deviation for each measurement. 
###########################################################################################################################################
features <- read.table(file.path(pathIn,  "features.txt"));  dim(features) #561   2
features <- features[,2]
meanAndStdCol <- grep("mean\\(\\)|std\\(\\)", features);length(meanAndStdCol) # 66
X = X[,meanAndStdCol]

# process feature names
features <- features[meanAndStdCol]
features <- gsub("BodyBody", "Body", features)
features <- gsub("\\(\\)", "", features)
features <- gsub("-","",features)
# Put in underscores to separate different variables
features <- gsub(
    "^(t|f)(Body|Gravity)(Acc|Gyro)(Jerk)?(Mag)?(mean|std)(X|Y|Z)?",
    "\\1_\\2_\\3_\\6_\\7\\5_\\4",features)
features <- gsub("_$", "_ ", features)

###########################################################################################################################################
#3.Uses descriptive activity names to name the activities in the data set
###########################################################################################################################################
activityLabels <-  read.table(file.path(pathIn,  "activity_labels.txt"));activityLabels
activityLabels <- tolower(activityLabels[,2])
activityLabels <- sub("_", " ", activityLabels)
temp <- as.factor(y[,1])
levels(temp) <- activityLabels # give names to variables
y[,1] <- temp
names(y) <- "Activity"
names(subjects) <- "SubjectID"
X <- cbind(subjects,y,X) # join everything together
dim(X)

###########################################################################################################################################
#4.Appropriately labels the data set with descriptive variable names. 
###########################################################################################################################################
names(X) 
featureNames <- as.vector(features);featureNames
colnames(X) <- c("SubjectID","Activity", featureNames )
colnames(X)

###########################################################################################################################################
#5.From the data set in step 4, creates a second, independent tidy data set with the average of each variable for each activity and each subject.
###########################################################################################################################################
library(dplyr)
library(tidyr)
X <- tbl_df(X) # convert to data table
X <- X %>% gather(M,Value,-SubjectID,-Activity) %>%
    separate(M,
             c("Domain","OriginOfEffect","Source","Statistic","Axis","Jerk"),
             sep="_",
             remove=T) %>%
    mutate(Domain = ifelse(Domain=="t","Time","Frequency")) %>%
    mutate(Source = ifelse(Source=="Acc","Accelerometer","Gyroscope"))

X2 <- X %>%
    group_by(SubjectID,Activity,Domain,OriginOfEffect,Source,Statistic,Axis,Jerk) %>%
    summarise(Average = mean(Value))

write.table(X2,"TidyData.txt",row.names=F)