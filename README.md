getdata009pr
============ 

For the Getting and Cleaning Data Course Project on Coursera.com (identified by the courseID getdata009).

run_analysis.R
--------------

For this script to function, it requires the `UCI HAR Dataset` [(download)](https://d396qusza40orc.cloudfront.net/getdata%2Fprojectfiles%2FUCI%20HAR%20Dataset.zip) directory to be in the R working directory. Keep the directory structure intact after extracting from the `.zip` file.

1. Merges the training and the test sets to create one data set.
  * Uses `table.read` to load the files, and `rbind()` to connect training and test sets.
2. Extracts only the measurements on the mean and standard deviation for each measurement.
  * The initial data contained both `mean()`  and `meanFreq()` variables. Only the `mean()` variables were included for this script.
3. Uses descriptive activity names to name the activities in the data set
  * Activity names were found in `activity_labels.txt`
  * This data set is clearly intended for machine learning. `X_train` and `X_test` contains the different measurements, while `y_train` and `y_test` contain the activity label (numbers from 1-6).
  * More information in the codebook.
4. Appropriately labels the data set with descriptive variable names.
  * This script reshapes the data with the `tidyr` and `dplyr` packages.
  * In the original dataset, each row was one experiment. The subject walks or lies down while a wearable device measures the activity. 
  * This script reshapes the data making it long and skinny. Each row is one measurement.
  * Tidy data is stored in the variable `X`
  * More information in the codebook.
5. From the data set in step 4, creates a second, independent tidy data set with the average of each variable for each activity and each subject.
  * Uses `group_by()` and `summarise()` to calculate the average of each variable.
  * Second data set is stored in the variable `X2`
