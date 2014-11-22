CodeBook
========
#### Background 
The data in `TidyData.txt`is for the Getting and Cleaning Data Course Project on Coursera.com. The assignment required us to download data from an [experiment](http://archive.ics.uci.edu/ml/datasets/Human+Activity+Recognition+Using+Smartphones#) on measurements from wearable technology. The data is available for [download](https://d396qusza40orc.cloudfront.net/getdata%2Fprojectfiles%2FUCI%20HAR%20Dataset.zip). **Quotes in this document are from the original experimentors.**

#### Experimental Design
From the original README;
>The experiments have been carried out with a group of 30 volunteers within an age bracket of 19-48 years. Each person performed six activities (WALKING, WALKING_UPSTAIRS, WALKING_DOWNSTAIRS, SITTING, STANDING, LAYING) wearing a smartphone (Samsung Galaxy S II) on the waist. Using its embedded accelerometer and gyroscope, we captured 3-axial linear acceleration and 3-axial angular velocity at a constant rate of 50Hz. The experiments have been video-recorded to label the data manually. The obtained dataset has been randomly partitioned into two sets, where 70% of the volunteers was selected for generating the training data and 30% the test data. 
>
>The sensor signals (accelerometer and gyroscope) were pre-processed by applying noise filters and then sampled in fixed-width sliding windows of 2.56 sec and 50% overlap (128 readings/window). The sensor acceleration signal, which has gravitational and body motion components, was separated using a Butterworth low-pass filter into body acceleration and gravity. The gravitational force is assumed to have only low frequency components, therefore a filter with 0.3 Hz cutoff frequency was used. From each window, a vector of features was obtained by calculating variables from the time and frequency domain. 

#### Raw Data
From the original `features_info.txt`:
>The features selected for this database come from the accelerometer and gyroscope 3-axial raw signals tAcc-XYZ and tGyro-XYZ. These time domain signals (prefix 't' to denote time) were captured at a constant rate of 50 Hz. Then they were filtered using a median filter and a 3rd order low pass Butterworth filter with a corner frequency of 20 Hz to remove noise. Similarly, the acceleration signal was then separated into body and gravity acceleration signals (tBodyAcc-XYZ and tGravityAcc-XYZ) using another low pass Butterworth filter with a corner frequency of 0.3 Hz. 
>
>Subsequently, the body linear acceleration and angular velocity were derived in time to obtain Jerk signals (tBodyAccJerk-XYZ and tBodyGyroJerk-XYZ). Also the magnitude of these three-dimensional signals were calculated using the Euclidean norm (tBodyAccMag, tGravityAccMag, tBodyAccJerkMag, tBodyGyroMag, tBodyGyroJerkMag). 
>
>Finally a Fast Fourier Transform (FFT) was applied to some of these signals producing fBodyAcc-XYZ, fBodyAccJerk-XYZ, fBodyGyro-XYZ, fBodyAccJerkMag, fBodyGyroMag, fBodyGyroJerkMag. (Note the 'f' to indicate frequency domain signals). 
>
>These signals were used to estimate variables of the feature vector for each pattern:  
'-XYZ' is used to denote 3-axial signals in the X, Y and Z directions.

#### Processed Data
Data was processed by the `run_analysis.R` script.

#### Data Dictionary
`SubjectID`
* The ID of the subjects coded as a factor (`1-30`)

`Activity`
* The physical activity (`walking, walking upstairs, walking downstairs, sitting, standing, laying`) the subject was performing for this measurement. 

`Domain`
* The measurements from the smartphones are originally in the domain of `Time`. A Fourier transform moves it to the `Frequency` domain.

`OriginOfEffect`
* Whether the measurement reflects the effects of `Gravity` or `Body` (motion by the subject).
* `Body` is obtained by subtracting the effects of `Gravity` from the raw data.

`Source`
* The smartphone has two measurement devices, an `Accelerometer` and a `Gyroscope`.
* **Units** for the accelerometer are in standard gravity units '**g**'
* **Units** for the gyroscope are in **radians/second**

`Statistic`
* Either the `mean` or `std` (standard deviation) of the raw data.

`Axis`
* Measurements by the device are done in the `X`, `Y`, or `Z` axis.
* `Mag` (magnitudes) are calculated using the euclidian norm of the three axes.

`Jerk`
* Some of the measurements are from the linear acceleration and angular velocity derived in time, and represent extra processing to obtain "Jerk" signals.
* `Jerk` means this measurement had this extra processing.
* a blank field ("` `") means no such processing was done.

`Average`
* **Units** depend on the `Source` (see `Source`)
* The raw data contained multiple instances of a subject performing an activity. The values in this column are from the mean of each variable (*i.e.* each unique set of the above variables) for each activity and each subject.
