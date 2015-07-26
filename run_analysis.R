###############################################################
##############################Step One#########################
###############################################################
#Merges the training and the test sets to create one data set for 
#the x, y and subject datasets.

#Extract Training sets and observes them
trainingDataX <- read.table("UCI HAR Dataset/train/X_train.txt")
trainingDataY <- read.table("UCI HAR Dataset/train/y_train.txt")
subjectTrainingData <- read.table("UCI HAR Dataset/train/subject_train.txt")
head(trainingDataY)
dim(trainingDataY)
head(trainingDataX)
dim(trainingDataX)

#Extract Test sets and observes them
testDataX <- read.table("UCI HAR Dataset/test/X_test.txt")
testDataY <- read.table("UCI HAR Dataset/test/y_test.txt")
subjectTestData <- read.table("UCI HAR Dataset/test/subject_test.txt")
head(testDataY)
dim(testDataY)
head(testDataX)
dim(testDataX)

#Bind data
#row bind both x test and training
rbindDataX <- rbind(testDataX, trainingDataX)
dim(rbindDataX)
head(rbindDataX)
tail(rbindDataX)

#row bind both y test and training
rbindDataY <- rbind(testDataY, trainingDataY)
dim(rbindDataY)
head(rbindDataY)
tail(rbindDataY)

#row bind both subject test and training
rbindDataSubject <- rbind(subjectTrainingData, subjectTestData)
dim(rbindDataSubject)
head(rbindDataSubject)
tail(rbindDataSubject)


###############################################################
##############################Step Two#########################
###############################################################

#Extracts only the measurements on the mean and standard deviation 
#for each measurement. 

#Loads in features and make dimension and header observations
features <- read.table("UCI HAR Dataset/features.txt")
dim(features) 
head(features)

#only grabs mean and standard deviation variables from the dataset
#and places it into the meSd (mean, standard deviation) variable
#based upon the second attribute in features
meSd <- grep("-(mean|std)\\(\\)", features[, 2])

#observe meSd and number of variables
meSd
length(meSd) 

#Combines meSd and rbindDataX together
rbindDataX <- rbindDataX[, meSd]
#check that the number of dimension corrensponds to the correct number 
#given previously in length(meSd)
dim(rbindDataX) 

#performs some basic cleaning
names(rbindDataX) <- gsub("[-()]" , "", features[meSd, 2])      #removes [-()]
names(rbindDataX) <- tolower(names(rbindDataX))                 #changes all cases to lower
names(rbindDataX) <- gsub("mean", "_MEAN_", names(rbindDataX))  #changes all instances of mean to _MEAN_ for easy recognition
names(rbindDataX) <- gsub("std", "_STD_", names(rbindDataX))    #changes all instances of std to _STD_ for easy recognition

#Check headers to make sure it conforms
head(rbindDataX)


###############################################################
##############################Step Three#######################
###############################################################

# Uses descriptive activity names to name the activities in the data set

# Extract activity labels from given dataset
RawActivityLabels <- read.table("UCI HAR Dataset/activity_labels.txt")

# Performs some basic cleaning
RawActivityLabels[, 2] <- gsub("_", "", RawActivityLabels[, 2])
RawActivityLabels[, 2] <- tolower(RawActivityLabels$V2)

#check factors of activity for necessary changes
factor(RawActivityLabels$V2)

#Capitalise Downstairs/Upstairs in walkingDownstairs and walkingUpstairs
substr(RawActivityLabels[2, 2], 8, 8) <- toupper(substr(RawActivityLabels[2, 2], 8, 8))
substr(RawActivityLabels[3, 2], 8, 8) <- toupper(substr(RawActivityLabels[3, 2], 8, 8))

#Change the numeric values in rbindDataY to their respective qualitative values
#from the RawActivityLabels by placing them into the activityLabels mask
activityLabels <- RawActivityLabels[rbindDataY[, 1], 2]
activityLabels
rbindDataY[, 1] <- activityLabels

#Check to make sure this occurred
head(rbindDataY)

#Name the Column
names(rbindDataY) <- "activity"
head(rbindDataY)

#Check rbindDataY for any inconsistencies
dim(rbindDataY)
class(rbindDataY$activity)
tail(rbindDataY)
factor(rbindDataY$activity)

###############################################################
##############################Step Four########################
###############################################################

#Appropriately labels the data set with descriptive variable names

#names the subject column
names(rbindDataSubject) <- "subject"

#merges the three different datasources into a single dataframe
mergedData <- cbind(rbindDataSubject, rbindDataY, rbindDataX)

#Check to ensure that this occured correctly
dim(mergedData) 
head(mergedData)
tail(mergedData)

#write the data to a text file
write.table(mergedData, "merged_data.txt") 

###############################################################
##############################Step Five########################
###############################Example#########################
#This is an example of how to compute the means without standard
#deviation, unsure if it was required to drop it
#so did both but kept the conservative one shown below where I kept
#standard deviation

#removeStdMergedData<- mergedData[,-grep("_STD_", colnames(mergedData), fixed = TRUE)] #remove std from observations
#head(removeStdMergedData)
#dim(removeStdMergedData)
#mergedDataAverages <- ddply(removeStdMergedData, .(subject, activity), function(x) colMeans(x[, 3:35]))
#head(mergedDataAverages)

###############################################################
##############################Step Five########################
###############################################################
# From the data set in step 4, creates a second, independent tidy 
# data set with the average of each variable for each activity and 
# each subject.

#Count the dimension of the merged data set
dim(mergedData)

#create a new data set which produced the mean for all observations, except
#for the first two (activity and subject)
mergedDataAverages <- ddply(mergedData, .(subject, activity), function(x) colMeans(x[, 3:68]))

#check to insure that the results display correctly
head(mergedDataAverages)
tail(mergedDataAverages)

#Write data to a text document
write.table(mergedDataAverages, "Merged_Data_Averages.txt", row.name=FALSE)
