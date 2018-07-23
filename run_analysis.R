# 1. Merge the training and the test sets to create one data set.

#select working directory 
setwd("C:/Users/HP/Desktop/Coursera/gdata/UCI HAR Dataset")

#Load training data and column names 
features <- read.table('./features.txt')
activitylabels <- read.table('./activity_labels.txt')
colnames(activitylabels) <- c("activityId","activityType")
subtrain <- read.table('./train/subject_train.txt') 
colnames(subtrain) <- "subjectId"
xtrain <- read.table('./train/x_train.txt')
colnames(xtrain) <- features[,2]
ytrain <- read.table('./train/y_train.txt') 
colnames(ytrain) <- "activityId"

#Merge training data
trainset = cbind(ytrain,subtrain,xtrain)

#Load test data and column names
subtest <- read.table('./test/subject_test.txt') 
colnames(subtest) <- "subjectId"
xtest <- read.table('./test/x_test.txt')
colnames(xtest) <- features[,2]
ytest <- read.table('./test/y_test.txt') 
colnames(ytest) <- "activityId"

#Merge test data
testset = cbind(ytest,subtest,xtest)

#Merge test and training data
mergeddata = rbind(trainset,testset)                                                                                                                                                                      


#2. Extracts only the measurements on the mean and standard deviation for each measurement.


#create vector for subsetting 
columnnames <- colnames(mergeddata)
vec <- (grepl("activity", columnnames) | grepl("subject", columnnames) | grepl("-mean", columnnames) | 
                grepl("-std", columnnames)) & !grepl("-meanFreq", columnnames) & !grepl("mean..-", columnnames) & 
                !grepl("std..-", columnnames)

#subset data
subsetdata <- mergeddata[vec==TRUE]


#3. Use descriptive activity names to name the activities in the data set


subsetdata <- merge(subsetdata,activitylabels,by='activityId',all.x=TRUE);
subsetdata$activityId <- activitylabels[,2][match(subsetdata$activityId, activitylabels[,1])] 
subsetdata$activityType<-NULL


#4. Appropriately labels the data set with descriptive variable names.

#create vector for subsetting 
columns <- colnames(subsetdata)

#rename colunm with more descriptive names
for (i in 1:length(columns)) 
{
        columns[i] <- gsub("\\()","",columns[i])
        columns[i] <- gsub("-std$","StdDev",columns[i])
        columns[i] <- gsub("-mean","Mean",columns[i])
        columns[i] <- gsub("^(t)","Time",columns[i])
        columns[i] <- gsub("^(f)","Freq",columns[i])
        columns[i] <- gsub("([Gg]ravity)","Gravity",columns[i])
        columns[i] <- gsub("([Bb]ody[Bb]ody|[Bb]ody)","Body",columns[i])
        columns[i] <- gsub("[Gg]yro","Gyro",columns[i])
        columns[i] <- gsub("AccMag","AccMagnitude",columns[i])
        columns[i] <- gsub("([Bb]odyaccjerkmag)","BodyAccJerkMagnitude",columns[i])
        columns[i] <- gsub("JerkMag","JerkMagnitude",columns[i])
        columns[i] <- gsub("GyroMag","GyroMagnitude",columns[i])
}

# Update subsetdata data set with new descriptive column names
colnames(subsetdata) <- columns


# 5. From the data set in step 4, creates a second, independent tidy data set with the average of each 
## variable for each activity and each subject.


# Average each activity and each subject as in new dataset
tidydata <- aggregate(subsetdata[,names(subsetdata) 
        !=c('activityId','subjectId')],by=list(activityId=subsetdata$activityId,
        subjectId=subsetdata$subjectId),mean)

# Export tidydata set 
write.table(tidydata, './tidydata.txt',row.names=FALSE,sep='\t')





