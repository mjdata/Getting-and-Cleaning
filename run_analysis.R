# Read All the tables
setwd("Desktop/UCI HAR Dataset/")
features <- read.table("./features.txt", header=TRUE, quote="\"")
activity_labels <- read.table("./activity_labels.txt", header=TRUE, quote="\"")
subject_train <- read.table("./train/subject_train.txt", header=TRUE, quote="\"")
X_train <- read.table("./train/X_train.txt", header=TRUE, quote="\"")
y_train <- read.table("./train/y_train.txt", header=TRUE, quote="\"")
subject_test <- read.table("./test/subject_test.txt", header=TRUE, quote="\"")
X_test <- read.table("./test/X_test.txt", header=TRUE, quote="\"")
subject_test <- read.table("./test/subject_test.txt", header=TRUE, quote="\"")
X_test <- read.table("./test/X_test.txt", header=TRUE, quote="\"")
y_test <- read.table("./test/y_test.txt", header=TRUE, quote="\"")

colnames(activity_labels) = c("activityId", "activityType")
colnames(subject_train) = c("subjectId")
colnames(X_train) = features[ ,2]
colnames(y_train) = "activityId"
colnames(subject_test) = "subjectId";
colnames(X_test) = features[,2]; 
colnames(y_test) = "activityId";

train <- cbind(y_train, subject_train, X_train)
test <- cbind(y_test, subject_test,X_test)
final <- rbind(train,test)
colNames <- colnames(final)

logicalVector <- (grepl("activity..",colNames) | grepl("subject..",colNames) | grepl("-mean..",colNames) & !grepl("-meanFreq..",colNames) & !grepl("mean..-",colNames) | grepl("-std..",colNames) & !grepl("-std()..-",colNames))
final <- final[logicalVector==TRUE]
final = merge(final,activity_labels,by='activityId',all.x=TRUE)
colNames <- colnames(final)

for (i in 1:length(colNames)){
    colNames[i] = gsub("\\()","",colNames[i])
    colNames[i] = gsub("-std$","StdDev",colNames[i])
    colNames[i] = gsub("-mean","Mean",colNames[i])
    colNames[i] = gsub("^(t)","time",colNames[i])
    colNames[i] = gsub("^(f)","freq",colNames[i])
    colNames[i] = gsub("([Gg]ravity)","Gravity",colNames[i])
    colNames[i] = gsub("([Bb]ody[Bb]ody|[Bb]ody)","Body",colNames[i])
    colNames[i] = gsub("[Gg]yro","Gyro",colNames[i])
    colNames[i] = gsub("AccMag","AccMagnitude",colNames[i])
    colNames[i] = gsub("([Bb]odyaccjerkmag)","BodyAccJerkMagnitude",colNames[i])
    colNames[i] = gsub("JerkMag","JerkMagnitude",colNames[i])
    colNames[i] = gsub("GyroMag","GyroMagnitude",colNames[i])   
}
colnames(final) <- colNames
final_no_Act <- final[,names(final) != "activityType"]
tidy <- aggregate(final_no_Act[,names(final_no_Act) != c("activityId","subjectId")],by=list(activityId=final_no_Act$activityId,subjectId = final_no_Act$subjectId),mean)
tidy <- merge(tidy,activity_labels,by='activityId',all.x=TRUE);
write.table(tidy, './tidyData.txt',row.names=FALSE,sep='\t');

