## title: run_analysis.R
## subtitle: Getting and Cleaning Data, Course Project
## author: "ptmerek"
## date: "February 19, 2015"
## output: "MyData.csv""

# load libraries
library(RCurl) ## for MAC OS to read https
library(stringr) ## for searching inside strings, used to find mean/std fields
library(dplyr)  ## summarizing
library(reshape2) ## melting df

setwd("/Users/petermerek/Desktop/Coursera/CleanData/Cleanprog")     


## Collect data, download data in compressed format; decompress
url<-("https://d396qusza40orc.cloudfront.net/getdata%2Fprojectfiles%2FUCI%20HAR%20Dataset.zip")
download.file(url, destfile="HARData.zip", method="curl")
unzip("/Users/petermerek/Desktop/Coursera/CleanData/Cleanprog/HARData.zip")

## Create metadata and descriptive column and variable names
features<-read.delim("/Users/petermerek/Desktop/Coursera/CleanData/Cleanprog/UCI HAR Dataset/features.txt",sep="", header=FALSE)
activity_labels<-read.delim("/Users/petermerek/Desktop/Coursera/CleanData/Cleanprog/UCI HAR Dataset/activity_labels.txt",sep="",header=FALSE)
colnames(features)<-c("feature_num", "feature")
colnames(activity_labels)<-c("activity_num", "activity")
fnames<-as.vector(features[,2])  ## variable names from features

## Read in and tidy test data (test_subject, test_x and test_y)

test_subject<-read.delim("/Users/petermerek/Desktop/Coursera/CleanData/Cleanprog/UCI HAR Dataset/test/subject_test.txt",sep="", header=FALSE)
colnames(test_subject)<-c("subject_num")

test_y<-read.delim("/Users/petermerek/Desktop/Coursera/CleanData/Cleanprog/UCI HAR Dataset/test/y_test.txt",sep="",header=FALSE)
colnames(test_y)<-c("activity_num") 

test<-cbind(test_subject,test_y) 
                 
test_x<-read.delim("/Users/petermerek/Desktop/Coursera/CleanData/Cleanprog/UCI HAR Dataset/test/X_test.txt",sep="",header=FALSE)
colnames(test_x)<-c(fnames) ## add descriptive names to variables Item 4 in grading

testM<-cbind(test,test_x) 

test1 <- merge(testM, activity_labels, by="activity_num") 
 

## Read in and tidy training data (train_subject, train_x, train_y)

train_subject<-read.delim("/Users/petermerek/Desktop/Coursera/CleanData/Cleanprog/UCI HAR Dataset/train/subject_train.txt",sep="", header=FALSE)
colnames(train_subject)<-c("subject_num")

train_y<-read.delim("/Users/petermerek/Desktop/Coursera/CleanData/Cleanprog/UCI HAR Dataset/train/y_train.txt",sep="",header=FALSE)
colnames(train_y)<-c("activity_num") 

train <- cbind(train_subject,train_y) 

train_x<-read.delim("/Users/petermerek/Desktop/Coursera/CleanData/Cleanprog/UCI HAR Dataset/train/X_train.txt",sep="",header=FALSE)
colnames(train_x)<-c(fnames) ## add descriptive names to variables, item 4 in grading 

trainM<-cbind(train,train_x) ## common row order, column combine works

train1 <- merge(trainM, activity_labels, by="activity_num") 


## Merge train and test data sets,  "melt" to long form 
data<-rbind(train1,test1)
data1 <- melt(data, id=c("activity_num", "subject_num","activity")
colnames(data1)<-c("activity_num","subject_num","activity","measurement","value") ## add descriptive names to variables, item 4 in grading 

## Extract mean and std dev only, create new tidy data set  
data2 <-
    data1 %>%    filter(str_detect(data1$measurement, 'Mean') | str_detect(data1$measurement, 'STD') | str_detect   (data1$measurement, 'mean') | str_detect(data1$measurement, 'std')) %>%
      subset(select=c(subject_num, activity, measurement , value)) %>%
         group_by(subject_num,activity,measurement) %>%
            summarise(value=mean(value))


## Write to CSV
write.table(data2, file = "MyData.csv",row.names=FALSE, na="",col.names=TRUE, sep=",")

###############################
##  cleanup of intermediate data sets
rm(train)
rm(train_x)
rm(test_y)
rm(test_subject)
rm(test_x)
rm(test)
rm(test1)
rm(train1)
rm(data)
rm(data1)
rm(data2)
