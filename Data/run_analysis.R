## read into the data for subject, y and x with respect to training and testing
## separately, and merge the data into the total
## Assuming working directory is in "UCI HAR Dataset"
  trainS <- read.table("./train/subject_train.txt")
  testX <- read.table("./test/X_test.txt")
  testY <- read.table("./test/y_test.txt")
  testS <- read.table("./test/subject_test.txt")
  train <- cbind(trainS,trainY,trainX)
  test <- cbind(testS,testY,testX)
  total <- rbind(train,test)
## read in the all 561 features and find the matching ones to "mean" and "std"
## separately with grep function
  myname <- read.table("./features.txt")
  labels <- as.character(myname[,2])
  IDmean <- grep("mean",labels)
  IDstd <- grep("std",labels)
## according to the matching ID, specify the names, and selected columns from
## merged data frame
  meannames <- rep(" ",length(IDmean))
  stdnames <- rep(" ",length(IDstd))
  selected <- cbind(total[,1],total[,2])
  for(i in 1:length(IDmean)){
      index <- IDmean[i]
      meannames[i] <- labels[index]
      selected <- cbind(selected,total[,index+2])
  }
  for(i in 1:length(IDstd)){
      index <- IDstd[i]
      stdnames[i] <- labels[index]
      selected <- cbind(selected,total[,index+2])
  }
  selected <- as.data.frame(selected)
## Assign the column names with variable names
  names <- c("subject","activity",meannames,stdnames)
  colnames(selected) <- names
## Assign the activity names using activity labels
  labelACT <- read.table("./activity_labels.txt")
  ACTnames <- as.character(labelACT[,2])
  for(i in 1:10299){
      inter <- as.numeric(selected$activity[i])
      selected$activity[i] <- ACTnames[inter]
  }
## taking the average of each variable for each activity
  bySUB <- split(selected,selected$subject)
  output <- NULL
  for(i in 1:30){
      inter <- bySUB[[i]]
      byACT <- split(inter,inter$activity)
          for(i in 1:6){
              inter1 <- byACT[[i]]
              inter2 <- apply(inter1[,3:81],2,mean)
              inter3 <- c(inter1[1,1],inter1[1,2],inter2)
              result <- t(as.data.frame(inter3))
              output <- rbind(output,result)
          }
  }
  output <- as.data.frame(output)
  output[,1] <- as.numeric(output[,1])
  for(i in 3:81){
      output[,i]=as.numeric(levels(output[,i]))[output[,i]]
  }
## reassign row and column names, arrange the data through subject and activity
  colnames(output)=names
  rownames(output)<-c(1:180)
  library(plyr)
  Final = arrange(output,subject,activity)