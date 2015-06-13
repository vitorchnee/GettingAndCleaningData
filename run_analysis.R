run_analysis<-function()
{
  ## Merging training and test data sets to create one data set.
  dtf1 <- read.table("train/X_train.txt")
  dtf2 <- read.table("test/X_test.txt")
  X <- rbind(dtf1, dtf2)
  dtf1 <- read.table("train/subject_train.txt")
  dtf2 <- read.table("test/subject_test.txt")
  S <- rbind(dtf1, dtf2)
  dtf1 <- read.table("train/y_train.txt")
  dtf2 <- read.table("test/y_test.txt")
  Y <- rbind(dtf1, dtf2)
  
  ## Extracting mean and standard deviation for each measurement.
  features <- read.table("features.txt")
  dtf_m_sd <- grep("-mean\\(\\)|-std\\(\\)", features[, 2])
  X <- X[, dtf_m_sd]
  names(X) <- features[dtf_m_sd, 2]
  names(X) <- gsub("\\(|\\)", "", names(X))
  names(X) <- tolower(names(X))
  
  ## Using descriptive activity names to name the activities in the data set.
  activities <- read.table("activity_labels.txt")
  activities[, 2] = gsub("_", "", tolower(as.character(activities[, 2])))
  Y[,1] = activities[Y[,1], 2]
  names(Y) <- "activity"
  
  ## Appropriately labels data set with descriptive activity names.
  names(S) <- "subject"
  dtf_all <- cbind(S, Y, X)
  write.table(dtf_all, file = "GettingCleaningData_Project_Labels.txt", row.names = FALSE)
  
  ## From the data set dtf_all, creates a second, independent tidy data set with the average of each variable for each activity and each subject.
  uniqueSubjects = unique(S)[,1]
  numSubjects = length(unique(S)[,1])
  numActivities = length(activities[,1])
  numCols = dim(dtf_all)[2]
  result = dtf_all[1:(numSubjects*numActivities), ]
  row = 1
  for (s in 1:numSubjects) {
    for (a in 1:numActivities) {
      result[row, 1] = uniqueSubjects[s]
      result[row, 2] = activities[a, 2]
      tmp <- dtf_all[dtf_all$subject==s & dtf_all$activity==activities[a, 2], ]
      result[row, 3:numCols] <- colMeans(tmp[, 3:numCols])
      row = row+1
    }
  }
  write.table(result, file = "GettingCleaningData_Project_Averages.txt", row.names = FALSE)
  print("End Run")
}
