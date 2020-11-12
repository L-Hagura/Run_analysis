testData <- read.table("./test/X_test.txt")
testLabel <- read.table("./test/y_test.txt")
testSubject <- read.table("./test/subject_test.txt")
trainData <- read.table("./train/X_train.txt")
trainLabel <- read.table("./train/y_train.txt")
trainSubject <- read.table("./train/subject_train.txt")
features <- read.table("features.txt")
activities <- read.table("activity_labels.txt")
cleanFeatures <- filter(features, grepl(pattern = "mean|std", 
                                        x = features$V2))
cleanFeatures <- filter(cleanFeatures, !grepl(pattern = "Freq", 
                                              x = cleanFeatures$V2))
cleanTestData <- testData[,cleanFeatures$V1]
cleanTrainData <- trainData[,cleanFeatures$V1]
colnames(cleanTestData) <- cleanFeatures[,2]
colnames(testLabel) <- c("Label")
colnames(testSubject) <- c("SubjectName")
testDataInfo <- cbind(testSubject, testLabel, cleanTestData)
colnames(cleanTrainData) <- cleanFeatures[,2]
colnames(trainLabel) <- c("Label")
colnames(trainSubject) <- c("SubjectName")
trainDataInfo <- cbind(trainSubject,trainLabel,cleanTrainData)
dataInfo <- rbind(testDataInfo, trainDataInfo)
cleanDataInfo <- dataInfo[order(dataInfo$SubjectName),]
for (i in activities$V1) {
  cleanDataInfo[which(cleanDataInfo$Label == i),2] <- activities[i,2]
}
averageData <- data.frame(matrix(NA, nrow = (30*nrow(activities)), ncol = ncol(cleanDataInfo)))
colnames(averageData) <- colnames(cleanDataInfo)
for (i in 1:30) {
  averageData[c((6*i-5):(6*i)), 1] <- i
  for (j in 1:6) {
    averageData[(6*i+j-6),2] <- activities[j,2]
    for (w in 3:(ncol(cleanDataInfo))) {
      averageData[(6*i+j-6),w] <- mean(
        cleanDataInfo[which(
          cleanDataInfo$SubjectName == i & 
            cleanDataInfo$Label == activities[j,2]),w])
    }
  }
}
write.table(averageData, "run_analysis.txt", row.names = FALSE)