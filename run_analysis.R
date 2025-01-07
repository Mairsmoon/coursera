#. Load library
library(tidyverse)

#. Read text files
features <- read.table("./data/features.txt")  
activity_labels <- read.table("./data/activity_labels.txt")
test_subject <- read.table("./data/test/subject_test.txt")
test_set <- read.table("./data/test/X_test.txt")
test_labels <-  read.table("./data/test/y_test.txt")
train_subject <- read.table("./data/train/subject_train.txt")
train_set <- read.table("./data/train/X_train.txt")
train_labels <-  read.table("./data/train/y_train.txt")

#. Merge the training and the test sets to create one data set 
test<-cbind(test_subject, test_labels, test_set)
feature_name<-features[ ,2]
names(test) <- c("subject", "activity", feature_name)
activity_label_name <- activity_labels[ ,2]
test$activity <- factor(test$activity, levels=c(1,2,3,4,5,6), labels=activity_label_name) 

train<-cbind(train_subject, train_labels, train_set)
names(train) <- c("subject", "activity", feature_name)
train$activity <- factor(train$activity, levels=c(1,2,3,4,5,6), labels=activity_label_name)

tidy_data <- rbind(test, train) 

#. Extract only the measurements on the mean and the standard deviation of each measurement 
tidy_mean_sd <- tidy_data[ ,c(1,2,grep("mean\\(\\)|std\\(\\)", names(tidy_data)))]

names(tidy_mean_sd)
names(tidy_mean_sd) <- gsub("-", "_", names(tidy_mean_sd), )
names(tidy_mean_sd) <- gsub("\\(\\)", "", names(tidy_mean_sd),)

#. From the data set in step 4, create a second, independent tidy data set with the average of each variable for each activity and each subject.
tidy_final <- tidy_mean_sd %>%
  group_by(subject, activity) %>%
  summarise(across(where(is.numeric), mean, na.rm=TRUE, .groups="drop"))

write.table(tidy_final, "tidy_final.txt", row.name=FALSE)
