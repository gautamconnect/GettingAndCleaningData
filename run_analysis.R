# Keep this file in the same folder containing `UCI HAR Dataset` folder

# Merge the training and test data sets and create one data set
merge_data <- function(directory) {
  # Read the data in test and training
  path <- paste("./", directory, "/test/X_test.txt", sep="")
  test_data <- read.table(path)
  path <- paste("./", directory, "/train/X_train.txt", sep="")
  train_data <- read.table(path)
  
  # Read the activity labels
  path <- paste("./", directory, "/activity_labels.txt", sep="")
  activity_labels <- read.table(path)
  
  # Read test and training subject labels
  path <- paste("./", directory, "/train/subject_train.txt", sep="")
  subject_train <- read.table(path)
  path <- paste("./", directory, "/test/subject_test.txt", sep="")
  subject_test <- read.table(path)
  
  # Read test and training y labels
  path <- paste("./", directory, "/train/y_train.txt", sep="")
  y_train <- read.table(path)
  path <- paste("./", directory, "/test/y_test.txt", sep="")
  y_test <- read.table(path)
  
  # Merge y test and training activity labels
  y_train_labels <- merge(y_train,activity_labels,by="V1")
  y_test_labels <- merge(y_test,activity_labels,by="V1")
  
  # Merge the test and training data and the respective labels
  train_data <- cbind(subject_train,y_train_labels,train_data)
  test_data <- cbind(subject_test,y_test_labels,test_data)
  
  # Merge the test and training data
  all_data <- rbind(train_data,test_data)

  return (all_data)
}

# Extracts the mean and standard deviation for each measurement
extract_mean_std <- function(data_set, directory) {
  path <- paste("./", directory, "/features.txt", sep="")
  features_data <- read.table(path)
  
  # Subset only those rows where the name contains the word mean and std
  mean_std_rows <- subset(features_data,  grepl("(mean\\(\\)|std\\(\\))", features_data$V2) )
  
  # Set the column headers for combined data with Subject, activity_id, activity
  colnames(data_set) <- c("Subject","Activity_Id","Activity",as.vector(features_data[,2]))
  
  # Extract the data from the merged data where the column names are mean or std
  mean_columns <- grep("mean()", colnames(data_set), fixed=TRUE)
  std_columns <- grep("std()", colnames(data_set), fixed=TRUE)
  
  # Put both mean and std columns into single vector
  mean_std_column_vector <- c(mean_columns, std_columns)
  
  # Sort the vector 
  mean_std_column_vector <- sort(mean_std_column_vector)
  
  # Extract the columns with std and mean in their column headers
  extracted_data_set <- data_set[,c(1,2,3,mean_std_column_vector)]
  return (extracted_data_set)
}

# Creates a second, independent tidy data set with the average of each variable for each activity and each subject
melt_data_and_write_tidy_set <- function(data_set, path_to_tidyset_file) {
  # Melt the data
  require(reshape2)
  melted_data <- melt(data_set, id=c("Subject","Activity_Id","Activity"))
  
  # Cast the data back to the tidy_data format
  tidy_data <- dcast(melted_data, formula = Subject + Activity_Id + Activity ~ variable, mean)
  
  # Format the column names
  col_names_vector <- colnames(tidy_data)
  col_names_vector <- gsub("-mean()","Mean",col_names_vector,fixed=TRUE)
  col_names_vector <- gsub("-std()","Std",col_names_vector,fixed=TRUE)
  col_names_vector <- gsub("BodyBody","Body",col_names_vector,fixed=TRUE)
  
  # Put back in the tidy column names
  colnames(tidy_data) <- col_names_vector
  
  # Write the output into a file
  write.table(tidy_data, file=path_to_tidyset_file, sep="\t", row.names=FALSE)
}

merged_data <- merge_data("UCI HAR Dataset")
extracted_mean_std_data_set <- extract_mean_std(merged_data, "UCI HAR Dataset")
melt_data_and_write_tidy_set(extracted_mean_std_data_set, "./tidyset.txt")

# Function to read tidy dataset, if needed
read_tidy_set <- function(path_to_tidyset_file) {
  tidy_set <- read.table(path_to_tidyset_file)
  
  return (tidy_set)
}