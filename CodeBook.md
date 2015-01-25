# CodeBook
Description: a code book that describes the variables, the data, and any transformations or work that you performed to clean up the data called CodeBook.md. 

## Assumptions
It is assumed that the Samsung data is unzipped into the working directory. The data therefore resides in the same folder as `run_analysis.R`

In other words, it should be like this:
```
 ---
  |
  |---run_analysis.R
  |
  |---UCI HAR Dataset/
```

## Variables

On lines 109-111 of run_analysis.R,

```
merged_data <- merge_data("UCI HAR Dataset")
extracted_mean_std_data_set <- extract_mean_std(merged_data, "UCI HAR Dataset")
melt_data_and_write_tidy_set(extracted_mean_std_data_set, "./tidyset.txt")
```
These are the lines of code that will kickstart the process of cleaning the data.

Major variables at the global environment are therefore:
 - merged_data: the data table containing the merged training and test dataset from `X_test.txt` and `X_train.txt`. This amounts to 10299 observations with 564 columns. The columns include the 561 features stated in `features.txt` and added `Subject`, `Activity Id`, and `Activity` columns which are taken from the various `activities.txt` and y labels. Column headers are included.
  - extracted_mean_std_data_set: the data table containing 10299 observations of 69 columns. This is derived by subsetting the `merged_data` from above. The 69 columns include the subject, activity, activity id columns. The other 66 columns are the columns with either the characters `mean()` or `std()` in the column headers. Column headers are included.
  
## merge_data
### Data of merge_data
Inside the function `merge_data`, we take data from the following text files:

 - `test/X_test.txt` will give us the local variable, `test_data` containing all the 541 columns of raw data in the test set
 - `train/X_train.txt` will give us the local variable, `train_data` containing all the 541 columns of raw data in the training set
 - `activities.txt` will give us the local variable, `activity_labels` containing all the different types of activities: `WALKING`, `WALKING_UPSTAIRS`, `SITTING`, etc
 - `train/subject_train.txt` will give us the local variable, `subject_train` containing all the possible subject data inside the training set.
 - `test/subject_test.txt` will give us the local variable, `subject_test` containing all the possible subject data inside the test set.
 - `train/y_train.txt` will give us the local variable, `y_train` containing all the possible label data inside the training set.
 - `test/y_test.txt` will give us the local variable, `y_test` containing all the possible label data inside the test set. 
 
### Transformations inside merge_data

 - `y_train_labels` is the merged data table of the label data based on the activity labels for training set because `y_train` alone tells us just 1, 2,3 when we want to have tha actual labels displayed as WALKING, etc.
 - `y_test_labels` is the merged data table of the label data based on the activity labels for test set because `y_test` alone tells us just 1, 2,3 when we want to have tha actual labels displayed as WALKING, etc.
 
 - `train_data` is the merged data table of the training data with the subject, activity, activity id, and the other 541 variables
 - `test_data` is the merged data table of the test data with the subject, activity, activity id, and the other 541 variables
 
 - `all_data` is the merged version of `train_data` and `test_data`
  
## extract_mean_std
### Data of extract_mean_std
Inside the function `extract_mean_std`, we take data from the following text files:

 - `features.txt` will give us the local variable, `features_data` containing all the 541 features and their names
 
### Transformations inside extract_mean_std

 - `mean_std_rows` is the subset data from the `features_data` where just the mean() and std() columns are extracted.
 - perform a colnames() on the giant data set to have the 3 new columns and the column headers for subject, activity, and activity id
 - `mean_columns` is the extracted data from the `data_set` where the mean() columns are extracted.
 - `std_columns` is the extracted data from the `data_set` where the std() columns are extracted. 
 - `mean_std_column_vector` is the vector containing the `mean_columns` and `std_columns` and sorted.
 - `extracted_data_set` is the final data table containing the mean and std columns and the 3 additional columns of Suject, Activity, and Activity Id

## melt_data_and_write_tidy_set
### Data of melt_data_and_write_tidy_set

 - All local variables inside this function are all derived after transformations
 
### Transformations inside melt_data_and_write_tidy_set

 - `melted_data` is the data after performing melt_data on the `data_set` parameter so that we can isolate down to just 66 observations.
 - `tidy_data` is the data after performing dcast on the `melted_data` so that we can get the mean of the related activities across the different variables of the data.
 - `col_names_vector` is the data vector containing new formatted column headers where we replace mean() with Mean, std() with Std, and BodyBody with Body
 - `tidy_data` will then be "crowned" with the new column headers from `col_names_vector`