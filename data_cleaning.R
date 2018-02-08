# ANDREA TONDELLA
# IE MBD - Class O2
# Oct. 2017 Intake

# Machine Learning 2 - Individual Assignment
# Kaggle House Prices

source("lib_loading.R")

### ---------- Data Reading ----------

training_data = read.csv("train.csv")
test_data = read.csv("test.csv")

### ---------- ID Column ----------

# Check for duplicates ID
length(unique(training_data$Id)) == nrow(training_data)

# Removing the ID column
training_data = training_data[ , -which(names(training_data) %in% c("Id"))]

### ---------- Dealing With NAs ----------

# Setting a threshold for the maximum number of NAs allowed in a column. Columns with more NAs than the threshold will be discarded
na.thres <- 0.10
na.cols.above <- which(colSums(is.na(training_data)) > (nrow(training_data)*na.thres))
sort(colSums(sapply(training_data[na.cols.above], is.na)), decreasing = TRUE)
paste('There are', length(na.cols.above), 'columns with the number of missing values above the threshold of', (na.thres*100), '%')

# Dropping the columns above the threshold
na.cols.drop <- names(na.cols.above)

training_data <- training_data[ ,!(names(training_data) %in% na.cols.drop)]
test_data <- test_data[ ,!(names(test_data) %in% na.cols.drop)]

# Counting columns with null values
na.cols <-which(colSums(is.na(training_data)) > 0)
sort(colSums(sapply(training_data[na.cols], is.na)), decreasing = TRUE)
paste('There are', length(na.cols), 'more columns with missing values')
