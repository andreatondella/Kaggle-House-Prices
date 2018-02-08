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

# Dealing with the remaining NAs

# BsmtQual etc : data description says NA for basement features is "no basement"
# Train data
training_data$BsmtQual = factor(training_data$BsmtQual, levels=c(levels(training_data$BsmtQual), "No"))
training_data$BsmtQual[is.na(training_data$BsmtQual)] = "No"

training_data$BsmtCond = factor(training_data$BsmtCond, levels=c(levels(training_data$BsmtCond), "No"))
training_data$BsmtCond[is.na(training_data$BsmtCond)] = "No"

training_data$BsmtExposure = factor(training_data$BsmtExposure, levels=c(levels(training_data$BsmtExposure), "No"))
training_data$BsmtExposure[is.na(training_data$BsmtExposure)] = "No"

training_data$BsmtFinType1 = factor(training_data$BsmtFinType1, levels=c(levels(training_data$BsmtFinType1), "No"))
training_data$BsmtFinType1[is.na(training_data$BsmtFinType1)] = "No"

training_data$BsmtFinType2 = factor(training_data$BsmtFinType2, levels=c(levels(training_data$BsmtFinType2), "No"))
training_data$BsmtFinType2[is.na(training_data$BsmtFinType2)] = "No"

# Test Data
test_data$BsmtQual = factor(test_data$BsmtQual, levels=c(levels(test_data$BsmtQual), "No"))
test_data$BsmtQual[is.na(test_data$BsmtQual)] = "No"

test_data$BsmtCond = factor(test_data$BsmtCond, levels=c(levels(test_data$BsmtCond), "No"))
test_data$BsmtCond[is.na(test_data$BsmtCond)] = "No"

test_data$BsmtExposure = factor(test_data$BsmtExposure, levels=c(levels(test_data$BsmtExposure), "No"))
test_data$BsmtExposure[is.na(test_data$BsmtExposure)] = "No"

test_data$BsmtFinType1 = factor(test_data$BsmtFinType1, levels=c(levels(test_data$BsmtFinType1), "No"))
test_data$BsmtFinType1[is.na(test_data$BsmtFinType1)] = "No"

test_data$BsmtFinType2 = factor(test_data$BsmtFinType2, levels=c(levels(test_data$BsmtFinType2), "No"))
test_data$BsmtFinType2[is.na(test_data$BsmtFinType2)] = "No"

# GarageType etc : data description says NA for garage features is "no garage"
# Train data
training_data$GarageType = factor(training_data$GarageType, levels=c(levels(training_data$GarageType), "No"))
training_data$GarageType[is.na(training_data$GarageType)] = "No"

training_data$GarageFinish = factor(training_data$GarageFinish, levels=c(levels(training_data$GarageFinish), "No"))
training_data$GarageFinish[is.na(training_data$GarageFinish)] = "No"

training_data$GarageQual = factor(training_data$GarageQual, levels=c(levels(training_data$GarageQual), "No"))
training_data$GarageQual[is.na(training_data$GarageQual)] = "No"

training_data$GarageCond = factor(training_data$GarageCond, levels=c(levels(training_data$GarageCond), "No"))
training_data$GarageCond[is.na(training_data$GarageCond)] = "No"

# Test Data
test_data$GarageType = factor(test_data$GarageType, levels=c(levels(test_data$GarageType), "No"))
test_data$GarageType[is.na(test_data$GarageType)] = "No"

test_data$GarageFinish = factor(test_data$GarageFinish, levels=c(levels(test_data$GarageFinish), "No"))
test_data$GarageFinish[is.na(test_data$GarageFinish)] = "No"

test_data$GarageQual = factor(test_data$GarageQual, levels=c(levels(test_data$GarageQual), "No"))
test_data$GarageQual[is.na(test_data$GarageQual)] = "No"

test_data$GarageCond = factor(test_data$GarageCond, levels=c(levels(test_data$GarageCond), "No"))
test_data$GarageCond[is.na(test_data$GarageCond)] = "No"





