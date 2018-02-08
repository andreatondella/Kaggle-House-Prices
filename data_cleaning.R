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

training_data$BsmtExposure = factor(training_data$BsmtExposure, levels=c(levels(training_data$BsmtExposure), "NoBsm"))
training_data$BsmtExposure[is.na(training_data$BsmtExposure)] = "NoBsm"

training_data$BsmtFinType1 = factor(training_data$BsmtFinType1, levels=c(levels(training_data$BsmtFinType1), "No"))
training_data$BsmtFinType1[is.na(training_data$BsmtFinType1)] = "No"

training_data$BsmtFinType2 = factor(training_data$BsmtFinType2, levels=c(levels(training_data$BsmtFinType2), "No"))
training_data$BsmtFinType2[is.na(training_data$BsmtFinType2)] = "No"

# Test Data
test_data$BsmtQual = factor(test_data$BsmtQual, levels=c(levels(test_data$BsmtQual), "No"))
test_data$BsmtQual[is.na(test_data$BsmtQual)] = "No"

test_data$BsmtCond = factor(test_data$BsmtCond, levels=c(levels(test_data$BsmtCond), "No"))
test_data$BsmtCond[is.na(test_data$BsmtCond)] = "No"

test_data$BsmtExposure = factor(test_data$BsmtExposure, levels=c(levels(test_data$BsmtExposure), "NoBsm"))
test_data$BsmtExposure[is.na(test_data$BsmtExposure)] = "NoBsm"

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

# MasVnrType : NA most likely means no veneer
# Train Data
training_data$MasVnrType[is.na(training_data$MasVnrType)] = "None"
training_data$MasVnrArea[is.na(training_data$MasVnrArea)] <- 0

# Test Data
test_data$MasVnrType[is.na(test_data$MasVnrType)] = "None"
test_data$MasVnrArea[is.na(test_data$MasVnrArea)] <- 0

# Electrical : NA means "UNK"
# Train Data
training_data$Electrical = factor(training_data$Electrical, levels=c(levels(training_data$Electrical), "UNK"))
training_data$Electrical[is.na(training_data$Electrical)] = "UNK"

# Test Data
test_data$Electrical = factor(test_data$Electrical, levels=c(levels(test_data$Electrical), "UNK"))
test_data$Electrical[is.na(test_data$Electrical)] = "UNK"

# GarageYrBlt: It seems reasonable that most houses would build a garage when the house itself was built.
# Train Data
idx <- which(is.na(training_data$GarageYrBlt))
training_data[idx, 'GarageYrBlt'] <- training_data[idx, 'YearBuilt']

# Test Data
idx <- which(is.na(test_data$GarageYrBlt))
test_data[idx, 'GarageYrBlt'] <- test_data[idx, 'YearBuilt']

# Counting columns with null values
na.cols.after <-which(colSums(is.na(training_data)) > 0)
paste('There are now', length(na.cols.after), 'columns with missing values')






### ---------- Factorizing the desired features ----------

# MS SubClass
training_data$MSSubClass <- as.factor(training_data$MSSubClass)
test_data$MSSubClass <- as.factor(test_data$MSSubClass)

# Mont Sold
training_data$MoSold <- as.factor(training_data$MoSold)
test_data$MoSold <- as.factor(test_data$MoSold)


### ---------- Dealing With Ordinal Features ----------

# List of ordinal variables:

# Lot Shape: Reg - IR1 - IR2 - IR3
# Utilities: AllPub - NoSewr - NoSeWa - ELO
# Land Slope: Gtl - Mod - Sev
# Exter Qual: Ex - Gd - TA - Fa - Po
# Exter Cond: Ex - Gd - TA - Fa - Po
# Bsm Qual: Ex - Gd - TA - Fa - Po - No
# Bsm Cond: Ex - Gd - TA - Fa - Po - No
# Bsm Exp: Gd - Av - Mn - No - NoBsm
# Bsm Fin Type 1: GLQ - ALQ - BLQ - Rec - Lwq - Unf - No
# Bsm Fin Type 2: GLQ - ALQ - BLQ - Rec - Lwq - Unf - No
# HeatingQC: Ex - Gd - TA - Fa - Po
# Electrical: SBrkr - FuseA - FuseF - FuseP - Mix
# KitchenQual: Ex - Gd - TA - Fa - Po
# Functional: Typ - Min1 - Min2 - Mod - Maj1 - Maj2 - Sev - Sal
# Garage Finish: Fin, RFn, Unf, No
# Garage Qual: Ex - Gd - TA - Fa - Po - No
# Garage Cond: Ex -Gd - TA - Fa - Po - No
# Paved Drive: Y - P - N


levels(training_data$LotShape)
training_data$LotShape <- recode(training_data$LotShape, "Reg" = 3, "IR1" = 3, "IR2" = 2, "IR3" = 1)
test_data$LotShape <- recode(test_data$LotShape, "Reg" = 3, "IR1" = 3, "IR2" = 2, "IR3" = 1)

levels(training_data$Utilities)
training_data$Utilities <- recode(training_data$Utilities, "AllPub" = 3, "NoSewr" = 2, "NoSeWa" = 1, "ELO" = 1)
test_data$Utilities <- recode(test_data$Utilities, "AllPub" = 3, "NoSewr" = 2, "NoSeWa" = 1, "ELO" = 1)

levels(training_data$LandSlope)
training_data$LandSlope <- recode(training_data$LandSlope, "Gtl" = 3, "Mod" = 2, "Sev" = 1)
test_data$LandSlope <- recode(test_data$LandSlope, "Gtl" = 3, "Mod" = 2, "Sev" = 1)

levels(training_data$ExterQual)
training_data$ExterQual <- recode(training_data$ExterQual, "Ex" = 3, "Gd" = 3, "TA" = 2, "Fa" = 1, "Po" = 1)
test_data$ExterQual <- recode(test_data$ExterQual, "Ex" = 3, "Gd" = 3, "TA" = 2, "Fa" = 1, "Po" = 1)

levels(training_data$ExterCond)
training_data$ExterCond <- recode(training_data$ExterCond, "Ex" = 3, "Gd" = 3, "TA" = 2, "Fa" = 1, "Po" = 1)
test_data$ExterCond <- recode(test_data$ExterCond, "Ex" = 3, "Gd" = 3, "TA" = 2, "Fa" = 1, "Po" = 1)

levels(training_data$BsmtQual)
training_data$BsmtQual <- recode(training_data$BsmtQual, "Ex" = 3, "Gd" = 3, "TA" = 2, "Fa" = 2, "Po" = 1, "No" = 1)
test_data$BsmtQual <- recode(test_data$BsmtQual, "Ex" = 3, "Gd" = 3, "TA" = 2, "Fa" = 2, "Po" = 1, "No" = 1)

levels(training_data$BsmtCond)
training_data$BsmtCond <- recode(training_data$BsmtCond, "Ex" = 3, "Gd" = 3, "TA" = 2, "Fa" = 2, "Po" = 1, "No" = 1)
test_data$BsmtCond <- recode(test_data$BsmtCond, "Ex" = 3, "Gd" = 3, "TA" = 2, "Fa" = 2, "Po" = 1, "No" = 1)

levels(training_data$BsmtExposure)
training_data$BsmtExposure <- recode(training_data$BsmtExposure, "Gd" = 3, "Av" = 3, "Mn" = 2, "No" = 1, "NoBsm" = 1)
test_data$BsmtExposure <- recode(test_data$BsmtExposure, "Gd" = 3, "Av" = 3, "Mn" = 2, "No" = 1, "NoBsm" = 1)

levels(training_data$BsmtFinType1)
training_data$BsmtFinType1 <- recode(training_data$BsmtFinType1, "GLQ" = 3, "ALQ" = 3, "BLQ" = 2, "Rec" = 2, "Lwq" = 2, "Unf" = 1, "No" = 1)
test_data$BsmtFinType1 <- recode(test_data$BsmtFinType1, "GLQ" = 3, "ALQ" = 3, "BLQ" = 2, "Rec" = 2, "Lwq" = 2, "Unf" = 1, "No" = 1)

levels(training_data$BsmtFinType2)
training_data$BsmtFinType2 <- recode(training_data$BsmtFinType2, "GLQ" = 3, "ALQ" = 3, "BLQ" = 2, "Rec" = 2, "Lwq" = 2, "Unf" = 1, "No" = 1)
test_data$BsmtFinType2 <- recode(test_data$BsmtFinType2, "GLQ" = 3, "ALQ" = 3, "BLQ" = 2, "Rec" = 2, "Lwq" = 2, "Unf" = 1, "No" = 1)

levels(training_data$HeatingQC)
training_data$HeatingQC <- recode(training_data$HeatingQC, "Ex" = 3, "Gd" = 3, "TA" = 2, "Fa" = 2, "Po" = 1)
test_data$HeatingQC <- recode(test_data$HeatingQC, "Ex" = 3, "Gd" = 3, "TA" = 2, "Fa" = 2, "Po" = 1)

levels(training_data$Electrical)
training_data$Electrical <- recode(training_data$Electrical, "SBrkr" = 3, "FuseA" = 2, "Mix" = 2,"FuseF" = 1, "FuseP" = 1)
test_data$Electrical <- recode(test_data$Electrical, "SBrkr" = 3, "FuseA" = 2, "Mix" = 2,"FuseF" = 1, "FuseP" = 1)

levels(training_data$KitchenQual)
training_data$KitchenQual <- recode(training_data$KitchenQual, "Ex" = 3, "Gd" = 3, "TA" = 2, "Fa" = 1, "Po" = 1)
test_data$KitchenQua <- recode(test_data$KitchenQual, "Ex" = 3, "Gd" = 3, "TA" = 2, "Fa" = 1, "Po" = 1)

levels(training_data$Functional)
training_data$Functional <- recode(training_data$Functional, "Typ" = 3, "Min1" = 3, "Min2" = 2, "Mod" = 2, "Maj1" = 2, "Maj2" = 1, "Sev" = 1, "Sal" = 1)
test_data$Functional <- recode(test_data$Functional, "Typ" = 3, "Min1" = 3, "Min2" = 2, "Mod" = 2, "Maj1" = 2, "Maj2" = 1, "Sev" = 1, "Sal" = 1)

levels(training_data$GarageFinish)
training_data$GarageFinish <- recode(training_data$GarageFinish, "Fin" = 3, "RFn" = 2, "Unf" = 1, "No" = 1)
test_data$GarageFinish <- recode(test_data$GarageFinish, "Fin" = 3, "RFn" = 2, "Unf" = 1, "No" = 1)

levels(training_data$GarageQual)
training_data$GarageQual <- recode(training_data$GarageQual, "Ex" = 3, "Gd" = 3, "TA" = 2, "Fa" = 2, "Po" = 1, "No" = 1)
test_data$GarageQual <- recode(test_data$GarageQual, "Ex" = 3, "Gd" = 3, "TA" = 2, "Fa" = 2, "Po" = 1, "No" = 1)

levels(training_data$GarageCond)
training_data$GarageCond <- recode(training_data$GarageCond, "Ex" = 3, "Gd" = 3, "TA" = 2, "Fa" = 2, "Po" = 1, "No" = 1)
test_data$GarageCond <- recode(test_data$GarageCond, "Ex" = 3, "Gd" = 3, "TA" = 2, "Fa" = 2, "Po" = 1, "No" = 1)

levels(training_data$PavedDrive)
training_data$PavedDrive <- recode(training_data$PavedDrive, "Y" = 3, "P" = 2, "N" = 1)
test_data$PavedDrive <- recode(test_data$PavedDrive, "Y" = 3, "P" = 2, "N" = 1)

