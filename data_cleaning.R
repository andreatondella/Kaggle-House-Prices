# ANDREA TONDELLA
# IE MBD - Class O2
# Oct. 2017 Intake

# Machine Learning 2 - Individual Assignment
# Kaggle House Prices

source("lib_loading.R")

### ---------- Data Reading ----------

raw_training_data = read.csv("train.csv")
raw_test_data = read.csv("test.csv")

raw_test_data$SalePrice <- 0

all_data <- rbind(raw_training_data, raw_test_data)

# ### ---------- ID Column ----------
# 
# # Check for duplicates ID
# length(unique(training_data$Id)) == nrow(training_data)
# 
# # Removing the ID column
# training_data = training_data[ , -which(names(training_data) %in% c("Id"))]


### ---------- Dealing With NAs ----------

# Setting a threshold for the maximum number of NAs allowed in a column. Columns with more NAs than the threshold will be discarded
na.thres <- 0.10
na.cols.above <- which(colSums(is.na(raw_training_data)) > (nrow(raw_training_data)*na.thres))
sort(colSums(sapply(raw_training_data[na.cols.above], is.na)), decreasing = TRUE)
paste('There are', length(na.cols.above), 'columns with the number of missing values above the threshold of', (na.thres*100), '%')

# Dropping the columns above the threshold
na.cols.drop <- names(na.cols.above)

all_data <- all_data[ ,!(names(all_data) %in% na.cols.drop)]
raw_training_data <- raw_training_data[ ,!(names(raw_training_data) %in% na.cols.drop)]
raw_test_data <- raw_test_data[ ,!(names(raw_test_data) %in% na.cols.drop)]

# Counting columns with null values
na.cols <-which(colSums(is.na(raw_training_data)) > 0)
sort(colSums(sapply(raw_training_data[na.cols], is.na)), decreasing = TRUE)
paste('There are', length(na.cols), 'more columns with missing values')

# Dealing with the remaining NAs

# BsmtQual etc : data description says NA for basement features is "no basement"
# Train data
all_data$BsmtQual = factor(all_data$BsmtQual, levels=c(levels(all_data$BsmtQual), "No"))
all_data$BsmtQual[is.na(all_data$BsmtQual)] = "No"

all_data$BsmtCond = factor(all_data$BsmtCond, levels=c(levels(all_data$BsmtCond), "No"))
all_data$BsmtCond[is.na(all_data$BsmtCond)] = "No"

all_data$BsmtExposure = factor(all_data$BsmtExposure, levels=c(levels(all_data$BsmtExposure), "NoBsm"))
all_data$BsmtExposure[is.na(all_data$BsmtExposure)] = "NoBsm"

all_data$BsmtFinType1 = factor(all_data$BsmtFinType1, levels=c(levels(all_data$BsmtFinType1), "No"))
all_data$BsmtFinType1[is.na(all_data$BsmtFinType1)] = "No"

all_data$BsmtFinType2 = factor(all_data$BsmtFinType2, levels=c(levels(all_data$BsmtFinType2), "No"))
all_data$BsmtFinType2[is.na(all_data$BsmtFinType2)] = "No"

# # Test Data
# raw_test_data$BsmtQual = factor(raw_test_data$BsmtQual, levels=c(levels(raw_test_data$BsmtQual), "No"))
# raw_test_data$BsmtQual[is.na(raw_test_data$BsmtQual)] = "No"
# 
# raw_test_data$BsmtCond = factor(raw_test_data$BsmtCond, levels=c(levels(raw_test_data$BsmtCond), "No"))
# raw_test_data$BsmtCond[is.na(raw_test_data$BsmtCond)] = "No"
# 
# raw_test_data$BsmtExposure = factor(raw_test_data$BsmtExposure, levels=c(levels(raw_test_data$BsmtExposure), "NoBsm"))
# raw_test_data$BsmtExposure[is.na(raw_test_data$BsmtExposure)] = "NoBsm"
# 
# raw_test_data$BsmtFinType1 = factor(raw_test_data$BsmtFinType1, levels=c(levels(raw_test_data$BsmtFinType1), "No"))
# raw_test_data$BsmtFinType1[is.na(raw_test_data$BsmtFinType1)] = "No"
# 
# raw_test_data$BsmtFinType2 = factor(raw_test_data$BsmtFinType2, levels=c(levels(raw_test_data$BsmtFinType2), "No"))
# raw_test_data$BsmtFinType2[is.na(raw_test_data$BsmtFinType2)] = "No"

# GarageType etc : data description says NA for garage features is "no garage"
# Train data
all_data$GarageType = factor(all_data$GarageType, levels=c(levels(all_data$GarageType), "No"))
all_data$GarageType[is.na(all_data$GarageType)] = "No"

all_data$GarageFinish = factor(all_data$GarageFinish, levels=c(levels(all_data$GarageFinish), "No"))
all_data$GarageFinish[is.na(all_data$GarageFinish)] = "No"

all_data$GarageQual = factor(all_data$GarageQual, levels=c(levels(all_data$GarageQual), "No"))
all_data$GarageQual[is.na(all_data$GarageQual)] = "No"

all_data$GarageCond = factor(all_data$GarageCond, levels=c(levels(all_data$GarageCond), "No"))
all_data$GarageCond[is.na(all_data$GarageCond)] = "No"

# # Test Data
# raw_test_data$GarageType = factor(raw_test_data$GarageType, levels=c(levels(raw_test_data$GarageType), "No"))
# raw_test_data$GarageType[is.na(raw_test_data$GarageType)] = "No"
# 
# raw_test_data$GarageFinish = factor(raw_test_data$GarageFinish, levels=c(levels(raw_test_data$GarageFinish), "No"))
# raw_test_data$GarageFinish[is.na(raw_test_data$GarageFinish)] = "No"
# 
# raw_test_data$GarageQual = factor(raw_test_data$GarageQual, levels=c(levels(raw_test_data$GarageQual), "No"))
# raw_test_data$GarageQual[is.na(raw_test_data$GarageQual)] = "No"
# 
# raw_test_data$GarageCond = factor(raw_test_data$GarageCond, levels=c(levels(raw_test_data$GarageCond), "No"))
# raw_test_data$GarageCond[is.na(raw_test_data$GarageCond)] = "No"

# MasVnrType : NA most likely means no veneer
# Train Data
all_data$MasVnrType[is.na(all_data$MasVnrType)] = "None"
all_data$MasVnrArea[is.na(all_data$MasVnrArea)] <- 0

# # Test Data
# raw_test_data$MasVnrType[is.na(raw_test_data$MasVnrType)] = "None"
# raw_test_data$MasVnrArea[is.na(raw_test_data$MasVnrArea)] <- 0

# Electrical : NA means "UNK"
# Train Data
all_data$Electrical = factor(all_data$Electrical, levels=c(levels(all_data$Electrical), "UNK"))
all_data$Electrical[is.na(all_data$Electrical)] = "UNK"

# # Test Data
# raw_test_data$Electrical = factor(raw_test_data$Electrical, levels=c(levels(raw_test_data$Electrical), "UNK"))
# raw_test_data$Electrical[is.na(raw_test_data$Electrical)] = "UNK"

# GarageYrBlt: It seems reasonable that most houses would build a garage when the house itself was built.
# Train Data
idx <- which(is.na(all_data$GarageYrBlt))
all_data[idx, 'GarageYrBlt'] <- all_data[idx, 'YearBuilt']

# # Test Data
# idx <- which(is.na(raw_test_data$GarageYrBlt))
# raw_test_data[idx, 'GarageYrBlt'] <- raw_test_data[idx, 'YearBuilt']

# Counting columns with null values (in the training portion of the dataset)
na.cols.after <-which(colSums(is.na(all_data[c(1:nrow(raw_training_data)), ])) > 0)
paste('There are now', length(na.cols.after), 'columns with missing values')



### ---------- Factorizing the desired features ----------

# MS SubClass
all_data$MSSubClass <- as.factor(all_data$MSSubClass)
# raw_test_data$MSSubClass <- as.factor(raw_test_data$MSSubClass)

# Mont Sold
all_data$MoSold <- as.factor(all_data$MoSold)
# raw_test_data$MoSold <- as.factor(raw_test_data$MoSold)

# Overall Quality
all_data$OverallQual <- ordered(as.factor(all_data$OverallQual), levels = c(1:10))

# Overall Condition
all_data$OverallCond <- ordered(as.factor(all_data$OverallCond), levels = c(1:10))

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


levels(all_data$LotShape)
all_data$LotShape <- ordered(as.factor(recode(all_data$LotShape, "Reg" = 3, "IR1" = 3, "IR2" = 2, "IR3" = 1)), levels = c(1, 2, 3))
# raw_test_data$LotShape <- recode(raw_test_data$LotShape, "Reg" = 3, "IR1" = 3, "IR2" = 2, "IR3" = 1)

levels(all_data$Utilities)
all_data$Utilities <- ordered(as.factor(recode(all_data$Utilities, "AllPub" = 3, "NoSewr" = 2, "NoSeWa" = 1, "ELO" = 1)), levels = c(1, 2, 3))
# raw_test_data$Utilities <- recode(raw_test_data$Utilities, "AllPub" = 3, "NoSewr" = 2, "NoSeWa" = 1, "ELO" = 1)

levels(all_data$LandSlope)
all_data$LandSlope <- ordered(as.factor(recode(all_data$LandSlope, "Gtl" = 3, "Mod" = 2, "Sev" = 1)), levels = c(1, 2, 3))
# raw_test_data$LandSlope <- recode(raw_test_data$LandSlope, "Gtl" = 3, "Mod" = 2, "Sev" = 1)

levels(all_data$ExterQual)
all_data$ExterQual <- ordered(as.factor(recode(all_data$ExterQual, "Ex" = 3, "Gd" = 3, "TA" = 2, "Fa" = 1, "Po" = 1)), levels = c(1, 2, 3))
# raw_test_data$ExterQual <- recode(raw_test_data$ExterQual, "Ex" = 3, "Gd" = 3, "TA" = 2, "Fa" = 1, "Po" = 1)

levels(all_data$ExterCond)
all_data$ExterCond <- ordered(as.factor(recode(all_data$ExterCond, "Ex" = 3, "Gd" = 3, "TA" = 2, "Fa" = 1, "Po" = 1)), levels = c(1, 2, 3))
# raw_test_data$ExterCond <- recode(raw_test_data$ExterCond, "Ex" = 3, "Gd" = 3, "TA" = 2, "Fa" = 1, "Po" = 1)

levels(all_data$BsmtQual)
all_data$BsmtQual <- ordered(as.factor(recode(all_data$BsmtQual, "Ex" = 3, "Gd" = 3, "TA" = 2, "Fa" = 2, "Po" = 1, "No" = 1)), levels = c(1, 2, 3))
# raw_test_data$BsmtQual <- recode(raw_test_data$BsmtQual, "Ex" = 3, "Gd" = 3, "TA" = 2, "Fa" = 2, "Po" = 1, "No" = 1)

levels(all_data$BsmtCond)
all_data$BsmtCond <- ordered(as.factor(recode(all_data$BsmtCond, "Ex" = 3, "Gd" = 3, "TA" = 2, "Fa" = 2, "Po" = 1, "No" = 1)), levels = c(1, 2, 3))
# raw_test_data$BsmtCond <- recode(raw_test_data$BsmtCond, "Ex" = 3, "Gd" = 3, "TA" = 2, "Fa" = 2, "Po" = 1, "No" = 1)

levels(all_data$BsmtExposure)
all_data$BsmtExposure <- ordered(as.factor(recode(all_data$BsmtExposure, "Gd" = 3, "Av" = 3, "Mn" = 2, "No" = 1, "NoBsm" = 1)), levels = c(1, 2, 3))
# raw_test_data$BsmtExposure <- recode(raw_test_data$BsmtExposure, "Gd" = 3, "Av" = 3, "Mn" = 2, "No" = 1, "NoBsm" = 1)

levels(all_data$BsmtFinType1)
all_data$BsmtFinType1 <- ordered(as.factor(recode(all_data$BsmtFinType1, "GLQ" = 3, "ALQ" = 3, "BLQ" = 2, "Rec" = 2, "LwQ" = 2, "Unf" = 1, "No" = 1)), levels = c(1, 2, 3))
# raw_test_data$BsmtFinType1 <- recode(raw_test_data$BsmtFinType1, "GLQ" = 3, "ALQ" = 3, "BLQ" = 2, "Rec" = 2, "LwQ" = 2, "Unf" = 1, "No" = 1)

levels(all_data$BsmtFinType2)
all_data$BsmtFinType2 <- ordered(as.factor(recode(all_data$BsmtFinType2, "GLQ" = 3, "ALQ" = 3, "BLQ" = 2, "Rec" = 2, "LwQ" = 2, "Unf" = 1, "No" = 1)), levels = c(1, 2, 3))
# raw_test_data$BsmtFinType2 <- recode(raw_test_data$BsmtFinType2, "GLQ" = 3, "ALQ" = 3, "BLQ" = 2, "Rec" = 2, "LwQ" = 2, "Unf" = 1, "No" = 1)

levels(all_data$HeatingQC)
all_data$HeatingQC <- ordered(as.factor(recode(all_data$HeatingQC, "Ex" = 3, "Gd" = 3, "TA" = 2, "Fa" = 2, "Po" = 1)), levels = c(1, 2, 3))
# raw_test_data$HeatingQC <- recode(raw_test_data$HeatingQC, "Ex" = 3, "Gd" = 3, "TA" = 2, "Fa" = 2, "Po" = 1)

levels(all_data$Electrical)
all_data$Electrical <- ordered(as.factor(recode(all_data$Electrical, "SBrkr" = 3, "FuseA" = 2, "Mix" = 2,"FuseF" = 1, "FuseP" = 1, "UNK" = 2)), levels = c(1, 2, 3))
# raw_test_data$Electrical <- recode(raw_test_data$Electrical, "SBrkr" = 3, "FuseA" = 2, "Mix" = 2,"FuseF" = 1, "FuseP" = 1, "UNK" = 2)

levels(all_data$KitchenQual)
all_data$KitchenQual <- ordered(as.factor(recode(all_data$KitchenQual, "Ex" = 3, "Gd" = 3, "TA" = 2, "Fa" = 1, "Po" = 1)), levels = c(1, 2, 3))
# raw_test_data$KitchenQua <- recode(raw_test_data$KitchenQual, "Ex" = 3, "Gd" = 3, "TA" = 2, "Fa" = 1, "Po" = 1)

levels(all_data$Functional)
all_data$Functional <- ordered(as.factor(recode(all_data$Functional, "Typ" = 3, "Min1" = 3, "Min2" = 2, "Mod" = 2, "Maj1" = 2, "Maj2" = 1, "Sev" = 1, "Sal" = 1)), levels = c(1, 2, 3))
# raw_test_data$Functional <- recode(raw_test_data$Functional, "Typ" = 3, "Min1" = 3, "Min2" = 2, "Mod" = 2, "Maj1" = 2, "Maj2" = 1, "Sev" = 1, "Sal" = 1)

levels(all_data$GarageFinish)
all_data$GarageFinish <- ordered(as.factor(recode(all_data$GarageFinish, "Fin" = 3, "RFn" = 2, "Unf" = 1, "No" = 1)), levels = c(1, 2, 3))
# raw_test_data$GarageFinish <- recode(raw_test_data$GarageFinish, "Fin" = 3, "RFn" = 2, "Unf" = 1, "No" = 1)

levels(all_data$GarageQual)
all_data$GarageQual <- ordered(as.factor(recode(all_data$GarageQual, "Ex" = 3, "Gd" = 3, "TA" = 2, "Fa" = 2, "Po" = 1, "No" = 1)), levels = c(1, 2, 3))
# raw_test_data$GarageQual <- recode(raw_test_data$GarageQual, "Ex" = 3, "Gd" = 3, "TA" = 2, "Fa" = 2, "Po" = 1, "No" = 1)

levels(all_data$GarageCond)
all_data$GarageCond <- ordered(as.factor(recode(all_data$GarageCond, "Ex" = 3, "Gd" = 3, "TA" = 2, "Fa" = 2, "Po" = 1, "No" = 1)), levels = c(1, 2, 3))
# raw_test_data$GarageCond <- recode(raw_test_data$GarageCond, "Ex" = 3, "Gd" = 3, "TA" = 2, "Fa" = 2, "Po" = 1, "No" = 1)

levels(all_data$PavedDrive)
all_data$PavedDrive <- ordered(as.factor(recode(all_data$PavedDrive, "Y" = 3, "P" = 2, "N" = 1)), levels = c(1, 2, 3))
# raw_test_data$PavedDrive <- recode(raw_test_data$PavedDrive, "Y" = 3, "P" = 2, "N" = 1)



### ---------- Outliers ----------

# Plotting SalePrice and Log(SalePrice)
par(mfrow = c(1,2))
boxplot(all_data[c(1:nrow(raw_training_data)), "SalePrice"], main = "Sale Price")
boxplot(log(all_data[c(1:nrow(raw_training_data)), "SalePrice"]), main = "log(Sale Price)")
# There are many outliers, but removing them all might bias the prediction for very cheap and very expensive houses, 
# since the total number of observations is relatively small

# Let's fit a linear model with all the variables and look if the residuals are more helpful in identifying true outliers:

# Fitting a linear model with all the variables
lm.outlier = lm(SalePrice ~ ., data = all_data[c(1:nrow(raw_training_data)), ])


# Plotting residuals to identify outliers
par(mfrow = c(2,2))
plot(lm.outlier)


# Looking at the cook distance, observations 826 and 524 have a clear high influence on the model, let's drop these two observations:
all_data <- all_data[-c(826, 524), ]

### ---------- Removing Utilities ----------
all_data <- all_data[,-which(names(all_data) == "Utilities")]

