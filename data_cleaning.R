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
# na.thres <- 0.10
# na.cols.above <- which(colSums(is.na(raw_training_data)) > (nrow(raw_training_data)*na.thres))
# sort(colSums(sapply(raw_training_data[na.cols.above], is.na)), decreasing = TRUE)
# paste('There are', length(na.cols.above), 'columns with the number of missing values above the threshold of', (na.thres*100), '%')
# 
# # Dropping the columns above the threshold
# na.cols.drop <- names(na.cols.above)
# 
# all_data <- all_data[ ,!(names(all_data) %in% na.cols.drop)]
# raw_training_data <- raw_training_data[ ,!(names(raw_training_data) %in% na.cols.drop)]
# raw_test_data <- raw_test_data[ ,!(names(raw_test_data) %in% na.cols.drop)]

# Counting columns with null values
na.cols <-which(colSums(is.na(all_data)) > 0)
sort(colSums(sapply(all_data[na.cols], is.na)), decreasing = TRUE)
paste('There are', length(na.cols), 'columns with missing values')


# PoolQC: data description says NA means "No Pool"
all_data$PoolQC = factor(all_data$PoolQC, levels = c(levels(all_data$PoolQC), "No"))
all_data$PoolQC[is.na(all_data$PoolQC)] = "No"

# MiscFeature: data description says NA means "No misc features"
all_data$MiscFeature = factor(all_data$MiscFeature, levels = c(levels(all_data$MiscFeature), "No"))
all_data$MiscFeature[is.na(all_data$MiscFeature)] = "No"

# Fence: data description says NA means "No fence"
all_data$Fence = factor(all_data$Fence, levels = c(levels(all_data$Fence), "No"))
all_data$Fence[is.na(all_data$Fence)] = "No"

# Alley: data description says NA means "No alley access"
all_data$Alley = factor(all_data$Alley, levels = c(levels(all_data$Alley), "No"))
all_data$Alley[is.na(all_data$Alley)] = "No"

# FireplaceQu: data description says NA means "No fireplace"
all_data$FireplaceQu = factor(all_data$FireplaceQu, levels = c(levels(all_data$FireplaceQu), "No"))
all_data$FireplaceQu[is.na(all_data$FireplaceQu)] = "No"

# Lot Frontage: missing values are derived by a linear model that accounts for all the features that affect the lot frontage of a house
plot(log(all_data$LotArea), log(all_data$LotFrontage), col= all_data$Neighborhood)
Lot_regression <- lm(log(all_data$LotFrontage) ~ log(all_data$LotArea) + all_data$LotConfig + all_data$LotShape + all_data$Neighborhood)

LotFrontage_pred <- exp(predict(Lot_regression, newdata = all_data))

for (i in c(1:nrow(all_data))){
  if (is.na(all_data$LotFrontage[i]) == TRUE){
    all_data$LotFrontage[i] <- LotFrontage_pred[i]
  }
}

plot(log(all_data$LotArea), log(all_data$LotFrontage), col= all_data$Neighborhood)
plot(all_data$LotArea, all_data$LotFrontage, col= all_data$Neighborhood)

# MSZoning 
all_data <- data.table(all_data)
summary(all_data$MSZoning)

all_data[all_data$MSZoning == "C (all)", ]
all_data[is.na(all_data$MSZoning), c("MSZoning", "Neighborhood", "LotArea", "GrLivArea")]
all_data[, median(GrLivArea), by = MSZoning]
ggplot(all_data, aes(x = reorder(MSZoning, SalePrice, FUN = mean), y = SalePrice)) + geom_boxplot() 

table(all_data[, Neighborhood, by = MSZoning])


# The majority of the commercial properties are locater in IDOTRR and the commercial buldings have the lowest median living area.
# We can conclude that the two NAs located in IDOTRR with low living area are commercial while the third is residential medium density. 
# The fourth Na is located in Mitchel and therefore is very likely to be residential low density.

all_data[is.na(MSZoning) & Neighborhood == "IDOTRR" & GrLivArea < 1000, MSZoning := "C (all)"] # Commercial properties in IDOTRR
all_data[is.na(MSZoning) & Neighborhood == "IDOTRR" & GrLivArea > 1000, MSZoning := "RM"] # Residential property in IDOTRR
all_data[is.na(MSZoning) & Neighborhood == "Mitchel", MSZoning := "RL"] # Residential in Mitchel

all_data <- data.frame(all_data)
# Utilities: there is one single level for utilities in the test set, therefore the whole column can be dropped:
all_data <- all_data[,-which(names(all_data) == "Utilities")]

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

all_data$BsmtFullBath[is.na(all_data$BsmtFullBath)] = 0

all_data$BsmtHalfBath[is.na(all_data$BsmtHalfBath)] = 0

all_data$BsmtFinSF1[is.na(all_data$BsmtFinSF1)] = 0

all_data$BsmtFinSF2[is.na(all_data$BsmtFinSF2)] = 0

all_data$BsmtUnfSF[is.na(all_data$BsmtUnfSF)] = 0

all_data$TotalBsmtSF[is.na(all_data$TotalBsmtSF)] = 0

# Functional: Home functionality (Assume typical unless deductions are warranted)
plot(all_data$Functional, all_data$SalePrice)
all_data$Functional[is.na(all_data$Functional)] = "Typ"



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

all_data$GarageCars[is.na(all_data$GarageCars)] = 0

all_data$GarageArea[is.na(all_data$GarageArea)] = 0

# Exterior Quality: looking at the average exterior quality of the neighborhood
all_data <- data.table(all_data)
summary(all_data$Exterior1st)

par(mfrow = c(2,1))


all_data[is.na(all_data$Exterior1st), c("Neighborhood", "MSZoning", "MSSubClass", "BldgType", "RoofMatl", "ExterQual", "YearBuilt", "LotArea", "YearRemodAdd")]

table(all_data[ Neighborhood == "Edwards", Exterior1st, by = ExterQual])
table(all_data[ Neighborhood == "Edwards", Exterior1st, by = RoofMatl])
table(all_data[ Neighborhood == "Edwards", Exterior1st, by = BldgType])

table(all_data[ Neighborhood == "Edwards" & YearBuilt < 1955 & YearBuilt > 1935, Exterior1st, by = ExterQual])
table(all_data[ Neighborhood == "Edwards" & LotArea > 15000, Exterior1st, by = ExterQual])
table(all_data[ Neighborhood == "Edwards" & YearRemodAdd > 2000, Exterior1st, by = ExterQual])


plot(all_data[ Neighborhood == "Edwards" & (Exterior1st == "VinylSd" | Exterior1st == "MetalSd" | Exterior1st == "Wd Sdng") & ExterQual == "TA", Exterior1st], all_data[ Neighborhood == "Edwards" & (Exterior1st == "VinylSd" | Exterior1st == "MetalSd" | Exterior1st == "Wd Sdng")  & ExterQual == "TA", SalePrice])

# It seems uncertain wether the exterior is made of Matel Wood or Vinyl, but Vinyl seems to be the most valid assumption

all_data$Exterior1st[is.na(all_data$Exterior1st)] = "VinylSd"
all_data$Exterior2nd[is.na(all_data$Exterior2nd)] = "VinylSd"
all_data <- data.frame(all_data)

# Kitchen Quality
all_data <- data.table(all_data)
summary(all_data$Exterior1st)

all_data[is.na(all_data$KitchenQual), c("Neighborhood", "MSZoning", "MSSubClass", "BldgType", "YearBuilt", "YearRemodAdd", "KitchenAbvGr")]

table(all_data[ Neighborhood == "ClearCr" & MSZoning == "RL", KitchenQual])
table(all_data[ Neighborhood == "ClearCr" & MSSubClass == 50, KitchenQual])
table(all_data[ Neighborhood == "ClearCr" & BldgType == "1Fam", KitchenQual])
table(all_data[ Neighborhood == "ClearCr" & MSZoning == "RL" & BldgType == "1Fam" & MSSubClass == 50, KitchenQual])

# It seems reasonable to assume that kitchen quality is TA (average)
all_data$KitchenQual[is.na(all_data$KitchenQual)] = "TA"

all_data <- data.frame(all_data)


# Sale Type
all_data <- data.table(all_data)
summary(all_data$SaleType)

all_data[is.na(all_data$SaleType), "SaleCondition"]
summary(all_data[SaleCondition == "Normal", SaleType])

# With no other variables influency the type of sale, it seems reasonable to assign WD to the NA sale type
all_data$SaleType[is.na(all_data$SaleType)] = "WD"

all_data <- data.frame(all_data)
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

# Electrical : Since there's only one missing raw, it can be replaced with SBrkr
# Train Data
all_data$Electrical[is.na(all_data$Electrical)] = "SBrkr"

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

na.cols <-which(colSums(is.na(all_data)) > 0)
paste('There are', length(na.cols), 'columns with missing values')



### ---------- Factorizing the desired features ----------

# MS SubClass
all_data$MSSubClass <- as.factor(all_data$MSSubClass)
# raw_test_data$MSSubClass <- as.factor(raw_test_data$MSSubClass)

# Mont Sold
all_data$MoSold <- as.factor(all_data$MoSold)
# raw_test_data$MoSold <- as.factor(raw_test_data$MoSold)

# # Overall Quality
# all_data$OverallQual <- ordered(as.factor(all_data$OverallQual), levels = c(1:10))
# 
# # Overall Condition
# all_data$OverallCond <- ordered(as.factor(all_data$OverallCond), levels = c(1:10))

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
all_data$LotShape <- recode(all_data$LotShape, "Reg" = 4, "IR1" = 3, "IR2" = 2, "IR3" = 1)
# raw_test_data$LotShape <- recode(raw_test_data$LotShape, "Reg" = 3, "IR1" = 3, "IR2" = 2, "IR3" = 1)

# levels(all_data$Utilities)
# all_data$Utilities <- ordered(as.factor(recode(all_data$Utilities, "AllPub" = 3, "NoSewr" = 2, "NoSeWa" = 1, "ELO" = 1)), levels = c(1, 2, 3))
# raw_test_data$Utilities <- recode(raw_test_data$Utilities, "AllPub" = 3, "NoSewr" = 2, "NoSeWa" = 1, "ELO" = 1)

levels(all_data$LandSlope)
all_data$LandSlope <- recode(all_data$LandSlope, "Gtl" = 3, "Mod" = 2, "Sev" = 1)
# raw_test_data$LandSlope <- recode(raw_test_data$LandSlope, "Gtl" = 3, "Mod" = 2, "Sev" = 1)

levels(all_data$ExterQual)
all_data$ExterQual <- recode(all_data$ExterQual, "Ex" = 5, "Gd" = 4, "TA" = 3, "Fa" = 2, "Po" = 1)
# raw_test_data$ExterQual <- recode(raw_test_data$ExterQual, "Ex" = 3, "Gd" = 3, "TA" = 2, "Fa" = 1, "Po" = 1)

levels(all_data$ExterCond)
all_data$ExterCond <- recode(all_data$ExterCond, "Ex" = 5, "Gd" = 4, "TA" = 3, "Fa" = 2, "Po" = 1)
# raw_test_data$ExterCond <- recode(raw_test_data$ExterCond, "Ex" = 3, "Gd" = 3, "TA" = 2, "Fa" = 1, "Po" = 1)

levels(all_data$BsmtQual)
all_data$BsmtQual <- recode(all_data$BsmtQual, "Ex" = 5, "Gd" = 4, "TA" = 3, "Fa" = 2, "Po" = 1, "No" = 0)
# raw_test_data$BsmtQual <- recode(raw_test_data$BsmtQual, "Ex" = 3, "Gd" = 3, "TA" = 2, "Fa" = 2, "Po" = 1, "No" = 1)

levels(all_data$BsmtCond)
all_data$BsmtCond <- recode(all_data$BsmtCond, "Ex" = 5, "Gd" = 4, "TA" = 3, "Fa" = 2, "Po" = 1, "No" = 0)
# raw_test_data$BsmtCond <- recode(raw_test_data$BsmtCond, "Ex" = 3, "Gd" = 3, "TA" = 2, "Fa" = 2, "Po" = 1, "No" = 1)

levels(all_data$BsmtExposure)
all_data$BsmtExposure <- recode(all_data$BsmtExposure, "Gd" = 4, "Av" = 3, "Mn" = 2, "No" = 1, "NoBsm" = 0)
# raw_test_data$BsmtExposure <- recode(raw_test_data$BsmtExposure, "Gd" = 3, "Av" = 3, "Mn" = 2, "No" = 1, "NoBsm" = 1)

levels(all_data$BsmtFinType1)
all_data$BsmtFinType1 <- recode(all_data$BsmtFinType1, "GLQ" = 6, "ALQ" = 5, "BLQ" = 4, "Rec" = 3, "LwQ" = 2, "Unf" = 1, "No" = 0)
# raw_test_data$BsmtFinType1 <- recode(raw_test_data$BsmtFinType1, "GLQ" = 3, "ALQ" = 3, "BLQ" = 2, "Rec" = 2, "LwQ" = 2, "Unf" = 1, "No" = 1)

levels(all_data$BsmtFinType2)
all_data$BsmtFinType2 <- recode(all_data$BsmtFinType2, "GLQ" = 6, "ALQ" = 5, "BLQ" = 4, "Rec" = 3, "LwQ" = 2, "Unf" = 1, "No" = 0)
# raw_test_data$BsmtFinType2 <- recode(raw_test_data$BsmtFinType2, "GLQ" = 3, "ALQ" = 3, "BLQ" = 2, "Rec" = 2, "LwQ" = 2, "Unf" = 1, "No" = 1)

levels(all_data$HeatingQC)
all_data$HeatingQC <- recode(all_data$HeatingQC, "Ex" = 5, "Gd" = 4, "TA" = 3, "Fa" = 2, "Po" = 1)
# raw_test_data$HeatingQC <- recode(raw_test_data$HeatingQC, "Ex" = 3, "Gd" = 3, "TA" = 2, "Fa" = 2, "Po" = 1)

levels(all_data$Electrical)
all_data$Electrical <- recode(all_data$Electrical, "SBrkr" = 5, "FuseA" = 4, "Mix" = 3,"FuseF" =2, "FuseP" = 1, "UNK" = 3)
# raw_test_data$Electrical <- recode(raw_test_data$Electrical, "SBrkr" = 3, "FuseA" = 2, "Mix" = 2,"FuseF" = 1, "FuseP" = 1, "UNK" = 2)

levels(all_data$KitchenQual)
all_data$KitchenQual <- recode(all_data$KitchenQual, "Ex" = 5, "Gd" = 4, "TA" = 3, "Fa" = 2, "Po" = 1)
# raw_test_data$KitchenQua <- recode(raw_test_data$KitchenQual, "Ex" = 3, "Gd" = 3, "TA" = 2, "Fa" = 1, "Po" = 1)

levels(all_data$Functional)
all_data$Functional <- recode(all_data$Functional, "Typ" = 8, "Min1" = 7, "Min2" = 6, "Mod" = 5, "Maj1" = 4, "Maj2" = 3, "Sev" = 2, "Sal" = 1)
# raw_test_data$Functional <- recode(raw_test_data$Functional, "Typ" = 3, "Min1" = 3, "Min2" = 2, "Mod" = 2, "Maj1" = 2, "Maj2" = 1, "Sev" = 1, "Sal" = 1)

levels(all_data$GarageFinish)
all_data$GarageFinish <- recode(all_data$GarageFinish, "Fin" = 3, "RFn" = 2, "Unf" = 1, "No" = 0)
# raw_test_data$GarageFinish <- recode(raw_test_data$GarageFinish, "Fin" = 3, "RFn" = 2, "Unf" = 1, "No" = 1)

levels(all_data$GarageQual)
all_data$GarageQual <- recode(all_data$GarageQual, "Ex" = 5, "Gd" = 4, "TA" = 3, "Fa" = 2, "Po" = 1, "No" = 0)
# raw_test_data$GarageQual <- recode(raw_test_data$GarageQual, "Ex" = 3, "Gd" = 3, "TA" = 2, "Fa" = 2, "Po" = 1, "No" = 1)

levels(all_data$GarageCond)
all_data$GarageCond <- recode(all_data$GarageCond, "Ex" = 5, "Gd" = 4, "TA" = 3, "Fa" = 2, "Po" = 1, "No" = 0)
# raw_test_data$GarageCond <- recode(raw_test_data$GarageCond, "Ex" = 3, "Gd" = 3, "TA" = 2, "Fa" = 2, "Po" = 1, "No" = 1)

levels(all_data$PavedDrive)
all_data$PavedDrive <- recode(all_data$PavedDrive, "Y" = 2, "P" = 1, "N" = 0)
# raw_test_data$PavedDrive <- recode(raw_test_data$PavedDrive, "Y" = 3, "P" = 2, "N" = 1)



# ### ---------- Outliers ----------
# 
# # Plotting SalePrice and Log(SalePrice)
# par(mfrow = c(1,2))
# boxplot(all_data[c(1:nrow(raw_training_data)), "SalePrice"], main = "Sale Price")
# boxplot(log(all_data[c(1:nrow(raw_training_data)), "SalePrice"]), main = "log(Sale Price)")
# # There are many outliers, but removing them all might bias the prediction for very cheap and very expensive houses, 
# # since the total number of observations is relatively small
# 
# # Let's fit a linear model with all the variables and look if the residuals are more helpful in identifying true outliers:
# 
# # Fitting a linear model with all the variables
# lm.outlier = lm(SalePrice ~ ., data = all_data[c(1:nrow(raw_training_data)), ])
# 
# 
# # Plotting residuals to identify outliers
# par(mfrow = c(2,2))
# plot(lm.outlier)
# 
# 
# # Looking at the cook distance, observations 826 and 524 have a clear high influence on the model, let's drop these two observations:
# all_data <- all_data[-c(826, 524, 1171, 1424), ]

### ---------- Removing Utilities ----------


