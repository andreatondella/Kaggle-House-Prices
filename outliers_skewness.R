# ANDREA TONDELLA
# IE MBD - Class O2
# Oct. 2017 Intake

# Machine Learning 2 - Individual Assignment
# Kaggle House Prices

source("lib_loading.R")
source("data_cleaning.R")
source("feature_creation.R")

# ---------- Outliers ----------

ggplot(all_data[c(1:nrow(raw_training_data)), ], aes(x = GrLivArea, y = SalePrice)) + geom_point()

# Observation with GrLivArea > 4000 and SalePrice < 200000

rownames(all_data[all_data$Id < nrow(raw_training_data) & all_data$SalePrice < 200000 & all_data$GrLivArea > 4000, ])

ggplot(all_data[c(1:nrow(raw_training_data)), ], aes(x = LotArea, y = SalePrice)) + geom_point()

# Observations with LotArea > 150000

rownames(all_data[all_data$Id < nrow(raw_training_data) & all_data$LotArea > 200000, ])

ggplot(all_data[c(1:nrow(raw_training_data)), ], aes(x = LotFrontage, y = SalePrice)) + geom_point()

# Observations with LotFrontage > 300

rownames(all_data[all_data$Id < nrow(raw_training_data) & all_data$LotFrontage > 300, ])



# Plotting SalePrice and Log(SalePrice)
# par(mfrow = c(1,2))
# boxplot(all_data[c(1:nrow(raw_training_data)), "SalePrice"], main = "Sale Price")
# boxplot(log(all_data[c(1:nrow(raw_training_data)), "SalePrice"]), main = "log(Sale Price)")
# There are many outliers, but removing them all might bias the prediction for very cheap and very expensive houses,
# since the total number of observations is relatively small

# Let's fit a linear model with all the variables and look if the residuals are more helpful in identifying true outliers:

# Fitting a linear model with all the variables
lm.outlier = lm(SalePrice ~ ., data = all_data[c(1:nrow(raw_training_data)), ])


# Plotting residuals to identify outliers
par(mfrow = c(2,2))
plot(lm.outlier)

outliers_rows <- c(524, 1299, 314, 336, 935, 826, 1171, 1424)
# Looking at the cook distance, observations 826 and 524 have a clear high influence on the model, let's drop these two observations:

all_data <- all_data[-outliers_rows, ]

# ---------- Log Transformation ----------
# Plotting SalePrice and Log(SalePrice)
# par(mfrow = c(1,2))
# boxplot(all_data[c(1:nrow(raw_training_data)), "SalePrice"], main = "Sale Price")
# boxplot(log(all_data[c(1:nrow(raw_training_data)), "SalePrice"]), main = "log(Sale Price)")

# Transforming The target variable

# all_data$SalePrice <- log1p(all_data$SalePrice)
# summary(all_data$SalePrice[c(1:(nrow(raw_training_data)-length(outliers_rows)))])


# Transforming the other variables above a threshold skewness
# all_data <- data.table(all_data)
# list_skewness <- abs(all_data[, sapply(.SD, skewness, na.rm = T), .SDcols = !names(which(sapply(all_data, is.factor)))])
# 
# # Set threshold for skewness
# skew_thres <- 0.65
# 
# # Subset all columns based on skewness threshold  
# skewed_columns <- names(list_skewness[list_skewness > skew_thres]) 
# 
# skewed_columns <- skewed_columns[skewed_columns != "SalePrice"]
# 
# 
# # Subset test for independet test skewness 
# #skewed_columns_test <- names(list_skewness_test[list_skewness_test > skew_thres]) 
# 
# # Transform skewed columns using log1p
# all_data[, (skewed_columns) := lapply(.SD, log1p), .SDcols = skewed_columns] 
# 
# all_data <- data.frame(all_data)

skew.thres <- 1
i = 0
for (i in c(1:ncol(all_data))){
  if ((colnames(all_data[i]) != "Id") & (colnames(all_data[i]) != "SalePrice") & ((class(all_data[ ,i]) == "factor") != TRUE)){
    print(paste(i, colnames(all_data[i]), class(all_data[ ,i]), "Skewness = ", skewness(all_data[ ,i])))
    if ((skewness(all_data[ ,i])) > skew.thres){
      all_data[i] <- log1p(all_data[i])
      print(paste("Converting = ", colnames(all_data[i]), "New Skewness = ", skewness(all_data[ ,i])))
    }
  }
}



