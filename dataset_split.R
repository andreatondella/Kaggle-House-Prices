# ANDREA TONDELLA
# IE MBD - Class O2
# Oct. 2017 Intake

# Machine Learning 2 - Individual Assignment
# Kaggle House Prices

# source("lib_loading.R")
# source("data_cleaning.R")
# source("feature_creation.R")
# source("outliers_skewness.R")

### ---------- All_data split ----------
all_data_split <- c(1:(nrow(raw_training_data)-length(outliers_rows)))
training_data <- all_data[all_data_split, ]
test_data <- all_data[-all_data_split, ]

training_data <- training_data[,-which(names(training_data) == "Id")]
test_data <- test_data[,-which(names(test_data) == "SalePrice")]

training_data$SalePrice <- log1p(training_data$SalePrice)

### ---------- Train and validation splitting ----------

# I found this function, that is worth to save for future ocasions.
splitdf <- function(dataframe, seed=NULL) {
  if (!is.null(seed)) set.seed(seed)
  index <- 1:nrow(dataframe)
  trainindex <- sample(index, trunc(length(index)/1.5))
  trainset <- dataframe[trainindex, ]
  testset <- dataframe[-trainindex, ]
  list(trainset=trainset,testset=testset)
}
splits <- splitdf(training_data, seed=1)
training <- data.table(splits$trainset)
validation <- data.table(splits$testset)



