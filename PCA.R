# ANDREA TONDELLA
# IE MBD - Class O2
# Oct. 2017 Intake

# Machine Learning 2 - Individual Assignment
# Kaggle House Prices

### ---------- Actual PCA ----------
all_data_PCA_df <- all_data

all_data_PCA_df$SalePrice <- NULL
all_data_PCA_df$Id <- NULL
SalePrice <- all_data$SalePrice
Id <- all_data$Id

all_data_PCA_df <- data.matrix(all_data_PCA_df)

all_data_PCA <- prcomp(all_data_PCA_df, center = FALSE, scale. = FALSE)

PCA_df <- all_data_PCA$x

principal_components <- 75
PCA_df <- PCA_df[ ,1:principal_components]

PCA_df <- data.frame(PCA_df)


PCA_df <- cbind(PCA_df, SalePrice, Id)


### ----------- Model Trial --------

PCA_all_data_split <- c(1:(nrow(raw_training_data)-length(outliers_rows)))
PCA_training_data <- PCA_df[PCA_all_data_split, ]
PCA_test_data <- PCA_df[-PCA_all_data_split, ]

PCA_training_data <- PCA_training_data[,-which(names(PCA_training_data) == "Id")]
PCA_test_data <- PCA_test_data[,-which(names(PCA_test_data) == "SalePrice")]

PCA_training_data$SalePrice <- log1p(PCA_training_data$SalePrice)

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
PCA_splits <- splitdf(PCA_training_data, seed=1)
PCA_training <- data.table(PCA_splits$trainset)
PCA_validation <- data.table(PCA_splits$testset)
