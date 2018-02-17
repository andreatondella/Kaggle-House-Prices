# ANDREA TONDELLA
# IE MBD - Class O2
# Oct. 2017 Intake

# Machine Learning 2 - Individual Assignment
# Kaggle House Prices

source("lib_loading.R")
source("data_cleaning.R")
source("feature_creation.R")
source("outliers_skewness.R")

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

# Evaluation
lambdas <- 10^seq(-3, 3, by = .1)

lasso.cv_fit <- cv.glmnet(x = data.matrix(training[, !"SalePrice", with = F]), y = training$SalePrice, alpha = 1, lambda = lambdas, nfolds = 20)
plot(lasso.cv_fit)

# Select the best lambda form the CV model, use it to predict the target value of the validation set and evaluate the results (in terms of RMSE)
bestlam <- lasso.cv_fit$lambda.min
paste("Best Lambda value from CV=", bestlam)
lasso.mod <- glmnet(x = data.matrix(training[, !"SalePrice", with = F]), y = training$SalePrice, alpha = 1, lambda = lambdas)
lasso.pred = predict(lasso.mod, s=bestlam, data.matrix(validation[, !"SalePrice", with = F]))
paste("RMSE for lambda ", bestlam, " = ", sqrt(mean((lasso.pred - validation$SalePrice)^2)))

# Select the λ1se value from the CV model to predict on the validation set
lam1se <- lasso.cv_fit$lambda.1se
paste("Lambda 1se value from CV=", lam1se)
lasso.mod <- glmnet(x = data.matrix(training[, !"SalePrice", with = F]), y=training$SalePrice, alpha = 1, lambda = lambdas)
lasso.pred=predict(lasso.mod, s=lam1se, data.matrix(validation[, !"SalePrice", with = F]))
paste("RMSE for lambda ", lam1se, " = ", sqrt(mean((lasso.pred - validation$SalePrice)^2)))

# Prediction on the test data
test_data <- data.table(test_data)
log_prediction <- predict(lasso.cv_fit,  s=lasso.cv_fit$lambda.min, newx = data.matrix(test_data[, !"Id", with = F]))
actual_pred <- exp(log_prediction)-1
hist(actual_pred)
submit <- data.frame(Id=test_data$Id,SalePrice=actual_pred)
colnames(submit) <-c("Id", "SalePrice")

submit$SalePrice[is.na(submit$SalePrice)] <- 0
replace_value_for_na <- sum(na.omit(submit$SalePrice))/(nrow(submit) - sum(submit$SalePrice == 0))
submit$SalePrice[submit$SalePrice == 0] <- replace_value_for_na

write.csv(submit,file="submission_V4.csv",row.names=F)
