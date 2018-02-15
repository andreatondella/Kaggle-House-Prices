# ANDREA TONDELLA
# IE MBD - Class O2
# Oct. 2017 Intake

# Machine Learning 2 - Individual Assignment
# Kaggle House Prices

source("lib_loading.R")
source("data_cleaning.R")

### ---------- All_data split ----------
all_data_split <- c(1:(nrow(raw_training_data)-2))
training_data <- all_data[all_data_split, ]
test_data <- all_data[-all_data_split, ]
### ---------- Sales Price skewness and log transformation ----------

# get data frame of SalePrice and log(SalePrice + 1) for plotting
df <- rbind(data.frame(version="log(price+1)",x=log(training_data$SalePrice + 1)),
            data.frame(version="price",x=training_data$SalePrice))

ggplot(data=df) +
  facet_wrap(~version,ncol=2,scales="free_x") +
  geom_histogram(aes(x=x), bins = 50)

# Log transform the target for official scoring
training_data$SalePrice <- log1p(training_data$SalePrice)
### ---------- Skewness and log transformation for other variables ----------
skew.thres = 0.75

# Train Data
column_types <- sapply(names(training_data),function(x){class(training_data[[x]])})
numeric_columns <-names(column_types[column_types == "integer"])

# skew of each variable
skew <- sapply(numeric_columns,function(x){skewness(training_data[[x]],na.rm = T)})

# transform all variables above a threshold skewness.
skew <- skew[skew > skew.thres]
for(x in names(skew)) {
  training_data[[x]] <- log(training_data[[x]] + 1)
}

# Test Data
column_types <- sapply(names(test_data),function(x){class(test_data[[x]])})
numeric_columns <-names(column_types[column_types == "integer"])

skew2 <- sapply(numeric_columns,function(x){skewness(test_data[[x]],na.rm = T)})
skew2 <- skew[skew > skew.thres]
for(x in names(skew2)) {
  test_data[[x]] <- log(test_data[[x]] + 1)
}
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
training <- splits$trainset
validation <- splits$testset
### ---------- Removal of incomplete cases ----------

# ## remove incomplete cases
# paste("Training set incomplete cases")
# sapply(lapply(na.omit(training)[sapply(na.omit(training), is.factor)], droplevels), nlevels)
# paste("Validation set incomplete cases")
# sapply(lapply(na.omit(validation)[sapply(na.omit(validation), is.factor)], droplevels), nlevels)
# paste("Test set incomplete cases")
# sapply(lapply(na.omit(test_data)[sapply(na.omit(test_data), is.factor)], droplevels), nlevels)


# Evaluation
lambdas <- 10^seq(-3, 3, by = .1)

lasso.cv_fit <- cv.glmnet(x = data.matrix(training[,-ncol(training)]), y=training$SalePrice, alpha = 1, lambda = lambdas)
plot(lasso.cv_fit)

# Select the best lambda form the CV model, use it to predict the target value of the validation set and evaluate the results (in terms of RMSE)
bestlam <- lasso.cv_fit$lambda.min
paste("Best Lambda value from CV=", bestlam)
lasso.mod <- glmnet(x = data.matrix(training[,-ncol(training)]), y=training$SalePrice, alpha = 1, lambda = lambdas)
lasso.pred=predict(lasso.mod, s=bestlam, data.matrix(validation[,-ncol(validation)]))
paste("RMSE for lambda ", bestlam, " = ", sqrt(mean((lasso.pred - validation$SalePrice)^2)))

# Select the λ1se value from the CV model to predict on the validation set
lam1se <- lasso.cv_fit$lambda.1se
paste("Lambda 1se value from CV=", lam1se)
lasso.mod <- glmnet(x = data.matrix(training[,-ncol(training)]), y=training$SalePrice, alpha = 1, lambda = lambdas)
lasso.pred=predict(lasso.mod, s=lam1se, data.matrix(validation[,-ncol(validation)]))
paste("RMSE for lambda ", lam1se, " = ", sqrt(mean((lasso.pred - validation$SalePrice)^2)))

# # Predictions against the actual values
# # Plot important coefficients
# my_data=as.data.frame(cbind(predicted=lasso.pred,observed=validation$SalePrice))
# 
# ggplot(my_data,aes(my_data["1"],observed))+
#   geom_point()+geom_smooth(method="lm")+
#   scale_x_continuous(expand = c(0,0)) +
#   labs(x="Predicted") +
#   ggtitle('Lasso')
# 
# # Variable Importance
# # Print, plot variable importance
# imp <- varImp(lasso.mod, lambda = bestlam)
# names <- rownames(imp)[order(imp$Overall, decreasing=TRUE)]
# importance <- imp[names,]
# 
# data.frame(row.names = names, importance)
# 
# # Variables selected by the lasso model (only those with importance larger than 0)
# filtered_names <- rownames(imp)[order(imp$Overall, decreasing=TRUE)][1:28]
# print(filtered_names)
# 
# Prediction on the test data
test_data <- test_data[,-which(names(all_data) == "SalePrice")]

log_prediction <- predict(lasso.cv_fit,  s=lasso.cv_fit$lambda.min, newx = data.matrix(test_data))
actual_pred <- exp(log_prediction)-1
hist(actual_pred)
submit <- data.frame(Id=test_data$Id,SalePrice=actual_pred)
colnames(submit) <-c("Id", "SalePrice")

submit$SalePrice[is.na(submit$SalePrice)] <- 0
replace_value_for_na <- sum(na.omit(submit$SalePrice))/(nrow(submit) - sum(submit$SalePrice == 0))
submit$SalePrice[submit$SalePrice == 0] <- replace_value_for_na

write.csv(submit,file="first_submission.csv",row.names=F)
