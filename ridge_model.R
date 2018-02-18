# ANDREA TONDELLA
# IE MBD - Class O2
# Oct. 2017 Intake

# Machine Learning 2 - Individual Assignment
# Kaggle House Prices


### ---------- Ridge Regression and Cross Validation ----------

# Model 
lambdas <- 10^seq(-2, 3, by = .1)
ridge.cv_fit <- cv.glmnet(x = data.matrix(training[, !"SalePrice", with = F]), y=training$SalePrice, alpha = 0, lambda = lambdas, nfolds = 20)

# Evaluation
RMSE = numeric(length(lambdas))
for (i in seq_along(lambdas)){
  ridge.pred=predict(ridge.cv_fit, s=lambdas[i], data.matrix(validation[, !"SalePrice", with = F]))
  RMSE[i] <- sqrt(mean((ridge.pred - validation$SalePrice)^2))
}
plot(lambdas, RMSE, main="Ridge", log="x", type = "b")

# Cross-Validation - Select the best lambda form the CV model, use it to predict the target value of the validation set and evaluate the results (in terms of RMSE)

bestlam <- ridge.cv_fit$lambda.min
paste("Best Lambda value from CV=", bestlam)
ridge.pred=predict(ridge.cv_fit, s=bestlam, data.matrix(validation[, !"SalePrice", with = F]))
paste("RMSE for lambda ", bestlam, " = ", sqrt(mean((ridge.pred - validation$SalePrice)^2)))

# Cross-Validation - Select the λ1se value from the CV model to predict on the validation set
lam1se <- ridge.cv_fit$lambda.1se
paste("Lambda 1se value from CV=", lam1se)
ridge.pred=predict(ridge.cv_fit, s=lam1se, data.matrix(validation[, !"SalePrice", with = F]))
paste("RMSE for lambda ", lam1se, " = ", sqrt(mean((ridge.pred - validation$SalePrice)^2)))

# Cross-Validation - Let's plot the predictions against the actual values to have an idea of the model performance

# Plot important coefficients 
my_data=as.data.frame(cbind(predicted=ridge.pred,observed=validation$SalePrice))

ggplot(my_data,aes(my_data["1"],observed))+
  geom_point()+geom_smooth(method="lm")+
  scale_x_continuous(expand = c(0,0)) +
  labs(x="Predicted") +
  ggtitle('Ridge')

# Cross-Validation - Rank the variables according to the importance attributed by the model

# Print, plot variable importance
imp <- varImp(ridge.cv_fit, lambda = bestlam)
names <- rownames(imp)[order(imp$Overall, decreasing=TRUE)]
importance <- imp[names,]

data.frame(row.names = names, importance)

# Prediction on the test data
test_data <- data.table(test_data)
log_prediction <- predict(ridge.cv_fit,  s=lasso.cv_fit$lambda.min, newx = data.matrix(test_data[, !"Id", with = F]))
actual_pred <- exp(log_prediction)-1
hist(actual_pred)
plot(actual_pred, test_data$LotArea)
submit <- data.frame(Id=test_data$Id,SalePrice=actual_pred)
colnames(submit) <-c("Id", "SalePrice")

submit$SalePrice[is.na(submit$SalePrice)] <- 0
replace_value_for_na <- sum(na.omit(submit$SalePrice))/(nrow(submit) - sum(submit$SalePrice == 0))
submit$SalePrice[submit$SalePrice == 0] <- replace_value_for_na

write.csv(submit,file="submission_V29.csv",row.names=F)
