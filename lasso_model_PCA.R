# Evaluation
PCA_lambdas <- 10^seq(-3, 3, by = .1)

PCA_lasso.cv_fit <- cv.glmnet(x = data.matrix(PCA_training[, !"SalePrice", with = F]), y = PCA_training$SalePrice, alpha = 1.5, lambda = PCA_lambdas, nfolds = 20)
plot(lasso.cv_fit)

# Select the best lambda form the CV model, use it to predict the target value of the validation set and evaluate the results (in terms of RMSE)
PCA_bestlam <- PCA_lasso.cv_fit$lambda.min
paste("Best Lambda value from CV=", PCA_bestlam)
PCA_lasso.mod <- glmnet(x = data.matrix(PCA_training[, !"SalePrice", with = F]), y = PCA_training$SalePrice, alpha = 1.5, lambda = PCA_lambdas)
PCA_lasso.pred = predict(PCA_lasso.mod, s=bestlam, data.matrix(PCA_validation[, !"SalePrice", with = F]))
paste("RMSE for lambda ", bestlam, " = ", sqrt(mean((PCA_lasso.pred - PCA_validation$SalePrice)^2)))

# Select the λ1se value from the CV model to predict on the validation set
PCA_lam1se <- PCA_lasso.cv_fit$lambda.1se
paste("Lambda 1se value from CV=", PCA_lam1se)
PCA_lasso.mod <- glmnet(x = data.matrix(PCA_training[, !"SalePrice", with = F]), y=PCA_training$SalePrice, alpha = 1.5, PCA_lambda = lambdas)
PCA_lasso.pred=predict(PCA_lasso.mod, s=PCA_lam1se, data.matrix(PCA_validation[, !"SalePrice", with = F]))
paste("RMSE for lambda ", PCA_lam1se, " = ", sqrt(mean((PCA_lasso.pred - PCA_validation$SalePrice)^2)))

# Plot important coefficients 
PCA_my_data=as.data.frame(cbind(PCA_predicted=PCA_lasso.pred,PCA_observed=PCA_validation$SalePrice))

ggplot(PCA_my_data,aes(PCA_my_data["1"],PCA_observed))+
  geom_point()+geom_smooth(method="lm")+
  scale_x_continuous(expand = c(0,0)) +
  labs(x="Predicted") +
  ggtitle('Lasso')

# Prediction on the test data
PCA_test_data <- data.table(PCA_test_data)
PCA_log_prediction <- predict(PCA_lasso.cv_fit,  s=PCA_lasso.cv_fit$lambda.min, newx = data.matrix(PCA_test_data[, !"Id", with = F]))
PCA_actual_pred <- exp(PCA_log_prediction)-1
dev.off()
hist(PCA_actual_pred)
plot(PCA_actual_pred, PCA_test_data$LotArea)
PCA_submit <- data.frame(Id=PCA_test_data$Id,SalePrice=PCA_actual_pred)
colnames(PCA_submit) <-c("Id", "SalePrice")

PCA_submit$SalePrice[is.na(PCA_submit$SalePrice)] <- 0
PCA_replace_value_for_na <- sum(na.omit(PCA_submit$SalePrice))/(nrow(PCA_submit) - sum(PCA_submit$SalePrice == 0))
PCA_submit$SalePrice[PCA_submit$SalePrice == 0] <- PCA_replace_value_for_na

write.csv(PCA_submit,file="PCA_submission.csv",row.names=F)
