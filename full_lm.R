# ANDREA TONDELLA
# IE MBD - Class O2
# Oct. 2017 Intake

# Machine Learning 2 - Individual Assignment
# Kaggle House Prices

# source("lib_loading.R")
# source("data_cleaning.R")
# source("feature_creation.R")
# source("outliers_skewness.R")
# source("dataset_split.R")

### ---------- Full lm model ----------

set.seed(121)
train_control_config <- trainControl(method = "repeatedcv", 
                                     number = 5, 
                                     repeats = 1,
                                     returnResamp = "all")

full.lm.mod <- train(SalePrice ~ ., data = training, 
                     method = "lm", 
                     metric = "RMSE",
                     preProc = c("center", "scale"),
                     trControl=train_control_config)

for (x in names(validation)) {
  full.lm.mod$xlevels[[x]] <- union(full.lm.mod$xlevels[[x]], levels(validation[[x]]))
}
full.lm.mod.pred <- predict(full.lm.mod, validation[, !"SalePrice", with = F])
full.lm.mod.pred[is.na(full.lm.mod.pred)] <- 0

my_data=as.data.frame(cbind(predicted=full.lm.mod.pred,observed=validation$SalePrice))

ggplot(my_data,aes(predicted,observed))+
  geom_point() + geom_smooth(method = "lm") +
  labs(x="Predicted") +
  ggtitle('Linear Model')

paste("Full Linear Regression RMSE = ", sqrt(mean((full.lm.mod.pred - validation$SalePrice)^2)))
