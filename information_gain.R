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
# source("full_lm.R")

### ---------- Information Gain Selection and Evaluation ----------

# Selection
weights<- data.frame(information.gain(SalePrice~., training_data))
weights$feature <- rownames(weights)
weights[order(weights$attr_importance, decreasing = TRUE),]
information_gain_features <- weights$feature[weights$attr_importance >= 0.015]

# Evaluation 
training <- data.frame(training)
ig.lm.mod <- train(SalePrice ~ ., data = training[append(information_gain_features, "SalePrice")], 
                   method = "lm", 
                   metric = "RMSE",
                   preProc = c("center", "scale"),
                   trControl=train_control_config)

for (x in names(validation)) {
  ig.lm.mod$xlevels[[x]] <- union(ig.lm.mod$xlevels[[x]], levels(validation[[x]]))
}
training <- data.table(training)
ig.lm.mod.pred <- predict(ig.lm.mod, validation[, !"SalePrice", with = F])
ig.lm.mod.pred[is.na(ig.lm.mod.pred)] <- 0

my_data=as.data.frame(cbind(predicted=ig.lm.mod.pred,observed=validation$SalePrice))

ggplot(my_data,aes(predicted,observed))+
  geom_point() + geom_smooth(method = "lm") +
  labs(x="Predicted") +
  ggtitle('Linear Model')

paste("IG Filtered Linear Regression RMSE = ", sqrt(mean((ig.lm.mod.pred - validation$SalePrice)^2)))

# Filtering
training <- data.frame(training)
validation <- data.frame(validation)
test_data <- data.frame(test_data)
training <- training[append(information_gain_features, "SalePrice")]
validation <- validation[append(information_gain_features, "SalePrice")]
test_data <- test_data[append(information_gain_features, "Id")]
training <- data.table(training)
validation <- data.table(validation)
test_data <- data.table(test_data)