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
all_data_split <- c(1:(nrow(raw_training_data)-4))
training_data <- all_data[all_data_split, ]
test_data <- all_data[-all_data_split, ]


