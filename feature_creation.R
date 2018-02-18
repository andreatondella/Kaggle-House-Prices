# ANDREA TONDELLA
# IE MBD - Class O2
# Oct. 2017 Intake

# Machine Learning 2 - Individual Assignment
# Kaggle House Prices

# source("lib_loading.R")
# source("data_cleaning.R")

# Garage Interaction = Garage Quality * Number of Cars Garage Holds
i = 0
all_data$GarageInter <- NULL

for (i in c(1:nrow(all_data))){
  all_data$GarageInter[i] <- all_data$GarageQual[i] * all_data$GarageCars[i]
}

plot1 <- ggplot(all_data[c(1:nrow(raw_training_data)), ], aes(x = GarageInter, y = SalePrice)) + geom_point()
plot2 <- ggplot(all_data[c(1:nrow(raw_training_data)), ], aes(GarageInter)) + geom_histogram()
plot3 <- ggplot(all_data[c(1:nrow(raw_training_data)), ], aes(x = log1p(GarageInter), y = log1p(SalePrice))) + geom_point()
plot4 <- ggplot(all_data[c(1:nrow(raw_training_data)), ], aes(log1p(GarageInter))) + geom_histogram()
grid.arrange(plot1, plot2, plot3, plot4, ncol = 2, nrow = 2)

# Total number of bathrooms = Full Bath + Half Bath + Basement Full Bath + Basement Half Bath
i = 0
all_data$TotBath <- NULL

for (i in c(1:nrow(all_data))){
  all_data$TotBath[i] <- all_data$FullBath[i] + all_data$BsmtFullBath[i] + 0.5*(all_data$HalfBath[i] + all_data$BsmtHalfBath[i])
}

plot1 <- ggplot(all_data[c(1:nrow(raw_training_data)), ], aes(x = TotBath, y = SalePrice)) + geom_point()
plot2 <- ggplot(all_data[c(1:nrow(raw_training_data)), ], aes(TotBath)) + geom_histogram()
plot3 <- ggplot(all_data[c(1:nrow(raw_training_data)), ], aes(x = log1p(TotBath), y = log1p(SalePrice))) + geom_point()
plot4 <- ggplot(all_data[c(1:nrow(raw_training_data)), ], aes(log1p(TotBath))) + geom_histogram()
grid.arrange(plot1, plot2, plot3, plot4, ncol = 2, nrow = 2)

# Average Room Size = Above-Ground Living Area / Total Number of R00ms Above Ground
i = 0
all_data$AvgRoomSize <- NULL

for (i in c(1:nrow(all_data))){
  all_data$AvgRoomSize[i] <- all_data$GrLivArea[i] / all_data$TotRmsAbvGrd[i]
}

plot1 <- ggplot(all_data[c(1:nrow(raw_training_data)), ], aes(x = AvgRoomSize, y = SalePrice)) + geom_point()
plot2 <- ggplot(all_data[c(1:nrow(raw_training_data)), ], aes(AvgRoomSize)) + geom_histogram()
plot3 <- ggplot(all_data[c(1:nrow(raw_training_data)), ], aes(x = log1p(AvgRoomSize), y = log1p(SalePrice))) + geom_point()
plot4 <- ggplot(all_data[c(1:nrow(raw_training_data)), ], aes(log1p(AvgRoomSize))) + geom_histogram()
grid.arrange(plot1, plot2, plot3, plot4, ncol = 2, nrow = 2)

# Bathroom to room ratio = (Full Bath + Half Bath) / Number of Bedrooms Above Ground
i = 0
all_data$BathRoomRat <- NULL

for (i in c(1:nrow(all_data))){
  if (all_data$BedroomAbvGr[i] == 0){
    all_data$BathRoomRat[i] <- 0
  }
  else{
    all_data$BathRoomRat[i] <- (all_data$FullBath[i] + all_data$HalfBath[i]) / all_data$BedroomAbvGr[i]
  }
}
plot1 <- ggplot(all_data[c(1:nrow(raw_training_data)), ], aes(x = BathRoomRat, y = SalePrice)) + geom_point()
plot2 <- ggplot(all_data[c(1:nrow(raw_training_data)), ], aes(BathRoomRat)) + geom_histogram()
plot3 <- ggplot(all_data[c(1:nrow(raw_training_data)), ], aes(x = log1p(BathRoomRat), y = log1p(SalePrice))) + geom_point()
plot4 <- ggplot(all_data[c(1:nrow(raw_training_data)), ], aes(log1p(BathRoomRat))) + geom_histogram()
grid.arrange(plot1, plot2, plot3, plot4, ncol = 2, nrow = 2)

# Comparative size of living area = Above-Ground Living Area / mean(Above-Ground Living Area)
i = 0
all_data$CompLivArea <- NULL

for (i in c(1:nrow(all_data))){
  all_data$CompLivArea[i] <- all_data$GrLivArea[i] / mean(all_data$GrLivArea)
}

plot1 <- ggplot(all_data[c(1:nrow(raw_training_data)), ], aes(x = CompLivArea, y = SalePrice)) + geom_point()
plot2 <- ggplot(all_data[c(1:nrow(raw_training_data)), ], aes(CompLivArea)) + geom_histogram()
plot3 <- ggplot(all_data[c(1:nrow(raw_training_data)), ], aes(x = log1p(CompLivArea), y = log1p(SalePrice))) + geom_point()
plot4 <- ggplot(all_data[c(1:nrow(raw_training_data)), ], aes(log1p(CompLivArea))) + geom_histogram()
grid.arrange(plot1, plot2, plot3, plot4, ncol = 2, nrow = 2)

# # Landscape-ability interaction = Lot Shape * Land Contour
# i = 0
# all_data$LandContourRecode <- NULL
# 
# for (i in c(1:nrow(all_data))){
#   if (all_data$LandContour[i] == "Lvl"){
#     all_data$LandContourRecode[i] <- 2
#   }
#   else{
#     all_data$LandContourRecode[i] <- 1
#   }
# }
# 
# i = 0
# 
# all_data$Landscape <- NULL
# 
# for (i in c(1:nrow(all_data))){
#   all_data$Landscape[i] <- all_data$LotShape[i] / all_data$LandContourRecode[i]
# }
# 
# plot1 <- ggplot(all_data[c(1:nrow(raw_training_data)), ], aes(x = Landscape, y = SalePrice)) + geom_point()
# plot2 <- ggplot(all_data[c(1:nrow(raw_training_data)), ], aes(Landscape)) + geom_histogram()
# plot3 <- ggplot(all_data[c(1:nrow(raw_training_data)), ], aes(x = log1p(Landscape), y = log1p(SalePrice))) + geom_point()
# plot4 <- ggplot(all_data[c(1:nrow(raw_training_data)), ], aes(log1p(Landscape))) + geom_histogram()
# grid.arrange(plot1, plot2, plot3, plot4, ncol = 2, nrow = 2)

# Remodeled = categorical (Y/N)
i = 0

all_data$Remod <- NULL

for (i in c(1:nrow(all_data))){
  if (all_data$YearBuilt[i] == all_data$YearRemodAdd[i]){
    all_data$Remod[i] <- 0
  }
  else{
    all_data$Remod[i] <- 1
  }
}

# New House = categorical (Y/N)
i = 0

all_data$NewHouse <- NULL

for (i in c(1:nrow(all_data))){
  if (all_data$YearBuilt[i] == all_data$YrSold[i]){
    all_data$NewHouse[i] <- 1
  }
  else{
    all_data$NewHouse[i] <- 0
  }
}

# Total Area = sum of all area variables
i = 0

all_data$TotArea <- NULL

for (i in c(1:nrow(all_data))){
  all_data$TotArea[i] <- all_data$GrLivArea[i] + all_data$TotalBsmtSF[i] 
}

plot1 <- ggplot(all_data[c(1:nrow(raw_training_data)), ], aes(x = TotArea, y = SalePrice)) + geom_point()
plot2 <- ggplot(all_data[c(1:nrow(raw_training_data)), ], aes(TotArea)) + geom_histogram()
plot3 <- ggplot(all_data[c(1:nrow(raw_training_data)), ], aes(x = log1p(TotArea), y = log1p(SalePrice))) + geom_point()
plot4 <- ggplot(all_data[c(1:nrow(raw_training_data)), ], aes(log1p(TotArea))) + geom_histogram()
grid.arrange(plot1, plot2, plot3, plot4, ncol = 2, nrow = 2)

# Basement Score

i = 0

all_data$BsmtScore <- NULL

for (i in c(1:nrow(all_data))){
  all_data$BsmtScore[i] <- all_data$BsmtQual[i] + all_data$BsmtCond[i] + all_data$BsmtFinType1[i] + all_data$BsmtFinType2[i] + all_data$BsmtExposure[i]
}

plot1 <- ggplot(all_data[c(1:nrow(raw_training_data)), ], aes(x = BsmtScore, y = SalePrice)) + geom_point()
plot2 <- ggplot(all_data[c(1:nrow(raw_training_data)), ], aes(BsmtScore)) + geom_histogram()
plot3 <- ggplot(all_data[c(1:nrow(raw_training_data)), ], aes(x = log1p(BsmtScore), y = log1p(SalePrice))) + geom_point()
plot4 <- ggplot(all_data[c(1:nrow(raw_training_data)), ], aes(log1p(BsmtScore))) + geom_histogram()
grid.arrange(plot1, plot2, plot3, plot4, ncol = 2, nrow = 2)


# Garage Score

i = 0

all_data$GarageScore <- NULL

for (i in c(1:nrow(all_data))){
  all_data$GarageScore[i] <- all_data$GarageFinish[i] + all_data$GarageQual[i] + all_data$GarageCond[i]
}

plot1 <- ggplot(all_data[c(1:nrow(raw_training_data)), ], aes(x = GarageScore, y = SalePrice)) + geom_point()
plot2 <- ggplot(all_data[c(1:nrow(raw_training_data)), ], aes(GarageScore)) + geom_histogram()
plot3 <- ggplot(all_data[c(1:nrow(raw_training_data)), ], aes(x = log1p(GarageScore), y = log1p(SalePrice))) + geom_point()
plot4 <- ggplot(all_data[c(1:nrow(raw_training_data)), ], aes(log1p(GarageScore))) + geom_histogram()
grid.arrange(plot1, plot2, plot3, plot4, ncol = 2, nrow = 2)

# Look for seasonality

plot1 <- ggplot(all_data, aes(x = as.factor(YrSold), y = SalePrice)) + geom_boxplot()
plot2 <- ggplot(all_data, aes(x = as.factor(YrSold), y = log(SalePrice))) + geom_boxplot()
plot3 <- ggplot(all_data, aes(x = as.factor(MoSold), y = SalePrice)) + geom_boxplot()
plot4 <- ggplot(all_data, aes(x = as.factor(MoSold), y = log(SalePrice))) + geom_boxplot()
grid.arrange(plot1, plot2, plot3, plot4, ncol = 2, nrow = 2)

# Shrinking Factor Variables

str(all_data)

all_data <- data.table(all_data)

factor_variables <- names(which(sapply(all_data, is.factor)))

i = 0

for (i in c(1:length(factor_variables))){
  a <- summary(all_data[ ,factor_variables[i], with = FALSE])
  print(a)
}

# MSZoning: new columns for residential vs non residential
summary(all_data$MSZoning)
all_data[ ,MSZoningSim := recode(MSZoning, "C (all)" = "Com", "FV" = "Res", "RH" = "Res", "RL" = "Res", "RM" = "Res")]
summary(all_data$MSZoningSim)

# Land Contour: levelled or not
summary(all_data$LandContour)
all_data[ ,LandContourSim := recode(LandContour, "Bnk" = "NonL", "HLS" = "NonL", "Low" = "NonL", "Lvl" = "Lev")]
summary(all_data$LandContourSim)

# BldgType: 1Fam or not
summary(all_data$BldgType)
all_data[ ,BldgTypeSim := recode(BldgType, "1Fam" = "1Fam", "2fmCon" = "Other", "Duplex" = "Other", "Twnhs" = "Other", "TwnhsE" = "Other")]
summary(all_data$BldgTypeSim)

# RoofMatl: CompShg or other
summary(all_data$RoofMatl)
all_data[ ,RoofMatlSim := recode(RoofMatl, "ClyTile" = "Other", "CompShg" = "CompShg", "Membran" = "Other", "Metal" = "Other", "Roll" = "Other", "Tar&Grv" = "Other", "WdShake" = "Other", "WdShngl" = "Other")]
summary(all_data$RoofMatlSim)

# Heating: GasA or other
summary(all_data$Heating)
all_data[ ,HeatingSim := recode(Heating, "Floor" = "Other",  "GasA" = "GasA", "GasW" = "Other",  "Grav" = "Other",  "OthW" = "Other",  "Wall" = "Other")]
summary(all_data$HeatingSim)

# SaleType: WD New or other
summary(all_data$SaleType)
all_data[ ,SaleTypeSim := recode(SaleType, "COD" = "Other", "Con" = "Other", "ConLD" = "Other", "ConLI" = "Other", "ConLw" = "Other", "CWD" = "Other", "New" = "New", "Oth" = "Other", "WD" = "WD")]
summary(all_data$SaleTypeSim)

all_data <- data.frame(all_data)
