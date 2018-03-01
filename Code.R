library(stringr)
library(corrplot)
library(PerformanceAnalytics)
library(caret)
library(e1071)

train = read.csv("train.csv", header = TRUE)
test  = read.csv("test.csv",  header = TRUE)
# Combining the test and train data set for data cleaning
df.combined <- rbind(within(train, rm('Id','SalePrice')), within(test, rm('Id')))
dim(df.combined)

# Checking the columns with NA values 
na.cols <- which(colSums(is.na(df.combined)) > 0)
sort(colSums(sapply(df.combined[na.cols], is.na)), decreasing = TRUE)

############################## PoolQC ###################################

# Replace with common variable based on higher correlation
# Plotting for feature PoolQC
counts <- table(df.combined$PoolQC)
barplot(counts, main="PoolQC Plot", xlab="Pool QC Levels", col= "blue")

# Checking how PoolQC is related to Pools with area > 0
df.combined[(df.combined$PoolArea > 0) & is.na(df.combined$PoolQC),c('PoolQC','PoolArea')]
# Only 3 of such values exist

df.combined[,c('PoolQC','PoolArea')] %>% group_by(PoolQC) %>% 
  summarise(mean = mean(PoolArea), counts = n()) 

# Switching 2421 and 2504 with catagory "Ex", 2600 with "Fa" and the rest with 'None' (no pool)
df.combined[c(2421, 2504),'PoolQC'] = 'Ex'
df.combined[2600,'PoolQC'] = 'Fa'

# Adding level "None" to PoolQC
levels(df.combined$PoolQC) = c(levels(df.combined$PoolQC), "None")

# Replacing NA with 'None'
df.combined$PoolQC[is.na(df.combined$PoolQC)] = 'None'

############################## Alley ###################################

# Plotting for Alley
counts <- table(df.combined$Alley)
barplot(counts, main="Alley Plot", xlab="Alley Levels", col= "blue")

# Adding level "None" to Alley
levels(df.combined$Alley) = c(levels(df.combined$Alley), "None")

# Replacing NA with 'None'
df.combined$Alley[is.na(df.combined$Alley)] = 'None'

############################## MiscFeature ###################################

# Plotting for MiscFeature
counts <- table(df.combined$MiscFeature)
barplot(counts, main="MiscFeature Plot", xlab="MiscFeature Levels", col= "blue")

# Adding level "None" to MiscFeature
levels(df.combined$MiscFeature) = c(levels(df.combined$MiscFeature), "None")

# Replacing NA with 'None'
df.combined$MiscFeature[is.na(df.combined$MiscFeature)] = 'None'

############################## Fence ###################################

# Plotting for Fence
counts <- table(df.combined$Fence)
barplot(counts, main="Fence Plot", xlab="Fence Levels", col= "blue")

# Adding level "None" to Fence
levels(df.combined$Fence) = c(levels(df.combined$Fence), "None")

# Replacing NA with 'None'
df.combined$Fence[is.na(df.combined$Fence)] = 'None'

############################## FireplaceQu ###################################

# Plotting for FireplaceQu
counts <- table(df.combined$FireplaceQu)
barplot(counts, main="FireplaceQu Plot", xlab="FireplaceQu Levels", col= "blue")

# Checking how Fireplaces is related to FireplaceQu with area > 0
df.combined[(df.combined$Fireplaces > 0) & is.na(df.combined$FireplaceQu),c('FireplaceQu','Fireplaces')]
# No fireplaces for these NA or they don't have a fireplace

# Adding level "None" to FireplaceQu
levels(df.combined$FireplaceQu) = c(levels(df.combined$FireplaceQu), "None")

# Replacing NA with 'None'
df.combined$FireplaceQu[is.na(df.combined$FireplaceQu)] = 'None'

############################## LotFrontage ###################################

df.combined['Nbrh.factor'] <- factor(df.combined$Neighborhood, levels = unique(df.combined$Neighborhood))

# LotFrontage by Neighborhood
lot.by.nbrh <- df.combined[,c('Neighborhood','LotFrontage')] %>%
  group_by(Neighborhood) %>%
  summarise(median = median(LotFrontage, na.rm = TRUE))
lot.by.nbrh

idx = which(is.na(df.combined$LotFrontage))

for (i in idx)
  {
  lot.median <- lot.by.nbrh[lot.by.nbrh$Neighborhood == df.combined$Neighborhood[i],'median']
  df.combined[i,'LotFrontage'] <- lot.median[[1]]
  }

############################## GarageYrBlt ###################################

idx = which(is.na(df.combined$GarageYrBlt))
df.combined[idx, 'GarageFinish'] # No Garage available so no start date (set to 0)
df.combined$GarageYrBlt[is.na(df.combined$GarageYrBlt)] = 0

############################## GarageFinish ###################################

# Adding level "None" to GarageFinish
levels(df.combined$GarageFinish) = c(levels(df.combined$GarageFinish), "None")

# Replacing NA with 'None'
df.combined$GarageFinish[is.na(df.combined$GarageFinish)] = 'None'

############################## GarageQual ###################################

garage.cols <- c('GarageArea', 'GarageCars', 'GarageQual', 'GarageFinish', 'GarageCond', 'GarageType')
df.combined[is.na(df.combined$GarageCond),garage.cols]
#2127        360          1       <NA>         None       <NA>     Detchd
#2577         NA         NA       <NA>         None       <NA>     Detchd

idx <- which(((df.combined$GarageArea < 370) & (df.combined$GarageArea > 350)) & (df.combined$GarageCars == 1))
names(sapply(df.combined[idx, garage.cols], function(x) sort(table(x), decreasing=TRUE)[1]))

df.combined[2127,c('GarageQual','GarageCond')] = 'TA'
df.combined[2127, 'GarageFinish'] = 'Unf'

df.combined[2577, 'GarageArea'] = 0
df.combined[2577, 'GarageCars'] = 0

# Adding level "None" to GarageQual
levels(df.combined$GarageQual) = c(levels(df.combined$GarageQual), "None")

# Replacing NA with 'None'
df.combined$GarageQual[is.na(df.combined$GarageQual)] = 'None'

# Adding level "None" to GarageQual
levels(df.combined$GarageQual) = c(levels(df.combined$GarageQual), "None")

# Replacing NA with 'None'
df.combined$GarageQual[is.na(df.combined$GarageQual)] = 'None'

############################## GarageCond ###################################

# Adding level "None" to GarageCond
levels(df.combined$GarageCond) = c(levels(df.combined$GarageCond), "None")

# Replacing NA with 'None'
df.combined$GarageCond[is.na(df.combined$GarageCond)] = 'None'

############################## GarageType ###################################

# Adding level "None" to GarageType
levels(df.combined$GarageType) = c(levels(df.combined$GarageType), "None")

# Replacing NA with 'None'
df.combined$GarageType[is.na(df.combined$GarageType)] = 'None'

############################## BsmtCond & BsmtExposure ###################################

bsmt.cols <- names(df.combined)[sapply(names(df.combined), function(x) str_detect(x, 'Bsmt'))]
df.combined[is.na(df.combined$BsmtExposure),bsmt.cols]

##############################################################################################
# [1] "BsmtQual"     "BsmtCond"     "BsmtExposure" "BsmtFinType1" "BsmtFinSF1"   "BsmtFinType2"
#[7] "BsmtFinSF2"   "BsmtUnfSF"    "TotalBsmtSF"  "BsmtFullBath" "BsmtHalfBath"
##############################################################################################

counts <- table(df.combined$BsmtExposure)
barplot(counts, main="BsmtExposure Plot", xlab="BsmtExposure Levels", col= "blue")

idx = which(df.combined$BsmtUnfSF != 0 & is.na(df.combined$BsmtExposure)==T)
df.combined[idx, 'BsmtExposure'] = 'No'

for (i in bsmt.cols){
  if (sapply(df.combined[i], is.numeric) == TRUE){
    df.combined[sapply(df.combined[i], is.na),i] = 0
  }
  else{
    levels(df.combined[,i]) = c(levels(df.combined[,i]), "None")
    df.combined[sapply(df.combined[i],is.na),i] = 'None'
  }
}

############################## MasVnrType & MasVnrArea ###################################

df.combined[(is.na(df.combined$MasVnrType)) | (is.na(df.combined$MasVnrArea)), c('MasVnrType', 'MasVnrArea')]

na.omit(df.combined[,c('MasVnrType','MasVnrArea')]) %>%
  group_by(na.omit(MasVnrType)) %>%
  summarise(MedianArea = median(MasVnrArea,na.rm = TRUE), counts = n()) %>%
  arrange(MedianArea)

# 4               BrkFace        203    879
df.combined[2611, 'MasVnrType'] = 'BrkFace'

df.combined$MasVnrType[is.na(df.combined$MasVnrType)] = 'None'
df.combined$MasVnrArea[is.na(df.combined$MasVnrArea)] = 0

############################## MSZoning ###################################

df.combined$MSZoning[c(2217, 2905)] = 'RL'
df.combined$MSZoning[c(1916, 2251)] = 'RM'

############################## Exterior1st & Exterior2nd ###################################

counts <- table(df.combined$Exterior1st)
barplot(counts, main="Exterior1st Plot", xlab="Exterior1st Levels", col= "blue")

# Adding level to Exterior1st
levels(df.combined$Exterior1st) = c(levels(df.combined$Exterior1st), 'Other')

# Filling the NA values
df.combined$Exterior1st[is.na(df.combined$Exterior1st)] = 'Other'
df.combined$Exterior2nd[is.na(df.combined$Exterior2nd)] = 'Other'

############################## Utilities ###################################

counts <- table(df.combined$Utilities)
barplot(counts, main="Utilities Plot", xlab="Utilities Levels", col= "blue")

idx = which(df.combined$Utilities == 'NoSeWa')
df.combined <- df.combined[,!names(df.combined) %in% c('Utilities')]

############################## Functional ###################################

df.combined$Functional[is.na(df.combined$Functional)] = 'Typ'

############################## Electrical ###################################

df.combined$SaleType[is.na(df.combined$SaleType)] = 'WD'

############################## KitchenQual ###################################

df.combined$KitchenQual[is.na(df.combined$KitchenQual)] = 'TA'

############################## SaleType ###################################

df.combined$SaleType[is.na(df.combined$SaleType)] = 'WD'

################################################################################
# Preparing for regression
################################################################################

numeric_features <- names(which(sapply(df.combined, is.numeric)))
#catagorical_features <- names(which(sapply(df.combined, is.character)))

df.numeric <- df.combined[numeric_features]

group.df <- df.combined[1:1460,]
group.df$SalePrice <- train$SalePrice


# function that groups a column by its features and returns the mdedian saleprice for each unique feature. 
group.prices <- function(col) {
  group.table <- group.df[,c(col, 'SalePrice', 'OverallQual')] %>%
    group_by_(col) %>%
    summarise(mean.Quality = round(mean(OverallQual),2),
              mean.Price = mean(SalePrice), no_of_houses = n()) %>%
    arrange(mean.Quality)
  
  print(qplot(x=reorder(group.table[[col]], -group.table[['mean.Price']]), y=group.table[['mean.Price']]) +
          geom_bar(stat='identity', fill='cornflowerblue') +
          theme_minimal() +
          labs(x=col, y='Mean SalePrice')+theme(axis.text.x = element_text(angle = 45)))
  
  return(data.frame(group.table))
}
group.prices('FireplaceQu')
  
# function that maps a categoric value to ordinal value and returns that column to the data frame
to_numeric <- function(cols, map.list, df){
  for (col in cols){
    df[col] <- as.numeric(map.list[df.combined[,col]])
  }
  return(df)
}

############################## Quality_columns ###################################

Quality_columns <- c('ExterQual', 'ExterCond', 'GarageQual', 'GarageCond', 'FireplaceQu', 'KitchenQual', 'HeatingQC', 'BsmtQual')
ordinal_list <- c('None' = 0, 'Po' = 1, 'Fa' = 2, 'TA' = 3, 'Gd' = 4, 'Ex' = 5)

df.numeric <- to_numeric(Quality_columns,ordinal_list,df.numeric)

############################## Basement ###################################

group.prices('BsmtExposure')
bsmt_columns <- c('BsmtExposure')
bsmt_list <- c('None' = 0, 'No' = 1, 'Mn' = 2, 'Av' = 3, 'Gd' = 4)

df.numeric <- to_numeric(bsmt_columns,bsmt_list,df.numeric)

############################## Basement ###################################

group.prices('BsmtFinType1')
group.prices('BsmtFinType1')

bsmt_fin_columns <- c('BsmtFinType1','BsmtFinType2')
bsmt_fin_list <- c('None' = 0, 'Unf' = 1, 'Rec' = 2,'BLQ'= 3, 'LwQ' = 4, 'ALQ' = 5, 'GLQ' = 6)
df.numeric <- to_numeric(bsmt_fin_columns, bsmt_fin_list, df.numeric)

############################## Functional ###################################

group.prices('Functional')
functional_columns <- c('Functional')
functional_list <- c('None' = 0, 'Sev' = 1, 'Maj2' = 2, 'Maj1' = 3, 'Mod' = 4, 'Min2' = 5, 'Min1' = 6, 'Typ'= 7)
df.numeric <- to_numeric(functional_columns, functional_list, df.numeric)

############################## GarageType ###################################

group.prices('GarageFinish')
garagefinish_list <- c('None' = 0,'Unf' = 1, 'RFn' = 1, 'Fin' = 2)

df.numeric['GarageFinish'] <- as.numeric(garagefinish_list[df.combined$GarageFinish])

############################## Fence ###################################

group.prices('Fence')
fence_list <- c('None' = 0, 'MnWw' = 1, 'GdWo' = 1, 'MnPrv' = 2, 'GdPrv' = 4)

df.numeric['Fence'] <- as.numeric(fence_list[df.combined$Fence])

############################## MSdwelling ###################################

MSdwelling_list <- c('20' = 1, '30'= 0, '40' = 0, '45' = 0,'50' = 0, '60' = 1, '70' = 0, '75' = 0, '80' = 0, '85' = 0, '90' = 0, '120' = 1, '150' = 0, '160' = 0, '180' = 0, '190' = 0)

df.numeric['NewerDwelling'] <- as.numeric(MSdwelling_list[as.character(df.combined$MSSubClass)])

###########################################################################################
# Checking correlation betwen data
###########################################################################################

for_corr_df <- cbind(df.numeric[1:1460,], train['SalePrice'])
corrs <- cor(for_corr_df)
high_corr <- as.matrix(sort(corrs[,'SalePrice'], decreasing = T))
# Picking highly correlated fields
idx <- names(which(apply(high_corr,1, function(x)( x>0.5 | x< -0.5))))

corrplot(as.matrix(corrs[idx,idx]), type = 'upper', method='color', addCoef.col = 'black', tl.cex = .7,cl.cex = .7, number.cex=.7)

###########################################################################################
# Checking the highly correlated variables

cor(df.numeric$GarageArea, df.numeric$GarageCars)
# They are highly corelated 0.8898902 which is as expected area prop to no of cars that can be put
# Lets keep GarageCars and leave GarageArea

cor(df.numeric$TotalBsmtSF, df.numeric$X1stFlrSF)
# They are highly corelated 0.8013759 as basement area is prop. to 1st floor area
# Lets keep TotalBsmtSF and leave X1stFlrSF

cor(df.numeric$GrLivArea, df.numeric$TotRmsAbvGrd)
# They are highly corelated 0.8083544 as total no of rooms is prop. to  total area
# Lets keep GrLivArea and leave TotRmsAbvGrd

LotShape.mostfreq <- names(which.max(prop.table(table(df.combined$LotShape))))
df.numeric['LotShape-Reg'] <- (df.combined$LotShape == LotShape.mostfreq) * 1

LandContour.mostfreq <- names(which.max(prop.table(table(df.combined$LandContour))))
df.numeric['LotContour-Lvl'] <- (df.combined$LandContour == LandContour.mostfreq) * 1

LotShape.mostfreq <- names(which.max(prop.table(table(df.combined$LotConfig))))
df.numeric['LotShape-Reg'] <- (df.combined$LotConfig == LotShape.mostfreq) * 1

LotSlope.mostfreq <- names(which.max(prop.table(table(df.combined$LandSlope))))
df.numeric['LotSlope-Gtl'] <- (df.combined$LandSlope == LotShape.mostfreq) * 1

prop.table(table(df.combined$GarageType))
GarageType.mostfreq <- names(which.max(prop.table(table(df.combined$GarageType))))
df.numeric['GarageType-Attchd'] <- (df.combined$GarageType == GarageType.mostfreq) * 1
df.numeric['GarageType-Detchd'] <- (df.combined$GarageType == 'Detchd') * 1

Electrical.mostfreq <- names(which.max(prop.table(table(df.combined$Electrical))))
df.numeric['Electrical-SB'] <- (df.combined$Electrical == Electrical.mostfreq) * 1

prop.table(table(df.combined$PavedDrive))
df.numeric['PavedDrive-Y'] <- (df.combined$PavedDrive == 'Y') * 1

prop.table(table(df.combined$MiscFeature))
df.numeric['MiscFeature-Shed'] <- (df.combined$MiscFeature == 'Shed') * 1

df.numeric['Remodeled'] <- (df.combined$YearBuilt != df.combined$YearRemodAdd) * 1
df.numeric['RecentRemodel'] <- (df.combined$YearRemodAdd >= df.combined$YrSold) * 1
df.numeric['NewHouse'] <- (df.combined$YearBuilt == df.combined$YrSold) * 1

# For binary type of variables
cols.binary <- c('X2ndFlrSF', 'MasVnrArea', 'WoodDeckSF', 'OpenPorchSF', 'EnclosedPorch', 'X3SsnPorch', 'ScreenPorch')

for (col in cols.binary){
  df.numeric[str_c(col,'-Y')] <- (df.combined[,col] != 0) * 1
}

# Splitting neighborhood based on median prices for houses
group.prices('Neighborhood')

posh.areas <- c('Crawfor', 'Somerst, Timber', 'StoneBr', 'NoRidge', 'NridgeHt')
df.numeric['PoshNeighborhood'] <- (df.combined$Neighborhood %in% posh.areas) *1

otherneighborhoods <- c('MeadowV' = 0, 'IDOTRR' = 1, 'Sawyer' = 1, 'BrDale' = 1, 'OldTown' = 1, 'Edwards' = 1, 
              'BrkSide' = 1, 'Blueste' = 1, 'SWISU' = 2, 'NAmes' = 2, 'NPkVill' = 2, 'Mitchel' = 2,
              'SawyerW' = 2, 'Gilbert' = 2, 'NWAmes' = 2, 'Blmngtn' = 2, 'CollgCr' = 2, 'ClearCr' = 3, 
              'Crawfor' = 3, 'Veenker' = 3, 'Somerst' = 3, 'Timber' = 3, 'StoneBr' = 4, 'NoRidge' = 4, 
              'NridgHt' = 4)

df.numeric['Neighborhoods'] <- as.numeric(otherneighborhoods[df.combined$Neighborhood])

# Splitting SaleCondition 
group.prices('SaleCondition')
prop.table(table(df.combined$SaleCondition))
df.numeric['SaleCondition-Partial'] <- (df.combined$SaleCondition == 'Partial') * 1

# Splitting HeatingQC
group.prices('HeatingQC')
prop.table(table(df.combined$SaleCondition))

heating_levels <- c('Po' = 0, 'Fa' = 1, 'TA' = 2, 'Gd' = 3, 'Ex' = 4)

df.numeric['HeatingQCscaled'] <- as.numeric(heating_levels[df.combined$HeatingQC])

# Creating total area, arear inside column which sums all the areas
area_cols <- c('LotFrontage', 'LotArea', 'MasVnrArea', 'BsmtFinSF1', 'BsmtFinSF2', 'BsmtUnfSF',
               'TotalBsmtSF', 'X1stFlrSF', 'X2ndFlrSF', 'GrLivArea', 'GarageArea', 'WoodDeckSF', 
               'OpenPorchSF', 'EnclosedPorch', 'X3SsnPorch', 'ScreenPorch', 'LowQualFinSF', 'PoolArea')

df.numeric['TotalArea'] <- as.numeric(rowSums(df.combined[,area_cols]))

df.numeric['AreaInside'] <- as.numeric(rowSums(df.combined[,c('X1stFlrSF', 'X2ndFlrSF')]))

# Assesing the age of the house age = 
df.numeric['Age'] <- as.numeric(2010 - df.combined$YearBuilt)
df.numeric['AgeSinceSold'] <- as.numeric(2010 - df.combined$YrSold)

# how many years since the house was remodelled and sold 
df.numeric['YearSinceRemodel'] <- as.numeric(df.combined$YrSold - df.combined$YearRemodAdd)

#Preprocessing the catagorical variables
catagorical_features <- names(df.combined)[!(names(df.combined) %in% numeric_features)]
dummy <- dummyVars(" ~ .", data=df.combined[,catagorical_features])
df.categoric <- data.frame(predict(dummy,newdata=df.combined[,catagorical_features]))

df<- cbind(df.numeric, df.categoric)


### Remove observations that are outliers from the training set
# GrLivingArea
boxplot(train$GrLivArea, main="GrLivArea", boxwex=0.1)
outlier_idx = which(train$GrLivArea>4000)
df <- df[!1:nrow(df) %in% outlier_idx,]
#df <- subset(df, GrLivArea < 4000 )

# LotArea (use if required)
#boxplot(train$LotArea, main="LotArea", boxwex=0.1)
# Check if any test data has above 100000
#summary(test$LotArea)
#df <- subset(df, LotArea < 100000)

# LotFrontage (use if required)
#boxplot(train$LotFrontage, main="LotFrontage", boxwex=0.1)
#summary(test$LotFrontage)
#df <- subset(df, LotFrontage <= 200 )

# Check if NA generated
na.cols <- which(colSums(is.na(df))>0)
na_index <- which(is.na(df[,(na.cols)[1]]))
df <- na.omit(df)

# Normalizing the data
skewed <- apply(df[,names(df.numeric)], 2, skewness)
skewed <- na.omit(skewed)
skewed <- skewed[(skewed > 0.8) | (skewed < -0.8)]

for(col in names(skewed)){
  if(0 %in% df.numeric[, col]) {
    df[, col] <- log(1+df[, col])
  }
  else {
    df[, col] <- log(df[, col])
  }
}

scaler <- preProcess(df[,names(df.numeric)])
df[,names(df.numeric)] <- predict(scaler, df[,names(df.numeric)])


# QQ Plot
qqnorm(train$SalePrice)
qqline(train$SalePrice)

# Dropping unnecessary cols
nzv.data <- nearZeroVar(df, saveMetrics = TRUE)

# take any of the near-zero-variance perdictors
drop_cols <- rownames(nzv.data)[nzv.data$nzv == TRUE]

df <- df[,!names(df) %in% drop_cols]
outlier_idx <- c(outlier_idx, na_index)

###########################################################################################
# Model Fitting
###########################################################################################

y_obs <- train$SalePrice[which(!1:1460 %in% outlier_idx)]
y_train <- log(y_obs + 1)
x_train <- df[1:1455,]
x_test <- df[1456:nrow(df),]

###########################################################################################
# Simple linear model
###########################################################################################
lm_model = lm(y_train ~ ., data = x_train)
y_pred_lm = predict(lm_model, x_train)

lm_RMSE <- Eval_RMSE(y_obs,y_pred_lm)
y_test <- predict(lm_model, x_test)

SalePrice_lm = exp(y_test)

###########################################################################################
# ForwardSelection
###########################################################################################
base_model = lm(y_train ~ ., data = x_train)
small_model = lm(SalePrice ~ 1, data = train)

aic_forward_model = stepAIC(object = small_model, 
                            scope = list(upper = base_model, lower = small_model) , 
                            direction = "forward")

summary(aic_forward_model)

# Predicting on the test data
y_hat_aic_fwd = predict(aic_forward_model, test)
summary(y_hat_aic_fwd )

###
### Make sure you exponentiate the predictions:
###

SalePrice_fs = exp(y_hat_aic_fwd)

###########################################################################################
# Lasso
###########################################################################################

cv_model_ridge <- cv.glmnet(as.matrix(x_train), y_train, alpha = 0, nfolds=10)
lambda_lasso <- cv_model_lasso$lambda.min
lasso_model <- glmnet(x = as.matrix(x_train), y = y_train, alpha = 1, lambda = lambda_lasso)
y_pred.lasso <- as.numeric(predict(lasso_model, as.matrix(x_train)))

# Function to evaluate RMSE
Eval_RMSE <- function(y_obs, y_pred) {
  mse <- mean((y_obs - exp(y_pred)-1)^2)
  return(sqrt(mse))
}

lasso_RMSE <- Eval_RMSE(y_obs,y_pred.lasso)

y_test <- predict(lasso_model, as.matrix(x_test))

summary(exp(y_test))

###
### Make sure you exponentiate the predictions:
###

SalePrice_lasso = exp(y_test)

###########################################################################################
# Ridge
###########################################################################################

cv_model_lasso <- cv.glmnet(as.matrix(x_train), y_train, alpha = 1, nfolds=10)
lambda_ridge <- cv_model_ridge$lambda.min
ridge_model <- glmnet(x = as.matrix(x_train), y = y_train, alpha = 0, lambda = lambda_ridge)
y_pred.ridge <- as.numeric(predict(ridge_model, as.matrix(x_train)))

ridge_RMSE <- Eval_RMSE(y_obs,y_pred.ridge)

y_test <- predict(ridge_model, as.matrix(x_test))

summary(exp(y_test))

###
### Make sure you exponentiate the predictions:
###

SalePrice_ridge = exp(y_test)

###########################################################################################
# ElasticNet
###########################################################################################

cv_model_elnet <- cv.glmnet(data.matrix(x_train), y_train, alpha = 0.08, nfolds=10)
lambda_elnet <- cv_model_elnet$lambda.min
elnet_model <- glmnet(x = as.matrix(x_train), y = y_train, alpha = 0, lambda = lambda_elnet)
y_pred_elnet <- as.numeric(predict(elnet_model, as.matrix(x_train)))

elnet_RMSE <- Eval_RMSE(y_obs,y_pred_elnet)
cbind( lm_RMSE,lasso_RMSE, ridge_RMSE ,elnet_RMSE)

###########################################################################################

alpha.grid <- seq(0,1,length = 10)
lambda.grid <- 10^seq(2,-10, length=100)

trnCntrl <- trainControl(method = "repeatedcv", number = 10, repeats = 5)

searchGrid <- expand.grid(.alpha=alpha.grid, .lambda = lambda.grid)

set.seed(1992)
elnet_train <- train(y_train~.,data=x_train, method="glmnet", tuneGrid = searchGrid,
                     trControl = trnCntrl, standardize = TRUE,maxit = 10)


plot(elnet_train)
attributes(elnet_train)
elnet_train$bestTune

###########################################################################################

y_test <- predict(elnet_model, as.matrix(x_test))

summary(exp(y_test))

###
### Make sure you exponentiate the predictions:
###

SalePrice_elnet = exp(y_test)


################################### XG Boost #########################################

dtrain <- xgb.DMatrix(x_train, label = y_train)
dtest <- xgb.DMatrix(as.matrix(x_test))

cv.ctrl <- trainControl(method = "repeatedcv", repeats = 1,number = 4, 
                        allowParallel=T)

xgb.grid <- expand.grid(nrounds = 750,
                        eta = c(0.01,0.005,0.001),
                        max_depth = c(4,6,8),
                        colsample_bytree=c(0,1,10),
                        min_child_weight = 2,
                        subsample=c(0,0.2,0.4,0.6),
                        gamma=0.01)
set.seed(45)
#xgb_tune <- train(as.matrix(x_train),
#        y_train,
#        method="xgbTree",
#        trControl=cv.ctrl,
#        tuneGrid=xgb.grid,
#        verbose=T,
#        metric="RMSE",
#        nthread =3)

xgb_params <- list(
  booster = 'gbtree',
  objective = 'reg:linear',
  colsample_bytree=1,
  eta=0.005,
  max_depth=4,
  min_child_weight=3,
  alpha=0.3,
  lambda=0.4,
  gamma=0.01, # less overfit
  subsample=0.6,
  seed=5,
  silent=TRUE)

bst <- xgb.train(xgb_params,dtrain, nrounds = 1000)

rmse_eval <- function(y.true, y.pred) {
  mse_eval <- sum((y.true - exp(y.pred)-1)^2) / length(y.true)
  return(sqrt(mse_eval))
}

y_pred.xgb <- predict(bst, dtrain)
Eval_RMSE(y_obs, y_pred.xgb)

y_hat = predict(bst, x_test)
summary(exp(y_hat))
SalePrice_xgboost = exp(y_hat)
