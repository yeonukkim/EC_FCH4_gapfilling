# install required libraries
#install.packages('tidyverse') # for data wrangling 
#install.packages('caret') # for machine learning run and tuning
#install.packages('randomForest') # randomforest model

# load libraries
library(tidyverse)
library(caret)

# load dataset
# df includes FCH4 and related variables.
# FCH4 should be quality controlled. Other variables should be fully gap-filled.
df <- read.csv("dataset.csv") 

# variable we need for FCH4 gap-filling
predictors <- c("FCH4", "Ustar","NEE","LE","H","Rg","Tair","Tsoil","rH","VPD","WTH","Pa","LWOUT","s","c","Day") # you can modify the inputs
ML.df <- df %>% select(predictors)

# period when FCH4 is not missing
wm_only <- ML.df[!is.na(ML.df$FCH4), ]

# 75% of data used for model tuning/validation
index <- createDataPartition(wm_only$FCH4, p=0.75, list=F) 
train_set <- wm_only[index,]
test_set <- wm_only[-index,]

############### Random forest run

#### option 1. random forest model with mtry tuning
#Add parallel processing for the fast processing if you want to
library(parallel)
library(doParallel)
cluster <- makeCluster(6)
registerDoParallel(cluster)
RF_FCH4 <- train(FCH4 ~ ., data = train_set[,predictors],
								 method = "rf",
								 preProcess = c("medianImpute"),                #impute missing met data with median
								 trControl=trainControl(method = "repeatedcv",   #three-fold cross-validation for model parameters 3 times
								 											number = 3,                #other option: "cv" without repetition
								 											repeats = 3),
								 na.action = na.pass,
								 allowParallel=TRUE, # This requires parallel packages. Otherwise you can choose FALSE.
								 ntree=400, # can generate more trees
								 importance = TRUE)


#### option 2. random forest model without tuning. 
# (when mtry value is already tunned or using squre root of the number of predictor)
RF_FCH4 <- train(FCH4 ~ ., data = train_set[,predictors],
								 method = "rf",
								 preProcess = c("medianImpute"),  # impute missing met data with median (but it will not be applied since we used gap-filled predictors)
								 trControl = trainControl(method = "none"),
								 tuneGrid=data.frame(mtry=8), # use known mtry value. (or squre root of the number of predictors)
								 na.action = na.pass,
								 allowParallel=FALSE, 
								 ntree=400, # can generate more trees
								 importance = TRUE)


############### Results
# variable importance
plot(varImp(RF_FCH4, scale = FALSE), main="variable importance")

#generate FCH4_rf predictions for testset
test_set$FCH4_rf <- predict(RF_FCH4, test_set)
regrRF <- lm(test_set$FCH4_rf ~ test_set$FCH4); 
print(summary(regrRF))
ggplot(test_set, aes(x=FCH4, y=FCH4_rf)) + geom_abline(slope = 1, intercept = 0)+
	geom_point() + geom_smooth(method = "lm") + ggtitle("testset")

# save FCH4_rf
result <- data.frame(FCH4 = ML.df$FCH4) # you can add datetime column here if you want to.
result$FCH4_RF_model <- predict(RF_FCH4, ML.df) # FCH4 RF model
result$FCH4_RF_filled <- ifelse(is.na(result$FCH4),result$FCH4_RF_model,result$FCH4) # gap-filled column (true value when it is, gap-filled value when missing)
result$FCH4_RF_residual <- ifelse(is.na(result$FCH4),NA,result$FCH4_RF_model - result$FCH4) # residual (model - obs). can be used for random uncertainty analysis


# write result
write.csv(result,"rf_FCH4_gapfilling.csv",row.names = F)

