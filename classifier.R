# classifier
require(xgboost)
require(caret)
# specify train and test set
# data(agaricus.train, package='xgboost')
# data(agaricus.test, package='xgboost')
# train <- agaricus.train
# test <- agaricus.test


# Input: dataset, the column to be predicted, and the number of round

get.gini <- function(dataset, dp.var, i){
  # grep dependent variable index
  outcome.ind <- grep(dp.var, colnames(dataset))
  
  # split dataset
  set.seed(1235)
  inTrain <- createDataPartition(y=dataset[,outcome.ind], p=0.75, list = FALSE)
  training <- train[inTrain,]
  testing <- train[-inTrain,]
  
  # More complex the relationship between your features and your label is, more passes you need
  bst <- xgboost(data = as.matrix(dataset[,-outcome.ind]), 
                 label = dataset[,outcome.ind], 
                 max.depth = 2, # tree depth
                 eta = 1, 
                 nthread = 3, #cpu
                 nround = i, # two passes of data
                 objective = "binary:logistic",
                 verbose = 0)
  
  
  pred <- predict(bst, as.matrix(testing[,-outcome.ind]))
  head(pred)
  
  #err <- mean(as.numeric(pred > 0.5) != testing$target)
  cat(paste("test-error=", err))
  
  norm.gini <- normalizedGini(testing$target, pred)
  return(list(norm.gini, bst))
}

for (i in c(70, 100, 150)){
  print(get.gini(train, "target", 70)[[1]])
}

bst.model <- get.gini(train, "target", 70)
