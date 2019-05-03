

library(catboost)
library(mlbench)
library(caret)
library(dplyr)


# load the dataset
data("BostonHousing")

housing <- BostonHousing

# split the dataset into a training and test set
set.seed(26)

split_index <- createDataPartition(housing$medv, p = 0.75, list = FALSE)

test <- housing[-split_index, ]
train <- housing[split_index, ]

y_train <- unlist(train[c('medv')])
X_train <- train %>%
  select(-medv)

y_test <- unlist(test[c('medv')]) 
X_test <- test %>%
  select(-medv)

train_pool <- catboost.load_pool(X_train, label = y_train)
test_pool <- catboost.load_pool(X_test, label = y_test)

params <- list(iterations=500,
               learning_rate=0.01,
               depth=10,
               loss_function='RMSE',
               eval_metric='RMSE',
               random_seed = 55,
               od_type='Iter',
               metric_period = 50,
               od_wait=20,
               use_best_model=TRUE)


model <- catboost.train(learn_pool = train_pool, test_pool = test_pool, params = params)

y_pred <- catboost.predict(model = model, pool = test_pool)

postResample(y_pred,test$medv)  
  
  
  
  
  
