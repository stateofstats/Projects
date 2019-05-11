
library(dplyr)
library(catboost)
library(caret)

# load the data
data_2019 <-read.csv("2019_clean.csv", stringsAsFactors = FALSE, header = TRUE)

data_2018 <-read.csv("2018_clean.csv", stringsAsFactors = FALSE, header = TRUE)

data <- rbind(data_2018, data_2019)

data %>%
  arrange(home_team)

dim(data)

# set seed for randomization
set.seed(26)

# split the data into training and test sets
validation_index <- createDataPartition(data$home_win, p = 0.75, list = FALSE)

validation <- data[-validation_index, ]
dataset <- data[validation_index, ]

# remove basic info that is irrelevant to model
valid_info <- validation %>%
  select(c('date', 'id', 'home_team', 'home_score',
           'away_team', 'away_score'))

dataset_info <- dataset %>%
  select(c('date', 'id', 'home_team', 'home_score',
           'away_team', 'away_score'))

# define the test (validation) and training (dataset) sets
validation <- validation %>%
  select(-c('date', 'id', 'home_team', 'home_score',
            'away_team', 'away_score'))

dataset <- dataset %>%
  select(-c('date', 'id', 'home_team', 'home_score',
            'away_team', 'away_score'))

dim(validation)  
dim(dataset)  

dim(valid_info)
dim(dataset_info)


# split the targets from the variables
y_train <- unlist(dataset[c('home_win')])
X_train <- dataset %>% select(-'home_win')

y_valid <- unlist(validation[c('home_win')])
X_valid <- validation %>% select(-'home_win')

# create the catboost pools
train_pool <- catboost.load_pool(data = X_train, label = y_train)
test_pool <- catboost.load_pool(data = X_valid, label = y_valid)

# set model parameters - START WITH ITERATIONS AT 500
params <- list(iterations=300,
               learning_rate=0.01,
               depth=10,
               loss_function='Logloss',
               eval_metric='Logloss',
               random_seed = 55,
               od_type='Iter',
               metric_period = 50,
               od_wait=20,
               use_best_model=TRUE,
               task_type = 'GPU')

# train the model
model <- catboost.train(learn_pool = train_pool, test_pool = test_pool, params = params)

# get feature importance
feature_imp <- catboost.get_feature_importance(model, train_pool)

# view features ranked by importance
data.frame(feature = rownames(as.data.frame(as.matrix(feature_imp))),
           importance = as.matrix(feature_imp)) %>%
  arrange(desc(importance))

# predict the test set results
y_pred <- catboost.predict(model, test_pool, prediction_type = "Probability")

range(y_pred)

# set range for confusion matrix accuracy
cutoff <- 0.50

preds_test <- ifelse(y_pred > cutoff, 1, 0)

str(preds_test)

confusionMatrix(as.factor(preds_test), as.factor(y_valid))

# find log loss
logLoss <- function(pred, actual) {
  -1*mean(log(pred[model.matrix(~ actual + 0) - pred > 0]))
}

logLoss(y_pred, y_valid)



