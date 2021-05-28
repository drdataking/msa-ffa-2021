# ----------------------------------------------------------------------------
# The R code file is for Forecasting and Forensic Analytics course at SMU
# taught by Prof Wang Jiwei (jwwang@smu.edu.sg) in the
# MSc in Accounting (Data & Analytics) program (www.smu.edu.sg/msa).
# You may share the code with people outside of the SMU community, but
# you are prohibited by law from sharing the data with people outside of SMU.
# ----------------------------------------------------------------------------

# Define html_df() function for displaying small tables in html format
libs <- c("knitr", "kableExtra")  # table styling
invisible(lapply(libs, library, character.only = TRUE))
html_df <- function(text, cols = NULL, col1 = FALSE, full = FALSE) {
  if(!length(cols)) {
    cols = colnames(text)
  }
  if(!col1) {
    kable(text, "html", col.names = cols, align = c("l", rep('c', length(cols)-1))) %>%
      kable_styling(bootstrap_options = c("striped", "hover"), full_width = full)
  } else {
    kable(text, "html", col.names = cols, align = c("l", rep('c', length(cols) - 1))) %>%
      kable_styling(bootstrap_options = c("striped", "hover"), full_width = full) %>%
      column_spec(1, bold = TRUE)
  }
}

library(tidyverse)

library(caret); set.seed(123)
data <- read.csv('../../Data/Session_10_ensemble.csv')
str(data)

# Imputing missing values using median and normalize the data
# by substracting the mean and divided by standard deviation
preProcValues <- preProcess(data, method = c("medianImpute","center","scale"))
data_processed <- predict(preProcValues, data)
sum(is.na(data_processed))

#Spliting dataset into train and test based on outcome: 75% and 25%
#List=FALSE returns an integer matrix which could be used for index
#Stratified sampling: randomly draw p% from each group of Loan_Status)
index <- createDataPartition(data_processed$Loan_Status, p=0.75, list=FALSE)
trainSet <- data_processed[ index, ]
testSet  <- data_processed[-index, ]

#Defining the training controls for multiple models
fitControl <- trainControl(
  method = "cv", # cross-validation
  number = 5, # k-fold
  savePredictions = 'final', # save the predictions for the optimal parameters
  classProbs = T) # probabilities be computed for classification models

#Defining the predictors and outcome
predictors <- c("Credit_History", "LoanAmount", "Loan_Amount_Term",
                "ApplicantIncome", "CoapplicantIncome")
outcomeName <- "Loan_Status"

# Training the random forest model
# The tuneLength specifies default values for the main parameter
# the main para for RF is the number of random features
model_rf <- train(trainSet[, predictors], trainSet[, outcomeName], method='rf',
                  trControl = fitControl, tuneLength = 3)

#Predicting using random forest model
testSet$pred_rf <- predict(object = model_rf, testSet[, predictors])

#Checking the accuracy of the random forest model
confusionMatrix(factor(testSet$Loan_Status), testSet$pred_rf)



# Training the knn model
# the main para for kNN is k
model_knn <- train(trainSet[, predictors], trainSet[, outcomeName],
                   method = 'knn', trControl = fitControl, tuneLength = 3)

#Predicting using knn model
testSet$pred_knn <- predict(object = model_knn, testSet[, predictors])

#Checking the accuracy of the knn model
confusionMatrix(factor(testSet$Loan_Status), testSet$pred_knn)

# Training the Logistic regression model
# The tuneLength is no applicable for logit
model_lr <- train(trainSet[, predictors], trainSet[, outcomeName],
                  method = 'glm', trControl = fitControl)

#Predicting using logit model
testSet$pred_lr <- predict(object = model_lr, testSet[, predictors])

#Checking the accuracy of the logit model
confusionMatrix(factor(testSet$Loan_Status), testSet$pred_lr)

#Predicting the probabilities
testSet$pred_rf_prob <- predict(object = model_rf, testSet[, predictors],
                                type = 'prob')
testSet$pred_knn_prob <- predict(object = model_knn, testSet[, predictors],
                                 type = 'prob')
testSet$pred_lr_prob <- predict(object = model_lr, testSet[, predictors],
                                type = 'prob')

#Taking average of predictions
testSet$pred_avg <- (testSet$pred_rf_prob$Y + testSet$pred_knn_prob$Y +
                     testSet$pred_lr_prob$Y) / 3

#Splitting into binary classes at 0.5
testSet$pred_avg <- as.factor(ifelse(testSet$pred_avg > 0.5, 'Y', 'N'))

confusionMatrix(factor(testSet$Loan_Status), testSet$pred_avg)

#Taking weighted average of predictions
testSet$pred_weighted_avg <- 
  (testSet$pred_rf_prob$Y * 0.25) + (testSet$pred_knn_prob$Y * 0.25) +
  (testSet$pred_lr_prob$Y * 0.5)

#Splitting into binary classes at 0.5
testSet$pred_weighted_avg <- 
  as.factor(ifelse(testSet$pred_weighted_avg > 0.5, 'Y', 'N'))

confusionMatrix(factor(testSet$Loan_Status), testSet$pred_weighted_avg)

#The majority vote
testSet$pred_majority <- 
  as.factor(ifelse(testSet$pred_rf == 'Y' & testSet$pred_knn== 'Y', 'Y',
  ifelse(testSet$pred_rf == 'Y' & testSet$pred_lr == 'Y', 'Y',
  ifelse(testSet$pred_knn =='Y' & testSet$pred_lr == 'Y', 'Y', 'N'))))

confusionMatrix(factor(testSet$Loan_Status), testSet$pred_majority)

#Correlation matrix for the 3 base model predictions
testSet$pred_rf1 <- as.numeric(ifelse(testSet$pred_rf == 'Y', 1, 0))
testSet$pred_knn1 <- as.numeric(ifelse(testSet$pred_knn == 'Y', 1, 0))
testSet$pred_lr1 <- as.numeric(ifelse(testSet$pred_lr == 'Y', 1, 0))
cor(testSet[ , c("pred_rf1", "pred_knn1", "pred_lr1")])

#Get the predicted outcome for training data
trainSet$pred_rf_prob <- model_rf$pred$Y[order(model_rf$pred$rowIndex)]
trainSet$pred_knn_prob <- model_knn$pred$Y[order(model_knn$pred$rowIndex)]
trainSet$pred_lr_prob <- model_lr$pred$Y[order(model_lr$pred$rowIndex)]

#Predicting probabilities for the test data
testSet$pred_rf_prob <- predict(model_rf, testSet[, predictors], type='prob')$Y
testSet$pred_knn_prob <- predict(model_knn, testSet[, predictors], type='prob')$Y
testSet$pred_lr_prob <- predict(model_lr, testSet[, predictors], type='prob')$Y

#Predictors for the top layer model
predictors_top<-c('pred_rf_prob', 'pred_knn_prob', 'pred_lr_prob')

#Stochastic Gradient Boosting Model (GBM) as the top layer model
model_gbm <- train(trainSet[, predictors_top], trainSet[, outcomeName],
                   method='gbm', trControl = fitControl)

#Predicting using GBM model
testSet$pred_gbm_ens <- predict(model_gbm, testSet[, predictors_top])

#Checking the accuracy of the GBM model
confusionMatrix(factor(testSet$Loan_Status), testSet$pred_gbm_ens)

df <- readRDS('../../Data/Session_10_models.rds')
head(df) %>% select(-pred_F, -pred_S) %>% slice(1:2) %>% html_df()

library(xgboost)

# Prep data
# -1 to remove the first column: Test or AAER
train_x <- model.matrix(AAER ~ ., data = df[df$Test == 0, -1])[, -1]
train_y <- model.frame(AAER ~ ., data = df[df$Test == 0, ])[, "AAER"]
test_x <- model.matrix(AAER ~ ., data = df[df$Test == 1, -1])[, -1]
test_y <- model.frame(AAER ~ ., data = df[df$Test == 1, ])[, "AAER"]

set.seed(468435)  #for reproducibility
xgbCV <- xgb.cv(max_depth = 5, eta = 0.10, gamma = 5, min_child_weight = 4,
                subsample = 0.57, objective = "binary:logistic", data = train_x,
                label = train_y, nrounds = 100, eval_metric = "auc", nfold = 10,
                stratified = TRUE, verbosity = 0)

fit_ens <- xgboost(params = xgbCV$params, data = train_x, label = train_y,
                   nrounds = which.max(xgbCV$evaluation_log$test_auc_mean))

pred_ens <- predict(fit_ens, test_x, type = "response")

library(ROCR)
ROCpred.ens <- prediction(as.numeric(pred_ens), as.numeric(test_y))
ROCperf.ens <- performance(ROCpred.ens, 'tpr','fpr')
df_ROC.ens <- data.frame(FalsePositive = c(ROCperf.ens@x.values[[1]]),
                          TruePositive = c(ROCperf.ens@y.values[[1]]))
auc.ens <- performance(ROCpred.ens, measure = "auc")

ROCpred.BCE <- prediction(as.numeric(df[df$Test == 1, ]$pred_BCE), as.numeric(test_y))
ROCperf.BCE <- performance(ROCpred.BCE, 'tpr','fpr')
df_ROC.BCE <- data.frame(FalsePositive = c(ROCperf.BCE@x.values[[1]]),
                          TruePositive = c(ROCperf.BCE@y.values[[1]]))
auc.BCE <- performance(ROCpred.BCE, measure = "auc")

ROCpred.lmin <- prediction(as.numeric(df[df$Test == 1, ]$pred_lmin), as.numeric(test_y))
ROCperf.lmin <- performance(ROCpred.lmin, 'tpr','fpr')
df_ROC.lmin <- data.frame(FalsePositive = c(ROCperf.lmin@x.values[[1]]),
                           TruePositive = c(ROCperf.lmin@y.values[[1]]))
auc.lmin <- performance(ROCpred.lmin, measure = "auc")

ROCpred.xgb <- prediction(as.numeric(df[df$Test == 1, ]$pred_xgb), as.numeric(test_y))
ROCperf.xgb <- performance(ROCpred.xgb, 'tpr','fpr')
df_ROC.xgb <- data.frame(FalsePositive = c(ROCperf.xgb@x.values[[1]]),
                          TruePositive = c(ROCperf.xgb@y.values[[1]]))
ROCperf.xgb <- performance(ROCpred.xgb, 'tpr','fpr')
auc.xgb <- performance(ROCpred.xgb, measure = "auc")

ggplot() +
  geom_line(data=df_ROC.ens, aes(x = FalsePositive, y = TruePositive, color = "Ensemble")) +
  geom_line(data=df_ROC.BCE, aes(x = FalsePositive, y = TruePositive, color = "Logit (BCE)")) + 
  geom_line(data=df_ROC.lmin, aes(x = FalsePositive, y = TruePositive, color = "Lasso (lambda.min)")) + 
  geom_line(data=df_ROC.xgb, aes(x = FalsePositive, y = TruePositive, color = "XGBoost")) + 
  geom_abline(slope=1) + ggtitle("ROC Curves across models")

aucs <- c(auc.ens@y.values[[1]], auc.BCE@y.values[[1]], auc.lmin@y.values[[1]], auc.xgb@y.values[[1]])
names(aucs) <- c("Ensemble", "Logit (BCE)", "Lasso (lambda.min)", "XGBoost")

aucs  # Out of sample

xgb.train.data = xgb.DMatrix(train_x, label = train_y, missing = NA)
col_names = attr(xgb.train.data, ".Dimnames")[[2]]
imp = xgb.importance(col_names, fit_ens)
# Variable importance
xgb.plot.importance(imp)


# The easiest way is to install h2o within RStudio through CRAN
# But the CRAN version is typically one version behind the most recent version
# It is fine and you may ignore the warning messages about Java and H2o cluster
# install.packages("h2o")

# If you want to install the most recent version of H2O AutoML
# Go to http://h2o-release.s3.amazonaws.com/h2o/rel-zermelo/4/index.html 
# and click "INSTALL IN R" tab

library(h2o)
h2o.init()

# use data.table package to handle large files
# you may need to install "bit64" package to activate data.table
options("h2o.use.data.table" = TRUE)
# Import the loan .csv data into H2O
# You may also convert an R dataframe to h2o format directly
data.h2o <- h2o.importFile("../../Data/Session_10_ensemble.csv")
summary(data.h2o)

data.h2o <- data.h2o[, c("Credit_History", "LoanAmount", "Loan_Amount_Term",
                         "ApplicantIncome", "CoapplicantIncome", "Loan_Status")]
# Construct test and train sets using sampling
h2o.impute(data.h2o, method = "median")
data.h2o.split = h2o.splitFrame(data = data.h2o, ratios = 0.75)
data.h2o.train = data.h2o.split[[1]]
data.h2o.test = data.h2o.split[[2]]

# Identify predictors and response
y <- "Loan_Status"
x <- setdiff(names(data.h2o.train), y)

# For binary classification, response should be a factor
data.h2o.train[, y] <- as.factor(data.h2o.train[, y])
data.h2o.test[, y] <- as.factor(data.h2o.test[, y])

# Run AutoML for 20 models (limited to 1 hour max_runtime_secs by default)
# use include_algos and exclude_algos to choose models.
# It will take some time, you may reduce the max_models number to save time
aml <- h2o.automl(x = x, y = y, training_frame = data.h2o.train,
                  max_models = 20, max_runtime_secs = 120,
                  #exclude_algos = c(""), include_algos = c(""),
                  seed = 123) # Set a seed for reproducibility

# View the AutoML Leaderboard
DT::datatable(as.data.frame(aml@leaderboard[, c("model_id", "auc")]),
              options = list(pageLength = 3))
# Make predictions using the best model @leader
# pred <- h2o.predict(aml@leader, data.h2o.test)


# Try the fraud detection data with H2O
# Convert the detection model data into H2O format
data.h2o <- as.h2o(df)
summary(data.h2o)

# split the data based on Test indicator
data.h2o.train = data.h2o[data.h2o[, "Test"] == 0, ]
data.h2o.test = data.h2o[data.h2o[, "Test"] == 1, ]

# Identify predictors and response
y <- "AAER"
x <- c("pred_F", "pred_S", "pred_FS", "pred_BCE", "pred_lmin",
       "pred_l1se", "pred_xgb")

# For binary classification, response should be a factor
data.h2o.train[, y] <- as.factor(data.h2o.train[, y])
data.h2o.test[, y] <- as.factor(data.h2o.test[, y])

# Run AutoML for 20 models (limited to 1 hour max_runtime_secs by default)
# use include_algos and exclude_algos to choose models.
# It will take some time, you may reduce the max_models number to save time
aml <- h2o.automl(x = x, y = y, training_frame = data.h2o.train,
                  max_models = 20, max_runtime_secs = 120,
                  #exclude_algos = c(""), include_algos = c(""),
                  seed = 123) # Set a seed for reproducibility

# View the AutoML Leaderboard
DT::datatable(as.data.frame(aml@leaderboard[, c("model_id", "auc")]),
              options = list(pageLength = 3))

## # install the reticulate package directly
## # install.packages("reticulate")
## 
## # launch the package
## library(reticulate)
## 
## # specify the Python version to use
## # https://rstudio.github.io/reticulate/articles/versions.html
## # I assume you use the Anaconda which is easier to manage
## use_python("C:\\ProgramData\\Anaconda3\\")

## # This is a python code, you can only run it directly in RMarkdown

## # If you want to run it together with your R session, please visit

## # https://github.com/rstudio/reticulate for more information

## import pandas as pd

## from tpot import TPOTClassifier

## from sklearn.impute import SimpleImputer

## from sklearn.model_selection import train_test_split

## # Read the data

## tpot_data = pd.read_csv('../../Data/Session_10_ensemble.csv')

## tpot_data = tpot_data[["Credit_History", "LoanAmount", "Loan_Amount_Term",

##                 "ApplicantIncome", "CoapplicantIncome", "Loan_Status"]]

## # Create a dataframe with all features (ie, all X)

## features = tpot_data.drop('Loan_Status', axis = 1) # drop column axis = 1

## # Create training and test datasets

## X_train, X_test, y_train, y_test = train_test_split(features,

##                                                     tpot_data['Loan_Status'],

##                                                     train_size = 0.75,

##                                                     test_size = 0.25)


## # This is a python code, you can only run it directly in RMarkdown

## # If you want to run it together with your R session, please visit

## # https://github.com/rstudio/reticulate for more information

## 
## # Setup TPOT optimizer parameters

## # generations: number of iterations, population_size: number of models

## # cv: k-fold cross validation; random_state: for reproducibility

## # verbosity: How much information TPOT communicates while it is running

## pipeline_optimizer = TPOTClassifier(generations = 5,

##                                     population_size = 20,

##                                     cv = 5,

##                                     random_state = 123,

##                                     verbosity = 0)

## # Fit the models

## pipeline_optimizer.fit(X_train, y_train)

## # evaluate and print out the testing set

## print(pipeline_optimizer.score(X_test, y_test))

## # Output the best model Python code

## pipeline_optimizer.export('tpot_exported_pipeline.py')

