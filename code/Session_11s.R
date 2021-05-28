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

library(keras)

# MNIST dataset consists of 28 x 28 grayscale images of handwritten digits
mnist <- dataset_mnist()
str(mnist)

x_train <- mnist$train$x
y_train <- mnist$train$y
x_test <- mnist$test$x
y_test <- mnist$test$y

table(mnist$train$y, mnist$train$y)

table(mnist$test$y, mnist$test$y)

par(mfrow = c(2,3)) # create 2x3 window to plot images
for (i in 1:6) plot(as.raster(x_train[i,,], max = 255))

hist(x_train[1,,])

# reshape
x_train <- array_reshape(x_train, c(nrow(x_train), 784))
x_test <- array_reshape(x_test, c(nrow(x_test), 784))
# rescale
x_train <- x_train / 255
x_test <- x_test / 255
str(x_train)

# One-hot encoding
y_train <- to_categorical(y_train, 10)
y_test <- to_categorical(y_test, 10)
head(y_train)

# Define model
model <- keras_model_sequential() # specify the model
model %>%
  # first input layer with output units, must specify input shape c(28, 28)
  # the default activation is linear f(x) = x, we use ReLu function
  layer_dense(units = 256, activation = 'relu', input_shape = c(784)) %>%
  # fraction of input units to drop at each update during training time
  # help to prevent overfitting
  layer_dropout(rate = 0.4) %>%
  # one more hidden layer
  layer_dense(units = 128, activation = 'relu') %>%
  layer_dropout(rate = 0.3) %>%
  # the output layer 10 numeric vectors
  # softmax function is to normalize numeric vectors into probabilities vectors
  layer_dense(units = 10, activation = 'softmax')
# how many layers in this model?

summary (model)
# 200960 = 784 * 256 + 256

# Compile model
# optimizer: https://keras.io/optimizers/
# loss: https://keras.io/losses/
# metrics: https://keras.io/metrics/
model %>% compile(
  loss = 'categorical_crossentropy', # for multicategorical probalems
  optimizer = optimizer_rmsprop(), # optimizer for RecurrentNN
  metrics = c('accuracy') # prediction accuracy
)

# train the whole data in batches of 128 images, repeat for 30 times/epochs
history <- model %>% fit(
  x_train, y_train, 
  epochs = 30, batch_size = 128, 
  validation_split = 0.2 # % of training data to be used as validation data
)

plot(history)

model %>% evaluate(x_test, y_test)
pred <- model %>% predict_classes(x_test)
pred[1:100]

table(Predicted = pred, Actual = mnist$test$y)

prob <- model %>% predict_proba(x_test)
cbind(prob, Predicted_class = pred, Actual = mnist$test$y)[1:5, ]

# The dataset is from https://www.kaggle.com/mlg-ulb/creditcardfraud/data
df <- read.csv("../../Data/Session_11_creditcard.csv")
str(df)

df %>% ggplot(aes(x = Time, y = Amount)) + geom_point() + facet_grid(Class ~ .)

# Look at smaller transactions
df$Class <- as.factor(df$Class)
df %>% filter(Amount<300) %>% ggplot(aes(x = Class, y = Amount)) + geom_violin()

# Split the sample into train (90%) and test (10%)
# sample() to take a sample of the specified size
index <- sample(nrow(df), size = 0.1 * nrow(df))
train <- df[-index, ]
test <- df[index, ]

y_train <- train$Class
y_test <- test$Class

# Remove the Time and Class columns for x
x_train <- train %>% select(-c("Time", "Class"))
x_test <- test %>% select(-c("Time", "Class"))

# Coerce the dataframe to matrix to perform the training
# The hierarchy for coercion is: logical < integer < numeric < character
# as.matrix() https://rdrr.io/cran/data.table/man/as.matrix.html
x_train <- as.matrix(x_train)
x_test <- as.matrix(x_test)

# Autoencoder with the keras Functional API
# https://keras.rstudio.com/articles/functional_api.html
# Define parameters
input_dim <- 29 # 29 predictors/features
outer_layer_dim <- 14
inner_layer_dim <- 7
# input layer
input_layer <- layer_input(shape = c(input_dim))
# outputs compose input + dense layers
encoder <- input_layer %>%
  layer_dense(units = outer_layer_dim, activation = "relu") %>%
  layer_dense(units = inner_layer_dim, activation = "relu")
decoder <- encoder %>%
  layer_dense(units = inner_layer_dim) %>%
  layer_dense(units = outer_layer_dim) %>%
  layer_dense(units = input_dim)

# create model
autoencoder <- keras_model(inputs = input_layer, outputs = decoder)
autoencoder

# compile model
# optimizer: https://keras.io/optimizers/
# loss: https://keras.io/losses/
# metrics: https://keras.io/metrics/
autoencoder %>% compile(
  optimizer = 'adam',
  loss = 'mean_squared_error',
  metrics = c('accuracy')
)

# to save time, set 10 epochs only
history <- autoencoder %>% fit(x_train, x_train,  epochs = 10,
                               batch_size = 32, validation_split = 0.2)
plot(history)

# Reconstruct on the test set
preds <- autoencoder %>% predict(x_test)
preds <- as.data.frame(preds)

# predict based on reconstruction error
# for this case, fraud is predicted when sum of reconstruction error is >=30
# the threshold is not set in stone, you may try to vary it
y_preds <- ifelse(rowSums((preds - x_test)^2) / 30 < 1,
                  rowSums((preds - x_test)^2) / 30, 1)

library(ROCR)
pred <- prediction(y_preds, y_test)
perf <- performance(pred, measure = "tpr", x.measure = "fpr") 
plot(perf, col = rainbow(10))
auc <- performance(pred, measure = "auc")
auc <- auc@y.values[[1]]
names(auc) <- "AUC of the autoencoder model"
auc

# You may also try the autoencoder using the H2O package
# To initiate h2o
library(h2o)
h2o.init()

# convert to h2o format
# autoencoder is unsupervised algo, no need Y data
x_train_h2o <- as.h2o(x_train)
x_test_h2o <- as.h2o(x_test)

# setup the autoencoder using h2o.deeplearning
# https://docs.h2o.ai/h2o/latest-stable/h2o-docs/data-science/deep-learning.html
h2o_autoencoder <- h2o.deeplearning(x = 1:29,
                                    training_frame = x_train_h2o,
                                    model_id = "H2O_Autoencoder",
                                    autoencoder = TRUE,
                                    ignore_const_cols = FALSE,
                                    seed = 2021,
                                    hidden = c(14, 7, 7, 14),
                                    epochs = 10,
                                    activation ="Tanh")
# Take a look at the setup
h2o_autoencoder

# return an H2OFrame object containing
# the reconstruction MSE or the per-feature squared error
# https://www.rdocumentation.org/packages/h2o/versions/3.32.0.1/topics/h2o.anomaly
h2o_anomaly_train <- h2o.anomaly(h2o_autoencoder, x_train_h2o) %>%
  as.data.frame()

# An indicator of training data
h2o_anomaly_train$test <- 0

# Take a look at the output
head(h2o_anomaly_train)

# choose 0.1% quantile as anomaly threshold
threshold <- quantile(h2o_anomaly_train$Reconstruction.MSE, probs = 0.999)

# predict on the test set
h2o_anomaly_test <- h2o.anomaly(h2o_autoencoder, x_test_h2o) %>%
  as.data.frame()

h2o_anomaly_test$test <- 1

# combine the training and test predictions (MSE)
results <- data.frame(rbind(h2o_anomaly_train, h2o_anomaly_test), threshold)
head(results)

# plot with log transformation MSE
# log is to have a better visual of Y axis values
results %>% ggplot(aes(x = 1:nrow(results),
                       y = log10(Reconstruction.MSE),
                       color = factor(test))) +
  geom_point() +
  geom_hline(yintercept = log10(threshold),
             linetype = "dashed",
             color = "blue") +
  ylab("Log-transformed Reconstruction MSE") +
  xlab("Observations") +
  ggtitle("Anomaly Detection Results")