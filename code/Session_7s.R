# ----------------------------------------------------------------------------
# The R code file is for Forecasting and Forensic Analytics course at SMU
# taught by Prof Wang Jiwei (jwwang@smu.edu.sg) in the
# MSc in Accounting (Data & Analytics) program (www.smu.edu.sg/msa).
# You may share the code with people outside of the SMU community, but
# you are prohibited by law from sharing the data with people outside of SMU.
# ----------------------------------------------------------------------------

# Define html_df() function for displaying small tables in html format
library(knitr)
library(kableExtra)
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
library(broom)
library(coefplot)

df <- read.csv("../../Data/Session_7.csv")

df %>%
  group_by(year) %>%
  mutate(total_AAERS = sum(AAER), total_observations = n(),
         percent = scales::percent(total_AAERS/total_observations,
                                   accuracy = 0.01)) %>%
  slice(1) %>%  ungroup() %>%
  select(year, total_AAERS, total_observations, percent) %>% html_df

fit_1990s <- glm(AAER ~ ebit + ni_revt + ni_at + log_lt + ltl_at + lt_seq +
                   lt_at + act_lct + aq_lct + wcap_at + invt_revt + invt_at +
                   ni_ppent + rect_revt + revt_at + d_revt + b_rect + b_rect +
                   r_gp + b_gp + gp_at + revt_m_gp + ch_at + log_at +
                   ppent_at + wcap,
                 data = df[df$Test == 0, ], #Test=0 is the training data
                 family = binomial)
tidy(fit_1990s)

library(ROCR)
# Have to remove all NA in prediction() with ROCR 1.0-11 version
# You may install the older version which does not require so
# https://cran.r-project.org/src/contrib/Archive/ROCR/ROCR_1.0-7.tar.gz
pred <- predict(fit_1990s, df, type="response")
ROCpred <- prediction(as.numeric(pred[df$Test == 0 & !is.na(pred)]),
                      as.numeric(df[df$Test == 0 & !is.na(pred), ]$AAER))
ROCpred_out <- prediction(as.numeric(pred[df$Test == 1 & !is.na(pred)]),
                          as.numeric(df[df$Test==1 & !is.na(pred), ]$AAER))
ROCperf <- performance(ROCpred, 'tpr', 'fpr')
ROCperf_out <- performance(ROCpred_out, 'tpr', 'fpr')
df_ROC_1990s <- data.frame(FalsePositive = c(ROCperf@x.values[[1]]),
                           TruePositive = c(ROCperf@y.values[[1]]))
df_ROC_out_1990s <- data.frame(FalsePositive = c(ROCperf_out@x.values[[1]]),
                               TruePositive = c(ROCperf_out@y.values[[1]]))

ggplot() +
  geom_line(data = df_ROC_1990s, aes(x = FalsePositive, y = TruePositive,
                                     color = "In Sample")) +
  geom_line(data = df_ROC_out_1990s, aes(x = FalsePositive, y = TruePositive,
                                         color = "Out of Sample")) + 
  geom_abline(slope = 1)

auc <- performance(ROCpred, measure = "auc")
auc_out <- performance(ROCpred_out, measure = "auc")
aucs_1990s <- c(auc@y.values[[1]], auc_out@y.values[[1]])
names(aucs_1990s) <- c("In sample AUC", "Out of sample AUC")
aucs_1990s

fit_2011 <- glm(AAER ~ logtotasset + rsst_acc + chg_recv + chg_inv +
                  soft_assets + pct_chg_cashsales + chg_roa + issuance +
                  oplease_dum + book_mkt + lag_sdvol + merger + bigNaudit +
                  midNaudit + cffin + exfin + restruct,
                data = df[df$Test == 0, ],  family = binomial)
tidy(fit_2011)

library(ROCR)
pred <- predict(fit_2011, df, type = "response")
ROCpred <- prediction(as.numeric(pred[df$Test == 0 & !is.na(pred)]),
                      as.numeric(df[df$Test==0 & !is.na(pred), ]$AAER))
ROCpred_out <- prediction(as.numeric(pred[df$Test == 1 & !is.na(pred)]),
                          as.numeric(df[df$Test == 1 & !is.na(pred), ]$AAER))
ROCperf <- performance(ROCpred, 'tpr', 'fpr')
ROCperf_out <- performance(ROCpred_out, 'tpr', 'fpr')
df_ROC_2011 <- data.frame(FalsePositive = c(ROCperf@x.values[[1]]),
                          TruePositive=c(ROCperf@y.values[[1]]))
df_ROC_out_2011 <- data.frame(FalsePositive = c(ROCperf_out@x.values[[1]]),
                              TruePositive = c(ROCperf_out@y.values[[1]]))

ggplot() +
  geom_line(data = df_ROC_2011, aes(x = FalsePositive, y = TruePositive,
                                    color = "In Sample")) +
  geom_line(data = df_ROC_out_2011, aes(x = FalsePositive, y = TruePositive,
                                        color = "Out of Sample")) + 
  geom_abline(slope = 1)

auc <- performance(ROCpred, measure = "auc")
auc_out <- performance(ROCpred_out, measure = "auc")
aucs_2011 <- c(auc@y.values[[1]], auc_out@y.values[[1]])
names(aucs_2011) <- c("In sample AUC", "Out of sample AUC")
aucs_2011

fit_2000s <- glm(AAER ~ bullets + headerlen + newlines + alltags +
                   processedsize + sentlen_u + wordlen_s + paralen_s +
                   repetitious_p + sentlen_s + typetoken + clindex + fog +
                   active_p + passive_p + lm_negative_p + lm_positive_p +
                   allcaps + exclamationpoints + questionmarks,
                 data=df[df$Test == 0, ], family = binomial)
tidy(fit_2000s)

library(ROCR)
pred <- predict(fit_2000s, df, type = "response")
ROCpred <- prediction(as.numeric(pred[df$Test == 0 & !is.na(pred)]),
                      as.numeric(df[df$Test == 0 & !is.na(pred), ]$AAER))
ROCpred_out <- prediction(as.numeric(pred[df$Test == 1 & !is.na(pred)]),
                          as.numeric(df[df$Test == 1 & !is.na(pred), ]$AAER))
ROCperf <- performance(ROCpred, 'tpr', 'fpr')
ROCperf_out <- performance(ROCpred_out, 'tpr', 'fpr')
df_ROC_2000s <- data.frame(FalsePositive = c(ROCperf@x.values[[1]]),
                           TruePositive = c(ROCperf@y.values[[1]]))
df_ROC_out_2000s <- data.frame(FalsePositive = c(ROCperf_out@x.values[[1]]),
                               TruePositive = c(ROCperf_out@y.values[[1]]))

ggplot() +
  geom_line(data = df_ROC_2000s, aes(x = FalsePositive, y = TruePositive,
                                     color = "In Sample")) +
  geom_line(data = df_ROC_out_2000s, aes(x = FalsePositive, y = TruePositive,
                                         color = "Out of Sample")) + 
  geom_abline(slope = 1)

auc <- performance(ROCpred, measure = "auc")
auc_out <- performance(ROCpred_out, measure = "auc")
aucs_2000s <- c(auc@y.values[[1]], auc_out@y.values[[1]])
names(aucs_2000s) <- c("In sample AUC", "Out of sample AUC")
aucs_2000s

fit_2000f <- glm(AAER ~ logtotasset + rsst_acc + chg_recv + chg_inv +
                   soft_assets + pct_chg_cashsales + chg_roa + issuance +
                   oplease_dum + book_mkt + lag_sdvol + merger + bigNaudit +
                   midNaudit + cffin + exfin + restruct + bullets + headerlen +
                   newlines + alltags + processedsize + sentlen_u + wordlen_s +
                   paralen_s + repetitious_p + sentlen_s + typetoken +
                   clindex + fog + active_p + passive_p + lm_negative_p +
                   lm_positive_p + allcaps + exclamationpoints + questionmarks,
                 data = df[df$Test == 0, ], family = binomial)
tidy(fit_2000f)

library(ROCR)
pred <- predict(fit_2000f, df, type="response")
ROCpred <- prediction(as.numeric(pred[df$Test == 0 & !is.na(pred)]),
                      as.numeric(df[df$Test == 0 & !is.na(pred), ]$AAER))
ROCpred_out <- prediction(as.numeric(pred[df$Test == 1 & !is.na(pred)]),
                          as.numeric(df[df$Test == 1 & !is.na(pred), ]$AAER))
ROCperf <- performance(ROCpred, 'tpr', 'fpr')
ROCperf_out <- performance(ROCpred_out, 'tpr', 'fpr')
df_ROC_2000f <- data.frame(FalsePositive = c(ROCperf@x.values[[1]]),
                           TruePositive = c(ROCperf@y.values[[1]]))
df_ROC_out_2000f <- data.frame(FalsePositive = c(ROCperf_out@x.values[[1]]),
                               TruePositive = c(ROCperf_out@y.values[[1]]))

ggplot() +
  geom_line(data = df_ROC_2000f, aes(x = FalsePositive, y = TruePositive,
                                     color = "In Sample")) +
  geom_line(data = df_ROC_out_2000f, aes(x = FalsePositive, y = TruePositive,
                                         color = "Out of Sample")) + 
  geom_abline(slope = 1)

auc <- performance(ROCpred, measure = "auc")
auc_out <- performance(ROCpred_out, measure = "auc")
aucs_2000f <- c(auc@y.values[[1]], auc_out@y.values[[1]])
names(aucs_2000f) <- c("In sample AUC", "Out of sample AUC")
aucs_2000f

BCE_eq = as.formula(paste("AAER ~ logtotasset + rsst_acc + chg_recv + chg_inv +
                          soft_assets + pct_chg_cashsales + chg_roa + issuance +
                          oplease_dum + book_mkt + lag_sdvol + merger +
                          bigNaudit + midNaudit + cffin + exfin + restruct +
                          bullets + headerlen + newlines + alltags +
                          processedsize + sentlen_u + wordlen_s + paralen_s +
                          repetitious_p + sentlen_s + typetoken + clindex + fog +
                          active_p + passive_p + lm_negative_p + lm_positive_p +
                          allcaps + exclamationpoints + questionmarks +",
                          paste(paste0("Topic_", 1:30, "_n_oI"), collapse=" + "),
                          collapse = "")) # Topic_1_n_oI is the topic variable

fit_BCE <- glm(BCE_eq, data = df[df$Test == 0, ], family = binomial)

tidy(fit_BCE) # Topic_1_n_oI is the topic variable

library(ROCR)
pred <- predict(fit_BCE, df, type = "response")
ROCpred <- prediction(as.numeric(pred[df$Test == 0 & !is.na(pred)]),
                      as.numeric(df[df$Test == 0 & !is.na(pred), ]$AAER))
ROCpred_out <- prediction(as.numeric(pred[df$Test == 1 & !is.na(pred)]),
                          as.numeric(df[df$Test == 1 & !is.na(pred), ]$AAER))
ROCperf <- performance(ROCpred, 'tpr', 'fpr')
ROCperf_out <- performance(ROCpred_out, 'tpr', 'fpr')
df_ROC_BCE <- data.frame(FalsePositive = c(ROCperf@x.values[[1]]),
                         TruePositive = c(ROCperf@y.values[[1]]))
df_ROC_out_BCE <- data.frame(FalsePositive = c(ROCperf_out@x.values[[1]]),
                             TruePositive = c(ROCperf_out@y.values[[1]]))

ggplot() +
  geom_line(data = df_ROC_BCE, aes(x = FalsePositive, y = TruePositive,
                                   color = "In Sample")) +
  geom_line(data = df_ROC_out_BCE, aes(x = FalsePositive, y = TruePositive,
                                       color = "Out of Sample")) + 
  geom_abline(slope = 1)

auc <- performance(ROCpred, measure = "auc")
auc_out <- performance(ROCpred_out, measure = "auc")
aucs_BCE <- c(auc@y.values[[1]], auc_out@y.values[[1]])
names(aucs_BCE) <- c("In sample AUC", "Out of sample AUC")
aucs_BCE

ggplot() +
  geom_line(data = df_ROC_out_BCE, aes(x = FalsePositive, y = TruePositive,
                                       color = "BCE")) +
  geom_line(data = df_ROC_out_2000f, aes(x = FalsePositive, y = TruePositive,
                                         color = "2000s + 2011")) + 
  geom_line(data = df_ROC_out_2000s, aes(x = FalsePositive, y = TruePositive,
                                         color = "2000s")) + 
  geom_line(data = df_ROC_out_2011, aes(x = FalsePositive, y = TruePositive,
                                        color = "2011")) + 
  geom_line(data = df_ROC_out_1990s, aes(x = FalsePositive, y = TruePositive,
                                         color = "1990s")) + 
  geom_abline(slope = 1) + 
  ggtitle("Out of Sample ROC Curves")

oos_aucs <- c(aucs_1990s[2], aucs_2011[2], aucs_2000s[2], aucs_2000f[2], aucs_BCE[2])
names(oos_aucs) <- c("1990s", "2011", "2000s", "2000s + 2011", "BCE")
oos_aucs

library(glmnet)
x <- model.matrix(BCE_eq, data = df[df$Test == 0, ])[ , -1] # remove intercept
y <- model.frame(BCE_eq, data = df[df$Test == 0, ])[ , "AAER"]
fit_LASSO <- glmnet(x = x, y = y,
                    family = "binomial", # "gaussian" for least squares (default)
                    alpha = 1  # LASSO, the default.  alpha = 0 is ridge
                               # alpha between 0 and 1: elastic net
                    )

plot(fit_LASSO, xvar = 'lambda', label = TRUE)

coefpath(fit_LASSO)

# It shows from left to right the number of nonzero coefficients (Df),
# the percent of deviance explained (%dev) and the value of Lambda.
# Although by default glmnet calls for 100 values of lambda the program stops
# early if %dev doesn't change sufficiently from one lambda to the next.
print(fit_LASSO)

coefplot(fit_LASSO, lambda = 0.002031, sort = 'magnitude')
#coef(fit_LASSO, s=0.002031) #to check the magnitude of coefficients

# na.pass has model.matrix retain NA values (so the # of rows is constant)
xp <- model.matrix(BCE_eq, data = df, na.action = 'na.pass')[ , -1]
# s= specifies the version of the model to use
pred <- predict(fit_LASSO, xp, type = "response", s = 0.002031)

ROCpred <- prediction(as.numeric(pred[df$Test == 0 & !is.na(pred)]),
                      as.numeric(df[df$Test == 0 & !is.na(pred), ]$AAER))
ROCpred_out <- prediction(as.numeric(pred[df$Test == 1 & !is.na(pred)]),
                          as.numeric(df[df$Test == 1 & !is.na(pred), ]$AAER))
ROCperf <- performance(ROCpred, 'tpr', 'fpr')
ROCperf_out <- performance(ROCpred_out, 'tpr', 'fpr')
df_ROC_L1 <- data.frame(FalsePositive = c(ROCperf@x.values[[1]]),
                        TruePositive = c(ROCperf@y.values[[1]]))
df_ROC_out_L1 <- data.frame(FalsePositive = c(ROCperf_out@x.values[[1]]),
                            TruePositive = c(ROCperf_out@y.values[[1]]))

ggplot() +
  geom_line(data = df_ROC_L1, aes(x = FalsePositive, y = TruePositive,
                                  color = "In Sample")) +
  geom_line(data = df_ROC_out_L1, aes(x = FalsePositive, y = TruePositive,
                                      color = "Out of Sample")) + 
  geom_abline(slope = 1)

auc <- performance(ROCpred, measure = "auc")
auc_out <- performance(ROCpred_out, measure = "auc")
aucs_L1 <- c(auc@y.values[[1]], auc_out@y.values[[1]])
names(aucs_L1) <- c("In sample AUC", "Out of sample AUC")
aucs_L1

# Cross validation
set.seed(2021)  #for reproducibility
cvfit = cv.glmnet(x = x, y = y,family = "binomial", alpha = 1,
                  type.measure = "auc")

plot(cvfit)

cvfit$lambda.min
cvfit$lambda.1se

#coef(cvfit, s = "lambda.min")
coefplot(cvfit, lambda = 'lambda.min', sort = 'magnitude') +
  theme(axis.text.y = element_text(size = 15))

#coef(cvfit, s = "lambda.1se")
coefplot(cvfit, lambda = 'lambda.1se', sort = 'magnitude') +
  theme(axis.text.y = element_text(size = 15))

# s= specifies the version of the model to use
pred <- predict(cvfit, xp, type = "response", s = "lambda.min")
pred2 <- predict(cvfit, xp, type = "response", s = "lambda.1se")

ROCpred <- prediction(as.numeric(pred[df$Test == 0 & !is.na(pred)]),
                      as.numeric(df[df$Test == 0 & !is.na(pred), ]$AAER))
ROCpred_out <- prediction(as.numeric(pred[df$Test == 1 & !is.na(pred)]),
                          as.numeric(df[df$Test == 1 & !is.na(pred), ]$AAER))
ROCperf <- performance(ROCpred, 'tpr', 'fpr')
ROCperf_out <- performance(ROCpred_out, 'tpr', 'fpr')
df_ROC_CV <- data.frame(FalsePositive = c(ROCperf@x.values[[1]]),
                        TruePositive = c(ROCperf@y.values[[1]]))
df_ROC_out_CV <- data.frame(FalsePositive = c(ROCperf_out@x.values[[1]]),
                            TruePositive = c(ROCperf_out@y.values[[1]]))
auc <- performance(ROCpred, measure = "auc")
auc_out <- performance(ROCpred_out, measure = "auc")
aucs_CV <- c(auc@y.values[[1]], auc_out@y.values[[1]])

ROCpred <- prediction(as.numeric(pred2[df$Test == 0 & !is.na(pred)]),
                      as.numeric(df[df$Test == 0 & !is.na(pred), ]$AAER))
ROCpred_out <- prediction(as.numeric(pred2[df$Test == 1 & !is.na(pred)]),
                          as.numeric(df[df$Test == 1 & !is.na(pred), ]$AAER))
ROCperf <- performance(ROCpred, 'tpr', 'fpr')
ROCperf_out <- performance(ROCpred_out, 'tpr', 'fpr')
df_ROC_CV2 <- data.frame(FalsePositive = c(ROCperf@x.values[[1]]),
                         TruePositive = c(ROCperf@y.values[[1]]))
df_ROC_out_CV2 <- data.frame(FalsePositive = c(ROCperf_out@x.values[[1]]),
                             TruePositive = c(ROCperf_out@y.values[[1]]))
auc <- performance(ROCpred, measure = "auc")
auc_out <- performance(ROCpred_out, measure = "auc")
aucs_CV2 <- c(auc@y.values[[1]], auc_out@y.values[[1]])

ggplot() +
  geom_line(data = df_ROC_CV, aes(x = FalsePositive, y = TruePositive,
                                  color = "In Sample, lambda.min")) +
  geom_line(data = df_ROC_out_CV, aes(x = FalsePositive, y = TruePositive,
                                      color = "Out of Sample, lambda.min")) +
  geom_line(data = df_ROC_CV2, aes(x = FalsePositive, y = TruePositive,
                                   color = "In Sample, lambda.1se")) +
  geom_line(data = df_ROC_out_CV2, aes(x = FalsePositive, y = TruePositive,
                                       color = "Out of Sample, lambda.1se")) + 
  geom_abline(slope = 1)

aucs <- c(aucs_CV, aucs_CV2)
names(aucs) <- c("In sample AUC, lambda.min", "Out of sample AUC, lambda.min", "In sample AUC, lambda.1se", "Out of sample AUC, lambda.1se")
aucs

add_auc <- aucs_CV[2]
names(add_auc) <- c("LASSO, lambda.min")
oos_aucs <- c(oos_aucs, add_auc)

fit_glm <- glm(AAER ~ logtotasset + soft_assets + merger + restruct +
                 processedsize + lm_negative_p + Topic_2_n_oI + Topic_6_n_oI +
                 Topic_8_n_oI + Topic_9_n_oI + Topic_12_n_oI + Topic_18_n_oI +
                 Topic_19_n_oI + Topic_26_n_oI + Topic_30_n_oI,
               data = df[df$Test == 0, ], family = binomial)
tidy(fit_glm)

# It takes 3 minutes on my Xeon W-2133 32G workstation
list.of.fits <- list() # To store the results
for (i in 0:10) {
  ## First, make a variable name that we can use later to refer
  ## to the model optimized for a specific alpha.
  ## For example, when alpha = 0, we will be able to refer to 
  ## that model with the variable name "alpha0".
  fit.name <- paste0("alpha", i/10)
  
  ## Now fit a model (i.e. optimize lambda) and store it in a list that 
  ## uses the variable name we just created as the reference.
  list.of.fits[[fit.name]] <-
    cv.glmnet(x = x, y = y, family = "binomial", alpha = i/10,
              type.measure = "auc")
}

## Now we see which alpha (0, 0.1, ... , 0.9, 1) does the best job
## predicting the values in the Testing dataset.
results <- data.frame()
for (i in 0:10) {
  fit.name <- paste0("alpha", i/10)
  
  ## Use each model to predict 'y' given the Testing dataset
  pred <- 
    predict(list.of.fits[[fit.name]], xp, type = "response",
            s = list.of.fits[[fit.name]]$lambda.1se)

  ROCpred_out <- prediction(as.numeric(pred[df$Test == 1 & !is.na(pred)]),
                            as.numeric(df[df$Test == 1 & !is.na(pred), ]$AAER))
  auc_out <- performance(ROCpred_out, measure = "auc")

  ## Store the results
  temp <- data.frame(alpha=i/10, auc = auc_out@y.values[[1]],
                     fit.name = fit.name)
  results <- rbind(results, temp)
}

## View the results
html_df(results)

# XGBoost Model Setup
library(xgboost)
set.seed(2021)

# These params take some work to pin down
params <- list(max_depth = 5, # Maximum depth of a tree
               eta = 0.2, # learning rate or step size for each boosting
               gamma = 10, # Min loss reduction to make a further partition
               min_child_weight = 20, # min obs needed to be in each node
               objective = "binary:logistic") # "reg:linear" for simple OLS
# The cross-validation function of xgboost
xgbCV <- xgb.cv(params = params, data = x, label = y,
                nrounds = 100, # The number of rounds/iterations for boosting
                eval_metric = "auc",
                # randomly partitioned into nfold equal size subsamples
                nfold = 10,
                # stratified sampling by the values of outcome labels
                # ie, same proportion of outcomes for subsamples
                stratified = TRUE)
# Boost at minimum number of iterations with the max AUC
# which(): position of the elements in a logical vector which are TRUE
numRounds <- min(which(xgbCV$evaluation_log$test_auc_mean ==
                        max(xgbCV$evaluation_log$test_auc_mean)))

fit_XGB <- xgboost(params = params,
                data = x,
                label = y,
                nrounds = numRounds,
                eval_metric = "auc")

# Display relative importance of variables for prediction
xtest <- model.matrix(BCE_eq, data = df[df$Test == 1, ])[ , -1]
ytest <- model.frame(BCE_eq, data = df[df$Test == 1, ])[ , "AAER"]
xgb.train.data = xgb.DMatrix(x, label = y, missing = NA)
xgb.test.data = xgb.DMatrix(xtest, label = ytest, missing = NA)
col_names = attr(xgb.train.data, ".Dimnames")[[2]]
imp = xgb.importance(col_names, fit_XGB)

xgb.plot.importance(imp)

# Usual AUC calculation
pred.xgb <- predict(fit_XGB, xtest, type = "response")
ROCpred.xgb <- prediction(as.numeric(pred.xgb), as.numeric(ytest))
ROCperf.xgb <- performance(ROCpred.xgb, 'tpr', 'fpr')
#plot(ROCperf.xgb)
df_ROC.xgb <- data.frame(FalsePositive = c(ROCperf.xgb@x.values[[1]]),
                         TruePositive = c(ROCperf.xgb@y.values[[1]]))
ggplot() +
  geom_line(data = df_ROC_out_2000f, aes(x = FalsePositive, y = TruePositive, color = "2000s + 2011")) +
  geom_line(data = df_ROC_out_2000s, aes(x = FalsePositive, y = TruePositive, color = "2000s")) +
  geom_line(data = df_ROC_out_2011, aes(x = FalsePositive, y = TruePositive, color = "2011")) +
  geom_line(data = df_ROC_out_1990s, aes(x = FalsePositive, y = TruePositive, color = "1990s")) +
  geom_line(data = df_ROC_out_BCE, aes(x = FalsePositive, y = TruePositive, color = "Logit BCE")) +
  geom_line(data = df_ROC_out_CV, aes(x = FalsePositive, y = TruePositive,  color = "LASSO lambda.min")) +
  geom_line(data = df_ROC.xgb, aes(x = FalsePositive, y = TruePositive, color = "XGBoost")) +
  geom_abline(slope=1) + ggtitle("ROC Curves across models")

auc.xgb <- performance(ROCpred.xgb, measure = "auc")

auc <- auc.xgb@y.values[[1]]
names(auc) <- c("XGBoost AUC")
oos_aucs <- c(oos_aucs, auc)
oos_aucs

# Use the following code to install the xgboostExplainer package
# install.packages("devtools")
# library(devtools)
# install_github("AppliedDataSciencePartners/xgboostExplainer")
library(xgboostExplainer)

explainer = buildExplainer(fit_XGB, xgb.train.data, type = "binary",
                           base_score = 0.5, trees_idx = NULL)
pred.breakdown = explainPredictions(fit_XGB, explainer, xgb.test.data)

# See what XGBoost is thinking of Enron Corp (gvkey 6127) in year 2000
# enron_row is the position of record for Enron (row number 2237 in the data)
# the prediction is -2.9 (the last black bar), so how much is the probability?
enron_row = 2237
showWaterfall(fit_XGB, explainer, xgb.test.data, data.matrix(df[df$Test == 1, ]),
              enron_row, type = "binary") +
  ggtitle("Enron Corp in 2000 -- what's probability to have fraudulent report?")

# pred.breakdown includes all 68 estimated coefficients
# the following is to compute logodds, odds, and probability
pred.breakdown <- pred.breakdown[, 1:68] %>%
  mutate(pred_logodds = rowSums(.),
         pred_odds = exp(pred_logodds),
         pred_prob = pred_odds / (1 + pred_odds),
         AAER = ytest
         )

# The distribution is very right skewed, I only show prob <=0.2 for better visual
# I am trying a ggplot themes package which includes many interesting themes used by websites such as Economists and WSJ
# install.packages('ggthemes')
# https://github.com/jrnold/ggthemes
library(ggthemes)
ggplot(pred.breakdown[pred_prob <= 0.2], aes(pred_prob, color = as.factor(AAER))) +
  geom_density(size = 1) +
  ggtitle("Predicted Probability of Fraud") +
  scale_color_economist(name = "data", labels = c("No Fraud: Negative", "Fraud: Positive")) +
  theme_economist()

ROCperf.xgb.cutoff <- performance(ROCpred.xgb, 'sens', 'cutoff')
plot(ROCperf.xgb.cutoff, col = "red")


# plot using ggplot2
#df_cutoff.xgb <- data.frame(Sensitivity = c(ROCperf.xgb.cutoff@x.values[[1]]),
#                            Cutoff = c(ROCperf.xgb.cutoff@y.values[[1]]))

#ggplot() +
#  geom_line(data = df_cutoff.xgb, aes(y = Sensitivity, x = Cutoff)) +
#  ggtitle("Sesitivity at Various Cutoff") +
#  theme_economist()

# ------------------------------------------------------------------------------------------
# http://ethen8181.github.io/machine-learning/unbalanced/unbalanced.html
# [ConfusionMatrixInfo] : 
# Obtain the confusion matrix plot and data.table for a given
# dataset that already consists the predicted score and actual outcome.
# @data    : your data.table or data.frame type data that consists the column
#            of the predicted score and actual outcome 
# @predict : predicted score's column name
# @actual  : actual results' column name
# @cutoff  : cutoff value for the prediction score 
# return   : 1. data : a data.table consisting of three column
#            		   the first two stores the original value of the prediction and actual outcome from
#			 		   the passed in data frame, the third indicates the type, which is after choosing the 
#			 		   cutoff value, will this row be a true/false positive/ negative 
#            2. plot : plot that visualizes the data.table 

ConfusionMatrixInfo <- function(data, predict, actual, cutoff)
{	
	# extract the column ;
	# relevel making 1 appears on the more commonly seen position in 
	# a two by two confusion matrix	
	predict <- data[[predict]]
	actual  <- relevel(as.factor(data[[actual]]), "1")
	
	result <- data.table(actual = actual, predict = predict)

	# caculating each pred falls into which category for the confusion matrix
	result[ , type := ifelse(predict >= cutoff & actual == 1, "TP",
	                         ifelse(predict >= cutoff & actual == 0, "FP",
	                                ifelse(predict <  cutoff & actual == 1, "FN", "TN"))) %>% as.factor()]

	# jittering : can spread the points along the x axis 
	plot <- ggplot(result, aes(actual, predict, color = type)) +
	  geom_violin(fill = "white", color = NA) +
	  geom_jitter(shape = 1) +
	  geom_hline(yintercept = cutoff, color = "blue", alpha = 0.6) +
	  scale_y_continuous(limits = c(0, 1)) +
	  scale_color_discrete(breaks = c("TP", "FN", "FP", "TN")) + # ordering of the legend
	  guides(col = guide_legend(nrow = 2)) + # adjust the legend to have two rows
	  ggtitle(sprintf("Confusion Matrix with Cutoff at %.2f", cutoff))

	return(list(data = result, plot = plot))
}

# visualize .05 cutoff
library(data.table)
cm_info <- ConfusionMatrixInfo(data = pred.breakdown, predict = "pred_prob",
                               actual = "AAER", cutoff = .05)
cm_info$plot

ggplot(data = data.frame(x = xtest[ , "logtotasset"],
                         y = pred.breakdown[ , logtotasset]),
       aes(x = x, y = y)) +
  geom_point(color = "blue") +
  theme(legend.position = "none") +
  ylab("log asset level impact on log-odds") +
  xlab("Log Asset") +
  ggtitle("Plot of how Log Asset relates to log odds of a fraud") +
  theme_economist() + scale_colour_economist()
