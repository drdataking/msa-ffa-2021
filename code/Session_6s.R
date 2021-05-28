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
library(plotly)
library(lubridate)

# Compustat annual financial data including bankruptcy indicator `dlsrn`
df <- read.csv("../../Data/Session_6-1.csv")
# Credit ratings data from Compustat, monthly
df_ratings <- read.csv("../../Data/Session_6-2-ratings.csv")
# Annual price and no of shares data from CRSP on WRDS
df_mve <- read.csv("../../Data/Session_6-3-mve.csv")
# Monthly risk free rate data from WRDS
df_rf <- read.csv("../../Data/Session_6-4-rf.csv")
# Processed annual stock return data from CRSP
df_stock <- read.csv("../../Data/Session_6-5-stock.csv")

# initial cleaning
# 100338 is an outlier in the bonds distribution
df <- df %>% filter(at >= 1, revt >= 1, gvkey != 100338)

## Merge in stock value
df$date <- as.Date(df$datadate)
df_mve$date <- as.Date(df_mve$datadate)
df_mve <- df_mve %>% rename(gvkey = GVKEY) # df_mve uses GVKEY, df uses gvkey
df_mve$MVE <- df_mve$csho * df_mve$prcc_f # MVE = no. of shares * price per share

df <- left_join(df, df_mve[ , c("gvkey", "date", "MVE")])

df <- df %>%
  group_by(gvkey) %>%
  mutate(bankrupt = ifelse(row_number() == n() & dlrsn == 2 &
                           !is.na(dlrsn), 1, 0)) %>%
  ungroup() #set the most recent year as the bankruptcy year

df <- df %>% # Calculate the measures needed
  mutate(wcap_at = wcap / at,  # x1
         re_at = re / at,  # x2
         ebit_at = ebit / at,  # x3
         mve_lt = MVE / lt,  # x4
         revt_at = revt / at)  # x5
# cleanup
df <- df %>% # to replace all infinite numbers with NA
  mutate_if(is.numeric, list(~replace(., !is.finite(.), NA)))

# Calculate the score
df <- df %>%
  mutate(Z = 1.2 * wcap_at + 1.4 * re_at + 3.3 * ebit_at +
             0.6 * mve_lt  + 0.999 * revt_at)

# Calculate date info for merging
df$date <- as.Date(df$datadate)
df$year <- year(df$date)
df$month <- month(df$date)

# df_ratings has credit ratings from Compustat

# Ratings, in order from worst to best
ratings <- c("D", "C", "CC", "CCC-", "CCC","CCC+", "B-", "B", "B+", "BB-",
             "BB", "BB+", "BBB-", "BBB", "BBB+", "A-", "A", "A+", "AA-", "AA",
             "AA+", "AAA-", "AAA", "AAA+")
# Convert string ratings (splticrm) to ordered factor ratings
df_ratings$rating <- factor(df_ratings$splticrm, levels = ratings, ordered = T)

df_ratings$date <- as.Date(df_ratings$datadate)
df_ratings$year <- year(df_ratings$date)
df_ratings$month <- month(df_ratings$date)

# Merge together data
df <- left_join(df, df_ratings[ , c("gvkey", "year", "month", "rating")])

plot <- df %>%
  filter(!is.na(Z), !is.na(rating)) %>%
  group_by(rating) %>%
  mutate(mean_Z = mean(Z, na.rm = T)) %>%
  slice(1) %>%
  ungroup() %>%
  select(rating, mean_Z) %>%
  ggplot(aes(y = mean_Z, x = rating)) + 
  geom_col() + 
  ylab('Mean Altman Z') + xlab('Credit rating') + 
  theme(axis.text.x = element_text(angle = 90))
ggplotly(plot)

df %>%
  filter(!is.na(Z),
         !is.na(bankrupt)) %>%
  group_by(bankrupt) %>%
  mutate(mean_Z=mean(Z,na.rm=T)) %>%
  slice(1) %>%
  ungroup() %>%
  select(bankrupt, mean_Z) %>%
  html_df()

plot <- df %>%
  filter(!is.na(Z), !is.na(rating), year >= 2000) %>%
  group_by(rating) %>%
  mutate(mean_Z = mean(Z, na.rm = T)) %>%
  slice(1) %>%
  ungroup() %>%
  select(rating, mean_Z) %>%
  ggplot(aes(y = mean_Z, x = rating)) + 
  geom_col() + 
  ylab('Mean Altman Z') + xlab('Credit rating') + 
  theme(axis.text.x = element_text(angle = 90))
ggplotly(plot)

df %>%
  filter(!is.na(Z),
         !is.na(bankrupt),
         year >= 2000) %>%
  group_by(bankrupt) %>%
  mutate(mean_Z=mean(Z,na.rm=T)) %>%
  slice(1) %>%
  ungroup() %>%
  select(bankrupt, mean_Z) %>%
  html_df()

fit_Z <- glm(bankrupt ~ Z, data = df, family = binomial)
summary(fit_Z)

# ROCR 1.0-11 requires manual removal of NA in the prediction() function
# Suggest to install the 1.0-7 version from the archive
# https://cran.r-project.org/src/contrib/Archive/ROCR/ROCR_1.0-7.tar.gz
library(ROCR)
dfZ <- df %>% filter(!is.na(bankrupt), !is.na(Z))
pred_Z <- predict(fit_Z, dfZ, type = "response")
ROCpred_Z <- prediction(as.numeric(pred_Z), as.numeric(dfZ$bankrupt))
ROCperf_Z <- performance(ROCpred_Z, 'tpr', 'fpr')

df_ROC_Z <- data.frame(
  FP = c(ROCperf_Z@x.values[[1]]),
  TP = c(ROCperf_Z@y.values[[1]]))
ggplot(data = df_ROC_Z,
       aes(x = FP, y = TP)) +
  geom_line() +
  geom_abline(slope = 1)

plot(ROCperf_Z)

ggplot(data=df_ROC_Z, aes(x=FP, y=TP)) +
  geom_line() +
  geom_abline(slope=1) +
  ylab("True positive rate (Sensitivity)") + 
  xlab("False positive rate (1 - Specificity)")

auc_Z <- performance(ROCpred_Z, measure = "auc")
auc_Z@y.values[[1]]

score = 1
m = 0
std = 1

funcShaded <- function(x, lower_bound) {
    y = dnorm(x, mean = m, sd = std)
    y[x < lower_bound] <- NA
    return(y)
}

ggplot(data.frame(x = c(-3, 3)), aes(x = x)) + 
  stat_function(fun = dnorm, args = list(mean = m, sd = std)) + 
  stat_function(fun = funcShaded, args = list(lower_bound = score), 
                geom = "area", fill = 'black', alpha = .2) +
  scale_x_continuous(name = "Score", breaks = seq(-3, 3, std)) + 
  geom_text(data = data.frame(x = c(1.5), y = c(0.05)), aes(x = x, y = y, label = "Prob(default)", size = 30)) + 
  geom_line(data = data.frame(x = c(1,1), y = c(0,0.4)), aes(x = x, y = y)) + 
  geom_text(data = data.frame(x = c(1.3), y = c(0.4)), aes(x = x, y = y, label = "DD", size = 30)) +
  theme(legend.position = "none")

# df_stock is an already prepped csv from CRSP data
df_stock$date <- as.Date(df_stock$date)
df <- left_join(df, df_stock[ , c("gvkey", "date", "ret", "ret.sd")])

df_rf$date <- as.Date(df_rf$dateff)
df_rf$year <- year(df_rf$date)
df_rf$month <- month(df_rf$date)

df <- left_join(df, df_rf[ , c("year", "month", "rf")])

df <- df %>%
  mutate(DD = (log(MVE / lt) + (rf - (ret.sd*sqrt(252))^2 / 2)) /
              (ret.sd * sqrt(252)))
# Clean the measure
df <- df %>%
  mutate_if(is.numeric, list(~replace(., !is.finite(.), NA)))

plot <- df %>%
  filter(!is.na(DD),
         !is.na(rating)) %>%
  group_by(rating) %>%
  mutate(mean_DD=mean(DD,na.rm=T),
         prob_default = pnorm(-1 * mean_DD)) %>%
  slice(1) %>%
  ungroup() %>%
  select(rating, prob_default) %>%
  ggplot(aes(y=prob_default, x=rating)) + 
  geom_col() + 
  ylab('Probability of default') + xlab('Credit rating') + 
  theme(axis.text.x = element_text(angle = 90))
ggplotly(plot)

df %>%
  filter(!is.na(DD),
         !is.na(bankrupt)) %>%
  group_by(bankrupt) %>%
  mutate(mean_DD=mean(DD, na.rm=T),
         prob_default =
           pnorm(-1 * mean_DD)) %>%
  slice(1) %>%
  ungroup() %>%
  select(bankrupt, mean_DD,
         prob_default) %>%
  html_df()

plot <- df %>%
  filter(!is.na(DD),
         !is.na(rating),
         year >= 2000) %>%
  group_by(rating) %>%
  mutate(mean_DD=mean(DD,na.rm=T),
         prob_default = pnorm(-1 * mean_DD)) %>%
  slice(1) %>%
  ungroup() %>%
  select(rating, prob_default) %>%
  ggplot(aes(y=prob_default, x=rating)) + 
  geom_col() + 
  ylab('Probability of default') + xlab('Credit rating') + 
  theme(axis.text.x = element_text(angle = 90))
ggplotly(plot)

df %>%
  filter(!is.na(DD),
         !is.na(bankrupt),
         year >= 2000) %>%
  group_by(bankrupt) %>%
  mutate(mean_DD=mean(DD, na.rm=T),
         prob_default =
           pnorm(-1 * mean_DD)) %>%
  slice(1) %>%
  ungroup() %>%
  select(bankrupt, mean_DD,
         prob_default) %>%
  html_df()

fit_DD <- glm(bankrupt ~ DD, data = df, family = binomial)
summary(fit_DD)

dfDD <- df %>% filter(!is.na(DD), !is.na(bankrupt))
pred_DD <- predict(fit_DD, dfDD, type = "response")
ROCpred_DD <- prediction(as.numeric(pred_DD), as.numeric(dfDD$bankrupt))
ROCperf_DD <- performance(ROCpred_DD, 'tpr', 'fpr')
df_ROC_DD <- data.frame(FalsePositive=c(ROCperf_DD@x.values[[1]]),
                 TruePositive=c(ROCperf_DD@y.values[[1]]))
ggplot() +
  geom_line(data=df_ROC_DD, aes(x=FalsePositive, y=TruePositive, color="DD")) + 
  geom_line(data=df_ROC_Z, aes(x=FP, y=TP, color="Z")) + geom_abline(slope=1)

#AUC
auc_DD <- performance(ROCpred_DD, measure = "auc")
AUCs <- c(auc_Z@y.values[[1]], auc_DD@y.values[[1]])
names(AUCs) <- c("Z", "DD")
AUCs

# calculate downgrade
df <- df %>% arrange(gvkey, date) %>%
  group_by(gvkey) %>%
  mutate(downgrade = ifelse(rating < lag(rating), 1, 0))

# training sample
train <- df %>% filter(year < 2015)
test <- df %>% filter(year >= 2015)

# glms
fit_Z2 <- glm(downgrade ~ Z, data = train, family = binomial)
fit_DD2 <- glm(downgrade ~ DD, data = train, family = binomial)

summary(fit_Z2)

summary(fit_DD2)

pred_Z2 <- predict(fit_Z2, train, type = "response")
pred_Z2 <- ifelse(!is.finite(pred_Z2), NA, pred_Z2)

ROCpred_Z2 <- prediction(as.numeric(pred_Z2[!is.na(train$downgrade) & !is.na(pred_Z2)]),
                         as.numeric(train[!is.na(train$downgrade) & !is.na(pred_Z2), ]$downgrade))
ROCperf_Z2 <- performance(ROCpred_Z2, 'tpr','fpr')
df_ROC_Z2 <- data.frame(FalsePositive = c(ROCperf_Z2@x.values[[1]]),
                        TruePositive = c(ROCperf_Z2@y.values[[1]]))
auc_Z2 <- performance(ROCpred_Z2, measure = "auc")

pred_DD2 <- predict(fit_DD2, train, type = "response")
ROCpred_DD2 <- prediction(as.numeric(pred_DD2[!is.na(train$downgrade) & !is.na(pred_DD2)]),
                          as.numeric(train[!is.na(train$downgrade) & !is.na(pred_DD2), ]$downgrade))
ROCperf_DD2 <- performance(ROCpred_DD2, 'tpr','fpr')
df_ROC_DD2 <- data.frame(FalsePositive = c(ROCperf_DD2@x.values[[1]]),
                         TruePositive = c(ROCperf_DD2@y.values[[1]]))
ggplot() + geom_line(data = df_ROC_DD2, aes(x = FalsePositive, y = TruePositive, color = 'DD')) +
  geom_line(data = df_ROC_Z2, aes(x = FalsePositive, y = TruePositive, color = 'Z')) +
  geom_abline(slope=1)
auc_DD2 <- performance(ROCpred_DD2, measure = "auc")
AUCs <- c(auc_Z2@y.values[[1]], auc_DD2@y.values[[1]])
names(AUCs) <- c("Z", "DD")
AUCs

pred_Z2 <- predict(fit_Z2, test, type = "response")
ROCpred_Z2 <- prediction(as.numeric(pred_Z2[!is.na(test$downgrade) & !is.na(pred_Z2)]),
                         as.numeric(test[!is.na(test$downgrade) & !is.na(pred_Z2), ]$downgrade))
ROCperf_Z2 <- performance(ROCpred_Z2, 'tpr','fpr')
df_ROC_Z2 <- data.frame(FalsePositive = c(ROCperf_Z2@x.values[[1]]),
                        TruePositive = c(ROCperf_Z2@y.values[[1]]))
auc_Z2 <- performance(ROCpred_Z2, measure = "auc")

pred_DD2 <- predict(fit_DD2, test, type = "response")
ROCpred_DD2 <- prediction(as.numeric(pred_DD2[!is.na(test$downgrade) & !is.na(pred_DD2)]),
                          as.numeric(test[!is.na(test$downgrade) & !is.na(pred_DD2), ]$downgrade))
ROCperf_DD2 <- performance(ROCpred_DD2, 'tpr','fpr')
df_ROC_DD2 <- data.frame(FalsePositive = c(ROCperf_DD2@x.values[[1]]),
                         TruePositive = c(ROCperf_DD2@y.values[[1]]))
ggplot() + geom_line(data = df_ROC_DD2, aes(x = FalsePositive, y = TruePositive, color = 'DD')) +
  geom_line(data = df_ROC_Z2, aes(x = FalsePositive, y = TruePositive, color = 'Z')) +
  geom_abline(slope = 1)
auc_DD2 <- performance(ROCpred_DD2, measure = "auc")
AUCs <- c(auc_Z2@y.values[[1]], auc_DD2@y.values[[1]])
names(AUCs) <- c("Z", "DD")
AUCs
