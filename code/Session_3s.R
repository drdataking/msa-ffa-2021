# Define html_df function for displaying small tables in html format
library(knitr)
library(kableExtra)
html_df <- function(text, cols=NULL, col1=FALSE, full=F) {
  if(!length(cols)) {
    cols=colnames(text)
  }
  if(!col1) {
    kable(text,"html", col.names = cols, align = c("l",rep('c',length(cols)-1))) %>%
      kable_styling(bootstrap_options = c("striped","hover"), full_width=full)
  } else {
    kable(text,"html", col.names = cols, align = c("l",rep('c',length(cols)-1))) %>%
      kable_styling(bootstrap_options = c("striped","hover"), full_width=full) %>%
      column_spec(1,bold=T)
  }
}

# Load in primary data set
library(tidyverse)
df <- read.csv("../../Data/Session_3-1.csv")

library(tidyverse)  # As always
library(plotly)  # interactive graphs
library(lubridate)  # import some sensible date functions

# Generate quarter over quarter growth "revtq_gr"
df <- df %>% group_by(gvkey) %>%
  mutate(revtq_gr = revtq / lag(revtq) - 1) %>% ungroup()

# Generate year-over-year growth "revtq_yoy"
df <- df %>% group_by(gvkey) %>%
  mutate(revtq_yoy = revtq / lag(revtq, 4) - 1) %>% ungroup()

# Generate first difference "revtq_d"
df <- df %>% group_by(gvkey) %>%
  mutate(revtq_d = revtq - lag(revtq)) %>% ungroup()

# Generate a proper date in R
# datadate (end of reporting period) is YYMMDDn8. (int 20200630)
# quarter() is to generate the calendar quarter based on date
# which may be different from company's fiscal quarter
df$date <- ymd(df$datadate)  # From lubridate
df$cqtr <- quarter(df$date)   # From lubridate

## # Generate a proper date in R
## # Datadate is YYMMDDn8. (integer 20200630)
## df$date <- as.Date(as.character(df$datadate), format = "%Y%m%d")

html_df(head(df[ , c("conm", "date", "revtq", "revtq_gr", "revtq_yoy", "revtq_d")]))

head(df[ , c("conm", "date", "datadate", "fqtr", "cqtr")])

## # Brute force code for variable generation of quarterly data lags
## df <- df %>%
##   group_by(gvkey) %>%
##   mutate(revtq_l1 = lag(revtq), revtq_l2 = lag(revtq, 2),
##          revtq_l3 = lag(revtq, 3), revtq_l4 = lag(revtq, 4),
##          revtq_l5 = lag(revtq, 5), revtq_l6 = lag(revtq, 6),
##          revtq_l7 = lag(revtq, 7), revtq_l8 = lag(revtq, 8),
##          revtq_gr1 = lag(revtq_gr), revtq_gr2 = lag(revtq_gr, 2),
##          revtq_gr3 = lag(revtq_gr, 3), revtq_gr4 = lag(revtq_gr, 4),
##          revtq_gr5 = lag(revtq_gr, 5), revtq_gr6 = lag(revtq_gr, 6),
##          revtq_gr7 = lag(revtq_gr, 7), revtq_gr8 = lag(revtq_gr, 8),
##          revtq_yoy1 = lag(revtq_yoy), revtq_yoy2 = lag(revtq_yoy, 2),
##          revtq_yoy3 = lag(revtq_yoy, 3), revtq_yoy4 = lag(revtq_yoy, 4),
##          revtq_yoy5 = lag(revtq_yoy, 5), revtq_yoy6 = lag(revtq_yoy, 6),
##          revtq_yoy7 = lag(revtq_yoy, 7), revtq_yoy8 = lag(revtq_yoy, 8),
##          revtq_d1 = lag(revtq_d), revtq_d2 = lag(revtq_d, 2),
##          revtq_d3 = lag(revtq_d, 3), revtq_d4 = lag(revtq_d, 4),
##          revtq_d5 = lag(revtq_d, 5), revtq_d6 = lag(revtq_d, 6),
##          revtq_d7 = lag(revtq_d, 7), revtq_d8 = lag(revtq_d, 8)) %>%
##   ungroup()

# Custom function to generate a series of lags
library(rlang)
multi_lag <- function(df, lags, var, postfix="") {
  var <- enquo(var)
  quosures <- map(lags, ~quo(lag(!!var, !!.x))) %>%
    set_names(paste0(quo_text(var), postfix, lags))
  return(ungroup(mutate(group_by(df, gvkey), !!!quosures)))
}
# Generate lags "revtq_l#"
df <- multi_lag(df, 1:8, revtq, "_l")

# Generate changes "revtq_gr#"
df <- multi_lag(df, 1:8, revtq_gr)

# Generate year-over-year changes "revtq_yoy#"
df <- multi_lag(df, 1:8, revtq_yoy)

# Generate first differences "revtq_d#"
df <- multi_lag(df, 1:8, revtq_d)

html_df(head(df[ , c("conm", "date", "revtq", "revtq_l1", "revtq_gr1", "revtq_yoy1", "revtq_d1")]))

# Clean the data: Replace NaN, Inf, and -Inf with NA
df <- df %>%
  mutate_if(is.numeric, list(~replace(., !is.finite(.), NA)))

# Split into training and test datasets
# Training dataset: We'll use data released before 2015
train <- filter(df, year(date) < 2016)

# Test dataset: We'll use data released 2016 through 2019 (till 3Q2019)
test <- filter(df, year(date) >= 2016)

summary(df[ , c("revtq", "revtq_gr", "revtq_yoy", "revtq_d", "fqtr")])

# These functions are a bit ugly, but can construct many charts quickly
# eval(parse(text = var)) is just a way to convert the string name to a variable reference
# Density plot for 1st to 99th percentile of data
plt_dist <- function(df, var) {
  df %>%
    filter(eval(parse(text = var)) < quantile(eval(parse(text = var)), 0.99, na.rm = TRUE),
           eval(parse(text = var)) > quantile(eval(parse(text = var)), 0.01, na.rm = TRUE)) %>%
    ggplot(aes(x = eval(parse(text = var)))) + 
    geom_density() + xlab(var)
}

# Density plot for 1st to 99th percentile of both columns
plt_bar <- function(df, var) {
  df %>%
    filter(eval(parse(text = var)) < quantile(eval(parse(text = var)), 0.99, na.rm = TRUE),
           eval(parse(text = var)) > quantile(eval(parse(text = var)), 0.01, na.rm = TRUE)) %>%
    ggplot(aes(y = eval(parse(text = var)), x = fqtr)) + 
    geom_bar(stat = "summary", fun.y = "mean") + xlab(var)
}

# Scatter plot with lag for 1st to 99th percentile of data
plt_sct <- function(df, var1, var2) {
  df %>%
    filter(eval(parse(text = var1)) < quantile(eval(parse(text = var1)), 0.99, na.rm = TRUE),
           eval(parse(text = var2)) < quantile(eval(parse(text = var2)), 0.99, na.rm = TRUE),
           eval(parse(text = var1)) > quantile(eval(parse(text = var1)), 0.01, na.rm = TRUE),
           eval(parse(text = var2)) > quantile(eval(parse(text = var2)), 0.01, na.rm = TRUE)) %>%
    ggplot(aes(y = eval(parse(text = var1)), x=eval(parse(text = var2)), color = factor(fqtr))) + 
    geom_point() + geom_smooth(method = "lm") + ylab(var1) + xlab(var2)
}

## library(ggplot2)  # or tidyverse -- it's part of tidyverse
## df %>%
##   ggplot(aes(y = var_for_y_axis, x = var_for_y_axis)) +
##   geom_point()  # scatterplot

## library(ggplot2)  # or tidyverse -- it's part of tidyverse
## df %>%
##   ggplot(aes(y = var_for_y_axis, x = var_for_y_axis)) +
##   geom_point()  # scatterplot

plt_dist(train, "revtq")

plt_dist(train, "revtq_gr")

plt_dist(train, "revtq_yoy")

plt_dist(train, "revtq_d")

plt_bar(train, "revtq")

plt_bar(train, "revtq_gr")

plt_bar(train, "revtq_yoy")

plt_bar(train, "revtq_d")

plt_sct(train, "revtq", "revtq_l1")

plt_sct(train, "revtq_gr", "revtq_gr1")

plt_sct(train, "revtq_yoy", "revtq_yoy1")

plt_sct(train, "revtq_d", "revtq_d1")

cor(train[,c("revtq","revtq_l1","revtq_l2","revtq_l3","revtq_l4")],
    use = "complete.obs") # delete row if with NA

cor(train[,c("revtq_gr","revtq_gr1","revtq_gr2","revtq_gr3","revtq_gr4")],
    use = "complete.obs")

cor(train[,c("revtq_yoy","revtq_yoy1","revtq_yoy2","revtq_yoy3","revtq_yoy4")],
    use="complete.obs")

cor(train[,c("revtq_d","revtq_d1","revtq_d2","revtq_d3","revtq_d4")],
    use="complete.obs")

mod1 <- lm(revtq ~ revtq_l1, data = train)

mod2 <- lm(revtq ~ revtq_l1 + revtq_l4, data = train)

mod3 <- lm(revtq ~ revtq_l1 + revtq_l2 + revtq_l3 + revtq_l4 + 
             revtq_l5 + revtq_l6 + revtq_l7 + revtq_l8, data = train)

mod4 <- lm(revtq ~ (revtq_l1 + revtq_l2 + revtq_l3 + revtq_l4 +
             revtq_l5 + revtq_l6 + revtq_l7 + revtq_l8):factor(fqtr),
           data = train)

summary(mod1)

summary(mod2)

summary(mod3)

summary(mod4)

rmse <- function(v1, v2) {
  sqrt(mean((v1 - v2)^2, na.rm = TRUE))
}

mae <- function(v1, v2) {
  mean(abs(v1-v2), na.rm = TRUE)
}

models <- list(mod1, mod2, mod3, mod4)
model_names <- c("1 period", "1 and 4 periods", "8 periods", "8 periods w/ quarters")

df_test <- data.frame(adj_r_sq = sapply(models, function(x)summary(x)[["adj.r.squared"]]),
                      rmse_in = sapply(models, function(x)rmse(train$revtq, predict(x, train))),
                      mae_in = sapply(models, function(x)mae(train$revtq, predict(x,train))),
                      rmse_out = sapply(models, function(x)rmse(test$revtq, predict(x,test))),
                      mae_out = sapply(models, function(x)mae(test$revtq, predict(x,test))))
rownames(df_test) <- model_names
html_df(df_test)  # Custom function using knitr and kableExtra

test %>%
  ggplot(aes(y = revtq, x = predict(mod1,test), color = factor(fqtr))) +
  geom_abline(slope = 1) + geom_point() +
  ylab("Actual revenue") + 
  xlab("Prediction: 1 period model")

test %>%
  ggplot(aes(y = revtq, x = predict(mod4,test), color = factor(fqtr))) +
  geom_abline(slope = 1) + geom_point() +
  ylab("Actual revenue") + 
  xlab("Prediction: 8 period X quarter model")

# models
mod1g <- lm(revtq_gr ~ revtq_gr1, data = train)
mod2g <- lm(revtq_gr ~ revtq_gr1 + revtq_gr4, data = train)
mod3g <- lm(revtq_gr ~ revtq_gr1 + revtq_gr2 + revtq_gr3 + revtq_gr4 + revtq_gr5 + revtq_gr6 + revtq_gr7 + revtq_gr8, data = train)
mod4g <- lm(revtq_gr ~ (revtq_gr1 + revtq_gr2 + revtq_gr3 + revtq_gr4 + revtq_gr5 + revtq_gr6 + revtq_gr7 + revtq_gr8):factor(fqtr), data = train)

models <- list(mod1g, mod2g, mod3g, mod4g)
model_names <- c("1 period", "1 and 4 periods", "8 periods", "8 periods w/ quarters")

df_test <- data.frame(adj_r_sq = sapply(models, function(x)summary(x)[["adj.r.squared"]]),
                      rmse_in = sapply(models, function(x)rmse(train$revtq, (1+predict(x,train))*train$revtq_l1)),
                      mae_in = sapply(models, function(x)mae(train$revtq, (1+predict(x,train))*train$revtq_l1)),
                      rmse_out = sapply(models, function(x)rmse(test$revtq, (1+predict(x,test))*test$revtq_l1)),
                      mae_out = sapply(models, function(x)mae(test$revtq, (1+predict(x,test))*test$revtq_l1)))
rownames(df_test) <- model_names
html_df(df_test)

test %>%
  ggplot(aes(y = revtq, x = (1 + predict(mod1g, test))*test$revtq_l1, color = factor(fqtr))) +
  geom_abline(slope=1) + geom_point() +
  ylab("Actual revenue") + 
  xlab("Prediction: 1 period model")

test %>%
  ggplot(aes(y = revtq, x = (1 + predict(mod4g,test))*test$revtq_l1, color = factor(fqtr))) +
  geom_abline(slope = 1) + geom_point() +
  ylab("Actual revenue") + 
  xlab("Prediction: 8 period X quarter model")

# models
mod1y <- lm(revtq_yoy ~ revtq_yoy1, data = train)
mod2y <- lm(revtq_yoy ~ revtq_yoy1 + revtq_yoy4, data = train)
mod3y <- lm(revtq_yoy ~ revtq_yoy1 + revtq_yoy2 + revtq_yoy3 + revtq_yoy4 + revtq_yoy5 + revtq_yoy6 + revtq_yoy7 + revtq_yoy8, data = train)
mod4y <- lm(revtq_gr ~ (revtq_yoy1 + revtq_yoy2 + revtq_yoy3 + revtq_yoy4 + revtq_yoy5 + revtq_yoy6 + revtq_yoy7 + revtq_yoy8):factor(fqtr), data = train)

models <- list(mod1y, mod2y, mod3y, mod4y)
model_names <- c("1 period", "1 and 4 periods", "8 periods", "8 periods w/ quarters")

df_test <- data.frame(adj_r_sq = sapply(models, function(x)summary(x)[["adj.r.squared"]]),
                      rmse_in = sapply(models, function(x)rmse(train$revtq, (1 + predict(x,train))*train$revtq_l4)),
                      mae_in = sapply(models, function(x)mae(train$revtq, (1 + predict(x,train))*train$revtq_l4)),
                      rmse_out = sapply(models, function(x)rmse(test$revtq, (1 + predict(x,test))*test$revtq_l4)),
                      mae_out = sapply(models, function(x)mae(test$revtq, (1 + predict(x,test))*test$revtq_l4)))
rownames(df_test) <- model_names
html_df(df_test)

test %>%
  ggplot(aes(y = revtq, x = (1 + predict(mod1y,test))*test$revtq_l4, color = factor(fqtr))) +
  geom_abline(slope = 1) + geom_point() +
  ylab("Actual revenue") + 
  xlab("Prediction: 1 period model")

test %>%
  ggplot(aes(y = revtq, x = (1 + predict(mod3y,test))*test$revtq_l4, color = factor(fqtr))) +
  geom_abline(slope = 1) + geom_point() +
  ylab("Actual revenue") + 
  xlab("Prediction: 8 period model")

# models
mod1d <- lm(revtq_d ~ revtq_d1, data = train)
mod2d <- lm(revtq_d ~ revtq_d1 + revtq_d4, data = train)
mod3d <- lm(revtq_d ~ revtq_d1 + revtq_d2 + revtq_d3 + revtq_d4 + revtq_d5 + revtq_d6 + revtq_d7 + revtq_d8, data = train)
mod4d <- lm(revtq_d ~ (revtq_d1 + revtq_d2 + revtq_d3 + revtq_d4 + revtq_d5 + revtq_d6 + revtq_d7 + revtq_d8):factor(fqtr), data = train)

models <- list(mod1d, mod2d, mod3d, mod4d)
model_names <- c("1 period", "1 and 4 periods", "8 periods", "8 periods w/ quarters")

df_test <- data.frame(adj_r_sq = sapply(models, function(x)summary(x)[["adj.r.squared"]]),
                      rmse_in = sapply(models, function(x)rmse(train$revtq, predict(x,train) + train$revtq_l1)),
                      mae_in = sapply(models, function(x)mae(train$revtq, predict(x, train) + train$revtq_l1)),
                      rmse_out = sapply(models, function(x)rmse(test$revtq, predict(x, test) + test$revtq_l1)),
                      mae_out = sapply(models, function(x)mae(test$revtq, predict(x, test) + test$revtq_l1)))
rownames(df_test) <- model_names
html_df(df_test)

test %>%
  ggplot(aes(y = revtq, x = predict(mod1d, test) + test$revtq_l1, color = factor(fqtr))) +
  geom_abline(slope = 1) + geom_point() +
  ylab("Actual revenue") + 
  xlab("Prediction: 1 period model")

test %>%
  ggplot(aes(y = revtq, x = predict(mod4d, test) + test$revtq_l1, color = factor(fqtr))) +
  geom_abline(slope = 1) + geom_point() +
  ylab("Actual revenue") + 
  xlab("Prediction: 8 period X quarter model")

# models
mod1g <- lm(revtq_gr ~ revtq_gr1, data = train)
mod2g <- lm(revtq_gr ~ revtq_gr1 + revtq_gr4, data = train)
mod3g <- lm(revtq_gr ~ revtq_gr1 + revtq_gr2 + revtq_gr3 + revtq_gr4 + revtq_gr5 + revtq_gr6 + revtq_gr7 + revtq_gr8, data = train)
mod4g <- lm(revtq_gr ~ (revtq_gr1 + revtq_gr2 + revtq_gr3 + revtq_gr4 + revtq_gr5 + revtq_gr6 + revtq_gr7 + revtq_gr8):factor(fqtr), data = train)

models <- list(mod1g, mod2g, mod3g, mod4g)
model_names <- c("1 period", "1 and 4 periods", "8 periods", "8 periods w/ quarters")

df_test <- data.frame(adj_r_sq = sapply(models, function(x)summary(x)[["adj.r.squared"]]),
                      rmse_in = sapply(models, function(x)rmse(train$revtq_gr, predict(x, train))),
                      mae_in = sapply(models, function(x)mae(train$revtq_gr, predict(x, train))),
                      rmse_out = sapply(models, function(x)rmse(test$revtq_gr, predict(x, test))),
                      mae_out = sapply(models, function(x)mae(test$revtq_gr, predict(x, test))))
rownames(df_test) <- model_names
html_df(df_test)

test %>%
  ggplot(aes(y = revtq_gr, x = predict(mod1g, test), color = factor(fqtr))) +
  geom_abline(slope = 1) + geom_point() +
  ylab("Actual revenue growth") + 
  xlab("Prediction: 1 period model")

test %>%
  ggplot(aes(y = revtq_gr, x = predict(mod4g,test), color = factor(fqtr))) +
  geom_abline(slope = 1) + geom_point() +
  ylab("Actual revenue growth") + 
  xlab("Prediction: 8 period X quarter model")

# models
mod1y <- lm(revtq_yoy ~ revtq_yoy1, data = train)
mod2y <- lm(revtq_yoy ~ revtq_yoy1 + revtq_yoy4, data = train)
mod3y <- lm(revtq_yoy ~ revtq_yoy1 + revtq_yoy2 + revtq_yoy3 + revtq_yoy4 + revtq_yoy5 + revtq_yoy6 + revtq_yoy7 + revtq_yoy8, data = train)
mod4y <- lm(revtq_gr ~ (revtq_yoy1 + revtq_yoy2 + revtq_yoy3 + revtq_yoy4 + revtq_yoy5 + revtq_yoy6 + revtq_yoy7 + revtq_yoy8):factor(fqtr), data = train)

models <- list(mod1y, mod2y, mod3y, mod4y)
model_names <- c("1 period", "1 and 4 periods", "8 periods", "8 periods w/ quarters")

df_test <- data.frame(adj_r_sq = sapply(models, function(x)summary(x)[["adj.r.squared"]]),
                      rmse_in = sapply(models, function(x)rmse(train$revtq_yoy, predict(x, train))),
                      mae_in = sapply(models, function(x)mae(train$revtq_yoy, predict(x, train))),
                      rmse_out = sapply(models, function(x)rmse(test$revtq_yoy, predict(x, test))),
                      mae_out = sapply(models, function(x)mae(test$revtq_yoy, predict(x, test))))
rownames(df_test) <- model_names
html_df(df_test)

test %>%
  ggplot(aes(y = revtq_yoy, x = predict(mod1y, test), color = factor(fqtr))) +
  geom_abline(slope = 1) + geom_point() +
  ylab("Actual year over year revenue growth") + 
  xlab("Prediction: 1 period model")

test %>%
  ggplot(aes(y = revtq_yoy, x = predict(mod3y, test), color = factor(fqtr))) +
  geom_abline(slope = 1) + geom_point() +
  ylab("Actual year over year revenue growth") + 
  xlab("Prediction: 8 period model")

# models
mod1d <- lm(revtq_d ~ revtq_d1, data = train)
mod2d <- lm(revtq_d ~ revtq_d1 + revtq_d4, data = train)
mod3d <- lm(revtq_d ~ revtq_d1 + revtq_d2 + revtq_d3 + revtq_d4 + revtq_d5 + revtq_d6 + revtq_d7 + revtq_d8, data = train)
mod4d <- lm(revtq_d ~ (revtq_d1 + revtq_d2 + revtq_d3 + revtq_d4 + revtq_d5 + revtq_d6 + revtq_d7 + revtq_d8):factor(fqtr), data = train)

models <- list(mod1d, mod2d, mod3d, mod4d)
model_names <- c("1 period", "1 and 4 periods", "8 periods", "8 periods w/ quarters")

df_test <- data.frame(adj_r_sq = sapply(models, function(x)summary(x)[["adj.r.squared"]]),
                      rmse_in = sapply(models, function(x)rmse(train$revtq_d, predict(x,train))),
                      mae_in = sapply(models, function(x)mae(train$revtq_d, predict(x,train))),
                      rmse_out = sapply(models, function(x)rmse(test$revtq_d, predict(x,test))),
                      mae_out = sapply(models, function(x)mae(test$revtq_d, predict(x,test))))
rownames(df_test) <- model_names
html_df(df_test)

test %>%
  ggplot(aes(y = revtq_d, x = predict(mod1d,test), color = factor(fqtr))) +
  geom_abline(slope = 1) + geom_point() +
  ylab("Actual revenue first difference") + 
  xlab("Prediction: 1 period model")

test %>%
  ggplot(aes(y = revtq_d, x = predict(mod4d, test), color = factor(fqtr))) +
  geom_abline(slope = 1) + geom_point() +
  ylab("Actual revenue first difference") + 
  xlab("Prediction: 8 period X quarter model")

# We use Walmart's qsales 2015Q4~2020Q3 (20Q) to predict 2020Q4
set.seed(8) # to produce the same random generated data
wmt <- df %>% filter(tic=="WMT", fyearq >= 2015) %>% select("revtq")
# Create time series
tseries <- ts(wmt$revtq, frequency = 4, start = c(2015, 4))
hist(tseries) # Check the distribution
tseries_df = as.data.frame(tseries) # create dataframe

# fit some distribution
library(fitdistrplus)
fit.norm <- fitdist(as.numeric(tseries_df$x), "norm")
fit.weibull <- fitdist(as.numeric(tseries_df$x), "weibull")
fit.lnorm <- fitdist(as.numeric(tseries_df$x), "lnorm")
fit.gamma <- fitdist(as.numeric(tseries_df$x), "gamma")
fit.logistic <- fitdist(as.numeric(tseries_df$x), "logis")
fit.cauchy <- fitdist(as.numeric(tseries_df$x), "cauchy")
# Compare Goodness-of-fit statistics
gofstat(list(fit.norm, fit.weibull, fit.lnorm, fit.gamma,
             fit.logistic, fit.cauchy),
        fitnames = c("fit.norm", "fit.weibull", "fit.lnorm",
                     "fit.gamma", "fit.logistic", "fit.cauchy"))

# the best Goodness-of-fit statistics is for the log normal distribution
summary(fit.lnorm)

# rlnorm() is the log normal distribution generator
fit.coef <- coef(fit.lnorm)
final_df <- as.data.frame(rlnorm(n=10^4,
                                 meanlog = fit.coef["meanlog"],
                                 sdlog = fit.coef["sdlog"]))
colnames(final_df) <- 'Forecast'
hist(final_df$Forecast) #plot histogram of forecasted quantities

myproba_lnorm <- sum(final_df$Forecast >= 120000) / 100
myproba_lnorm

# normal distribution generator
library(truncnorm)
fit.coef <- coef(fit.norm)
final_df <- as.data.frame(rtruncnorm(n = 10^4,
                                     a = min(tseries_df$x),
                                     b = max(tseries_df$x),
                                     mean = fit.coef["mean"],
                                     sd = fit.coef["sd"]))
colnames(final_df) <- 'Forecast'
myproba_norm <- sum(final_df$Forecast >= 120000) / 100
myproba_norm

## # gamma distribution
## fit.coef <- coef(fit.gamma)
## final_df <- as.data.frame(rgamma(n=10^4, shape = fit.coef["shape"], rate = fit.coef["rate"]))
## colnames(final_df) <- 'Forecast'
## myproba_gamma <- sum(final_df$Forecast >= 120000) / 100
## myproba_gamma

## # logistic distribution
## fit.coef <- coef(fit.logistic)
## final_df <- as.data.frame(rlogis(n=10^4, location = fit.coef["location"],
##                                          scale = fit.coef["scale"]))
## colnames(final_df) <- 'Forecast'
## myproba_logis <- sum(final_df$Forecast >= 120000) / 100
## myproba_logis
