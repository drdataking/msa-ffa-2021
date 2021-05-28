# Define html_df() function for displaying small tables in html format
library(knitr)
library(kableExtra)
html_df <- function(text, cols = NULL, col1 = FALSE, full = F) {
  if(!length(cols)) {
    cols = colnames(text)
  }
  if(!col1) {
    kable(text,"html", col.names = cols, align = c("l",rep('c',length(cols)-1))) %>%
      kable_styling(bootstrap_options = c("striped","hover"), full_width = full)
  } else {
    kable(text,"html", col.names = cols, align = c("l", rep('c',length(cols)-1))) %>%
      kable_styling(bootstrap_options = c("striped","hover"), full_width = full) %>%
      column_spec(1, bold = T)
  }
}

library(tidyverse)  # we'll extensively use dplyr here
library(lubridate)  # Great for simple date functions
library(broom) # Display regression results in a tidy way
weekly <- read.csv("../../Data/Session_4_WMT_train.csv")
weekly.test <- read.csv("../../Data/Session_4_WMT_test.csv")
weekly.features <- read.csv("../../Data/Session_4_WMT_features.csv")
weekly.stores <- read.csv("../../Data/Session_4_WMT_stores.csv")

head(weekly)
summary(weekly)

# Construct a function in R to calculate WMAE
wmae <- function(actual, predicted, holidays) {
  sum(abs(actual - predicted) * (holidays * 4 + 1)) /
    (length(actual) + 4 * sum(holidays))
}

preprocess_data <- function(df) {
  # Merge the data together (Pulled data from outside of function -- "scoping")
  # https://bookdown.org/rdpeng/rprogdatascience/scoping-rules-of-r.html
  df <- inner_join(df, weekly.stores)
  # last col 'isHoliday' is already in train data, join the first 11 col only.
  df <- inner_join(df, weekly.features[ , 1:11])
  # I am not sure what exactly the five markdowns represent
  # All missing markdowns will be assigned to 0 and record the last non-missing
  df$markdown <- 0
  df[!is.na(df$MarkDown1), ]$markdown <- df[!is.na(df$MarkDown1), ]$MarkDown1
  df[!is.na(df$MarkDown2), ]$markdown <- df[!is.na(df$MarkDown2), ]$MarkDown2
  df[!is.na(df$MarkDown3), ]$markdown <- df[!is.na(df$MarkDown3), ]$MarkDown3
  df[!is.na(df$MarkDown4), ]$markdown <- df[!is.na(df$MarkDown4), ]$MarkDown4
  df[!is.na(df$MarkDown5), ]$markdown <- df[!is.na(df$MarkDown5), ]$MarkDown5
  # Fix dates and add useful time variables
  df$date <- as.Date(df$Date)
  df$week <- week(df$date)
  df$year <- year(df$date)
  df
}
df <- preprocess_data(weekly)
df[df$Weekly_Sales < 0, ]$Weekly_Sales <- 0
df_test <- preprocess_data(weekly.test)

df[91:94, ] %>%
  select(Store, date, markdown, MarkDown3, MarkDown4, MarkDown5) %>%
  html_df()

df[1:2, ] %>% select(date, week, year) %>% html_df()

# Fill in missing CPI and Unemployment data
df_test <- df_test %>%
  group_by(Store, year) %>%
  mutate(CPI = ifelse(is.na(CPI), mean(CPI, na.rm = T), CPI),
         Unemployment = ifelse(is.na(Unemployment),
                               mean(Unemployment, na.rm = T),
                               Unemployment)) %>%
  ungroup()

# Unique IDs in the data
df$id <- df$Store *10000 + df$week * 100 + df$Dept
df_test$id <- df_test$Store *10000 + df_test$week * 100 + df_test$Dept

# Unique ID and factor building
swd <- c(df$id, df_test$id)  # Pool all IDs
swd <- unique(swd)  # Only keep unique elements
swd <- data.frame(id = swd)  # Make a data frame
swd$swd <- factor(swd$id)  # Extract factors for using later

# Add unique factors to data -- ensures same factors for both data sets
df <- left_join(df, swd)
df_test <- left_join(df_test, swd)

df_test$Id <- paste0(df_test$Store, '_', df_test$Dept, "_", df_test$date)

html_df(df_test[c(20000, 40000, 60000),
                c("Store", "week", "Dept", "id", "swd", "Id")])

# Calculate average sales by store-dept
df <- df %>%
  group_by(Store, Dept) %>% 
  mutate(store_avg = mean(Weekly_Sales, rm.na = T)) %>%
  ungroup()
# Select the first average sales data for each store-dept
df_sa <- df %>%
  group_by(Store, Dept) %>%
  slice(1) %>% # Select rows by position
  select(Store, Dept, store_avg) %>%
  ungroup()
# Distribute the store-dept average sales to the testing data
df_test <- left_join(df_test, df_sa)
# 36 observations have messed up department codes -- ignore (set to 0)
df_test[is.na(df_test$store_avg), ]$store_avg  <- 0

# Calculate multipliers based on store_avg (and removing NaN and Inf)
df$Weekly_mult <- df$Weekly_Sales / df$store_avg
df[!is.finite(df$Weekly_mult), ]$Weekly_mult <- NA

# Calculate mean by week-store-dept and distribute to df_test
df <- df %>%
  group_by(Store, Dept, week) %>%
  mutate(naive_mean = mean(Weekly_Sales, rm.na = T)) %>%
  ungroup()
df_wm <- df %>%
  group_by(Store, Dept, week) %>%
  slice(1) %>%
  ungroup() %>%
  select(Store, Dept, week, naive_mean)
df_test <- df_test %>% arrange(Store, Dept, week)
df_test <- left_join(df_test, df_wm)

table(is.na(df_test$naive_mean))

## df_test <- df_test %>%
##   arrange(Store, Dept, date) %>%
##   group_by(Store, Dept) %>%
##   mutate(naive_mean=ifelse(is.na(naive_mean), lag(naive_mean), naive_mean)) %>%
##   ungroup()

# there are a lot of scattered missing weeks -- fill with a lag first
df_test$naive_na <- is.na(df_test$naive_mean)
df_test <- df_test %>%
  arrange(Store, Dept, date) %>%
  group_by(Store, Dept) %>%
  mutate(naive_mean = ifelse(is.na(naive_mean), lag(naive_mean), naive_mean)) %>%
  ungroup()
df_test <- df_test %>%
  arrange(Store, Dept, date) %>%
  group_by(Store, Dept) %>%
  mutate(naive_mean = ifelse(is.na(naive_mean), lag(naive_mean, 2), naive_mean)) %>%
  ungroup()
df_test <- df_test %>%
  arrange(Store, Dept, date) %>%
  group_by(Store, Dept) %>%
  mutate(naive_mean = ifelse(is.na(naive_mean), lead(naive_mean, 1), naive_mean)) %>%
  ungroup()
df_test <- df_test %>%
  arrange(Store, Dept, date) %>%
  group_by(Store, Dept) %>%
  mutate(naive_mean = ifelse(is.na(naive_mean), lead(naive_mean, 2), naive_mean)) %>%
  ungroup()
df_test$naive_mean <- ifelse(is.na(df_test$naive_mean), df_test$store_avg, df_test$naive_mean)

df %>% 
  group_by(week, Store) %>%
  mutate(sales = mean(Weekly_Sales)) %>%
  slice(1) %>%
  ungroup() %>%
  ggplot(aes(y = sales, x = week, color = factor(Store))) +
  geom_line() + xlab("Week") + ylab("Sales for Store (dept average)") + 
  theme(legend.position = "none") # remove the plot legend

mod1 <- lm(Weekly_mult ~ factor(IsHoliday) + factor(markdown > 0) +
                         markdown + Temperature +
                         Fuel_Price + CPI + Unemployment,
           data = df)
tidy(mod1)
glance(mod1)

# Out of sample result
df_test$Weekly_mult <- predict(mod1, df_test)
df_test$Weekly_Sales <- df_test$Weekly_mult * df_test$store_avg

# Required to submit a csv of Id and Weekly_Sales
write.csv(df_test[ , c("Id", "Weekly_Sales")], "WMT_linear.csv",
          row.names = FALSE)

# track
df_test$WS_linear <- df_test$Weekly_Sales

# Check in sample WMAE
df$WS_linear <- predict(mod1, df) * df$store_avg
w <- wmae(actual = df$Weekly_Sales, predicted = df$WS_linear,
          holidays = df$IsHoliday)
names(w) <- "Linear"
wmaes <- c(w)
wmaes

# compute WMAE for each obs
wmae_obs <- function(actual, predicted, holidays) {
  abs(actual - predicted) * (holidays * 4 + 1) /
    (length(actual) + 4 * sum(holidays))
}
df$wmaes  <- wmae_obs(actual = df$Weekly_Sales, predicted = df$WS_linear,
                      holidays = df$IsHoliday)
ggplot(data = df, aes(y = wmaes, x = week, color = factor(IsHoliday))) + 
  geom_jitter(width = 0.25) + xlab("Week") + ylab("WMAE")

mod2 <- lm(Weekly_mult ~ factor(week) + factor(IsHoliday) + factor(markdown>0) +
             markdown + Temperature + Fuel_Price + CPI + Unemployment, data=df)
tidy(mod2)
glance(mod2)

# Out of sample result
df_test$Weekly_mult <- predict(mod2, df_test)
df_test$Weekly_Sales <- df_test$Weekly_mult * df_test$store_avg

# Required to submit a csv of Id and Weekly_Sales
write.csv(df_test[ , c("Id", "Weekly_Sales")], "WMT_linear2.csv",
          row.names = FALSE)

# track
df_test$WS_linear2 <- df_test$Weekly_Sales

# Check in sample WMAE
df$WS_linear2 <- predict(mod2, df) * df$store_avg
w <- wmae(actual = df$Weekly_Sales, predicted = df$WS_linear2,
          holidays = df$IsHoliday)
names(w) <- "Linear 2"
wmaes <- c(wmaes, w)
wmaes

wmaes_out <- c(4954.4, 5540.3)
names(wmaes_out) <- c("Linear", "Linear 2")
wmaes_out

df$wmaes  <- wmae_obs(actual = df$Weekly_Sales, predicted = df$WS_linear2,
                      holidays = df$IsHoliday)
ggplot(data=df, aes(y = wmaes,
                    x = week,
                    color = factor(IsHoliday))) + 
  geom_jitter(width = 0.25) + xlab("Week") + ylab("WMAE")

ggplot(data=df, aes(y = wmae_obs(actual = Weekly_Sales,
                                 predicted = WS_linear2,
                                 holidays = IsHoliday),
                    x = week, color = factor(Store))) + 
  geom_jitter(width = 0.25) + xlab("Week") + ylab("WMAE") + 
  theme(legend.position = "none")

ggplot(data = df, aes(y = wmae_obs(actual = Weekly_Sales,
                                   predicted = WS_linear2,
                                   holidays = IsHoliday),
                    x = week, color = factor(Dept))) + 
  geom_jitter(width = 0.25) + xlab("Week") + ylab("WMAE") + 
  theme(legend.position = "none")

## mod3 <- lm(Weekly_mult ~ factor(week):factor(Store):factor(Dept) +
##              factor(IsHoliday) + factor(markdown>0) + markdown + Temperature +
##              Fuel_Price + CPI + Unemployment, data = df)
## ## Error: cannot allocate vector of size 606.8Gb

library(lfe)
mod3 <- felm(Weekly_mult ~ markdown + Temperature + Fuel_Price + CPI +
               Unemployment | swd, data = df) # now you know why create swd
tidy(mod3)
glance(mod3)

predict.felm <- function(object, newdata, use.fe = T, ...) {
  # compatible with tibbles
  newdata <- as.data.frame(newdata)
  co <- coef(object)
  
  y.pred <- t(as.matrix(unname(co))) %*% t(as.matrix(newdata[ , names(co)]))
  
  fe.vars <- names(object$fe)
  
  all.fe <- getfe(object)
  for (fe.var in fe.vars) {
    level <- all.fe[all.fe$fe == fe.var, ]
    frows <- match(newdata[[fe.var]], level$idx)
    myfe <- level$effect[frows]
    myfe[is.na(myfe)] = 0
      
    y.pred <- y.pred + myfe
  }
  as.vector(y.pred)
}

# Out of sample result
df_test$Weekly_mult <- predict(mod3, df_test)
df_test$Weekly_Sales <- df_test$Weekly_mult * df_test$store_avg

# Required to submit a csv of Id and Weekly_Sales
write.csv(df_test[ , c("Id", "Weekly_Sales")], "WMT_FE.csv",
          row.names = FALSE)

# track
df_test$WS_FE <- df_test$Weekly_Sales

# Check in sample WMAE
df$WS_FE <- predict(mod3, df) * df$store_avg
w <- wmae(actual = df$Weekly_Sales, predicted = df$WS_FE,
          holidays = df$IsHoliday)
names(w) <- "FE"
wmaes <- c(wmaes, w)
wmaes

wmaes_out <- c(4954.4, 5540.3, 3357.9)
names(wmaes_out) <- c("Linear", "Linear 2", "FE")
wmaes_out

df$wmaes  <- wmae_obs(actual = df$Weekly_Sales, predicted = df$WS_FE,
                      holidays = df$IsHoliday)
ggplot(data=df, aes(y = wmaes,
                    x = week,
                    color = factor(IsHoliday))) + 
  geom_jitter(width = 0.25) + xlab("Week") + ylab("WMAE")

# Function to map holidays
holidf <- function(df, years, weeks) {
  if(length(years) == 2) {
    years = c(years, 9999)
    weeks = c(weeks, 99)
  }
  df %>%
  filter(week(df$date) == weeks[1] & year(df$date) == years[1] |
         week(df$date) == weeks[2] & year(df$date) == years[2] |
         week(df$date) == weeks[3] & year(df$date) == years[3]) %>%
  select(Store, Dept, Weekly_Sales) %>%
  group_by(Store, Dept) %>%
  mutate(holiday_sales = mean(Weekly_Sales, rm.na = T)) %>%
  slice(1) %>%
  ungroup() %>%
  select(Store, Dept, holiday_sales) %>%
  as.data.frame()
}
# Add holiday shift to data
add_holiday <- function(df, holiday, wk) {
  q <- left_join(df, holiday)
  q <- q %>% transmute(Weekly_Sales = ifelse(week == wk, holiday_sales, Weekly_Sales))
  q[[1]]
}

# Start with our FE approach
df_test$Weekly_Sales <- df_test$WS_FE

# CHRISTMAS
# 2010: 53, 2011: 52, 2012: *52*
# Get a df with Christmas specific sales
s_xmas <- holidf(df, c(2010, 2011), c(53, 52))
s_xmas_m1 <- holidf(df, c(2010, 2011), c(52, 51))

df_test$Weekly_Sales <- add_holiday(df_test, s_xmas, 52)
df_test$Weekly_Sales <- add_holiday(df_test, s_xmas_m1, 51)

# BLACK FRIDAY 2010-11-26: 48; 2011-11-25: 47, 2012-11-23: *47*
s_bf <- holidf(df, c(2010, 2011), c(48, 47))

df_test$Weekly_Sales <- add_holiday(df_test, s_bf, 47)

# Note that the superbowl is weeks 6, 6, 7, *6*
s_sb <- holidf(df, c(2010, 2011, 2012), c(6, 6, 7))
s_sb_m1 <- holidf(df, c(2010, 2011, 2012), c(5, 5, 6))

df_test$Weekly_Sales <- add_holiday(df_test, s_sb, 7)
df_test$Weekly_Sales <- add_holiday(df_test, s_sb_m1, 6)

# Clean up any missing values added
df_test[is.na(df_test$Weekly_Sales), ]$Weekly_Sales <- df_test[is.na(df_test$Weekly_Sales), ]$naive_mean

# Calculate yearly growth
# A bit difficult since the data is a partial year, a full year, and a partial year
yg1 <- df %>%
  arrange(Store, year) %>%
  filter(year == 2010) %>%
  group_by(Store, year) %>%
  mutate(sales2010 = mean(Weekly_Sales)) %>%
  slice(1) %>%
  ungroup() %>%
  select(Store, sales2010)
yg2 <- df %>%
  arrange(Store, year) %>%
  filter(date > as.Date("2011-02-05") & year == 2011) %>%
  group_by(Store, year) %>%
  mutate(sales2011a = mean(Weekly_Sales)) %>%
  slice(1) %>%
  ungroup() %>%
  select(Store, sales2011a)
yg3 <- df %>%
  arrange(Store, year) %>%
  filter(date < as.Date("2011-10-06") & year == 2011) %>%
  group_by(Store, year) %>%
  mutate(sales2011b = mean(Weekly_Sales)) %>%
  slice(1) %>%
  ungroup() %>%
  select(Store, sales2011b)
yg4 <- df %>%
  arrange(Store, year) %>%
  filter(year == 2012) %>%
  group_by(Store, year) %>%
  mutate(sales2012 = mean(Weekly_Sales)) %>%
  slice(1) %>%
  ungroup() %>%
  select(Store, sales2012)
yg <- full_join(yg1, yg2) %>% full_join(yg3) %>% full_join(yg4)
yg <- yg %>%
  mutate(growth1 = sales2011a/sales2010,
         growth2 = sales2012/sales2011b,
         growth = ifelse(is.na(growth1), ifelse(is.na(growth2), 1, growth2),
                         ifelse(is.na(growth2), 1, (growth1 + growth2)/2))) %>%
  select(Store, growth)

# Add growth data to our df_test data frame
df_test <- left_join(df_test, yg)
df_test[df_test$year == 2012, ]$Weekly_Sales <- df_test[df_test$year == 2012, ]$Weekly_Sales * df_test[df_test$year == 2012, ]$growth
df_test[df_test$year == 2013, ]$Weekly_Sales <- df_test[df_test$year == 2013, ]$Weekly_Sales * df_test[df_test$year == 2013, ]$growth

write.csv(df_test[ , c("Id", "Weekly_Sales")], file = "WMT_FE_shift.csv", row.names = FALSE)
df_test$WS_FE_shift <- df_test$Weekly_Sales

# BONUS: Ensembling
# Ensemble: Building multiple models and combining them into 1

# This example: add in a naive mean approach with shifting + growth
df_test$Weekly_Sales <- df_test$naive_mean
df_test$Weekly_Sales <- add_holiday(df_test, s_xmas, 52)
df_test$Weekly_Sales <- add_holiday(df_test, s_xmas_m1, 51)
df_test$Weekly_Sales <- add_holiday(df_test, s_bf, 47)
df_test$Weekly_Sales <- add_holiday(df_test, s_sb, 7)
df_test$Weekly_Sales <- add_holiday(df_test, s_sb_m1, 6)
df_test[is.na(df_test$Weekly_Sales), ]$Weekly_Sales <- df_test[is.na(df_test$Weekly_Sales), ]$naive_mean
df_test[df_test$year == 2012, ]$Weekly_Sales <- df_test[df_test$year == 2012, ]$Weekly_Sales * df_test[df_test$year == 2012, ]$growth
df_test[df_test$year == 2013, ]$Weekly_Sales <- df_test[df_test$year == 2013, ]$Weekly_Sales * df_test[df_test$year == 2013, ]$growth
write.csv(df_test[ , c("Id", "Weekly_Sales")], file = "WMT_naivemean.csv", row.names = FALSE)
df_test$WS_naivemean <- df_test$Weekly_Sales

# Ensembles -- in this case, these don't work so well though. Better with more models
# apply(X, MARGIN, FUN, …) is applying a FUNction to MARGINs (1 means cols and 2 means rows) of an array or matrix
df_test$Weekly_Sales <- apply(df_test[ , c("WS_FE_shift", "WS_naivemean")], 1, min)
#df_test$Weekly_Sales <- apply(df_test[ , c("WS_FE_shift", "WS_naivemean")], 1, max)
#df_test$Weekly_Sales <- apply(df_test[ , c("WS_FE_shift", "WS_naivemean")], 1, mean)

write.csv(df_test[ , c("Id", "Weekly_Sales")], file = "WMT_ens.csv", row.names = FALSE)

wmaes_out <- c(4954.4, 5540.3, 3357.9, 3249.1)
names(wmaes_out) <- c("Linear", "Linear 2", "FE", "Shifted FE")
wmaes_out

wmaes_out <- c(4954.4, 5540.3, 3357.9, 3249.1, 3167.99)
names(wmaes_out) <- c("Linear", "Linear 2", "FE", "Shifted FE", "Naive Mean")
wmaes_out

wmaes_out <- c(4954.4, 5540.3, 3357.9, 3249.1, 3167.99, 3173.3)
names(wmaes_out) <- c("Linear", "Linear 2", "FE", "Shifted FE", "Naive Mean", "Ensemble")
wmaes_out
