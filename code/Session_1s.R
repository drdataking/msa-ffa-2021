# Use R package to query the Google Books Ngram Viewer which allows you
# to enter a list of phrases and then display a graph showing how often
# the phrases have occurred in a large corpus of books over time
# R Package "ngramr" is available on CRAN.

library(ngramr)
library(ggplot2)
ngd <- c("analytics", "machine learning")
ggram(ngd, year_start = 1960, year_end = 2019, ignore_case = TRUE, 
      geom = "area", geom_options = list(position = "stack")) + 
      labs(y = NULL)

# The data contains all US listed firms Net Income for fiscal years 2017-2018
# extracted from Compustat, check tic and sort by datadate
# To read the data, you may need to change the read_csv file directory
library(readr)
library(ggplot2)
suppressPackageStartupMessages(library(plotly))
df <- read_csv("../../Data/Session_1-1.csv")
df <- df[df$indfmt != "FS",]  # remove Financial Services codes
df <- df[order(df$gvkey,df$datadate), ] # sort by gvkey and datadate
df$ni_lag <- c(NA, df$ni[1:nrow(df)-1]) # get lag net income
df <- df[floor(df$datadate/10000) == 2018, ] # keep 2018 data only
df <- df[!(is.na(df$ni) | is.na(df$ni_lag)), ] # remove NA observations
cor <- cor(x = df$ni, y = df$ni_lag, method = "pearson") # correlation
set.seed(11)
df_s <- df[sample(nrow(df), 400), ] # a random sample of 400 obs

# Past company earnings predicts future company earnings
plot <- ggplot(df, aes(x = ni_lag, y = ni)) + 
  geom_point(shape = 1, aes(text = sprintf("Ticker: %s", tic))) + 
  geom_smooth(aes(x = ni_lag, y = ni), method = lm, se = T) + 
  labs(x = "2017 Net Income ($M USD)",
       y = "2018 Net Income ($M USD)")
ggplotly(plot, tooltip = "text")

# How to predict GDP growth in Singapore
# GDP data from https://data.gov.sg/dataset/per-capita-gni-and-per-capita-gdp-at-current-market-prices-annual
library(readr)
dfg <- read_csv("../../Data/per-capita-gni-and-per-capita-gdp-at-current-market-prices-annual-2020.01.02.csv")
dfg <- dfg[dfg$level_1 == "Per Capita GDP", ]
dfg <- dfg[order(dfg$year), ]

# Unemployment data from https://stats.mom.gov.sg/Pages/UnemploymentTimeSeries.aspx
dfu <- read_csv("../../Data/overall-unemployment-rate-annual-2020.01.02.csv")
dfu <- dfu[order(dfu$year), ]

# merge into one dataset
df <- data.frame(year=1993:2018,
                 growth = dfg[dfg$year > 1992, ]$value / dfg[dfg$year > 1991 & dfg$year < 2018, ]$value - 1,
                 unemploy = dfu[dfu$year > 1992, ]$unemployment_rate,
                 unemploy_lag = dfu[dfu$year<2018, ]$unemployment_rate)

library(ggplot2)
suppressPackageStartupMessages(library(plotly))
cor <- cor(x = df$growth, y = df$unemploy, method = "pearson")

plot <- ggplot(df, aes(x = unemploy, y = growth)) + 
  geom_point(shape = 1, aes(text = sprintf("Year: %i", year))) + 
  geom_smooth(method = lm, se = T) + 
  labs(x = "Unemployment rate in SG",
       y = "GDP Growth in SG")
ggplotly(plot, tooltip = "text")


# Define a detector
# repeat the test with frequentist statistics
detector <- function() {
  dice <- sample(1:6, size = 2, replace = TRUE)
  if (sum(dice) == 12) {
    "exploded"
  } else {
    "still there"
  }
}

experiment <- replicate(1000, detector())
# p value
paste("p-value: ",
      sum(experiment == "still there") / 1000,
      "-- Reject H_A that sun exploded")

# Roll a dice
i <- 1
dice <- 0
times <- 10000
while (i <= times) {
  dice <- dice + sample(1:6, 1)
  i <- i + 1
}
paste("Roll", times, "times dice and the mean is", dice/times)

i <- 0; meandice <- c()
while (i <= 10000) {
  meandice <- append(meandice,
                     mean(sample(1:6, 30, replace = TRUE)))
  i <- i + 1
}
hist(meandice, col = "lightgreen", breaks = 20)
abline(v = 3.5, col = "blue")
abline(v = mean(meandice), col = "red")

library(tidyverse)
df <- read.csv("../../Data/Session_1-2.csv")
ggplot(data = df, aes(y = revtq, x = atq)) + 
  geom_point() + 
  geom_smooth(method = "lm") + 
  xlab("Assets") + 
  ylab("Revenue")

