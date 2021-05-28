# import all real estate companies excluding North America
# data extracted from Compustat Global, industry code gind = 601020
library(tidyverse)
df <- read.csv("../../Data/Session_2-1.csv")
df_full <- df
uol <- filter(df, isin == "SG1S83002349") # for UOL only
# clean_df <- subset(df, fyear == 2017 & !is.na(revt) & !is.na(ni) & revt > 1)

# revt: Revenue, at: Assets
summary(uol[ , c("revt", "at")])



mod1 <- lm(revt ~ at, data = uol)
summary(mod1)

# Graph showing squared error
uolg <- uol[ , c("at","revt")]
uolg$resid <- mod1$residuals
uolg$xleft <- ifelse(uolg$resid < 0, uolg$at, uolg$at - uolg$resid)
uolg$xright <- ifelse(uolg$resid < 0, uolg$at - uolg$resid, uol$at)
uolg$ytop <- ifelse(uolg$resid < 0, uolg$revt - uolg$resid, uol$revt)
uolg$ybottom <- ifelse(uolg$resid < 0, uolg$revt, uolg$revt - uolg$resid)
uolg$point <- TRUE

uolg2 <- uolg
uolg2$point <- FALSE
uolg2$at <- ifelse(uolg$resid < 0, uolg2$xright, uolg2$xleft)
uolg2$revt <- ifelse(uolg$resid < 0, uolg2$ytop, uolg2$ybottom)

uolg <- rbind(uolg, uolg2)

uolg %>% ggplot(aes(y = revt, x = at, group = point)) + 
         geom_point(aes(shape = point)) + 
         scale_shape_manual(values = c(NA, 18)) + 
         geom_smooth(method = "lm", se = FALSE) +
         geom_errorbarh(aes(xmax = xright, xmin = xleft)) + 
         geom_errorbar(aes(ymax = ytop, ymin = ybottom)) + 
         theme(legend.position = "none")

# tidyverse with pipe %>%
uol <- uol %>%
  mutate(revt_growth1 = revt / lag(revt, order_by = fyear) - 1)

# which is equivalent to
uol <- mutate(uol, revt_growth2 = revt / lag(revt) - 1)

# Base R way, [-n] to index vector less the nth element
uol$revt_growth3 = uol$revt / c(NA, uol$revt[-length(uol$revt)]) - 1
identical(uol$revt_growth1, uol$revt_growth3)

# magrittr %<>% to combine <- and %>%
library(magrittr)
uol %<>% mutate(revt_growth4 = revt / lag(revt) - 1)
identical(uol$revt_growth1, uol$revt_growth4)

# Use order_by if data not already ordered
dff <- data.frame(year = 2001:2003, value = (1:3) ^ 2)
scrambled <- dff[sample(nrow(dff)), ]

wrong <- mutate(scrambled, prev = lag(value))
arrange(wrong, year)

right <- mutate(scrambled, prev = lag(value, order_by = year))
arrange(right, year)

# Make the other needed change
uol <- uol %>%
  mutate(at_growth = at / lag(at) - 1)  # From dplyr
# Rename our revenue growth variable
uol <- rename(uol, revt_growth = revt_growth1)  # From dplyr
# Run the OLS model
mod2 <- lm(revt_growth ~ at_growth, data = uol)
summary(mod2)

# lct: short term liabilities, che: cash and equivalents, ebit: EBIT
uol <- uol %>%
  mutate_at(vars(lct, che, ebit), funs(growth = . / lag(.) - 1))
mod3 <- lm(revt_growth ~ lct_growth+che_growth+ebit_growth, data=uol)
summary(mod3)

anova(mod2, mod3, test = "Chisq")

# Ensure firms have at least $1M (local currency), and have revenue
# df contains all real estate companies excluding North America
df_clean <- filter(df, df$at > 1, df$revt > 0)

# We cleaned out 2,177 observations!
print(c(nrow(df), nrow(df_clean)))

# Another useful cleaning function:
# Replaces NaN, Inf, and -Inf with NA for all numeric variables!
df_clean <- df_clean %>%
  mutate_if(is.numeric, funs(replace(., !is.finite(.), NA)))

uol <- uol %>% mutate(revt_lead = lead(revt))  # From dplyr
forecast1 <-
  lm(revt_lead ~ lct + che + ebit, data=uol)
library(broom)  # To display regression outputs in a tidy fashion
tidy(forecast1)  # present regression output
glance(forecast1)  # present regression statistics

forecast2 <- 
  lm(revt_lead ~ revt + act + che + lct + dp + ebit , data=uol)
tidy(forecast2)

glance(forecast2)

anova(forecast1, forecast2, test="Chisq")

# Create wide dataset using expand.grid()
university_wide <- data.frame(
    expand.grid(university = c("SMU", "NTU", "NUS"),
                rand.2016 = round(runif(1, 1, 100),0),
                rand.2017 = round(runif(1, 1, 100),0),
                rand.2018 = round(runif(1, 1, 100),0)))

university_wide # randomly generated numbers

# convert wide to long dataset
library("tidyr", "dplyr")
university_long <- university_wide %>%
    gather(year, rand, rand.2016:rand.2018) %>%
    mutate(year = as.numeric(gsub("rand.", "", year))) %>%
    arrange(desc(year))
university_long

# group_by - without it, lead() will pull from the subsequent firm!
# ungroup() tells R that we finished grouping
df_clean <- df_clean %>% 
  group_by(isin) %>% 
  mutate(revt_lead = lead(revt)) %>%
  ungroup()

forecast3 <- lm(revt_lead ~ revt + act + che + lct + dp + ebit,
                data = df_clean[df_clean$fic == "SGP", ])
tidy(forecast3)

glance(forecast3)

forecast4 <-
  lm(revt_lead ~ revt + act + che + lct + dp + ebit , data = df_clean)
tidy(forecast4)

glance(forecast4)

forecast3.1 <-
  lm(revt_lead ~ revt + act + che + lct + dp + ebit + factor(isin),
     data = df_clean[df_clean$fic == "SGP", ])
# n=7 to prevent outputting every fixed effect
print(tidy(forecast3.1), n=7)

glance(forecast3.1)
anova(forecast3, forecast3.1, test="Chisq")

library(lfe)
forecast3.2 <-
  felm(revt_lead ~ revt + act + che + lct + dp + ebit | factor(isin),
       data = df_clean[df_clean$fic == "SGP", ])
summary(forecast3.2)

df_clean %>%
  filter(fic == "SGP") %>%
  group_by(isin) %>%
  mutate(mean_revt_lead = mean(revt_lead, na.rm = T)) %>%
  slice(1) %>%
  ungroup() %>%
  ggplot(aes(x = mean_revt_lead)) + geom_histogram(aes(y = ..density..)) +  geom_density(alpha = .4, fill = "#FF6666")

expectations <- read_csv("../../Data/general-business-expectations-by-detailed-services-industry-quarterly.csv") %>%
  mutate(year = as.numeric(substr(quarter, 1, 4))) %>%    # split out year
  mutate(quarter = as.numeric(substr(quarter, 7, 7))) %>% # split out quarter
  mutate(value = as.numeric(value))                       # Ensue value is numeric

library(DT)
expectations %>%
  arrange(level_2, level_3, desc(year)) %>%  # sort the data
  select(year, quarter, level_2, level_3, value) %>%
  datatable(options = list(pageLength = 3), rownames=FALSE)

# extract out F&I only, calculate annual average value
expectations_avg <- expectations %>%
  filter(level_2 == "Financial & Insurance") %>%     # Keep F&I sector
  group_by(year) %>%                                 # Group data by year
  mutate(fin_sentiment=mean(value, na.rm=TRUE)) %>%  # Calculate yearly average
  slice(1)                                           # Take only 1 row per group
head(expectations_avg)

# subset out our data, since our macro data is Singapore-specific
df_SG <- df_clean %>% filter(fic == "SGP")

# Create year in df_SG (date is given by datadate as YYYYMMDD)
df_SG$year = round(df_SG$datadate / 10000, digits=0)

# Combine datasets
# Notice how it automatically figures out to join by "year"
df_SG_macro <- left_join(df_SG,
                         expectations_avg[,c("year","fin_sentiment")])

macro1 <- lm(revt_lead ~ revt + act + che + lct + dp + ebit + fin_sentiment,
             data = df_SG_macro)
tidy(macro1)

r_min <- min(df_SG_macro$fin_sentiment, na.rm=T)
r_max <- max(df_SG_macro$fin_sentiment, na.rm=T)

df_SG_macro %>%
  ggplot(aes(y=revt_lead,
             x=fin_sentiment)) + 
  geom_point()

df_SG_macro %>%
  ggplot(aes(y=revt_lead,
    x=scale(fin_sentiment)*revt)) + 
  geom_point()

# Scale creates z-scores with 0 mean and 1 sd
df_SG_macro$fin_sent_scaled <- scale(df_SG_macro$fin_sentiment)
summary(df_SG_macro[,c("fin_sentiment", "fin_sent_scaled")])

df_SG_macro %>%
  ggplot(aes(x=fin_sentiment)) +
  geom_histogram(aes(y=..density..),      # Histogram with density instead of count on y-axis
                 binwidth=4.1,
                 colour="black", fill="white") +
  geom_density(alpha=.2, fill="#FF6666") +  # Overlay with transparent density plot
  geom_vline(aes(xintercept=mean(fin_sentiment, na.rm=T)),   # Ignore NA values for mean
             color="red", linetype="dashed", size=1)

df_SG_macro %>%
  ggplot(aes(x=fin_sent_scaled)) +
  geom_histogram(aes(y=..density..),      # Histogram with density instead of count on y-axis
                 binwidth=0.22,
                 colour="black", fill="white") +
  geom_density(alpha=.2, fill="#FF6666") +  # Overlay with transparent density plot
  geom_vline(aes(xintercept=mean(fin_sent_scaled, na.rm=T)),   # Ignore NA values for mean
             color="red", linetype="dashed", size=1)

# Scale creates z-scores
df_SG_macro$fin_sent_scaled <- scale(df_SG_macro$fin_sentiment)
macro3 <-
  lm(revt_lead ~ revt + act + che + lct + dp + ebit + fin_sent_scaled:revt,
     data=df_SG_macro)  # fin_sent_scaled:revt = fin_sent_scaled * revt
tidy(macro3)

baseline <-
  lm(revt_lead ~ revt + act + che + lct + dp + ebit,
     data=df_SG_macro[!is.na(df_SG_macro$fin_sentiment),])
glance(baseline)
glance(macro3)

anova(baseline, macro3, test="Chisq")

r_sd <- round(sd(df_SG_macro$fin_sentiment, na.rm=T),1)
r_min <- min(df_SG_macro$fin_sentiment, na.rm=T)
r_max <- max(df_SG_macro$fin_sentiment, na.rm=T)
r_mean <- mean(df_SG_macro$fin_sentiment, na.rm=T)
rev <- macro3$coefficients[["revt:fin_sent_scaled"]]
r_rev = round(100 * rev,0)
rev_min <- round((((r_min - r_mean) / r_sd) * rev)*100, 1)
rev_max <- round((((r_max - r_mean) / r_sd) * rev)*100, 1)

p_uol <- predict(forecast2, uol[uol$fyear==2018,])
p_base <- predict(baseline,
  df_SG_macro[df_SG_macro$isin=="SG1S83002349" & df_SG_macro$fyear==2018,])
p_macro <- predict(macro3,
  df_SG_macro[df_SG_macro$isin=="SG1S83002349" & df_SG_macro$fyear==2018,])
p_world <- predict(forecast4,
  df_clean[df_clean$isin=="SG1S83002349" & df_clean$fyear==2018,])
preds <- c(p_uol, p_base, p_macro, p_world)
names(preds) <- c("UOL 2019 UOL", "UOL 2019 Base", "UOL 2019 Macro",
                  "UOL 2019 World")
preds

library(plotly)
df_SG_macro$pred_base <- predict(baseline, df_SG_macro)
df_SG_macro$pred_macro <- predict(macro3, df_SG_macro)
df_clean$pred_world <- predict(forecast4, df_clean)
uol$pred_uol <- predict(forecast2, uol)
# create a df for the 2019 forecasts from the four models
# to be consistent with other data, we use lagged fyear which indicates the lead forecast, ie, 2019 forecast
df_preds <- data.frame(preds=preds, fyear=c(2018,2018,2018,2018), model=c("UOL only", "Base", "Macro", "World"))

# The actual revenue, as y is the lead, so fyear < 2019
# for predictions, we plot with line from 1989 to 2017
# and then plot the point for 2018 (for 2019 forecast) separately (the last geom_point)
plot <- ggplot() + 
  geom_point(data=df_SG_macro[df_SG_macro$isin=="SG1S83002349" & df_SG_macro$fyear < 2019,], aes(y=revt_lead,x=fyear, color="Actual")) +
  geom_line(data=df_SG_macro[df_SG_macro$isin=="SG1S83002349" & df_SG_macro$fyear < 2019,], aes(y=revt_lead,x=fyear, color="Actual")) + 
  geom_point(data=uol[uol$fyear < 2018,], aes(y=pred_uol,x=fyear, color="UOL only")) +
  geom_line(data=uol[uol$fyear < 2018,], aes(y=pred_uol,x=fyear, color="UOL only")) +
  geom_point(data=df_SG_macro[df_SG_macro$isin=="SG1S83002349" & df_SG_macro$fyear < 2018,], aes(y=pred_base,x=fyear, color="Base")) +
  geom_line(data=df_SG_macro[df_SG_macro$isin=="SG1S83002349" & df_SG_macro$fyear < 2018,], aes(y=pred_base,x=fyear, color="Base")) +
  geom_point(data=df_SG_macro[df_SG_macro$isin=="SG1S83002349" & df_SG_macro$fyear < 2018,], aes(y=pred_macro,x=fyear, color="Macro")) +
  geom_line(data=df_SG_macro[df_SG_macro$isin=="SG1S83002349" & df_SG_macro$fyear < 2018,], aes(y=pred_macro,x=fyear, color="Macro")) + 
  geom_point(data=df_clean[df_clean$isin=="SG1S83002349" & df_clean$fyear < 2018,], aes(y=pred_world,x=fyear, color="World")) +
  geom_line(data=df_clean[df_clean$isin=="SG1S83002349" & df_clean$fyear < 2018,], aes(y=pred_world,x=fyear, color="World")) + 
  geom_point(data=df_preds, aes(y=preds, x=fyear, color=model), size=1.5, shape=18)
ggplotly(plot)

actual_series <- df_SG_macro[df_SG_macro$isin=="SG1S83002349" & df_SG_macro$fyear < 2019,]$revt_lead
uol_series <- uol[uol$fyear < 2019,]$pred_uol
base_series <- df_SG_macro[df_SG_macro$isin=="SG1S83002349" & df_SG_macro$fyear < 2019,]$pred_base
macro_series <- df_SG_macro[df_SG_macro$isin=="SG1S83002349" & df_SG_macro$fyear < 2019,]$pred_macro
world_series <- df_clean[df_clean$isin=="SG1S83002349" & df_clean$fyear < 2019,]$pred_world

# series data is calculated, see the R code file
# Root Mean Square Error (RMSE)
# the st. deviation of the residuals (prediction errors).
rmse <- function(v1, v2) {
  sqrt(mean((v1 - v2)^2, na.rm=T))
}

rmse <- c(rmse(actual_series, uol_series),
          rmse(actual_series, base_series),
          rmse(actual_series, macro_series),
          rmse(actual_series, world_series))
names(rmse) <- c("UOL 2019 UOL", "UOL 2019 Base",
                 "UOL 2019 Macro", "UOL 2019 World")
rmse
