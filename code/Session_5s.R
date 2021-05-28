# Define html_df() function for displaying small tables in html format
library(knitr)
library(kableExtra)
html_df <- function(text, cols = NULL, col1 = FALSE, full = FALSE) {
  if(!length(cols)) {
    cols = colnames(text)
  }
  if(!col1) {
    kable(text,"html", col.names = cols, align = c("l", rep('c', length(cols)-1))) %>%
      kable_styling(bootstrap_options = c("striped", "hover"), full_width = full)
  } else {
    kable(text, "html", col.names = cols, align = c("l", rep('c', length(cols) - 1))) %>%
      kable_styling(bootstrap_options = c("striped", "hover"), full_width = full) %>%
      column_spec(1, bold = TRUE)
  }
}

## # Happy Niu Year of 2021
## # The following is a package to produce word cloud, for your reference only
## library(tidyverse)
## text <- read_file("names.txt")
## 
## # Convert text to tokens with frequency count
## # We will cover this in a future topic
## library(tidytext)
## df_doc <- data.frame(text = c(text)) %>%
##   unnest_tokens(word, text)
## terms <- df_doc %>%
##   count(word, sort = TRUE) %>%
##   ungroup() %>%
##   rename(freq = n)
## 
## # must install from github, CRAN does not work
## # devtools::install_github("lchiffon/wordcloud2")
## library(wordcloud2) # Install from Github required
## terms <- as.data.frame(terms)
## letterCloud(terms, word = "NIU", wordSize = 1.5)

## mod <- glm(y ~ x1 + x2 + x3 + ..., data = df, family = binomial)
## 
## summary(mod)

# Import Walmart data from Session_4s_Kaggle
library(tidyverse)
library(lubridate)
library(broom)
weekly <- read.csv("../../Data/Session_4_WMT_train.csv")
weekly.features <- read.csv("../../Data/Session_4_WMT_features.csv")
weekly.stores <- read.csv("../../Data/Session_4_WMT_stores.csv")

preprocess_data <- function(df) {
  # Merge the data together (Pulled from outside of function -- "scoping")
  df <- left_join(df, weekly.stores)
  df <- left_join(df, weekly.features[, 1:11])
  
  # Compress the weird markdown information to 1 variable
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
  df$month <- month(df$date)
  
  df
}

df <- preprocess_data(weekly)
#df$id <- df$Store *10000 + df$week * 100 + df$Dept
df <- df %>%
  group_by(Store, Dept) %>% 
  mutate(store_avg = mean(Weekly_Sales, rm.na = T)) %>%
  ungroup()

# Create the binary variable from Walmart sales data
df$double <- ifelse(df$Weekly_Sales > df$store_avg * 2, 1, 0)
model1 <- glm(double ~ IsHoliday, data = df, family = binomial)
tidy(model1)

# Automating the above:
exp(coef(model1))

IsHoliday <- c(FALSE, TRUE)
test_data <- as.data.frame(IsHoliday)
predict(model1, test_data)  # log odds if no type = "response"

predict(model1, test_data, type = "response")  #probabilities

model2 <- glm(double ~ IsHoliday + Temperature + Fuel_Price,
              data = df, family = binomial)
tidy(model2)

# Odds
exp(coef(model2))

# Average probability in September
hday_sep    <- mean(predict(model2, filter(df, IsHoliday, month == 9),
                            type = "response"))
no_hday_sep <- mean(predict(model2, filter(df, !IsHoliday, month == 9),
                            type = "response"))
# Average probability in December
hday_dec    <- mean(predict(model2, filter(df, IsHoliday, month == 12),
                            type="response"))
no_hday_dec <- mean(predict(model2, filter(df, !IsHoliday, month == 12),
                            type="response"))

html_df(data.frame(Month=c(9, 9, 12, 12), IsHoliday=c(FALSE, TRUE, FALSE, TRUE),
                   Probability=c(no_hday_sep, hday_sep, no_hday_dec, hday_dec)))





## # Plot 3D logistic curve
## # https://stackoverflow.com/questions/36438615/producing-logistic-curve-for-my-logistic-regression-model
## model2.2 <- glm(double ~ Temperature + Fuel_Price, data = df, family = binomial)
## # Specify x-axis and y-axis variables range
## Temperature_grid <- seq(min(df$Temperature), max(df$Temperature), by = 2)
## Fuel_Price_grid <- seq(min(df$Fuel_Price), max(df$Fuel_Price), by = 0.05)
## # Create new data pair with length(Fuel_Price_grid) * length(Temperature_grid) records
## newdata <- data.frame(Temperature = rep(Temperature_grid, times = length(Fuel_Price_grid)),
##                       Fuel_Price =  rep(Fuel_Price_grid, each = length(Temperature_grid)))
## # Predict probability using the new pair of data
## pred = predict(model2.2, newdata, type = "response")
## # Specify z-axis which is the predicted probability
## z <- matrix(pred, length(Temperature_grid))
## # Plot the 3D graph, theta and phi to specify the angle
## # persp() is to draw perspective plots of a surface
## persp(Temperature_grid, Fuel_Price_grid, z, xlab = "Temperature", ylab = "Fuel_Price", zlab = "Predicted probability", main = "logistic curve (3D)", theta = 40, phi = 20, col = "red")
## 
## # For 2D, we must specify values for other variables
## # We define the following function to plot Temperature given different values of Fuel_Price
## curve_2D_fix_fuelprice <- function(model, Fuel_Price = 3, col = "black") {
##   Temperature_grid = seq(min(df$Temperature), max(df$Temperature), by = 5)
##   newdata <- data.frame(Temperature = Temperature_grid, Fuel_Price = Fuel_Price)
##   pred <- predict(model, newdata, type = "response")
##   plot(Temperature_grid, pred, xlab = "Temperature", ylab = "Predicted probability", type = "l",
##        col = col, main = "logistic curve (2D)")
##   abline(h = c(0, 0.5, 1), lty = 2, col = col)
##   }
## 
## curve_2D_fix_fuelprice(model2.2, Fuel_Price = 2, col = "blue")

percent <- function(x) {
  paste0(round(x * 100, 2), "%")
}
library(rlang)
# Another use for a quosure :)
# You could always just use quoted arguments here though
extract_margin <- function(m, x) {
  x <- enquo(x)
  percent(filter(summary(m), factor == quo_text(x))$AME)
}

# Calculate Average Marginal Effects (AME)
# It will take a while
library(margins)
m <- margins(model2)
m

summary(m) %>%
  html_df()

plot(m, which = summary(m)$factor)
# Note: The `which...` part is absolutely necessary at the moment
# due to a bug in the package (mismatch of factors and AME values
# you may try to remove `which...` to see what happened

margins(model2, at = list(IsHoliday = c(TRUE, FALSE)),
        variables = c("Temperature", "Fuel_Price")) %>%
  summary() %>%
  html_df()

margins(model2, at = list(Temperature = c(0, 20, 40, 60, 80)),
        variables = c("IsHoliday")) %>%
  summary() %>%
  html_df()


# load data
df <- read.csv("../../Data/Shipping/shipping_dataset.csv", na = "NA")
df_ships <- read.csv("../../Data/Shipping/index.csv", na = "NA")
df_ports <- read.csv("../../Data/Shipping/port_dataset.csv", na = "NA")
typhoon <- read.csv("../../Data/Shipping/typhoon.csv")

# rename lat and lon to avoid conflicts
df_ports <- df_ports %>% rename(port_lat = lat, port_lon = lon)

# Join in ships and ports
df <- left_join(df, df_ships)
df <- df %>% filter(!imo == 9528029 | reported_destination == "AU ADL")
df <- left_join(df, df_ports, by = c("left_port" = "port"))
df <- df %>% rename(left_lat = port_lat, left_lon = port_lon)
df <- left_join(df, df_ports, by = c("arrive_port" = "port"))
df <- df %>% rename(arrive_lat = port_lat, arrive_lon = port_lon)

# fix dates
df$last_update = as.POSIXct(df$last_update, tz = "UTC", origin = "1970-01-01")
df$left_time = as.POSIXct(df$left_time, tz = "UTC", origin = "1970-01-01")
df$arrive_time = as.POSIXct(df$arrive_time, tz = "UTC", origin = "1970-01-01")

# Fix typhoon dates
typhoon$date <- as.POSIXct(paste(typhoon$date, typhoon$time), format="%Y%b%d %H%M%S", tz="UTC", origin="1970-01-01")
# Fix typhoon longitude -- they have lon * -1 in the data
typhoon$lon <- typhoon$lon * - 1

typhoon_all <- typhoon
# filter to dates in sample
typhoon <- typhoon %>% filter(date > as.POSIXct("2018-08-30", tz = "UTC"))

df <- df %>% rename(frame = run)

df_all <- df[df$frame != 32, ]
df <- df %>% filter(last_update > as.POSIXct("2018-08-30", tz = "UTC"))

library(plotly)  # needed for the toRGB() function
geo <- list(
  showland = TRUE,
  showlakes = TRUE,
  showcountries = TRUE,
  showocean = TRUE,
  countrywidth = 0.5,
  landcolor = toRGB("grey90"),
  lakecolor = toRGB("aliceblue"),
  oceancolor = toRGB("aliceblue"),
  projection = list(
    type = 'orthographic',  # detailed at https://plot.ly/r/reference/#layout-geo-projection
    rotation = list(
      lon = 100,
      lat = 1,
      roll = 0
    )
  ),
  lonaxis = list(
    showgrid = TRUE,
    gridcolor = toRGB("gray40"),
    gridwidth = 0.5
  ),
  lataxis = list(
    showgrid = TRUE,
    gridcolor = toRGB("gray40"),
    gridwidth = 0.5
  )
)

typhoon_Aug31 <- typhoon[typhoon$date > as.POSIXct("2018-08-31 00:00:00", tz = "UTC") & typhoon$date < as.POSIXct("2018-09-01 00:00:00", tz = "UTC"), ]
typhoon_Jebi <- typhoon_all[typhoon_all$typhoon_name == "JEBI_Y", ]
df_Aug31 <- df[df$frame == 1, ]

library(plotly)  # for plotting
library(RColorBrewer)  # for colors
# plot with boats, ports, and typhoons
# Note: geo is defined in the code file -- it controls layout
palette = brewer.pal(8, "Dark2")[c(1, 8, 3, 2)]
p <- plot_geo(colors = palette) %>%
  add_markers(data = df_ports, x = ~port_lon, y = ~port_lat, color = "Port") %>%
  add_markers(data = df_Aug31, x = ~lon, y = ~lat, color = ~ship_type,
              text = ~paste('Ship name', shipname)) %>%
  add_markers(data = typhoon_Aug31, x = ~lon, y = ~lat, color = "TYPHOON",
              text = ~paste("Name", typhoon_name)) %>%
   layout(showlegend = TRUE, geo = geo,
    title = 'Singaporean owned container and tanker ships, August 31, 2018')
p

## library(plotly)  # for plotting
## library(RColorBrewer)  # for colors
## # plot with boats, ports, and typhoons
## # Note: geo is defined in the code file -- it controls layout
## palette = brewer.pal(8, "Dark2")[c(1, 8, 3, 2)]
## p <- plot_geo(colors = palette) %>%
##   add_markers(data = df_ports, x = ~port_lon, y = ~port_lat, color = "Port") %>%
##   add_markers(data = df_Aug31, x = ~lon, y = ~lat, color = ~ship_type,
##               text = ~paste('Ship name', shipname)) %>%
##   add_markers(data = typhoon_Aug31, x = ~lon, y = ~lat, color="TYPHOON",
##               text = ~paste("Name", typhoon_name)) %>%
##    layout(showlegend = TRUE, geo = geo,
##     title = 'Singaporean owned container and tanker ships, August 31, 2018')
## p

# plot with boats and typhoons
palette = brewer.pal(8, "Dark2")[c(1, 3, 2)]
p <- plot_geo(colors = palette) %>%
  add_markers(data = df_all[df_all$frame == 14,], x = ~lon, y = ~lat,
              color = ~ship_type, text = ~paste('Ship name', shipname)) %>%
  add_markers(data = typhoon_Jebi, x = ~lon, y = ~lat, color="Typhoon Jebi",
              text = ~paste("Name", typhoon_name, "</br>Time: ", date)) %>%
   layout(
    showlegend = TRUE, geo = geo,
    title = 'Singaporean container/tanker ships, September 4, 2018, evening')
p

## # plot with boats and typhoons
## palette = brewer.pal(8, "Dark2")[c(1, 3, 2)]
## p <- plot_geo(colors = palette) %>%
##   add_markers(data = df_all[df_all$frame == 14,], x = ~lon, y = ~lat,
##               color = ~ship_type, text = ~paste('Ship name', shipname)) %>%
##   add_markers(data = typhoon_Jebi, x = ~lon,
##               y = ~lat, color = "Typhoon Jebi",
##               text = ~paste("Name", typhoon_name, "</br>Time: ", date)) %>%
##    layout(showlegend = TRUE, geo = geo,
##           title = 'Singaporean ships, September 4, 2018, evening')
## p

library(leaflet) # another interactive map package
library(leaflet.extras)

# typhoon icons
icons <- pulseIcons(color = 'red',
  heartbeat = ifelse(typhoon_Jebi$intensity_vmax > 150/1.852, 0.8,
    ifelse(typhoon$intensity_vmax < 118/1.852, 1.6, 1.2)),
  iconSize=ifelse(typhoon_Jebi$intensity_vmax > 150/1.852, 12,
    ifelse(typhoon_Jebi$intensity_vmax < 118/1.852, 3, 8)))

# ship icons
shipicons <- iconList(
  ship = makeIcon("../../Figures/ship.png", NULL, 18, 18)
)
leaflet() %>%
  addTiles() %>% 
  setView(lng = 136, lat = 34, zoom = 4) %>%
  addPulseMarkers(data = typhoon_Jebi[seq(1, nrow(typhoon_Jebi), 5), ],
                  lng = ~lon, lat = ~lat, label = ~date, icon = icons) %>%
  addCircleMarkers(data = typhoon_Jebi[typhoon_Jebi$intensity_vmax > 150/1.852, ],
                   lng = ~lon, lat = ~lat, stroke = TRUE, radius = 12,
                   color = "red", label = ~date) %>%
  addCircleMarkers(data = typhoon_Jebi[typhoon_Jebi$intensity_vmax <= 150/1.852 &
                                         typhoon_Jebi$intensity_vmax > 118/1.852, ],
                   lng = ~lon, lat = ~lat, stroke = TRUE, radius = 12,
                   color = "red", label = ~date) %>%
  addCircleMarkers(data = typhoon_Jebi[typhoon_Jebi$intensity_vmax <= 118/1.852, ],
                   lng = ~lon, lat = ~lat, stroke = TRUE, radius = 12,
                   color = "red", label = ~date) %>%
  addMarkers(data = df_all[df_all$frame == 14, ], lng = ~lon, lat = ~lat,
             label = ~shipname, icon = shipicons)

## library(leaflet) # another interactive map package
## library(leaflet.extras)
## 
## # typhoon icons
## icons <- pulseIcons(color = 'red',
##   heartbeat = ifelse(typhoon_Jebi$intensity_vmax > 150/1.852, 0.8,
##     ifelse(typhoon$intensity_vmax < 118/1.852, 1.6, 1.2)),
##   iconSize=ifelse(typhoon_Jebi$intensity_vmax > 150/1.852, 12,
##     ifelse(typhoon_Jebi$intensity_vmax < 118/1.852, 3, 8)))
## 
## # ship icons
## shipicons <- iconList(
##   ship = makeIcon("../../Figures/ship.png", NULL, 18, 18)
## )
## leaflet() %>%
##   addTiles() %>%
##   setView(lng = 136, lat = 34, zoom = 4) %>%
##   addPulseMarkers(data = typhoon_Jebi[seq(1, nrow(typhoon_Jebi), 5), ],
##                   lng = ~lon, lat = ~lat, label = ~date, icon = icons) %>%
##   addCircleMarkers(data = typhoon_Jebi[typhoon_Jebi$intensity_vmax > 150/1.852, ],
##                    lng = ~lon, lat = ~lat, stroke = TRUE, radius = 12,
##                    color = "red", label = ~date) %>%
##   addCircleMarkers(data = typhoon_Jebi[typhoon_Jebi$intensity_vmax <= 150/1.852 &
##                                          typhoon_Jebi$intensity_vmax > 118/1.852, ],
##                    lng = ~lon, lat = ~lat, stroke = TRUE, radius = 12,
##                    color = "red", label = ~date) %>%
##   addCircleMarkers(data = typhoon_Jebi[typhoon_Jebi$intensity_vmax <= 118/1.852, ],
##                    lng = ~lon, lat = ~lat, stroke = TRUE, radius = 12,
##                    color = "red", label = ~date) %>%
##   addMarkers(data = df_all[df_all$frame == 14, ], lng = ~lon, lat = ~lat,
##              label = ~shipname, icon = shipicons)

# Load datas
df3 <- read.csv("../../Data/Shipping/combined.csv")

library(geosphere)

# Calculate distance to and from ports
df3$dist_toport <- distVincentyEllipsoid(as.matrix(df3[ , c("lon", "lat")]),
                                         as.matrix(df3[ , c("arrive_lon", "arrive_lat")]))
df3$dist_fromport <- distVincentyEllipsoid(as.matrix(df3[ , c("lon", "lat")]),
                                           as.matrix(df3[ , c("left_lon", "left_lat")]))
df3 <- df3 %>%
  arrange(imo, last_update) %>%
  group_by(imo) %>%
  mutate(delayed = ifelse(difftime(arrive_time, lead(arrive_time), units = "days") > 1/8 &
                            arrive_time > last_update & left_port == lead(left_port), 1, 0)) %>%
  ungroup()

library(geosphere)
x <- as.matrix(df3[ , c("lon", "lat")])  # ship location
y <- as.matrix(df3[ , c("ty_lon", "ty_lat")]) # typhoon location

df3$dist_typhoon <- distVincentyEllipsoid(x, y) / 1000 # convert to KM

df3$typhoon_500 = ifelse(df3$dist_typhoon < 500 & 
                         df3$dist_typhoon >= 0, 1, 0)
df3$typhoon_1000 = ifelse(df3$dist_typhoon < 1000 &
                          df3$dist_typhoon >= 500, 1, 0)
df3$typhoon_2000 = ifelse(df3$dist_typhoon < 2000 &
                          df3$dist_typhoon >= 1000, 1, 0)

fit1 <- glm(delayed ~ typhoon_500 + typhoon_1000 + typhoon_2000,
            data = df3, family = binomial)
summary(fit1)

odds1 <- exp(coef(fit1))
odds1

m1 <- margins(fit1)
summary(m1)

prob_odds1 <- c(exp(coef(fit1)[1]),
                exp(coef(fit1)[c(2, 3, 4)] + coef(fit1)[c(1, 1, 1)]))
probability1 <- prob_odds1 / (1 + prob_odds1)
probability1

# 1 knot (nautical mile/h) = 1.852 km/h
# cut() makes a categorical variable out of a numerical variable
# using specified bins
df3$Super <- ifelse(df3$intensity_vmax * 1.852 >= 185, 1, 0)
df3$Moderate <- ifelse(df3$intensity_vmax * 1.852 >= 88 &
                         df3$intensity_vmax * 1.852 < 185, 1, 0)
df3$Weak <- ifelse(df3$intensity_vmax * 1.852 >= 41 & 
                   df3$intensity_vmax * 1.852 < 88, 1, 0)
df3$HK_intensity <- cut(df3$intensity_vmax * 1.852, c(-1, 41, 62, 87, 117,
                                                      149, 999))
table(df3$HK_intensity)

fit2 <- glm(delayed ~ (typhoon_500 + typhoon_1000 + typhoon_2000) : 
              (Weak + Moderate + Super), data = df3,
            family = binomial)
tidy(fit2)

odds2 <- exp(coef(fit2))
odds2[c(1, 3, 10)]

prob_odds2 <- c(exp(coef(fit2)[1]),
                exp(coef(fit2)[c(3, 10)] + coef(fit2)[c(1, 1)]))
probability2 <- prob_odds2 / (1 + prob_odds2)
probability2

m2 <- margins(fit2)
summary(m2) %>%
  html_df()

