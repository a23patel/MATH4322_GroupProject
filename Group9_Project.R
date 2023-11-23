# Highway Hustle - Unraveling Metro Interstate Traffic Trends

# IMPORTING THE DATASET

traffic = read.csv("C:\\KevinWorkArea\\CollegeAcademics\\Fall2023\\math4322\\GroupProject\\Metro_Interstate_Traffic_Volume.csv")
summary(traffic)

# Printing the first 6 rows of the dataset
head(traffic)

# checking if there are any missing values in the entire dataset (returns TRUE if there are missing values , otherwise FALSE)
any(is.na(traffic))

# Converting the temperatures from Kelvin to Farenheit
traffic$temp <- (traffic$temp - 273.15) * 9/5 + 32

# DATA VISUALIZATIONS
library(ggplot2)
ggplot(traffic, aes(x = temp, y = factor(1))) +
  geom_boxplot() +
  labs(x = "Temperature", y = NULL) 
ggtitle("Boxplot of Temperature")

ggplot(traffic, aes(x = rain_1h, y = factor(1))) +
  geom_boxplot() +
  labs(x = "Rain", y = NULL) 
ggtitle("Boxplot of Rain")

# From the the above boxplots, we observe that there is an outlier in both the plots which could affect model performance.
#So, we will remove the outliers when we do data cleaning.

ggplot(traffic, aes(y = holiday, fill=holiday)) +
  geom_bar(show.legend=FALSE) +
  scale_fill_manual(values=c("lightblue", "lightcoral", "lightgreen", "yellow", "red", "green", "pink", "purple","gold", "maroon","blue", 'seagreen'))+
  labs(y = "Holiday") +
  theme_minimal() +
  ggtitle("Countplot of Holidays") 

# We notice that 'None' is far greater than the other holiday days. So, we will now remove 'None' to visualize the other holidays

ggplot(subset(traffic, holiday != "None"), aes(y = holiday, fill=holiday)) +
  geom_bar(show.legend = FALSE) +
  scale_fill_manual(values= c("lightblue", "lightcoral", "lightgreen", "yellow", "red", "green", "pink", "purple","gold", "maroon","blue")) +
  labs(y = "Holiday") +
  theme_minimal() +
  ggtitle("Countplot of Holidays")

ggplot(traffic, aes(x = snow_1h)) +
  geom_histogram(fill = "lightblue", color = "black") +
  labs(x = "Snow (mm)", y = "Frequency") +
  ggtitle("Histogram of Snow") +
  theme_minimal()

# DATA CLEANING (PART OF DATA PRE-PROCESSING) - IMPORTANT PART IN DATA MINING PROCESS

traffic$date_time <- strptime(traffic$date_time, format = "%d-%m-%Y %H:%M")

# Formatting the date_time column in the desired format (%Y-%m-%d %H:%M)
traffic$date_time <- strftime(traffic$date_time, format = "%Y-%m-%d %H:%M")

# Extracting additional features from date_time variable (For weekdays, Monday is 0 and Sunday is 6)
# The hours from 4 to 7 refers to Early Morning, hours from 8 to 11 refer to Morning 
# Hours from 12 to 15 refer to Afternoon, hours from 16 to 19 refer to Evening.
# Hours from 20 to 23 refer to Night and hours from 24 to 3 refer to Late Night.
traffic$date_time = as.POSIXct(traffic$date_time)
traffic$date = as.Date(traffic$date_time)
traffic$weekday = as.factor(format(traffic$date_time, "%w"))
traffic$hour = as.numeric(format(traffic$date_time, "%H"))
traffic$month = as.factor(format(traffic$date_time, "%m"))
traffic$year = as.factor(format(traffic$date_time, "%Y"))

# Removing the outlier in temp variable 
traffic <- traffic[traffic$temp > -400, ]

# Removing the outlier in rain_1h variable
traffic <- traffic[traffic$rain_1h <2500, ]

# Since other holidays are very sparse compared to none holidays.Hence, we will encode holidays as 0 (None) or 1 (holidays exist)
traffic$holiday <- ifelse(traffic$holiday == 'None', 0, 1)

# Defining the list of weather conditions (thunderstorm, mist, fog, haze are the most distinct weather condition)
# So, we will replace rows containing "thunderstorm" with "thunderstorm" and replace other weather conditions with "other"
weather_conditions <- c("thunderstorm", "mist", "fog", "haze")
traffic$weather_description <- ifelse(grepl("thunderstorm", traffic$weather_description), "thunderstorm", traffic$weather_description)
traffic$weather_description <- ifelse(!(traffic$weather_description %in% weather_conditions), "other", traffic$weather_description)
table(traffic$weather_description)  # counts the occurrences of each weather condition

#creating dummy variables for weather_description column (encoding the categorical variables which would help in model creation)
traffic <- cbind(traffic, model.matrix(~0 + weather_description, data = traffic))

head(traffic)  # printing first 6 rows of the cleaned dataset so far

# Renaming the the encoded columns of weather_description
colnames(traffic)[colnames(traffic) == "weather_descriptionfog"] <- "fog"
colnames(traffic)[colnames(traffic) == "weather_descriptionhaze"] <- "haze"
colnames(traffic)[colnames(traffic) == "weather_descriptionmist"] <- "mist"
colnames(traffic)[colnames(traffic) == "weather_descriptionthunderstorm"] <- "thunderstorm"

# Separating snow_1h into categories such as "snow" and "no_snow"
traffic$snow_1h <- ifelse(traffic$snow_1h > 0, "snow", "no_snow")
table(traffic$snow_1h) 

# creating new column snow_present (binary variable) which specifies if there is a snow or not
traffic$snow_present <- ifelse(traffic$snow_1h == "snow", 1, 0)

head(traffic)  # printing the first 6 rows of the cleaned dataset so far

# Dropping the unnecessary or not required columns (date_time, weather_descriptionother column, weather_description and weather_main column)
# Dropped the date_time column because we already extracted the features from the date_time column.
# Dropped the rain_1h and snow_1h since we already one-hot encoded these columns.
traffic <- traffic[, !colnames(traffic) %in% c("date_time", "snow_1h", "weather_description","weather_descriptionother", "weather_main")]

# Checking if there are negative or zero values in traffic_volume column
if (any(traffic$traffic_volume < 0)) {
  cat("There are negative values in traffic_volume column.\n")
} else if (any(traffic$traffic_volume == 0)) {
  cat("There are zero values in traffic_volume column.\n")
} else {
  cat("There are no negative or zero values in traffic_volume column.\n")
}

# Excluding rows with zero or negative traffic_volume
traffic <- traffic[traffic$traffic_volume > 0, ]

any(is.nan(traffic$traffic_volume))
any(is.infinite(traffic$traffic_volume))

# Print the first few rows of the cleaned dataset and displaying summary of the cleaned dataset
head(traffic)
summary(traffic)


# DATA MODELING: Linear Regression
traffic.lm = lm(traffic_volume ~ ., data=traffic)
traffic.lm
summary(traffic.lm)

# plotting the linear regression and seeing if any assumptions are violated
par(mfrow=c(2,2))
plot(traffic.lm)

MSE = rep(0,10)

# Loop for 10 iterations
for (i in 1:10) {
  set.seed(i) 
  sample <- sample(1:nrow(traffic), 0.8 * nrow(traffic))
  train_data <- traffic[sample, ]
  test_data <- traffic[-sample, ]
  
  #scaling numeric variables to have zero mean and unit variance (min-max scaling)
  numeric_cols <- sapply(train_data, is.numeric)
  train_data_scaled <- as.data.frame(scale(train_data[, numeric_cols]))
  test_data_scaled <- as.data.frame(scale(test_data[, numeric_cols]))
  
  traffic.lm <- lm(traffic_volume ~ ., data = train_data_scaled)
  
  yhat <- predict(traffic.lm, newdata = test_data_scaled)
  MSE[i] = mean((yhat - test_data_scaled$traffic_volume)^2)
}

cat("MSE Values:", MSE, "\n")
cat("Average Test MSE:", mean(MSE), "\n")


# DATA MODELING: Random Forest
library(randomForest)

#split into train and test 80/20
train = sample(1:nrow(traffic), floor(nrow(traffic)*.8))
traffic.train = traffic[train, ]
traffic.train
traffic.test = traffic[-train, ]
traffic.test
#random forest model
traffic.rf = randomForest(traffic_volume ~., data = traffic, subset = train, mtry = (ncol(traffic)-1) / 3, importance = TRUE)
traffic.rf
summary(traffic.rf)

#calculate Test MSE
yhat.rf = predict(traffic.rf, newdata = traffic.test)
yhat.rf
mean((traffic.test$traffic_volume-yhat.rf)^2)

