# Highway Hustle - Unraveling Metro Interstate Traffic Trends

# IMPORTING THE DATASET

traffic = read.csv("C:/Users/hp/Downloads/Metro_Interstate_Traffic_Volume.csv")
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
traffic$date_time = as.POSIXct(traffic$date_time)
traffic$weekday = as.numeric(format(traffic$date_time, "%w"))
traffic$date = as.Date(traffic$date_time)
traffic$hour = as.numeric(format(traffic$date_time, "%H"))
traffic$month = as.numeric(format(traffic$date_time, "%m"))
traffic$year = as.numeric(format(traffic$date_time, "%Y"))

# Classifying the hours as Early Morning, Morning, Afternoon, Evening, Night, Late Night
modify_hours <- function(x) {
  Early_Morning <- c(4, 5, 6, 7)
  Morning <- c(8, 9, 10, 11)
  Afternoon <- c(12, 13, 14, 15)
  Evening <- c(16, 17, 18, 19)
  Night <- c(20, 21, 22, 23)
  Late_Night <- c(24, 1, 2, 3)
  
  if (x %in% Early_Morning) {
    return('Early_Morning')
  } else if (x %in% Morning) {
    return('Morning')
  } else if (x %in% Afternoon) {
    return('Afternoon')
  } else if (x %in% Evening) {
    return('Evening')
  } else if (x %in% Night) {
    return('Night')
  } else {
    return('Late_Night')
  }
}

# Applying the modify_hours function to the 'hour' column
traffic$hour <- sapply(traffic$hour, modify_hours)
head(traffic)

# Removing the outlier in temp variable 
traffic <- traffic[traffic$temp > -400, ]

# Removing the outlier in rain_1h variable
traffic <- traffic[traffic$rain_1h <2500, ]

# Since other holidays are very sparse compared to none holidays.Hence, we will encode holidays as TRUE or FALSE
traffic$holiday <- ifelse(traffic$holiday == 'None', FALSE, TRUE)

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

# Dropping the unnecessary or not required columns (weather_descriptionother column, weather_description and weather_main column)
traffic <- traffic[, !colnames(traffic) %in% c("weather_description","weather_descriptionother", "weather_main")]

# Separating "rain_1h" into different categories (light, moderate, heavy)
traffic$rain_1h <- ifelse(traffic$rain_1h == 0, "no_rain",
                          ifelse(traffic$rain_1h <= 1, "light",
                                 ifelse(traffic$rain_1h <= 10, "moderate", "heavy")))


# Encoding "snow_1h" column  as "snow" or "no_snow"
traffic$snow_1h[traffic$snow_1h > 0] <- "snow"
traffic$snow_1h[traffic$snow_1h == 0] <- "no_snow"
table(traffic$snow_1h) # Counting the occurrences of "snow" and "no_snow"

# converting all categorical variables into factors to make it suitable for the regression models
traffic$holiday <- as.factor(traffic$holiday)
traffic$hour <- as.factor(traffic$hour)
traffic$rain_1h <- as.factor(traffic$rain_1h)
traffic$snow_1h <- as.factor(traffic$snow_1h)

# Print the first few rows of the cleaned dataset and displaying summary of the cleaned dataset
head(traffic)
summary(traffic)

