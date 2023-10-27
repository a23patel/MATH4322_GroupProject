# Highway Hustle - Unraveling Metro Interstate Traffic Trends

# Importing the dataset
traffic = read.csv("C:/Users/hp/Downloads/Metro_Interstate_Traffic_Volume.csv")
attach(traffic)
summary(traffic)

# Printing the first 6 rows of the dataset
head(traffic)

# checking if there are any missing values in the entire dataset (returns TRUE if there are missing values , otherwise FALSE)
any(is.na(traffic))

# Data cleaning 
