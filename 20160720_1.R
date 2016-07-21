# 1) What are the dimensions (number of rows and number of columns) of the data set?

dim(airquality)

# 1) What are the dimensions (number of rows and number of columns) of the data set?
dim(airquality)
# [1] 153   6

# 2) What is the name of the last column of the data set?
colnames(airquality)[dim(airquality)[2]]
#[1] "Day"

# 3) For the same column, what is the datatype?
typeof(airquality$Day)
#[1] "integer"

# 4) List all values for the 50th item (values in line 50) in the data set?
airquality[50, ]
# Ozone Solar.R Wind Temp Month Day
# 50    12     120 11.5   73     6  19

# 5) Remove from the data all records with NA in the first column in the data set.
mydata <- airquality[complete.cases(airquality[, 1]), ]

# 6) List all values for the new 50th item (values in line 50) in the data set?
mydata[50, ]
# Ozone Solar.R Wind Temp Month Day
# 79    61     285  6.3   84     7  18

# 7) Remove all incomplete cases from the dataset (row with an NA in any column).
mydata2 <- airquality[complete.cases(airquality[, 1:6]), ]

# 8) What are the dimensions of the data set for all complete cases?
dim(mydata2)
# [1] 111   6

# 9) Get the date (day and month) with the highest Ozone value for all complete cases in the dataset.
mydata2[mydata2$Ozone == max(mydata2$Ozone),c(6,5)]
# Day Month
# 117  25     8

# 10) Create a subset with only the 7th and 9th months.
mydata3<-airquality[airquality$Month == 7 | airquality$Month == 9,]

# 11) What is the average temperature in the subset created in question 10?
mean(mydata3$Temp, na.rm = TRUE)
# [1] 80.45902

# 12) What is Fridays' average temperature of the subset created in question 10?
# add a column to the dataframe which contains the day of week (0: Sunday, 1: Monday, etc)
mydata3$wd<-apply(mydata3, 2, function(x) as.POSIXlt(paste("2016", mydata3$Month, mydata3$Day, sep="-"))$wday)[,1]
# > head(mydata3)
# Ozone Solar.R Wind Temp Month Day wd
# 62   135     269  4.1   84     7   1  5
# 63    49     248  9.2   85     7   2  6
# 64    32     236  9.2   81     7   3  0
# 65    NA     101 10.9   84     7   4  1
# 66    64     175  4.6   83     7   5  2
# 67    40     314 10.9   83     7   6  3

# Apply mean function to Temp column grouped by newly created column wd and get the 6th position of the array
# (not the wd value equals to 6 which would be Saturday, but the 6th occurence of the results array)
tapply(mydata3$Temp, mydata3$wd, mean)[6]
# 5 
# 81.7


# 13) Create a table containing the Wind's average, minimum value, 25th percentile (2nd quartile), 
# median value, 75th percentile (3rd quartile) and maximum value for May and August (month 5 and 8) 
# complete cases for all days in the dataset. (Consider the data is for the year 2016)

# 14) Create a table containing the Wind's average, minimum value, 25th percentile (2nd quartile), 
#median value, 75th percentile (3rd quartile) and maximum value for May and August (month 5 and 8) 
# complete cases for all days except Mondays in the dataset. (Consider the data is for the year 2016)