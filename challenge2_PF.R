# Using the 331 files from specdata 
# 
# 1) Create a single function to load the dataset of complete cases 
# given the folder location:eg. load_complete('specdata') 
loadcomplete <- function(directory) {
  ## "directory" is a character vector of length 1 indicating
  ## the location of the CSV files
  
  ## prepare a vector of filenames to be processed
  id <- c(1:332)
  j <- str_pad(id, 3, pad = "0")
  k <- paste(j, "csv", sep = ".")
  
  ## creates an empty data frame with proper column names
  u <-
    data.frame(
      Date = as.Date(character()),
      sulfate = numeric(0),
      nitrate = numeric(0),
      ID = integer(0),
      stringsAsFactors = FALSE
    )
  
  ## concatenates all data frames
  i <- 1
  while (i <= length(id)) {
    fn <- paste(directory, k[[i]], sep = "/")
    df <- read.csv(fn, header = TRUE, sep = ",")
    x <- rbind(u, df)
    u <- x
    i <- i + 1
  }
  
  return(na.omit(u))
}

# 2) What are the dimensions (number of rows and columns) of the dataset.
mydf <- loadcomplete("./specdata")
dim(mydf)
# [1] 111802      4
 
# 3) Specify the column name and the data type for each column of the dataset.
sapply(mydf, typeof)
# Date   sulfate   nitrate        ID 
# "integer"  "double"  "double" "integer" 
 
# 4) What is the minimum and maximum of all the numeric columns of the dataset. 
apply(mydf, 2, min)
# Date       sulfate       nitrate            ID 
# "2000-02-09" " 0.00000000"  " 0.0000000"         "  1" 
apply(mydf, 2, max)
# Date       sulfate       nitrate            ID 
# "2010-05-14" "35.90000000"  "53.9000000"         "332"

# 5) What is the date range of the data included. 
# (earliest and latest day that can be found) in the dataset 
min(as.character(mydf$Date))
# [1] "2000-02-09"
max(as.character(mydf$Date))
# [1] "2010-05-14"

# 6) Get the daily mean for the sulfate polutant levels in the dataset. 
lapply(split(mydf$sulfate, mydf$Date), mean)

# 7) Get the mean nitrate levels for each monitor in the dataset.
lapply(split(mydf$nitrate, mydf$ID), mean)

# Using iris dataset 
# 8) Load the iris dataset 
library(datasets)
datasets::iris

# 9) What are the dimensions of the iris dataset? 
dim(iris)
# [1] 150   5

# 10) What are the column name and the data type foreach column
sapply(iris, typeof)
# Sepal.Length  Sepal.Width Petal.Length  Petal.Width      Species 
# "double"     "double"     "double"     "double"    "integer"
# Species is actually a Factor with three levels, that's why its
# type is integer

# 11) What is the minimum and maximum of all the numeric columns 
apply(iris[,1:4], 2, min)
# Sepal.Length  Sepal.Width Petal.Length  Petal.Width 
# 4.3          2.0          1.0          0.1
apply(iris[,1:4], 2, max)
# Sepal.Length  Sepal.Width Petal.Length  Petal.Width
# 7.9          4.4          6.9          2.5 

# 12) What are the different categories of species that exist 
unique(as.character(iris$Species))
# [1] "setosa"     "versicolor" "virginica"
 
# 13) What is the mean sepal length for the species versicolor 
mean(iris[which(iris$Species == "versicolor"),1])
# [1] 5.936

# 14) Obtain a vector with the means of the 
# sepal lenght, sepal width, petal length and petal width 
# across all species
setosa <- iris[which(iris$Species == "setosa"),]
versicolor <- iris[which(iris$Species == "versicolor"),]
virginica <- iris[which(iris$Species == "virginica"),]
sapply(setosa[,1:4], mean)
# Sepal.Length  Sepal.Width Petal.Length  Petal.Width 
# 5.006        3.428        1.462        0.246 
sapply(versicolor[,1:4], mean)
# Sepal.Length  Sepal.Width Petal.Length  Petal.Width 
# 5.936        2.770        4.260        1.326 
sapply(virginica[,1:4], mean)
# Sepal.Length  Sepal.Width Petal.Length  Petal.Width
# 6.588        2.974        5.552        2.026

# 15) Obtain the mean petal length for each of the species.
sapply(split(iris$Petal.Length, iris$Species), mean)
# setosa versicolor  virginica 
# 1.462      4.260      5.552 
