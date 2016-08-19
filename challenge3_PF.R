
# 1) create a vector with 10000 random normal values with a mean of 15 and a standard deviation of 35 (seed at 1439).
set.seed(1439)
v = rnorm(10000, 15, 35)

# 2) create a vector with 10000 random normal values with a mean of 1 and a standard deviation of 0.
v2 = rnorm(10000, 0, 1)

# 3) Get the density of the probability distribution function for both vectors.
d = density(v)
d2 = density(v2)

# 4) Plot the result from question 3, with it's corresponding vector.
hist(v)
hist(v2)


# 5) Download the the gas excel file used for quizz 1 of
# Getting and Cleaning Data (https://d396qusza40orc.cloudfront.net/getdata%2Fdata%2FDATA.gov_NGAP.xlsx)
# and load the contractors table.
fn = "https://d396qusza40orc.cloudfront.net/getdata/data/DATA.gov_NGAP.xlsx"
download.file(fn, destfile = "./data/NGAP.xlsx", mode = "wb")

rowIdx <- c(18:23)
colIdx <- c(1:15)
mydata <-
  read.xlsx(
    "./data/NGAP.xlsx",
    rowIndex = rowIdx,
    colIndex = colIdx,
    sheetIndex = 1
  )
str(mydata)

# 6) Give the dimmensions and column names for each column.
dim(mydata)
names(mydata)

# 7) How many contractors are in Tulsa?
length(which(mydata$City == "Tulsa"))

# 8) Download the last csv file used in the quizz 1 of
# getting and cleaning data
# ( URL 'https://d396qusza40orc.cloudfront.net/getdata%2Fdata%2Fss06pid.csv')
# and load it into a data table.
library(data.table)
download.file(
  "https://d396qusza40orc.cloudfront.net/getdata%2Fdata%2Fss06pid.csv",
  destfile = "./data/ss06pid.csv",
  method = "auto"
)
remove(mydata)
mydata <- read.csv("./data/ss06pid.csv", header = TRUE, sep = ",")

myDT <- data.table(mydata)
str(myDT)
# 9) What is the average age in the data?.}
myDT[, mean(myDT$AGEP, na.rm = TRUE)]

# 10) What is the average age by gender.
myDT[, mean(myDT$AGEP, na.rm = TRUE), by = myDT$SEX]

# BONUS QUESTIONS!!!!!
#
# 11) Load the xml file from the getting and cleaning data quizz 1
# (https://d396qusza40orc.cloudfront.net/getdata%2Fdata%2Frestaurants.xml),
# and get the number of restaurants by police district (policedistrict).
# (3 extra point)
library(XML)
# got an error while reading https, took the 's' off
fileUrl <-
  "http://d396qusza40orc.cloudfront.net/getdata%2Fdata%2Frestaurants.xml"
doc <- xmlTreeParse(fileUrl, useInternalNodes = TRUE)
rootNode <- xmlRoot(doc)
xmlSApply(rootNode, xmlValue)
policedistrictList <-
  xpathSApply(rootNode, "//policedistrict", xmlValue)
table(policedistrictList)
# output:
# policedistrictList
# CENTRAL      EASTERN NORTHEASTERN     NORTHERN NORTHWESTERN SOUTHEASTERN
# 288           67           72          157           52          385
# SOUTHERN SOUTHWESTERN      WESTERN
# 213           55           38

# 12) The programming assignment for week 3 uses functions to obtain the best
# hospital by state, to rank all hospital by best or worst, and to
# rank hospital by state and outcome.
# Alter this functions to  filter by a minimal number of cases
# (given as a new input  for the formulas) for their analysis,
# and set the default to be 25.
# (5 extra points for each formula correctly modified)

best2 <- function(state, outcome, minCases = 25) {
  ## Read outcome data
  hospitals <-
    read.csv("outcome-of-care-measures.csv", colClasses = "character")
  
  ## Check that state and outcome are valid
  myHS <-
    hospitals[which(hospitals$State == state), c(2, 7, 11, 17, 23)]
  nRegHS <- nrow(myHS)
  if (nRegHS == 0) {
    stop("Invalid state")
  }
  ##########################################
  ## CHALLENGE 3
  # invalidates state x outcome with less than minimum observations threshold
  if (nRegHS < minCases) {
    stop(
      paste(
        "dataset with too few observations (min =",
        minCases,
        ", observations ="
      ),
      nRegHS,
      ")"
    )
  }
  # END CHALLENGE 3
  #########################################
  
  if (outcome == "heart attack") {
    colIndex <- 3
  }
  else if (outcome == "heart failure") {
    colIndex <- 4
  }
  else if (outcome == "pneumonia") {
    colIndex <- 5
  }
  else
    stop("Invalid outcome")
  
  ## Return hospital name in that state with lowest 30-day death rate
  myHS[, colIndex] <- suppressWarnings(as.numeric(myHS[, colIndex]))
  # retrieve sorted alphabetically the names of hospitals which have the min value
  hospitalName <-
    sort(myHS[which(myHS[, colIndex] == min(myHS[, colIndex], na.rm = TRUE)), 1])
  # returns the first one in the list
  hospitalName[1]
}
# output:
# >  best2("AK", "heart attack", 18)
# Error in best2("AK", "heart attack", 18) : 
#   dataset with too few observations (min = 18 , observations =17)
# > best2("AK", "heart attack", 17)
# [1] "PROVIDENCE ALASKA MEDICAL CENTER"
# > best2("AK", "heart attack")
# Error in best2("AK", "heart attack") : 
#   dataset with too few observations (min = 25 , observations =17)

rankhospital2 <-
  function(state,
           outcome,
           num = "best",
           minCases = 25) {
    ## Read outcome data
    hospitals <-
      read.csv("outcome-of-care-measures.csv", colClasses = "character")
    
    ## Check that state and outcome are valid
    myHS <-
      hospitals[which(hospitals$State == state), c(2, 7, 11, 17, 23)]
    
    if (outcome == "heart attack") {
      colIndex <- 3
    }
    else if (outcome == "heart failure") {
      colIndex <- 4
    }
    else if (outcome == "pneumonia") {
      colIndex <- 5
    }
    else
      stop("Invalid outcome")
    
    myHS[, colIndex] <-
      suppressWarnings(as.numeric(myHS[, colIndex]))
    valid <- length(which(!is.na(myHS[, colIndex])))
    if (valid == 0) {
      stop("Invalid state")
    }
    ##########################################
    ## CHALLENGE 3
    if (valid < minCases) {
      stop(
        paste(
          "dataset with too few observations (min =",
          minCases,
          ", observations ="
        ),
        valid,
        ")"
      )
    }
    # END CHALLENGE 3
    #########################################
    if (!is.numeric(num)) {
      if (num == "best") {
        idx <- 1
      }
      else if (num == "worst") {
        idx <- -1
      }
      else {
        stop("invalid num")
      }
    }
    else {
      if (num > valid) {
        return(NA)
      }
      idx <- num
    }
    
    ## Return hospital name in that state with the given rank
    ## 30-day death rate
    
    
    # applies reverse order in case of "worse"
    if (idx == -1) {
      ii <-
        order(myHS[, colIndex], myHS[, 1], decreasing = TRUE, na.last = TRUE)
      idx <- 1
    }
    else {
      ii <- order(myHS[, colIndex], myHS[, 1], na.last = TRUE)
    }
    
    j <- c(1:length(ii))
    myHS$rnk <- j
    
    j <- 1
    for (i in ii) {
      myHS$rnk[i] <- j
      j <- j + 1
    }
    myRnkHS <- myHS[order(myHS$rnk),]
    myRnkHS[idx, 1]
  }
# output:
# > rankhospital2("AK", "heart attack", minCases = 6)
# Error in rankhospital2("AK", "heart attack", minCases = 6) : 
#   dataset with too few observations (min = 6 , observations =5)
# > rankhospital2("AK", "heart attack", minCases = 3)
# [1] "PROVIDENCE ALASKA MEDICAL CENTER"
# > rankhospital2("AK", "heart attack")
# Error in rankhospital2("AK", "heart attack") : 
#   dataset with too few observations (min = 25 , observations =5)

rankall2 <- function(outcome,
                     num = "best",
                     minCases = 25) {
  ## Read outcome data
  hospitals <-
    read.csv("outcome-of-care-measures.csv", colClasses = "character")
  myHS <- hospitals[, c(2, 7, 11, 17, 23)]
  
  ## Check that outcome and num are valid
  if (outcome == "heart attack") {
    colIndex <- 3
  }
  else if (outcome == "heart failure") {
    colIndex <- 4
  }
  else if (outcome == "pneumonia") {
    colIndex <- 5
  }
  else
    stop("Invalid outcome")
  
  if (!is.numeric(num)) {
    if (num == "best") {
      idx <- 1
    }
    else if (num == "worst") {
      idx <- -1
    }
    else {
      stop("invalid num")
    }
  }
  else {
    idx <- num
  }
  
  ## For each state, find the hospital of the given rank
  myHS[, colIndex] <- suppressWarnings(as.numeric(myHS[, colIndex]))
  s <- split(myHS, myHS$State)
  
  # applies reverse order in case of "worse"
  if (idx == -1) {
    t <-
      lapply(s, function(x)
        order(x[, colIndex], x[, 1], decreasing = TRUE, na.last = TRUE))
    idx <- 1
  }
  else {
    t <-
      lapply(s, function(x)
        order(x[, colIndex], x[, 1], na.last = TRUE))
  }
  
  #r <- lapply(t, function(x) c(1:length(x[])))
  ## Return a data frame with the hospital names and the
  ## (abbreviated) state name
  
  res <-
    data.frame(
      hospital = character(),
      state = character(),
      stringsAsFactors = FALSE
    )
  
  for (i in c(1:length(s))) {
    ##########################################
    ## CHALLENGE 3
    # filter out results from states with less than minimal number of obs.
    numObs <- length(s[[i]][, 1])
    if (numObs > minCases) {
      if (is.na(s[[i]][t[[i]][idx], 2])) {
        st <- s[[i]][t[[i]][1], 2]
      }
      else {
        st <- s[[i]][t[[i]][idx], 2]
      }
      auxres <- data.frame(
        hospital = s[[i]][t[[i]][idx], 1],
        state = st,
        stringsAsFactors = FALSE
      )
      u <- rbind(res, auxres)
      res <- u
    }
    # END CHALLENGE 3
    ##########################################
  }
  res
}

# output:
# > c<-rankall2("heart attack")
# > length(c[,1])
# [1] 46
# > c<-rankall2("heart attack", minCases = 25)
# > length(c[,1])
# [1] 46
# > c<-rankall2("heart attack", minCases = 50)
# > length(c[,1])
# [1] 35
# > c<-rankall2("heart attack", minCases = 250)
# > length(c[,1])
# [1] 2
# > c<-rankall2("heart attack", minCases = 1)
# > length(c[,1])
# [1] 53
# > c<-rankall2("heart attack", minCases = 0)
# > length(c[,1])
# [1] 54
# > c<-rankall2("heart attack", minCases = 400)
# > length(c[,1])
# [1] 0
