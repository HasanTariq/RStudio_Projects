HelloWord <- function(x){
  if (x == 1){
    print ("Hellow World")
    x * 100
  } else {
    print ("Get lost")
    x * 100000
  }
  
}

above10 <- function(x){
  aboveTen <- x>10

  print (x[aboveTen])
}

# columnMean function intend to go through data set or matrix
# and calculate the mean of each column in this data frame with
# removing the NA values.
# The function will take 2 arguments:
# x : the data frame you are inputing
# removeNA: logical value to remove NA values

columnMean <- function(x, removeNA= TRUE){
  
  # First we need to find out how many columns do we have in this data frame
  nc <- ncol(x)
  
  # Then we need to assign it to variable and initialize it
  means <- numeric(nc)
  
  # For loop will take care looping through each columns and compute the mean
  for (i in 1:nc){
    
    means[i] <- mean(x[ , i],na.rm = removeNA)
  }
  
  #print it  
  print (means)
  
  
}


# Write a function named 'pollutantmean' that calculates the 
# mean of a pollutant (sulfate or nitrate) across a specified 
# list of monitors. The function 'pollutantmean' takes three 
# arguments: 'directory', 'pollutant', and 'id'. Given a vector 
# monitor ID numbers, 'pollutantmean' reads that monitors' 
# particulate matter data from the directory specified in 
# the 'directory' argument and returns the mean of the pollutant 
# across all of the monitors, ignoring any missing values coded as 
# NA. A prototype of the function is as follows

pollutantmean <- function(directory, pollutant, id = 1:332){
  # create a new object which will contain  the full path of 
  # all the CSV files in the given directory.
  
  Full_Files <- list.files(directory, pattern = ".csv", full.names = TRUE)
  
  # Create an empty data frame which will contain all the data
  # from the given monitor ID
  
  dat <- data.frame()
  
  # Loop through the given ID numbers to append all the csv in one data frame
  for (file in id) {
    dat <- rbind(dat, read.csv(Full_Files[file]))
    
  }
  
  # We will use mean function to get the mean of requested 
  # pollutant and remove NA values before calaculating the mean
  
  mean(dat[ , pollutant],na.rm = TRUE)
  
  
}


# Write a function that reads a directory full 
# of files and reports the number of completely observed 
# cases in each data file. The function should return a data 
# frame where the first column is the name of the file and the 
# second column is the number of complete cases. A prototype 
# of this function follows

complete <- function(directory, id = 1:332) {
  
  # create a new object which will contain  the full path of 
  # all the CSV files in the given directory.
  
  Full_Files <- list.files(directory, pattern = ".csv", full.names = TRUE)
  
  # Create an empty data frame which will contain each
  # given monitor ID and the number of complete cases
  
  dat <- data.frame(id = NULL, nobs = NULL )
  
  # Create for loop where we will go through each given monitor ID
  # and find the complete cases and append it to empty created data frame
  
  for (file in id){
    
    # find the completed cases for file and assign it varaible completed
    completed <-  complete.cases(read.csv(Full_Files[file]))
    
    #append the id number and the number of completed cases to the data frame
    dat <- rbind( dat, data.frame(id= file, nobs= sum(completed)))   
    
      }
  #print data frame
  dat
}



# Write a function that takes a directory of data 
# files and a threshold for complete cases and 
# calculates the correlation between sulfate and 
# nitrate for monitor locations where the number 
# of completely observed cases (on all variables) 
# is greater than the threshold. The function 
# should return a vector of correlations for the 
# monitors that meet the threshold requirement. 
# If no monitors meet the threshold requirement, then 
# the function should return a numeric vector of length 0. 
# A prototype of this function follows

corr <- function(directory, threshold = 0) {
  
  # create a new object which will contain  the full path of 
  # all the CSV files in the given directory.
  
  Full_Files <- list.files(directory, pattern = ".csv", full.names = TRUE)
  
  #initialize your data frame and result 
  dat <- data.frame()
  corr_vecttor <- numeric()
  
  # loop through each CSV file 
  for (i in 1:332) {
    
    # read CSV file and assign it to DAT variable 
    dat <- read.csv(Full_Files[i])
                    
    #find the completed cases for file and assign it varaible completed
    completed<- complete.cases(dat)
    
    # re-assign it again to reflect the new data without the missing values
    dat <- dat[completed,]
      
    # check number of completed cases for each file. 
    # if it is greater than threshold, then calculate correlation
    if  (nrow(dat) > threshold) {
      
      # Calculate the correlation and append it in vector
      corr_vecttor <- c(corr_vecttor, cor(dat$nitrate, dat$sulfate))
      
    }
  }
  #print you final result
  corr_vecttor
}