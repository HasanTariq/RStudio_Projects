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