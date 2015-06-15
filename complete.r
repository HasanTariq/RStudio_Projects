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
