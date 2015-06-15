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
