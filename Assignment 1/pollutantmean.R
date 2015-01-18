pollutantmean <- function(directory, pollutant, id = 1:332){
	## 'directory' is a character vector of length 1 indicating
      ## the location of the CSV files

      ## 'pollutant' is a character vector of length 1 indicating
      ## the name of the pollutant for which we will calculate the
      ## mean; either "sulfate" or "nitrate".

      ## 'id' is an integer vector indicating the monitor ID numbers
      ## to be used

      ## Return the mean of the pollutant across all monitors list
      ## in the 'id' vector (ignoring NA values)
  
	N <- as.integer(0)
  	D <- as.integer(0)
  
  	for(i in id[1]:id[length(id)]){
    
    		number <- formatC(i, width = 3, flag = '0')
    		filename <- file.path(directory, paste(number, ".csv", sep=""))
    
    		data <- read.csv(filename) #reading from file number "i"
    
    		X <- data[[pollutant]]
    		X <- na.omit(X)
    
    		N <- N + sum(X)    
    		D <- D + length(X)    
  	} 
  	N/D 
}