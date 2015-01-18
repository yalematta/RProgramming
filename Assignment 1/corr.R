corr <- function(directory, threshold = 0){
	## 'directory' is a character vector of length 1 indicating
      ## the location of the CSV files

      ## 'threshold' is a numeric vector of length 1 indicating the
      ## number of completely observed observations (on all
      ## variables) required to compute the correlation between
      ## nitrate and sulfate; the default is 0

      ## Return a numeric vector of correlations

	C <- numeric()
  	V <- numeric()
  	j <- 1
  	M <- complete(directory, 1:332)
  
  	for (i in 1:332){
    		if(M[i,2] > threshold){
      		V[j] <- M[i,1]
      		j <- j + 1
    		}
  	}
  	j <- 1
  
  	for (i in V){
    		number <- formatC(i, width = 3, flag = '0')
    		filename <- file.path(directory, paste(number, ".csv", sep=""))
    
    		data <- read.csv(filename) 
    
    		sul <- data$sulfate
    		nit <- data$nitrate
    		C[j] <- cor(sul, nit, use="pairwise.complete.obs") 
    		j <- j + 1
  	}
  	C
}