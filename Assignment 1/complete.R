 complete <- function(directory, id = 1:332){
  	## 'directory' is a character vector of length 1 indicating
  	## the location of the CSV files
  
  	## 'id' is an integer vector indicating the monitor ID numbers
  	## to be used
  
  	## Return a data frame of the form:
  	## id nobs
  	## 1  117
  	## 2  1041
  	## ...
  	## where 'id' is the monitor ID number and 'nobs' is the
  	## number of complete cases
  
  	M <- matrix(, nrow = length(id), ncol = 2)
  	colnames(M) <- c("id", "nobs")
  	rownames(M) <- 1:length(id)
  	j <- 1
  
  	for(i in id){ 
    		number <- formatC(i, width = 3, flag = '0')
    		filename <- file.path(directory, paste(number, ".csv", sep =""))
    
    		data <- read.csv(filename)
    
   		nid <- i
    		ncc <- sum(complete.cases(data))
    
    		if(j < length(id) + 1){
      		M[j,] <- cbind(nid, ncc)
      		j <- j + 1
    		}
  	}
  	as.data.frame(M)
}