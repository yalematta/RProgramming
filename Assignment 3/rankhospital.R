rankhospital <- function(state, outcome, num = "best"){

	## Read outcome data
	outcomedata <- read.csv("outcome-of-care-measures.csv", colClasses="character")

	## Check that state is valid
	if(!(state %in% outcomedata$State))
		stop('invalid state')

	## Check that outcome is valid
	if(!(outcome %in% c("heart attack", "heart failure", "pneumonia")))
		stop('invalid outcome')

	## Return hospital name in that state with lowest 30-day death rate
		data <- subset(outcomedata, State == state)

	if(identical(outcome, "heart attack"))
		mortalityRate <- data[,11]

	else if(identical(outcome, "heart failure"))
		mortalityRate <- data[,17]

	else mortalityRate <- data[,23]
		
	mortalityRate <- as.numeric(mortalityRate)
	hospitalName <- data[,2]
		
	m1 <- matrix(hospitalName, length(hospitalName),1)
	m2 <- matrix(mortalityRate, length(mortalityRate),1)
	m <- data.frame(m1,m2)

	m <- m[order(m$m2, m$m1),]
	m <- na.omit(m)
		
	rank <- matrix(1:nrow(m),nrow(m),1)
	m <- cbind(m,rank)
	colnames(m) <- c("Hospital.Name", "Rate", "Rank") 
	m[,1] <- as.character(m[,1])

	if(identical(num, "best")){
		index <- which(m$Rank == 1)
		bestHospital <- m[index,1]
	}
		
	if(identical(num, "worst")){
		index <- which(m$Rank == nrow(m))
		sub <- subset(m, m$Rate == m[index,2])
		bestHospital <- sub[1,1]
	}

	else{
		if(num > nrow(m))
			bestHospital <- NA
		else{
			index <- which(m$Rank == num)
			sub <- subset(m, m$Rate == m[index,2])
			bestHospital <- sub [1,1]
		}
	}		
	bestHospital
}