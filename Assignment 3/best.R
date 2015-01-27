best <- function(state, outcome){
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
		mortalityRate <- data[,13]

	else if(identical(outcome, "heart failure"))
		mortalityRate <- data[,19]

	else mortalityRate <- data[,25]
	
	mortalityRate <- as.numeric(mortalityRate)
	hospitalName <- data[,2]

	s <- split(hospitalName, mortalityRate)
	s <- na.omit(s)
	s	

	bestHospital <- s[[1]][1]
	bestHospital 
}