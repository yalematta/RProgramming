rankall <- function(outcome, num = "best"){

	## Read outcome data
	data <- read.csv("outcome-of-care-measures.csv", colClasses="character")

	## Check that outcome is valid
	if(!(outcome %in% c("heart attack", "heart failure", "pneumonia")))
		stop('invalid outcome')

	## For each state, find the hospital of the given rank
	if(identical(outcome, "heart attack"))
		the_column <- 11

	else if(identical(outcome, "heart failure"))
		the_column <- 17

	else the_column <- 23

	## Initialize the 2 lists that'll hold the hospitals and states 
      hospitals_list <- character()
      states_list <- character()

	## Make an alphabetically sorted vector of all states in the CSV file
      states <- sort(unique(as.vector(data[,7])))

      ## Loop through each state in that vector
      for (state in states){
      	state_data <- subset(data, data[,7] == state & data[,the_column] != "Not Available")[,c(2,the_column)]
            rankings <- as.numeric(state_data[,2])
            slim_frame <- data.frame(state_data[,1],rankings)
	
 		## Sort that list by rank first and second by hospital name
            sorted_frame <- slim_frame[order(slim_frame$rankings, slim_frame[,1]),]

		if (num == "best") 
			tnum <- 1 
            else if (num == "worst") 
			tnum <- nrow(sorted_frame) 
            else 
			tnum <- num 
		
		## Get the hospital name of the row with the specified rank
            hospital_name <- as.character(sorted_frame[tnum,1])

		## Add that hospital and state to our data frame
            hospitals_list <- c(hospitals_list, hospital_name)
            states_list <- c(states_list, state)
	}
	## Return a data frame with the hospital names and the abbreviated state name
	final_frame <- data.frame(hospitals_list, states_list)
      names(final_frame) <- c("hospital","state")
      final_frame
}