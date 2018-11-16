## Write a function called rankall that takes two arguments: an outcome name
## (outcome) and a hospital ranking (num). The function reads the
## outcome-of-care-measures.csv file and returns a 2-column data frame
## containing the hospital in each state that has the ranking specified in num.
## For example the function call rankall("heart attack", "best") would return
## a data frame containing the names of the hospitals that are the best in
## their respective states for 30-day heart attack death rates. The function
## should return a value for every state (some may be NA). The first column in
## the data frame is named hospital, which contains the hospital name, and the
## second column is named state, which contains the 2-character abbreviation for
## the state name. Hospitals that do not have data on a particular outcome
## should be excluded from the set of hospitals when deciding the rankings.

## Handling ties. The rankall function should handle ties in the 30-day
## mortality rates in the same way that the rankhospital function handles ties

## NOTE: For the purpose of this part of the assignment (and for efficiency),
## your function should NOT call the rankhospital function from the previous
## section. The function should check the validity of its arguments. If an
## invalid outcome value is passed to rankall, the function should throw an
## error via the stop function with the exact message “invalid outcome”. The
## num variable can take values “best”, “worst”, or an integer indicating the
## ranking (smaller numbers are better). If the number given by num is larger
## than the number of hospitals in that state, then the function should
## return NA.

rankall <- function(outcome, num = "best") {
	## Read outcome data
	readCSV <- read.csv("outcome-of-care-measures.csv", colClasses = "character")
	collectedData <- as.data.frame(cbind(readCSV[, 2], readCSV[, 7], readCSV[, 11],
							readCSV[, 17], readCSV[, 23]),
					stringsAsFactors = FALSE)
	
	## Put header names
	colnames(collectedData) <-
			c("hospital", "state", "heart attack", "heart failure",
					"pneumonia")
	
	## Combine the outcomes in a list
	outcomeChoice <- c("heart attack", "heart failure", "pneumonia")
	
	## Check that state is valid
	if (!outcome %in% outcomeChoice) {
		stop("invalid outcome")
	}
	
	## Convert values to numeric values
	collectedData[, eval(outcome)] <- as.numeric(collectedData[, eval(outcome)])
	
	## Create a copy of the data and split them by state
	extractedData <- with(collectedData, split(collectedData, state))
	
	## Create a list container
	listOfState <- list()
	
	## Check if the num is a number
	if(is.numeric(num)) {
		## Loop the data
		for (i in seq_along(extractedData)){
			## Get the data of the hospital ranks in ascending order
			extractedData[[i]] <- extractedData[[i]][order(extractedData[[i]][, eval(outcome)], 
							extractedData[[i]][, "hospital"]), ]
			## Put the sorted data to the list
			listOfState[[i]]  <- c(extractedData[[i]][num, "hospital"], extractedData[[i]][, "state"][1])
		}
		
		## Arrange the data into rows
		result <- do.call(rbind, listOfState)
		
		## Put the data into a data frame
		rankAllData <- as.data.frame(result, row.names = result[, 2])
		
		## Put headers to the final data
		names(rankAllData) <- c("hospital", "state")
	} else {
		if (num == "best") {
			## Loop the data
			for (i in seq_along(extractedData)){
				## Get the data of the hospital ranks in ascending order
				extractedData[[i]] <- extractedData[[i]][order(extractedData[[i]][, eval(outcome)], 
								extractedData[[i]][, "hospital"]), ]
				## Put the sorted data to the list and get the first value
				listOfState[[i]]  <- c(extractedData[[i]][1, c("hospital", "state")])
			}
			## Arrange the data into rows
			result <- do.call(rbind, listOfState)
			
			## Put the data into a data frame
			rankAllData <- as.data.frame(result)
			
			## Acquire the row names for the final data
			rownames(rankAllData) <- rankAllData[, 2]
		} else if (num == "worst") {
			## Loop the data
			for (i in seq_along(extractedData)){
				## Get the data of the hospital ranks in descending order
				extractedData[[i]] <- extractedData[[i]][order(extractedData[[i]][, eval(outcome)], 
								extractedData[[i]][, "hospital"], 
								decreasing = TRUE), ]
				## Put the sorted data to the list and get the first value
				listOfState[[i]]  <- c(extractedData[[i]][1, c("hospital", "state")])
			}
			## Put the sorted data to the list and get the first value
			result <- do.call(rbind, listOfState)
			## Put the sorted data to the list and get the first value
			rankAllData <- as.data.frame(result)
			## Put the sorted data to the list and get the first value
			rownames(rankAllData) <- rankAllData[, 2]
		} else if(num > sum(nrow(extractedData))) {
			stop("NA")
		} 	
	}
	## For each state, find the hospital of the given rank
	## Return a data frame with the hospital names and the
	## (abbreviated) state name
	return(rankAllData)
}