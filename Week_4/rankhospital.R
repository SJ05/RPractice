## Write a function called rankhospital that takes three arguments: the
## 2-character abbreviated name of a state (state), an outcome (outcome),
## and the ranking of a hospital in that state for that outcome (num).

rankhospital <- function(state, outcome, num = "best") {
	## Read outcome data
	readCSV <-
			read.csv("outcome-of-care-measures.csv", colClasses = "character")
	collectedData <-
			as.data.frame(cbind(readCSV[, 2], readCSV[, 7], readCSV[, 11],
							readCSV[, 17], readCSV[, 23]),
					stringsAsFactors = FALSE)
	
	## Put header names
	colnames(collectedData) <-
			c("Hospital Name",
					"State",
					"heart attack",
					"heart failure",
					"pneumonia")
	
	## Combine the outcomes in a list
	outcomeChoice <-
			c("heart attack", "heart failure", "pneumonia")
	
	## Check that state is valid
	if (!state %in% collectedData[, "State"]) {
		stop("invalid state")
	} 
	## Check that outcome is valid
	else if (!outcome %in% outcomeChoice) {
		stop("invalid outcome")
	}
	## Check if the row number is greater than the length of the selected data
	else if(num > sum(nrow(readCSV))) {
		stop("NA")
	} 
	else if(is.numeric(rank) ) {
		## Return hospital name in that state with the given rank
		## 30-day death rate
		listOfSelectedData <-
				which(collectedData[, "State"] == state)
		
		## Extract the data
		extractedData = collectedData[listOfSelectedData,]
		
		## Get the Values per index and since it is characters, change to numeric
		numericValues <-
				as.numeric(extractedData[, eval(outcome)])
		
		## Put them in variable
		result  <-
				extractedData[, "Hospital Name"][which(numericValues == minimumValue)]
		
		## Arrange into alphabetical order
		finalData <- result[order(result)]
	}
	
	return(finalData)

}

