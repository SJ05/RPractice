## Write a function called rankhospital that takes three arguments:
## the 2-character abbreviated name of a state (state), an outcome (outcome),
## and the ranking of a hospital in that state for that outcome (num).
## T he function reads the outcome-of-care-measures.csv file and returns
## a character vector with the name of the hospital that has the ranking
## specified by the num argument. For example, the call
## rankhospital("MD", "heart failure", 5) would return a character vector
## containing the name of the hospital with the 5th lowest 30-day death rate
## for heart failure. The num argument can take values “best”, “worst”, or an
## integer indicating the ranking (smaller numbers are better). If the number
## given by num is larger than the number of hospitals in that state, then the
## function should return NA. Hospitals that do not have data on a particular 
## outcome should be excluded from the set of hospitals when deciding the 
## rankings.

## Handling ties. It may occur that multiple hospitals have the same 30-day
## mortality rate for a given cause of death. In those cases ties should be
## broken by using the hospital name. For example, in Texas (“TX”),
## the hospitals with lowest 30-day mortality rate for heart failure are shown
## here.

## The function should check the validity of its arguments. If an invalid state
## value is passed to rankhospital, the function should throw an error via the
## stop function with the exact message “invalid state”. If an invalid outcome
## value is passed to rankhospital, the function should throw an error via the
## stop function with the exact message “invalid outcome”.

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
			c("Hospital Name", "State", "heart attack", "heart failure",
					"pneumonia")
	
	## Combine the outcomes in a list
	outcomeChoice <- c("heart attack", "heart failure", "pneumonia")
	
	## Check that state is valid
	if (!state %in% collectedData[, "State"]) {
		stop("invalid state")
	} 
	## Check that outcome is valid
	else if (!outcome %in% outcomeChoice) {
		stop("invalid outcome")
	}
	
	## Get the values based from the State
	listOfSelectedData <- which(collectedData[, "State"] == state)
	
	## Extract the data to be evaluated
	extractedData <- collectedData[listOfSelectedData,]	
	
	## Check if the num is numeric
	if(is.numeric(num)) {
		## Order it from highest to lowest but make sure the outcome is numeric
		extractedData <- extractedData[order(as.numeric(extractedData[, eval(outcome)]), extractedData[, "Hospital Name"]), ]
		
		rankData <- extractedData[, "Hospital Name"][num]
	}
	## Check if the num is not numeric
	else {
		## Check if the num is "best"
		if(num == "best") {
			## Order it using increasing value but make sure the outcome is numeric
			extractedData <- extractedData[order(as.numeric(extractedData[, eval(outcome)]), extractedData[, "Hospital Name"]), ]
			
			## Get the 1st hospital
			rankData <- extractedData[, "Hospital Name"][1]
		}
		## Check if the num is "worst"
		else if (num == "worst") {
			## Order it using decreasing value but make sure the outcome is numeric
			extractedData <- extractedData[order(as.numeric(extractedData[, eval(outcome)]), extractedData[, "Hospital Name"], decreasing = TRUE), ]
			
			## Get the 1st hospital
			rankData <- extractedData[, "Hospital Name"][1]
		} 
		## Check if the row number is greater than the length of the selected data
		else if(num > sum(nrow(extractedData))) {
			stop("NA")
		} 	
	}
	## Return hospital name in that state with the given rank
	## 30-day death rate
	return(rankData)
}

