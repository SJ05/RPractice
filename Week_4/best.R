## Write a function called best that take two arguments: the 2-character
## abbreviated name of a state and an outcome name. The function reads the
## outcome-of-care-measures.csv file and returns a character vector with the
## name of the hospital that has the best (i.e. lowest) 30-day mortality for 
## the specified outcome in that state. The hospital name is the name provided
## in the Hospital.Name variable. The outcomes can be one of “heart attack”,
## “heart failure”, or “pneumonia”. Hospitals that do not have data on a
## particular outcome should be excluded from the set of hospitals when
## deciding the rankings.

## Handling ties. If there is a tie for the best hospital for a given outcome,
## then the hospital names should be sorted in alphabetical order and the first
## hospital in that set should be chosen (i.e. if hospitals “b”, “c”,
## and “f” are tied for best, then hospital “b” should be returned).


## The function should check the validity of its arguments. If an invalid state
## value is passed to best, the function should throw an error via the stop
## function with the exact message “invalid state”. If an invalid outcome
## value is passed to best, the function should throw an error via the stop
## function with the exact message “invalid outcome”.

best <- function(state, outcome) {
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
        
        ## Check that state and outcome are valid
        if (!state %in% collectedData[, "State"]) {
                stop("invalid state")
        } else if (!outcome %in% outcomeChoice) {
                stop("invalid outcome")
        } else {
                ## Return hospital name in that state with lowest 30-day death
                ## rate
                listOfSelectedData <-
                        which(collectedData[, "State"] == state)
                
                ## Extract the data
                extractedData <- collectedData[listOfSelectedData,]
                
                ## Get the Values per index and since it is characters, change to numeric
                numericValues <-
                        as.numeric(extractedData[, eval(outcome)])
                
                ## Removed the na values and get the minimum values
                minimumValue <- min(numericValues, na.rm = TRUE)
                
                ## Put them in variable
                result  <-
                        extractedData[, "Hospital Name"][which(numericValues == minimumValue)]
                
                ## Arrange into alphabetical order
                finalData <- result[order(result)]
        }
        
        return(finalData)
}
