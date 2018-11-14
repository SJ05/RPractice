# Write a function called best that take two arguments: the 2-character
# abbreviated name of a state and an outcome name.

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
                extractedData = collectedData[listOfSelectedData,]
                
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
