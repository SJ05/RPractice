## Write a function that reads a directory full of files and reports the number of completely observed cases in each data file. The function should return a data frame where the first column is the name of the file and the second column is the number of complete cases. A prototype of this function follows
complete <- function(directory, id = 1:332) {
	## 'directory' is a character vector of length indicating
        ## the location of the CSV files
	listOfCSV <- as.character(list.files(directory))

	# Checks if the file in the directory exist
	specDataExist <- dir.exists(directory)

	# If spec data folder exist change the directory structure
	if(specDataExist == TRUE) {
		directory <- './specdata/'
	}

	# Create a path for each of the files in the folder
	filePaths <- paste(directory, listOfCSV, sep="")

	# Store the data
	data <- data.frame()
		
        ## 'id' is an integer vector indicating the monitor ID numbers
        ## to be used
	for(filesTobeRead in id) {
		# Reads each file that the user selected
		readCSVFiles <- read.csv(filePaths[filesTobeRead], header=TRUE)

		# Removes NA's or any missing value by using complete.cases()
		# See https://stackoverflow.com/questions/4862178/remove-rows-with-all-or-some-nas-missing-values-in-data-frame
		# Count the rows which do not contain 'NA' by using sum()
		countCompleteValues <- sum(complete.cases(readCSVFiles))

		# Create a data frame where the file ids and valid values is shown
		completeData <- data.frame(id = filesTobeRead, nobs = countCompleteValues)

		# Use rbind. It will show the values in rows
		data <- rbind(data, completeData)
	}

	## Return a data frame of the form:
	## id nobs
	## 1  117
	## 2  1041
	## ...
	## where 'id' is the monitor ID number and 'nobs is the number of complete cases
	return (data)
}
