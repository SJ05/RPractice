## Write a function that takes a directory of data files and a threshold for complete cases and calculates the correlation between sulfate and nitrate for monitor locations where the number of completely observed cases (on all variables) is greater than the threshold. The function should return a vector of correlations for the monitors that meet the threshold requirement. If no monitors meet the threshold requirement, then the function should return a numeric vector of length 0. A prototype of this function follows

corr <- function(directory, threshold = 0) {
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
	corrValue = NULL
	
	# Loop the files
	for(filesToRead in 1:332) {
		# Reads each file
		readCSVFiles <- read.csv(filePaths[filesToRead], header=TRUE)
		
		# Removes NA's or any missing value by using complete.cases()
		# Create it by row
		completeValues = readCSVFiles[complete.cases(readCSVFiles), ]
		
		## 'threshold' is a numeric vector of length 1 indicating the
		## number of completely observed (on all variables)
		## required to compute the correlation between
		## nitrate and sulfate; the default is 0
		
		if(nrow(completeValues) > threshold) {
			corrValue <- c(corrValue, cor(completeValues[,"sulfate"], completeValues[, "nitrate"]))
		}
	}
	
	## Return a numeric vector of correlations
	## NOTE: Do not round the result!
	return (corrValue)
}
