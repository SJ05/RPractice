## Write a function named 'pollutantmean' that calculates the mean of a pollutant (sulfate or nitrate) across a specified list of monitors. The function 'pollutantmean' takes three arguments: 'directory', 'pollutant', and 'id'. Given a vector monitor ID numbers, 'pollutantmean' reads that monitors' particulate matter data from the directory specified in the 'directory' argument and returns the mean of the pollutant across all of the monitors, ignoring any missing values coded as NA. A prototype of the function is as follows
pollutantmean <- function(directory, pollutant, id = 1:332) {

	## 'directory' is a character vector of length 1 indicating
	## the location of the CSV files
	listOfCSV <- as.character(list.files(directory))
	
	# Checks the current working directory
	currWD <- getwd()
	
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
	
	## 'pollutant' is a character of length 1 indicating
	## the name of the pollutant for which we will calculate the
	## mean; either "sulfate" or "nitrate".

	## 'id' is an integer vector indicating the monitor ID numbers
	## to be used.
	for(filesTobeRead in id) {
		# Reads each file that the user selected
		readCSVFiles <- read.csv(filePaths[filesTobeRead], header=TRUE)
		
		# Collect the data that is read in each file by using rbind. It will show as rows
		data <- rbind(data, readCSVFiles)
	}
	
	## Return the mean of the pollutant across all monitors list
	## in the 'id' vector (ignoring NA values)
	## NOTE: Do not round the result!
	# na.rm removes all NA values
	return(mean(data[,pollutant], na.rm = TRUE))

}
