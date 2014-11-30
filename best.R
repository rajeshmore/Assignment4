best <- function(stateChr, outcomeChr) {
# Function to find the best hospital in a state
# Read file data
file_data <- read.csv("outcome-of-care-measures.csv",colClasses = "character")
# Convert data type from char to numeric and suppress warning
suppressWarnings(file_data[, 11] <- as.numeric(file_data[, 11]))
suppressWarnings(file_data[, 17] <- as.numeric(file_data[, 17]))
suppressWarnings(file_data[, 23] <- as.numeric(file_data[, 23]))
#Merge data set
hospital_data <- file_data[,c(2,7,11,17,23)]
#provide proper column names
colnames(hospital_data) <- c("hospital","state","heart attack","heart failure","pneumonia")

# Check for valid input argument
if ( stateChr %in% hospital_data$state == FALSE) 
  stop("invalid state")
if (outcomeChr %in% c("heart attack","heart failure","pneumonia") == FALSE) 
  stop("invalid outcome")

# Eliminate NA Values
mydata <- na.omit(hospital_data[which(hospital_data$state==stateChr),])
#Find row number for min value by outcome
if (outcomeChr == "heart attack") {rownum<- which(mydata$"heart attack" == min(mydata[,3]))}
if (outcomeChr == "heart failure")  {rownum<- which(mydata$"heart failure" == min(mydata[,4]))}
if (outcomeChr == "pneumonia")  {rownum<- which(mydata$"pneumonia" == min(mydata[,5]))}
#Return Hospital name for identified row
mydata[rownum,1]
}

# best("TX", "heart failure")
# best("MD", "heart attack")
# best("MD", "pneumonia")
# best("BB", "heart attack")
# best("NY", "hert attack")

