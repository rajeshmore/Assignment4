rankhospital <- function(stateChr, outcomeChr, rankObj) {
  
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
  
  # Eliminate NA
  mydata <-  na.omit(hospital_data[which(hospital_data$state==stateChr),])
  
  # Equate number value for best/worst
  if (rankObj == "worst") {rankObj = nrow(mydata)}
  if (rankObj == "best") {rankObj = 1}
  
  #sort by outcome type 
  if (outcomeChr == "heart attack") {mydata_sorted <- mydata[order(mydata$"heart attack",mydata$hospital, decreasing=FALSE), ]}
  if (outcomeChr == "heart failure")  {mydata_sorted <- mydata[order(mydata$"heart failure",mydata$hospital, decreasing=FALSE), ]}
  if (outcomeChr == "pneumonia")  {mydata_sorted <- mydata[order(mydata$"pneumonia",mydata$hospital, decreasing=FALSE), ]}
  
  # Return Hospitical namw by outcome and rank
  mydata_sorted[rankObj,1]
  
}