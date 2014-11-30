rankall <- function(outcome, num = "best") {
  
  # Rank all hospital by state
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
  
  if (outcome %in% c("heart attack","heart failure","pneumonia") == FALSE) 
    stop("invalid outcome")
  
  #Get distinct States
  statelist <- unique(hospital_data[,c("state")])
  #print (ust)
  # Soert States list
  ordered_statelist <- ust[order(nchar(statelist), statelist)]

  #Loop in for each state
  hospitalname  <- character(0)
  hospitalstate <- character(0)
  hname  <- character(0)
  for (st in ordered_statelist) 
    { 
    mydata <-  hospital_data[which(hospital_data$state==st),]
    
    #sort by outcome type  
    if (outcome  == "heart attack") {
      mydata_sorted <- mydata[order(mydata$"heart attack",mydata$hospital, decreasing=FALSE), ]
      mydata_df <- data.frame(hospital=mydata_sorted$hospital,state=mydata_sorted$state,"heart attack"=mydata_sorted$"heart attack")
    }
    if (outcome  == "heart failure")  {
      mydata_sorted <- mydata[order(mydata$"heart failure",mydata$hospital, decreasing=FALSE), ]
      mydata_df <- data.frame(hospital=mydata_sorted$hospital,state=mydata_sorted$state,"heart failure"=mydata_sorted$"heart failure")
    }
    if (outcome  == "pneumonia")  {
      mydata_sorted <-  mydata[order(mydata$"pneumonia",mydata$hospital, decreasing=FALSE), ]
      mydata_df <- data.frame(hospital=mydata_sorted$hospital,state=mydata_sorted$state,pneumonia=mydata_sorted$pneumonia)
    }
    
    mydata_clean<- na.omit(mydata_df)
    # Equate number value for best/worst
    if (is.character(num) == TRUE) {
        if (num == "worst") {mynum = nrow(mydata_clean)}
        if (num == "best") {mynum = 1}
    }else
    {
      mynum = num
    }
  
    if (mynum <= nrow(mydata_clean)) {
      #hname <- mydata_clean[mynum,1]
      hname <- as.character(mydata_clean[mynum, ]$hospital)
    }
      else
      {
        hname = "<NA>"
      }
    # Return Hospitical namw by outcome and rank
    hospitalname  <- c(hospitalname, hname)
    hospitalstate <- c(hospitalstate, st)
    
#    print(hospitalname)
    }
  return(data.frame(hospital = hospitalname, state = hospitalstate))

}



