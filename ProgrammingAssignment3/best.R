best <- function(state, outcome) {
  ## Read outcome data
  data <- read.csv("outcome-of-care-measures.csv",colClasses="character")
  ## Check that state and outcome are valid
  if(outcome=="heart attack"){
      data1 <- as.numeric(data[,11])
      data2 <- cbind(data[,2],data[,7],data1)
      temp <-as.data.frame(data2)
      indicator1 <- complete.cases(temp)
      data3 <- temp[indicator1,]
      colnames(data3) <- c("hospital","id","rate")
      temp1 <- split(data3,data3$id)
      if(is.null(temp1[[state]])){
          stop('invalid state')
      }
      else{
          result <- temp1[[state]]
      }
  }
  else if(outcome=="heart failure"){
      data1 <- as.numeric(data[,17])
      data2 <- cbind(data[,2],data[,7],data1)
      temp <-as.data.frame(data2)
      indicator1 <- complete.cases(temp)
      data3 <- temp[indicator1,]
      colnames(data3) <- c("hospital","id","rate")
      temp1 <- split(data3,data3$id)
      if(is.null(temp1[[state]])){
        stop('invalid state')
      }
      else{
        result <- temp1[[state]]
      }
  }
  else if(outcome=="pneumonia"){
      data1 <- as.numeric(data[,23])
      data2 <- cbind(data[,2],data[,7],data1)
      temp <-as.data.frame(data2)
      indicator1 <- complete.cases(temp)
      data3 <- temp[indicator1,]
      colnames(data3) <- c("hospital","id","rate")
      temp1 <- split(data3,data3$id)
      if(is.null(temp1[[state]])){
        stop('invalid state')
      }
      else{
        result <- temp1[[state]]
      }
  }
  else{
      stop('invalid outcome')
  }
  ## Return hospital name in that state with lowest 30-day death
  ## rate
  temp2 <- result$rate
  temp3 <- as.numeric(levels(temp2))[temp2]
  int <- which.min(temp3)
  besthospital <- result$hospital[int]
  as.character(besthospital)
}