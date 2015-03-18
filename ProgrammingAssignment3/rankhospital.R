rankhospital <- function(state, outcome, num = "best") {
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
  ## Return hospital name in that state with the given rank
  ## 30-day death rate
  temp2 <- result$rate
  temp3 <- as.numeric(levels(temp2))[temp2]
  if(is.numeric(num)&&num>length(temp3)){
      rankname <- NA
  }
  else if(num=="best"){
      int <- which.min(temp3)
      besthospital <- result$hospital[int]
      rankname <- as.character(besthospital)
  }
  else if(num=="worst"){
      int <- which.max(temp3)
      worsthospital <- result$hospital[int]
      rankname <- as.character(worsthospital)
  }
  else if(is.numeric(num)){
      temp4 <- order(temp3,partial=result$hospital)
      ranking <- temp4[num]
      hospital <- result$hospital[ranking]
      rankname <- as.character(hospital)
  }
  else{
      stop('invalid number')
  }
  rankname
}