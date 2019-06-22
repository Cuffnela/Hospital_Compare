## 
## Laurie Cuffney 6/20/2019
## Data used pulled from Hospital Compare website
## (http://hospitalcompare.hhs.gov)
## Focused on data set outcome-of-care-measures.csv
## 
## Program reads in data and finds the hospital with the best mortality rate for
## a provided outcome

## read in data from outcome-of-care-measures.csv
data<- read.csv("outcome-of-care-measures.csv")

## vector of accepted outcomes
valid <<- c("heart attack"=11, "heart failure"= 17,"pneumonia"=23)

## function checks for valid state and outcome inputs
validcheck<-function(state,outcome){
    
    statecheck<-state %in% state.abb
    outcomecheck<-outcome %in% names(valid)
    if (statecheck=="FALSE"){
        stop("invalid state")
    }else if(outcomecheck=="FALSE"){
        stop("invalid outcome")
    }
}

# function cleans data and ensures that data is numeric for outcome columns
cleanup<-function(data,outcome){
  cleandata<-NULL
  ## Get outcome column reference for data set
  index<<-valid[[outcome]]
  
  ## coerce Hospital 30 day death rates of given outcome
  ## to numerics may produce warning message due to NA, this is fine
  data[,index]<-as.numeric(paste(data[,index]))
  
  bad<-is.na(data[,index])
  cleandata<<-data[!bad,]
  
}


# function best takes two arguments: State  abbreviated, outcome name
# returns name of hospital with best (lowest) 30-day mortality for given outcome
# in the case of a tie: first alphabetic hospital returned
best<-function(state,outcome){

    ## checks for valid input and returns an error if incorrect
    validcheck(state,outcome)
    

    ## Read outcome data
    cleanup(data,outcome)
    mins<-tapply(cleandata[,index],cleandata$State,min)
    statemin<-mins[state]
    besthospital<-cleandata[cleandata[7]==state & cleandata[,index]==statemin][2]
    
    # Create histogram of 30 day death rates from outcome
    hist(cleandata[,index],xlab="30 day mortality rate", ylab="frequency",
         main=paste("Historgram of", outcome))
    
    # tie breaker: alphabetize and return first in list
    if (length(besthospital)>1){
         alphahospital<-sort(besthospital)
         print(paste("alphahospital",class(alphahospital)))
         besthospital<-alphahospital[1]
      }
     print(besthospital)
}
