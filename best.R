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

## coerce Hospital 30 day death rates from heart attack, heart failure, and 
## pneumonia to numerics may produce warning message due to NA, this is fine
data[,11]<-as.numeric(data[,11])
data[,17]<-as.numeric(paste(data[,17]))
data[,23]<-as.numeric(paste(data[,23]))
# Create histogram of 30 day death rates from heart attack
hist(data[,11])

## function checks for valid state and outcome inputs
validcheck<-function(state,outcome){
    ## vector of accepted outcomes
    valid <<- c("heart attack"=11, "heart failure"= 17,"pneumonia"=23)
    
    statecheck<-state %in% state.abb
    outcomecheck<-outcome %in% names(valid)
    if (statecheck=="FALSE"){
        stop("invalid state")
    }else if(outcomecheck=="FALSE"){
        stop("invalid outcome")
    }
}


# function best takes two arguments: State  abbreviated, outcome name
# returns name of hospital with best (lowest) 30-day mortality for given outcome
# in the case of a tie: first alphabetic hospital returned
best<-function(state,outcome){

    ## checks for valid input and returns an error if incorrect
    validcheck(state,outcome)
    
    ## Get outcome column reference for data set
    index<-valid[[outcome]]

    ## Read outcome data
    bad<-is.na(data[,index])
    cleandata<-data[!bad,]
    mins<-tapply(cleandata[,index],cleandata$State,min)
    statemin<-mins[state]
    besthospital<-cleandata[cleandata[7]==state & cleandata[,index]==statemin][2]
    
    # tie breaker: alphabetize and return first in list
    if (length(besthospital)>1){
         alphahospital<-sort(besthospital)
         print(paste("alphahospital",class(alphahospital)))
         besthospital<-alphahospital[1]
      }
     print(besthospital)
}
