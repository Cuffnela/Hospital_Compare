## 
## Laurie Cuffney 6/20/2019
## Data used pulled from Hospital Compare website
## (http://hospitalcompare.hhs.gov)
## Focused on two data sets hospital-data.csv and outcome-of-care-measures.csv
## 
## Program reads in data and finds the hospital with the best mortality rate for
## a provided outcome

## read in data from outcome-of-care-measures.csv
data<- read.csv("outcome-of-care-measures.csv")

## coerce Hospital 30 day death rates from heart attack to numerics
## may produce warning message due to NA, this is fine
data[,11]<-as.numeric(data[,11])
# Create histogram of 30 day death rates from heart attack
hist(data[,11])



# function best takes two arguments: State  abbreviated, outcome name
# returns name of hospital with best (lowest) 30-day mortality for given outcome
# in the case of a tie: first alphabetic hospital returned
best<-function(state,outcome){
    ## vector of accepted outcomes
    valid <- c("heart attack"=11, "heart failure"= 17,"pneumonia"=23)
    
    ## checks for valid input and returns an error if incorrect
    statecheck<-state %in% state.abb
    outcomecheck<-outcome %in% valid
    if (statecheck=="FALSE"){
        stop("invalid state")
    }else if(outcomecheck=="FALSE"){
        stop("invalid outcome")
    }
    
    ## Get outcome column reference for data set
    index<-valid[[outcome]]

    ## Read outcome data
    stateSelect<-data$State==state
    statedata<-data[stateSelect,]
  
    # find minimum/best outcome
    mindeath<-min(as.numeric(paste(statedata[,index])))
   
    #get hospital name(s)
    findhospital<-statedata[index]==mindeath
    besthospital<-statedata[findhospital,][2]
    
    # tie breaker: alphabetize and return first in list
    if (nrow(besthospital)>1){
        alphahospital<-sort(besthospital[,1])
        print("alphahospital")
        besthospital<-alphahospital[1,1]
    }
    print(besthospital)
}
