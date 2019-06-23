################################################################################
##                                                                            ##
## Laurie Cuffney 6/22/2019                                                   ## 
## Data used pulled from Hospital Compare website                             ##
## (http://hospitalcompare.hhs.gov)                                           ##
## Focused on data set outcome-of-care-measures.csv                           ##
##                                                                            ##
## Program reads in data and returns data frame  with name of hospital        ##
## in each state at a given rank for 30-day mortaily of provided outcome.     ##
## Hospitals ranked from best (lowest) to worst (highest) mortality           ##
##                                                                            ##
################################################################################


#read in data from outcome-of-care-measures.csv
data<- read.csv("outcome-of-care-measures.csv")

#vector of accepted outcomes, cached for other program use
valid <- c("heart attack"=11, "heart failure"= 17,"pneumonia"=23)

################################################################################
## function checks for valid state and outcome inputs                         ##
################################################################################
validcheck<-function(outcome){
    
    outcomecheck<-outcome %in% names(valid)
    if(outcomecheck=="FALSE"){
        stop("invalid outcome")
    }
}

################################################################################
## function cleans data and ensures that data is numeric for outcome columns  ##
################################################################################
cleanup<-function(data,index){
    cleandata<-NULL
    
    #coerce Hospital 30 day death rates of given outcome
    #to numerics may produce warning message due to NA, this is fine
    data[,index]<-as.numeric(paste(data[,index]))
    
    bad<-is.na(data[,index])
    cleandata<<-data[!bad,]
    
}

################################################################################
## function rankall takes two arguments outcome and rank and returns a data   ##
## frame with the hospital in each state that has the ranking specified.      ##
################################################################################
rankall<-function(outcome,num="best"){
    #checks for valid input and returns an error if incorrect
    validcheck(outcome)
    
    #get outcome column reference for data set
    index<-valid[[outcome]]
    
    #cleans data if needed
    cleanup(data,index)
    
    #sorts and ranks data
    simplifydata<-cleandata[,c(2,7,index)]
    datasplit<-split(simplifydata,simplifydata$State)
    ranked<-lapply(datasplit, function(x) x[order(x[,3],x[,1]),])
    
    if(num=="best"){
        Nthranks<-lapply(ranked,function(x) x[1,])
        combinelist<-do.call(rbind,Nthranks)[,c(1,2)]
    }else if(num=="worst"){
        Nthranks<-lapply(ranked,function(x) tail(x,n=1))
        combinelist<-do.call(rbind,Nthranks)[,c(1,2)]
    }else{
        Nthranks<-lapply(ranked,function(x) x[num,])
        combinelist<-do.call(rbind,Nthranks)[,c(1,2)]
    }
    combinelist
    
}

