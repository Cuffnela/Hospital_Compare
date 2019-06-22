################################################################################
##                                                                            ##
## Laurie Cuffney 6/20/2019                                                   ## 
## Data used pulled from Hospital Compare website                             ##
## (http://hospitalcompare.hhs.gov)                                           ##
## Focused on data set outcome-of-care-measures.csv                           ##
##                                                                            ##
## Program reads in data and returns vector with name of hospital at given    ##
## rank for 30-day mortaily of provided outcome. Hospitals ranked from best   ##
## (lowest) to worst (highest mortality)                                      ##
##                                                                            ##
################################################################################

#source in best.R to utilize best function
source('~/Code/Coursera-DataScience/Hospital_Compare/best.R')
cleandata<-NULL


################################################################################
## function datacheck checks if data is already cleaned if not calls cleanup  ##
## from best.R and caches cleaned data to avoid recleaning when possible      ##
################################################################################
datacheck<-function(data,outcome){

    if (is.null(cleandata)==TRUE){
        cleandata<<-cleanup(data,outcome)
        message("data cleaned")
    }else{
        message("data already cleaned")
    }
}


################################################################################
## function rankhospital returns name of hospital with the nth rank in outcome## 
## provided to function                                                       ##
################################################################################
rankhospital<-function(state,outcome,num="best"){
    
    #check if input is valid
    validcheck(state,outcome)
    #checks data is clean and cleans if needed
    datacheck(data,outcome)
    
    if(num=="best"){ #if looking for best calls function from best.R
        rankNhospital<-best(state,outcome) 
    }else{ 
        #pulls only required data
        statedata<-cleandata[cleandata$State==state,c(2,index)]
        
        #creates ranking of mortality rates for given outcome and alphabetizes
        #duplicated rates to tie break rank
        rankorder<-statedata[order(statedata[2],statedata[1]),]
        if(num=="worst"){ #pulls from bottom for worst rank
            Nthhospital<-tail(rankorder,n=1)[1]
        }else{
            Nthhospital<-rankorder[num,1]
        }
    }
    print(Nthhospital)
    
}