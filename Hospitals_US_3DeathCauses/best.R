best<-function(state,outcome){
        ## Read outcome data
        data<-read.csv("outcome-of-care-measures.csv",colClasses="character")
        
        ## Check that state and outcome are valid
        if (!(state %in% data$State)){
                stop("invalid state")
        }
        if (!(outcome %in% c("heart attack","heart failure","pneumonia"))){
                stop("invalid outcome")
        }
        
        outcome_state<-data[data$State==state,]
        
        ## Return hospital name in that state with lowest 30-day death rate
        if (outcome=="heart attack"){
                outcome_state$Hospital.30.Day.Death..Mortality..Rates.from.Heart.Attack<-as.numeric(outcome_state$Hospital.30.Day.Death..Mortality..Rates.from.Heart.Attack)
                outcome_min<-outcome_state[outcome_state$Hospital.30.Day.Death..Mortality..Rates.from.Heart.Attack==min(outcome_state$Hospital.30.Day.Death..Mortality..Rates.from.Heart.Attack,na.rm=TRUE),]
                }  
        else if (outcome=="heart failure"){
                outcome_state$Hospital.30.Day.Death..Mortality..Rates.from.Heart.Failure<-as.numeric(outcome_state$Hospital.30.Day.Death..Mortality..Rates.from.Heart.Failure)
              outcome_min<-outcome_state[outcome_state$Hospital.30.Day.Death..Mortality..Rates.from.Heart.Failure==min(outcome_state$Hospital.30.Day.Death..Mortality..Rates.from.Heart.Failure,na.rm=TRUE),]
        } 
        else if (outcome=="pneumonia"){
                outcome_state$Hospital.30.Day.Death..Mortality..Rates.from.Pneumonia<-as.numeric(outcome_state$Hospital.30.Day.Death..Mortality..Rates.from.Pneumonia)  
              outcome_min<-outcome_state[outcome_state$Hospital.30.Day.Death..Mortality..Rates.from.Pneumonia==min(outcome_state$Hospital.30.Day.Death..Mortality..Rates.from.Pneumonia,na.rm=TRUE),]
        }  
        
        hospitalName<-outcome_min$Hospital.Name
       
        if (length(hospitalName>1)){
                 hospitalName<-sort(hospitalName)[1]
        }
        hospitalName
}