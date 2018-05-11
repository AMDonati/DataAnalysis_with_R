rankhospital<-function(state,outcome,num="best"){
        
        ## Read outcome data
        data<-read.csv("outcome-of-care-measures.csv",colClasses="character")
        
        ## check that state and outcome are valid
        if (!(state %in% data$State)){
                stop("invalid state")
        }
        if (!(outcome %in% c("heart attack","heart failure","pneumonia"))){
                stop("invalid outcome")
        }
        
        outcome_state<-data[data$State==state,]
        
        
        ## Return hospital name in that state with the given rank 30-day death rate 
        if (outcome=="heart attack"){
                outcome_state$Hospital.30.Day.Death..Mortality..Rates.from.Heart.Attack<-as.numeric(outcome_state$Hospital.30.Day.Death..Mortality..Rates.from.Heart.Attack)
                outcome_state<-outcome_state[order(outcome_state$Hospital.30.Day.Death..Mortality..Rates.from.Heart.Attack,outcome_state$Hospital.Name,na.last=NA),]        
        
        }  
        else if (outcome=="heart failure"){
                outcome_state$Hospital.30.Day.Death..Mortality..Rates.from.Heart.Failure<-as.numeric(outcome_state$Hospital.30.Day.Death..Mortality..Rates.from.Heart.Failure)
                outcome_state<-outcome_state[order(outcome_state$Hospital.30.Day.Death..Mortality..Rates.from.Heart.Failure,outcome_state$Hospital.Name,na.last=NA),]
        }       
        else if (outcome=="pneumonia"){
                outcome_state$Hospital.30.Day.Death..Mortality..Rates.from.Pneumonia<-as.numeric(outcome_state$Hospital.30.Day.Death..Mortality..Rates.from.Pneumonia)
                outcome_state<-outcome_state[order(outcome_state$Hospital.30.Day.Death..Mortality..Rates.from.Pneumonia,outcome_state$Hospital.Name,na.last=NA),]    
        }
        
        if (num=="best"){
                num=1
        }
        else if (num=="worst"){
                num=nrow(outcome_state)
        }
        outcome_state$Hospital.Name[num]
        # View(outcome_state)
}