rankall<-function(outcome,num="best"){
        ## Read outcome data
        data<-read.csv("outcome-of-care-measures.csv",colClasses="character")
        
        ## Check that state and outcome are valid
        if (!(outcome %in% c("heart attack","heart failure","pneumonia"))){
                stop("invalid outcome")
        }
        ## For each state, find the hospital of the given rank
        s<-split(data,data$State)
        
        FUN<-function(x,num){
                if (outcome=="heart attack"){
                        x$Hospital.30.Day.Death..Mortality..Rates.from.Heart.Attack<-as.numeric(x$Hospital.30.Day.Death..Mortality..Rates.from.Heart.Attack)
                        x<-x[order(x$Hospital.30.Day.Death..Mortality..Rates.from.Heart.Attack,x$Hospital.Name,na.last=NA),]        
                        
                }  
                else if (outcome=="heart failure"){
                        x$Hospital.30.Day.Death..Mortality..Rates.from.Heart.Failure<-as.numeric(x$Hospital.30.Day.Death..Mortality..Rates.from.Heart.Failure)
                        x<-x[order(x$Hospital.30.Day.Death..Mortality..Rates.from.Heart.Failure,x$Hospital.Name,na.last=NA),]
                }       
                else if (outcome=="pneumonia"){
                        x$Hospital.30.Day.Death..Mortality..Rates.from.Pneumonia<-as.numeric(x$Hospital.30.Day.Death..Mortality..Rates.from.Pneumonia)
                        x<-x[order(x$Hospital.30.Day.Death..Mortality..Rates.from.Pneumonia,x$Hospital.Name,na.last=NA),]    
                }
                
                if (num=="best"){
                        num=1
                }
                else if (num=="worst"){
                        num=nrow(x)
                }
                x$Hospital.Name[num]
        }
        
        ## Return a dataframe with the hospital names and the state names 
        mat<-sapply(s,FUN,num=num)
        df<-as.data.frame(mat)
        colnames(df)<-"hospital"
        df$state=rownames(df)
        df
}