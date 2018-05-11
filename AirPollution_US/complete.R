complete <- function(directory, id=1:332){
        # return a dataframe with the number of complete cases of each file in the directory 
        # the first column is the ID of the file 
        # the second column is the number of complete cases 
        
        #set the directory to directory 
        old_dir<-getwd()
        if (old_dir!=directory){
        setwd(directory)
        }
                
        l<-length(id)
        lf<-list.files()
       
        #create an empty dataframe 
        df<-data.frame("id"=numeric(l),"nobs"=numeric(l))
        j<-1
        
        #fill the dataframe with the ID and the number of complete cases for each file 
        for (i in id){
          #fill the dataframe with the ID and the number of complete cases for each file
        data<-read.csv(lf[i])
        df$id[j]<-i
        df$nobs[j]<-sum(complete.cases(data))
        j<-j+1
           
        }
        #reset the directory to old directory 
        setwd(old_dir)
        df
}
