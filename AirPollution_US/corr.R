corr<-function(directory,threshold=0){
        # return the vector of correlation between nitrate and sulfate for the monitor location 
        #with # of complete cases > threshold
        
        # set directory and list files 
        old_dir<-getwd()
        if (old_dir!=directory){
                setwd(directory)
        }
        l<-list.files()
        correl<-vector(mode="numeric")
        j<-1
        setwd(old_dir)
        
        #calling the function complete to get the number of complete cases for each monitor location
        cc<-complete(directory)
        setwd(directory)
      
        #getting the sub dataframe of complete with rows <-> complete cases > thresold
        cc2<-cc[cc$nobs>threshold,]
        
        #checking if the threshold is reached: 
        if (nrow(cc2)!=0){
                #reading the files corresponding on the monitor locations meeting the threshold requirement 
                #calculate the correlation for each of these files and store it in a vector
                for (i in cc2$id){
                        data<-read.csv(l[i])
                        correl[j]<-cor(data$sulfate,data$nitrate,use="na.or.complete")
                        j<-j+1
                }
        }
        setwd(old_dir)
        correl
        }

