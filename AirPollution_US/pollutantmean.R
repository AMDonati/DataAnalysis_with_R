pollutantmean <- function(directory,pollutant,id=1:332){
    #return the mean of a pollutant accross all monitor list in the id vector (ignoring NA values)
    #directory is a charactor vector of length 1 indicating the location of the CSV files 
    
    #set the directory to directory 
    #reading the files in id and converting them to a dataframe with read.csv
    #extracting the right value in each file: the vector of the pollutant values 
    #store this value in a vector 
    #calculate the mean of this vector, ignoring NA values 
    
    setwd(directory)
    vect<-vector(mode="numeric")
    
    for (i in id){
            l<-list.files()
            data<-read.csv(l[i])
                
        #extracting the right data and store it in a vector 
        if (pollutant=="sulfate"){
            polvalues<-data$sulfate
        }
        else if (pollutant=="nitrate"){
                polvalues<-data$nitrate
        }
        else {print("please select a valid pollutant")}
        vect=c(vect,polvalues)
    }
    # calculating the mean of concatenated vector, ignoring the NA values 
    setwd("..")
    mean(vect,na.rm=TRUE)
}