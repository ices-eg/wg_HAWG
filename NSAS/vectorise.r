vectorise<-
  function (tab){
    #This function converts the output of tapply (matrix) into a data.frame (relational database format)
    n<-length(attributes(tab)[[1]])
    dims<-attributes(tab)[[1]]
    len<-prod(attributes(tab)[[1]])
    d2<-c(dims, 0)
    #set up the data frame to be the correct length
    df<-data.frame(as.vector(tab))
    names(df)<-"value"
    j<-2
    for(i in 1 : n){
      ech<- max(1,prod(dims[0:(i-1)]))  # this is the number of sets
      reps<-max(1,prod(d2[(i+1):n]))  # this is the number of repeats of each number within a set
      df[j]<-rep(dimnames(tab)[[i]],reps,each=ech)
      j<-j+1
    }
    df
  }