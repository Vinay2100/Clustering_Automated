#install it if it's not present on ur system
install.packages('xlsx')
install.packages('dummy')
install.packages("animation")
# P R E S S "Ctrl+ENTER"
# H E R E The first click and last click
vmclustering<-function()
{
  
  library(xlsx)
  t<-readline(prompt = "Type 'csv' (or) 'xlsx':")
  if("csv" %in% t){
    dataset<-read.csv(file.choose()) #or
  }
  if("xlsx" %in% t){
    sh<-readline(prompt = "Enter sheet no. of the xlsx file:")
    dataset<-read.xlsx(file.choose(),as.integer(sh))
  }
  
  {
    
    Column_Names<-colnames(dataset)
    Numeric<-sapply(dataset, is.numeric)
    Index<-c(1:ncol(dataset))
    ab<-cbind(Index,Column_Names,Numeric)
    print(ab)
    View(dataset)
    d<-readline(prompt = "Do you want to remove any unnecessary (or) label data: y (or) n :")
    if("y" %in% d)
    {
      j<-as.integer(strsplit(readline(prompt = "Enter the index you want to remove[seperate the index with commas if multiple]: "),",")[[1]])
      dataset_numerical<-data.frame(dataset[-c(j)])
      View(dataset_numerical)
    }
    else
    {
      dataset_numerical<-dataset[]
      
    }
    
    
    Index<-c(1:ncol(dataset_numerical))
    Numeric<-sapply(dataset_numerical, is.numeric)
    ab<-cbind(Index,Numeric)
    print(ab)
    View(dataset_numerical)
    d<-readline(prompt = "Do you want to make any Dummy data(categorical): y (or) n :")
    
    if("y" %in% d)
    {
      j<-as.integer(strsplit(readline(prompt = "enter the index you want to dummify[seperate the indexes with commas if multiple]: "),",")[[1]])
      dataset1<-dataset_numerical[,-c(j)]
      dataset2<-dataset_numerical[c(j)]
      
      library('dummy')
      dataset2<-data.frame(dummy(x=dataset2))
      
      {
        #Normalization function for Binary scaling
        normalize <- function(x)
        {
          return ((x - min(x)) / (max(x) - min(x)))
        }
        dfNorm <- as.data.frame(lapply(dataset1, normalize))
        
        normalized_data<-data.frame(dfNorm,dataset2)#cleaned dataset with only numeric data is taken
        View(normalized_data)
        j<-1
        k<-data.frame()
        wss<-NULL
      }
      
      
    }
    else
    {
      
      normalized_data<-scale(dataset_numerical)#cleaned dataset with only numeric data is taken
      j<-1
      k<-data.frame()
      
      
      
    }
    plots<-function()
      {
    wss<-NULL
    n<-nrow(dataset)
    mlast<-as.integer(2*sqrt(n/2))
    if(mlast>50)
    {
      mlast=50
    }
    for (m in 2:mlast)
    {
      km<-kmeans(normalized_data,centers = m)
      wss<-c(wss,km$tot.withinss)
      k[j,1]<-km$tot.withinss
      k[j,2]<-km$betweenss
      j<-j+1
    }
    wss
    plot(2:mlast,wss,type = "b",xlab = "no. of clusters",ylab = "tot.withinss")
    title(main = "scree-plot")
    colnames(k)<-c("totalwithinss","betweenss")
    
    readinteger<-function()
    {
      i <- readline(prompt="Enter the desired(or)decided value of clusters: ")
      return(as.integer(i))
    }
    
    readinteger1<-function()
    {
      i <- readline(prompt="Enter the no. of clusters for Animation: ")
      return(as.integer(i))
    }
    
    km<-kmeans(normalized_data,readinteger())
    kmeans_aggregate<-aggregate(normalized_data,by=list(km$cluster),FUN=mean)
    final<-data.frame(km$cluster,dataset)
    View(kmeans_aggregate)
    print(km)####
    library(animation)
    
    km <- kmeans.ani(normalized_data,readinteger1())
    km$centers
    
    f<-readline(prompt = "Do you want to store the membership file: y(or)n :")
    if("y" %in% f)
    {
      y <- readline(prompt="Enter the name of the file & mention xlsx extension: ")
      library(xlsx)
      write.xlsx(final,file = y)
      print("Clustering Done!!!")
      
    }
    if("n" %in% f)
    {
      View(kmeans_aggregate)
      print(km)#####
      
    }
    }
    plots()
    ####################################
    readiter<-function()
    {
      ik <- readline(prompt="Want to run again? If yes press 1::If no press 0: ")
      return(as.integer(ik))
    }
    rpeat=readiter()
    
    while(rpeat==1){
      
      plots()
      rpeat=readiter()
    }
      if(rpeat==0)
      {
        
        f<-readline(prompt = "Do you want to store the recent(or)last iterated membership file again: y(or)n :")
        if("y" %in% f)
        {
          y <- readline(prompt="Enter the name of the file with xlsx extension: ")
          library(xlsx)
          write.xlsx(final,file = y)
        }  
          if("n" %in% f)
          {
            print("clustering done !!!")
          }
          
          
        
      }
    
  }
}

vmclustering()

