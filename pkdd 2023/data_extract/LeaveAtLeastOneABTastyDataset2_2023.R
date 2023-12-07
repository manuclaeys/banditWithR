#Generate csv with time series from visitors logs
library(jsonlite)

data.train  <- jsonlite::fromJSON("~/data/parcoursuserDatabaseFinalABTastyDataset2_2023.JSON", simplifyDataFrame = TRUE)
visitorReward <- jsonlite::fromJSON("~/Documents/bandit_time_serie_git/cluster/dorcel/parcoursrewardFinalABTastyDataset2_2023.JSON", simplifyDataFrame = TRUE)

#global summary of data
summary(as.factor(visitorReward$A))
summary(as.factor(visitorReward$B))

#which covariate will be observe? 
#here covariates are time series
listInteger   = c( "presence_time_serie" ,  "time_spend_time_serie" ,  "connexion_time_time_serie" )

#don't use time series with lenght smaller than 2
data.train <-data.train[data.train$size_time_serie>1 , ]
#how many transaction do we have now? 
summary(as.factor(data.train$transactions))

#define time series as numerical time series
data.train$presence_time_serie <- lapply(data.train$presence_time_serie, as.numeric)
data.train$connexion_time_time_serie <- lapply(data.train$connexion_time_time_serie, as.numeric)
data.train$time_spend_time_serie <- lapply(data.train$time_spend_time_serie, as.numeric)

#remplace NA by 0 (encoding problem for 2 time series)
for(i in 1:nrow(data.train)) data.train$time_spend_time_serie[i] <- lapply(data.train$time_spend_time_serie[i] ,function(x) replace(x,is.na(x),0))

total <- merge(data.train ,visitorReward, by="fullVisitorId")

rm(list=ls()[! ls() %in% c("total")])

listInteger   = c( "presence_time_serie" ,  "time_spend_time_serie" ,  "connexion_time_time_serie" )

set.seed(123)

library(dtwclust)
k=1
for(k in k:nrow(total)){
  temp =  total[k,]
  size = temp$size_time_serie
  pool = total[ total$size_time_serie==size,]
  pool = pool[- c( pool$fullVisitorId == temp$fullVisitorId) , ]
  while(nrow(pool)==0 && size>0){
    size = size - 1
    pool = total[ "size_time_serie"==size,]
  }
  
  for(i in listInteger ) pool[[paste("max",i,sep = "")]] = rep(NA,nrow(pool))
  
  
  for(j in 1:nrow(pool)){
    for(i in listInteger ){
      pool[[paste("max",i,sep = "")]][j]  = dtw2(pool[[i]][j] , temp[[i]] )$distance
    }
  }
  
  if(is.na(temp$A[1])){
    pool = pool[ is.na(pool$B), ]
    
    list_min = c()
    for(i in listInteger){
      list_min = append( list_min, order( pool[[paste("max",i,sep = "")]]   )[1:5]   )
    }
    pool2 = pool[list_min,]
    total[k,'A'] = rbinom(n=1,1,prob = mean(pool2$transactions))
  }
  
  if(is.na(temp$B[1])){
    pool = pool[ is.na(pool$B), ]
    list_min = c()
    for(i in listInteger){
      list_min = append( list_min, order( pool[[paste("max",i,sep = "")]]   )[1:5]   )
    }
    pool2 = pool[list_min,]
    total[k,'B'] = rbinom(n=1,1,prob = mean(pool2$transactions))
  }
  
}
the_dataset = total[,c("fullVisitorId","A","B")]
#write.csv(the_dataset,"~/data/ABTastyDataset2_2023LeaveAtLeastOne.csv", row.names = TRUE)

