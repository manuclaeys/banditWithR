#library(devtools)
#install_github("https://github.com/manuclaeys/banditWithR")

library(banditwithR)
library(partykit)

remove(list = ls())
set.seed(1234)


library(rstudioapi)

# Getting the path of your current open file
current_path = rstudioapi::getActiveDocumentContext()$path
setwd(dirname(current_path ))
print( getwd() )

library(stringr)

#Importe data
library(jsonlite)
library(readr)

data.train  <- jsonlite::fromJSON(paste(str_sub( getwd() , end = -10),"data/parcoursuserDatabaseFinalABTastyDataset1_2023.JSON",sep=""), simplifyDataFrame = TRUE)
visitorReward <- read.csv2(paste(str_sub( getwd() , end = -10),"data/ABTastyDataset1_2023LeaveAtLeastOne.csv",sep=""), header = TRUE, sep = ',',colClasses=c("fullVisitorId"="character"))
visitorReward$X = NULL


total <- merge(data.train ,visitorReward, by="fullVisitorId")


#global summary of data
summary(as.factor(total$A))
summary(as.factor(total$B))

#which covariate will be observe?
#here covariates are time series
listInteger   = c( "presence_time_serie" ,  "time_spend_time_serie" ,  "connexion_time_time_serie" )

#don't use time series with lenght smaller than 2
total <-total[total$size_time_serie>1 , ]
#how many transaction do we have now?
summary(as.factor(total$transactions))

#define time series as numerical time series
total$presence_time_serie <- lapply(total$presence_time_serie, as.numeric)
total$connexion_time_time_serie <- lapply(total$connexion_time_time_serie, as.numeric)
total$time_spend_time_serie <- lapply(total$time_spend_time_serie, as.numeric)

#remplace NA by 0 (encoding problem for 2 time series)
for(i in 1:nrow(total)) total$time_spend_time_serie[i] <- lapply(total$time_spend_time_serie[i] ,function(x) replace(x,is.na(x),0))

library(tidyr)
total = total %>% drop_na(A)
total = total %>% drop_na(B)


rm(list=ls()[! ls() %in% c("total")])


listSerie = c("presence_time_serie","time_spend_time_serie","connexion_time_time_serie")
listSerieCluster = c(5:20)

library(dtwclust)
library(doParallel)

clusterFunction <- function(dt,listSerieUnique,listKCentroidsUnique){
  #fast clustering

  cl <- makeCluster(detectCores())
  invisible(clusterEvalQ(cl, library(dtwclust)))
  registerDoParallel(cl)

  #init

    # DBA
      ts <-   tsclust( dt[[listSerieUnique]], k = listKCentroidsUnique,
                       centroid = "dba",
                       seed = 3251, trace = TRUE)


  # Stop parallel workers
  stopCluster(cl)

  # Return to sequential computations. This MUST be done after stopCluster()
  registerDoSEQ()

  return(ts@cluster)

}

#Add clusters
#presence_time_serie
for(i in listSerieCluster){
  total[[paste("cluster","presence_time_serie",i,sep = "")]] = clusterFunction(dt=total,
                                                                      listSerieUnique ="presence_time_serie",
                                                                      listKCentroidsUnique=i)
}


for(i in listSerieCluster){
  total[[paste("cluster","presence_time_serie",i,sep = "")]] = as.factor( total[[paste("cluster","presence_time_serie",i,sep = "")]])
}

total$A = as.factor(total$A)
total$B = as.factor(total$B)

### multivariate responses A + B
library(partykit)
treeRes <- ctree(A+B ~ clusterpresence_time_serie5 +
                   clusterpresence_time_serie6 +
                   clusterpresence_time_serie7 +
                   clusterpresence_time_serie8 +
                   clusterpresence_time_serie9 +
                   clusterpresence_time_serie10 +
                   clusterpresence_time_serie11 +
                   clusterpresence_time_serie12 +
                   clusterpresence_time_serie13 +
                   clusterpresence_time_serie14 +
                   clusterpresence_time_serie15 +
                   clusterpresence_time_serie16 +
                   clusterpresence_time_serie17 +
                   clusterpresence_time_serie18 +
                   clusterpresence_time_serie19 +
                   clusterpresence_time_serie20
                 , data = total)
treeRes
plot(treeRes)

###############


#Add clusters
#presence_time_serie
for(i in listSerieCluster){
  total[[paste("cluster","time_spend_time_serie",i,sep = "")]] = clusterFunction(dt=total,
                                                                             listSerieUnique ="time_spend_time_serie",
                                                                             listKCentroidsUnique=i)
}

for(i in listSerieCluster){
  total[[paste("cluster","time_spend_time_serie",i,sep = "")]] = as.factor( total[[paste("cluster","time_spend_time_serie",i,sep = "")]])
}


library(partykit)
treeRes <- ctree(B~ clustertime_spend_time_serie5 +
                   clustertime_spend_time_serie6 +
                   clustertime_spend_time_serie7 +
                   clustertime_spend_time_serie8 +
                   clustertime_spend_time_serie9 +
                   clustertime_spend_time_serie10 +
                   clustertime_spend_time_serie11 +
                   clustertime_spend_time_serie12 +
                   clustertime_spend_time_serie13 +
                   clustertime_spend_time_serie14 +
                   clustertime_spend_time_serie15 +
                   clustertime_spend_time_serie16 +
                   clustertime_spend_time_serie17 +
                   clustertime_spend_time_serie18 +
                   clustertime_spend_time_serie19 +
                   clustertime_spend_time_serie20
                 , data = total)
treeRes
plot(treeRes)

###############


#Add clusters
#presence_time_serie
for(i in listSerieCluster){
  total[[paste("cluster","connexion_time_time_serie",i,sep = "")]] = clusterFunction(dt=total,
                                                                                 listSerieUnique ="connexion_time_time_serie",
                                                                                 listKCentroidsUnique=i)
}

for(i in listSerieCluster){
  total[[paste("cluster","connexion_time_time_serie",i,sep = "")]] = as.factor( total[[paste("cluster","connexion_time_time_serie",i,sep = "")]])
}

library(partykit)
treeRes <- ctree(B~ clusterconnexion_time_time_serie5 +
                   clusterconnexion_time_time_serie6 +
                   clusterconnexion_time_time_serie7 +
                   clusterconnexion_time_time_serie8 +
                   clusterconnexion_time_time_serie9 +
                   clusterconnexion_time_time_serie10 +
                   clusterconnexion_time_time_serie11 +
                   clusterconnexion_time_time_serie12 +
                   clusterconnexion_time_time_serie13 +
                   clusterconnexion_time_time_serie14 +
                   clusterconnexion_time_time_serie15 +
                   clusterconnexion_time_time_serie16 +
                   clusterconnexion_time_time_serie17 +
                   clusterconnexion_time_time_serie18 +
                   clusterconnexion_time_time_serie19 +
                   clusterconnexion_time_time_serie20
                 , data = total)
treeRes
plot(treeRes)

