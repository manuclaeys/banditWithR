#library(devtools)
#install_github("https://github.com/manuclaeys/banditWithR")
library(banditwithR)
library(partykit)

remove(list = ls())
set.seed(1234)


library(rstudioapi)
library(stringr)

# Getting the path of your current open file
current_path = rstudioapi::getActiveDocumentContext()$path
setwd(dirname(current_path ))
print( getwd() )



#Importe data
library(jsonlite)
library(readr)

data.train  <- jsonlite::fromJSON(paste(str_sub( getwd() , end = -10),"data/parcoursuserDatabaseFinalABTastyDataset1_2023.JSON",sep=""), simplifyDataFrame = TRUE)
visitorReward <- jsonlite::fromJSON(paste(str_sub( getwd() , end = -10),"data/parcours_reward_ABTastyDataset1_2023.JSON",sep=""), simplifyDataFrame = TRUE)



total <- merge(data.train ,visitorReward, by="fullVisitorId")


#global summary of data
summary(as.factor(total$A))
summary(as.factor(total$B))

#which covariate will be observe?
#here covariates are time series
listInteger   = c( "presence_time_serie" ,  "time_spend_time_serie" ,  "connexion_time_time_serie" )

#don't use time series with lenght smaller than 2
total <-total[total$size_time_serie>1, ]
#how many transaction do we have now?
summary(as.factor(total$transactions))

#define time series as numerical time series
total$presence_time_serie <- lapply(total$presence_time_serie, as.numeric)
total$connexion_time_time_serie <- lapply(total$connexion_time_time_serie, as.numeric)
total$time_spend_time_serie <- lapply(total$time_spend_time_serie, as.numeric)

#remplace NA by 0 (encoding problem for 2 time series)
for(i in 1:nrow(total)) total$time_spend_time_serie[i] <- lapply(total$time_spend_time_serie[i] ,function(x) replace(x,is.na(x),0))



data.train = total[1:as.integer(nrow(total)*1),]



rm(list=ls()[! ls() %in% c("data.train","listInteger")])


######Clusters####
###get the optimal cluster number

library(dtwclust)
require(doParallel)
library(xtable)

pc.dba_clust <- list()

for(i in listInteger){
	print(i)
	cl <- makeCluster(detectCores())
	invisible(clusterEvalQ(cl, library(dtwclust)))
	registerDoParallel(cl)

	pc.dba <- tsclust(data.train[[i]], k = 3L:20L, centroid = "dba",
					  seed = 3251, trace = TRUE,
					  control = partitional_control(nrep = 1L,iter.max = 10L )
	)

	names(pc.dba) <- paste0("k_", 3L:20L)
	res <- sapply(pc.dba, cvi, type = "internal")

	print(res)

	for(j in 1:nrow(res))  plot(res[j,], xlab=paste(i,rownames(res)[j]), sep=" " )
	print(xtable(res, type = "latex"))


	# Stop parallel workers
	stopCluster(cl)

	# Return to sequential computations. This MUST be done after stopCluster()
	registerDoSEQ()

}
