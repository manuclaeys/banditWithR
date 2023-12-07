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


write.csv(the_dataset,"~/data/ABTastyDataset1_2023LeaveAtLeastOne.csv", row.names = TRUE)
