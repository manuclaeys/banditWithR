#library(devtools)
#install_github("https://github.com/manuclaeys/banditWithR")

library(banditwithR)
library(partykit)
library(matlib)
remove(list = ls())
set.seed(1234)


# Getting the path of your current open file
current_path = rstudioapi::getActiveDocumentContext()$path
setwd(dirname(current_path ))
print( getwd() )



#Importe data
library(jsonlite)
library(readr)
library(stringr)

data.train  <- jsonlite::fromJSON(paste(str_sub( getwd() , end = -10),"data/parcoursuserDatabaseFinalABTastyDataset1_2023.JSON",sep=""), simplifyDataFrame = TRUE)

visitorReward <- read.csv2(paste(str_sub( getwd() , end = -10),"data/ABTastyDataset1_2023LeaveAtLeastOne.csv",sep=""), header = TRUE, sep = ',',colClasses=c("fullVisitorId"="character"))
visitorReward$X = NULL
#don't use time series with lenght smaller than 2
data.train <-data.train[data.train$size_time_serie>1 , ]

total <- merge(data.train ,visitorReward, by="fullVisitorId")



#global summary of data
summary(as.factor(total$A))
summary(as.factor(total$B))



#which covariate will be observe?
#here covariates are time series
listInteger   = c( "presence_time_serie" ,  "time_spend_time_serie" ,  "connexion_time_time_serie" )


#define time series as numerical time series
total$presence_time_serie <- lapply(total$presence_time_serie, as.numeric)
total$connexion_time_time_serie <- lapply(total$connexion_time_time_serie, as.numeric)
total$time_spend_time_serie <- lapply(total$time_spend_time_serie, as.numeric)

#remplace NA by 0 (encoding problem for 2 time series)
#for(i in 1:nrow(total)) total$time_spend_time_serie[i] <- lapply(total$time_spend_time_serie[i] ,function(x) replace(x,is.na(x),0))

library(tidyr)
total = total %>% drop_na(A)
total = total %>% drop_na(B)


total  = total[sample(nrow(total)),]


rm(list=ls()[! ls() %in% c("total")])

#define time series as numerical time series
total$presence_time_serie <- lapply(total$presence_time_serie, as.numeric)
total$connexion_time_time_serie <- lapply(total$connexion_time_time_serie, as.numeric)
total$time_spend_time_serie <- lapply(total$time_spend_time_serie, as.numeric)

#remplace NA by 0 (encoding problem for 2 time series)
for(i in 1:nrow(total)) total$time_spend_time_serie[i] <- lapply(total$time_spend_time_serie[i] ,function(x) replace(x,is.na(x),0))



presence_time_serie = 3
time_spend_time_serie = 5
connexion_time_time_serie = 5


#presence_time_serie = 5
#time_spend_time_serie = 5
#connexion_time_time_serie = 10



#learn_size = 11120 #100% of original
parameter_size = 0.3
#parameter_size = 1
  if(parameter_size == 1){
    total = rbind(total,total)
    parameter_size = 0.5
  }

  learn_size = nrow(total)*parameter_size   #30 ou 100% of dataset original


  visitor_reward = total[,c("A","B")]
  dt = total[,c("presence_time_serie","time_spend_time_serie","connexion_time_time_serie")]

  K=ncol(visitor_reward)

  arm_for_learn = 'A'

  #Parametrage
  ctree_parameters_control=ctreeucb_parameters_control_default(dt,visitor_reward,
                                                               alpha = 1,learn_size =learn_size,
                                                               is_reward_are_boolean = TRUE,
                                                               arm_for_learn = arm_for_learn,
                                                               ctree_control_val = ctree_control(alpha = 0.1)
                                                               )

  listSerie = c("presence_time_serie","time_spend_time_serie","connexion_time_time_serie")
  listKCentroids=c(presence_time_serie,time_spend_time_serie,connexion_time_time_serie)

  resVal <- dbactreeucbRejectionSamplingBanditObjectEvaluation(dt,
                                                               visitor_reward,
                                                               K,
                                                               listSerie, listKCentroids ,
                                                               ctree_parameters_control)

  #Evaluation
  choiceList <- resVal$dbactreeucb_rejection_sampling_bandit_alloc$choice
  summary(as.factor(choiceList))
  rewardABtest <- visitor_reward[ resVal$dbactreeucb_rejection_sampling_bandit_alloc$first_train_element: nrow(dt)   , ]
  plot(resVal$cum_rew_dbactreeucb_rejection_sampling_alloc, type='l')
  averageReward_dbactreeucb = max(resVal$cum_rew_dbactreeucb_rejection_sampling_alloc)/length(resVal$cum_rew_dbactreeucb_rejection_sampling_alloc)
  averageReward_dbactreeucb
  regret1 <- cumulativeRegret(choiceList ,rewardABtest)
  reward1 <- resVal$cum_rew_dbactreeucb_rejection_sampling_alloc
  max(regret1)

  ###### Comparatif avec DBALinUCB
  dbalinucb_rejection_sampling_alloc  <- dbalinucbRejectionSamplingBanditObjectEvaluation(dt,
                                                                      visitor_reward,
                                                                      alpha=1, K=K,
                                                                      listSerie, listKCentroids ,
                                                                      learn_size = as.integer(nrow(dt)*parameter_size),
                                                                      IsRewardAreBoolean = FALSE,
                                                                      listCategorial=0 )

  #Evaluation
   choiceListDBALINUCB <-  dbalinucb_rejection_sampling_alloc$dbalinucb_rejection_sampling_bandit_alloc$choice
   summary(as.factor(choiceListDBALINUCB))

   averageReward_dbalinucb = max(dbalinucb_rejection_sampling_alloc$cum_rew_dbalinucb_rejection_sampling_alloc)/length(dbalinucb_rejection_sampling_alloc$cum_rew_dbalinucb_rejection_sampling_alloc)
   regret2 <- cumulativeRegret(choiceListDBALINUCB,rewardABtest)
   reward2 <- dbalinucb_rejection_sampling_alloc$cum_rew_dbalinucb_rejection_sampling_alloc
   max(regret2)
   barplot(prop.table(table(choiceListDBALINUCB)))


  ###### Comparatif avec LinUCB
 explanatory_variable = vector()
  for(i in listSerie){
    explanatory_variable =  append( explanatory_variable, paste("mean",i,sep = ""))
    dt[[paste("mean",i,sep = "")]] <- sapply(dt[[i]],mean)
  }

  library(matlib)
    linucb_alloc  <- LinucbRejectionSamplingBanditObjectEvaluation(
    dt = dt[ resVal$dbactreeucb_rejection_sampling_bandit_alloc$first_train_element: nrow(dt)   , explanatory_variable],
    visitor_reward =  rewardABtest,
    alpha = 1,
    K = ncol(visitor_reward),
#    average = FALSE,
    IsRewardAreBoolean = FALSE
    )



  #Evaluation
  choiceListLINUCB <- linucb_alloc$linucb_rejection_sampling_bandit_alloc$choice
  summary(as.factor(choiceListLINUCB))

  regret3 <- cumulativeRegret(choiceListLINUCB,rewardABtest)
  reward3 <- linucb_alloc$cum_rew_linucb_rejection_sampling_alloc
  max(regret3)
  barplot(prop.table(table(choiceListLINUCB)))


  #### CTREE UCB
  library(partykit)
  parameters = ctreeucb_parameters_control_default(
    dt =  dt[ , explanatory_variable],
    visitor_reward = visitor_reward,
    is_reward_are_boolean = TRUE,
    learn_size = as.integer(nrow(dt) * parameter_size),
    arm_for_learn = names(visitor_reward)[1],
    explanatory_variable =explanatory_variable,
    alpha = 1,
    ctree_control_val = ctree_control()
  )



  ctreeucb_alloc  <- ctreeucbBanditObjectEvaluation(
    dt = dt[ , explanatory_variable],
    visitor_reward = visitor_reward,
    ctree_parameters_control = parameters,
    average = TRUE
  )

  #Evaluation

  choiceListCTREEUCB <-  ctreeucb_alloc$ctreeucb_bandit_alloc$choice
  temp = reward_cumulative(choice=choiceListCTREEUCB,
                    visitor_reward=visitor_reward[ctreeucb_alloc$ctreeucb_bandit_alloc$first_train_element:nrow(visitor_reward),])
  print(max(temp)/length(temp), sep = " " )
  summary(as.factor(choiceListCTREEUCB))

  regret4 <- cumulativeRegret(choiceListCTREEUCB,rewardABtest)
  ##############
  reward4 <- regret4
  for(i in 1:nrow(rewardABtest)){
    reward4[i] = rewardABtest[i,choiceListCTREEUCB[i]]
  }
  reward4 = cumsum(reward4)
  ##############
  max(regret4)
  barplot(prop.table(table(choiceListCTREEUCB)))



  ### Random ###
  unif_alloc <- uniform_bandit_object_evaluation(visitor_reward=visitor_reward[ resVal$dbactreeucb_rejection_sampling_bandit_alloc$first_train_element: nrow(dt)   , ],average = TRUE, IsRewardAreBoolean = FALSE)
  choiceListUNIFORM <- unif_alloc$uniform_bandit_alloc$choice
  temp = reward_cumulative(choice=choiceListUNIFORM,
                           visitor_reward=visitor_reward[ctreeucb_alloc$ctreeucb_bandit_alloc$first_train_element:nrow(visitor_reward),])
  print(max(temp)/length(temp), sep = " " )
  summary(as.factor(choiceListUNIFORM))
  regret5 <- cumulativeRegret(choiceListUNIFORM,rewardABtest)
  ##############
  reward5 <- regret5
  for(i in 1:nrow(rewardABtest)){
    reward5[i] = rewardABtest[i,choiceListUNIFORM[i]]
  }
  reward5 <- cumsum(reward5)
  ##############
  max(regret5)
  barplot(prop.table(table(choiceListUNIFORM)))
  ### END RANDOM ###



  ####Plot all
  library(ggplot2)
  library(reshape2)

  # original data in a 'wide' format
  x  <- 1:length(regret1)
  Regret_DBA_CTREE_UCB <- regret1
  Regret_DBA_LIN_UCB  <- regret2
  Regret_LIN_UCB  <- regret3
  Regret_CTREE_UCB <- regret4
  Regret_UNIFORM <- regret5

  total <- data.frame(x, Regret_DBA_CTREE_UCB , Regret_DBA_LIN_UCB, Regret_LIN_UCB ,Regret_CTREE_UCB, Regret_UNIFORM )

  linetype1 = c('twodash', 'solid', 'longdash','dotted','dashed')

  ggplot(total, aes(c(1:nrow(total)), y = value, color = Algorithm)) +
    geom_line(aes(y =log(Regret_DBA_CTREE_UCB), col = "DBA_CTREE_UCB"),size = 0.5) +
    geom_line(aes(y =log(Regret_DBA_LIN_UCB), col = "DBA_LIN_UCB"),size = 0.5) +
    geom_line(aes(y =log(Regret_LIN_UCB), col = "LIN_UCB"),size = 0.5) +
    geom_line(aes(y =log(Regret_CTREE_UCB) , col = "CTREE_UCB"),size = 0.5) +
    geom_line(aes(y =log(Regret_UNIFORM ), col = "UNIFORM"),size = 0.5) +
    xlab("Time") +
    ylab("Cumulative Regret") +
    scale_linetype_manual(values = linetype1)

  ggplot(total, aes(c(1:nrow(total)), y = value, color = Algorithm)) +
    geom_line(aes(y =Regret_DBA_CTREE_UCB, col = "DBA_CTREE_UCB"),size = 0.5) +
    geom_line(aes(y =Regret_DBA_LIN_UCB, col = "DBA_LIN_UCB"),size = 0.5) +
    geom_line(aes(y =Regret_LIN_UCB, col = "LIN_UCB"),size = 0.5) +
    geom_line(aes(y =Regret_CTREE_UCB , col = "CTREE_UCB"),size = 0.5) +
    geom_line(aes(y =Regret_UNIFORM , col = "UNIFORM"),size = 0.5) +
    xlab("Time") +
    ylab("Cumulative Regret") +
    scale_linetype_manual(values = linetype1)


  #### Reward
  library(ggplot2)
  library(reshape2)

  # original data in a 'wide' format
  x  <- 1:length(reward1)
  Reward_DBA_CTREE_UCB <- reward1
  Reward_DBA_LIN_UCB  <- reward2
  Reward_LIN_UCB  <- reward3
  Reward_CTREE_UCB <- reward4
  Reward_UNIFORM <- reward5

  total <- data.frame(x, Reward_DBA_CTREE_UCB , Reward_DBA_LIN_UCB, Reward_LIN_UCB ,Reward_CTREE_UCB, Reward_UNIFORM )

  linetype1 = c('twodash', 'solid', 'longdash','dotted','dashed')

  ggplot(total, aes(c(1:nrow(total)), y = value, color = Algorithm)) +
    geom_line(aes(y =Reward_DBA_CTREE_UCB, col = "DBA_CTREE_UCB"),size = 0.5) +
    geom_line(aes(y =Reward_DBA_LIN_UCB, col = "DBA_LIN_UCB"),size = 0.5) +
    geom_line(aes(y =Reward_LIN_UCB, col = "LIN_UCB"),size = 0.5) +
    geom_line(aes(y =Reward_CTREE_UCB , col = "CTREE_UCB"),size = 0.5) +
    geom_line(aes(y =Reward_UNIFORM , col = "UNIFORM"),size = 0.5) +
    xlab("Time") +
    ylab("Cumulative Reward") +
    scale_linetype_manual(values = linetype1)

