library(stringr)

# Getting the path of your current open file
current_path = rstudioapi::getActiveDocumentContext()$path
setwd(dirname(current_path ))
print( getwd() )

#Importe data
library(jsonlite)
library(readr)




data.train  <- jsonlite::fromJSON(paste(str_sub( getwd() , end = -10),"data/chevilleDroite.JSON",sep=""), simplifyDataFrame = TRUE)


set.seed(123)

data.train  = data.train[sample(nrow(data.train)),]


install_github("https://github.com/manuclaeys/banditWithR")
library(banditwithR)
require(tidyr)
require(dplyr)

visitorReward <- as.data.frame(transform_categorial_to_binary( listCategorial = c("etat"), dt=data.train))
for(i in 1:ncol(visitorReward)) visitorReward[,i] <- as.integer(visitorReward[,i])

dt = data.train[,c("V5","V6","V7")]

#subset
size = 3000
visitorReward=visitorReward[1:size,]
dt = dt[1:size,]
rm(list=ls()[! ls() %in% c("dt","visitorReward")])

#on séléctionne uniquement deux colonnes à la reward identique
visitorReward = visitorReward[,c(5,7)]

K=ncol(visitorReward)
alpha = 1
config = 0.3



#config = 0.3
#config = 1
if(config == 1){
  dt = rbind(dt,dt)
  visitorReward = rbind(visitorReward,visitorReward)
  config = 0.5
}


#Parametrage
ctree_parameters_control=ctreeucb_parameters_control_default(dt,visitorReward,
                                                             is_reward_are_boolean = TRUE,
                                                             alpha = alpha,
                                                             arm_for_learn = names(visitorReward)[1],
                                                             learn_size = as.integer(nrow(dt) * config))
listSerie = c("V5","V6","V7")
#listKCentroids=c(5,5,5)
listKCentroids=c(10,10,10)

resVal_dbactreeucb <- dbactreeucbRejectionSamplingBanditObjectEvaluation(dt,visitorReward,K, listSerie, listKCentroids , ctree_parameters_control)


#Evaluation
choiceList <- resVal_dbactreeucb$dbactreeucb_rejection_sampling_bandit_alloc$choice
summary(as.factor(choiceList))
rewardABtest <- visitorReward[ resVal_dbactreeucb$dbactreeucb_rejection_sampling_bandit_alloc$first_train_element : nrow(dt)   , ]

regret1 <- cumulativeRegret(choiceList ,rewardABtest)
reward1 <- resVal_dbactreeucb$cum_rew_dbactreeucb_rejection_sampling_alloc

barplot(prop.table(table(choiceList)))

###### Comparatif avec DBALinUCB
dbalinucb_rejection_sampling_alloc  <- DBALINUCB_rejection_sampling(dt,
                                                                    visitorReward,
                                                                    alpha=alpha, K=ncol(visitorReward),
                                                                    listSerie, listKCentroids ,
                                                                    learn_size = as.integer(nrow(dt)*config),
                                                                    IsRewardAreBoolean = TRUE,
                                                                    listCategorial=0 , listInteger=0)

#Evaluation
choiceListDBALINUCB <-  dbalinucb_rejection_sampling_alloc$choice
summary(as.factor(choiceListDBALINUCB))

regret2 <- cumulativeRegret(choiceListDBALINUCB,rewardABtest)
reward2 <- cumsum(rewardABtest[,dbalinucb_rejection_sampling_alloc$choice])

barplot(prop.table(table(choiceListDBALINUCB)))


###### Comparatif avec LinUCB
dt$meanV5 <- sapply(dt$V5,mean)
dt$meanV6 <- sapply(dt$V6,mean)
dt$meanV7 <- sapply(dt$V7,mean)

library(matlib)
linucb_alloc  <- LinucbBanditObjectEvaluation(
  dt[ resVal_dbactreeucb$dbactreeucb_rejection_sampling_bandit_alloc$first_train_element : nrow(dt)   , c("meanV5","meanV6","meanV6")],
  visitorReward[ resVal_dbactreeucb$dbactreeucb_rejection_sampling_bandit_alloc$first_train_element : nrow(dt)   , ],
  alpha = 1,
  K = ncol(visitorReward),
  average = FALSE,
  IsRewardAreBoolean = TRUE,
  explanatory_variable = c("meanV5","meanV6","meanV6")
)

#Evaluation
choiceListLINUCB <-  linucb_alloc$linucb_bandit_alloc$choice
summary(as.factor(choiceListLINUCB))

regret3 <- cumulativeRegret(choiceListLINUCB,rewardABtest)
reward3 <- linucb_alloc$cum_reg_linucb_bandit_alloc


barplot(prop.table(table(choiceListLINUCB)))


#### CTREE UCB
library(partykit)
parameters = ctreeucb_parameters_control_default(
  dt = dt[ , c("meanV5","meanV6","meanV6")],
  visitor_reward = visitorReward,
  is_reward_are_boolean = TRUE,
  learn_size = as.integer(nrow(dt) * config),
  arm_for_learn = names(visitorReward)[1],
  explanatory_variable =c("meanV5","meanV6","meanV6"),
  alpha = 1,
  ctree_control_val = ctree_control()
)



ctreeucb_alloc  <- ctreeucbBanditObjectEvaluation(
  dt = dt[ , c("meanV5","meanV6","meanV6")],
  visitor_reward = visitorReward,
  ctree_parameters_control = parameters
)

#Evaluation
choiceListCTREEUCB <-  ctreeucb_alloc$ctreeucb_bandit_alloc$choice
summary(as.factor(choiceListCTREEUCB))

regret4 <- cumulativeRegret(choiceListCTREEUCB,rewardABtest)
reward4 <- regret4
for(i in 1:nrow(rewardABtest)){
  reward4[i] = rewardABtest[i,choiceListCTREEUCB[i]]
}
reward4 = cumsum(reward4)
barplot(prop.table(table(choiceListCTREEUCB)))



### Random ###
unif_alloc <- uniform_bandit_object_evaluation(visitor_reward=visitorReward[ resVal_dbactreeucb$dbactreeucb_rejection_sampling_bandit_alloc$first_train_element : nrow(dt)   , ],average = TRUE, IsRewardAreBoolean = FALSE)
choiceListUNIFORM <- unif_alloc$uniform_bandit_alloc$choice
summary(as.factor(choiceListUNIFORM))
regret5 <- cumulativeRegret(choiceListUNIFORM,rewardABtest)
##############
reward5 <- regret5
for(i in 1:nrow(rewardABtest)){
  reward5[i] = rewardABtest[i,choiceListUNIFORM[i]]
}
reward5 <- cumsum(reward5)
##############
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
  geom_line(aes(y =Regret_DBA_CTREE_UCB, col = "DBA_CTREE_UCB"),size = 0.5) +
  geom_line(aes(y =Regret_DBA_LIN_UCB, col = "DBA_LIN_UCB"),size = 0.5) +
  geom_line(aes(y =Regret_LIN_UCB, col = "LIN_UCB"),size = 0.5) +
  geom_line(aes(y =Regret_CTREE_UCB , col = "CTREE_UCB"),size = 0.5) +
  geom_line(aes(y =Regret_UNIFORM , col = "UNIFORM"),size = 0.5) +
  xlab("Time") +
  ylab("Cumulative Regret") +
  scale_linetype_manual(values = linetype1)

