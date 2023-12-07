library(devtools)
install_github("https://github.com/manuclaeys/banditWithR")
install_github("https://github.com/cran/listdtr")
library(banditwithR)
library(partykit)
size.tot = 9000
set.seed(4649)                          # this makes the example exactly reproducible
x1 = runif(size.tot, min=0, max=10)          # you have 4, largely uncorrelated predictors
x2 = runif(size.tot, min=0, max=10)


K1 = rep(0,size.tot)
K2 = rep(0,size.tot)


# Time series
#premier type : petite série et peu de visites
#taille de la série
x <- 1:10
#fréquence
f <- x/5
y1 <-  jitter( cos(2*pi*f*x) , factor = 1)
plot(x, y1, type='l', col='darkblue')


x <- 1:20
f <- x/3
y2 <- jitter(  5*cos(2*pi*f*x) , factor = 1)
plot(x, y2, type='l', col='darkblue')


x <- 1:40
f <- x/3
y3 <- jitter(  10*cos(2*pi*f*x) , factor = 1)
plot(x, y3, type='l', col='darkblue')


y <- as.data.frame(c(1))
colnames(y) = "ID"


temp=1
for (i in 1:size.tot){

    if(i%%3 == 0){
      y[temp, "time_series"][[1]] <- list(unlist(y1)  +   rnorm(10, mean = 0, sd = 2))
      y[temp, "cluster"][[1]] <- 1
    }

    if(i%%3 == 1){
    y[temp, "time_series"][[1]] <-  list(unlist(y2)  +   rnorm(20, mean = 0, sd = 2))
    y[temp, "cluster"][[1]] <- 2
  }

    if(i%%3 == 2){
    y[temp, "time_series"][[1]] <- list(unlist(y3)  +   rnorm(40, mean = 0, sd = 3))
    y[temp, "cluster"][[1]] <- 3
  }

    y$ID[temp] = temp
    temp = temp +1

}

dt <-  as.data.frame(cbind(x1,x2,y$time_series))
colnames(dt) <- c("x1","x2","time_series")


K1 = rep(0,size.tot)
K2 = rep(0,size.tot)
K3 = rep(0,size.tot)
visitor_reward <-  data.frame(K1,K2,K3)


for(i in 1:nrow(dt)) {
  if(y$cluster[i] == 1){
    visitor_reward$K1[i] = 10 +   rnorm(1, mean = 0, sd = 1)
    visitor_reward$K2[i] = 5  +   rnorm(1, mean = 0, sd = 1)
    visitor_reward$K3[i] = 0  +   rnorm(1, mean = 0, sd = 1)
  }
  if(y$cluster[i] == 2){
    visitor_reward$K1[i] = 5 +   rnorm(1, mean = 0, sd = 1)
    visitor_reward$K2[i] = 0 +   rnorm(1, mean = 0, sd = 1)
    visitor_reward$K3[i] = 10 +   rnorm(1, mean = 0, sd = 1)
  }
  if(y$cluster[i] == 3){
    visitor_reward$K1[i] = 0 +   rnorm(1, mean = 0, sd = 1)
    visitor_reward$K2[i] = 10 +   rnorm(1, mean = 0, sd = 1)
    visitor_reward$K3[i] = 5 +   rnorm(1, mean = 0, sd = 1)
  }
}


plot(density(visitor_reward$K1))
lines(density(visitor_reward$K2))
lines(density(visitor_reward$K3))




dt$x1 <- as.numeric(dt$x1)
dt$x2 <- as.numeric(dt$x2)

K=ncol(visitor_reward)

#Parametrage
ctree_parameters_control=ctreeucb_parameters_control_default(dt,visitor_reward, alpha = 5,learn_size = as.integer(nrow(dt) * 0.1))
listSerie = c("time_series")
listKCentroids=c(3)
resVal <- dbactreeucbRejectionSamplingBanditObjectEvaluation(dt,visitor_reward,K, listSerie, listKCentroids , ctree_parameters_control)

#Evaluation
choiceList <- resVal$dbactreeucb_rejection_sampling_bandit_alloc$choice
summary(as.factor(choiceList))
rewardABtest <- visitor_reward[ resVal$dbactreeucb_rejection_sampling_bandit_alloc$first_train_element : nrow(dt)   , ]

regret1 <- cumulativeRegret(choiceList ,rewardABtest)


barplot(prop.table(table(choiceList)))

###### Comparatif avec DBALinUCB
dbalinucb_rejection_sampling_alloc  <- DBALINUCB_rejection_sampling(dt,
                                                                    visitor_reward,
                                                                    alpha=5, K=ncol(visitor_reward),
                                                                    listSerie, listKCentroids ,
                                                                    learn_size = as.integer(nrow(dt)*0.1),
                                                                    IsRewardAreBoolean = FALSE,
                                                                    listCategorial=0 , listInteger=c("x1","x2"))

#Evaluation
choiceListDBALINUCB <-  dbalinucb_rejection_sampling_alloc$choice
summary(as.factor(choiceListDBALINUCB))

regret2 <- cumulativeRegret(choiceListDBALINUCB,rewardABtest)

barplot(prop.table(table(choiceListDBALINUCB)))


###### Comparatif avec LinUCB
dt$meanTS <- sapply(dt$time_series,mean)

linucb_alloc  <- LinucbBanditObjectEvaluation(
     dt[ resVal$dbactreeucb_rejection_sampling_bandit_alloc$first_train_element : nrow(dt)   , c("x1","x2","meanTS")],
     visitor_reward[ resVal$dbactreeucb_rejection_sampling_bandit_alloc$first_train_element : nrow(dt)   , ],
     alpha = 5,
      K = ncol(visitor_reward),
     average = FALSE,
      IsRewardAreBoolean = FALSE,
     explanatory_variable = c("x1","x2","meanTS")
)

#Evaluation
choiceListLINUCB <-  linucb_alloc$linucb_bandit_alloc$choice
summary(as.factor(choiceListLINUCB))

regret3 <- cumulativeRegret(choiceListLINUCB,rewardABtest)

barplot(prop.table(table(choiceListLINUCB)))


#### CTREE UCB
library(partykit)
parameters = ctreeucb_parameters_control_default(
  dt = dt[ , c("x1","x2","meanTS")],
  visitor_reward = visitor_reward,
  is_reward_are_boolean = FALSE,
  learn_size = as.integer(nrow(dt) * 0.1),
  arm_for_learn = names(visitor_reward)[1],
  explanatory_variable =c("x1","x2","meanTS"),
  alpha = 1,
  ctree_control_val = ctree_control()
)



ctreeucb_alloc  <- ctreeucbBanditObjectEvaluation(
  dt = dt[ , c("x1","x2","meanTS")],
  visitor_reward = visitor_reward,
  ctree_parameters_control = parameters
)

#Evaluation
choiceListCTREEUCB <-  ctreeucb_alloc$ctreeucb_bandit_alloc$choice
summary(as.factor(choiceListCTREEUCB))

regret4 <- cumulativeRegret(choiceListCTREEUCB,rewardABtest)

barplot(prop.table(table(choiceListCTREEUCB)))



### Random ###
unif_alloc <- uniform_bandit_object_evaluation(visitor_reward=visitor_reward[ resVal$dbactreeucb_rejection_sampling_bandit_alloc$first_train_element : nrow(dt)   , ],average = TRUE, IsRewardAreBoolean = FALSE)
choiceListUNIFORM <- unif_alloc$uniform_bandit_alloc$choice
summary(as.factor(choiceListUNIFORM))
regret5 <- cumulativeRegret(choiceListUNIFORM,rewardABtest)
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



ggplot(total, aes(c(1:nrow(total)), y = value, color = Algorithm)) +
  geom_line(aes(y =log(Regret_DBA_CTREE_UCB), col = "DBA_CTREE_UCB"),size = 0.5) +
  geom_line(aes(y =log(Regret_DBA_LIN_UCB), col = "DBA_LIN_UCB"),size = 0.5) +
  geom_line(aes(y =log(Regret_LIN_UCB), col = "LIN_UCB"),size = 0.5) +
  geom_line(aes(y =log(Regret_CTREE_UCB) , col = "CTREE_UCB"),size = 0.5) +
  geom_line(aes(y =log(Regret_UNIFORM), col = "UNIFORM"),size = 0.5) +
  xlab("Time") +
  ylab("Cumulative Regret") +
  scale_linetype_manual(values = linetype1)

