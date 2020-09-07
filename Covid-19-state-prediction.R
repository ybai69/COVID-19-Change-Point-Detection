rm(list=ls(all=TRUE))
gc()

######## Loading Packages #######################
library("NbClust")
library("factoextra")
library("Rcpp")
library("RcppArmadillo")
library("RColorBrewer")
library("usmap")
library("maps")
library("xtable")
library("vars")
library("lattice")
library("zoo")

######## Call Functions #########################
source("functions_BFL.R")
sourceCpp("functions_BFL.cpp")

######## Loading Datasets #######################
######## State-level ###########################
## data extracted from New York Times state-level data obtained from following Github repository
# https://github.com/nytimes/covid-19-data
# load nyt case data
data.states <- read.csv("states-08-18.csv", header = TRUE)
data.states$date <-  as.Date(data.states$date)

# population data extracted from NATIONAL BUREAU OF ECONOMIC RESEARCH
states.population <- read.csv("co-est2019-alldata.csv", header = TRUE)
states.population <- states.population[as.character(states.population$STNAME) == as.character(states.population$CTYNAME),
                                       c("STNAME", "CTYNAME", "POPESTIMATE2019")]
# distance data extracted from US Census Bureau 
states.distance <-read.csv("sf12010statedistancemiles.csv", header = TRUE)

#########Change/choose the state name, lockdown date and reopen date below!!!!!!
state.name <- "New York"
Date.1 <- '2020-03-22'
Date.2 <- '2020-05-15'

state.name <- "Oregon"
Date.1 <- '2020-03-23'
Date.2 <- '2020-05-15'

state.name <- "Florida"
Date.1 <- '2020-04-03'
# phase 1 reopen date
# Date.2 <- '2020-05-04'
# phase 2 reopen date
Date.2 <- '2020-06-05'

state.name <- "California"
Date.1 <- '2020-03-19'
Date.2 <- '2020-05-08'

state.name <- "Texas"
Date.1 <- '2020-04-02'
# phase 1 reopen date
# Date.2 <- '2020-04-30'
# phase 3 reopen date
Date.2 <- '2020-06-03'

#############################################
print("first updated date:"); print(min(data.states$date))
print("last updated date:"); print(max(data.states$date))

lambda.1 <- c(1, 0.5, 0.1, 0.05, 0.01, 0.005, 0.001)

# test dataset : two weeks
n.test <- 14
#################################################
# max distance 
miles.distance <- 500
# max number of neighboring regions
max.neighbor <- 5
region.fips <- as.numeric(fips(state = state.name))
states.distance[states.distance$state1 == region.fips, ][1:max.neighbor,]
neighbor.fips <- states.distance[(states.distance$state1 == region.fips) & (states.distance$mi_to_state < miles.distance)  , "state2"]
if(length(neighbor.fips) > max.neighbor){
  neighbor.fips <-neighbor.fips[1:max.neighbor]
}
fips_info(neighbor.fips)
state.names <- fips_info(neighbor.fips)$full
state.names <- c(state.name, state.names)
state.lowernames <- tolower(state.names)

#################construct the dataframe including multiple regions
dataList <- list()
for( i in c(1:length(state.names))){
  data.subset <- data.states[data.states$state==state.names[i], c("date", "cases", "deaths")]
  assign(paste("data", state.lowernames[i] , sep="."), data.subset)
  dataList <-append(dataList, list(data.subset))
}

multi_full <- Reduce(
  function(x, y){merge(x, y, all = TRUE, by ="date")},
  dataList
)
for(i in 1:length(state.names)){
  names(multi_full)[2*i] <- paste0('cases.', state.lowernames[i])
  names(multi_full)[2*i+1] <- paste0('deaths.', state.lowernames[i])
}
#replace the na value with 0
n.domains <- length(state.names)
for(i in 1:(n.domains*2) ){
  multi_full[is.na(multi_full[,i+1]),i+1]=0
}
# multi_full$date <- as.Date(droplevels(multi_full$date))
multi_full$date <- as.Date(multi_full$date)
#choose the first day when the region has one confrimed case as the start date 
multi_full <- multi_full[multi_full[,2]>0,]

##################################################
cases.all <- multi_full[,seq(2, ncol(multi_full), 2)]
deaths.all<- multi_full[,seq(3, ncol(multi_full), 2)]

#T: number of time points
T <- nrow(cases.all);
#R: the number of people who have recovered
#using the nationwide recovered and death number to predict the recovered number
R.all <- floor((1 + (5.5))*deaths.all);
#I: the number of people infected at time t
I.all <- cases.all - R.all;
#n: population 
n.all <- rep(0,length(state.names) )
for(i in 1:length(state.names)){
  n.all[i] <- unique(states.population[ 
    (states.population$CTYNAME  == state.names[i]) & 
      states.population$STNAME== state.names[i] , "POPESTIMATE2019"])
}

#S: the number of susceptible people
n.all.matrix <- matrix(rep(n.all,T),ncol = n.domains,byrow = TRUE)
S.all <- n.all.matrix - I.all - R.all;

#the fraction of S, I and R
S.rate.all <- sapply(1:n.domains, function(jjj) S.all[,jjj]/n.all[jjj])
I.rate.all <- sapply(1:n.domains, function(jjj) I.all[,jjj]/n.all[jjj])
R.rate.all <- sapply(1:n.domains, function(jjj) R.all[,jjj]/n.all[jjj])


I <- I.all[,1]
R <- R.all[,1]

############################################
######## construct varibles ################
############################################
y.list <- vector("list",T-1);
x.list <- vector("list",T-1);

for(i in 2:T){
  y.list[[i-1]] <- matrix(c(R.rate.all[i,1]-R.rate.all[i-1, 1], I.rate.all[i, 1]-I.rate.all[i-1,1]), 2, 1);
  x.temp <- matrix(0,2,2);
  x.temp[1,2] <- I.rate.all[i-1, 1];
  x.temp[2,1] <- S.rate.all[i-1, 1]*I.rate.all[i-1, 1];
  x.temp[2,2] <- -I.rate.all[i-1, 1];
  x.list[[i-1]] <- x.temp;
}

Y <- y.list[[1]];
for(i in 2:(T-1)){
  Y <- rbind(Y, y.list[[i]])
}

X <- x.list[[1]];
for(i in 2:(T-1)){
  X <- rbind(X, x.list[[i]])
}

beta_t <- sapply(1:length(y.list), function(jjj)  (y.list[[jjj]][1]+y.list[[jjj]][2])/I.rate.all[jjj,1])
gamma_t <- sapply(1:length(y.list), function(jjj)  y.list[[jjj]][1]/I.rate.all[jjj,1])


Y.full <- Y
X.full <- X


Y.test <- as.matrix(Y.full[(nrow(Y) - (n.test - 1)*2 + 1):nrow(Y), ])
X.test <- X.full[(nrow(X) - (n.test - 1)*2 + 1):nrow(X), ]
Y.train <- as.matrix(Y.full[1 : (nrow(Y) - (n.test - 1)*2 ), ])
X.train <- X.full[1 : (nrow(X) - (n.test - 1)*2 ), ]

I.test <- I[(T - n.test + 1):T]
R.test <- R[(T - n.test + 1):T]

Delta.R <- rep(0, n.test - 1)
for(i in 2:n.test ){
  Delta.R[i - 1] <- Y.test[(i - 2)*2 + 1]
}
Delta.R <- Delta.R*n.all[1]


Delta.I <- rep(0, n.test - 1)
for(i in 2:n.test){
  Delta.I[i - 1] <- Y.test[(i - 2)*2 + 2]
}
Delta.I <- Delta.I*n.all[1]


#################################################
######### Model 1  ##############################
#################################################
#scaling but not centering data
Y_std.train <- sd(Y.train)
Y_s.train <- scale(Y.train, center = FALSE, scale = apply(Y.train, 2, sd, na.rm = TRUE))

X_std.train <- apply(X.train, MARGIN=2, FUN=sd)
X_s.train <- scale(X.train, center = FALSE, scale = apply(X.train, 2, sd, na.rm = TRUE))


p.x <- ncol(X_s.train)
p.y <- ncol(Y_s.train)
n <- nrow(X_s.train)
tol <- 10^(-4); # tolerance 
max.iteration <- 200; # max number of iteration for the LASSO solution
method <- c("MLR")
#p is the number of variables for each day (I_t and R_t) 
p <- 2
#b_t is the block size among the time points (1 : (length(date) - 1))
# b_t <- 7

# temp <- tbfl(method, Y_s, X_s, lambda.2.cv = 0, max.iteration = max.iteration, tol = tol, block.size = b_n)
if(state.name %in% c("Texas")){
  b_t <- 5
  gamma.val <- 10
  HBIC = TRUE
  
}else if(state.name %in% c("Florida")){
  b_t <- 7
  gamma.val <- 5
  HBIC = TRUE
  
}else if(state.name %in% c("California")){
  b_t <- 7
  gamma.val <- 5
  HBIC = TRUE
  
}else{
  b_t <- 7
  gamma.val <- 10
  HBIC = TRUE
}


b_n <- p * b_t
temp.1 <- tbfl(method, Y_s.train, X_s.train, lambda.1.cv = lambda.1, lambda.2.cv = 0,
             max.iteration = max.iteration, tol = tol, block.size = b_n, HBIC = HBIC, gamma.val = gamma.val)

beta.est <- temp.1$beta.est
beta.est <- lapply(beta.est, function (x) x*Y_std.train/X_std.train)

date.region <- multi_full$date
date.region[floor( ( temp.1$cp.final -1) / p) + 1]


#############################################
##### refit model
#############################################
cp <- c(1, temp.1$cp.final, n+1)
m <- length(cp) - 1
X.new.new <- matrix(0, nrow = n, ncol = m*p.x)
for(i in 1:m){
  X.new.new[cp[i]: (cp[i+1]-1), (p.x*(i-1)+1) : (p.x*i) ] <- X.train[cp[i]: (cp[i+1]-1), ]
}

est.1 <- lm(Y.train ~ X.new.new  - 1)
summary(est.1)


cp <- temp.1$cp.final
m <- length(cp)
Y.temp.1 <- Y.train[cp[m]: (n), ]
X.temp.1 <- X.train[cp[m]: (n), ]
est.temp.1 <- lm(Y.temp.1 ~ X.temp.1  - 1)
est.temp.1
Y.hat.1.new <- X.test%*%c(est.temp.1$coefficients) 
Y.hat.1.train <- est.1$fitted.values

Delta.R.hat.1 <- rep(0, n.test - 1)
for(i in 2:n.test ){
  Delta.R.hat.1[i - 1] <- Y.hat.1.new[(i - 2)*2 + 1]
}
Delta.R.hat.1 <- Delta.R.hat.1*n.all[1]

Delta.I.hat.1 <- rep(0, n.test - 1)
for(i in 2:n.test){
  Delta.I.hat.1[i - 1] <- Y.hat.1.new[(i - 2)*2 + 2]
}
Delta.I.hat.1 <- Delta.I.hat.1*n.all[1]


R.hat.1.new <- rep(0, n.test)
R.hat.1.new[1] <- R.rate.all[T - n.test + 1, 1]
for(i in 2:n.test){
  R.hat.1.new[i] <- R.rate.all[T - n.test + (i-1), 1] + Y.hat.1.new[(i-2)*2+1]
}
R.hat.1.new <- R.hat.1.new*n.all[1]

I.hat.1.new <- rep(0, n.test)
I.hat.1.new[1] <- I.rate.all[T - n.test + 1, 1]
for(i in 2:n.test){
  I.hat.1.new[i] <-  I.rate.all[T - n.test + (i-1), 1] + Y.hat.1.new[(i-2)*2+2]
}
I.hat.1.new <- I.hat.1.new*n.all[1]

I.test <- I[(T - n.test + 1):T]
R.test <- R[(T - n.test + 1):T]


MRPE_1_new_I <- mean(  abs ( (     c(I.hat.1.new[-1]) - c(I.test[-1])     )  /c(I.test[-1])  )[c(I.test[-1]) > 0]  )
print(round(MRPE_1_new_I, 4))
MRPE_1_new_R <- mean(  abs ( (     c(R.hat.1.new[-1]) - c(R.test[-1])     )  /c(R.test[-1])  )[c(R.test[-1]) > 0]  )
print(round(MRPE_1_new_R, 4))


#################################################
######### Model 2  ##############################
#################################################
#################################################
###### Model 2.1 equal weight 
###### spatial components are chosen by distance
#################################################
distance_1 <- rep(1, n.domains)
distance_1[1] <- 0

Omega_1 <- distance_1 / sum(distance_1)

rows.combined <- 2*(T-1)
cols.combined <- n.domains
matrix.combined <- matrix(0, nrow = rows.combined, ncol = cols.combined)
matrix.combined[ seq(1, rows.combined, 2),] <- as.matrix(unname(R.rate.all))[-1,]-as.matrix(unname(R.rate.all))[-T,]
matrix.combined[ seq(2, rows.combined, 2),] <- as.matrix(unname(I.rate.all))[-1,]-as.matrix(unname(I.rate.all))[-T,]
neighbor.matrix <- matrix.combined
neighbor.weighted_1 <- neighbor.matrix %*% (Omega_1)

#remove the last two elements  at time point t=T
#and add two elements for time point t= 0
neighbor.weighted_1 <- as.matrix( neighbor.weighted_1 [-c(rows.combined-1, rows.combined),])
neighbor.weighted_1 <- as.matrix( c(neighbor.weighted_1[c(1,2),], neighbor.weighted_1) )

neighbor.weighted_1.test <- as.matrix(neighbor.weighted_1[(nrow(neighbor.weighted_1) - (n.test - 1)*2 + 1) : nrow(neighbor.weighted_1), ])
neighbor.weighted_1.train <- as.matrix(neighbor.weighted_1[1 : (nrow(neighbor.weighted_1) - (n.test - 1)*2 ), ])


p.x <- ncol(X.train)
p.y <-ncol(Y.train)
n <- nrow(X.train)

#############################################
##### refit model
#############################################
cp <- c(1, temp.1$cp.final, n+1)
m <- length(cp) - 1
X.new.1 <- matrix(0, nrow = n, ncol = m*p.x + 1)
for(i in 1:m){
  X.new.1[cp[i]: (cp[i+1]-1), (p.x*(i-1)+1) : (p.x*i) ] <- X.train[cp[i]: (cp[i+1]-1), ]
}
X.new.1[, m*p.x + 1] <- neighbor.weighted_1[1:n, ]

est.2.1 <- lm(Y.train[-c(1:2), ] ~ X.new.1[-c(1:2), ]  - 1)
summary(est.2.1 )

beta.t.est <- est.2.1$coefficients[m*p.x - 1]
gamma.t.est <- est.2.1$coefficients[m*p.x ]
alpha.t.est <- est.2.1$coefficients[m*p.x + 1]
Y.hat.2.1.new <- X.test%*%c(beta.t.est, gamma.t.est) + neighbor.weighted_1.test%*%alpha.t.est

R.hat.2.1.new <- rep(0, n.test)
R.hat.2.1.new[1] <- R.rate.all[T - n.test + 1, 1]
for(i in 2:n.test){
  R.hat.2.1.new[i] <- R.rate.all[T - n.test + (i-1), 1] + Y.hat.2.1.new[(i-2)*2+1]
}
R.hat.2.1.new <- R.hat.2.1.new*n.all[1]

I.hat.2.1.new <- rep(0, n.test)
I.hat.2.1.new[1] <- I.rate.all[T - n.test + 1, 1]
for(i in 2:n.test){
  I.hat.2.1.new[i] <-  I.rate.all[T - n.test + (i-1), 1] + Y.hat.2.1.new[(i-2)*2+2]
}
I.hat.2.1.new <- I.hat.2.1.new*n.all[1]

I.test <- I[(T - n.test + 1):T]
R.test <- R[(T - n.test + 1):T]


MRPE_2.1_new_I <- mean(  abs ( (     c(I.hat.2.1.new[-1]) - c(I.test[-1])     )  /c(I.test[-1])  )[c(I.test[-1]) > 0]  )
print(round(MRPE_2.1_new_I, 4))
MRPE_2.1_new_R <- mean(  abs ( (     c(R.hat.2.1.new[-1]) - c(R.test[-1])     )  /c(R.test[-1])  )[c(R.test[-1]) > 0]  )
print(round(MRPE_2.1_new_R, 4))


##################################
#  model 2.2 distance-based weight
##################################
distance_2 <- rep(0, n.domains)
#Power Distance Weights.
for(i in 2:n.domains){
  temp.fips <- as.numeric(fips(state = state.names[i]))
  distance.temp <- states.distance[ (states.distance$state1== region.fips[1] ) &
                                      (states.distance$state2== temp.fips ), "mi_to_state"]
  print(distance.temp)
  distance_2[i] <- 1/(distance.temp)^1
}

Omega_2 <- distance_2/sum(distance_2)

rows.combined <- 2*(T-1)
cols.combined <- n.domains
matrix.combined <- matrix(0, nrow=rows.combined, ncol=cols.combined)
matrix.combined[ seq(1, rows.combined, 2),] <- as.matrix(unname(R.rate.all))[-1,]-as.matrix(unname(R.rate.all))[-T,]
matrix.combined[ seq(2, rows.combined, 2),] <- as.matrix(unname(I.rate.all))[-1,]-as.matrix(unname(I.rate.all))[-T,]
neighbor.matrix <- matrix.combined
neighbor.weighted_2 <- neighbor.matrix %*% (Omega_2)

#remove the last two elements  at time point t=T
#and add two elements for time point t= 0
neighbor.weighted_2 <- as.matrix(neighbor.weighted_2[-c(rows.combined-1, rows.combined),])
neighbor.weighted_2 <- as.matrix(c(neighbor.weighted_2[c(1,2),], neighbor.weighted_2))

neighbor.weighted_2.test <- as.matrix(neighbor.weighted_2[(nrow(neighbor.weighted_2) - (n.test - 1)*2 + 1) : nrow(neighbor.weighted_2), ])
neighbor.weighted_2.train <- as.matrix(neighbor.weighted_2[1 : (nrow(neighbor.weighted_2) - (n.test - 1)*2 ), ])



#############################################
##### refit model
#############################################
cp <- c(1, temp.1$cp.final, n+1)
m <- length(cp) - 1
X.new.2 <- matrix(0, nrow = n, ncol = m*p.x + 1)
for(i in 1:m){
  X.new.2[cp[i]: (cp[i+1]-1), (p.x*(i-1)+1) : (p.x*i) ] <- X.train[cp[i]: (cp[i+1]-1), ]
}
X.new.2[, m*p.x + 1] <- neighbor.weighted_2[1:n, ]


est.2.2 <- lm(Y.train[-c(1:2), ] ~ X.new.2[-c(1:2), ]  - 1)
summary(est.2.2 )

beta.t.est <- est.2.2$coefficients[m*p.x - 1]
gamma.t.est <- est.2.2$coefficients[m*p.x ]
alpha.t.est <- est.2.2$coefficients[m*p.x + 1]
Y.hat.2.2.new <- X.test%*%c(beta.t.est, gamma.t.est) + neighbor.weighted_2.test%*%alpha.t.est


R.hat.2.2.new <- rep(0, n.test)
R.hat.2.2.new[1] <- R.rate.all[T - n.test + 1, 1]
for(i in 2:n.test){
  R.hat.2.2.new[i] <- R.rate.all[T - n.test + (i-1), 1] + Y.hat.2.2.new[(i-2)*2+1]
}
R.hat.2.2.new <- R.hat.2.2.new*n.all[1]

I.hat.2.2.new <- rep(0, n.test)
I.hat.2.2.new[1] <- I.rate.all[T - n.test + 1, 1]
for(i in 2:n.test){
  I.hat.2.2.new[i] <-  I.rate.all[T - n.test + (i-1), 1] + Y.hat.2.2.new[(i-2)*2+2]
}
I.hat.2.2.new <- I.hat.2.2.new*n.all[1]

I.test <- I[(T - n.test + 1):T]
R.test <- R[(T - n.test + 1):T]


MRPE_2.2_new_I <- mean(  abs ( (     c(I.hat.2.2.new[-1]) - c(I.test[-1])     )  /c(I.test[-1])  )[c(I.test[-1]) > 0]  )
print(round(MRPE_2.2_new_I, 4))
MRPE_2.2_new_R <- mean(  abs ( (     c(R.hat.2.2.new[-1]) - c(R.test[-1])     )  /c(R.test[-1])  )[c(R.test[-1]) > 0]  )
print(round(MRPE_2.2_new_R, 4))


#################################################
##########model 2.3    similarity weight ########
#################################################
# get all the states fips in the United States
neighbor.fips <- c()
# region.fips <- fips(state = state.name)
state.fip <- as.numeric(fips(state.name))
for(i in 1: length(state.fips$fips)){
  if(state.fips$fips[i]!= state.fip ){
    neighbor.fips <- c(neighbor.fips, state.fips$fips[i]) 
    
  }
}
neighbor.fips <- unique(neighbor.fips)
fips_info(neighbor.fips)
state.names <- fips_info(neighbor.fips)$full
state.names <- c(state.name, state.names)
state.lowernames <- tolower(state.names)


n.all <- rep(0,length(state.names) )
for(i in 1:length(state.names)){
  n.all[i] <- unique(states.population[ 
    (states.population$CTYNAME  == state.names[i]) & 
      states.population$STNAME== state.names[i] , "POPESTIMATE2019"])
}


state.names.all <- state.names
state.lowernames.all <- state.lowernames


dataList <- list()
for( i in c(1:length(state.names))){
  data.subset <- data.states[data.states$state==state.names[i], c("date", "cases", "deaths")]
  assign(paste("data", state.lowernames[i] , sep="."), data.subset)
  dataList <-append(dataList, list(data.subset))
}

multi_full <- Reduce(
  function(x, y){merge(x, y, all = TRUE, by ="date")},
  dataList
)
for(i in 1:length(state.names)){
  names(multi_full)[2*i] <- paste0('cases.', state.lowernames[i])
  names(multi_full)[2*i+1] <- paste0('deaths.', state.lowernames[i])
}
#replace the na value with 0
n.domains <- length(state.names)
for(i in 1:(n.domains*2) ){
  multi_full[is.na(multi_full[,i+1]),i+1]=0
}
# multi_full$date <- as.Date(droplevels(multi_full$date))
multi_full$date <- as.Date(multi_full$date)
#choose the first day when the region has one confrimed case as the start date 
multi_full <- multi_full[multi_full[,2]>0,]


cases.all <- multi_full[,seq(2, ncol(multi_full), 2)]
deaths.all<- multi_full[,seq(3, ncol(multi_full), 2)]

#T: number of time points
T <- nrow(cases.all);
#R: the number of people who have recovered
#using the nationwide recovered and death number to predict the recovered number
R.all <- floor((1 + (5.5))*deaths.all);
#I: the number of people infected at time t
I.all <- cases.all - R.all;

#S: the number of susceptible people
n.all.matrix <- matrix(rep(n.all,T),ncol = n.domains,byrow = TRUE)
S.all <- n.all.matrix - I.all - R.all;

#the fraction of S, I and R
S.rate.all <- sapply(1:n.domains, function(jjj) S.all[,jjj]/n.all[jjj])
I.rate.all <- sapply(1:n.domains, function(jjj) I.all[,jjj]/n.all[jjj])
R.rate.all <- sapply(1:n.domains, function(jjj) R.all[,jjj]/n.all[jjj])

n.regions <- ncol(I.rate.all)
y.list <- vector("list", T-1);
for(i in 2:T){
  y.list[[i-1]] <- matrix(c(R.rate.all[i, ] - R.rate.all[i-1, ], I.rate.all[i, ] - I.rate.all[i-1, ]), 2, ncol = n.regions, byrow = TRUE);
}

Y.all <- y.list[[1]];
for(i in 2:(T-1)){
  Y.all <- rbind(Y.all, y.list[[i]])
}

# max number of neighboring regions
max.neighbor <- 5 + 1
l2.diff <- c()
for(i in 1:n.regions){
  l2.diff <- c(l2.diff, sqrt(sum( (Y.all[ , i] - Y.all[ , 1] )^2)))
}
plot(l2.diff)
state.index <- order(l2.diff)[1: max.neighbor]
state.names <- state.names.all[state.index]
state.lowernames <- state.lowernames.all[state.index]
state.names
state.lowernames


n.all.full <- n.all
S.rate.all.full <- S.rate.all
I.rate.all.full <- I.rate.all
R.rate.all.full <- R.rate.all

n.domains <- length(state.names)
n.all <- n.all[state.index]
S.rate.all <- S.rate.all[, state.index]
I.rate.all <- I.rate.all[, state.index]
R.rate.all <- R.rate.all[, state.index]

date.region <- multi_full$date
I <- I.all[, 1]
R <- R.all[, 1]

distance_3 <- rep(0, n.domains)
#Power Distance Weights.
for(i in 2:n.domains){
  distance.temp <- sort(l2.diff)[i]
  distance_3[i] <- 1/(distance.temp)^1
}
Omega_3 <- distance_3/sum(distance_3)


rows.combined <- 2*(T-1)
cols.combined <- n.domains
matrix.combined <- matrix(0, nrow=rows.combined, ncol=cols.combined)
matrix.combined[ seq(1, rows.combined, 2),] <- as.matrix(unname(R.rate.all))[-1,]-as.matrix(unname(R.rate.all))[-T,]
matrix.combined[ seq(2, rows.combined, 2),] <- as.matrix(unname(I.rate.all))[-1,]-as.matrix(unname(I.rate.all))[-T,]
neighbor.matrix <- matrix.combined
neighbor.weighted_3 <- neighbor.matrix %*% (Omega_3)

# remove the last two elements  at time point t=T
# and add two elements for time point t= 0
# We use the spatial effect at t= k-1 to estimate/predict the Y at t = k
neighbor.weighted_3 <- as.matrix(neighbor.weighted_3[-c(rows.combined - 1,rows.combined),])
neighbor.weighted_3 <- as.matrix(c(neighbor.weighted_3[c(1,2),], neighbor.weighted_3))


neighbor.weighted_3.test <- as.matrix(neighbor.weighted_3[(nrow(neighbor.weighted_3) - (n.test - 1)*2 + 1) : nrow(neighbor.weighted_3), ])
neighbor.weighted_3.train <- as.matrix(neighbor.weighted_3[1 : (nrow(neighbor.weighted_3) - (n.test - 1)*2 ), ])



#############################################
##### refit model
#############################################
cp <- c(1, temp.1$cp.final, n+1)
m <- length(cp) - 1
X.new.3 <- matrix(0, nrow = n, ncol = m*p.x + 1)
for(i in 1:m){
  X.new.3[cp[i]: (cp[i+1]-1), (p.x*(i-1)+1) : (p.x*i) ] <- X.train[cp[i]: (cp[i+1]-1), ]
}
X.new.3[, m*p.x + 1] <- neighbor.weighted_3[1:n, ]


est.2.3 <- lm(Y.train[-c(1:2), ] ~ X.new.3[-c(1:2), ]  - 1)
#fitted data in tranining set
Y.hat.2.3 <- c(0, 0, est.2.3 $fitted.values)
summary(est.2.3 )

beta.t.est <- est.2.3$coefficients[m*p.x - 1]
gamma.t.est <- est.2.3$coefficients[m*p.x ]
alpha.t.est <- est.2.3$coefficients[m*p.x + 1]
Y.hat.2.3.new <- X.test%*%c(beta.t.est, gamma.t.est) + neighbor.weighted_3.test%*%alpha.t.est


R.hat.2.3.new <- rep(0, n.test)
R.hat.2.3.new[1] <- R.rate.all[T - n.test + 1, 1]
for(i in 2:n.test){
  R.hat.2.3.new[i] <- R.rate.all[T - n.test + (i-1), 1] + Y.hat.2.3.new[(i-2)*2+1]
}
R.hat.2.3.new <- R.hat.2.3.new*n.all[1]

I.hat.2.3.new <- rep(0, n.test)
I.hat.2.3.new[1] <- I.rate.all[T - n.test + 1, 1]
for(i in 2:n.test){
  I.hat.2.3.new[i] <-  I.rate.all[T - n.test + (i-1), 1] + Y.hat.2.3.new[(i-2)*2+2]
}
I.hat.2.3.new <- I.hat.2.3.new*n.all[1]

I.test <- I[(T - n.test + 1):T]
R.test <- R[(T - n.test + 1):T]


MRPE_2.3_new_I <- mean(  abs ( (     c(I.hat.2.3.new[-1]) - c(I.test[-1])     )  /c(I.test[-1])  )[c(I.test[-1]) > 0]  )
print(round(MRPE_2.3_new_I, 4))
MRPE_2.3_new_R <- mean(  abs ( (     c(R.hat.2.3.new[-1]) - c(R.test[-1])     )  /c(R.test[-1])  )[c(R.test[-1]) > 0]  )
print(round(MRPE_2.3_new_R, 4))


## predict value of Delta.R and Delta.I
Delta.R.hat.2 <- rep(0, n.test - 1)
for(i in 2:n.test ){
  Delta.R.hat.2[i - 1] <- Y.hat.2.3.new[(i - 2)*2 + 1]
}
Delta.R.hat.2 <- Delta.R.hat.2*n.all[1]


Delta.I.hat.2 <- rep(0, n.test - 1)
for(i in 2:n.test){
  Delta.I.hat.2[i - 1] <- Y.hat.2.3.new[(i - 2)*2 + 2]
}
Delta.I.hat.2 <- Delta.I.hat.2*n.all[1]


#################################################
######### Model 3  use VAR(p) model##############
#################################################

Delta.R.hat.train <- rep(0, T - n.test)
Delta.R.train <- rep(0, T - n.test)
for(i in 2: (T - n.test + 1) ){
  Delta.R.hat.train[i - 1] <- Y.hat.2.3[(i - 2)*2 + 1]
  Delta.R.train[i - 1] <- Y.train[(i - 2)*2 + 1]
}
Delta.R.hat.train <- Delta.R.hat.train*n.all[1]
Delta.R.train <- Delta.R.train*n.all[1]


Delta.I.hat.train <- rep(0, T - n.test)
Delta.I.train <- rep(0, T - n.test)
for(i in 2:(T - n.test + 1)){
  Delta.I.hat.train[i - 1] <- Y.hat.2.3[(i - 2)*2 + 2]
  Delta.I.train[i - 1] <- Y.train[(i - 2)*2 + 2]
}
Delta.I.hat.train <- Delta.I.hat.train*n.all[1]
Delta.I.train <- Delta.I.train*n.all[1]



residual_Delta.I.train <- Delta.I.train - Delta.I.hat.train 
residual_Delta.R.train <- Delta.R.train - Delta.R.hat.train 

res <- c()
for(i in 1:n){
  res <- c(res,  Y.train[i] - Y.hat.2.3[i])
}
res <- res*n.all[1]


residual.matrix.train <- cbind(residual_Delta.I.train, residual_Delta.R.train)
print("estimated sample variance for hat residual:")
print(var(residual.matrix.train))

var <- VARselect(residual.matrix.train, lag.max = 7, type = "none")

var$selection
# choose the p by BIC
p.est <- var$selection["SC(n)"]

#change point detection
method <- c("VAR")
if(p.est >= b_n/2){
  temp.var <- tbfl(method, residual.matrix.train, lambda.1.cv = NULL, lambda.2.cv = 0, q = p.est, max.iteration = max.iteration, tol = tol, block.size = p.est+1)
  print(p.est)
}else{
  temp.var <- tbfl(method, residual.matrix.train, lambda.1.cv = NULL, lambda.2.cv = 0, q = p.est, max.iteration = max.iteration, tol = tol, block.size = b_n/2)
}
temp.var$cp.final


if(is.null(temp.var$cp.final)){
  print("residual structure no change point!")
  var1 <- VAR(residual.matrix.train, p = p.est, type = "none")
  coef.matrix <- Bcoef(var1)
  #fitted the residual in training set
  residual.hat <- fitted(var1)
  
  #vectorize the fitted residuals
  residual.hat.vec <- rep(0, nrow(residual.hat)*2)
  #fitted residual of Delta R
  residual.hat.vec[seq(1, nrow(residual.hat)*2, 2)] <- residual.hat[, 2]
  #fitted residual of Delta I
  residual.hat.vec[seq(2, nrow(residual.hat)*2, 2)] <- residual.hat[, 1]
  
  Y.hat.3.train <- Y.hat.2.3 + c(rep(0, p.est*2),  residual.hat.vec/n.all[1])
  
  
}else{
  print("residual structure has change point!")
  cp.residual<- c(1, temp.var$cp.final, nrow(residual.matrix.train)+1)
  m <- length(cp.residual) - 1
  residual.hat <- matrix(0, nrow = nrow(residual.matrix.train), ncol = ncol(residual.matrix.train))
  
  for(i in 1:m){
    residual.matrix.train.temp <- residual.matrix.train[ cp.residual[i]: (cp.residual[i+1]-1),  ]
    var.temp <- VARselect(residual.matrix.train.temp, lag.max = 7, type = "none")
    var.temp$selection
    p.est <- var.temp$selection["SC(n)"]
    var1 <- VAR(residual.matrix.train.temp, p = p.est, type = "none")
    residual.hat[cp.residual[i]: (cp.residual[i+1]-1), ] <- rbind( matrix(0, ncol = 2, nrow = p.est), fitted(var1)) 
  }
  coef.matrix <- Bcoef(var1)
  
  #vectorize the fitted residuals
  residual.hat.vec <- rep(0, nrow(residual.hat)*2)
  #fitted residual of Delta R
  residual.hat.vec[seq(1, nrow(residual.hat)*2, 2)] <- residual.hat[, 2]
  #fitted residual of Delta I
  residual.hat.vec[seq(2, nrow(residual.hat)*2, 2)] <- residual.hat[, 1]
  
  Y.hat.3.train <- Y.hat.2.3 +  residual.hat.vec/n.all[1]
  
}


##predict the residual 
residual.matrix.test.hat <- matrix(0, nrow = n.test - 1, ncol = 2)

predict.temp <- predict(var1, n.ahead = n.test - 1)$fcst
residual.matrix.test.hat[, 1] <- predict.temp$residual_Delta.I.train[, 1]
residual.matrix.test.hat[, 2] <- predict.temp$residual_Delta.R.train[, 1]



#vectorize the fitted residuals
residual.test.hat.vec <- rep(0, nrow(residual.matrix.test.hat)*2)
#fitted residual of Delta R
residual.test.hat.vec[seq(1, nrow(residual.matrix.test.hat)*2, 2)] <- residual.matrix.test.hat[, 2]
#fitted residual of Delta I
residual.test.hat.vec[seq(2, nrow(residual.matrix.test.hat)*2, 2)] <- residual.matrix.test.hat[, 1]


Y.hat.3.new <- Y.hat.2.3.new +  residual.test.hat.vec/n.all[1]

R.hat.3.new <- rep(0, n.test)
R.hat.3.new[1] <- R.rate.all[T - n.test + 1, 1]
for(i in 2:n.test){
  R.hat.3.new[i] <- R.rate.all[T - n.test + (i-1), 1] + Y.hat.3.new[(i-2)*2+1]
}
R.hat.3.new <- R.hat.3.new*n.all[1]

I.hat.3.new <- rep(0, n.test)
I.hat.3.new[1] <- I.rate.all[T - n.test + 1, 1]
for(i in 2:n.test){
  I.hat.3.new[i] <-  I.rate.all[T - n.test + (i-1), 1] + Y.hat.3.new[(i-2)*2+2]
}
I.hat.3.new <- I.hat.3.new*n.all[1]

I.test <- I[(T - n.test + 1):T]
R.test <- R[(T - n.test + 1):T]

MRPE_3_new_I <- mean(  abs ( (     c(I.hat.3.new[-1]) - c(I.test[-1])     )  /c(I.test[-1])  )[c(I.test[-1]) > 0]  )
print(round(MRPE_3_new_I, 4))
MRPE_3_new_R <- mean(  abs ( (     c(R.hat.3.new[-1]) - c(R.test[-1])     )  /c(R.test[-1])  )[c(R.test[-1]) > 0]  )
print(round(MRPE_3_new_R, 4))






## predict value of Delta.R and Delta.I
Delta.R.hat.3 <- rep(0, n.test - 1)
for(i in 2:n.test ){
  Delta.R.hat.3[i - 1] <- Y.hat.3.new[(i - 2)*2 + 1]
}
Delta.R.hat.3 <- Delta.R.hat.3*n.all[1]


Delta.I.hat.3 <- rep(0, n.test - 1)
for(i in 2:n.test){
  Delta.I.hat.3[i - 1] <- Y.hat.3.new[(i - 2)*2 + 2]
}
Delta.I.hat.3 <- Delta.I.hat.3*n.all[1]





# filename <- paste0("Delta_I_PI_", state.lowernames[1], "_all.pdf")
# pdf(filename, width = 11, height = 8.5)
par(mar = c(4, 5, 1.5, 1))
ylim_min <- min(0, Delta.I,  Delta.I.hat.1, Delta.I.hat.2, Delta.I.hat.3)
ylim_max <- max(0, Delta.I,  Delta.I.hat.1, Delta.I.hat.2, Delta.I.hat.3)
plot(multi_full$date[(T-n.test+1):(T-1)], Delta.I , col = '1', ylim =c(ylim_min, ylim_max), lty = 1, type = "l", lwd = 3,
     ylab = expression(paste(Delta, "I(t)")), xlab = 'Date', cex.lab = 2, cex.axis = 2)
lines(multi_full$date[(T-n.test+1):(T-1)], Delta.I.hat.1, col = 'orange', lty = 1, type = "b", lwd = 3)
lines(multi_full$date[(T-n.test+1):(T-1)], Delta.I.hat.2, col = 'green', lty = 1, type = "b", lwd = 3)
lines(multi_full$date[(T-n.test+1):(T-1)], Delta.I.hat.3, col = 'blue', lty = 1, type = "b", lwd = 3)
# dev.off()


# filename <- paste0("Delta_R_PI_", state.lowernames[1], "_all.pdf")
# pdf(filename, width=11, height=8.5)
par(mar = c(4., 5, 1.5, 1))
ylim_min <- min(0, Delta.R, Delta.R.hat.1, Delta.R.hat.2, Delta.R.hat.3)
ylim_max <- max(0, Delta.R, Delta.R.hat.1, Delta.R.hat.2, Delta.R.hat.3)
plot(multi_full$date[(T-n.test+1):(T-1)], Delta.R, col='1', ylim = c(ylim_min, ylim_max), lty=1, type="l", lwd = 3,
     ylab = expression(paste(Delta, "R(t)")), xlab= 'Date', cex.lab=2, cex.axis=2)
lines(multi_full$date[(T-n.test+1):(T-1)], Delta.R.hat.1, col='orange', lty=1, type= "b", lwd = 3)
lines(multi_full$date[(T-n.test+1):(T-1)], Delta.R.hat.2, col='green', lty=1, type="b", lwd = 3)
lines(multi_full$date[(T-n.test+1):(T-1)], Delta.R.hat.3, col='blue', lty=1, type="b", lwd = 3)
# dev.off()

#######################################
# model 2.4 use all the states #######
######################################

n.domains <- length(state.names.all)
distance_4 <- rep(0, n.domains)
#Power Distance Weights.
for(i in 2:n.domains){
  distance.temp <- sort(l2.diff)[i]
  distance_4[i] <- 1/(distance.temp)
}


Omega_4 <- distance_4/sum(distance_4)

rows.combined <- 2*(T-1)
cols.combined <- n.domains
matrix.combined <- matrix(0, nrow=rows.combined, ncol=cols.combined)
matrix.combined[ seq(1, rows.combined, 2),] <- as.matrix(unname(R.rate.all.full))[-1, ] - as.matrix(unname(R.rate.all.full))[-T, ]
matrix.combined[ seq(2, rows.combined, 2),] <- as.matrix(unname(I.rate.all.full))[-1, ] - as.matrix(unname(I.rate.all.full))[-T, ]
neighbor.matrix <- matrix.combined
neighbor.weighted_4 <- neighbor.matrix %*% (Omega_4)

#remove the last two elements  at time point t=T
#and add two elements for time point t= 0
neighbor.weighted_4 <- as.matrix(neighbor.weighted_4[-c(rows.combined-1,rows.combined),])
neighbor.weighted_4 <- as.matrix(c(neighbor.weighted_4[c(1,2),], neighbor.weighted_4))

neighbor.weighted_4.test <- as.matrix(neighbor.weighted_4[(nrow(neighbor.weighted_4) - (n.test - 1)*2 + 1) : nrow(neighbor.weighted_4), ])
neighbor.weighted_4.train <- as.matrix(neighbor.weighted_4[1 : (nrow(neighbor.weighted_4) - (n.test - 1)*2 ), ])

#############################################
##### refit model
#############################################
cp <- c(1, temp.1$cp.final, n+1)
m <- length(cp) - 1
X.new.4 <- matrix(0, nrow = n, ncol = m*p.x + 1)
for(i in 1:m){
  X.new.4[cp[i]: (cp[i+1]-1), (p.x*(i-1)+1) : (p.x*i) ] <- X.train[cp[i]: (cp[i+1]-1), ]
}
X.new.4[, m*p.x + 1] <- neighbor.weighted_4[1:n, ]


est.2.4 <- lm(Y.train[-c(1:2), ] ~ X.new.4[-c(1:2), ]  - 1)
summary(est.2.4 )

beta.t.est <- est.2.4$coefficients[m*p.x - 1]
gamma.t.est <- est.2.4$coefficients[m*p.x ]
alpha.t.est <- est.2.4$coefficients[m*p.x + 1]
Y.hat.2.4.new <- X.test%*%c(beta.t.est, gamma.t.est) + neighbor.weighted_4.test%*%alpha.t.est


R.hat.2.4.new <- rep(0, n.test)
R.hat.2.4.new[1] <- R.rate.all[T - n.test + 1, 1]
for(i in 2:n.test){
  R.hat.2.4.new[i] <- R.rate.all[T - n.test + (i-1), 1] + Y.hat.2.4.new[(i-2)*2+1]
}
R.hat.2.4.new <- R.hat.2.4.new*n.all[1]

I.hat.2.4.new <- rep(0, n.test)
I.hat.2.4.new[1] <- I.rate.all[T - n.test + 1, 1]
for(i in 2:n.test){
  I.hat.2.4.new[i] <-  I.rate.all[T - n.test + (i-1), 1] + Y.hat.2.4.new[(i-2)*2+2]
}
I.hat.2.4.new <- I.hat.2.4.new*n.all[1]

I.test <- I[(T - n.test + 1):T]
R.test <- R[(T - n.test + 1):T]


MRPE_2.4_new_I <- mean(  abs ( (     c(I.hat.2.4.new[-1]) - c(I.test[-1])     )  /c(I.test[-1])  )[c(I.test[-1]) > 0]  )
print(round(MRPE_2.4_new_I, 4))
MRPE_2.4_new_R <- mean(  abs ( (     c(R.hat.2.4.new[-1]) - c(R.test[-1])     )  /c(R.test[-1])  )[c(R.test[-1]) > 0]  )
print(round(MRPE_2.4_new_R, 4))





##########################################
filename <- paste0(state.lowernames[1], "_domain_prediction.RData")
save.image(filename)





MRPE_1_new_I
MRPE_1_new_R

MRPE_2.1_new_I
MRPE_2.1_new_R

MRPE_2.2_new_I
MRPE_2.2_new_R

MRPE_2.3_new_I
MRPE_2.3_new_R

MRPE_2.4_new_I
MRPE_2.4_new_R

MRPE_3_new_I
MRPE_3_new_R


library("xtable")
# statesnames <- c("new york", "washington", "florida", "california", "texas")
# statesnames <- c("iowa", "colorado", "oregon", "alabama", "connecticut")
statesnames <- c("new york", "oregon", "florida", "california", "texas")
MPE_res_I<- c()
MPE_res_R <- c()
for(i in 1:length(statesnames)){
  filename <- paste0(statesnames[i],"_domain_prediction.RData")
  load(filename)
  MPE_res_I <- cbind(MPE_res_I,  round(c(MRPE_1_new_I, MRPE_2.1_new_I, MRPE_2.2_new_I, MRPE_2.3_new_I, MRPE_2.4_new_I, MRPE_3_new_I ),4))
  MPE_res_R <- cbind(MPE_res_R,  round(c(MRPE_1_new_R, MRPE_2.1_new_R, MRPE_2.2_new_R, MRPE_2.3_new_R, MRPE_2.4_new_R, MRPE_3_new_R),4))
}

MPE_res_I <- cbind(c("Model 1", "Model 2.1 ", "Model 2.2 ", "Model 2.3 " ,"Model 2.4 "  ,"Model 3 "), MPE_res_I)
MPE_res_I <- rbind(c(" ", statesnames), MPE_res_I)

table.MPE_I_res <- xtable(MPE_res_I, hline.after = c(1,2))
print(table.MPE_I_res,include.rownames = FALSE, include.colnames = FALSE)


MPE_res_R <- cbind(c("Model 1 ", "Model 2.1 ", "Model 2.2 ", "Model 2.3 " ,"Model 2.4 " ,"Model 3 "), MPE_res_R)
MPE_res_R <- rbind(c(" ", statesnames), MPE_res_R)
table.MPE_R_res <- xtable(MPE_res_R, hline.after = c(1,2))
print(table.MPE_R_res,include.rownames = FALSE, include.colnames = FALSE)



# statesnames <- c("new york", "washington", "florida", "california", "texas")
statesnames <- c("new york", "oregon", "florida", "california", "texas")
alpha_res <- c()
for(i in 1:length(statesnames)){
  filename <- paste0(statesnames[i],"_domain_prediction.RData")
  load(filename)
  temp_1 <- c(round(est.2.1$coefficients[length(est.2.1$coefficients)], 4),  
              summary(est.2.1)$coefficients[length(est.2.1$coefficients), 4], 
              paste("(", round(confint(est.2.1), 4)[length(est.2.1$coefficients), 1] ,",", 
                    round(confint(est.2.1), 4)[length(est.2.1$coefficients), 2], ")" ))
  
  temp_2 <- c(round(est.2.2$coefficients[length(est.2.2$coefficients)], 4),  
              summary(est.2.2)$coefficients[length(est.2.2$coefficients), 4], 
              paste("(", round(confint(est.2.2), 4)[length(est.2.2$coefficients), 1] ,",", 
                    round(confint(est.2.2), 4)[length(est.2.2$coefficients), 2], ")" ))
  
  temp_3 <- c(round(est.2.3$coefficients[length(est.2.3$coefficients)], 4),  
              summary(est.2.3)$coefficients[length(est.2.3$coefficients), 4], 
              paste("(", round(confint(est.2.3), 4)[length(est.2.3$coefficients), 1] ,",", 
                    round(confint(est.2.3), 4)[length(est.2.3$coefficients), 2], ")" ))
  
  temp_4 <- c(round(est.2.4$coefficients[length(est.2.4$coefficients)], 4),  
              summary(est.2.4)$coefficients[length(est.2.4$coefficients), 4], 
              paste("(", round(confint(est.2.4), 4)[length(est.2.4$coefficients), 1] ,",", 
                    round(confint(est.2.4), 4)[length(est.2.4$coefficients), 2], ")" ))
  alpha_res <- rbind(alpha_res,temp_1, temp_2, temp_3, temp_4)
  
}
alpha_res <- cbind(rep(c( "Model 2.1","Model 2.2", "Model 2.3","Model 2.4" ), length(statesnames) ), alpha_res)
# alpha_res <- cbind(c("\\multirow{ 4}{*}{NY}","", "", "","\\multirow{ 4}{*}{WA}", "", "", "", "\\multirow{ 4}{*}{FL}", "", "", "", 
                     # "\\multirow{ 4}{*}{CA}","", "", "", "\\multirow{ 4}{*}{TX}", "", "", "" ), 
                   # alpha_res)
alpha_res <- cbind(c("\\multirow{ 4}{*}{NY}","", "", "","\\multirow{ 4}{*}{OR}", "", "", "", "\\multirow{ 4}{*}{FL}", "", "", "", 
                     "\\multirow{ 4}{*}{CA}","", "", "", "\\multirow{ 4}{*}{TX}", "", "", "" ), 
                   alpha_res)
alpha_res <- rbind(c("","model", "estimate", "p-value",  "confidence interval"), alpha_res)
table.alpha_res <- xtable(alpha_res, hline.after = c(1, 2))
print(table.alpha_res,include.rownames = FALSE, include.colnames = FALSE ,hline.after = c(1,1), sanitize.text.function=identity)




library("xtable")
# statesnames <- c("new york", "washington", "florida", "california", "texas")
statesnames <- c("new york", "oregon", "florida", "california", "texas")
for(i in 1:length(statesnames)){
  print(statesnames[i])
  filename <- paste0(statesnames[i],"_domain_prediction.RData")
  load(filename)
  print(temp.var$cp.final)
}





