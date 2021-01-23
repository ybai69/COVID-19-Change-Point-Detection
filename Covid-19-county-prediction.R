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
library("zoo")
library("lattice")


######## Call Functions #########################
source("functions_BFL.R")
sourceCpp("functions_BFL.cpp")

######## Loading Datasets #######################
######## County-level ###########################
## data extracted from New York Times county-level data obtained from following Github repository
# https://github.com/nytimes/covid-19-data
# load nyt case data
# data.counties <- as.data.frame(data.table::fread("https://raw.githubusercontent.com/nytimes/covid-19-data/master/us-counties.csv"))
# use the data after March
# data.counties<- data.counties[as.Date(data.counties$date) >= as.Date('2020-03-01'),]
# data.counties <- data.counties[as.Date(data.counties$date) <= as.Date('2020-08-18'),]
data.counties <- read.csv("counties-12-23.csv", header = TRUE)
data.counties <- data.counties[as.Date(data.counties$date) <= as.Date('2020-12-13'),]
data.counties$date <-  as.Date(data.counties$date)


# population data extracted from NATIONAL BUREAU OF ECONOMIC RESEARCH
# counties.population <- as.data.frame(data.table::fread("https://www2.census.gov/programs-surveys/popest/datasets/2010-2019/counties/totals/co-est2019-alldata.csv"))
counties.population <- read.csv("co-est2019-alldata.csv", header = TRUE)
counties.population <- counties.population[, c("STNAME", "CTYNAME", "POPESTIMATE2019")]
# distance data extracted from US Census Bureau 
# counties.distance.100 <- as.data.frame(data.table::fread("http://data.nber.org/distance/2010/sf1/county/sf12010countydistance100miles.csv"))
counties.distance.100<-read.csv("sf12010countydistance100miles.csv", header = TRUE)


#########Change the county name, state name, lockdown date and reopen date below!!!!!!
state.name <- "New York"
city.name <- "New York City"
type <- "city"
Date.1 <- '2020-03-22'
Date.2 <- '2020-05-15'

state.name <- "Washington"
county.name <- "King"
type <- "county"
Date.1 <- '2020-03-23'
Date.2 <- '2020-05-30'

state.name <- "Florida"
county.name <- "Miami-Dade"
type <- "county"
Date.1 <- '2020-04-03'
Date.2 <- '2020-05-04'


state.name <- "Louisiana"
county.name <- "Orleans"
type <- "county"
Date.1 <- '2020-03-23'
# phase 1
# Date.2 <- '2020-05-15'
#phase 2
Date.2 <- '2020-06-05'

state.name <- "Louisiana"
county.name <- "Jefferson"
type <- "county"
Date.1 <- '2020-03-23'
# phase 1
# Date.2 <- '2020-05-15'
#phase 2
Date.2 <- '2020-06-05'


# Charleston (mask mandate 7/1), Greenville (no mask mandate), Richland (7/6), Horry (7/3)
state.name <- "South Carolina"
county.name <- "Charleston"
type <- "county"
Date.1 <- '2020-04-07'
Date.2 <- '2020-04-20'
#mask mandate
Date.3 <- '2020-07-01'

state.name <- "South Carolina"
county.name <- "Greenville"
type <- "county"
Date.1 <- '2020-04-07'
Date.2 <- '2020-04-20'
#mask mandate
Date.3 <- NULL

state.name <- "South Carolina"
county.name <- "Richland"
type <- "county"
Date.1 <- '2020-04-07'
Date.2 <- '2020-04-20'
#mask mandate
Date.3 <- '2020-07-06'

state.name <- "South Carolina"
county.name <- "Horry"
type <- "county"
Date.1 <- '2020-04-07'
Date.2 <- '2020-04-20'
#mask mandate
Date.3 <- '2020-07-03'


################################################
# The NYC contains five counties
special <- c("New York", "Bronx", "Kings",  "Queens", "Richmond")
if( type == "city"){
  if( city.name == "New York City"){
    county.name <- special
  }else{
    print("change special variable!")
  }
}

# test dataset : two weeks
n.test <- 7*2

#################################################
# max distance 
miles.distance <- 100
# max number of neighboring regions
if(type == "city"){
  max.neighbor <- 9
}else{
  max.neighbor <- 5
}
# "Louisiana" use parish instead of county!!!
if(state.name == "Louisiana"){
  region.fips <- fips(state = state.name, county = paste(county.name, "parish") )
  
}else{
  region.fips <- fips(state = state.name, county = county.name)
  
}
# region.fips <- fips(state = state.name, county = county.name)
counties.distance.100[counties.distance.100$county1 == region.fips[1],]
neighbor.fips <- counties.distance.100[(counties.distance.100$county1== region.fips[1]) & (counties.distance.100$mi_to_county < miles.distance) , "county2"]
if(length(neighbor.fips) > max.neighbor){
  neighbor.fips <-neighbor.fips[1:max.neighbor]
}
fips_info(neighbor.fips)
state.names <- fips_info(neighbor.fips)$full
state.names <- c(state.name, state.names)
neighbor.names <- fips_info(neighbor.fips)$county 
neighbor.names.short <- gsub(" Parish", "", neighbor.names)
neighbor.names.short <- gsub(" County", "", neighbor.names.short)
# neighbor.names.short <-gsub(" County", "", neighbor.names)
county.names <- c(county.name[1], neighbor.names.short)
county.lowernames <- tolower(county.names)

#n: population 
county.names.full <- paste0(county.names, " County")
n.all <- rep(0, (length(county.names) - length(county.name) + 1) )
idx <- 0
for(i in 1:length(county.names)){
  if(county.names[i] == "La Salle"){
    county.names[i] = "LaSalle"
  }
  if(state.names[i] == "Louisiana"){
    county.names.full[i] <- paste0(county.names[i], " Parish")
  }else{
    county.names.full[i] <- paste0(county.names[i], " County")
  }
  print(county.names.full[i])
  if(county.names[i] %in% county.name){
    idx <- idx +1
    n.all[1] <- n.all[1] + counties.population[ 
      (counties.population$CTYNAME  == county.names.full[i]) & 
        counties.population$STNAME== state.names[i] , "POPESTIMATE2019"]
  }else{
    n.all[i - idx + 1] <- counties.population[ 
      (counties.population$CTYNAME  == county.names.full[i]) & 
        counties.population$STNAME== state.names[i] , "POPESTIMATE2019"]
  }
}


if( type == "city"){
  state.names  <- c(state.name, state.names[!(county.names %in% special)])
  county.names <- c(city.name, county.names[!(county.names %in% special)])
  county.lowernames <- tolower(county.names)
}



dataList <- list()
for( i in c(1:length(county.names))){
  data.subset <- data.counties[data.counties$county == county.names[i] & data.counties$state==state.names[i], c("date", "cases", "deaths")]
  assign(paste("data",county.lowernames[i] , sep="."), data.subset)
  dataList <-append(dataList, list(data.subset))
}
#construct the dataframe including multiple regions
multi_full <- Reduce(
  function(x, y){merge(x, y, all = TRUE, by ="date")},
  dataList
)
for(i in 1:length(county.names)){
  names(multi_full)[2*i] <- paste0('cases.',county.lowernames[i])
  names(multi_full)[2*i+1] <- paste0('deaths.',county.lowernames[i])
}
#replace the na value with 0
n.domains <- length(county.names)
for(i in 1:(n.domains*2) ){
  multi_full[is.na(multi_full[, i+1]), i+1] = 0
}
multi_full$date <- as.Date(multi_full$date)
#choose the first day when the region has one confrimed case as the start date 
multi_full <- multi_full[multi_full[,2]>0, ]

##################################################
cases.all <- multi_full[,seq(2,ncol(multi_full),2)]
deaths.all<- multi_full[,seq(3,ncol(multi_full),2)]

#T: number of time points
T <- nrow(cases.all);
#R: the number of people who have recovered
#using the nationwide recovered and death number to predict the recovered number
R.all <- floor((1+(10))*deaths.all);
if( type == "city"){
  if( city.name == "New York City"){
    R.all <- floor((1+(5.5))*deaths.all);
    
  }
}
#I: the number of people infected at time t
I.all <- cases.all - R.all;

#S: the number of susceptible people
n.all.matrix <- matrix(rep(n.all,T), ncol = n.domains, byrow = TRUE)
# S.all <- n.all.matrix - I.all - R.all;
# suppose 0.5% recovered people can became infected again later.
immune.rate <- 0.995
S.all <- n.all.matrix - I.all - R.all*immune.rate;


#the fraction of S, I and R
S.rate.all <- sapply(1:n.domains, function(jjj) S.all[,jjj]/n.all[jjj])
I.rate.all <- sapply(1:n.domains, function(jjj) I.all[,jjj]/n.all[jjj])
R.rate.all <- sapply(1:n.domains, function(jjj) R.all[,jjj]/n.all[jjj])


I.rate.all.obs <- I.rate.all
R.rate.all.obs <- R.rate.all


I.obs <- I.all[, 1]
R.obs <- R.all[, 1]



#### grid search find a  quadratic function 
#################################### 
# a.vals <- c(0.1, 0.2, 0.4, 0.5, 1 )
# a.vals <- c(0.2, 0.5, 1, 2, 5, 50)
# a.vals <- c(0.2, 0.25, 0.3)
a.vals <-   c(0.1, 0.15, 0.2, 0.25, 0.3)
MRPE_1_new.full <- c()
temp.full <- vector("list", length(a.vals));
est.1.full <- vector("list", length(a.vals));
set.seed(123456)
for(idx in 1:length(a.vals)){
  a.val <- a.vals[idx]
  I <-  I.obs
  R <-  R.obs
  for(t in 2:T){
    rate <- 1/(((t)+a.val*T)/((1 + a.val )*T))^2
    print(1/rate)
    I[t] <- (I.obs[t] - I.obs[t-1])*rate + I[t-1]
  }
  
  plot( 1- (((1:T)+a.val*T)/((1 + a.val )*T))^2 )
  
  
  
  R.rate.all[, 1]<- R/n.all[1]
  I.rate.all[, 1]<- I/n.all[1]
  S.rate.all[, 1]<- (n.all.matrix[, 1] - I - R*immune.rate)/n.all[1];
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
  
  cols <- c("dark orange", "purple", "darkolivegreen4", "blue" )
  plot(multi_full$date[-length(multi_full$date)], beta_t,type='l',col=cols[1],lty=1,lwd = 3,
       ylab ='Rate', xlab= 'Date',cex.lab=2 , cex.axis=2)
  lines(as.Date(multi_full$date[-length(multi_full$date)]),gamma_t,col=cols[3],lty=1,type="l",lwd = 3)
  
  
  Y.full <- Y
  X.full <- X
  
  
  Y.test <- as.matrix(Y.full[(nrow(Y) - (n.test - 1)*2 + 1):nrow(Y), ])
  X.test <- X.full[(nrow(X) - (n.test - 1)*2 + 1):nrow(X), ]
  Y.train <- as.matrix(Y.full[1 : (nrow(Y) - (n.test - 1)*2 ), ])
  X.train <- X.full[1 : (nrow(X) - (n.test - 1)*2 ), ]
  
  # I.test <- I[(T - n.test + 1):T]
  # R.test <- R[(T - n.test + 1):T]
  
  # Delta.R <- rep(0, n.test - 1)
  # for(i in 2:n.test ){
  #   Delta.R[i - 1] <- Y.test[(i - 2)*2 + 1]
  # }
  # Delta.R <- Delta.R*n.all[1]
  
  
  # Delta.I <- rep(0, n.test - 1)
  # for(i in 2:n.test){
  #   rate <- (((T - n.test + i)+a.val*T)/((1 + a.val )*T))^2
  #   Delta.I[i - 1] <- Y.test[(i - 2)*2 + 2]*rate
  # }
  # Delta.I <- Delta.I*n.all[1]
  
  
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
  
  lambda.1 <- c(1, 0.5, 0.1, 0.05, 0.01, 0.005, 0.001)
  if("Miami-Dade" %in% county.name){
    gamma.val = 5
    b_t <- 7
    HBIC = TRUE
  }else if("South Carolina" %in% state.name){
    # b_t <- 7
    b_t <- 5
    gamma.val = 5
    # gamma.val = 1
    HBIC = TRUE
    
  }else if("Louisiana" %in% state.name){
    b_t <- 10
    gamma.val = 1
    HBIC = TRUE
    
  }else{
    b_t <- 7
    HBIC = FALSE
    gamma.val = NULL
  }
  
  
  b_n <- p * b_t
  temp.1 <- tbfl(method, Y_s.train, X_s.train, lambda.1.cv = lambda.1, lambda.2.cv = 0,
                 max.iteration = max.iteration, tol = tol, block.size = b_n, HBIC = HBIC, gamma.val = gamma.val)
  
  temp.full[[idx]] <- temp.1
  
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
  
  # cp <- temp.1$cp.final
  # m <- length(cp)
  # Y.temp.1 <- Y.train[cp[m]: (n), ]
  # X.temp.1 <- X.train[cp[m]: (n), ]
  # est.temp.1 <- lm(Y.temp.1 ~ X.temp.1  - 1)
  # est.temp.1
  # Y.hat.1.new <- X.test%*%c(est.temp.1$coefficients) 
  Y.hat.1.train <- est.1$fitted.values
  
  
  # MRPE_1_new  <- mean(  abs ( (c(Y.hat.1.new[seq(2, 2*(n.test-1), 2)]) - c(Y.test[seq(2, 2*(n.test-1), 2)])  )  /
  #                               c(Y.test[seq(2, 2*(n.test-1), 2)])  )[c(Y.test[seq(2, 2*(n.test-1), 2)]) > 0]  )
  
  MRPE_1_new <- mean(  abs( (Y.hat.1.train[seq(2, n , 2)] - Y.train[seq(2, n, 2)] )  /c(Y.train[seq(2,n,2)])  )[c(Y.train[seq(2,n,2)]) > 0]  )
  
  MRPE_1_new.full <- c(MRPE_1_new.full, MRPE_1_new)
  
}
idx <- which.min(MRPE_1_new.full)
cp.final <- temp.full[[idx]]$cp.final 
cp.date <- c(1:n)[floor( (cp.final-1) / p) + 1]
a.final <- a.vals[idx]
lm.res <- est.1.full[[idx]]
MRPE_1_new.full
date.region[cp.date]
state.name
county.name
a.final
a.vals




######################
set.seed(123456)
a.val <- a.final
I <-  I.obs
R <-  R.obs
for(t in 2:T){
  rate <- 1/(((t)+a.val*T)/((1 + a.val )*T))^2
  print(1/rate)
  I[t] <- (I.obs[t] - I.obs[t-1])*rate + I[t-1]
  
}

plot( 1- (((1:T)+a.val*T)/((1 + a.val )*T))^2 )



R.rate.all[, 1]<- R/n.all[1]
I.rate.all[, 1]<- I/n.all[1]
S.rate.all[, 1]<- (n.all.matrix[, 1] - I - R*immune.rate)/n.all[1];

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

cols <- c("dark orange", "purple", "darkolivegreen4", "blue" )
plot(multi_full$date[-length(multi_full$date)], beta_t,type='l',col=cols[1],lty=1,lwd = 3,
     ylab ='Rate', xlab= 'Date',cex.lab=2 , cex.axis=2)
lines(as.Date(multi_full$date[-length(multi_full$date)]),gamma_t,col=cols[3],lty=1,type="l",lwd = 3)


Y.full <- Y
X.full <- X

Y.test <- as.matrix(Y.full[(nrow(Y) - (n.test - 1)*2 + 1):nrow(Y), ])
X.test <- X.full[(nrow(X) - (n.test - 1)*2 + 1):nrow(X), ]
Y.train <- as.matrix(Y.full[1 : (nrow(Y) - (n.test - 1)*2 ), ])
X.train <- X.full[1 : (nrow(X) - (n.test - 1)*2 ), ]

I.test <- I.obs[(T - n.test + 1):T]
R.test <- R.obs[(T - n.test + 1):T]


Delta.R <- rep(0, n.test - 1)
for(i in 2:n.test ){
  Delta.R[i - 1] <- Y.test[(i - 2)*2 + 1]
}
Delta.R <- Delta.R*n.all[1]


Delta.I <- rep(0, n.test - 1)
for(i in 2:n.test){
  rate <- (((T - n.test + i)+a.val*T)/((1 + a.val )*T))^2
  Delta.I[i - 1] <- Y.test[(i - 2)*2 + 2]*rate
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
b_t <- 7

lambda.1 <- c(1, 0.5, 0.1, 0.05, 0.01, 0.005, 0.001)
if("Miami-Dade" %in% county.name){
  gamma.val = 5
  b_t <- 7
  HBIC = TRUE
}else if("South Carolina" %in% state.name){
  b_t <- 5
  gamma.val = 5
  HBIC = TRUE
  
}else if("Louisiana" %in% state.name){
  b_t <- 10
  gamma.val = 1
  HBIC = TRUE
  
}else{
  HBIC = FALSE
  gamma.val = NULL
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
  rate <- (((T - n.test + i)+a.val*T)/((1 + a.val )*T))^2
  Delta.I.hat.1[i - 1] <- Y.hat.1.new[(i - 2)*2 + 2]*rate
}
Delta.I.hat.1 <- Delta.I.hat.1*n.all[1]


# Delta.I.hat.1 <- rep(0, n.test - 1)
# for(i in 2:n.test){
#   Delta.I.hat.1[i - 1] <- Y.hat.1.new[(i - 2)*2 + 2]
# }
# Delta.I.hat.1 <- Delta.I.hat.1*n.all[1]

R.hat.1.new <- rep(0, n.test)
R.hat.1.new[1] <- R.rate.all[T - n.test + 1, 1]
for(i in 2:n.test){
  R.hat.1.new[i] <- R.rate.all[T - n.test + (i-1), 1] + Y.hat.1.new[(i-2)*2+1]
}
R.hat.1.new <- R.hat.1.new*n.all[1]


I.hat.1.new <- rep(0, n.test)
I.hat.1.new[1] <- I.rate.all.obs[T - n.test + 1, 1]
for(i in 2:n.test){
  rate <- (((T - n.test + i)+a.val*T)/((1 + a.val )*T))^2
  print(rate)
  I.hat.1.new[i] <-  I.rate.all.obs[T - n.test + (i-1), 1] + Y.hat.1.new[(i-2)*2+2]*rate
}
I.hat.1.new <- I.hat.1.new*n.all[1]

I.test <- I.obs[(T - n.test + 1):T]
R.test <- R.obs[(T - n.test + 1):T]

# I.hat.1.new <- rep(0, n.test)
# I.hat.1.new[1] <- I.rate.all[T - n.test + 1, 1]
# for(i in 2:n.test){
#   I.hat.1.new[i] <-  I.rate.all[T - n.test + (i-1), 1] + Y.hat.1.new[(i-2)*2+2]
# }
# I.hat.1.new <- I.hat.1.new*n.all[1]

# I.test <- I[(T - n.test + 1):T]
# R.test <- R[(T - n.test + 1):T]


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
a.val <- a.final
I.rate.all <- I.rate.all.obs
R.rate.all <- R.rate.all.obs
for(t in 2:T){
  rate <- 1/(((t)+a.val*T)/((1 + a.val )*T))^2
  print(1/rate)
  I.rate.all[t, ] <- (I.rate.all.obs[t, ] - I.rate.all.obs[t-1, ])*rate + I.rate.all[t-1, ]
}

distance_1 <- rep(1, n.domains)
distance_1[1] <- 0

Omega_1 <- distance_1 / sum(distance_1)

rows.combined <- 2*(T-1)
cols.combined <- n.domains
matrix.combined <- matrix(0, nrow=rows.combined, ncol=cols.combined)
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

# I.hat.2.1.new <- rep(0, n.test)
# I.hat.2.1.new[1] <- I.rate.all[T - n.test + 1, 1]
# for(i in 2:n.test){
#   I.hat.2.1.new[i] <-  I.rate.all[T - n.test + (i-1), 1] + Y.hat.2.1.new[(i-2)*2+2]
# }
# I.hat.2.1.new <- I.hat.2.1.new*n.all[1]
# 
# I.test <- I[(T - n.test + 1):T]
# R.test <- R[(T - n.test + 1):T]


I.hat.2.1.new <- rep(0, n.test)
I.hat.2.1.new[1] <- I.rate.all.obs[T - n.test + 1, 1]
for(i in 2:n.test){
  rate <- (((T - n.test + i)+a.val*T)/((1 + a.val )*T))^2
  print(rate)
  I.hat.2.1.new[i] <-  I.rate.all.obs[T - n.test + (i-1), 1] + Y.hat.2.1.new[(i-2)*2+2]*rate
}
I.hat.2.1.new <- I.hat.2.1.new*n.all[1]


MRPE_2.1_new_I <- mean(  abs ( (     c(I.hat.2.1.new[-1]) - c(I.test[-1])     )  /c(I.test[-1])  )[c(I.test[-1]) > 0]  )
print(round(MRPE_2.1_new_I, 4))
MRPE_2.1_new_R <- mean(  abs ( (     c(R.hat.2.1.new[-1]) - c(R.test[-1])     )  /c(R.test[-1])  )[c(R.test[-1]) > 0]  )
print(round(MRPE_2.1_new_R, 4))



##################################
##  model 2.2 distance-based weight
##################################
distance_2 <- rep(0, n.domains)
for(i in 2:n.domains){
  if(state.names[i] == "Louisiana"){
    temp.fips <- as.numeric(fips(state = state.names[i], county = paste(county.names[i], "parish")))
    distance.temp <- counties.distance.100[ (counties.distance.100$county1== region.fips[1] ) &
                                              (counties.distance.100$county2== temp.fips ), "mi_to_county"]
    
  }else{
    temp.fips <- as.numeric(fips(state = state.names[i], county = county.names[i]))
    distance.temp <- counties.distance.100[ (counties.distance.100$county1== region.fips[1] ) &
                                              (counties.distance.100$county2== temp.fips ), "mi_to_county"]
  }
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

# I.hat.2.2.new <- rep(0, n.test)
# I.hat.2.2.new[1] <- I.rate.all[T - n.test + 1, 1]
# for(i in 2:n.test){
#   I.hat.2.2.new[i] <-  I.rate.all[T - n.test + (i-1), 1] + Y.hat.2.2.new[(i-2)*2+2]
# }
# I.hat.2.2.new <- I.hat.2.2.new*n.all[1]
# 
# I.test <- I[(T - n.test + 1):T]
# R.test <- R[(T - n.test + 1):T]


I.hat.2.2.new <- rep(0, n.test)
I.hat.2.2.new[1] <- I.rate.all.obs[T - n.test + 1, 1]
for(i in 2:n.test){
  rate <- (((T - n.test + i)+a.val*T)/((1 + a.val )*T))^2
  print(rate)
  I.hat.2.2.new[i] <-  I.rate.all.obs[T - n.test + (i-1), 1] + Y.hat.2.2.new[(i-2)*2+2]*rate
}
I.hat.2.2.new <- I.hat.2.2.new*n.all[1]



MRPE_2.2_new_I <- mean(  abs ( (     c(I.hat.2.2.new[-1]) - c(I.test[-1])     )  /c(I.test[-1])  )[c(I.test[-1]) > 0]  )
print(round(MRPE_2.2_new_I, 4))
MRPE_2.2_new_R <- mean(  abs ( (     c(R.hat.2.2.new[-1]) - c(R.test[-1])     )  /c(R.test[-1])  )[c(R.test[-1]) > 0]  )
print(round(MRPE_2.2_new_R, 4))


#################################################
##########model 2.3    similarity weight ########
#################################################
# get all the counites fips in one state
neighbor.fips <- c()
# "Louisiana" use parish instead of county!!!
if(state.name == "Louisiana"){
  region.fips <- fips(state = state.name, county = paste(county.name, "parish") )
  
}else{
  region.fips <- fips(state = state.name, county = county.name)
  
}
# region.fips <- as.numeric(fips(state = state.name, county = county.name))
state.fip <- as.numeric(fips(state.name))
for(i in 1: length(county.fips$fips)){
  if(nchar(county.fips$fips[i]) == 5){
    state.fips <- substr(county.fips$fips[i], 1, 2)
  }
  if(nchar(county.fips$fips[i]) == 4){
    state.fips <- substr(county.fips$fips[i], 1, 1)
  }
  if(state.fips == state.fip & county.fips$fips[i]!=region.fips[1] ){
    neighbor.fips <- c(neighbor.fips, county.fips$fips[i]) 
    
  }
}

fips_info(neighbor.fips)

state.names <- fips_info(neighbor.fips)$full
state.names <- c(state.name, state.names)
neighbor.names <- fips_info(neighbor.fips)$county 
neighbor.names.short <- gsub(" Parish", "", neighbor.names)
neighbor.names.short <- gsub(" County", "", neighbor.names.short)
# neighbor.names.short <-gsub(" County", "", neighbor.names)
county.names <- c(county.name[1], neighbor.names.short)
county.lowernames <- tolower(county.names)



#n: population 
county.names.full <- paste0(county.names, " County")
n.all <- rep(0, (length(county.names) - length(county.name) + 1) )
idx <- 0
for(i in 1:length(county.names)){
  #consistent with the counties.population dataset 
  if(county.names[i] == "La Salle"){
    county.names[i] = "LaSalle"
  }
  if(state.names[i] == "Louisiana"){
    county.names.full[i] <- paste0(county.names[i], " Parish")
  }else{
    county.names.full[i] <- paste0(county.names[i], " County")
  }
  print(county.names.full[i])
  if(county.names[i] %in% county.name){
    idx <- idx +1
    n.all[1] <- n.all[1] + counties.population[ 
      (counties.population$CTYNAME  == county.names.full[i]) & 
        counties.population$STNAME== state.names[i] , "POPESTIMATE2019"]
  }else{
    n.all[i - idx + 1] <- counties.population[ 
      (counties.population$CTYNAME  == county.names.full[i]) & 
        counties.population$STNAME== state.names[i] , "POPESTIMATE2019"]
  }
}


if( type == "city"){
  state.names  <- c(state.name, state.names[!(county.names %in% special)])
  county.names <- c(city.name, county.names[!(county.names %in% special)])
  county.lowernames <- tolower(county.names)
}

county.names.all <- county.names
state.names.all <- state.names
county.lowernames.all <- county.lowernames


dataList <- list()
for( i in c(1:length(county.names))){
  data.subset <- data.counties[data.counties$county == county.names[i] & data.counties$state==state.names[i], c("date", "cases", "deaths")]
  assign(paste("data",county.lowernames[i] , sep="."), data.subset)
  dataList <-append(dataList, list(data.subset))
}
#construct the dataframe including multiple regions
multi_full <- Reduce(
  function(x, y){merge(x, y, all = TRUE, by ="date")},
  dataList
)
for(i in 1:length(county.names)){
  names(multi_full)[2*i] <- paste0('cases.',county.lowernames[i])
  names(multi_full)[2*i+1] <- paste0('deaths.',county.lowernames[i])
}
#replace the na value with 0
n.domains <- length(county.names)
for(i in 1:(n.domains*2) ){
  multi_full[is.na(multi_full[, i+1]), i+1] = 0
}
multi_full$date <- as.Date(multi_full$date)
#choose the first day when the region has one confrimed case as the start date 
multi_full <- multi_full[multi_full[,2] > 0, ]

##################################################
cases.all <- multi_full[,seq(2,ncol(multi_full),2)]
deaths.all<- multi_full[,seq(3,ncol(multi_full),2)]

#T: number of time points
T <- nrow(cases.all);
#R: the number of people who have recovered
#using the nationwide recovered and death number to predict the recovered number
R.all <- floor((1+(10))*deaths.all);
if( type == "city"){
  if( city.name == "New York City"){
    R.all <- floor((1+(5.5))*deaths.all);
    
  }
}
#I: the number of people infected at time t
I.all <- cases.all - R.all;

#S: the number of susceptible people
n.all.matrix <- matrix(rep(n.all,T), ncol = n.domains, byrow = TRUE)
# S.all <- n.all.matrix - I.all - R.all;
S.all <- n.all.matrix - I.all - R.all*immune.rate;


#the fraction of S, I and R
# S.rate.all <- sapply(1:n.domains, function(jjj) S.all[,jjj]/n.all[jjj])
I.rate.all <- sapply(1:n.domains, function(jjj) I.all[,jjj]/n.all[jjj])
R.rate.all <- sapply(1:n.domains, function(jjj) R.all[,jjj]/n.all[jjj])


I.rate.all.obs <- I.rate.all
R.rate.all.obs <- R.rate.all

a.val <- a.final
for(t in 2:T){
  rate <- 1/(((t)+a.val*T)/((1 + a.val )*T))^2
  print(1/rate)
  I.rate.all[t, ] <- (I.rate.all.obs[t, ] - I.rate.all.obs[t-1, ])*rate + I.rate.all[t-1, ]
}



n.regions <- ncol(I.rate.all)
y.list <- vector("list",T-1);
for(i in 2:T){
  y.list[[i-1]] <- matrix(c(R.rate.all[i,] - R.rate.all[i-1, ], I.rate.all[i, ] - I.rate.all[i-1, ]), 2, ncol = n.regions, byrow = TRUE);
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
county.index <- order(l2.diff)[1: max.neighbor]
county.names <- county.names.all[county.index]
county.lowernames <- county.lowernames.all[county.index]
state.names <- state.names.all[county.index]
county.names



n.all.full <- n.all
# S.rate.all.full <- S.rate.all
I.rate.all.full <- I.rate.all
R.rate.all.full <- R.rate.all

n.domains <- length(county.names)
n.all <- n.all[county.index]
# S.rate.all <- S.rate.all[, county.index]
I.rate.all <- I.rate.all[, county.index]
R.rate.all <- R.rate.all[, county.index]

date.region <- multi_full$date
# I <- I.all[, 1]
# R <- R.all[, 1]

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


#remove the last two elements  at time point t=T
#and add two elements for time point t= 0
neighbor.weighted_3 <- as.matrix(neighbor.weighted_3[-c(rows.combined-1,rows.combined),])
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
I.hat.2.3.new[1] <- I.rate.all.obs[T - n.test + 1, 1]
for(i in 2:n.test){
  rate <- (((T - n.test + i)+a.val*T)/((1 + a.val )*T))^2
  print(rate)
  I.hat.2.3.new[i] <-  I.rate.all.obs[T - n.test + (i-1), 1] + Y.hat.2.3.new[(i-2)*2+2]*rate
}
I.hat.2.3.new <- I.hat.2.3.new*n.all[1]



# I.hat.2.3.new <- rep(0, n.test)
# I.hat.2.3.new[1] <- I.rate.all[T - n.test + 1, 1]
# for(i in 2:n.test){
#   I.hat.2.3.new[i] <-  I.rate.all[T - n.test + (i-1), 1] + Y.hat.2.3.new[(i-2)*2+2]
# }
# I.hat.2.3.new <- I.hat.2.3.new*n.all[1]
# 
# I.test <- I[(T - n.test + 1):T]
# R.test <- R[(T - n.test + 1):T]


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


# Delta.I.hat.2 <- rep(0, n.test - 1)
# for(i in 2:n.test){
#   Delta.I.hat.2[i - 1] <- Y.hat.2.3.new[(i - 2)*2 + 2]
# }
# Delta.I.hat.2 <- Delta.I.hat.2*n.all[1]

Delta.I.hat.2 <- rep(0, n.test - 1)
for(i in 2:n.test){
  rate <- (((T - n.test + i)+a.val*T)/((1 + a.val )*T))^2
  Delta.I.hat.2[i - 1] <- Y.hat.2.3.new[(i - 2)*2 + 2]*rate
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

# I.hat.3.new <- rep(0, n.test)
# I.hat.3.new[1] <- I.rate.all[T - n.test + 1, 1]
# for(i in 2:n.test){
#   I.hat.3.new[i] <-  I.rate.all[T - n.test + (i-1), 1] + Y.hat.3.new[(i-2)*2+2]
# }
# I.hat.3.new <- I.hat.3.new*n.all[1]
# 
# I.test <- I[(T - n.test + 1):T]
# R.test <- R[(T - n.test + 1):T]


I.hat.3.new <- rep(0, n.test)
I.hat.3.new[1] <- I.rate.all.obs[T - n.test + 1, 1]
for(i in 2:n.test){
  rate <- (((T - n.test + i)+a.val*T)/((1 + a.val )*T))^2
  print(rate)
  I.hat.3.new[i] <-  I.rate.all.obs[T - n.test + (i-1), 1] + Y.hat.3.new[(i-2)*2+2]*rate
}
I.hat.3.new <- I.hat.3.new*n.all[1]


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


# Delta.I.hat.3 <- rep(0, n.test - 1)
# for(i in 2:n.test){
#   Delta.I.hat.3[i - 1] <- Y.hat.3.new[(i - 2)*2 + 2]
# }
# Delta.I.hat.3 <- Delta.I.hat.3*n.all[1]


Delta.I.hat.3 <- rep(0, n.test - 1)
for(i in 2:n.test){
  rate <- (((T - n.test + i)+a.val*T)/((1 + a.val )*T))^2
  Delta.I.hat.3[i - 1] <- Y.hat.3.new[(i - 2)*2 + 2]*rate
}
Delta.I.hat.3 <- Delta.I.hat.3*n.all[1]


filename <- paste0("Delta_I_PI_", county.lowernames[1], "_all.pdf")
pdf(filename, width = 11, height = 8.5)
par(mar = c(4.5, 6.2, 2, 2.2), mgp=c(3.5, 1.2, 0))
ylim_min <- min(0, Delta.I,  Delta.I.hat.1, Delta.I.hat.2, Delta.I.hat.3)
ylim_max <- max(0, Delta.I,  Delta.I.hat.1, Delta.I.hat.2, Delta.I.hat.3)
plot(multi_full$date[(T-n.test+1):(T-1)], Delta.I , col = '1', ylim =c(ylim_min, ylim_max), lty = 1, type = "l", lwd = 3,
     ylab = expression(paste(Delta, "I(t)")), xlab = 'Date', cex.lab = 3, cex.axis = 3)
lines(multi_full$date[(T-n.test+1):(T-1)], Delta.I.hat.1, col = 'orange', lty = 1, type = "b", lwd = 3)
lines(multi_full$date[(T-n.test+1):(T-1)], Delta.I.hat.2, col = 'green', lty = 1, type = "b", lwd = 3)
lines(multi_full$date[(T-n.test+1):(T-1)], Delta.I.hat.3, col = 'blue', lty = 1, type = "b", lwd = 3)
dev.off()


filename <- paste0("Delta_R_PI_", county.lowernames[1], "_all.pdf")
pdf(filename, width=11, height=8.5)
par(mar = c(4.5, 6.2, 2, 2.2), mgp=c(3.5, 1.2, 0))
ylim_min <- min(0, Delta.R, Delta.R.hat.1, Delta.R.hat.2, Delta.R.hat.3)
ylim_max <- max(0, Delta.R, Delta.R.hat.1, Delta.R.hat.2, Delta.R.hat.3)
plot(multi_full$date[(T-n.test+1):(T-1)], Delta.R, col='1', ylim = c(ylim_min, ylim_max), lty=1, type="l", lwd = 3,
     ylab = expression(paste(Delta, "R(t)")), xlab= 'Date', cex.lab = 3, cex.axis = 3)
lines(multi_full$date[(T-n.test+1):(T-1)], Delta.R.hat.1, col='orange', lty=1, type= "b", lwd = 3)
lines(multi_full$date[(T-n.test+1):(T-1)], Delta.R.hat.2, col='green', lty=1, type="b", lwd = 3)
lines(multi_full$date[(T-n.test+1):(T-1)], Delta.R.hat.3, col='blue', lty=1, type="b", lwd = 3)
dev.off()

#######################################
# model 2.4 use all the states #######
######################################
# get all the counites fips in one state

n.domains <- length(county.names.all)
distance_4 <- rep(0, n.domains)
#Power Distance Weights.
for(i in 2:n.domains){
  distance.temp <- sort(l2.diff)[i]
  distance_4[i] <- 1/(distance.temp)^1
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

# I.hat.2.4.new <- rep(0, n.test)
# I.hat.2.4.new[1] <- I.rate.all[T - n.test + 1, 1]
# for(i in 2:n.test){
#   I.hat.2.4.new[i] <-  I.rate.all[T - n.test + (i-1), 1] + Y.hat.2.4.new[(i-2)*2+2]
# }
# I.hat.2.4.new <- I.hat.2.4.new*n.all[1]
# 
# I.test <- I[(T - n.test + 1):T]
# R.test <- R[(T - n.test + 1):T]

I.hat.2.4.new <- rep(0, n.test)
I.hat.2.4.new[1] <- I.rate.all.obs[T - n.test + 1, 1]
for(i in 2:n.test){
  rate <- (((T - n.test + i)+a.val*T)/((1 + a.val )*T))^2
  print(rate)
  I.hat.2.4.new[i] <-  I.rate.all.obs[T - n.test + (i-1), 1] + Y.hat.2.4.new[(i-2)*2+2]*rate
}
I.hat.2.4.new <- I.hat.2.4.new*n.all[1]



MRPE_2.4_new_I <- mean(  abs ( (     c(I.hat.2.4.new[-1]) - c(I.test[-1])     )  /c(I.test[-1])  )[c(I.test[-1]) > 0]  )
print(round(MRPE_2.4_new_I, 4))
MRPE_2.4_new_R <- mean(  abs ( (     c(R.hat.2.4.new[-1]) - c(R.test[-1])     )  /c(R.test[-1])  )[c(R.test[-1]) > 0]  )
print(round(MRPE_2.4_new_R, 4))



#############################################
filename <- paste0(county.lowernames[1], "_domain_prediction.RData")
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
countiesnames <- c("new york city", "king", "miami-dade", "orleans", "jefferson")
countiesnames <- c("charleston", "greenville" ,"richland", "horry") 
MPE_res <- c()
for(i in 1:length(countiesnames)){
  filename <- paste0(countiesnames[i],"_domain_prediction.RData")
  load(filename)
  MPE_res <- cbind(MPE_res,  round(c(MRPE_1_new_I, MRPE_2.1_new_I, MRPE_2.2_new_I, MRPE_2.3_new_I, MRPE_2.4_new_I, MRPE_3_new_I ),4), 
                   round(c(MRPE_1_new_R, MRPE_2.1_new_R, MRPE_2.2_new_R, MRPE_2.3_new_R, MRPE_2.4_new_R, MRPE_3_new_R),4))
}
MPE_res <- cbind(c("Model 1", "Model 2.1", "Model 2.2", "Model 2.3", "Model 2.4", "Model 3"), MPE_res)
table.MPE_res <- xtable(MPE_res, hline.after = c(1,2))
print(table.MPE_res,include.rownames = FALSE, include.colnames = FALSE)




# countiesnames <- c("new york city", "king", "miami-dade")
# countiesnames <- c("charleston", "greenville" ,"richland", "horry") 
# countiesnames <- c( "orleans", "jefferson")
# countiesnames <- c("new york city", "king", "miami-dade",  "jefferson", 
                   # "greenville", "horry") 
# countiesnames <- c("orleans", "charleston", "richland")
countiesnames <- c("new york city", "king", "miami-dade", "orleans", "jefferson", 
                   "charleston", "greenville", "richland", "horry") 
alpha_res <- c()
for(i in 1:length(countiesnames)){
  filename <- paste0(countiesnames[i],"_domain_prediction.RData")
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
alpha_res <- cbind(rep(c( "Model 2.1","Model 2.2", "Model 2.3","Model 2.4" ), length(countiesnames) ), alpha_res)
# alpha_res <- cbind(c("\\multirow{ 4}{*}{NYC}","","","","\\multirow{ 4}{*}{King}", "","","", "\\multirow{ 4}{*}{Miami-Dade}", "","","" ), 
#                    alpha_res)
alpha_res <- cbind(c("\\multirow{ 4}{*}{NYC}","","","","\\multirow{ 4}{*}{King}", "","","", "\\multirow{ 4}{*}{Miami-Dade}", "","","",
                     "\\multirow{ 4}{*}{Orleans}","","","","\\multirow{ 4}{*}{Jefferson}", "","","", 
                     "\\multirow{ 4}{*}{Charleston}", "","","", "\\multirow{ 4}{*}{Greenville}","","","",
                     "\\multirow{ 4}{*}{Richland}", "","","", "\\multirow{ 4}{*}{Horry}", "","",""
                     ), 
                   alpha_res)
alpha_res <- rbind(c("","model", "estimate", "p-value",  "confidence interval"), alpha_res)
table.alpha_res <- xtable(alpha_res, hline.after = c(1, 2))
print(table.alpha_res,include.rownames = FALSE, include.colnames = FALSE ,hline.after = c(1,1), sanitize.text.function=identity)




library("xtable")
# countiesnames <- c("new york city", "king", "miami-dade")
# countiesnames <- c("charleston", "greenville" ,"richland", "horry") 
# countiesnames <- c( "orleans", "jefferson")
countiesnames <- c("new york city", "king", "miami-dade", "orleans", "jefferson", 
                   "charleston", "greenville", "richland", "horry") 
for(i in 1:length(countiesnames)){
  print(countiesnames[i])
  filename <- paste0(countiesnames[i],"_domain_prediction.RData")
  load(filename)
  print(temp.var$cp.final)
  print(date.region[floor( (temp.var$cp.final-1) / p) + 1])
}









