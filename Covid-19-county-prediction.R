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


######## Call Functions #########################
source("functions_BFL.R")
sourceCpp("functions_BFL.cpp")

######## Loading Datasets #######################
######## County-level ###########################
## data extracted from New York Times county-level data obtained from following Github repository
# https://github.com/nytimes/covid-19-data
# load nyt case data
data.counties <- as.data.frame(data.table::fread("https://raw.githubusercontent.com/nytimes/covid-19-data/master/us-counties.csv"))
# use the data after March
data.counties<- data.counties[as.Date(data.counties$date) >= as.Date('2020-03-01'),]
data.counties <- data.counties[as.Date(data.counties$date) <= as.Date('2020-08-18'),]
print("last updated date:"); print(max(data.counties$date))

# population data extracted from NATIONAL BUREAU OF ECONOMIC RESEARCH
counties.population <- as.data.frame(data.table::fread("https://www2.census.gov/programs-surveys/popest/datasets/2010-2019/counties/totals/co-est2019-alldata.csv"))
counties.population <- counties.population[, c("STNAME", "CTYNAME", "POPESTIMATE2019")]
# distance data extracted from US Census Bureau 
counties.distance.100 <- as.data.frame(data.table::fread("http://data.nber.org/distance/2010/sf1/county/sf12010countydistance100miles.csv"))


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
R.all <- floor((1+(5.5))*deaths.all);
#I: the number of people infected at time t
I.all <- cases.all - R.all;

#S: the number of susceptible people
n.all.matrix <- matrix(rep(n.all,T), ncol = n.domains, byrow = TRUE)
S.all <- n.all.matrix - I.all - R.all;

#the fraction of S, I and R
S.rate.all <- sapply(1:n.domains, function(jjj) S.all[,jjj]/n.all[jjj])
I.rate.all <- sapply(1:n.domains, function(jjj) I.all[,jjj]/n.all[jjj])
R.rate.all <- sapply(1:n.domains, function(jjj) R.all[,jjj]/n.all[jjj])


I <-I.all[, 1]
R <-R.all[, 1]

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


#################################################
######### Model 1 ###############################
#################################################
Y.full <- Y
X.full <- X

Y.test <- as.matrix(Y.full[(nrow(Y) - (n.test - 1)*2 + 1):nrow(Y), ])
X.test <- X.full[(nrow(X) - (n.test - 1)*2 + 1):nrow(X), ]
Y.train <- as.matrix(Y.full[1 : (nrow(Y) - (n.test - 1)*2 ), ])
X.train <- X.full[1 : (nrow(X) - (n.test - 1)*2 ), ]

I.test <- I[(T - n.test + 1):T]
R.test <- R[(T - n.test + 1):T]



est <- lm((Y.train)~(X.train)-1)
Y.hat.1.new <- (X.test)%*% est$coefficients


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



MSPE_1_new <- mean((c(I.hat.1.new[-1], R.hat.1.new[-1]) - c(I.test [-1], R.test[-1]))^2)
print(round(MSPE_1_new))
MSPE_1_new_I <- mean((I.hat.1.new[-1] - I.test[-1])^2)
print(round(MSPE_1_new_I))
MSPE_1_new_R <- mean((R.hat.1.new[-1] - R.test[-1])^2)
print(round(MSPE_1_new_R))

MRPE_1_new <- mean(  abs ( (     c(R.hat.1.new[-1], I.hat.1.new[-1]) - c(R.test[-1], I.test[-1])     )  /c(R.test[-1], I.test[-1])  )[c(R.test[-1], I.test[-1]) > 0]  )
print(round(MRPE_1_new, 4))
MRPE_1_new_I <- mean(  abs ( (     c(I.hat.1.new[-1]) - c(I.test[-1])     )  /c(I.test[-1])  )[c(I.test[-1]) > 0]  )
print(round(MRPE_1_new_I, 4))
MRPE_1_new_R <- mean(  abs ( (     c(R.hat.1.new[-1]) - c(R.test[-1])     )  /c(R.test[-1])  )[c(R.test[-1]) > 0]  )
print(round(MRPE_1_new_R, 4))

MRPE_1_new_Delta <- mean(  abs ( (  c(Y.hat.1.new) - c(Y.test)  )  /c(Y.test)  )[c(Y.test) > 0]  )
print(round(MRPE_1_new_Delta, 4))
MRPE_1_new_I_Delta <- mean(  abs ( (c(Y.hat.1.new[seq(2, nrow(Y.test), 2)]) - c(Y.test[seq(2, nrow(Y.test), 2)])  )  /c(Y.test[seq(2, nrow(Y.test), 2)])  )[c(Y.test[seq(2, nrow(Y.test), 2)]) > 0]  )
print(round(MRPE_1_new_I_Delta, 4))
MRPE_1_new_R_Delta <- mean(  abs ( (c(Y.hat.1.new[seq(1, nrow(Y.test), 2)]) - c(Y.test[seq(1, nrow(Y.test), 2)])  )  /c(Y.test[seq(1, nrow(Y.test), 2)])  )[c(Y.test[seq(1, nrow(Y.test), 2)]) > 0]  )
print(round(MRPE_1_new_R_Delta, 4))



Delta.R.hat <- rep(0, n.test - 1)
Delta.R <- rep(0, n.test - 1)
for(i in 2:n.test ){
  Delta.R.hat[i - 1] <- Y.hat.1.new[(i - 2)*2 + 1]
  Delta.R[i - 1] <- Y.test[(i - 2)*2 + 1]
}
Delta.R.hat <- Delta.R.hat*n.all[1]
Delta.R <- Delta.R*n.all[1]


Delta.I.hat <- rep(0, n.test - 1)
Delta.I <- rep(0, n.test - 1)
for(i in 2:n.test){
  Delta.I.hat[i - 1] <- Y.hat.1.new[(i - 2)*2 + 2]
  Delta.I[i - 1] <- Y.test[(i - 2)*2 + 2]
}
Delta.I.hat <- Delta.I.hat*n.all[1]
Delta.I <- Delta.I*n.all[1]


residual_full <- est$residuals*n.all[1]
residual_Delta.R <- residual_full[seq(1, length(Y.train), 2 )]
residual_Delta.I <- residual_full[seq(2, length(Y.train), 2 )]

Delta.I.hat.CI.ub  <-  Delta.I.hat; Delta.I.hat.CI.lb  <-  Delta.I.hat
Delta.R.hat.CI.ub  <-  Delta.R.hat; Delta.R.hat.CI.lb  <-  Delta.R.hat

X.temp <- n.all[1] * X.train
XX.temp <- t(X.temp) %*% X.temp
XXinv.temp <- solve(XX.temp)
X.test.temp <- n.all[1] * X.test

sd.temp.error <- sqrt(sum(est$residuals^2)/(nrow(Y.train) - 2))*n.all[1]
for(i in 1: (n.test-1)){
  
  # for I 
  sd.temp.I <- sd.temp.error*sqrt(1 + matrix(X.test.temp[2*i, ], nrow = 1) %*% XXinv.temp%*%matrix(X.test.temp[2*i, ], ncol = 1))
  # for R
  sd.temp.R <- sd.temp.error*sqrt(1 + matrix(X.test.temp[2*i-1, ], nrow = 1) %*% XXinv.temp%*%matrix(X.test.temp[2*i-1, ], ncol = 1))
  
  print(sd.temp.I)
  Delta.I.hat.CI.ub[i]  <-  Delta.I.hat[i] + qt(.975, df = nrow(Y.train)-2)*sd.temp.I
  Delta.I.hat.CI.lb[i]  <-  Delta.I.hat[i] - qt(.975, df = nrow(Y.train)-2)*sd.temp.I
  Delta.R.hat.CI.ub[i]  <-  Delta.R.hat[i] + qt(.975, df = nrow(Y.train)-2)*sd.temp.R
  Delta.R.hat.CI.lb[i]  <-  Delta.R.hat[i] - qt(.975, df = nrow(Y.train)-2)*sd.temp.R
  
}
cbind(Delta.I, Delta.I.hat, Delta.I.hat.CI.lb, Delta.I.hat.CI.ub)
cbind(Delta.R, Delta.R.hat, Delta.R.hat.CI.lb, Delta.R.hat.CI.ub)


filename <- paste0("Delta_I_PI_", county.lowernames[1], "_1.pdf")
pdf(filename, width=11, height=8.5)
par(mar = c(4, 5, 1.5, 1))
plot(multi_full$date[(T-n.test+1):(T-1)], Delta.I , col='1', ylim=c(min(0, Delta.I.hat.CI.lb), max(Delta.I, Delta.I.hat.CI.ub)), lty=1, type="l", lwd = 3,
     ylab = expression(paste(Delta, "I(t)")), xlab= 'Date', cex.lab=2 , cex.axis=2)
lines(multi_full$date[(T-n.test+1):(T-1)], Delta.I.hat, col='2', lty=1, type="l", lwd = 3)
lines(multi_full$date[(T-n.test+1):(T-1)], Delta.I.hat.CI.ub, col='3', lty = 2, type="l", lwd = 3)
lines(multi_full$date[(T-n.test+1):(T-1)], Delta.I.hat.CI.lb, col='3', lty = 2, type="l", lwd = 3)
dev.off()


filename <- paste0("Delta_R_PI_", county.lowernames[1] ,"_1.pdf")
pdf(filename, width=11, height=8.5)
par(mar = c(4., 5, 1.5, 1))
plot(multi_full$date[(T-n.test+1):(T-1)], Delta.R , col='1', ylim=c(min(0, Delta.R.hat.CI.lb), max(Delta.R, Delta.R.hat.CI.ub)), lty=1, type="l", lwd = 3,
     ylab = expression(paste(Delta, "R(t)")), xlab= 'Date', cex.lab=2, cex.axis=2)
lines(multi_full$date[(T-n.test+1):(T-1)], Delta.R.hat, col='2', lty=1, type="l", lwd = 3)
lines(multi_full$date[(T-n.test+1):(T-1)], Delta.R.hat.CI.ub, col='3', lty = 2, type="l", lwd = 3)
lines(multi_full$date[(T-n.test+1):(T-1)], Delta.R.hat.CI.lb, col='3', lty = 2, type="l", lwd = 3)
dev.off()


Delta.I.full <- rep(0, T-1)
for(i in 2:T){
  Delta.I.full[i - 1] <- Y.full[(i - 2)*2 + 2]
}
Delta.I.full <- Delta.I.full*n.all[1]

Delta.R.full <- rep(0, T-1)
for(i in 2:T){
  Delta.R.full[i - 1] <- Y.full[(i - 2)*2 + 1]
}
Delta.R.full <- Delta.R.full*n.all[1]



filename <- paste0("Delta_I_PI_", county.lowernames[1], "_1_full.pdf")
pdf(filename, width=11, height=8.5)
par(mar = c(4, 5, 1.5, 1))
plot(multi_full$date[1:(T-1)], Delta.I.full , col='1', ylim=c(min(0, Delta.I.hat.CI.lb), max(Delta.I.full, Delta.I.hat.CI.ub)), lty=1, type="l", lwd = 3,
     ylab = expression(paste(Delta, "I(t)")), xlab= 'Date', cex.lab=2 , cex.axis=2)
lines(multi_full$date[(T-n.test+1):(T-1)], Delta.I.hat, col='2', lty=1, type="l", lwd = 3)
lines(multi_full$date[(T-n.test+1):(T-1)], Delta.I.hat.CI.ub, col='3', lty = 2, type="l", lwd = 3)
lines(multi_full$date[(T-n.test+1):(T-1)], Delta.I.hat.CI.lb, col='3', lty = 2, type="l", lwd = 3)
dev.off()


filename <- paste0("Delta_R_PI_", county.lowernames[1] ,"_1_full.pdf")
pdf(filename, width=11, height=8.5)
par(mar = c(4., 5, 1.5, 1))
plot(multi_full$date[1:(T-1)], Delta.R.full , col='1', ylim=c(min(0, Delta.R.hat.CI.lb), max(Delta.R.full, Delta.R.hat.CI.ub )), lty=1, type="l", lwd = 3,
     ylab = expression(paste(Delta, "R(t)")), xlab= 'Date', cex.lab=2, cex.axis=2)
lines(multi_full$date[(T-n.test+1):(T-1)], Delta.R.hat, col='2', lty=1, type="l", lwd = 3)
lines(multi_full$date[(T-n.test+1):(T-1)], Delta.R.hat.CI.ub, col='3', lty = 2, type="l", lwd = 3)
lines(multi_full$date[(T-n.test+1):(T-1)], Delta.R.hat.CI.lb, col='3', lty = 2, type="l", lwd = 3)
dev.off()


#################################################
######### Model 2  ##############################
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
  b_t <- 5
  HBIC = TRUE
}else if("South Carolina" %in% state.name){
  b_t <- 7
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
#################

#############################################
##### refit model
#############################################
cp <- c(1, temp.1$cp.final, n+1)
m <- length(cp) - 1
X.new.new <- matrix(0, nrow = n, ncol = m*p.x)
for(i in 1:m){
  X.new.new[cp[i]: (cp[i+1]-1), (p.x*(i-1)+1) : (p.x*i) ] <- X.train[cp[i]: (cp[i+1]-1), ]
}

est.2 <- lm(Y.train ~ X.new.new  - 1)
summary(est.2 )


cp <- temp.1$cp.final
m <- length(cp)
Y.temp.1 <- Y.train[cp[m]: (n), ]
X.temp.1 <- X.train[cp[m]: (n), ]
est.temp.1 <- lm(Y.temp.1 ~ X.temp.1  - 1)
est.temp.1
Y.hat.2.new <- X.test%*%c(est.temp.1$coefficients) 
Y.hat.2.train <- est.2$fitted.values

Delta.R.hat.2 <- rep(0, n.test - 1)
for(i in 2:n.test ){
  Delta.R.hat.2[i - 1] <- Y.hat.2.new[(i - 2)*2 + 1]
}
Delta.R.hat.2 <- Delta.R.hat.2*n.all[1]


Delta.I.hat.2 <- rep(0, n.test - 1)
for(i in 2:n.test){
  Delta.I.hat.2[i - 1] <- Y.hat.2.new[(i - 2)*2 + 2]
}
Delta.I.hat.2 <- Delta.I.hat.2*n.all[1]



Delta.I.hat.CI.ub.2  <-  Delta.I.hat.2; Delta.I.hat.CI.lb.2  <-  Delta.I.hat.2
Delta.R.hat.CI.ub.2  <-  Delta.R.hat.2; Delta.R.hat.CI.lb.2  <-  Delta.R.hat.2

X.temp <- n.all[1] * X.new.new
XX.temp <- t(X.temp) %*% X.temp
XXinv.temp <- solve(XX.temp)
X.test.new <- matrix(0, ncol = ncol(X.new.new), nrow = nrow(X.test) )
X.test.new[, (ncol(X.new.new)-1):(ncol(X.new.new)) ] <- X.test
X.test.temp <- n.all[1] * X.test.new
sd.temp.error.2 <- sqrt(sum( ( Y.hat.2.train - Y.train )^2)/(nrow(Y.train) - 2))*n.all[1]

for(i in 1: (n.test-1)){
  # for I 
  sd.temp.I <- sd.temp.error.2*sqrt(1 + matrix(X.test.temp[2*i, ], nrow = 1) %*% XXinv.temp%*%matrix(X.test.temp[2*i, ], ncol = 1))
  # for R
  sd.temp.R <- sd.temp.error.2*sqrt(1 + matrix(X.test.temp[2*i-1, ], nrow = 1) %*% XXinv.temp%*%matrix(X.test.temp[2*i-1, ], ncol = 1))
  
  print(sd.temp.I)
  Delta.I.hat.CI.ub.2[i]  <-  Delta.I.hat.2[i] + qt(.975, df = nrow(Y.train)-2)*sd.temp.I
  Delta.I.hat.CI.lb.2[i]  <-  Delta.I.hat.2[i] - qt(.975, df = nrow(Y.train)-2)*sd.temp.I
  Delta.R.hat.CI.ub.2[i]  <-  Delta.R.hat.2[i] + qt(.975, df = nrow(Y.train)-2)*sd.temp.R
  Delta.R.hat.CI.lb.2[i]  <-  Delta.R.hat.2[i] - qt(.975, df = nrow(Y.train)-2)*sd.temp.R
  
}
cbind(Delta.I, Delta.I.hat.2, Delta.I.hat.CI.lb.2, Delta.I.hat.CI.ub.2)
cbind(Delta.R, Delta.R.hat.2, Delta.R.hat.CI.lb.2, Delta.R.hat.CI.ub.2)


filename <- paste0("Delta_I_PI_", county.lowernames[1], "_2.pdf")
pdf(filename, width=11, height=8.5)
par(mar = c(4, 5, 1.5, 1))
plot(multi_full$date[(T-n.test+1):(T-1)], Delta.I , col='1', ylim=c(min(0, Delta.I.hat.CI.lb.2), max(Delta.I, Delta.I.hat.CI.ub.2)), lty=1, type="l", lwd = 3,
     ylab = expression(paste(Delta, "I(t)")), xlab= 'Date', cex.lab=2 , cex.axis=2)
lines(multi_full$date[(T-n.test+1):(T-1)], Delta.I.hat.2, col='2', lty=1, type="l", lwd = 3)
lines(multi_full$date[(T-n.test+1):(T-1)], Delta.I.hat.CI.ub.2, col='3', lty = 2, type="l", lwd = 3)
lines(multi_full$date[(T-n.test+1):(T-1)], Delta.I.hat.CI.lb.2, col='3', lty = 2, type="l", lwd = 3)
dev.off()


filename <- paste0("Delta_R_PI_", county.lowernames[1] ,"_2.pdf")
pdf(filename, width=11, height=8.5)
par(mar = c(4., 5, 1.5, 1))
plot(multi_full$date[(T-n.test+1):(T-1)], Delta.R , col='1', ylim = c(min(0, Delta.R.hat.CI.lb.2), max(Delta.R, Delta.R.hat.CI.ub.2)), lty=1, type="l", lwd = 3,
     ylab = expression(paste(Delta, "R(t)")), xlab= 'Date', cex.lab=2, cex.axis=2)
lines(multi_full$date[(T-n.test+1):(T-1)], Delta.R.hat.2, col='2', lty=1, type="l", lwd = 3)
lines(multi_full$date[(T-n.test+1):(T-1)], Delta.R.hat.CI.ub.2, col='3', lty = 2, type="l", lwd = 3)
lines(multi_full$date[(T-n.test+1):(T-1)], Delta.R.hat.CI.lb.2, col='3', lty = 2, type="l", lwd = 3)
dev.off()





filename <- paste0("Delta_I_PI_", county.lowernames[1], "_2_full.pdf")
pdf(filename, width=11, height=8.5)
par(mar = c(4, 5, 1.5, 1))
plot(multi_full$date[1:(T-1)], Delta.I.full , col='1', ylim=c(min(0, Delta.I.hat.CI.lb.2), max(Delta.I.full, Delta.I.hat.CI.ub.2)), lty=1, type="l", lwd = 3,
     ylab = expression(paste(Delta, "I(t)")), xlab= 'Date', cex.lab=2 , cex.axis=2)
lines(multi_full$date[(T-n.test+1):(T-1)], Delta.I.hat.2, col='2', lty=1, type="l", lwd = 3)
lines(multi_full$date[(T-n.test+1):(T-1)], Delta.I.hat.CI.ub.2, col='3', lty = 2, type="l", lwd = 3)
lines(multi_full$date[(T-n.test+1):(T-1)], Delta.I.hat.CI.lb.2, col='3', lty = 2, type="l", lwd = 3)
dev.off()


filename <- paste0("Delta_R_PI_", county.lowernames[1] ,"_2_full.pdf")
pdf(filename, width=11, height=8.5)
par(mar = c(4., 5, 1.5, 1))
plot(multi_full$date[1:(T-1)], Delta.R.full , col='1', ylim=c(min(0, Delta.R.hat.CI.lb.2), max(Delta.R.full, Delta.R.hat.CI.ub.2 )), lty=1, type="l", lwd = 3,
     ylab = expression(paste(Delta, "R(t)")), xlab= 'Date', cex.lab=2, cex.axis=2)
lines(multi_full$date[(T-n.test+1):(T-1)], Delta.R.hat.2, col='2', lty=1, type="l", lwd = 3)
lines(multi_full$date[(T-n.test+1):(T-1)], Delta.R.hat.CI.ub.2, col='3', lty = 2, type="l", lwd = 3)
lines(multi_full$date[(T-n.test+1):(T-1)], Delta.R.hat.CI.lb.2, col='3', lty = 2, type="l", lwd = 3)
dev.off()




R.hat.2.new <- rep(0, n.test)
R.hat.2.new[1] <- R.rate.all[T - n.test + 1, 1]
for(i in 2:n.test){
  R.hat.2.new[i] <- R.rate.all[T - n.test + (i-1), 1] + Y.hat.2.new[(i-2)*2+1]
}
R.hat.2.new <- R.hat.2.new*n.all[1]

I.hat.2.new <- rep(0, n.test)
I.hat.2.new[1] <- I.rate.all[T - n.test + 1, 1]
for(i in 2:n.test){
  I.hat.2.new[i] <-  I.rate.all[T - n.test + (i-1), 1] + Y.hat.2.new[(i-2)*2+2]
}
I.hat.2.new <- I.hat.2.new*n.all[1]

I.test <- I[(T - n.test + 1):T]
R.test <- R[(T - n.test + 1):T]


MSPE_2_new <- mean((c(I.hat.2.new[-1], R.hat.2.new[-1]) - c(I.test [-1], R.test[-1]))^2)
print(round(MSPE_2_new))
MSPE_2_new_I <- mean((I.hat.2.new[-1] - I.test[-1])^2)
print(round(MSPE_2_new_I))
MSPE_2_new_R <- mean((R.hat.2.new[-1] - R.test[-1])^2)
print(round(MSPE_2_new_R))

MRPE_2_new <- mean(  abs ( (     c(R.hat.2.new[-1], I.hat.2.new[-1]) - c(R.test[-1], I.test[-1])     )  /c(R.test[-1], I.test[-1])  )[c(R.test[-1], I.test[-1]) > 0]  )
print(round(MRPE_2_new, 4))
MRPE_2_new_I <- mean(  abs ( (     c(I.hat.2.new[-1]) - c(I.test[-1])     )  /c(I.test[-1])  )[c(I.test[-1]) > 0]  )
print(round(MRPE_2_new_I, 4))
MRPE_2_new_R <- mean(  abs ( (     c(R.hat.2.new[-1]) - c(R.test[-1])     )  /c(R.test[-1])  )[c(R.test[-1]) > 0]  )
print(round(MRPE_2_new_R, 4))


MRPE_2_new_Delta <- mean(  abs ( (  c(Y.hat.2.new) - c(Y.test)  )  /c(Y.test)  )[c(Y.test) > 0]  )
print(round(MRPE_2_new_Delta, 4))
MRPE_2_new_I_Delta <- mean(  abs ( (c(Y.hat.2.new[seq(2, nrow(Y.test), 2)]) - c(Y.test[seq(2, nrow(Y.test), 2)])  )  /c(Y.test[seq(2, nrow(Y.test), 2)])  )[c(Y.test[seq(2, nrow(Y.test), 2)]) > 0]  )
print(round(MRPE_2_new_I_Delta, 4))
MRPE_2_new_R_Delta <- mean(  abs ( (c(Y.hat.2.new[seq(1, nrow(Y.test), 2)]) - c(Y.test[seq(1, nrow(Y.test), 2)])  )  /c(Y.test[seq(1, nrow(Y.test), 2)])  )[c(Y.test[seq(1, nrow(Y.test), 2)]) > 0]  )
print(round(MRPE_2_new_R_Delta, 4))


#################################################
######### Model 3  ##############################
#################################################
##################################
# equal weight
##################################
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

est.3.1 <- lm(Y.train[-c(1:2), ] ~ X.new.1[-c(1:2), ]  - 1)
summary(est.3.1 )

beta.t.est <- est.3.1$coefficients[m*p.x - 1]
gamma.t.est <- est.3.1$coefficients[m*p.x ]
alpha.t.est <- est.3.1$coefficients[m*p.x + 1]
Y.hat.3.1.new <- X.test%*%c(beta.t.est, gamma.t.est) + neighbor.weighted_1.test%*%alpha.t.est



R.hat.3.1.new <- rep(0, n.test)
R.hat.3.1.new[1] <- R.rate.all[T - n.test + 1, 1]
for(i in 2:n.test){
  R.hat.3.1.new[i] <- R.rate.all[T - n.test + (i-1), 1] + Y.hat.3.1.new[(i-2)*2+1]
}
R.hat.3.1.new <- R.hat.3.1.new*n.all[1]

I.hat.3.1.new <- rep(0, n.test)
I.hat.3.1.new[1] <- I.rate.all[T - n.test + 1, 1]
for(i in 2:n.test){
  I.hat.3.1.new[i] <-  I.rate.all[T - n.test + (i-1), 1] + Y.hat.3.1.new[(i-2)*2+2]
}
I.hat.3.1.new <- I.hat.3.1.new*n.all[1]

I.test <- I[(T - n.test + 1):T]
R.test <- R[(T - n.test + 1):T]


MSPE_3.1_new <- mean((c(I.hat.3.1.new[-1], R.hat.3.1.new[-1]) - c(I.test [-1], R.test[-1]))^2)
print(round(MSPE_3.1_new))
MSPE_3.1_new_I <- mean((I.hat.3.1.new[-1] - I.test[-1])^2)
print(round(MSPE_3.1_new_I))
MSPE_3.1_new_R <- mean((R.hat.3.1.new[-1] - R.test[-1])^2)
print(round(MSPE_3.1_new_R))

MRPE_3.1_new <- mean(  abs ( (     c(R.hat.3.1.new[-1], I.hat.3.1.new[-1]) - c(R.test[-1], I.test[-1])     )  /c(R.test[-1], I.test[-1])  )[c(R.test[-1], I.test[-1]) > 0]  )
print(round(MRPE_3.1_new, 4))
MRPE_3.1_new_I <- mean(  abs ( (     c(I.hat.3.1.new[-1]) - c(I.test[-1])     )  /c(I.test[-1])  )[c(I.test[-1]) > 0]  )
print(round(MRPE_3.1_new_I, 4))
MRPE_3.1_new_R <- mean(  abs ( (     c(R.hat.3.1.new[-1]) - c(R.test[-1])     )  /c(R.test[-1])  )[c(R.test[-1]) > 0]  )
print(round(MRPE_3.1_new_R, 4))



MRPE_3.1_new_Delta <- mean(  abs ( (  c(Y.hat.3.1.new) - c(Y.test)  )  /c(Y.test)  )[c(Y.test) > 0]  )
print(round(MRPE_3.1_new_Delta, 4))
MRPE_3.1_new_I_Delta <- mean(  abs ( (c(Y.hat.3.1.new[seq(2, nrow(Y.test), 2)]) - c(Y.test[seq(2, nrow(Y.test), 2)])  )  /c(Y.test[seq(2, nrow(Y.test), 2)])  )[c(Y.test[seq(2, nrow(Y.test), 2)]) > 0]  )
print(round(MRPE_3.1_new_I_Delta, 4))
MRPE_3.1_new_R_Delta <- mean(  abs ( (c(Y.hat.3.1.new[seq(1, nrow(Y.test), 2)]) - c(Y.test[seq(1, nrow(Y.test), 2)])  )  /c(Y.test[seq(1, nrow(Y.test), 2)])  )[c(Y.test[seq(1, nrow(Y.test), 2)]) > 0]  )
print(round(MRPE_3.1_new_R_Delta, 4))



##################################
#  distance weight
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


est.3.2 <- lm(Y.train[-c(1:2), ] ~ X.new.2[-c(1:2), ]  - 1)
summary(est.3.2 )

beta.t.est <- est.3.2$coefficients[m*p.x - 1]
gamma.t.est <- est.3.2$coefficients[m*p.x ]
alpha.t.est <- est.3.2$coefficients[m*p.x + 1]
Y.hat.3.2.new <- X.test%*%c(beta.t.est, gamma.t.est) + neighbor.weighted_2.test%*%alpha.t.est


R.hat.3.2.new <- rep(0, n.test)
R.hat.3.2.new[1] <- R.rate.all[T - n.test + 1, 1]
for(i in 2:n.test){
  R.hat.3.2.new[i] <- R.rate.all[T - n.test + (i-1), 1] + Y.hat.3.2.new[(i-2)*2+1]
}
R.hat.3.2.new <- R.hat.3.2.new*n.all[1]

I.hat.3.2.new <- rep(0, n.test)
I.hat.3.2.new[1] <- I.rate.all[T - n.test + 1, 1]
for(i in 2:n.test){
  I.hat.3.2.new[i] <-  I.rate.all[T - n.test + (i-1), 1] + Y.hat.3.2.new[(i-2)*2+2]
}
I.hat.3.2.new <- I.hat.3.2.new*n.all[1]

I.test <- I[(T - n.test + 1):T]
R.test <- R[(T - n.test + 1):T]


MSPE_3.2_new <- mean((c(I.hat.3.2.new[-1], R.hat.3.2.new[-1]) - c(I.test [-1], R.test[-1]))^2)
print(round(MSPE_3.2_new))
MSPE_3.2_new_I <- mean((I.hat.3.2.new[-1] - I.test[-1])^2)
print(round(MSPE_3.2_new_I))
MSPE_3.2_new_R <- mean((R.hat.3.2.new[-1] - R.test[-1])^2)
print(round(MSPE_3.2_new_R))

MRPE_3.2_new <- mean(  abs ( (     c(R.hat.3.2.new[-1], I.hat.3.2.new[-1]) - c(R.test[-1], I.test[-1])     )  /c(R.test[-1], I.test[-1])  )[c(R.test[-1], I.test[-1]) > 0]  )
print(round(MRPE_3.2_new, 4))
MRPE_3.2_new_I <- mean(  abs ( (     c(I.hat.3.2.new[-1]) - c(I.test[-1])     )  /c(I.test[-1])  )[c(I.test[-1]) > 0]  )
print(round(MRPE_3.2_new_I, 4))
MRPE_3.2_new_R <- mean(  abs ( (     c(R.hat.3.2.new[-1]) - c(R.test[-1])     )  /c(R.test[-1])  )[c(R.test[-1]) > 0]  )
print(round(MRPE_3.2_new_R, 4))

MRPE_3.2_new_Delta <- mean(  abs ( (  c(Y.hat.3.2.new) - c(Y.test)  )  /c(Y.test)  )[c(Y.test) > 0]  )
print(round(MRPE_3.2_new_Delta, 4))
MRPE_3.2_new_I_Delta <- mean(  abs ( (c(Y.hat.3.2.new[seq(2, nrow(Y.test), 2)]) - c(Y.test[seq(2, nrow(Y.test), 2)])  )  /c(Y.test[seq(2, nrow(Y.test), 2)])  )[c(Y.test[seq(2, nrow(Y.test), 2)]) > 0]  )
print(round(MRPE_3.2_new_I_Delta, 4))
MRPE_3.2_new_R_Delta <- mean(  abs ( (c(Y.hat.3.2.new[seq(1, nrow(Y.test), 2)]) - c(Y.test[seq(1, nrow(Y.test), 2)])  )  /c(Y.test[seq(1, nrow(Y.test), 2)])  )[c(Y.test[seq(1, nrow(Y.test), 2)]) > 0]  )
print(round(MRPE_3.2_new_R_Delta, 4))



###############
# model 3.3  similarity
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
R.all <- floor((1+(5.5))*deaths.all);
#I: the number of people infected at time t
I.all <- cases.all - R.all;

#S: the number of susceptible people
n.all.matrix <- matrix(rep(n.all,T), ncol = n.domains, byrow = TRUE)
S.all <- n.all.matrix - I.all - R.all;

#the fraction of S, I and R
S.rate.all <- sapply(1:n.domains, function(jjj) S.all[,jjj]/n.all[jjj])
I.rate.all <- sapply(1:n.domains, function(jjj) I.all[,jjj]/n.all[jjj])
R.rate.all <- sapply(1:n.domains, function(jjj) R.all[,jjj]/n.all[jjj])


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
S.rate.all.full <- S.rate.all
I.rate.all.full <- I.rate.all
R.rate.all.full <- R.rate.all

n.domains <- length(county.names)
n.all <- n.all[county.index]
S.rate.all <- S.rate.all[, county.index]
I.rate.all <- I.rate.all[, county.index]
R.rate.all <- R.rate.all[, county.index]

date.region <- multi_full$date
I <- I.all[, 1]
R <- R.all[, 1]


##################################
#  similarity weight
##################################
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


est.3.3 <- lm(Y.train[-c(1:2), ] ~ X.new.3[-c(1:2), ]  - 1)
#fitted data in tranining set
Y.hat.3.3 <- c(0, 0, est.3.3 $fitted.values)
summary(est.3.3 )

beta.t.est <- est.3.3$coefficients[m*p.x - 1]
gamma.t.est <- est.3.3$coefficients[m*p.x ]
alpha.t.est <- est.3.3$coefficients[m*p.x + 1]
Y.hat.3.3.new <- X.test%*%c(beta.t.est, gamma.t.est) + neighbor.weighted_3.test%*%alpha.t.est


R.hat.3.3.new <- rep(0, n.test)
R.hat.3.3.new[1] <- R.rate.all[T - n.test + 1, 1]
for(i in 2:n.test){
  R.hat.3.3.new[i] <- R.rate.all[T - n.test + (i-1), 1] + Y.hat.3.3.new[(i-2)*2+1]
}
R.hat.3.3.new <- R.hat.3.3.new*n.all[1]

I.hat.3.3.new <- rep(0, n.test)
I.hat.3.3.new[1] <- I.rate.all[T - n.test + 1, 1]
for(i in 2:n.test){
  I.hat.3.3.new[i] <-  I.rate.all[T - n.test + (i-1), 1] + Y.hat.3.3.new[(i-2)*2+2]
}
I.hat.3.3.new <- I.hat.3.3.new*n.all[1]

I.test <- I[(T - n.test + 1):T]
R.test <- R[(T - n.test + 1):T]


MSPE_3.3_new <- mean((c(I.hat.3.3.new[-1], R.hat.3.3.new[-1]) - c(I.test [-1], R.test[-1]))^2)
print(round(MSPE_3.3_new))
MSPE_3.3_new_I <- mean((I.hat.3.3.new[-1] - I.test[-1])^2)
print(round(MSPE_3.3_new_I))
MSPE_3.3_new_R <- mean((R.hat.3.3.new[-1] - R.test[-1])^2)
print(round(MSPE_3.3_new_R))

MRPE_3.3_new <- mean(  abs ( (     c(R.hat.3.3.new[-1], I.hat.3.3.new[-1]) - c(R.test[-1], I.test[-1])     )  /c(R.test[-1], I.test[-1])  )[c(R.test[-1], I.test[-1]) > 0]  )
print(round(MRPE_3.3_new, 4))
MRPE_3.3_new_I <- mean(  abs ( (     c(I.hat.3.3.new[-1]) - c(I.test[-1])     )  /c(I.test[-1])  )[c(I.test[-1]) > 0]  )
print(round(MRPE_3.3_new_I, 4))
MRPE_3.3_new_R <- mean(  abs ( (     c(R.hat.3.3.new[-1]) - c(R.test[-1])     )  /c(R.test[-1])  )[c(R.test[-1]) > 0]  )
print(round(MRPE_3.3_new_R, 4))



MRPE_3.3_new_Delta <- mean(  abs ( (  c(Y.hat.3.3.new) - c(Y.test)  )  /c(Y.test)  )[c(Y.test) > 0]  )
print(round(MRPE_3.3_new_Delta, 4))
MRPE_3.3_new_I_Delta <- mean(  abs ( (c(Y.hat.3.3.new[seq(2, nrow(Y.test), 2)]) - c(Y.test[seq(2, nrow(Y.test), 2)])  )  /c(Y.test[seq(2, nrow(Y.test), 2)])  )[c(Y.test[seq(2, nrow(Y.test), 2)]) > 0]  )
print(round(MRPE_3.3_new_I_Delta, 4))
MRPE_3.3_new_R_Delta <- mean(  abs ( (c(Y.hat.3.3.new[seq(1, nrow(Y.test), 2)]) - c(Y.test[seq(1, nrow(Y.test), 2)])  )  /c(Y.test[seq(1, nrow(Y.test), 2)])  )[c(Y.test[seq(1, nrow(Y.test), 2)]) > 0]  )
print(round(MRPE_3.3_new_R_Delta, 4))



## predict value of Delta.R and Delta.I
Delta.R.hat.3 <- rep(0, n.test - 1)
for(i in 2:n.test ){
  Delta.R.hat.3[i - 1] <- Y.hat.3.3.new[(i - 2)*2 + 1]
}
Delta.R.hat.3 <- Delta.R.hat.3*n.all[1]


Delta.I.hat.3 <- rep(0, n.test - 1)
for(i in 2:n.test){
  Delta.I.hat.3[i - 1] <- Y.hat.3.3.new[(i - 2)*2 + 2]
}
Delta.I.hat.3 <- Delta.I.hat.3*n.all[1]




Delta.I.hat.CI.ub.3  <-  Delta.I.hat.3 ; Delta.I.hat.CI.lb.3  <-  Delta.I.hat.3 
Delta.R.hat.CI.ub.3  <-  Delta.R.hat.3 ; Delta.R.hat.CI.lb.3  <-  Delta.R.hat.3

X.temp <- n.all[1] * X.new.3
XX.temp <- t(X.temp) %*% X.temp
XXinv.temp <- solve(XX.temp)
X.test.new <- matrix(0, ncol = ncol(X.new.3), nrow = nrow(X.test) )
X.test.new[, (ncol(X.new.3)-2):(ncol(X.new.3)-1) ] <- X.test
X.test.new[, ncol(X.new.3)] <- neighbor.weighted_3.test
X.test.temp <- n.all[1] * X.test.new



sd.temp.error.3 <- sqrt(sum( ( Y.hat.3.3 - Y.train )^2)/(nrow(Y.train) - 2))*n.all[1]
for(i in 1: (n.test-1)){
  # for I 
  sd.temp.I <- sd.temp.error.3*sqrt(1 + matrix(X.test.temp[2*i, ], nrow = 1) %*% XXinv.temp%*%matrix(X.test.temp[2*i, ], ncol = 1))
  # for R
  sd.temp.R <- sd.temp.error.3*sqrt(1 + matrix(X.test.temp[2*i-1, ], nrow = 1) %*% XXinv.temp%*%matrix(X.test.temp[2*i-1, ], ncol = 1))
  
  print(sd.temp.I)
  Delta.I.hat.CI.ub.3[i]  <-  Delta.I.hat.3[i] + qt(.975, df = nrow(Y.train)-3)*sd.temp.I
  Delta.I.hat.CI.lb.3[i]  <-  Delta.I.hat.3[i] - qt(.975, df = nrow(Y.train)-3)*sd.temp.I
  Delta.R.hat.CI.ub.3[i]  <-  Delta.R.hat.3[i] + qt(.975, df = nrow(Y.train)-3)*sd.temp.R
  Delta.R.hat.CI.lb.3[i]  <-  Delta.R.hat.3[i] - qt(.975, df = nrow(Y.train)-3)*sd.temp.R
  
}
cbind(Delta.I, Delta.I.hat.3, Delta.I.hat.CI.lb.3, Delta.I.hat.CI.ub.3)
cbind(Delta.R, Delta.R.hat.3, Delta.R.hat.CI.lb.3, Delta.R.hat.CI.ub.3)


filename <- paste0("Delta_I_PI_", county.lowernames[1], "_3.pdf")
pdf(filename, width = 11, height = 8.5)
par(mar = c(4, 5, 1.5, 1))
plot(multi_full$date[(T-n.test+1):(T-1)], Delta.I , col = '1', ylim = c(min(0, Delta.I.hat.CI.lb.3), max(Delta.I, Delta.I.hat.CI.ub.3)), lty = 1, type = "l", lwd = 3,
     ylab = expression(paste(Delta, "I(t)")), xlab = 'Date', cex.lab = 2, cex.axis = 2)
lines(multi_full$date[(T-n.test+1):(T-1)], Delta.I.hat.3, col = '2', lty = 1, type = "l", lwd = 3)
lines(multi_full$date[(T-n.test+1):(T-1)], Delta.I.hat.CI.ub.3, col = '3', lty = 2, type = "l", lwd = 3)
lines(multi_full$date[(T-n.test+1):(T-1)], Delta.I.hat.CI.lb.3, col = '3', lty = 2, type = "l", lwd = 3)
dev.off()


filename <- paste0("Delta_R_PI_", county.lowernames[1], "_3.pdf")
pdf(filename, width=11, height=8.5)
par(mar = c(4., 5, 1.5, 1))
plot(multi_full$date[(T-n.test+1):(T-1)], Delta.R, col='1', ylim = c(min(0, Delta.R.hat.CI.lb.3), max(Delta.R, Delta.R.hat.CI.ub.3)), lty=1, type="l", lwd = 3,
     ylab = expression(paste(Delta, "R(t)")), xlab= 'Date', cex.lab=2, cex.axis=2)
lines(multi_full$date[(T-n.test+1):(T-1)], Delta.R.hat.3, col='2', lty=1, type="l", lwd = 3)
lines(multi_full$date[(T-n.test+1):(T-1)], Delta.R.hat.CI.ub.3, col='3', lty = 2, type="l", lwd = 3)
lines(multi_full$date[(T-n.test+1):(T-1)], Delta.R.hat.CI.lb.3, col='3', lty = 2, type="l", lwd = 3)
dev.off()


filename <- paste0("Delta_I_PI_", county.lowernames[1], "_3_full.pdf")
pdf(filename, width=11, height=8.5)
par(mar = c(4, 5, 1.5, 1))
plot(multi_full$date[1:(T-1)], Delta.I.full, col='1', ylim=c(min(0, Delta.I.hat.CI.lb.3), max(Delta.I.full, Delta.I.hat.CI.ub.3)), lty=1, type="l", lwd = 3,
     ylab = expression(paste(Delta, "I(t)")), xlab= 'Date', cex.lab=2, cex.axis=2)
lines(multi_full$date[(T-n.test+1):(T-1)], Delta.I.hat.3, col='2', lty=1, type="l", lwd = 3)
lines(multi_full$date[(T-n.test+1):(T-1)], Delta.I.hat.CI.ub.3, col='3', lty = 2, type="l", lwd = 3)
lines(multi_full$date[(T-n.test+1):(T-1)], Delta.I.hat.CI.lb.3, col='3', lty = 2, type="l", lwd = 3)
dev.off()


filename <- paste0("Delta_R_PI_", county.lowernames[1], "_3_full.pdf")
pdf(filename, width = 11, height = 8.5)
par(mar = c(4., 5, 1.5, 1))
plot(multi_full$date[1:(T-1)], Delta.R.full , col='1', ylim=c(min(0, Delta.R.hat.CI.lb.3), max(Delta.R.full, Delta.R.hat.CI.ub.3 )), lty=1, type="l", lwd = 3,
     ylab = expression(paste(Delta, "R(t)")), xlab= 'Date', cex.lab=2, cex.axis=2)
lines(multi_full$date[(T-n.test+1):(T-1)], Delta.R.hat.3, col='2', lty=1, type="l", lwd = 3)
lines(multi_full$date[(T-n.test+1):(T-1)], Delta.R.hat.CI.ub.3, col='3', lty = 2, type="l", lwd = 3)
lines(multi_full$date[(T-n.test+1):(T-1)], Delta.R.hat.CI.lb.3, col='3', lty = 2, type="l", lwd = 3)
dev.off()


######################################
#####################################
###### model 4 use VAR(p) model
#####################################

Delta.R.hat.train <- rep(0, T - n.test)
Delta.R.train <- rep(0, T - n.test)
for(i in 2: (T - n.test + 1) ){
  Delta.R.hat.train[i - 1] <- Y.hat.3.3[(i - 2)*2 + 1]
  Delta.R.train[i - 1] <- Y.train[(i - 2)*2 + 1]
}
Delta.R.hat.train <- Delta.R.hat.train*n.all[1]
Delta.R.train <- Delta.R.train*n.all[1]


Delta.I.hat.train <- rep(0, T - n.test)
Delta.I.train <- rep(0, T - n.test)
for(i in 2:(T - n.test + 1)){
  Delta.I.hat.train[i - 1] <- Y.hat.3.3[(i - 2)*2 + 2]
  Delta.I.train[i - 1] <- Y.train[(i - 2)*2 + 2]
}
Delta.I.hat.train <- Delta.I.hat.train*n.all[1]
Delta.I.train <- Delta.I.train*n.all[1]



residual_Delta.I.train <- Delta.I.train - Delta.I.hat.train 
residual_Delta.R.train <- Delta.R.train - Delta.R.hat.train 

res <- c()
for(i in 1:n){
  res <- c(res,  Y.train[i] - Y.hat.3.3[i])
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
  
  Y.hat.4.train <- Y.hat.3.3 + c(rep(0, p.est*2),  residual.hat.vec/n.all[1])
  
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
  
  Y.hat.4.train <- Y.hat.3.3 +  residual.hat.vec/n.all[1]
  
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


Y.hat.4.new <- Y.hat.3.3.new +  residual.test.hat.vec/n.all[1]

R.hat.4.new <- rep(0, n.test)
R.hat.4.new[1] <- R.rate.all[T - n.test + 1, 1]
for(i in 2:n.test){
  R.hat.4.new[i] <- R.rate.all[T - n.test + (i-1), 1] + Y.hat.4.new[(i-2)*2+1]
}
R.hat.4.new <- R.hat.4.new*n.all[1]

I.hat.4.new <- rep(0, n.test)
I.hat.4.new[1] <- I.rate.all[T - n.test + 1, 1]
for(i in 2:n.test){
  I.hat.4.new[i] <-  I.rate.all[T - n.test + (i-1), 1] + Y.hat.4.new[(i-2)*2+2]
}
I.hat.4.new <- I.hat.4.new*n.all[1]

I.test <- I[(T - n.test + 1):T]
R.test <- R[(T - n.test + 1):T]


MSPE_4_new <- mean((c(I.hat.4.new[-1], R.hat.4.new[-1]) - c(I.test [-1], R.test[-1]))^2)
print(round(MSPE_4_new))
MSPE_4_new_I <- mean((I.hat.4.new[-1] - I.test[-1])^2)
print(round(MSPE_4_new_I))
MSPE_4_new_R <- mean((R.hat.4.new[-1] - R.test[-1])^2)
print(round(MSPE_4_new_R))

MRPE_4_new <- mean(  abs ( (     c(R.hat.4.new[-1], I.hat.4.new[-1]) - c(R.test[-1], I.test[-1])     )  /c(R.test[-1], I.test[-1])  )[c(R.test[-1], I.test[-1]) > 0]  )
print(round(MRPE_4_new, 4))
MRPE_4_new_I <- mean(  abs ( (     c(I.hat.4.new[-1]) - c(I.test[-1])     )  /c(I.test[-1])  )[c(I.test[-1]) > 0]  )
print(round(MRPE_4_new_I, 4))
MRPE_4_new_R <- mean(  abs ( (     c(R.hat.4.new[-1]) - c(R.test[-1])     )  /c(R.test[-1])  )[c(R.test[-1]) > 0]  )
print(round(MRPE_4_new_R, 4))


MRPE_4_new_Delta <- mean(  abs ( (  c(Y.hat.4.new) - c(Y.test)  )  /c(Y.test)  )[c(Y.test) > 0]  )
print(round(MRPE_4_new_Delta, 4))
MRPE_4_new_I_Delta <- mean(  abs ( (c(Y.hat.4.new[seq(2, nrow(Y.test), 2)]) - c(Y.test[seq(2, nrow(Y.test), 2)])  )  /c(Y.test[seq(2, nrow(Y.test), 2)])  )[c(Y.test[seq(2, nrow(Y.test), 2)]) > 0]  )
print(round(MRPE_4_new_I_Delta, 4))
MRPE_4_new_R_Delta <- mean(  abs ( (c(Y.hat.4.new[seq(1, nrow(Y.test), 2)]) - c(Y.test[seq(1, nrow(Y.test), 2)])  )  /c(Y.test[seq(1, nrow(Y.test), 2)])  )[c(Y.test[seq(1, nrow(Y.test), 2)]) > 0]  )
print(round(MRPE_4_new_R_Delta, 4))



## predict value of Delta.R and Delta.I
Delta.R.hat.4 <- rep(0, n.test - 1)
for(i in 2:n.test ){
  Delta.R.hat.4[i - 1] <- Y.hat.4.new[(i - 2)*2 + 1]
}
Delta.R.hat.4 <- Delta.R.hat.4*n.all[1]


Delta.I.hat.4 <- rep(0, n.test - 1)
for(i in 2:n.test){
  Delta.I.hat.4[i - 1] <- Y.hat.4.new[(i - 2)*2 + 2]
}
Delta.I.hat.4 <- Delta.I.hat.4*n.all[1]




Delta.I.hat.CI.ub.4  <-  Delta.I.hat.4 ; Delta.I.hat.CI.lb.4  <-  Delta.I.hat.4 
Delta.R.hat.CI.ub.4  <-  Delta.R.hat.4 ; Delta.R.hat.CI.lb.4  <-  Delta.R.hat.4

X.temp <- n.all[1] * X.new.3
XX.temp <- t(X.temp) %*% X.temp
XXinv.temp <- solve(XX.temp)
X.test.new <- matrix(0, ncol = ncol(X.new.3), nrow = nrow(X.test) )
X.test.new[, (ncol(X.new.3)-2):(ncol(X.new.3)-1) ] <- X.test
X.test.new[, ncol(X.new.3)] <- neighbor.weighted_3.test
X.test.temp <- n.all[1] * X.test.new



sd.temp.error.4 <- sqrt(sum( ( Y.hat.4.train - Y.train )^2)/(nrow(Y.train) - 2))*n.all[1]
for(i in 1: (n.test-1)){
  # for I 
  sd.temp.I <- sd.temp.error.4*sqrt(1 + matrix(X.test.temp[2*i, ], nrow = 1) %*% XXinv.temp%*%matrix(X.test.temp[2*i, ], ncol = 1))
  # for R
  sd.temp.R <- sd.temp.error.4*sqrt(1 + matrix(X.test.temp[2*i-1, ], nrow = 1) %*% XXinv.temp%*%matrix(X.test.temp[2*i-1, ], ncol = 1))
  
  print(sd.temp.I)
  Delta.I.hat.CI.ub.4[i]  <-  Delta.I.hat.4[i] + qt(.975, df = nrow(Y.train)-3)*sd.temp.I
  Delta.I.hat.CI.lb.4[i]  <-  Delta.I.hat.4[i] - qt(.975, df = nrow(Y.train)-3)*sd.temp.I
  Delta.R.hat.CI.ub.4[i]  <-  Delta.R.hat.4[i] + qt(.975, df = nrow(Y.train)-3)*sd.temp.R
  Delta.R.hat.CI.lb.4[i]  <-  Delta.R.hat.4[i] - qt(.975, df = nrow(Y.train)-3)*sd.temp.R
  
}
cbind(Delta.I, Delta.I.hat.4, Delta.I.hat.CI.lb.4, Delta.I.hat.CI.ub.4)
cbind(Delta.R, Delta.R.hat.4, Delta.R.hat.CI.lb.4, Delta.R.hat.CI.ub.4)


filename <- paste0("Delta_I_PI_", county.lowernames[1], "_4.pdf")
pdf(filename, width = 11, height = 8.5)
par(mar = c(4, 5, 1.5, 1))
plot(multi_full$date[(T-n.test+1):(T-1)], Delta.I , col = '1', ylim = c(min(0, Delta.I.hat.CI.lb.4), max(Delta.I, Delta.I.hat.CI.ub.4)), lty = 1, type = "l", lwd = 3,
     ylab = expression(paste(Delta, "I(t)")), xlab = 'Date', cex.lab = 2, cex.axis = 2)
lines(multi_full$date[(T-n.test+1):(T-1)], Delta.I.hat.4, col = '2', lty = 1, type = "l", lwd = 3)
lines(multi_full$date[(T-n.test+1):(T-1)], Delta.I.hat.CI.ub.4, col = '3', lty = 2, type = "l", lwd = 3)
lines(multi_full$date[(T-n.test+1):(T-1)], Delta.I.hat.CI.lb.4, col = '3', lty = 2, type = "l", lwd = 3)
dev.off()


filename <- paste0("Delta_R_PI_", county.lowernames[1], "_4.pdf")
pdf(filename, width=11, height=8.5)
par(mar = c(4., 5, 1.5, 1))
plot(multi_full$date[(T-n.test+1):(T-1)], Delta.R, col='1', ylim = c(min(0, Delta.R.hat.CI.lb.4), max(Delta.R, Delta.R.hat.CI.ub.4)), lty=1, type="l", lwd = 3,
     ylab = expression(paste(Delta, "R(t)")), xlab= 'Date', cex.lab=2, cex.axis=2)
lines(multi_full$date[(T-n.test+1):(T-1)], Delta.R.hat.4, col='2', lty=1, type="l", lwd = 3)
lines(multi_full$date[(T-n.test+1):(T-1)], Delta.R.hat.CI.ub.4, col='3', lty = 2, type="l", lwd = 3)
lines(multi_full$date[(T-n.test+1):(T-1)], Delta.R.hat.CI.lb.4, col='3', lty = 2, type="l", lwd = 3)
dev.off()


filename <- paste0("Delta_I_PI_", county.lowernames[1], "_4_full.pdf")
pdf(filename, width = 11, height = 8.5)
par(mar = c(4, 5, 1.5, 1))
plot(multi_full$date[1:(T-1)], Delta.I.full, col='1', ylim=c(min(0, Delta.I.hat.CI.lb.4), max(Delta.I.full, Delta.I.hat.CI.ub.4)), lty=1, type="l", lwd = 3,
     ylab = expression(paste(Delta, "I(t)")), xlab= 'Date', cex.lab=2, cex.axis=2)
lines(multi_full$date[(T-n.test+1):(T-1)], Delta.I.hat.4, col='2', lty=1, type="l", lwd = 3)
lines(multi_full$date[(T-n.test+1):(T-1)], Delta.I.hat.CI.ub.4, col='3', lty = 2, type="l", lwd = 3)
lines(multi_full$date[(T-n.test+1):(T-1)], Delta.I.hat.CI.lb.4, col='3', lty = 2, type="l", lwd = 3)
dev.off()


filename <- paste0("Delta_R_PI_", county.lowernames[1], "_4_full.pdf")
pdf(filename, width = 11, height = 8.5)
par(mar = c(4., 5, 1.5, 1))
plot(multi_full$date[1:(T-1)], Delta.R.full , col='1', ylim=c(min(0, Delta.R.hat.CI.lb.4), max(Delta.R.full, Delta.R.hat.CI.ub.4 )), lty=1, type="l", lwd = 3,
     ylab = expression(paste(Delta, "R(t)")), xlab= 'Date', cex.lab=2, cex.axis=2)
lines(multi_full$date[(T-n.test+1):(T-1)], Delta.R.hat.4, col='2', lty=1, type="l", lwd = 3)
lines(multi_full$date[(T-n.test+1):(T-1)], Delta.R.hat.CI.ub.4, col='3', lty = 2, type="l", lwd = 3)
lines(multi_full$date[(T-n.test+1):(T-1)], Delta.R.hat.CI.lb.4, col='3', lty = 2, type="l", lwd = 3)
dev.off()




filename <- paste0("Delta_I_PI_", county.lowernames[1], "_all.pdf")
pdf(filename, width = 11, height = 8.5)
par(mar = c(4, 5, 1.5, 1))
ylim_min <- min(0, Delta.I, Delta.I.hat, Delta.I.hat.2, Delta.I.hat.3, Delta.I.hat.4)
ylim_max <- max(0, Delta.I, Delta.I.hat, Delta.I.hat.2, Delta.I.hat.3, Delta.I.hat.4)
plot(multi_full$date[(T-n.test+1):(T-1)], Delta.I , col = '1', ylim =c(ylim_min, ylim_max), lty = 1, type = "l", lwd = 3,
     ylab = expression(paste(Delta, "I(t)")), xlab = 'Date', cex.lab = 2, cex.axis = 2)
lines(multi_full$date[(T-n.test+1):(T-1)], Delta.I.hat, col = 'red', lty = 1, type = "b", lwd = 3)
lines(multi_full$date[(T-n.test+1):(T-1)], Delta.I.hat.2, col = 'orange', lty = 1, type = "b", lwd = 3)
lines(multi_full$date[(T-n.test+1):(T-1)], Delta.I.hat.3, col = 'green', lty = 1, type = "b", lwd = 3)
lines(multi_full$date[(T-n.test+1):(T-1)], Delta.I.hat.4, col = 'blue', lty = 1, type = "b", lwd = 3)
dev.off()


filename <- paste0("Delta_R_PI_", county.lowernames[1], "_all.pdf")
pdf(filename, width=11, height=8.5)
par(mar = c(4., 5, 1.5, 1))
ylim_min <- min(0, Delta.R, Delta.R.hat, Delta.R.hat.2, Delta.R.hat.3, Delta.R.hat.4)
ylim_max <- max(0, Delta.R, Delta.R.hat, Delta.R.hat.2, Delta.R.hat.3, Delta.R.hat.4)
plot(multi_full$date[(T-n.test+1):(T-1)], Delta.R, col='1', ylim = c(ylim_min, ylim_max), lty=1, type="l", lwd = 3,
     ylab = expression(paste(Delta, "R(t)")), xlab= 'Date', cex.lab=2, cex.axis=2)
lines(multi_full$date[(T-n.test+1):(T-1)], Delta.R.hat, col='red', lty =1, type="b", lwd = 3)
lines(multi_full$date[(T-n.test+1):(T-1)], Delta.R.hat.2, col='orange', lty=1, type= "b", lwd = 3)
lines(multi_full$date[(T-n.test+1):(T-1)], Delta.R.hat.3, col='green', lty=1, type="b", lwd = 3)
lines(multi_full$date[(T-n.test+1):(T-1)], Delta.R.hat.4, col='blue', lty=1, type="b", lwd = 3)
# legend(as.Date(multi_full$date[(T-n.test+1)]), ylim_min*1/4, legend=c("Observed", "Model 1", "Model 2", "Model 3", "Model 4" ),
#        col=1:5,  bty = "n", lwd = 3, cex=1.5, pt.cex = 1, bg="transparent",
#        seg.len=1.5, y.intersp=1 , x.intersp=1)
dev.off()

###############
# model 3.4  similarity
#################################################
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
# matrix.combined[ seq(1, rows.combined, 2),] <- as.matrix(unname(R.rate.all))[-1,]-as.matrix(unname(R.rate.all))[-T,]
# matrix.combined[ seq(2, rows.combined, 2),] <- as.matrix(unname(I.rate.all))[-1,]-as.matrix(unname(I.rate.all))[-T,]
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




est.3.4 <- lm(Y.train[-c(1:2), ] ~ X.new.4[-c(1:2), ]  - 1)
summary(est.3.4 )

beta.t.est <- est.3.4$coefficients[m*p.x - 1]
gamma.t.est <- est.3.4$coefficients[m*p.x ]
alpha.t.est <- est.3.4$coefficients[m*p.x + 1]
Y.hat.3.4.new <- X.test%*%c(beta.t.est, gamma.t.est) + neighbor.weighted_4.test%*%alpha.t.est


R.hat.3.4.new <- rep(0, n.test)
R.hat.3.4.new[1] <- R.rate.all[T - n.test + 1, 1]
for(i in 2:n.test){
  R.hat.3.4.new[i] <- R.rate.all[T - n.test + (i-1), 1] + Y.hat.3.4.new[(i-2)*2+1]
}
R.hat.3.4.new <- R.hat.3.4.new*n.all[1]

I.hat.3.4.new <- rep(0, n.test)
I.hat.3.4.new[1] <- I.rate.all[T - n.test + 1, 1]
for(i in 2:n.test){
  I.hat.3.4.new[i] <-  I.rate.all[T - n.test + (i-1), 1] + Y.hat.3.4.new[(i-2)*2+2]
}
I.hat.3.4.new <- I.hat.3.4.new*n.all[1]

I.test <- I[(T - n.test + 1):T]
R.test <- R[(T - n.test + 1):T]


MSPE_3.4_new <- mean((c(I.hat.3.4.new[-1], R.hat.3.4.new[-1]) - c(I.test [-1], R.test[-1]))^2)
print(round(MSPE_3.4_new))
MSPE_3.4_new_I <- mean((I.hat.3.4.new[-1] - I.test[-1])^2)
print(round(MSPE_3.4_new_I))
MSPE_3.4_new_R <- mean((R.hat.3.4.new[-1] - R.test[-1])^2)
print(round(MSPE_3.4_new_R))

MRPE_3.4_new <- mean(  abs ( (     c(R.hat.3.4.new[-1], I.hat.3.4.new[-1]) - c(R.test[-1], I.test[-1])     )  /c(R.test[-1], I.test[-1])  )[c(R.test[-1], I.test[-1]) > 0]  )
print(round(MRPE_3.4_new, 4))
MRPE_3.4_new_I <- mean(  abs ( (     c(I.hat.3.4.new[-1]) - c(I.test[-1])     )  /c(I.test[-1])  )[c(I.test[-1]) > 0]  )
print(round(MRPE_3.4_new_I, 4))
MRPE_3.4_new_R <- mean(  abs ( (     c(R.hat.3.4.new[-1]) - c(R.test[-1])     )  /c(R.test[-1])  )[c(R.test[-1]) > 0]  )
print(round(MRPE_3.4_new_R, 4))

MRPE_3.4_new_Delta <- mean(  abs ( (  c(Y.hat.3.4.new) - c(Y.test)  )  /c(Y.test)  )[c(Y.test) > 0]  )
print(round(MRPE_3.4_new_Delta, 4))
MRPE_3.4_new_I_Delta <- mean(  abs ( (c(Y.hat.3.4.new[seq(2, nrow(Y.test), 2)]) - c(Y.test[seq(2, nrow(Y.test), 2)])  )  /c(Y.test[seq(2, nrow(Y.test), 2)])  )[c(Y.test[seq(2, nrow(Y.test), 2)]) > 0]  )
print(round(MRPE_3.4_new_I_Delta, 4))
MRPE_3.4_new_R_Delta <- mean(  abs ( (c(Y.hat.3.4.new[seq(1, nrow(Y.test), 2)]) - c(Y.test[seq(1, nrow(Y.test), 2)])  )  /c(Y.test[seq(1, nrow(Y.test), 2)])  )[c(Y.test[seq(1, nrow(Y.test), 2)]) > 0]  )
print(round(MRPE_3.4_new_R_Delta, 4))

#############################################
filename <- paste0(county.lowernames[1], "_domain_prediction.RData")
save.image(filename)



# MRPE_1
MRPE_1_new_I
MRPE_1_new_R

MRPE_2_new_I
MRPE_2_new_R

MRPE_3.1_new_I
MRPE_3.1_new_R

MRPE_3.2_new_I
MRPE_3.2_new_R

MRPE_3.3_new_I
MRPE_3.3_new_R

MRPE_4_new_I
MRPE_4_new_R

MRPE_3.4_new_I
MRPE_3.4_new_R




library("xtable")
countiesnames <- c("new york city", "king", "miami-dade")
countiesnames <- c("charleston", "greenville" ,"richland", "horry") 
countiesnames <- c( "orleans", "jefferson")
MPE_res <- c()
for(i in 1:length(countiesnames)){
  filename <- paste0(countiesnames[i],"_domain_prediction.RData")
  load(filename)
  MPE_res <- cbind(MPE_res,  round(c(MRPE_1_new_I, MRPE_2_new_I, MRPE_3.1_new_I, MRPE_3.2_new_I, MRPE_3.3_new_I, MRPE_3.4_new_I, MRPE_4_new_I ),4), 
                   round(c(MRPE_1_new_R, MRPE_2_new_R, MRPE_3.1_new_R, MRPE_3.2_new_R, MRPE_3.3_new_R, MRPE_3.4_new_R, MRPE_4_new_R),4))
}
MPE_res <- cbind(c("Model 1", "Model 2", "Model 3.1", "Model 3.2", "Model 3.3", "Model 3.4", "Model 4"), MPE_res)
table.MPE_res <- xtable(MPE_res, hline.after = c(1,2))
print(table.MPE_res,include.rownames = FALSE, include.colnames = FALSE)




countiesnames <- c("new york city", "king", "miami-dade")
countiesnames <- c("charleston", "greenville" ,"richland", "horry") 
countiesnames <- c( "orleans", "jefferson")

alpha_res <- c()
for(i in 1:length(countiesnames)){
  filename <- paste0(countiesnames[i],"_domain_prediction.RData")
  load(filename)
  temp_1 <- c(round(est.3.1$coefficients[length(est.3.1$coefficients)], 4),  
              summary(est.3.1)$coefficients[length(est.3.1$coefficients), 4], 
              paste("(", round(confint(est.3.1), 4)[length(est.3.1$coefficients), 1] ,",", 
                    round(confint(est.3.1), 4)[length(est.3.1$coefficients), 2], ")" ))
  
  temp_2 <- c(round(est.3.2$coefficients[length(est.3.2$coefficients)], 4),  
              summary(est.3.2)$coefficients[length(est.3.2$coefficients), 4], 
              paste("(", round(confint(est.3.2), 4)[length(est.3.2$coefficients), 1] ,",", 
                    round(confint(est.3.2), 4)[length(est.3.2$coefficients), 2], ")" ))
  
  temp_3 <- c(round(est.3.3$coefficients[length(est.3.3$coefficients)], 4),  
              summary(est.3.3)$coefficients[length(est.3.3$coefficients), 4], 
              paste("(", round(confint(est.3.3), 4)[length(est.3.3$coefficients), 1] ,",", 
                    round(confint(est.3.3), 4)[length(est.3.3$coefficients), 2], ")" ))
  
  temp_4 <- c(round(est.3.4$coefficients[length(est.3.4$coefficients)], 4),  
              summary(est.3.4)$coefficients[length(est.3.4$coefficients), 4], 
              paste("(", round(confint(est.3.4), 4)[length(est.3.4$coefficients), 1] ,",", 
                    round(confint(est.3.4), 4)[length(est.3.4$coefficients), 2], ")" ))
  alpha_res <- rbind(alpha_res,temp_1, temp_2, temp_3, temp_4)
  
}
alpha_res <- cbind(rep(c( "Model 3.1","Model 3.2", "Model 3.3","Model 3.4" ), length(countiesnames) ), alpha_res)
alpha_res <- cbind(c("\\multirow{ 4}{*}{NYC}","","","","\\multirow{ 4}{*}{King}", "","","", "\\multirow{ 4}{*}{Miami-Dade}", "","","" ), 
                   alpha_res)
alpha_res <- rbind(c("","model", "estimate", "p-value",  "confidence interval"), alpha_res)
table.alpha_res <- xtable(alpha_res, hline.after = c(1, 2))
print(table.alpha_res,include.rownames = FALSE, include.colnames = FALSE ,hline.after = c(1,1), sanitize.text.function=identity)




library("xtable")
countiesnames <- c("new york city", "king", "miami-dade")
countiesnames <- c("charleston", "greenville" ,"richland", "horry") 
countiesnames <- c( "orleans", "jefferson")
for(i in 1:length(countiesnames)){
  print(countiesnames[i])
  filename <- paste0(countiesnames[i],"_domain_prediction.RData")
  load(filename)
  print(temp.var$cp.final)
}









