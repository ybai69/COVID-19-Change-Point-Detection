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
library("zoo")
library("vars")
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
# data.counties <- data.counties[as.Date(data.counties$date) <= as.Date('2020-12-23'),]
# write.csv(data.counties,'counties-12-23.csv', row.names = FALSE)
data.counties <- read.csv("counties-12-23.csv", header = TRUE)
data.counties <- data.counties[as.Date(data.counties$date) <= as.Date('2020-11-30'),]
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
# phase 1 reopen date
# Date.2 <- '2020-05-04'
# phase 2 reopen date
Date.2 <- '2020-06-05'

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



#######################################################
# The NYC contains five counties
special <- c("New York", "Bronx", "Kings", "Queens", "Richmond")
if( type == "city"){
  if( city.name == "New York City"){
    county.name <- special
  }else{
    print("change special variable!")
  }
}
print("first updated date:"); print(min(data.counties$date))
print("last updated date:"); print(max(data.counties$date))


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

counties.distance.100[counties.distance.100$county1 == region.fips[1], ]
neighbor.fips <- counties.distance.100[(counties.distance.100$county1 == region.fips[1]) & (counties.distance.100$mi_to_county < miles.distance) , "county2"]
if(length(neighbor.fips) > max.neighbor){
  neighbor.fips <- neighbor.fips[1:max.neighbor]
}
fips_info(neighbor.fips)
state.names <- fips_info(neighbor.fips)$full
state.names <- c(state.name, state.names)
neighbor.names <- fips_info(neighbor.fips)$county 
neighbor.names.short <- gsub(" Parish", "", neighbor.names)
neighbor.names.short <- gsub(" County", "", neighbor.names.short)
county.names <- c(county.name[1], neighbor.names.short)
county.lowernames <- tolower(county.names)

#n: population 
county.names.full <- paste0(county.names, " County")
n.all <- rep(0, (length(county.names) - length(county.name) + 1) )
idx <- 0
for(i in 1:length(county.names)){
  if(state.names[i] == "Louisiana"){
      county.names.full[i] <- paste0(county.names[i], " Parish")
  }else{
    county.names.full[i] <- paste0(county.names[i], " County")
  }
  print(county.names.full[i])
  if(county.names[i] %in% county.name){
    print(i)
    print("inside city")
    idx <- idx + 1
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
multi_full <- multi_full[multi_full[, 2] > 0, ]

##################################################
cases.all <- multi_full[, seq(2, ncol(multi_full), 2)]
deaths.all<- multi_full[, seq(3, ncol(multi_full), 2)]

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
# S.rate.all.obs <- S.rate.all


date.region <- multi_full$date
I.obs <- I.all[, 1]
R.obs <- R.all[, 1]
cols <- brewer.pal(n.domains, "Spectral")

# filename <- paste0("I_rate_", county.lowernames[1], "_all.pdf")
# pdf(filename, width = 11, height = 8.5)
par(mar = c(4., 4.5, 1.5, 1))
plot(multi_full$date, I.rate.all[,1] ,col=cols[1],lty=1,type="l",lwd = 3, ylim = c(0,max(I.rate.all)),
     ylab ='Fraction of Infected Cases', xlab= 'Date',cex.lab=2 , cex.axis=2)
for(i in 2:length(county.names)){
  lines(multi_full$date, I.rate.all[,i] ,col=cols[i],lty=1,type="l",lwd = 3)
}
legend(multi_full$date[1],max(I.rate.all) , legend=c(county.names),
       col=cols,bg="transparent",bty = "n",
       lwd = 3, cex=1.5, pt.cex = 1, seg.len=1.5, y.intersp=1 , x.intersp=1)
# dev.off()

# filename <- paste0("R_rate_", county.lowernames[1], "_all.pdf")
# pdf(filename, width = 11, height = 8.5)
par(mar = c(4., 4.5, 1.5, 1))
plot(multi_full$date, R.rate.all[,1] ,col=cols[1],lty=1,type="l",lwd = 3,ylim = c(0,max(R.rate.all)),
     ylab ='Fraction of Recovered Cases', xlab = 'Date',cex.lab = 2 , cex.axis = 2)
for(i in 2:length(county.names)){
  lines(multi_full$date, R.rate.all[,i] ,col = cols[i], lty = 1, type = "l",lwd = 3)
}
legend(multi_full$date[1],max(R.rate.all) , legend = c(county.names),
       col=cols,bg="transparent",bty = "n",
       lwd = 3, cex=1.5, pt.cex = 1, seg.len=1.5, y.intersp = 1 , x.intersp=1)
# dev.off()

filename <- paste0("numbers_", county.lowernames[1], ".pdf")
pdf(filename, width = 11, height = 8.5)
# par(mar = c(4., 4.5, 1.5, 1))
par(mar = c(4.5, 6, 2, 1.5), mgp=c(3.5, 1.2, 0))
plot(date.region, R.obs, col='dark green', ylim=c(min(I.obs, R.obs), max(I.obs, R.obs)),
     lty=1, type = "l", lwd = 3,
     ylab ='Number of Cases', xlab = 'Date', cex.lab = 3, cex.axis = 3)
lines(date.region, I.obs, col='dark orange', lty=1, type="l", lwd = 3)
legend(as.Date(date.region[length(date.region)*4/5]), 1/5*max(I.obs, R.obs),
       legend=c(expression(I(t)), expression(R(t))),
       col=c( 'dark orange', 'dark green'), bg="transparent", bty = "n",
       lwd = 3, cex = 2, pt.cex = 2, seg.len=1.5, y.intersp=1, x.intersp=1)
abline(v = as.Date(Date.1), col = 1,lwd = 2, lty = 3)
abline(v = as.Date(Date.2), col = 1,lwd = 2, lty = 3)
text(x= as.Date(Date.1), y = 3/4*max(I.obs, R.obs), col ="black", labels = as.character(as.Date(Date.1)), cex = 2.5)
text(x= as.Date(Date.2), y = 2/4*max(I.obs, R.obs), col = "black", labels = as.character(as.Date(Date.2)), cex = 2.5)
dev.off()




#### grid search find a  quadratic function 
#################################### 
# a.vals <- c(0.1, 0.2, 0.5, 1, 2, 5, 10)
# a.vals <- c(0.2, 0.25, 0.3)
a.vals <-   c(0.1, 0.15, 0.2, 0.25, 0.3)
t.test <- 13
T.full <- T + t.test
MRPE_1_new.full <- c()
temp.full <- vector("list", length(a.vals));
est.1.full <- vector("list", length(a.vals));
set.seed(123456)
for(idx in 1:length(a.vals)){
  a.val <- a.vals[idx]
  I <-  I.obs
  R <-  R.obs
  for(t in 2:T){
    rate <- 1/(((t)+a.val*T.full )/((1 + a.val )*T.full))^2
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
  
  #################################################
  ######### Model 1  ##############################
  #################################################
  #scaling but not centering data
  Y_std <- sd(Y)
  Y_s <- scale(Y, center = FALSE, scale = apply(Y, 2, sd, na.rm = TRUE))
  
  X_std <- apply(X, MARGIN=2, FUN=sd)
  X_s <- scale(X, center = FALSE, scale = apply(X, 2, sd, na.rm = TRUE))
  
  ############################################################
  ############## change point detection #######################
  ############################################################
  p.x <- ncol(X_s)
  p.y <- ncol(Y_s)
  n <- nrow(X_s)
  tol <- 10^(-4); # tolerance 
  max.iteration <- 200; # max number of iteration for the LASSO solution
  method <- c("MLR")
  #p is the number of variables for each day (I_t and R_t) 
  p <- 2
  #b_t is the block size among the time points (1 : (length(date) - 1))
  
  lambda.1 <- c(1, 0.5, 0.1, 0.05, 0.01, 0.005, 0.001)
  if("Miami-Dade" %in% county.name){
    gamma.val = 5
    # b_t <- 5
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
  
  temp.1 <- tbfl(method, Y_s, X_s, lambda.1.cv = lambda.1, lambda.2.cv = 0,
                 max.iteration = max.iteration, tol = tol, block.size = b_n, HBIC = HBIC, gamma.val = gamma.val)
  
  temp.full[[idx]] <- temp.1
  
  cp <- c(1, temp.1$cp.final, n+1)
  m <- length(cp) - 1
  X.new.new <- matrix(0, nrow = n, ncol = m*p.x)
  for(i in 1:m){
    X.new.new[cp[i]: (cp[i+1]-1), (p.x*(i-1)+1) : (p.x*i) ] <- X[cp[i]: (cp[i+1]-1), ]
  }
  # est.1 <- lm(Y[-c(1:2), ] ~ X.new.new[-c(1:2), ]  - 1)
  est.1 <- lm(Y ~ X.new.new  - 1)
  est.1.full[[idx ]] <- est.1
  Y.hat.1 <- est.1$fitted.values
  MRPE_1_new <- mean(  abs ( (    Y.hat.1[seq(2, n , 2)] - Y[seq(2, n, 2)] )  /c(Y[seq(2,n,2)])  )[c(Y[seq(2,n,2)]) > 0]  )
  MRPE_1_new.full <- c(MRPE_1_new.full, MRPE_1_new)
  
  
  
}
idx <- which.min(MRPE_1_new.full)
cp.final <- temp.full[[idx]]$cp.final 
cp.date <- c(1:n)[floor( (cp.final-1) / p) + 1]
a.final <- a.vals[idx]
a.final
MRPE_1_new.full
date.region[cp.date]
state.name
county.name
a.vals
gamma.val




set.seed(123456)
a.val <- a.final
I <-  I.obs
R <-  R.obs
for(t in 2:T){
  rate <- 1/(((t)+a.val*T.full)/((1 + a.val )*T.full))^2
  print(1/rate)
  I[t] <- (I.obs[t] - I.obs[t-1])*rate + I[t-1]
  
}



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


#################################################
######### Model 1  ##############################
#################################################
#scaling but not centering data
Y_std <- sd(Y)
Y_s <- scale(Y, center = FALSE, scale = apply(Y, 2, sd, na.rm = TRUE))

X_std <- apply(X, MARGIN=2, FUN=sd)
X_s <- scale(X, center = FALSE, scale = apply(X, 2, sd, na.rm = TRUE))


############################################################
############## change point detection #######################
############################################################
p.x <- ncol(X_s)
p.y <- ncol(Y_s)
n <- nrow(X_s)
tol <- 10^(-4); # tolerance 
max.iteration <- 200; # max number of iteration for the LASSO solution
method <- c("MLR")
#p is the number of variables for each day (I_t and R_t) 
p <- 2
#b_t is the block size among the time points (1 : (length(date) - 1))

lambda.1 <- c(1, 0.5, 0.1, 0.05, 0.01, 0.005, 0.001)
if("Miami-Dade" %in% county.name){
  gamma.val = 5
  # b_t <- 5
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

temp.1 <- tbfl(method, Y_s, X_s, lambda.1.cv = lambda.1, lambda.2.cv = 0,
               max.iteration = max.iteration, tol = tol, block.size = b_n, HBIC = HBIC, gamma.val = gamma.val)



b_t
gamma.val
HBIC


beta.est <- temp.1$beta.est
beta.est <- lapply(beta.est, function (x) x*Y_std/X_std)


cp.final <- temp.1$cp.final 
date.region[floor( (cp.final-1) / p) + 1]
cp.date <- date.region[floor( (cp.final-1) / p) + 1]

index.date <- seq(1, length(date.region), b_t)
if(index.date[length(index.date)] < length(date.region)){
  index.date <- c(index.date[-length(index.date)],length(date.region))
}

cols <- c("dark orange", "purple", "darkolivegreen4", "blue" )
beta_t_est <- unlist(beta.est)[seq(1, length(unlist(beta.est)), 2)]
gamma_t_est <- unlist(beta.est)[seq(2, length(unlist(beta.est)), 2)]

ylim_max <- 0.10

beta_t_smooth <- beta_t
beta_t_smooth[beta_t == Inf] = 0
beta_t_smooth[-c(1:6)] <- rollmean(beta_t_smooth, k = 7, align = 'right')
beta_t_smooth[ c(1:6)] <- mean(beta_t_smooth[ c(1:6)])

gamma_t_smooth <- gamma_t
gamma_t_smooth[-c(1:6)] <- rollmean(gamma_t, k = 7, align = 'right')
gamma_t_smooth[ c(1:6)] <- mean(gamma_t[ c(1:6)])


filename <- paste0("beta_gamma_", county.lowernames[1] ,"_smooth.pdf")
pdf(filename, width = 11, height = 8.5)
par(mar = c(4.5, 6, 2, 1.5), mgp=c(3.5, 1.2, 0))
plot(date.region[-length(date.region)], beta_t_smooth, type='l',col=cols[1],lty=1,lwd = 3,
     ylab ='Rate', xlab= 'Date',cex.lab = 3 , cex.axis = 3, ylim = c(0,ylim_max),
     xlim=c(as.Date('2020-03-15'),date.region[length(date.region)]))
lines(date.region[index.date[-length(index.date)]], beta_t_est,col=cols[2],lty=1,type="l",lwd = 3)
lines(as.Date(date.region[-length(date.region)]), gamma_t_smooth, col=cols[3],lty=1,type="l",lwd = 3)
lines(date.region[index.date[-length(index.date)]], gamma_t_est,col=cols[4],lty=1,type="l",lwd = 3)
legend(as.Date(date.region[length(date.region)*3/4]), ylim_max, legend=c(expression(beta(t)),  expression(hat(beta)(t)), expression(gamma(t)),  expression(hat(gamma)(t))),
       col=cols, bty = "n", lwd = 3, cex = 2, pt.cex = 2, bg="transparent",
       seg.len=1.5, y.intersp=1 , x.intersp=1)
abline(v = as.Date(cp.date), col = "dark red",cex = 2 ,lwd = 2, lty = 2)
if(length(cp.date) > 3){
  text(x= as.Date(cp.date[seq(1, length(cp.date), 2)]), y = 4/5*ylim_max, col = "dark red", labels = as.character(cp.date[seq(1, length(cp.date), 2)]), cex = 2.50)
  text(x = as.Date(cp.date[seq(2, length(cp.date)-2, 2)]), y = 3/5*ylim_max, col = "dark red", labels = as.character(cp.date[seq(2, length(cp.date)-2, 2)]), cex = 2.50)
  text(x= as.Date(cp.date[length(cp.date)]), y = 2/5*ylim_max, col = "dark red", labels = as.character(cp.date[length(cp.date)]), cex = 2.50)
  # text(x = as.Date(cp.date[-2]), y = 2/3*ylim_max, col = "dark red", labels = as.character(cp.date[-2]), cex = 2.50)
}else if(length(cp.date) == 3){
  text(x= as.Date(cp.date[c(1, 3 )]), y = 4/5*ylim_max, col = "dark red", labels = as.character(cp.date[c(1, 3)]), cex = 2.50)
  text(x = as.Date(cp.date[2]), y = 3/5*ylim_max, col = "dark red", labels = as.character(cp.date[2]), cex = 2.50)
}else if(length(cp.date) == 2){
  if(cp.date[2] - cp.date[1] < 60){
    text(x= as.Date(cp.date[2]), y = 3/5*ylim_max, col = "dark red", labels = as.character(cp.date[2]), cex = 2.50)
    text(x = as.Date(cp.date[1]), y = 2/5*ylim_max, col = "dark red", labels = as.character(cp.date[1]), cex = 2.50)
  }else{
    text(x = as.Date(cp.date), y = 2/3*ylim_max, col = "dark red", labels = as.character(cp.date), cex = 2.50)
  }
}else{
  text(x = as.Date(cp.date), y = 3/5*ylim_max, col = "dark red", labels = as.character(cp.date), cex = 2.50)
}

abline(v = as.Date(Date.1), col = 1, cex = 2 ,lwd = 2, lty = 3)
abline(v = as.Date(Date.2), col = 1, cex = 2 ,lwd = 2, lty = 3)
if(as.Date(Date.2) - as.Date(Date.1) < 60){
  text(x= as.Date(Date.1), y = 3/4*ylim_max, col ="black",labels = as.character(as.Date(Date.1)), cex = 2.50)
  text(x= as.Date(Date.2), y = 2/4*ylim_max, col = "black",labels = as.character(as.Date(Date.2)), cex = 2.50)
  
}else{
  text(x= as.Date(Date.1), y = 3/4*ylim_max, col ="black", labels = as.character(as.Date(Date.1)), cex = 2.50)
  text(x= as.Date(Date.2), y = 2/4*ylim_max, col = "black", labels = as.character(as.Date(Date.2)), cex = 2.50)
  
}
dev.off()


####plot the estimation of beta and gamma
# filename <- paste0("beta_gamma_", county.lowernames[1] ,".pdf")
# pdf(filename, width = 11, height = 8.5)
par(mar = c(4., 4.5, 1.5, 1))
plot(date.region[-length(date.region)],beta_t,type='l',col=cols[1],lty=1,lwd = 3,
     ylab ='Rate', xlab= 'Date',cex.lab=2 , cex.axis=2,ylim = c(0,ylim_max),
     xlim=c(as.Date('2020-03-15'),date.region[length(date.region)]))
lines(date.region[index.date[-length(index.date)]], beta_t_est,col=cols[2],lty=1,type="l",lwd = 3)
lines(as.Date(date.region[-length(date.region)]),gamma_t,col=cols[3],lty=1,type="l",lwd = 3)
lines(date.region[index.date[-length(index.date)]], gamma_t_est,col=cols[4],lty=1,type="l",lwd = 3)
legend(as.Date(date.region[length(date.region)*3/4]), ylim_max, legend=c(expression(beta(t)),  expression(hat(beta)(t)), expression(gamma(t)),  expression(hat(gamma)(t))),
       col=cols, bty = "n", lwd = 3, cex=1.5, pt.cex = 1, bg="transparent",
       seg.len=1.5, y.intersp=1 , x.intersp=1)
abline(v = as.Date(cp.date), col = "dark red",cex = 2 ,lwd = 2, lty = 2)
# text(x= as.Date(cp.date), y = 2/3*ylim_max, col = "dark red",labels = as.character(cp.date),cex = 2.00)
if(length(cp.date) > 2){
  text(x= as.Date(cp.date[2]), y = 2/4*ylim_max, col = "dark red", labels = as.character(cp.date[2]), cex = 2.00)
  text(x = as.Date(cp.date[-2]), y = 2/3*ylim_max, col = "dark red", labels = as.character(cp.date[-2]), cex = 2.00)
}else if(length(cp.date) == 2){
  if(cp.date[2] - cp.date[1] < 30){
    text(x= as.Date(cp.date[2]), y = 2/4*ylim_max, col = "dark red", labels = as.character(cp.date[2]), cex = 2.00)
    text(x = as.Date(cp.date[1]), y = 2/3*ylim_max, col = "dark red", labels = as.character(cp.date[1]), cex = 2.00)
  }else{
    text(x = as.Date(cp.date), y = 2/3*ylim_max, col = "dark red", labels = as.character(cp.date), cex = 2.00)
  }
}else{
  text(x = as.Date(cp.date), y = 2/3*ylim_max, col = "dark red", labels = as.character(cp.date), cex = 2.00)
}

abline(v = as.Date(Date.1), col = 1, cex = 2 ,lwd = 2, lty = 3)
abline(v = as.Date(Date.2), col = 1, cex = 2 ,lwd = 2, lty = 3)
if(as.Date(Date.2) - as.Date(Date.1) < 30){
  text(x= as.Date(Date.1), y = 3/4*ylim_max, col ="black",labels = as.character(as.Date(Date.1)), cex = 2.00)
  text(x= as.Date(Date.2), y = 2/4*ylim_max, col = "black",labels = as.character(as.Date(Date.2)), cex = 2.00)
  
}else{
  text(x= as.Date(Date.1), y = 3/4*ylim_max, col ="black", labels = as.character(as.Date(Date.1)), cex = 2.00)
  text(x= as.Date(Date.2), y = 3/4*ylim_max, col = "black", labels = as.character(as.Date(Date.2)), cex = 2.00)
  
}
# dev.off()

abs(beta_t_est[-1]-beta_t_est[-length(beta_t_est)])
####plot the change of beta and gamma
# filename <- paste0("change_", county.lowernames[1] ,"_smooth.pdf")
# pdf(filename, width=11, height=8.5)
par(mar = c(4., 4.5, 1.5, 1))
plot(date.region[-c(1,length(date.region))],abs(beta_t_smooth[-1]-beta_t_smooth[-length(beta_t_smooth)]), type='l',col=cols[1],lty=1,lwd = 3,
     ylab ='Rate', xlab= 'Date',cex.lab=2 , cex.axis=2, ylim = c(0, ylim_max),
     xlim=c(as.Date('2020-03-15'),date.region[length(date.region)]))
lines(date.region[index.date[-c(1,length(index.date))]],abs(beta_t_est[-1]-beta_t_est[-length(beta_t_est)]),col=cols[2],lty=1,type="l",lwd = 3)
lines(date.region[-c(1,length(date.region))], abs(gamma_t_smooth[-1]-gamma_t_smooth[-length(gamma_t_smooth)]),col=cols[3],lty=1,type="l",lwd = 3)
lines(date.region[index.date[-c(1,length(index.date))]],abs(gamma_t_est[-1]-gamma_t_est[-length(gamma_t_est)]),col=cols[4],lty=1,type="l",lwd = 3)
legend(as.Date(date.region[length(date.region)*2/3]), ylim_max, legend=c(expression(paste("|",beta(t)-beta(t-1) ,"|")),  expression(paste("|",hat(beta)(t)-hat(beta)(t-1),"|")),
                                                                         expression(paste("|",gamma(t)-gamma(t-1),"|")),  expression(paste("|",hat(gamma)(t)-hat(gamma)(t-1),"|"))),
       col=cols, bty = "n", lwd = 3, cex=1.5, pt.cex = 1, bg="transparent", 
       seg.len=1.5, y.intersp=1 , x.intersp=1)
abline(v = as.Date(cp.date), col = "dark red",cex = 2 ,lwd = 2, lty = 2)
# text(x= as.Date(cp.date), y = 2/3*ylim_max, col = "dark red",labels = as.character(cp.date),cex = 2.00)
if(length(cp.date) > 2){
  text(x= as.Date(cp.date[2]), y = 2/4*ylim_max, col = "dark red", labels = as.character(cp.date[2]), cex = 2.00)
  text(x = as.Date(cp.date[-2]), y = 2/3*ylim_max, col = "dark red", labels = as.character(cp.date[-2]), cex = 2.00)
}else if(length(cp.date) == 2){
  if(cp.date[2] - cp.date[1] < 30){
    text(x= as.Date(cp.date[2]), y = 2/4*ylim_max, col = "dark red", labels = as.character(cp.date[2]), cex = 2.00)
    text(x = as.Date(cp.date[1]), y = 2/3*ylim_max, col = "dark red", labels = as.character(cp.date[1]), cex = 2.00)
  }else{
    text(x = as.Date(cp.date), y = 2/3*ylim_max, col = "dark red", labels = as.character(cp.date), cex = 2.00)
  }
}else{
  text(x = as.Date(cp.date), y = 2/3*ylim_max, col = "dark red", labels = as.character(cp.date), cex = 2.00)
}

abline(v = as.Date(Date.1), col = 1, cex = 2 ,lwd = 2, lty = 3)
abline(v = as.Date(Date.2), col = 1, cex = 2 ,lwd = 2, lty = 3)
if(as.Date(Date.2) - as.Date(Date.1) < 30){
  text(x= as.Date(Date.1), y = 3/4*ylim_max, col ="black",labels = as.character(as.Date(Date.1)), cex = 2.00)
  text(x= as.Date(Date.2), y = 2/4*ylim_max, col = "black",labels = as.character(as.Date(Date.2)), cex = 2.00)
  
}else{
  text(x= as.Date(Date.1), y = 3/4*ylim_max, col ="black", labels = as.character(as.Date(Date.1)), cex = 2.00)
  text(x= as.Date(Date.2), y = 3/4*ylim_max, col = "black", labels = as.character(as.Date(Date.2)), cex = 2.00)
  
}
# dev.off()

# filename <- paste0("change_", county.lowernames[1] ,".pdf")
# pdf(filename, width=11, height=8.5)
par(mar = c(4., 4.5, 1.5, 1))
plot(date.region[-c(1,length(date.region))],abs(beta_t[-1]-beta_t[-length(beta_t)]), type='l',col=cols[1],lty=1,lwd = 3,
     ylab ='Rate', xlab= 'Date',cex.lab=2 , cex.axis=2, ylim = c(0, ylim_max),
     xlim=c(as.Date('2020-03-15'),date.region[length(date.region)]))
lines(date.region[index.date[-c(1,length(index.date))]],abs(beta_t_est[-1]-beta_t_est[-length(beta_t_est)]),col=cols[2],lty=1,type="l",lwd = 3)
lines(date.region[-c(1,length(date.region))], abs(gamma_t[-1]-gamma_t[-length(gamma_t)]),col=cols[3],lty=1,type="l",lwd = 3)
lines(date.region[index.date[-c(1,length(index.date))]],abs(gamma_t_est[-1]-gamma_t_est[-length(gamma_t_est)]),col=cols[4],lty=1,type="l",lwd = 3)
legend(as.Date(date.region[length(date.region)*2/3]), ylim_max, legend=c(expression(paste("|",beta(t)-beta(t-1) ,"|")),  expression(paste("|",hat(beta)(t)-hat(beta)(t-1),"|")),
                                                                           expression(paste("|",gamma(t)-gamma(t-1),"|")),  expression(paste("|",hat(gamma)(t)-hat(gamma)(t-1),"|"))),
       col=cols, bty = "n", lwd = 3, cex=1.5, pt.cex = 1, bg="transparent", 
       seg.len=1.5, y.intersp=1 , x.intersp=1)
abline(v = as.Date(cp.date), col = "dark red",cex = 2 ,lwd = 2, lty = 2)
# text(x= as.Date(cp.date), y = 2/3*ylim_max, col = "dark red",labels = as.character(cp.date),cex = 2.00)
if(length(cp.date) > 2){
  text(x= as.Date(cp.date[2]), y = 2/4*ylim_max, col = "dark red", labels = as.character(cp.date[2]), cex = 2.00)
  text(x = as.Date(cp.date[-2]), y = 2/3*ylim_max, col = "dark red", labels = as.character(cp.date[-2]), cex = 2.00)
}else if(length(cp.date) == 2){
  if(cp.date[2] - cp.date[1] < 30){
    text(x= as.Date(cp.date[2]), y = 2/4*ylim_max, col = "dark red", labels = as.character(cp.date[2]), cex = 2.00)
    text(x = as.Date(cp.date[1]), y = 2/3*ylim_max, col = "dark red", labels = as.character(cp.date[1]), cex = 2.00)
  }else{
    text(x = as.Date(cp.date), y = 2/3*ylim_max, col = "dark red", labels = as.character(cp.date), cex = 2.00)
  }
}else{
  text(x = as.Date(cp.date), y = 2/3*ylim_max, col = "dark red", labels = as.character(cp.date), cex = 2.00)
}

abline(v = as.Date(Date.1), col = 1, cex = 2 ,lwd = 2, lty = 3)
abline(v = as.Date(Date.2), col = 1, cex = 2 ,lwd = 2, lty = 3)
if(as.Date(Date.2) - as.Date(Date.1) < 30){
  text(x= as.Date(Date.1), y = 3/4*ylim_max, col ="black",labels = as.character(as.Date(Date.1)), cex = 2.00)
  text(x= as.Date(Date.2), y = 2/4*ylim_max, col = "black",labels = as.character(as.Date(Date.2)), cex = 2.00)
  
}else{
  text(x= as.Date(Date.1), y = 3/4*ylim_max, col ="black", labels = as.character(as.Date(Date.1)), cex = 2.00)
  text(x= as.Date(Date.2), y = 3/4*ylim_max, col = "black", labels = as.character(as.Date(Date.2)), cex = 2.00)
  
}
# dev.off()



filename <- paste0("R0_", county.lowernames[1], ".pdf")
pdf(filename, width=11, height=8.5)
ylim_max = 15
par(mar = c(4.5, 6, 2, 1.5), mgp=c(3.5, 1.2, 0))
plot(date.region[-length(date.region)], beta_t/gamma_t, type = 'l', col = cols[1], lty = 1, lwd = 3,
     ylab = 'Rate', xlab = 'Date', cex.lab = 3, cex.axis = 3, ylim = c(0, ylim_max),
     xlim = c(as.Date('2020-03-15'), date.region[length(date.region)]))
lines(date.region[index.date[-length(index.date)]], beta_t_est/gamma_t_est, col = cols[2], lty = 1, type = "l", lwd = 3)
legend(as.Date(date.region[length(date.region)*3/4]), ylim_max, legend = c(expression(R(t)),  expression(hat(R)(t))),
       col = cols, bty = "n", lwd = 3, cex = 2, pt.cex = 2, bg="transparent",
       seg.len=1.5, y.intersp=1 , x.intersp=1)
abline(v = as.Date(cp.date), col = "dark red",cex = 2 ,lwd = 2, lty = 2)
if(length(cp.date) > 3){
  text(x= as.Date(cp.date[seq(1, length(cp.date), 2)]), y = 4/5*ylim_max, col = "dark red", labels = as.character(cp.date[seq(1, length(cp.date), 2)]), cex = 2.50)
  text(x = as.Date(cp.date[seq(2, length(cp.date)-2, 2)]), y = 3/5*ylim_max, col = "dark red", labels = as.character(cp.date[seq(2, length(cp.date)-2, 2)]), cex = 2.50)
  text(x= as.Date(cp.date[length(cp.date)]), y = 2/5*ylim_max, col = "dark red", labels = as.character(cp.date[length(cp.date)]), cex = 2.50)
  # text(x = as.Date(cp.date[-2]), y = 2/3*ylim_max, col = "dark red", labels = as.character(cp.date[-2]), cex = 2.50)
}else if(length(cp.date) == 3){
  text(x= as.Date(cp.date[c(1, 3 )]), y = 4/5*ylim_max, col = "dark red", labels = as.character(cp.date[c(1, 3)]), cex = 2.50)
  text(x = as.Date(cp.date[2]), y = 3/5*ylim_max, col = "dark red", labels = as.character(cp.date[2]), cex = 2.50)
}else if(length(cp.date) == 2){
  if(cp.date[2] - cp.date[1] < 60){
    text(x= as.Date(cp.date[2]), y = 2/4*ylim_max, col = "dark red", labels = as.character(cp.date[2]), cex = 2.50)
    text(x = as.Date(cp.date[1]), y = 2/3*ylim_max, col = "dark red", labels = as.character(cp.date[1]), cex = 2.50)
  }else{
    text(x = as.Date(cp.date), y = 2/3*ylim_max, col = "dark red", labels = as.character(cp.date), cex = 2.50)
  }
}else{
  text(x = as.Date(cp.date), y = 3/5*ylim_max, col = "dark red", labels = as.character(cp.date), cex = 2.50)
}
# text(x= as.Date(cp.date), y = 2/3*ylim_max, col = "dark red",labels = as.character(cp.date), cex = 2.00)
abline(v = as.Date(Date.1), col = 1, cex = 2, lwd = 2, lty = 3)
abline(v = as.Date(Date.2), col = 1, cex = 2, lwd = 2, lty = 3)
# text(x = as.Date(Date.1), y = 3/4*ylim_max, col = "black", labels = as.character(as.Date(Date.1)), cex = 2.50)
# text(x = as.Date(Date.2), y = 3/4*ylim_max, col = "black", labels = as.character(as.Date(Date.2)), cex = 2.50)
if(as.Date(Date.2) - as.Date(Date.1) < 60){
  text(x= as.Date(Date.1), y = 3/4*ylim_max, col ="black",labels = as.character(as.Date(Date.1)), cex = 2.50)
  text(x= as.Date(Date.2), y = 2/4*ylim_max, col = "black",labels = as.character(as.Date(Date.2)), cex = 2.50)
  
}else{
  text(x= as.Date(Date.1), y = 4/5*ylim_max, col ="black", labels = as.character(as.Date(Date.1)), cex = 2.50)
  text(x= as.Date(Date.2), y = 2/5*ylim_max, col = "black", labels = as.character(as.Date(Date.2)), cex = 2.50)
  
}
abline(h = 1, col = "red", cex = 2, lwd = 2, lty = 3)
dev.off()

#################
# refit the model 
#################
cp <- c(1, temp.1$cp.final, n+1)
m <- length(cp) - 1
Y.hat.1 <- matrix(0, n)
for(i in 1:m){
  Y.temp.1 <- Y[cp[i]: (cp[i+1]-1), ]
  X.temp.1 <- X[cp[i]: (cp[i+1]-1), ]
  est.temp.1 <- lm(Y.temp.1 ~ X.temp.1  - 1)
  print(est.temp.1)
  Y.hat.1[cp[i]: (cp[i+1]-1), ] <- est.temp.1$fitted.values
}



X.new.new <- matrix(0, nrow = n, ncol = m*p.x)
for(i in 1:m){
  X.new.new[cp[i]: (cp[i+1]-1), (p.x*(i-1)+1) : (p.x*i) ] <- X[cp[i]: (cp[i+1]-1), ]
}
est.1 <- lm(Y[-c(1:2), ] ~ X.new.new[-c(1:2), ]  - 1)

summary(est.1)


R.hat.1 <- rep(0,T)
R.hat.1[1] <- R.rate.all[1,1]
for(i in 2:T){
  R.hat.1[i] <- R.rate.all[i-1,1]+Y.hat.1[(i-2)*2+1]
}
R.hat.1 <- R.hat.1*n.all[1]

# I.hat.1 <- rep(0,T)
# I.hat.1[1] <- I.rate.all[1,1]
# for(i in 2:T){
#   I.hat.1[i] <-  I.rate.all[i-1,1]+Y.hat.1[(i-2)*2+2]
# }
# I.hat.1 <- I.hat.1*n.all[1]

I.hat.obs.1 <- rep(0,T)
I.hat.obs.1[1] <- I.rate.all.obs[1,1]
for(i in 2:T){
  rate <- (((i)+a.val*T.full)/((1 + a.val )*T.full))^2
  I.hat.obs.1[i] <-  I.rate.all.obs[i-1,1] + Y.hat.1[(i-2)*2+2]*rate
}
I.hat.obs.1 <- I.hat.obs.1*n.all[1]


MRPE_1_I <- mean(  abs ( (     c(I.hat.obs.1[-1]) - c(I.obs[-1])     )  /c(I.obs[-1])  )[intersect( which(c(I.obs[-1]) > 0  ), 2:(T-1) )  ]  )
print(round(MRPE_1_I, 4))
MRPE_1_R <- mean(  abs ( (     c(R.hat.1[-1]) - c(R.obs[-1])     )  /c(R.obs[-1])  )[intersect( which(c(R.obs[-1]) > 0  ), 2:(T-1) )  ]  )
print(round(MRPE_1_R, 4))

# MRPE_1_I <- mean(  abs ( (     c(I.hat.1[-1]) - c(I[-1])     )  /c(I[-1])  )[intersect( which(c(I[-1]) > 0  ), 2:(T-1) )  ]  )
# print(round(MRPE_1_I,4))
# MRPE_1_R <- mean(  abs ( (     c(R.hat.1[-1]) - c(R[-1])     )  /c(R[-1])  )[intersect( which(c(R[-1]) > 0  ), 2:(T-1) )  ]  )
# print(round(MRPE_1_R,4))


R.tilde.1 <- rep(0,T)
R.tilde.1[1] <- R.rate.all[1,1]
for(i in 2:T){
  R.tilde.1[i] <- R.tilde.1[i-1]+Y.hat.1[(i-2)*2+1]
}
R.tilde.1 <- R.tilde.1*n.all[1]


I.tilde.1 <- rep(0,T)
I.tilde.1[1] <- I.rate.all.obs[1,1]
for(i in 2:T){
  rate <- (((i)+a.val*T.full)/((1 + a.val )*T.full))^2
  I.tilde.1[i] <- I.tilde.1[i-1] + Y.hat.1[(i-2)*2+2]*rate
}
I.tilde.1 <- I.tilde.1*n.all[1]


# I.tilde.1 <- rep(0,T)
# I.tilde.1[1] <- I.rate.all[1,1]
# for(i in 2:T){
#   I.tilde.1[i] <- I.tilde.1[i-1]+Y.hat.1[(i-2)*2+2]
# }
# I.tilde.1 <- I.tilde.1*n.all[1]



filename <- paste0("Infected_", county.lowernames[1] ,"_1.pdf")
pdf(filename, width=11, height=8.5)
par(mar = c(4.5, 6, 2.8, 1.5), mgp=c(3.5, 1.2, 0))
plot(date.region, I.obs , col='1', ylim=c(0,max(I.obs, I.tilde.1)), lty=1, type="l", lwd = 3,
     ylab ='Number of Infected Cases', xlab = 'Date', cex.lab = 3, cex.axis = 3)
lines(date.region, I.tilde.1, col='2',lty=1,type="l",lwd = 3)
legend(as.Date(date.region[length(date.region)*3/4]), 1/4*max(I.obs, I.tilde.1), 
       legend=c(expression(I(t)), expression(tilde(I)(t))),
       col=c( 1,2),bg="transparent",bty = "n", 
       lwd = 3, cex = 2, pt.cex = 2, seg.len = 1.5, y.intersp = 1 , x.intersp = 1)
dev.off()


filename <- paste0("Recovered_", county.lowernames[1] ,"_1.pdf")
pdf(filename, width=11, height=8.5)
par(mar = c(4.5, 6, 2.8, 1.5), mgp=c(3.5, 1.2, 0))
plot(date.region, R, type='l',col='1',ylim=c(0,max(R, R.tilde.1)),lty=1,lwd = 3,
     ylab ='Number of Recovered Cases', xlab= 'Date',cex.lab = 3, cex.axis = 3)
lines(date.region, R.tilde.1, col='2',lty=1,type="l",lwd = 3)
legend(as.Date(date.region[length(date.region)*3/4]), 1/4*max(R, R.tilde.1), legend = c(expression(R(t)), expression(tilde(R)(t))),
       col=c( 1,2),bg="transparent",bty = "n", 
       lwd = 3, cex = 2, pt.cex = 2, seg.len=1.5, y.intersp = 1 , x.intersp=1)
dev.off()

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
  # rate <- 1/(((t)+a.val*T)/((1 + a.val )*T))^2
  rate <- 1/(((t)+a.val*T.full)/((1 + a.val )*T.full))^2
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

p.x <- ncol(X)
p.y <-ncol(Y)
n <- nrow(X)


cp <- c(1, temp.1$cp.final, n+1)
m <- length(cp) - 1
X.new <- matrix(0, nrow = n, ncol = m*p.x + 1)
for(i in 1:m){
  X.new[cp[i]: (cp[i+1]-1), (p.x*(i-1)+1) : (p.x*i) ] <- X[cp[i]: (cp[i+1]-1), ]
}
X.new[, m*p.x + 1] <- neighbor.weighted_1


est.2.1 <- lm(Y[-c(1:2), ] ~ X.new[-c(1:2), ]  - 1)
Y.hat.2.1 <- c(0, 0, est.2.1 $fitted.values)

summary(est.2.1 )


R.hat.2.1 <- rep(0, T)
R.hat.2.1[1] <- R.rate.all[1, 1]
for(i in 2:T){
  R.hat.2.1[i] <- R.rate.all[i-1, 1] + Y.hat.2.1[(i-2)*2+1]
}
R.hat.2.1 <- R.hat.2.1*n.all[1]

# I.hat.2.1 <- rep(0,T)
# I.hat.2.1[1] <- I.rate.all[1,1]
# for(i in 2:T){
#   I.hat.2.1[i] <-  I.rate.all[i-1,1] + Y.hat.2.1[(i-2)*2+2]
# }
# I.hat.2.1 <- I.hat.2.1*n.all[1]

I.hat.obs.2.1 <- rep(0, T)
I.hat.obs.2.1[1] <- I.rate.all.obs[1,1]
for(i in 2:T){
  rate <- (((i)+a.val*T.full)/((1 + a.val )*T.full))^2
  I.hat.obs.2.1[i] <-  I.rate.all.obs[i-1,1] + Y.hat.2.1[(i-2)*2+2]*rate
}
I.hat.obs.2.1 <- I.hat.obs.2.1*n.all[1]



MRPE_2.1_I <- mean(  abs ( (     c(I.hat.obs.2.1[-1]) - c(I.obs[-1]))  /c(I.obs[-1])  )[ intersect( which(c(I.obs[-1]) > 0  ), 2:(T-1) )   ]  )
print(round(MRPE_2.1_I, 4))
MRPE_2.1_R <- mean(  abs ( (     c(R.hat.2.1[-1]) - c(R[-1]))  /c(R[-1])  )[ intersect( which(c(R[-1]) > 0  ), 2:(T-1) ) ]  )
print(round(MRPE_2.1_R, 4))

# MRPE_2.1_I <- mean(  abs ( (     c(I.hat.2.1[-1]) - c(I[-1])     )  /c(I[-1])  )[ intersect( which(c(I[-1]) > 0  ), 2:(T-1) )   ]  )
# print(round(MRPE_2.1_I, 4))
# MRPE_2.1_R <- mean(  abs ( (     c(R.hat.2.1[-1]) - c(R[-1])     )  /c(R[-1])  )[ intersect( which(c(R[-1]) > 0  ), 2:(T-1) ) ]  )
# print(round(MRPE_2.1_R, 4))



R.tilde.2.1 <- rep(0,T)
R.tilde.2.1[1] <- R.rate.all[1,1]
for(i in 2:T){
  R.tilde.2.1[i] <- R.tilde.2.1[i-1] + Y.hat.2.1[(i-2)*2+1]
}
R.tilde.2.1 <- R.tilde.2.1*n.all[1]

# I.tilde.2.1 <- rep(0,T)
# I.tilde.2.1[1] <- I.rate.all[1,1]
# for(i in 2:T){
#   I.tilde.2.1[i] <- I.tilde.2.1[i-1] + Y.hat.2.1[(i-2)*2+2]
# }
# I.tilde.2.1 <- I.tilde.2.1*n.all[1]

I.tilde.2.1 <- rep(0,T)
I.tilde.2.1[1] <- I.rate.all.obs[1,1]
for(i in 2:T){
  rate <- (((i)+a.val*T.full)/((1 + a.val )*T.full))^2
  I.tilde.2.1[i] <- I.tilde.2.1[i-1] + Y.hat.2.1[(i-2)*2+2]*rate
}
I.tilde.2.1 <- I.tilde.2.1*n.all[1]



filename <- paste0("Infected_",  county.lowernames[1] ,"_2.1.pdf")
pdf(filename, width=11, height=8.5)
par(mar = c(4.5, 6, 2.8, 1.5), mgp=c(3.5, 1.2, 0))
plot(date.region, I.obs, col='1', ylim=c(0,max(I.obs, I.tilde.2.1)),lty=1, type="l", lwd = 3,
     ylab ='Number of Infected Cases', xlab= 'Date', cex.lab = 3, cex.axis = 3)
lines(date.region, I.tilde.2.1, col='2', lty=1, type="l", lwd = 3)
legend(as.Date(date.region[length(date.region)*3/4]), 1/4*max(I.obs, I.tilde.2.1), 
       legend=c(expression(I(t)), expression(tilde(I)(t))),
       col=c( 1,2),bg="transparent",bty = "n", 
       lwd = 3, cex = 2, pt.cex = 2, seg.len=1.5, y.intersp=1 , x.intersp=1)
dev.off()

# Open a pdf file
filename <- paste0("Recovered_",  county.lowernames[1] ,"_2.1.pdf")
pdf(filename, width=11, height=8.5)
par(mar = c(4.5, 6, 2.8, 1.5), mgp=c(3.5, 1.2, 0))
plot(date.region, R, type='l', col='1', ylim=c(0, max(R, R.tilde.2.1)), lty=1, lwd = 3,
     ylab ='Number of Recovered Cases', xlab= 'Date', cex.lab = 3 , cex.axis = 3)
lines(date.region, R.tilde.2.1, col='2', lty=1, type="l",lwd = 3)
legend(as.Date(date.region[length(date.region)*3/4]), 1/4*max(R, R.tilde.2.1), legend=c(expression(R(t)), expression(tilde(R)(t))),
       col=c( 1,2),bg="transparent", bty = "n", 
       lwd = 3, cex = 2, pt.cex = 2, seg.len=1.5, y.intersp=1, x.intersp=1)
dev.off()

#################################################
###### Model 2.2 distance based weight
###### spatial components are chosen by distance
#################################################

distance_2 <- rep(0, n.domains)
# "Louisiana" use parish instead of county!!!
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
neighbor.weighted_2 <- as.matrix(neighbor.weighted_2[-c(rows.combined-1,rows.combined),])
neighbor.weighted_2 <- as.matrix(c(neighbor.weighted_2[c(1,2),], neighbor.weighted_2))


p.x <- ncol(X)
p.y <-ncol(Y)
n <- nrow(X)



cp <- c(1, temp.1$cp.final, n+1)
m <- length(cp) - 1
X.new.2 <- matrix(0, nrow = n, ncol = m*p.x + 1)
for(i in 1:m){
  X.new.2[cp[i]: (cp[i+1]-1), (p.x*(i-1)+1) : (p.x*i) ] <- X[cp[i]: (cp[i+1]-1), ]
}
X.new.2[, m*p.x + 1] <- neighbor.weighted_2


est.2.2 <- lm(Y[-c(1:2), ] ~ X.new.2[-c(1:2), ]  - 1)
Y.hat.2.2 <- c(0, 0, est.2.2 $fitted.values)
summary(est.2.2 )


R.hat.2.2 <- rep(0, T)
R.hat.2.2[1] <- R.rate.all[1, 1]
for(i in 2:T){
  R.hat.2.2[i] <- R.rate.all[i-1, 1] + Y.hat.2.2[(i-2)*2+1]
}
R.hat.2.2 <- R.hat.2.2*n.all[1]

I.hat.obs.2.2 <- rep(0,T)
I.hat.obs.2.2[1] <- I.rate.all.obs[1,1]
for(i in 2:T){
  rate <- (((i)+a.val*T.full)/((1 + a.val )*T.full))^2
  I.hat.obs.2.2[i] <-  I.rate.all.obs[i-1,1] + Y.hat.2.2[(i-2)*2+2]*rate
}
I.hat.obs.2.2 <- I.hat.obs.2.2*n.all[1]


MRPE_2.2_I <- mean(  abs ( (     c(I.hat.obs.2.2[-1]) - c(I.obs[-1]))  /c(I.obs[-1])  )[intersect( which(c(I.obs[-1]) > 0  ), 2:(T-1) ) ]  )
print(round(MRPE_2.2_I,4))
MRPE_2.2_R <- mean(  abs ( (     c(R.hat.2.2[-1]) - c(R[-1])     )  /c(R[-1])  )[intersect( which(c(R[-1]) > 0  ), 2:(T-1) ) ]  )
print(round(MRPE_2.2_R,4))

# I.hat.2.2 <- rep(0,T)
# I.hat.2.2[1] <- I.rate.all[1,1]
# for(i in 2:T){
#   I.hat.2.2[i] <-  I.rate.all[i-1,1] + Y.hat.2.2[(i-2)*2+2]
# }
# I.hat.2.2 <- I.hat.2.2*n.all[1]
# 
# 
# MRPE_2.2_I <- mean(  abs ( (     c(I.hat.2.2[-1]) - c(I[-1])     )  /c(I[-1])  )[intersect( which(c(I[-1]) > 0  ), 2:(T-1) ) ]  )
# print(round(MRPE_2.2_I,4))
# MRPE_2.2_R <- mean(  abs ( (     c(R.hat.2.2[-1]) - c(R[-1])     )  /c(R[-1])  )[intersect( which(c(R[-1]) > 0  ), 2:(T-1) ) ]  )
# print(round(MRPE_2.2_R,4))



R.tilde.2.2 <- rep(0,T)
R.tilde.2.2[1] <- R.rate.all[1,1]
for(i in 2:T){
  R.tilde.2.2[i] <- R.tilde.2.2[i-1] + Y.hat.2.2[(i-2)*2+1]
}
R.tilde.2.2 <- R.tilde.2.2*n.all[1]

I.tilde.2.2 <- rep(0,T)
I.tilde.2.2[1] <- I.rate.all.obs[1,1]
for(i in 2:T){
  rate <- (((i)+a.val*T.full)/((1 + a.val )*T.full))^2
  I.tilde.2.2[i] <- I.tilde.2.2[i-1] + Y.hat.2.2[(i-2)*2+2]*rate
}
I.tilde.2.2 <- I.tilde.2.2*n.all[1]

# I.tilde.2.2 <- rep(0,T)
# I.tilde.2.2[1] <- I.rate.all[1,1]
# for(i in 2:T){
#   I.tilde.2.2[i] <- I.tilde.2.2[i-1] + Y.hat.2.2[(i-2)*2+2]
# }
# I.tilde.2.2 <- I.tilde.2.2*n.all[1]


filename <- paste0("Infected_", county.lowernames[1] ,"_2.2.pdf")
pdf(filename, width=11, height=8.5)
par(mar = c(4.5, 6, 2.8, 1.5), mgp = c(3.5, 1.2, 0))
plot(date.region, I.obs, col='1', ylim=c(0,max(I.obs, I.tilde.2.2)), lty=1, type="l", lwd = 3,
     ylab ='Number of Infected Cases', xlab= 'Date',cex.lab=3, cex.axis=3)
lines(date.region, I.tilde.2.2, col='2', lty=1, type="l", lwd = 3)
legend(as.Date(date.region[length(date.region)*3/4]), 1/4*max(I.obs, I.tilde.2.2), 
       legend=c(expression(I(t)), expression(tilde(I)(t))),
       col=c( 1,2), bg="transparent", bty = "n", 
       lwd = 3, cex = 2, pt.cex = 2, seg.len=1.5, y.intersp=1 , x.intersp=1)
dev.off()

# Open a pdf file
filename <- paste0("Recovered_", county.lowernames[1], "_2.2.pdf")
pdf(filename, width = 11, height = 8.5)
par(mar = c(4.5, 6, 2.8, 1.5), mgp = c(3.5, 1.2, 0))
plot(date.region, R, type='l',col='1', ylim=c(0,max(R, R.tilde.2.2)), lty = 1, lwd = 3,
     ylab ='Number of Recovered Cases', xlab= 'Date', cex.lab = 3 , cex.axis = 3)
lines(date.region, R.tilde.2.2, col='2',lty=1,type="l",lwd = 3)
legend(as.Date(date.region[length(date.region)*3/4]), 1/4*max(R, R.tilde.2.2), legend=c(expression(R(t)), expression(tilde(R)(t))),
       col=c( 1,2), bg="transparent", bty = "n", 
       lwd = 3, cex = 2, pt.cex = 2, seg.len=1.5, y.intersp=1, x.intersp=1)
dev.off()

#################################################
###### Model 2.3 similarity based weight
###### spatial components are chosen by similarity
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
S.rate.all <- sapply(1:n.domains, function(jjj) S.all[,jjj]/n.all[jjj])
I.rate.all <- sapply(1:n.domains, function(jjj) I.all[,jjj]/n.all[jjj])
R.rate.all <- sapply(1:n.domains, function(jjj) R.all[,jjj]/n.all[jjj])



I.rate.all.obs <- I.rate.all
R.rate.all.obs <- R.rate.all
# S.rate.all.obs <- S.rate.all

a.val <- a.final
for(t in 2:T){
  rate <- 1/(((t)+a.val*T.full)/((1 + a.val )*T.full))^2
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
county.res <-xtable(matrix(c(county.names[1], 
                             do.call(paste, c(as.list(county.names[-1]), sep = ", ")) ), nrow = 1))
print(county.res, include.rownames = FALSE)


n.all.full <- n.all
# S.rate.all.full <- S.rate.all
I.rate.all.full <- I.rate.all
R.rate.all.full <- R.rate.all

n.domains <- length(county.names)
n.all <- n.all[county.index]
# S.rate.all <- S.rate.all[, county.index]
I.rate.all <- I.rate.all[, county.index]
R.rate.all <- R.rate.all[, county.index]

I.rate.all.obs.part <- I.rate.all.obs[, county.index]
R.rate.all.obs.part <- R.rate.all.obs[, county.index]
# S.rate.all.obs.part <- S.rate.all.obs[, county.index]

date.region <- multi_full$date
# I <-I.all[,1]
# R <-R.all[,1]
cols <- brewer.pal(n.domains, "Spectral")

filename <- paste0("I_rate_", county.lowernames[1] ,"_all_new.pdf")
pdf(filename, width=11, height=8.5)
par(mar = c(4.5, 6, 2, 1.5), mgp=c(3.5, 1.2, 0))
plot(multi_full$date, I.rate.all.obs.part[,1] ,col=cols[1],lty=1,type="l",lwd = 3, 
     ylim = c(0,max(I.rate.all.obs.part)),
     ylab ='Fraction of Infected Cases', xlab= 'Date',cex.lab=3 , cex.axis=3)
for(i in 2:length(county.names)){
  lines(multi_full$date, I.rate.all.obs.part[,i] ,col=cols[i],lty=1,type="l",lwd = 3)
}
legend(multi_full$date[1],max(I.rate.all.obs.part) , legend=c(county.names),
       col=cols,bg="transparent",bty = "n",
       lwd = 3, cex=2, pt.cex = 2, seg.len=1.5, y.intersp=1 , x.intersp=1)
dev.off()

filename <- paste0("R_rate_", county.lowernames[1] ,"_all_new.pdf")
pdf(filename, width=11, height=8.5)
par(mar = c(4.5, 6, 2, 1.5), mgp=c(3.5, 1.2, 0))
plot(multi_full$date, R.rate.all[,1] ,col=cols[1],lty=1,type="l",lwd = 3,ylim = c(0,max(R.rate.all)),
     ylab ='Fraction of Recovered Cases', xlab= 'Date',cex.lab=3 , cex.axis=3)
for(i in 2:length(county.names)){
  lines(multi_full$date, R.rate.all[,i] ,col=cols[i],lty=1,type="l",lwd = 3)
}
legend(multi_full$date[1],max(R.rate.all) , legend=c(county.names),
       col=cols,bg="transparent",bty = "n",
       lwd = 3, cex=2, pt.cex = 2, seg.len=1.5, y.intersp=1 , x.intersp=1)
dev.off()


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


p.x <- ncol(X)
p.y <-ncol(Y)
n <- nrow(X)


cp <- c(1, temp.1$cp.final, n+1)
m <- length(cp) - 1
X.new.3 <- matrix(0, nrow = n, ncol = m*p.x + 1)
for(i in 1:m){
  X.new.3[cp[i]: (cp[i+1]-1), (p.x*(i-1)+1) : (p.x*i) ] <- X[cp[i]: (cp[i+1]-1), ]
}
X.new.3[, m*p.x + 1] <- neighbor.weighted_3


est.2.3 <- lm(Y[-c(1:2), ] ~ X.new.3[-c(1:2), ]  - 1)
Y.hat.2.3 <- c(0, 0, est.2.3 $fitted.values)
summary(est.2.3 )

R.hat.2.3 <- rep(0, T)
R.hat.2.3[1] <- R.rate.all[1, 1]
for(i in 2:T){
  R.hat.2.3[i] <- R.rate.all[i-1, 1] + Y.hat.2.3[(i-2)*2+1]
}
R.hat.2.3 <- R.hat.2.3*n.all[1]

I.hat.obs.2.3 <- rep(0,T)
I.hat.obs.2.3[1] <- I.rate.all.obs[1,1]
for(i in 2:T){
  rate <- (((i)+a.val*T.full)/((1 + a.val )*T.full))^2
  I.hat.obs.2.3[i] <-  I.rate.all.obs[i-1,1] + Y.hat.2.3[(i-2)*2+2]*rate
}
I.hat.obs.2.3 <- I.hat.obs.2.3*n.all[1]


MRPE_2.3_I <- mean(  abs ( (     c(I.hat.obs.2.3[-1]) - c(I.obs[-1]))  /c(I.obs[-1])  )[intersect( which(c(I.obs[-1]) > 0  ), 2:(T-1) ) ]  )
print(round(MRPE_2.3_I,4))
MRPE_2.3_R <- mean(  abs ( (     c(R.hat.2.3[-1]) - c(R[-1])     )  /c(R[-1])  )[intersect( which(c(R[-1]) > 0  ), 2:(T-1) ) ]  )
print(round(MRPE_2.3_R,4))



# I.hat.2.3 <- rep(0,T)
# I.hat.2.3[1] <- I.rate.all[1,1]
# for(i in 2:T){
#   I.hat.2.3[i] <-  I.rate.all[i-1,1] + Y.hat.2.3[(i-2)*2+2]
# }
# I.hat.2.3 <- I.hat.2.3*n.all[1]
# 
# 
# MRPE_2.3_I <- mean(  abs ( (     c(I.hat.2.3[-1]) - c(I[-1])     )  /c(I[-1])  )[intersect( which(c(I[-1]) > 0  ), 2:(T-1) ) ]  )
# print(round(MRPE_2.3_I,4))
# MRPE_2.3_R <- mean(  abs ( (     c(R.hat.2.3[-1]) - c(R[-1])     )  /c(R[-1])  )[intersect( which(c(R[-1]) > 0  ), 2:(T-1) ) ]  )
# print(round(MRPE_2.3_R,4))



R.tilde.2.3 <- rep(0,T)
R.tilde.2.3[1] <- R.rate.all[1,1]
for(i in 2:T){
  R.tilde.2.3[i] <- R.tilde.2.3[i-1] + Y.hat.2.3[(i-2)*2+1]
}
R.tilde.2.3 <- R.tilde.2.3*n.all[1]

I.tilde.2.3 <- rep(0, T)
I.tilde.2.3[1] <- I.rate.all.obs[1,1]
for(i in 2:T){
  rate <- (((i)+a.val*T.full)/((1 + a.val )*T.full))^2
  I.tilde.2.3[i] <- I.tilde.2.3[i-1] + Y.hat.2.3[(i-2)*2+2]*rate
}
I.tilde.2.3 <- I.tilde.2.3*n.all[1]


# I.tilde.2.3 <- rep(0,T)
# I.tilde.2.3[1] <- I.rate.all[1,1]
# for(i in 2:T){
#   I.tilde.2.3[i] <- I.tilde.2.3[i-1] + Y.hat.2.3[(i-2)*2+2]
# }
# I.tilde.2.3 <- I.tilde.2.3*n.all[1]


filename <- paste0("Infected_", county.lowernames[1] ,"_2.3.pdf")
pdf(filename, width=11, height=8.5)
par(mar = c(4.5, 6, 2.8, 1.5), mgp=c(3.5, 1.2, 0))
plot(date.region, I.obs, col='1', ylim=c(0,max(I.obs, I.tilde.2.3)), lty=1, type="l", lwd = 3,
     ylab ='Number of Infected Cases', xlab= 'Date',cex.lab=3, cex.axis=3)
lines(date.region, I.tilde.2.3, col='2', lty=1, type="l", lwd = 3)
legend(as.Date(date.region[length(date.region)*3/4]), 1/4*max(I.obs, I.tilde.2.3),
       legend=c(expression(I(t)), expression(tilde(I)(t))),
       col=c( 1,2), bg="transparent", bty = "n", 
       lwd = 3, cex=2, pt.cex = 2, seg.len=1.5, y.intersp=1 , x.intersp=1)
dev.off()


filename <- paste0("Recovered_", county.lowernames[1], "_2.3.pdf")
pdf(filename, width=11, height=8.5)
par(mar = c(4.5, 6, 2.8, 1.5), mgp=c(3.5, 1.2, 0))
plot(date.region, R, type='l',col='1', ylim=c(0,max(R, R.tilde.2.3)), lty=1, lwd = 3,
     ylab ='Number of Recovered Cases', xlab= 'Date', cex.lab=3 , cex.axis=3)
lines(date.region, R.tilde.2.3, col='2',lty=1,type="l",lwd = 3)
legend(as.Date(date.region[length(date.region)*3/4]), 1/4*max(R, R.tilde.2.3), legend=c(expression(R(t)), expression(tilde(R)(t))),
       col=c( 1,2), bg="transparent", bty = "n", 
       lwd = 3, cex=2, pt.cex = 2, seg.len=1.5, y.intersp=1, x.intersp=1)
dev.off()


#####################################
###### model 3 use VAR(p) model
#####################################

Delta.R.hat <- rep(0, T - 1)
Delta.R <- rep(0, T - 1)
for(i in 2:T){
  Delta.R.hat[i - 1] <- Y.hat.2.3[(i - 2)*2 + 1]
  Delta.R[i - 1] <- Y[(i - 2)*2 + 1]
}
Delta.R.hat <- Delta.R.hat*n.all[1]
Delta.R <- Delta.R*n.all[1]


Delta.I.hat <- rep(0, T - 1)
Delta.I <- rep(0, T - 1)
for(i in 2:T){
  Delta.I.hat[i - 1] <- Y.hat.2.3[(i - 2)*2 + 2]
  Delta.I[i - 1] <- Y[(i - 2)*2 + 2]
}
Delta.I.hat <- Delta.I.hat*n.all[1]
Delta.I <- Delta.I*n.all[1]


residual_Delta.I <- Delta.I - Delta.I.hat 
residual_Delta.R <- Delta.R - Delta.R.hat 


res <- c()
for(i in 1:n){
  res <- c(res,  Y[i] - Y.hat.2.3[i])
}
res <- res*n.all[1]


filename <- paste0("acf_residual_Delta_I_", county.lowernames[1], "_b_",  b_t, ".pdf")
pdf(filename, width=11, height=8.5)
par(mar = c(4.8, 6, 1.7, 1.5), mgp=c(3.5, 1.1, 0))
acf(residual_Delta.I, cex.lab = 3 , cex.axis = 3, lwd = 3)
dev.off()

filename <- paste0("acf_residual_Delta_R_", county.lowernames[1], "_b_",  b_t, ".pdf")
pdf(filename, width=11, height=8.5)
par(mar = c(4.8, 6, 1.7, 1.5), mgp=c(3.5, 1.1, 0))
acf(residual_Delta.R, cex.lab = 3, cex.axis = 3, lwd = 3)
dev.off()


residual.matrix <- cbind(residual_Delta.I, residual_Delta.R)
print("estimated sample variance for hat residual:")
print(var(residual.matrix))

var <- VARselect(residual.matrix, lag.max = 7, type = "none")

var$selection
# choose the p by BIC
p.est <- var$selection["SC(n)"]

#change point detection
method <- c("VAR")
if(p.est >= b_n/2){
  temp.var <- tbfl(method, residual.matrix, lambda.1.cv = NULL, lambda.2.cv = 0, q = p.est, max.iteration = max.iteration, tol = tol, block.size = p.est+1)
  print(p.est)
}else{
  temp.var <- tbfl(method, residual.matrix, lambda.1.cv = NULL, lambda.2.cv = 0, q = p.est, max.iteration = max.iteration, tol = tol, block.size = b_n/2)
}



if(is.null(temp.var$cp.final)){
  print("residual structure no change point!")
  var1 <- VAR(residual.matrix, p = p.est, type = "none")
  coef.matrix <- Bcoef(var1)
  #fitted the residual in training set
  residual.hat <- fitted(var1)
  residual.hat <- rbind(matrix(0, ncol = 2, nrow = p.est), residual.hat) 
  # residual.tilde <- resid(var1)
  residual.tilde <- residual.matrix -  residual.hat
}else{
  print("residual structure has change point!")
  cp.residual<- c(1, temp.var$cp.final, nrow(residual.matrix)+1)
  m <- length(cp.residual) - 1
  residual.hat <- matrix(0, nrow = nrow(residual.matrix), ncol = ncol(residual.matrix))
  
  for(i in 1:m){
    residual.matrix.temp <- residual.matrix[ cp.residual[i]: (cp.residual[i+1]-1),  ]
    var.temp <- VARselect(residual.matrix.temp, lag.max = 7, type = "none")
    var.temp$selection
    p.est <- var.temp$selection["SC(n)"]
    print(p.est)
    var1 <- VAR(residual.matrix.temp, p = p.est, type = "none")
    residual.hat[cp.residual[i]: (cp.residual[i+1]-1), ] <- rbind( matrix(0, ncol = 2, nrow = p.est), fitted(var1)) 
  }
  residual.tilde <- residual.matrix -  residual.hat
  coef.matrix <- Bcoef(var1)
}


filename <- paste0("var_coef_", county.lowernames[1], "_b_",  b_t, ".pdf")
pdf(filename, width = 11, height = 8.5)
par(mar = c(4., 4.5, 1.5, 1))
print(plot.ar.matrix(coef.matrix, p = p.est))
dev.off()

filename <- paste0("acf_residual_tilde_Delta_I_", county.lowernames[1], "_b_",  b_t, ".pdf")
pdf(filename, width=11, height = 8.5)
par(mar = c(4.8, 6, 1.7, 1.5), mgp=c(3.5, 1.1, 0))
acf(residual.tilde[, 1], cex.lab = 3 , cex.axis = 3, lwd = 3)
dev.off()

filename <- paste0("acf_residual_tilde_Delta_R_", county.lowernames[1], "_b_",  b_t, ".pdf")
pdf(filename, width=11, height=8.5)
par(mar = c(4.8, 6, 1.7, 1.5), mgp=c(3.5, 1.1, 0))
acf(residual.tilde[, 2], cex.lab = 3 , cex.axis = 3, lwd = 3)
dev.off()

#vectorize the fitted residuals
residual.hat.vec <- rep(0, nrow(residual.hat)*2)
#fitted residual of Delta R
residual.hat.vec[seq(1, nrow(residual.hat)*2, 2)] <- residual.hat[, 2]
#fitted residual of Delta I
residual.hat.vec[seq(2, nrow(residual.hat)*2, 2)] <- residual.hat[, 1]


Y.hat.3 <- Y.hat.2.3 + residual.hat.vec/n.all[1]

R.hat.3 <- rep(0, T)
R.hat.3[1] <- R.rate.all[1, 1]
for(i in 2:T){
  R.hat.3[i] <- R.rate.all[i-1, 1] + Y.hat.3[(i-2)*2+1]
}
R.hat.3 <- R.hat.3*n.all[1]

# I.hat.3 <- rep(0,T)
# I.hat.3[1] <- I.rate.all[1,1]
# for(i in 2:T){
#   I.hat.3[i] <-  I.rate.all[i-1,1] + Y.hat.3[(i-2)*2+2]
# }
# I.hat.3 <- I.hat.3*n.all[1]
# 
# 
# MRPE_3_I <- mean(  abs ( (     c(I.hat.3[-1]) - c(I[-1])     )  /c(I[-1])  )[intersect( which(c(I[-1]) > 0  ), 2:(T-1) ) ]  )
# print(round(MRPE_3_I,4))
# MRPE_3_R <- mean(  abs ( (     c(R.hat.3[-1]) - c(R[-1])     )  /c(R[-1])  )[intersect( which(c(R[-1]) > 0  ), 2:(T-1) ) ]  )
# print(round(MRPE_3_R,4))


I.hat.obs.3 <- rep(0,T)
I.hat.obs.3[1] <- I.rate.all.obs[1,1]
for(i in 2:T){
  rate <- (((i)+a.val*T.full)/((1 + a.val )*T.full))^2
  I.hat.obs.3[i] <-  I.rate.all.obs[i-1,1] + Y.hat.3[(i-2)*2+2]*rate
}
I.hat.obs.3 <- I.hat.obs.3*n.all[1]


MRPE_3_I <- mean(  abs ( (     c(I.hat.obs.3[-1]) - c(I.obs[-1]))  /c(I.obs[-1])  )[intersect( which(c(I.obs[-1]) > 0  ), 2:(T-1) ) ]  )
print(round(MRPE_3_I,4))
MRPE_3_R <- mean(  abs ( (     c(R.hat.3[-1]) - c(R[-1])     )  /c(R[-1])  )[intersect( which(c(R[-1]) > 0  ), 2:(T-1) ) ]  )
print(round(MRPE_3_R,4))

R.tilde.3 <- rep(0,T)
R.tilde.3[1] <- R.rate.all[1,1]
for(i in 2:T){
  R.tilde.3[i] <- R.tilde.3[i-1] + Y.hat.3[(i-2)*2+1]
}
R.tilde.3 <- R.tilde.3*n.all[1]

# I.tilde.3 <- rep(0,T)
# I.tilde.3[1] <- I.rate.all[1,1]
# for(i in 2:T){
#   I.tilde.3[i] <- I.tilde.3[i-1] + Y.hat.3[(i-2)*2+2]
# }
# I.tilde.3 <- I.tilde.3*n.all[1]

I.tilde.3 <- rep(0,T)
I.tilde.3[1] <- I.rate.all[1,1]
for(i in 2:T){
  rate <- (((i)+a.val*T.full)/((1 + a.val )*T.full))^2
  I.tilde.3[i] <- I.tilde.3[i-1] + Y.hat.3[(i-2)*2+2]*rate
}
I.tilde.3 <- I.tilde.3*n.all[1]



filename <- paste0("Infected_", county.lowernames[1] ,"_3.pdf")
pdf(filename, width=11, height=8.5)
par(mar = c(4.5, 6, 2.8, 1.5), mgp=c(3.5, 1.2, 0))
plot(date.region, I.obs, col='1', ylim=c(0,max(I.obs, I.tilde.3)), lty=1, type="l", lwd = 3,
     ylab ='Number of Infected Cases', xlab= 'Date',cex.lab = 3, cex.axis = 3)
lines(date.region, I.tilde.3, col='2', lty=1, type="l", lwd = 3)
legend(as.Date(date.region[length(date.region)*3/4]), 1/4*max(I.obs, I.tilde.3), 
       legend=c(expression(I(t)), expression(tilde(I)(t))),
       col=c( 1,2), bg="transparent", bty = "n", 
       lwd = 3, cex = 2, pt.cex = 2, seg.len=1.5, y.intersp=1 , x.intersp=1)
dev.off()


filename <- paste0("Recovered_", county.lowernames[1], "_3.pdf")
pdf(filename, width=11, height=8.5)
par(mar = c(4.5, 6, 2.8, 1.5), mgp=c(3.5, 1.2, 0))
plot(date.region, R, type='l',col='1', ylim=c(0,max(R, R.tilde.3)), lty=1, lwd = 3,
     ylab ='Number of Recovered Cases', xlab= 'Date', cex.lab = 3 , cex.axis = 3)
lines(date.region, R.tilde.3, col='2',lty=1,type="l",lwd = 3)
legend(as.Date(date.region[length(date.region)*3/4]), 1/4*max(R, R.tilde.3), legend=c(expression(R(t)), expression(tilde(R)(t))),
       col=c( 1,2), bg="transparent", bty = "n", 
       lwd = 3, cex = 2, pt.cex = 2, seg.len=1.5, y.intersp=1, x.intersp=1)
dev.off()

##################################################
# model 2.4 : similarity based weight, 
# use all the states 
#################################################
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
matrix.combined <- matrix(0, nrow = rows.combined, ncol = cols.combined)
matrix.combined[ seq(1, rows.combined, 2),] <- as.matrix(unname(R.rate.all.full))[-1, ] - as.matrix(unname(R.rate.all.full))[-T, ]
matrix.combined[ seq(2, rows.combined, 2),] <- as.matrix(unname(I.rate.all.full))[-1, ] - as.matrix(unname(I.rate.all.full))[-T, ]

neighbor.matrix <- matrix.combined
neighbor.weighted_4 <- neighbor.matrix %*% (Omega_4)

#remove the last two elements  at time point t=T
#and add two elements for time point t= 0
neighbor.weighted_4 <- as.matrix(neighbor.weighted_4[-c(rows.combined - 1, rows.combined), ])
neighbor.weighted_4 <- as.matrix(c(neighbor.weighted_4[c(1, 2), ], neighbor.weighted_4))


p.x <- ncol(X)
p.y <-ncol(Y)
n <- nrow(X)



cp <- c(1, temp.1$cp.final, n+1)
m <- length(cp) - 1
X.new.4 <- matrix(0, nrow = n, ncol = m*p.x + 1)
for(i in 1:m){
  X.new.4[cp[i]: (cp[i+1]-1), (p.x*(i-1)+1) : (p.x*i) ] <- X[cp[i]: (cp[i+1]-1), ]
}
X.new.4[, m*p.x + 1] <- neighbor.weighted_4


est.2.4 <- lm(Y[-c(1:2), ] ~ X.new.4[-c(1:2), ]  - 1)
Y.hat.2.4 <- c(0, 0, est.2.4 $fitted.values)
summary(est.2.4 )


R.hat.2.4 <- rep(0, T)
R.hat.2.4[1] <- R.rate.all[1, 1]
for(i in 2:T){
  R.hat.2.4[i] <- R.rate.all[i-1, 1] + Y.hat.2.4[(i-2)*2+1]
}
R.hat.2.4 <- R.hat.2.4*n.all[1]

# I.hat.2.4 <- rep(0,T)
# I.hat.2.4[1] <- I.rate.all[1,1]
# for(i in 2:T){
#   I.hat.2.4[i] <-  I.rate.all[i-1,1] + Y.hat.2.4[(i-2)*2+2]
# }
# I.hat.2.4 <- I.hat.2.4*n.all[1]
# 
# 
# MRPE_2.4_I <- mean(  abs ( (     c(I.hat.2.4[-1]) - c(I[-1])     )  /c(I[-1])  )[intersect( which(c(I[-1]) > 0  ), 2:(T-1) ) ]  )
# print(round(MRPE_2.4_I,4))
# MRPE_2.4_R <- mean(  abs ( (     c(R.hat.2.4[-1]) - c(R[-1])     )  /c(R[-1])  )[intersect( which(c(R[-1]) > 0  ), 2:(T-1) ) ]  )
# print(round(MRPE_2.4_R,4))

I.hat.obs.2.4 <- rep(0,T)
I.hat.obs.2.4[1] <- I.rate.all.obs[1,1]
for(i in 2:T){
  rate <- (((i)+a.val*T.full)/((1 + a.val )*T.full))^2
  I.hat.obs.2.4[i] <-  I.rate.all.obs[i-1,1] + Y.hat.2.4[(i-2)*2+2]*rate
}
I.hat.obs.2.4 <- I.hat.obs.2.4*n.all[1]


MRPE_2.4_I <- mean(  abs ( (     c(I.hat.obs.2.4[-1]) - c(I.obs[-1]))  /c(I.obs[-1]))[intersect( which(c(I.obs[-1]) > 0  ), 2:(T-1) ) ]  )
print(round(MRPE_2.4_I,4))
MRPE_2.4_R <- mean(  abs ( (     c(R.hat.2.4[-1]) - c(R[-1])     )  /c(R[-1])  )[intersect( which(c(R[-1]) > 0  ), 2:(T-1) ) ]  )
print(round(MRPE_2.4_R,4))



R.tilde.2.4 <- rep(0,T)
R.tilde.2.4[1] <- R.rate.all[1,1]
for(i in 2:T){
  R.tilde.2.4[i] <- R.tilde.2.4[i-1] + Y.hat.2.4[(i-2)*2+1]
}
R.tilde.2.4 <- R.tilde.2.4*n.all[1]

I.tilde.2.4 <- rep(0,T)
I.tilde.2.4[1] <- I.rate.all.obs[1,1]
for(i in 2:T){
  rate <- (((i)+a.val*T.full)/((1 + a.val )*T.full))^2
  I.tilde.2.4[i] <- I.tilde.2.4[i-1] + Y.hat.2.4[(i-2)*2+2]*rate
}
I.tilde.2.4 <- I.tilde.2.4*n.all[1]

# I.tilde.2.4 <- rep(0,T)
# I.tilde.2.4[1] <- I.rate.all[1,1]
# for(i in 2:T){
#   I.tilde.2.4[i] <- I.tilde.2.4[i-1] + Y.hat.2.4[(i-2)*2+2]
# }
# I.tilde.2.4 <- I.tilde.2.4*n.all[1]


filename <- paste0("Infected_", county.lowernames[1] ,"_2.4.pdf")
pdf(filename, width=11, height=8.5)
par(mar = c(4.5, 6, 2.8, 1.5), mgp=c(3.5, 1.2, 0))
plot(date.region, I.obs, col='1', ylim=c(0,max(I.obs, I.tilde.2.4)), lty=1, type="l", lwd = 3,
     ylab ='Number of Infected Cases', xlab= 'Date',cex.lab=3, cex.axis=3)
lines(date.region, I.tilde.2.4, col='2', lty=1, type="l", lwd = 3)
legend(as.Date(date.region[length(date.region)*3/4]), 1/4*max(I.obs, I.tilde.2.4), 
       legend=c(expression(I(t)), expression(tilde(I)(t))),
       col=c( 1,2), bg="transparent", bty = "n", 
       lwd = 3, cex=2, pt.cex = 2, seg.len=1.5, y.intersp=1 , x.intersp=1)
dev.off()


filename <- paste0("Recovered_", county.lowernames[1], "_2.4.pdf")
pdf(filename, width=11, height=8.5)
par(mar = c(4.5, 6, 2.8, 1.5), mgp=c(3.5, 1.2, 0))
plot(date.region, R, type='l',col='1', ylim=c(0,max(R, R.tilde.2.4)), lty=1, lwd = 3,
     ylab ='Number of Recovered Cases', xlab= 'Date', cex.lab=3, cex.axis=3)
lines(date.region, R.tilde.2.4, col='2',lty=1,type="l",lwd = 3)
legend(as.Date(date.region[length(date.region)*3/4]), 1/4*max(R, R.tilde.2.4), legend=c(expression(R(t)), expression(tilde(R)(t))),
       col=c( 1,2), bg="transparent", bty = "n", 
       lwd = 3, cex=2, pt.cex = 2, seg.len=1.5, y.intersp=1, x.intersp=1)
dev.off()

filename <- paste0(county.lowernames[1], "_domain.RData")
save.image(filename)

print(county.res, include.rownames = FALSE)


MRPE_1_I
MRPE_1_R
MRPE_2.1_I
MRPE_2.1_R
MRPE_2.2_I
MRPE_2.2_R
MRPE_2.3_I
MRPE_2.3_R
MRPE_2.4_I
MRPE_2.4_R
MRPE_3_I
MRPE_3_R





library("xtable")
countiesnames <- c("new york city", "king", "miami-dade")
MPE_res <- c()
for(i in 1:length(countiesnames)){
  filename <- paste0(countiesnames[i],"_domain.RData")
  load(filename)
  MPE_res <- cbind(MPE_res,  round(c(MRPE_1_I,
                                     MRPE_2.1_I, MRPE_2.2_I, 
                                     MRPE_2.3_I, MRPE_2.4_I, 
                                     MRPE_3_I), 4), 
                   round(c(MRPE_1_R,  
                           MRPE_2.1_R, MRPE_2.2_R, 
                           MRPE_2.3_R, MRPE_2.4_R, 
                           MRPE_3_R), 4))
}
MPE_res <- cbind(c("Model 1", "Model 2.1", "Model 2.2","Model 2.3", "Model 2.4", "Model 3"), MPE_res)
table.MPE_res <- xtable(MPE_res, hline.after = c(1,2))
print(table.MPE_res,include.rownames = FALSE, include.colnames = FALSE)





library("xtable")
countiesnames <- c("new york city", "king", "miami-dade", "orleans", "jefferson",
                   "charleston", "greenville" ,"richland", "horry"
)
MPE_res <- c()
for(i in 1:length(countiesnames)){
  filename <- paste0(countiesnames[i],"_domain.RData")
  load(filename)
  print(T-1)
  print(b_t)
  print(cp.date)
  print(county.res, include.rownames = FALSE)
}

rm(list=ls(all=TRUE))
gc()
a.res <- c()
library("xtable")
library(stringr) 
countiesnames <- c("new york city", "king", "miami-dade", "orleans", "jefferson",
                   "charleston", "greenville" ,"richland", "horry"
)
for(i in 1:length(countiesnames)){
  filename <- paste0(countiesnames[i],"_domain.RData")
  load(filename)
  a.final
  a.res <- c(a.res, a.final)
}

a.res
cols <- brewer.pal(length(a.res), "Spectral")

filename <- paste0("underreport_counties.pdf")
pdf(filename, width=11, height=8.5)
par(mar = c(4.5, 6, 2, 1.5), mgp=c(3.5, 1.2, 0))
rate.full = 1- (((1:T.full)+ a.res[1]*T.full)/((1 + a.res[1] )*T.full))^2
plot( date.region, rate.full[1:T],  type = 'l', col = cols[1], lty = 1, lwd = 3,
      ylab = 'Underreporting rate', xlab = 'Date', cex.lab = 3, cex.axis = 3, ylim = c(0, 1))
for(i in 2:length(a.res)){
  rate.full = 1- (((1:T.full)+ a.res[i]*T.full)/((1 + a.res[i] )*T.full))^2
  lines( date.region, rate.full[1:T], col = cols[i], lty = i, type = "l", lwd = 3 )
}
legend(as.Date(date.region[length(date.region)*3/4]), 1, 
       legend = str_to_title(countiesnames) ,
       col = cols, bty = "n", lwd = 3, cex= 2, pt.cex = 2, bg = "transparent",
       seg.len=1.5, y.intersp=1 , x.intersp=1)
dev.off()



