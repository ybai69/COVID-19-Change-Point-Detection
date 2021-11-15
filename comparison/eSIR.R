rm(list=ls())
library(devtools)
#install_github("lilywang1988/eSIR")
library(eSIR)
#install.packages("ellipsis")
set.seed(20192020)
library(eSIR)

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

data.states <- read.csv("states-05-07.csv", header = TRUE)
data.states <- data.states[as.Date(data.states$date) >= as.Date('2020-03-01'),]
data.states <- data.states[as.Date(data.states$date) <= as.Date('2021-04-13'),]
data.states$date <-  as.Date(data.states$date)

# population data extracted from NATIONAL BUREAU OF ECONOMIC RESEARCH
# states.population <- as.data.frame(data.table::fread("https://www2.census.gov/programs-surveys/popest/datasets/2010-2019/counties/totals/co-est2019-alldata.csv"))
states.population <- read.csv("co-est2019-alldata.csv", header = TRUE)
states.population <- states.population[as.character(states.population$STNAME) == as.character(states.population$CTYNAME),
                                       c("STNAME", "CTYNAME", "POPESTIMATE2019")]

# distance data extracted from US Census Bureau 
# states.distance<- as.data.frame(data.table::fread("http://data.nber.org/distance/2010/sf1/state/sf12010statedistancemiles.csv"))
states.distance <-read.csv("sf12010statedistancemiles.csv", header = TRUE)

state.name <- "New York"
Date.1 <- '2020-03-22'
#Date.2 <- '2020-05-15'
# phase 3 reopen date
Date.2 <- '2020-07-06'

state.name <- "Oregon"
Date.1 <- '2020-03-23'
# phase 1 reopen date
Date.2 <- '2020-05-15'

state.name <- "Florida"
Date.1 <- '2020-04-03'
# phase 1 reopen date
# Date.2 <- '2020-05-04'
# phase 2 reopen date
Date.2 <- '2020-06-05'

state.name <- "California"
Date.1 <- '2020-03-19'
# phase 1 reopen date
#Date.2 <- '2020-05-08'
# phase 2 reopen date
Date.2 <- '2020-06-12'

state.name <- "Texas"
Date.1 <- '2020-04-02'
# phase 1 reopen date
# Date.2 <- '2020-04-30'
# phase 3 reopen date
Date.2 <- '2020-06-03'


print("first updated date:"); print(min(data.states$date))
print("last updated date:"); print(max(data.states$date))

#############################################
lambda.1 <- c(1, 0.5, 0.1, 0.05, 0.01, 0.005, 0.001)


# n.test days for prediction
n.test <- 14-1

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
R.all <- floor((1 + (10))*deaths.all);
if( state.name == "New York"){
  R.all <- floor((1 + (5.5))*deaths.all);
}

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
S.rate.all.obs <- S.rate.all

# I <- I.all[,1]
# R <- R.all[,1]
date.region <- multi_full$date
I.obs <- I.all[,1]
R.obs <- R.all[,1]
cols <- brewer.pal(n.domains, "Spectral")





NI_complete <- I.obs
RI_complete <- R.obs
N <- n.all[1]

plot(NI_complete)
plot(RI_complete)

R <- RI_complete / N
Y <- NI_complete / N 
Y[Y < 0] <- 1/N

### Step function of pi(t)
if(state.name == "New York"){
  change_time <- c("04/04/2020")
  pi0 <- c(1.0, 0.5)
  casename <- "NY_step"
}
if(state.name == "Oregon"){
  change_time <- c("04/06/2020", "06/06/2020", "07/18/2020", "01/16/2021")
  pi0 <- c(1.0, 0.5, 0.8, 0.6, 0.5)
  casename <- "OR_step"
}
if(state.name == "Florida"){
  change_time <- c("04/13/2020", "06/17/2020", "07/25/2020")
  pi0 <- c(1.0, 0.5, 0.8, 0.6)
  casename <- "FL_step"
}
if(state.name == "California"){
  change_time <- c("04/11/2020", "04/29/2020", "08/12/2020", "12/06/2020", "01/19/2021")
  pi0 <- c(1.0, 0.8, 0.6, 0.5, 0.7, 0.5)
  casename <- "CA_step"
}
if(state.name == "Texas"){
  change_time <- c("04/18/2020", "06/03/2020", "06/15/2020", "07/23/2020",
                   "01/03/2021", "02/10/2021")
  pi0 <- c(1.0, 0.6, 0.7, 0.8, 0.6, 0.7, 0.6)
  casename <- "TX_step"
}
if(state.name == "California"){
  change_time <- c("04/11/2020", "04/29/2020", "08/12/2020", "12/06/2020", "01/19/2021")
  pi0 <- c(1.0, 0.8, 0.6, 0.5, 0.7, 0.5)
  casename <- "CA_step"
}


ptm <- proc.time()

res_infect <- rep(0, n.test)
res_remove <- rep(0, n.test)

for(i in 1:n.test){
  res.step <- tvt.eSIR(Y[1: (396+i-1)], R[1:(396+i-1)], begin_str = "03/01/2020", T_fin = 396+(i-1) + 2,
                       pi0 = pi0, change_time = change_time, dic = T, 
                       casename = casename, save_files = T, save_mcmc=F,
                       save_plot_data = F, M = 5e3, nburnin = 2e3)
  res_infect_temp <- tail(res.step$plot_infection$data$y, 4)
  res_remove_temp <- tail(res.step$plot_removed$data$y, 4)
  res_infect[i] <- (res_infect_temp[1] + res_infect_temp[3])/2
  res_remove[i] <- (res_remove_temp[1] + res_remove_temp[3])/2
  
}

comp.time <- proc.time() - ptm

Y.test<- Y[397:length(Y)]
R.test <- R[397:length(Y)]






MRPE_I <- mean(  abs ( (  c(res_infect) - c(Y.test)     )  /c(Y.test)  )[c(Y.test) > 0]  )
print(round(MRPE_I, 4))
MRPE_R <- mean(  abs ( (  c(res_remove) - c(R.test)     )  /c(R.test)  )[c(R.test) > 0]  )
print(round(MRPE_R, 4))



filename <- paste0(state.lowernames[1], "_eSIR_prediction_new.RData")
save.image(filename)



#####################################

rm(list=ls())
load("new york_eSIR_prediction_new.RData")
load("oregon_eSIR_prediction_new.RData")
load("florida_eSIR_prediction_new.RData")
load("california_eSIR_prediction_new.RData")
load("texas_eSIR_prediction_new.RData")
print(round(MRPE_I, 4))
print(round(MRPE_R, 4))


MRPE_I <- mean(  abs ( (  c(res_infect) - c(Y.test)     )  /c(Y.test)  )[c(Y.test) > 0]  )
print(round(MRPE_I, 4))
MRPE_R <- mean(  abs ( (  c(res_remove) - c(R.test)     )  /c(R.test)  )[c(R.test) > 0]  )
print(round(MRPE_R, 4))




sd_I <- sd(  abs ( (  c(res_infect) - c(Y.test)     )  /c(Y.test)  )[c(Y.test) > 0]  )
print(round(sd_I, 4))
sd_R <- sd(  abs ( (  c(res_remove) - c(R.test)     )  /c(R.test)  )[c(R.test) > 0]  )
print(round(sd_R, 4))









