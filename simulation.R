rm(list=ls(all=TRUE))
gc()

######## Loading Packages #######################
library("factoextra")
library("Rcpp")
library("RcppArmadillo")
library("RColorBrewer")
library("lattice")
library("mvtnorm")
library("vars")
######## Call Functions #########################
source("functions_BFL.R")
sourceCpp("functions_BFL.cpp")

#########Change/choose the scenario number and block size paramter below!!!!!!
# Scenario A, B, C, D, E, F-> 1, 2, 3, 4, 5, 6
# sim 1: SIR + spatial + VAR(p)
sim <- 1
#sim 2: exp function for underreporting rate
sim <- 2
#sim 3: quadratic function + spatial + VAR(p)
sim <- 3
# sim 4:  SIR
sim <- 4
# sim 5: SIR + spatial 
sim <- 5
#sim 6: quadratic function for underreporting rate
sim <- 6


#b_t is the block size among the time points (1 : (length(date) - 1))
b_t <- 4
b_t <- 8
b_t <- 12


######## General Parameters #########################

if(sim %in% c(1, 3, 5)){
    T <- 200
    brk <- c( floor(T/2), T+1)
    beta_1<- 0.10; beta_2 <- 0.05
    gamma_1 <- 0.04; gamma_2 <- 0.04
    beta <- c(rep(beta_1, brk[1] - 1), rep(beta_2, T - brk[1] ))
    gamma <- c(rep(gamma_1, brk[1] - 1), rep(gamma_2, T - brk[1]))
    
    alpha <- 1
    beta.neighbor <- c(0.10, 0.05)
    gamma.neighbor <- c(0.04, 0.04)
    beta.neighbor <- seq(beta.neighbor[1], beta.neighbor[2], length.out = T-1 )
    gamma.neighbor <- seq(gamma.neighbor[1], gamma.neighbor[2], length.out = T-1)
    
    
}else if(sim %in% c(4)){
    #sim 4: two change points
    T <- 250
    brk <- c( floor(T/5*2), floor(T/5*4), T+1)
    sd.log <- 0.01^0.5
    beta_1 <- 0.10; beta_2 <- 0.05; beta_3 <- 0.10
    gamma_1 <- 0.04; gamma_2 <- 0.06; gamma_3 <- 0.04
    beta <- c(rep(beta_1, brk[1] - 1), rep(beta_2, (brk[2] - 1) - (brk[1] - 1) ), rep(beta_3, T - brk[2]) )
    gamma <- c(rep(gamma_1, brk[1] - 1), rep(gamma_2,  (brk[2] - 1) - (brk[1] - 1) ), rep(gamma_3, T - brk[2]) )
    
}else if(sim %in% c(2, 6)){
    T <- 250
    T.train <- 250
    brk <- c( floor(T/5*2), floor(T/5*4), T+1)
    sd.log <- 0.005^0.5
    beta_1 <- 0.10; beta_2 <- 0.05; beta_3 <- 0.10
    gamma_1 <- 0.04; gamma_2 <- 0.06; gamma_3 <- 0.04
    beta <- c(rep(beta_1, brk[1] - 1), rep(beta_2, (brk[2] - 1) - (brk[1] - 1) ), rep(beta_3, T - brk[2]) )
    gamma <- c(rep(gamma_1, brk[1] - 1), rep(gamma_2,  (brk[2] - 1) - (brk[1] - 1) ), rep(gamma_3, T - brk[2]) )
    
}



if(sim %in% c(1, 3, 5)){
    lambda.1 <- NULL
}else{
    lambda.1 <- c(0.01, 0.005, 0.001, 0.0005, 0.0001)
    
}


##################################
# number of replicates
N <- 100

lm.res <- vector("list", N); var.coef.res <- vector("list", N); 

Y.std.res <- vector("list", N); neighbor.std.res <- vector("list", N);

pts.final <- vector("list", N);
a.final <- vector("list", N); b.final <- vector("list", N);
MRPE_Delta.final <- vector("list", N);


if(sim %in% c(1, 3) ){
    MRPE_1_Delta.final <- rep(0, N); MRPE_1_I_Delta.final <- rep(0, N); MRPE_1_R_Delta.final <- rep(0, N); 
    MRPE_2_Delta.final <- rep(0, N); MRPE_2_I_Delta.final <- rep(0, N); MRPE_2_R_Delta.final <- rep(0, N); 
    MRPE_3_Delta.final <- rep(0, N); MRPE_3_I_Delta.final <- rep(0, N); MRPE_3_R_Delta.final <- rep(0, N); 
    
    MRPE_1.final <- rep(0, N); MRPE_1_I.final <- rep(0, N); MRPE_1_R.final <- rep(0, N); 
    MRPE_2.final <- rep(0, N); MRPE_2_I.final <- rep(0, N); MRPE_2_R.final <- rep(0, N); 
    MRPE_3.final <- rep(0, N); MRPE_3_I.final <- rep(0, N); MRPE_3_R.final <- rep(0, N); 
    
}
if(sim %in% c(3) ){
    MRPE_1.final.true <- rep(0, N); MRPE_1_I.final.true <- rep(0, N); MRPE_1_R.final.true <- rep(0, N); 
    MRPE_2.final.true <- rep(0, N); MRPE_2_I.final.true <- rep(0, N); MRPE_2_R.final.true <- rep(0, N); 
    MRPE_3.final.true <- rep(0, N); MRPE_3_I.final.true <- rep(0, N); MRPE_3_R.final.true <- rep(0, N); 
    
}


for ( j.1 in 1:N){
    set.seed(12345*j.1)
    if( sim %in% c(4)){
        I <- rep(0, T)
        R <- rep(0, T)
        beta_t <- c()
        gamma_t <- c()
        I[1] <- 1
        for(i in 2:T){
            temp.1 <- rlnorm(1, meanlog = log(beta[i-1]), sdlog = sd.log )
            temp.2 <- rlnorm(1, meanlog = log(gamma[i-1]), sdlog = sd.log)
            beta_t <- c(beta_t, temp.1)
            gamma_t <- c(gamma_t, temp.2)
            if(i == 2){
                I[i] <- temp.1*I[i-1] - temp.2[1]*I[i-1] + I[i-1]
                R[i] <- temp.2[1]*I[i-1] + R[i-1] 
            }else{
                I[i] <- temp.1*I[i-1] - temp.2*I[i-1] + I[i-1] 
                R[i] <- temp.2*I[i-1] + R[i-1]
            }
        }
        plot(1:T, I)
        plot(1:T, R)
        
    }
    if( sim %in% c(5)){
        data_e <- rmvnorm(T-1, mean = rep(0, 2), sigma = 1*diag(2))
        
        I.neighbor <- rep(0, T)
        R.neighbor <- rep(0, T)
        I.neighbor[1] <- 1
        beta_t.neighbor <- c()
        gamma_t.neighbor <- c()
        for(i in 2:T){
            temp.1 <- beta.neighbor[i-1]
            temp.2 <- gamma.neighbor[i-1]
            beta_t.neighbor <- c(beta_t.neighbor, temp.1)
            gamma_t.neighbor <- c(gamma_t.neighbor, temp.2)
            I.neighbor[i] <- (temp.1*I.neighbor[i-1] - temp.2*I.neighbor[i-1] + I.neighbor[i-1])
            R.neighbor[i] <- (temp.2*I.neighbor[i-1] + R.neighbor[i-1])
        }
        
        plot(1:T, I.neighbor)
        plot(1:T, R.neighbor)
        
        I<-rep(0, T)
        R<-rep(0, T)
        beta_t <- c()
        gamma_t <- c()
        I[1] <- 10
        for(i in 2:T){
            temp.1 <- beta[i-1]
            temp.2 <- gamma[i-1]
            beta_t <- c(beta_t, temp.1)
            gamma_t <- c(gamma_t, temp.2)
            if(i == 2){
                I[i] <- (temp.1*I[i-1] - temp.2[1]*I[i-1] + I[i-1] + alpha*(I.neighbor[i] - I.neighbor[i-1])) + data_e[1, 1] 
                R[i] <- (temp.2[1]*I[i-1] + R[i-1] + alpha*(R.neighbor[i] - R.neighbor[i-1]  )) + data_e[1, 2] 
            }else{
                I[i] <- (temp.1*I[i-1] - temp.2*I[i-1] + I[i-1] + alpha*(I.neighbor[i-1] - I.neighbor[i-2])) + data_e[i-1, 1] 
                R[i] <- (temp.2*I[i-1] + R[i-1] + alpha*(R.neighbor[i-1] - R.neighbor[i-2]))  + data_e[i-1, 2]
            }
            
        }
        
        plot(1:T, I)
        plot(1:T, R)
    }
    if( sim %in% c(1)){
        q.t <- 1; p <- 2
        phi.full <- matrix(0, p, p)
        phi.full[1, 1] <- 0.8
        phi.full[2, 2] <- 0.7
        phi.full[1, 2] <- 0
        phi.full[2, 1] <- 0.2
        print(plot.ar.matrix((phi.full), p = 1))
        
        phi.full%*%phi.full%*%phi.full%*%phi.full%*%phi.full%*%phi.full%*%phi.full%*%phi.full%*%phi.full%*%phi.full%*%phi.full%*%phi.full%*%phi.full%*%phi.full%*%phi.full%*%phi.full%*%phi.full%*%phi.full%*%phi.full%*%phi.full%*%phi.full%*%phi.full%*%phi.full%*%phi.full%*%phi.full%*%phi.full%*%phi.full%*%phi.full%*%phi.full%*%phi.full%*%phi.full%*%phi.full%*%phi.full%*%phi.full
        
        e.sigma <- as.matrix(0.1*diag(p));
        # e.sigma <- as.matrix(1*diag(p));
        t.test <- 20
        # try = var.sim.break(T-1, arlags=seq(1, q.t, 1), malags = NULL, phi = phi.full, sigma = e.sigma, skip = 1, brk = c(T))
        try = var.sim.break(T - 1 + t.test, arlags=seq(1, q.t, 1), malags = NULL, phi = phi.full, sigma = e.sigma, skip = 1, brk = c(T + t.test))
        data_e <- try$series
        data_e <- as.matrix(data_e, ncol = p)
        
        
        data_e_vec <-  rep(0, nrow(data_e)*2)
        data_e_vec[seq(1, nrow(data_e)*2, 2)] <- data_e[, 1]
        data_e_vec[seq(2, nrow(data_e)*2, 2)] <- data_e[, 2]
        data_e
        VAR(data_e, p = q.t, type = "none")
        
        
        I.neighbor <- rep(0, T + t.test)
        R.neighbor <- rep(0, T + t.test)
        I.neighbor[1] <- 1
        beta_t.neighbor <- c()
        gamma_t.neighbor <- c()
        for(i in 2:(T + t.test)){
            if(i > T){
                temp.1 <- beta.neighbor[T-1]
                temp.2 <- gamma.neighbor[T-1]
            }else{
                temp.1 <- beta.neighbor[i-1]
                temp.2 <- gamma.neighbor[i-1]
            }
            
            beta_t.neighbor <- c(beta_t.neighbor, temp.1)
            gamma_t.neighbor <- c(gamma_t.neighbor, temp.2)
            I.neighbor[i] <- (temp.1*I.neighbor[i-1] - temp.2*I.neighbor[i-1] + I.neighbor[i-1])
            R.neighbor[i] <- (temp.2*I.neighbor[i-1] + R.neighbor[i-1])
        }
        
        plot(1:(T + t.test), I.neighbor)
        plot(1:(T + t.test), R.neighbor)
        
        I<-rep(0, (T + t.test))
        R<-rep(0, (T + t.test))
        beta_t <- c()
        gamma_t <- c()
        I[1] <- 10
        for(i in 2:(T + t.test)){
            if(i > T){
                temp.1 <- beta[T-1]
                temp.2 <- gamma[T-1]
            }else{
                temp.1 <- beta[i-1]
                temp.2 <- gamma[i-1]
            }
            
            
            beta_t <- c(beta_t, temp.1)
            gamma_t <- c(gamma_t, temp.2)
            if(i == 2){
                I[i] <- (temp.1*I[i-1] - temp.2[1]*I[i-1] + I[i-1] + alpha*(I.neighbor[i] - I.neighbor[i-1])) + data_e[i-1, 2]
                R[i] <- (temp.2[1]*I[i-1] + R[i-1] + alpha*(R.neighbor[i] - R.neighbor[i-1]  )) + data_e[1, 1]
            }else{
                I[i] <- (temp.1*I[i-1] - temp.2*I[i-1] + I[i-1] + alpha*(I.neighbor[i-1] - I.neighbor[i-2])) + data_e[i-1, 2]
                R[i] <- (temp.2*I[i-1] + R[i-1] + alpha*(R.neighbor[i-1] - R.neighbor[i-2])) + data_e[i-1, 1]
            }
            
        }

        
        
        plot(1:(T + t.test), I)
        plot(1:(T + t.test), R)
        
        
        
    }
    if( sim %in% c(6)){
        I <- rep(0, T)
        R <- rep(0, T)
        beta_t <- c()
        gamma_t <- c()
        I[1] <- 1
        for(i in 2:T){
            temp.1 <- rlnorm(1, meanlog = log(beta[i-1]), sdlog = sd.log )
            temp.2 <- rlnorm(1, meanlog = log(gamma[i-1]), sdlog = sd.log)
            beta_t <- c(beta_t, temp.1)
            gamma_t <- c(gamma_t, temp.2)
            if(i == 2 ){
                I[i] <- temp.1*I[i-1] - temp.2*I[i-1] + I[i-1]
                R[i] <- temp.2*I[i-1] + R[i-1] 
            }else{
                I[i] <- temp.1*I[i-1] - temp.2*I[i-1] + I[i-1] 
                R[i] <- temp.2*I[i-1] + R[i-1] 
            }
        }
        
        I.true <- I
        R.true <- R
        plot(1:T, I.true)
        plot(1:T, R.true)
        # case 1
        a <- 0.5
        b <- 0.0
        # if there is underreport issue
        for(t in 1:T){
            rate <- ((t + a*T)/( (1+ a)*T))^2
            print(rate)
            if(t < 2){
                I[t] <- I.true[t]
                R[t] <- R.true[t]
                
            }else{
                I[t] <- (I.true[t]-I.true[t-1])*rate + I[t-1]
            }
            
            
        }
        plot(1:T, 1- ((1:T + a*T)/( (1+ a)*T))^2, ylab = "underreporting rate", xlab = 't')
        plot(1:T, I)
        plot(1:T, R)
    }
    if( sim %in% c(2)){
        I <- rep(0, T)
        R <- rep(0, T)
        beta_t <- c()
        gamma_t <- c()
        I[1] <- 1
        for(i in 2:T){
            temp.1 <- rlnorm(1, meanlog = log(beta[i-1]), sdlog = sd.log )
            temp.2 <- rlnorm(1, meanlog = log(gamma[i-1]), sdlog = sd.log)
            beta_t <- c(beta_t, temp.1)
            gamma_t <- c(gamma_t, temp.2)
            if(i == 2 ){
                I[i] <- temp.1*I[i-1] - temp.2*I[i-1] + I[i-1]
                R[i] <- temp.2*I[i-1] + R[i-1] 
            }else{
                I[i] <- temp.1*I[i-1] - temp.2*I[i-1] + I[i-1] 
                R[i] <- temp.2*I[i-1] + R[i-1] 
            }
        }
        
        I.true <- I
        R.true <- R
        plot(1:T, I.true)
        plot(1:T, R.true)
        
        a <- 1/100*5
        b <- 10
        # if there is underreport issue
        for(t in 1:T){
            rate <- 1/(1+ b*exp(-a*(t-1)) )
            print(rate)
            if(t < 2){
                I[t] <- I.true[t]
                R[t] <- R.true[t]
                
            }else{
                I[t] <- (I.true[t]-I.true[t-1])*rate + I[t-1]
            }
            
            
        }
        plot(1:T, 1- 1/(1+ b*exp(-a*(1:T-1)) ), ylab = "underreporting rate", xlab = 't')
        plot(1:T, I)
        plot(1:T, R)
        
        
    }
    if( sim %in% c(3)){
        q.t <- 1; p <- 2
        phi.full <- matrix(0, p, p)
        phi.full[1, 1] <- 0.8
        phi.full[2, 2] <- 0.7
        phi.full[1, 2] <- 0
        phi.full[2, 1] <- 0.2
        print(plot.ar.matrix((phi.full), p = 1))
        
        phi.full%*%phi.full%*%phi.full%*%phi.full%*%phi.full%*%phi.full%*%phi.full%*%phi.full%*%phi.full%*%phi.full%*%phi.full%*%phi.full%*%phi.full%*%phi.full%*%phi.full%*%phi.full%*%phi.full%*%phi.full%*%phi.full%*%phi.full%*%phi.full%*%phi.full%*%phi.full%*%phi.full%*%phi.full%*%phi.full%*%phi.full%*%phi.full%*%phi.full%*%phi.full%*%phi.full%*%phi.full%*%phi.full%*%phi.full
        
        e.sigma <- as.matrix(0.1*diag(p));
        # e.sigma <- as.matrix(1*diag(p));
        t.test <- 20
        # try = var.sim.break(T-1, arlags=seq(1, q.t, 1), malags = NULL, phi = phi.full, sigma = e.sigma, skip = 1, brk = c(T))
        try = var.sim.break(T - 1 + t.test, arlags=seq(1, q.t, 1), malags = NULL, phi = phi.full, sigma = e.sigma, skip = 1, brk = c(T + t.test))
        data_e <- try$series
        data_e <- as.matrix(data_e, ncol = p)
        
        
        data_e_vec <-  rep(0, nrow(data_e)*2)
        data_e_vec[seq(1, nrow(data_e)*2, 2)] <- data_e[, 1]
        data_e_vec[seq(2, nrow(data_e)*2, 2)] <- data_e[, 2]
        data_e
        VAR(data_e, p = q.t, type = "none")
        
        
        I.neighbor <- rep(0, T + t.test)
        R.neighbor <- rep(0, T + t.test)
        I.neighbor[1] <- 1
        beta_t.neighbor <- c()
        gamma_t.neighbor <- c()
        for(i in 2:(T + t.test)){
            if(i > T){
                temp.1 <- beta.neighbor[T-1]
                temp.2 <- gamma.neighbor[T-1]
            }else{
                temp.1 <- beta.neighbor[i-1]
                temp.2 <- gamma.neighbor[i-1]
            }
            
            beta_t.neighbor <- c(beta_t.neighbor, temp.1)
            gamma_t.neighbor <- c(gamma_t.neighbor, temp.2)
            I.neighbor[i] <- (temp.1*I.neighbor[i-1] - temp.2*I.neighbor[i-1] + I.neighbor[i-1])
            R.neighbor[i] <- (temp.2*I.neighbor[i-1] + R.neighbor[i-1])
        }
        
        plot(1:(T + t.test), I.neighbor)
        plot(1:(T + t.test), R.neighbor)
        
        I<-rep(0, (T + t.test))
        R<-rep(0, (T + t.test))
        beta_t <- c()
        gamma_t <- c()
        I[1] <- 10
        for(i in 2:(T + t.test)){
            if(i > T){
                temp.1 <- beta[T-1]
                temp.2 <- gamma[T-1]
            }else{
                temp.1 <- beta[i-1]
                temp.2 <- gamma[i-1]
            }
            
            
            beta_t <- c(beta_t, temp.1)
            gamma_t <- c(gamma_t, temp.2)
            if(i == 2){
                I[i] <- (temp.1*I[i-1] - temp.2[1]*I[i-1] + I[i-1] + alpha*(I.neighbor[i] - I.neighbor[i-1])) + data_e[i-1, 2]
                R[i] <- (temp.2[1]*I[i-1] + R[i-1] + alpha*(R.neighbor[i] - R.neighbor[i-1]  )) + data_e[1, 1]
            }else{
                I[i] <- (temp.1*I[i-1] - temp.2*I[i-1] + I[i-1] + alpha*(I.neighbor[i-1] - I.neighbor[i-2])) + data_e[i-1, 2]
                R[i] <- (temp.2*I[i-1] + R[i-1] + alpha*(R.neighbor[i-1] - R.neighbor[i-2])) + data_e[i-1, 1]
            }
            
        }
        
        
        
        plot(1:(T + t.test), I)
        plot(1:(T + t.test), R)
        
        
        T.full <- T + t.test
        I.true <- I
        R.true <- R
        plot(1:T.full, I.true)
        plot(1:T.full, R.true)
        # case 1
        a <- 0.5
        b <- 0.0
        # if there is underreport issue
        for(t in 1:T.full){
            rate <- ((t + a*T.full)/( (1+ a)*T.full))^2
            print(rate)
            if(t < 2){
                I[t] <- I.true[t]
                R[t] <- R.true[t]
                
            }else{
                I[t] <- (I.true[t]-I.true[t-1])*rate + I[t-1]
            }
            
            
        }
        plot(1:T.full, 1- ((1:T.full + a*T.full)/( (1+ a)*T.full))^2, 
             ylab = "underreporting rate", xlab = 't')
        plot(1:T.full, I)
        plot(1:T.full, R)
        
        
        
    }
    
    
    if( j.1 == 1 & sim %in% c(1, 4, 5)){
        filename <- paste0("Sim_", sim ,".pdf")
        pdf(filename, width = 11, height = 8.5)
        ylim_max <- 0.15
        par(mar = c(4., 5, 1.5, 1))
        plot(1:length(beta_t), beta_t, type = "l", col = "dark orange", lwd=3, ylim = c(0,ylim_max),
             xlab = "t", ylab = "rate" , cex.lab=3 , cex.axis=3)
        lines(1:length(gamma_t), gamma_t,type = "l",col="dark green", lwd=3)
        legend( T*3/4, ylim_max, legend=c(expression(beta(t)),  expression(gamma(t)) ),
                col=c("dark orange", "dark green"), bty = "n", lwd = 3, cex=2, pt.cex = 2, bg="transparent",
                seg.len=1.5, y.intersp=1 , x.intersp=1)
        dev.off()
    }
    if( j.1 == 1 & sim == 1){
        filename <- paste0("Sim_", sim ,"_varcoef.pdf")
        pdf(filename, width = 11, height = 8.5)
        ylim_max <- 0.15
        par(mar = c(4., 4.5, 1.5, 1))
        print(plot.ar.matrix((phi.full), p = 1))
        dev.off()
    }
    if( j.1 == 1 & sim %in% c(6, 3)){
        filename <- paste0("Sim_", sim , "_a_", a, "_b_", b, ".pdf")
        pdf(filename, width = 11, height = 8.5)
        par(mar = c(4., 5, 1.5, 1))
        plot(1:T, 1- ((1:T + a*T)/( (1+ a)*T))^2, ylab = "underreporting rate", xlab = 't',lwd=3 , cex.lab=3 , cex.axis=3)
        dev.off()
        
        
        filename <- paste0("Sim_", sim , ".pdf")
        pdf(filename, width = 11, height = 8.5)
        ylim_max <- 0.15
        par(mar = c(4., 5, 1.5, 1))
        plot(1:length(beta_t), beta_t, type = "l", col = "dark orange", lwd=3, ylim = c(0,ylim_max),
             xlab = "t", ylab = "rate" , cex.lab=3 , cex.axis=3)
        lines(1:length(gamma_t), gamma_t,type = "l",col="dark green", lwd=3)
        legend( T*3/4, ylim_max, legend=c(expression(beta(t)),  expression(gamma(t)) ),
                col=c("dark orange", "dark green"), bty = "n", lwd = 3, cex=2, pt.cex = 2, bg="transparent",
                seg.len=1.5, y.intersp=1 , x.intersp=1)
        dev.off()
    }
    if( j.1 == 1 & sim %in% c(2)){
        filename <- paste0("Sim_", sim , "_a_", a, "_b_", b, ".pdf")
        pdf(filename, width = 11, height = 8.5)
        par(mar = c(4., 5, 1.5, 1))
        plot(1:T, 1- 1/(1+ b*exp(-a*(1:T-1)) ), ylab = "underreporting rate", xlab = 't',lwd=3 , cex.lab=3 , cex.axis=3)
        dev.off()
        
        
        filename <- paste0("Sim_", sim , ".pdf")
        pdf(filename, width = 11, height = 8.5)
        ylim_max <- 0.15
        par(mar = c(4., 5, 1.5, 1))
        plot(1:length(beta_t), beta_t, type = "l", col = "dark orange", lwd=3, ylim = c(0,ylim_max),
             xlab = "t", ylab = "rate" , cex.lab=3 , cex.axis=3)
        lines(1:length(gamma_t), gamma_t,type = "l",col="dark green", lwd=3)
        legend( T*3/4, ylim_max, legend=c(expression(beta(t)),  expression(gamma(t)) ),
                col=c("dark orange", "dark green"), bty = "n", lwd = 3, cex=2, pt.cex = 2, bg="transparent",
                seg.len=1.5, y.intersp=1 , x.intersp=1)
        dev.off()
        
    }
    if( (sim %in% c(5)) & (j.1 == 1)){
        filename <- paste0("Sim_", sim ,"_spatial.pdf")
        pdf(filename, width=11, height=8.5)
        ylim_max <- 0.2
        par(mar = c(4., 5, 1.5, 1))
        plot(1:length(beta_t.neighbor), beta_t.neighbor, type = "l", col = "dark orange", lwd = 3, ylim = c(0,ylim_max),
             xlab = "t", ylab = "rate" , cex.lab = 3 , cex.axis = 3)
        lines(1:length(gamma_t.neighbor), gamma_t.neighbor, type = "l", col = "dark green", lwd = 3)
        legend( T*3/4, ylim_max, legend=c(expression(beta(t)),  expression(gamma(t))),
                col = c("dark orange", "dark green"), bty = "n", lwd = 3, cex = 2, pt.cex = 2, bg = "transparent",
                seg.len = 1.5, y.intersp = 1 , x.intersp = 1)
        dev.off()
        
    }
    
    ############################################
    ######## construct varibles ################
    ############################################

    if(sim %in% c(6)){
        I.obs <- I
        R.obs <- R
        a.vals <- c(0.1, 0.25, 0.5, 0.75, 1)
        b.vals <- c(b)
        ab.vals <- expand.grid(a.vals, b.vals)
        MRPE_1_new.full <- c()
        temp.full <- vector("list", nrow(ab.vals));
        est.1.full <- vector("list", nrow(ab.vals));
        for(idx in 1:nrow(ab.vals)){
            a.val <- ab.vals[idx, 1]
            b.val <- ab.vals[idx, 2]
            I <-  I.obs
            R <-  R.obs
            for(t in 2:T){
                rate <- 1/((t + a.val*T)/( (1+ a.val)*T))^2
                # print(1/rate)
                I[t] <- (I.obs[t] - I.obs[t-1])*rate + I[t-1]
                
            }
            
            y.list <- vector("list", T-1);
            x.list <- vector("list", T-1);
            
            for(i in 2:T){
                y.list[[i-1]] <- matrix(c(R[i] - R[i-1], I[i] - I[i-1]), 2, 1);
                x.temp <- matrix(0, 2, 2);
                x.temp[1,2] <- I[i-1];
                x.temp[2,1] <- I[i-1];
                x.temp[2,2] <- -I[i-1];
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
            
            beta_t.obs <- sapply(1:length(y.list), function(jjj)  (y.list[[jjj]][1] + y.list[[jjj]][2])/I[jjj])
            gamma_t.obs <- sapply(1:length(y.list), function(jjj)  y.list[[jjj]][1]/I[jjj])
            
            plot(beta_t.obs, ylim = c(0, max(beta_t.obs)))
            lines(gamma_t.obs)
            
            
            Y_std <- sd(Y)
            Y_s <- scale(Y, center = FALSE, scale = apply(Y, 2, sd, na.rm = TRUE))
            
            X_std <- apply(X, MARGIN = 2, FUN = sd)
            X_s <- scale(X, center = FALSE, scale = apply(X, 2, sd, na.rm = TRUE))
            
            p.x <- ncol(X)
            p.y <- ncol(Y)
            n <- nrow(X)
            
            tol <- 10^(-4); # tolerance 
            max.iteration <- 200; # max number of iteration for the LASSO solution
            method <- c("MLR")
            #p is the number of variables for each day (I_t and R_t) 
            p <- 2
            b_n <- p * b_t
            HBIC = TRUE
            gamma.val <- 10
            temp <- tbfl(method, Y_s, X_s, lambda.1.cv = lambda.1, lambda.2.cv = 0, 
                         max.iteration = max.iteration, tol = tol, block.size = b_n, HBIC = HBIC, gamma.val = gamma.val)
            
            temp.full[[idx]] <- temp
            
            cp <- c(1, temp$cp.final, n + 1)
            m <- length(cp) - 1
            X.new.new <- matrix(0, nrow = n, ncol = m*p.x)
            for(i in 1:m){
                X.new.new[cp[i]: (cp[i+1]-1), (p.x*(i-1)+1) : (p.x*i) ] <- X[cp[i]: (cp[i+1]-1), ]
            }
            
            est.1 <- lm(Y ~ X.new.new  - 1)
            summary(est.1 )
            est.1.full[[idx]] <- est.1
            
            Y.hat.1 <- est.1$fitted.values
            R.hat.1<- rep(0, T)
            R.hat.1[1] <- R[1]
            for(i in 2:T){
                R.hat.1[i] <- R[(i - 1)] + Y.hat.1[(i - 2)*2 + 1]
            }
            
            I.hat.1 <- rep(0, T)
            I.hat.1[1] <- I[1]
            for(i in 2:T){
                I.hat.1[i] <- I[(i - 1)] + Y.hat.1[(i - 2)*2 + 2]
            }
            
            MRPE_1_new <- mean(  abs ( (    Y.hat.1[seq(2,n,2)] - Y[seq(2,n,2)] )  /c(Y[seq(2,n,2)])  )[c(Y[seq(2,n,2)]) > 0]  )
            MRPE_1_new.full <- c(MRPE_1_new.full, MRPE_1_new)
        }
        
        idx <- which.min(MRPE_1_new.full)
        cp.final <- temp.full[[idx]]$cp.final 
        cp.date <- c(1:n)[floor( (cp.final-1) / p) + 1]
        pts.final[[j.1]] <- cp.date;
        a.final[[j.1]] <- ab.vals[idx, 1]
        b.final[[j.1]] <- ab.vals[idx, 2]
        lm.res[[j.1]] <- est.1.full[[idx]]
        MRPE_Delta.final[[j.1]] <- MRPE_1_new.full
        
    }
    if(sim %in% c(2)){
        I.obs <- I
        R.obs <- R
        a.vals <- c(1, 0.5, 0.1, 0.05, 0.01, 0.005)
        b.vals <- c(b)
        ab.vals <- expand.grid(a.vals, b.vals)
        MRPE_1_new.full <- c()
        temp.full <- vector("list", nrow(ab.vals));
        est.1.full <- vector("list", nrow(ab.vals));
        for(idx in 1:nrow(ab.vals)){
            a.val <- ab.vals[idx, 1]
            b.val <- ab.vals[idx, 2]
            I <-  I.obs
            R <-  R.obs
            for(t in 2:T){
                rate <- (1+ b.val*exp(-a.val*(t-1)) )
                I[t] <- (I.obs[t] - I.obs[t-1])*rate + I[t-1]
                
            }
            
            y.list <- vector("list", T-1);
            x.list <- vector("list", T-1);
            
            for(i in 2:T){
                y.list[[i-1]] <- matrix(c(R[i] - R[i-1], I[i] - I[i-1]), 2, 1);
                x.temp <- matrix(0, 2, 2);
                x.temp[1,2] <- I[i-1];
                x.temp[2,1] <- I[i-1];
                x.temp[2,2] <- -I[i-1];
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
            
            beta_t.obs <- sapply(1:length(y.list), function(jjj)  (y.list[[jjj]][1] + y.list[[jjj]][2])/I[jjj])
            gamma_t.obs <- sapply(1:length(y.list), function(jjj)  y.list[[jjj]][1]/I[jjj])
            
            plot(beta_t.obs, ylim = c(0, max(beta_t.obs)))
            lines(gamma_t.obs)
            
            Y_std <- sd(Y)
            Y_s <- scale(Y, center = FALSE, scale = apply(Y, 2, sd, na.rm = TRUE))
            
            X_std <- apply(X, MARGIN = 2, FUN = sd)
            X_s <- scale(X, center = FALSE, scale = apply(X, 2, sd, na.rm = TRUE))
            
            p.x <- ncol(X)
            p.y <- ncol(Y)
            n <- nrow(X)
            
            tol <- 10^(-4); # tolerance 
            max.iteration <- 200; # max number of iteration for the LASSO solution
            method <- c("MLR")
            #p is the number of variables for each day (I_t and R_t) 
            p <- 2
            b_n <- p * b_t
            HBIC = TRUE
            gamma.val <- 10
            temp <- tbfl(method, Y_s, X_s, lambda.1.cv = lambda.1, lambda.2.cv = 0, 
                         max.iteration = max.iteration, tol = tol, block.size = b_n, HBIC = HBIC, gamma.val = gamma.val)
            
            temp.full[[idx]] <- temp
            
            cp <- c(1, temp$cp.final, n + 1)
            m <- length(cp) - 1
            X.new.new <- matrix(0, nrow = n, ncol = m*p.x)
            for(i in 1:m){
                X.new.new[cp[i]: (cp[i+1]-1), (p.x*(i-1)+1) : (p.x*i) ] <- X[cp[i]: (cp[i+1]-1), ]
            }
            
            est.1 <- lm(Y~ X.new.new  - 1)
            summary(est.1 )
            est.1.full[[idx]] <- est.1
            
            Y.hat.1 <- est.1$fitted.values
            
            
            R.hat.1<- rep(0, T)
            R.hat.1[1] <- R[1]
            for(i in 2:T){
                R.hat.1[i] <- R[(i - 1)] + Y.hat.1[(i - 2)*2 + 1]
            }
            
            I.hat.1 <- rep(0, T)
            I.hat.1[1] <- I[1]
            for(i in 2:T){
                I.hat.1[i] <- I[(i - 1)] + Y.hat.1[(i - 2)*2 + 2]
            }
            
            MRPE_1_new <- mean(  abs ( (    Y.hat.1[seq(2,n,2)] - Y[seq(2,n,2)] )  /c(Y[seq(2,n,2)])  )[c(Y[seq(2,n,2)]) > 0]  )
            MRPE_1_new.full <- c(MRPE_1_new.full, MRPE_1_new)
        }
        
        idx <- which.min(MRPE_1_new.full)
        cp.final <- temp.full[[idx]]$cp.final 
        cp.date <- c(1:n)[floor( (cp.final-1) / p) + 1]
        pts.final[[j.1]] <- cp.date;
        a.final[[j.1]] <- ab.vals[idx, 1]
        b.final[[j.1]] <- ab.vals[idx, 2]
        lm.res[[j.1]] <- est.1.full[[idx]]
        MRPE_Delta.final[[j.1]] <- MRPE_1_new.full
        
    }
    if(sim %in% c(3)){
        I.obs <- I
        R.obs <- R
        a.vals <- c(0.1, 0.25, 0.5, 0.75, 1)
        b.vals <- c(b)
        ab.vals <- expand.grid(a.vals, b.vals)
        MRPE_1_new.full <- c()
        temp.full <- vector("list", nrow(ab.vals));
        est.1.full <- vector("list", nrow(ab.vals));
        for(idx in 1:nrow(ab.vals)){
            a.val <- ab.vals[idx, 1]
            b.val <- ab.vals[idx, 2]
            I <-  I.obs
            R <-  R.obs
            for(t in 2:T.full){
                rate <- 1/((t + a.val*T.full)/( (1+ a.val)*T.full))^2
                I[t] <- (I.obs[t] - I.obs[t-1])*rate + I[t-1]
                
            }
            
            y.list <- vector("list", T + t.test - 1);
            x.list <- vector("list", T + t.test - 1);
            
            for(i in 2:(T + t.test) ){
                y.list[[i-1]] <- matrix(c(R[i] - R[i-1], I[i] - I[i-1]), 2, 1);
                x.temp <- matrix(0, 2, 2);
                x.temp[1,2] <- I[i-1];
                x.temp[2,1] <- I[i-1];
                x.temp[2,2] <- -I[i-1];
                x.list[[i-1]] <- x.temp;
            }
            
            Y <- y.list[[1]];
            for(i in 2:(T + t.test -1)){
                Y <- rbind(Y, y.list[[i]])
            }
            
            X <- x.list[[1]];
            for(i in 2:(T + t.test -1)){
                X <- rbind(X, x.list[[i]])
            }
            
            Y.full <- Y
            X.full <- X
            
            Y.test <- as.matrix(Y.full[(nrow(Y) - (t.test)*2 + 1):nrow(Y), ])
            X.test <- X.full[(nrow(X) - (t.test)*2 + 1):nrow(X), ]
            Y.train <- as.matrix(Y.full[1 : (nrow(Y) - (t.test)*2 ), ])
            X.train <- X.full[1 : (nrow(X) - (t.test)*2 ), ]
            
            
            beta_t.obs <- sapply(1:length(y.list), function(jjj)  (y.list[[jjj]][1] + y.list[[jjj]][2])/I[jjj])
            gamma_t.obs <- sapply(1:length(y.list), function(jjj)  y.list[[jjj]][1]/I[jjj])
            
            plot(beta_t.obs, ylim = c(0, max(beta_t.obs)))
            lines(gamma_t.obs)
            
            
            Y_std.train <- sd(Y.train)
            Y_s.train <- scale(Y.train, center = FALSE, scale = apply(Y.train, 2, sd, na.rm = TRUE))
            
            X_std.train <- apply(X.train, MARGIN = 2, FUN=sd)
            X_s.train <- scale(X.train, center = FALSE, scale = apply(X.train, 2, sd, na.rm = TRUE))
            
            p.x <- ncol(X_s.train)
            p.y <- ncol(Y_s.train)
            n <- nrow(X_s.train)
            
            
            tol <- 10^(-4); # tolerance 
            max.iteration <- 200; # max number of iteration for the LASSO solution
            method <- c("MLR")
            #p is the number of variables for each day (I_t and R_t) 
            p <- 2
            b_n <- p * b_t
            HBIC = TRUE
            gamma.val <- 10
            temp <- tbfl(method, Y_s.train, X_s.train, lambda.1.cv = lambda.1, lambda.2.cv = 0, 
                         max.iteration = max.iteration, tol = tol, block.size = b_n,  HBIC = HBIC, gamma.val = gamma.val)
            
            temp.full[[idx]] <- temp
            
            cp <- c(1, temp$cp.final, n + 1)
            m <- length(cp) - 1
            X.new.new <- matrix(0, nrow = n, ncol = m*p.x)
            for(i in 1:m){
                X.new.new[cp[i]: (cp[i+1]-1), (p.x*(i-1)+1) : (p.x*i) ] <- X.train[cp[i]: (cp[i+1]-1), ]
            }
            
            est.1 <- lm(Y.train ~ X.new.new  - 1)
            summary(est.1 )
            est.1.full[[idx]] <- est.1
            
            Y.hat.1 <- est.1$fitted.values
            
            MRPE_1_new <- mean(  abs ( (    Y.hat.1[seq(2,n,2)] - Y.train[seq(2,n,2)] ) 
                                       /c(Y.train[seq(2,n,2)])  )[c(Y.train[seq(2,n,2)]) > 0]  )
            MRPE_1_new.full <- c(MRPE_1_new.full, MRPE_1_new)
        }
        
        idx <- which.min(MRPE_1_new.full)
        cp.final <- temp.full[[idx]]$cp.final 
        cp.date <- c(1:n)[floor( (cp.final-1) / p) + 1]
        pts.final[[j.1]] <- cp.date;
        a.final[[j.1]] <- ab.vals[idx, 1]
        b.final[[j.1]] <- ab.vals[idx, 2]
        lm.res[[j.1]] <- est.1.full[[idx]]
        MRPE_Delta.final[[j.1]] <- MRPE_1_new.full
        
        
        a.val <- ab.vals[idx, 1]
        I <-  I.obs
        R <-  R.obs
        for(t in 2:T.full){
            rate <- 1/(((t)+a.val*T.full)/((1 + a.val )*T.full))^2
            print(1/rate)
            I[t] <- (I.obs[t] - I.obs[t-1])*rate + I[t-1]
            
        }
        
        y.list <- vector("list", T + t.test -1);
        x.list <- vector("list", T + t.test -1);
        
        for(i in 2:(T + t.test) ){
            y.list[[i-1]] <- matrix(c(R[i] - R[i-1], I[i] - I[i-1]), 2, 1);
            x.temp <- matrix(0, 2, 2);
            x.temp[1,2] <- I[i-1];
            x.temp[2,1] <- I[i-1];
            x.temp[2,2] <- -I[i-1];
            x.list[[i-1]] <- x.temp;
        }
        
        Y <- y.list[[1]];
        for(i in 2:(T + t.test -1)){
            Y <- rbind(Y, y.list[[i]])
        }
        
        X <- x.list[[1]];
        for(i in 2:(T + t.test -1)){
            X <- rbind(X, x.list[[i]])
        }
        
        z.list <- vector("list", T + t.test -1);
        for(i in 2:(T + t.test) ){
            z.list[[i-1]] <- matrix(c(R.neighbor[i] - R.neighbor[i-1], 
                                      I.neighbor[i] - I.neighbor[i-1]), 2, 1);
        }
        Z <- z.list[[1]];
        for(i in 2:(T + t.test - 1)){
            Z <- rbind(Z, z.list[[i]])
        }
        
        Z <- as.matrix(Z[-c( (nrow(Z) - 1), nrow(Z)), ])
        Z <- as.matrix(c(Z[c(1, 2), ],  Z))
        
        Y.full <- Y
        X.full <- X
        
        Y.test <- as.matrix(Y.full[(nrow(Y) - (t.test)*2 + 1):nrow(Y), ])
        X.test <- X.full[(nrow(X) - (t.test)*2 + 1):nrow(X), ]
        Y.train <- as.matrix(Y.full[1 : (nrow(Y) - (t.test)*2 ), ])
        X.train <- X.full[1 : (nrow(X) - (t.test)*2 ), ]
        
        I.test <- I.obs[(T + 1): (T + t.test)]
        R.test <- R.obs[(T + 1): (T + t.test)]
        
        
        
        Y_std.train <- sd(Y.train)
        Y_s.train <- scale(Y.train, center = FALSE, scale = apply(Y.train, 2, sd, na.rm = TRUE))
        
        X_std.train <- apply(X.train, MARGIN = 2, FUN=sd)
        X_s.train <- scale(X.train, center = FALSE, scale = apply(X.train, 2, sd, na.rm = TRUE))
        
        p.x <- ncol(X_s.train)
        p.y <- ncol(Y_s.train)
        n <- nrow(X_s.train)
        
        tol <- 10^(-4); # tolerance 
        max.iteration <- 200; # max number of iteration for the LASSO solution
        method <- c("MLR")
        p <- 2
        b_n <- p * b_t
        HBIC = TRUE
        gamma.val <- 10
        temp <- tbfl(method, Y_s.train, X_s.train, lambda.1.cv = lambda.1, lambda.2.cv = 0, 
                     max.iteration = max.iteration, tol = tol, block.size = b_n,  HBIC = HBIC, gamma.val = gamma.val)
        
        #change point date
        cp.final <- temp$cp.final 
        c(1:n)[floor( (cp.final-1) / p) + 1]
        cp.date <- c(1:n)[floor( (cp.final-1) / p) + 1]
        
        
        
        cp <- c(1, temp$cp.final, n + 1)
        m <- length(cp) - 1
        X.new.new <- matrix(0, nrow = n, ncol = m*p.x)
        for(i in 1:m){
            X.new.new[cp[i]: (cp[i+1]-1), (p.x*(i-1)+1) : (p.x*i) ] <- X.train[cp[i]: (cp[i+1]-1), ]
        }
        
        est.1 <- lm(Y.train ~ X.new.new  - 1)
        summary(est.1 )
        
        Y.hat.1 <- est.1$fitted.values
        
        Y.temp <- Y.train[cp[m]: (n), ]
        X.temp <- X.train[cp[m]: (n), ]
        est.temp <- lm(Y.temp ~ X.temp  - 1)
        est.temp
        Y.hat.1.new <- X.test%*%c(est.temp$coefficients) 
        
        R.hat.1.new <- rep(0, t.test)
        for(i in 1:t.test){
            R.hat.1.new[i] <- R[T + (i - 1)] + Y.hat.1.new[(i - 1)*2 + 1]
        }
        
        I.hat.1.new <- rep(0, t.test)
        for(i in 1:t.test){
            rate <- (((T+ i)+a.val*T.full)/((1 + a.val )*T.full))^2
            I.hat.1.new[i] <- I.obs[T + (i - 1)] + Y.hat.1.new[(i - 1)*2 + 2]*rate
        }
        
        MRPE_1_new <- mean(  abs ( (     c(R.hat.1.new, I.hat.1.new) - c(R.test, I.test) )  /c(R.test, I.test)  )[c(R.test, I.test) > 0]  )
        MRPE_1_new_I <- mean(  abs ( (  c(I.hat.1.new) - c(I.test) )  /c(I.test)  )[c(I.test) > 0]  )
        MRPE_1_new_R <- mean(  abs ( (     c(R.hat.1.new) - c(R.test)     )  /c(R.test)  )[c(R.test) > 0]  )
        
        # MRPE_1_new_Delta <- mean(  abs ( (  c(Y.hat.1.new) - c(Y.test)  )  /c(Y.test)  )[c(Y.test) > 0]  )
        # MRPE_1_new_I_Delta <- mean(  abs ( (c(Y.hat.1.new[seq(2, 2*t.test, 2)]) - c(Y.test[seq(2, 2*t.test, 2)])  )  /c(Y.test[seq(2, 2*t.test, 2)])  )[c(Y.test[seq(2, 2*t.test, 2)]) > 0]  )
        # MRPE_1_new_R_Delta <- mean(  abs ( (c(Y.hat.1.new[seq(1, 2*t.test, 2)]) - c(Y.test[seq(1, 2*t.test, 2)])  )  /c(Y.test[seq(1, 2*t.test, 2)])  )[c(Y.test[seq(1, 2*t.test, 2)]) > 0]  )

        
        # MRPE_1_Delta.final[j.1] <- MRPE_1_new_Delta; MRPE_1_I_Delta.final[j.1] <- MRPE_1_new_I_Delta; MRPE_1_R_Delta.final[j.1] <- MRPE_1_new_R_Delta; 
        MRPE_1.final.true[j.1] <- MRPE_1_new; 
        MRPE_1_I.final.true[j.1] <- MRPE_1_new_I; 
        MRPE_1_R.final.true[j.1] <- MRPE_1_new_R; 
        
        
        ###################################
        ########  model 2
        ###################################
        cp <- c(1, temp$cp.final, n + 1)
        m <- length(cp) - 1
        X.new.2 <- matrix(0, nrow = n, ncol = m*p.x + 1)
        for(i in 1:m){
            X.new.2[cp[i]: (cp[i+1]-1), (p.x*(i-1)+1) : (p.x*i) ] <- X.train[cp[i]: (cp[i+1]-1), ]
        }
        X.new.2[, m*p.x + 1] <- Z[1:n, ]
        
        est.2 <- lm(Y.train[, ] ~ X.new.2[, ] - 1)
        summary(est.2)
        lm.res[[j.1]] <- est.2
        Y.hat.2 <- est.2 $fitted.values
        
        beta.t.est <- est.2$coefficients[m*p.x - 1]
        gamma.t.est <- est.2$coefficients[m*p.x ]
        alpha.t.est <- est.2$coefficients[m*p.x + 1]
        Y.hat.2.new <- X.test%*%c(beta.t.est, gamma.t.est) + as.matrix(Z[(n+1): nrow(Z), ])%*%c(alpha.t.est)
        
        
        R.hat.2.new <- rep(0, t.test)
        for(i in 1:t.test){
            R.hat.2.new[i] <- R[T + (i - 1)] + Y.hat.2.new[(i - 1)*2 + 1]
        }
        
        I.hat.2.new <- rep(0, t.test)
        for(i in 1:t.test){
            rate <- (((T+ i)+a.val*T.full)/((1 + a.val )*T.full))^2
            I.hat.2.new[i] <- I.obs[T + (i - 1)] + Y.hat.2.new[(i - 1)*2 + 2]*rate
        }
        
        MRPE_2_new <- mean(  abs ( (     c(R.hat.2.new, I.hat.2.new) - c(R.test, I.test) )  /c(R.test, I.test)  )[c(R.test, I.test) > 0]  )
        MRPE_2_new_I <- mean(  abs ( (  c(I.hat.2.new) - c(I.test) )  /c(I.test)  )[c(I.test) > 0]  )
        MRPE_2_new_R <- mean(  abs ( (     c(R.hat.2.new) - c(R.test)     )  /c(R.test)  )[c(R.test) > 0]  )
        
        # MRPE_2_new_Delta <- mean(  abs ( (  c(Y.hat.2.new) - c(Y.test)  )  /c(Y.test)  )[c(Y.test) > 0]  )
        # MRPE_2_new_I_Delta <- mean(  abs ( (c(Y.hat.2.new[seq(2, 2*t.test, 2)]) - c(Y.test[seq(2, 2*t.test, 2)])  )  /c(Y.test[seq(2, 2*t.test, 2)])  )[c(Y.test[seq(2, 2*t.test, 2)]) > 0]  )
        # MRPE_2_new_R_Delta <- mean(  abs ( (c(Y.hat.2.new[seq(1, 2*t.test, 2)]) - c(Y.test[seq(1, 2*t.test, 2)])  )  /c(Y.test[seq(1, 2*t.test, 2)])  )[c(Y.test[seq(1, 2*t.test, 2)]) > 0]  )
        
        
        # MRPE_2_Delta.final[j.1] <- MRPE_2_new_Delta; MRPE_2_I_Delta.final[j.1] <- MRPE_2_new_I_Delta; MRPE_2_R_Delta.final[j.1] <- MRPE_2_new_R_Delta; 
        MRPE_2.final.true[j.1] <- MRPE_2_new; 
        MRPE_2_I.final.true[j.1] <- MRPE_2_new_I; 
        MRPE_2_R.final.true[j.1] <- MRPE_2_new_R; 
        
        ###################################
        ########  model 3
        ###################################
        Delta.R.hat <- rep(0, T - 1)
        Delta.R <- rep(0, T - 1)
        for(i in 2:T){
            Delta.R.hat[i - 1] <- Y.hat.2[(i - 2)*2 + 1]
            Delta.R[i - 1] <- Y[(i - 2)*2 + 1]
        }
        Delta.R.hat <- Delta.R.hat
        Delta.R <- Delta.R
        
        Delta.I.hat <- rep(0, T - 1)
        Delta.I <- rep(0, T - 1)
        for(i in 2:T){
            Delta.I.hat[i - 1] <- Y.hat.2[(i - 2)*2 + 2]
            Delta.I[i - 1] <- Y[(i - 2)*2 + 2]
        }
        Delta.I.hat <- Delta.I.hat
        Delta.I <- Delta.I
        
        residual_Delta.I <- Delta.I - Delta.I.hat 
        residual_Delta.R <- Delta.R - Delta.R.hat 
        
        res <- c()
        for(i in 1:n){
            res <- c(res,  Y[i] - Y.hat.2[i])
        }
        
        residual.matrix <- cbind(residual_Delta.R, residual_Delta.I)
        # print("estimated sample variance for hat residual:")
        # print(var(residual.matrix))
        
        var <- VARselect(residual.matrix, lag.max = 7, type = "none")
        
        var$selection
        # choose the p by BIC
        p.est <- var$selection["SC(n)"]
        var1 <- VAR(residual.matrix, p = p.est, type = "none")
        norm.res1 <-normality.test(var1)
        norm.res1
        Sigma.est <- summary(var1)$covres
        coef.matrix <- Bcoef(var1)
        coef.matrix
        var.coef.res[[j.1]] <- coef.matrix
        #fitted the residual
        residual.hat <- fitted(var1)
        #vectorize the fitted residuals
        residual.hat.vec <- rep(0, nrow(residual.hat)*2)
        #fitted residual of Delta R
        residual.hat.vec[seq(1, nrow(residual.hat)*2, 2)] <- residual.hat[, 1]
        #fitted residual of Delta I
        residual.hat.vec[seq(2, nrow(residual.hat)*2, 2)] <- residual.hat[, 2]
        
        Y.hat.3 <- Y.hat.2 + c(rep(0, p.est*2),  residual.hat.vec)
        
        ##predict the residual 
        predict.temp <- predict(var1, n.ahead = t.test)$fcst
        #vectorize the fitted residuals
        residual.test.hat.vec <- rep(0, t.test*2)
        #fitted residual of Delta R
        residual.test.hat.vec[seq(1, t.test*2, 2)] <- predict.temp$residual_Delta.R[, 1]
        #fitted residual of Delta I
        residual.test.hat.vec[seq(2, t.test*2, 2)] <- predict.temp$residual_Delta.I[, 1]
        
        Y.hat.3.new <- Y.hat.2.new +  residual.test.hat.vec
        
        R.hat.3.new <- rep(0, t.test)
        for(i in 1:t.test){
            R.hat.3.new[i] <- R[T + (i - 1)] + Y.hat.3.new[(i - 1)*2 + 1]
        }
        
        I.hat.3.new <- rep(0, t.test)
        for(i in 1:t.test){
            rate <- (((T+ i)+a.val*T.full)/((1 + a.val )*T.full))^2
            I.hat.3.new[i] <- I.obs[T + (i - 1)] + Y.hat.3.new[(i - 1)*2 + 2]*rate
        }
        
        MRPE_3_new <- mean(  abs ( (     c(R.hat.3.new, I.hat.3.new) - c(R.test, I.test) )  /c(R.test, I.test)  )[c(R.test, I.test) > 0]  )
        MRPE_3_new_I <- mean(  abs ( (  c(I.hat.3.new) - c(I.test) )  /c(I.test)  )[c(I.test) > 0]  )
        MRPE_3_new_R <- mean(  abs ( (     c(R.hat.3.new) - c(R.test)     )  /c(R.test)  )[c(R.test) > 0]  )
        
        # MRPE_3_new_Delta <- mean(  abs ( (  c(Y.hat.3.new) - c(Y.test)  )  /c(Y.test)  )[c(Y.test) > 0]  )
        # MRPE_3_new_I_Delta <- mean(  abs ( (c(Y.hat.3.new[seq(2, 2*t.test, 2)]) - c(Y.test[seq(2, 2*t.test, 2)])  )  /c(Y.test[seq(2, 2*t.test, 2)])  )[c(Y.test[seq(2, 2*t.test, 2)]) > 0]  )
        # MRPE_3_new_R_Delta <- mean(  abs ( (c(Y.hat.3.new[seq(1, 2*t.test, 2)]) - c(Y.test[seq(1, 2*t.test, 2)])  )  /c(Y.test[seq(1, 2*t.test, 2)])  )[c(Y.test[seq(1, 2*t.test, 2)]) > 0]  )
        
        # MRPE_3_Delta.final[j.1] <- MRPE_3_new_Delta; MRPE_3_I_Delta.final[j.1] <- MRPE_3_new_I_Delta; MRPE_3_R_Delta.final[j.1] <- MRPE_3_new_R_Delta; 
        MRPE_3.final.true[j.1] <- MRPE_3_new; 
        MRPE_3_I.final.true[j.1] <- MRPE_3_new_I; 
        MRPE_3_R.final.true[j.1] <- MRPE_3_new_R; 
        
        
    }
    
    
    
    if(sim != 1 & !(sim %in% c(2, 3, 6))){
        y.list <- vector("list", T-1);
        x.list <- vector("list", T-1);
        
        for(i in 2:T){
            y.list[[i-1]] <- matrix(c(R[i] - R[i-1], I[i] - I[i-1]), 2, 1);
            x.temp <- matrix(0, 2, 2);
            x.temp[1,2] <- I[i-1];
            x.temp[2,1] <- I[i-1];
            x.temp[2,2] <- -I[i-1];
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
        
        if(sim %in% c(5)){
            z.list <- vector("list", T-1);
            for(i in 2:T){
                z.list[[i-1]] <- matrix(c(R.neighbor[i] - R.neighbor[i-1], 
                                          I.neighbor[i] - I.neighbor[i-1]), 2, 1);
            }
            Z <- z.list[[1]];
            for(i in 2:(T-1)){
                Z <- rbind(Z, z.list[[i]])
            }
            
            Z <- as.matrix(Z[-c( (nrow(Z) - 1), nrow(Z)), ])
            Z <- as.matrix(c(Z[c(1, 2), ],  Z))
            
            cor(X, Z)
        }
        beta_t.obs <- sapply(1:length(y.list), function(jjj)  (y.list[[jjj]][1] + y.list[[jjj]][2])/I[jjj])
        gamma_t.obs <- sapply(1:length(y.list), function(jjj)  y.list[[jjj]][1]/I[jjj])
        
        plot(beta_t.obs, ylim = c(0, max(beta_t.obs)))
        lines(gamma_t.obs)
        
        Y_std <- sd(Y)
        Y_s <- scale(Y, center = FALSE, scale = apply(Y, 2, sd, na.rm = TRUE))
        plot(seq(1, length(Y_s), 1), Y_s, type='l')
        
        X_std <- apply(X, MARGIN = 2, FUN = sd)
        X_s <- scale(X, center = FALSE, scale = apply(X, 2, sd, na.rm = TRUE))
        
        ###################################
        p.x <- ncol(X)
        p.y <-ncol(Y)
        n <- nrow(X)
        
        tol <- 10^(-4); # tolerance 
        max.iteration <- 200; # max number of iteration for the LASSO solution
        method <- c("MLR")
        #p is the number of variables for each day (I_t and R_t) 
        p <- 2
        b_n <- p * b_t
        if(sim %in% c(4) ){
            HBIC = FALSE
            temp <- tbfl(method, Y_s, X_s, lambda.1.cv = lambda.1, lambda.2.cv = 0, 
                         max.iteration = max.iteration, tol = tol, block.size = b_n, HBIC = HBIC)
        }else{
            HBIC = TRUE
            gamma.val <- 10
            temp <- tbfl(method, Y_s, X_s, lambda.1.cv = lambda.1, lambda.2.cv = 0, 
                         max.iteration = max.iteration, tol = tol, block.size = b_n, HBIC = HBIC, gamma.val = gamma.val)
        }
        
        #change point date
        cp.final <- temp$cp.final 
        c(1:n)[floor( (cp.final-1) / p) + 1]
        cp.date <- c(1:n)[floor( (cp.final-1) / p) + 1]
        pts.final[[j.1]] <- cp.date;
        
        
    }
    
    
    if(sim %in% c(1, 3)){
        I <- I.obs
        
        y.list <- vector("list", T + t.test -1);
        x.list <- vector("list", T + t.test -1);
        
        for(i in 2:(T + t.test) ){
            y.list[[i-1]] <- matrix(c(R[i] - R[i-1], I[i] - I[i-1]), 2, 1);
            x.temp <- matrix(0, 2, 2);
            x.temp[1,2] <- I[i-1];
            x.temp[2,1] <- I[i-1];
            x.temp[2,2] <- -I[i-1];
            x.list[[i-1]] <- x.temp;
        }
        
        Y <- y.list[[1]];
        for(i in 2:(T + t.test -1)){
            Y <- rbind(Y, y.list[[i]])
        }
        
        X <- x.list[[1]];
        for(i in 2:(T + t.test -1)){
            X <- rbind(X, x.list[[i]])
        }
        
        z.list <- vector("list", T + t.test -1);
        for(i in 2:(T + t.test) ){
            z.list[[i-1]] <- matrix(c(R.neighbor[i] - R.neighbor[i-1], 
                                      I.neighbor[i] - I.neighbor[i-1]), 2, 1);
        }
        Z <- z.list[[1]];
        for(i in 2:(T + t.test - 1)){
            Z <- rbind(Z, z.list[[i]])
        }
        
        Z <- as.matrix(Z[-c( (nrow(Z) - 1), nrow(Z)), ])
        Z <- as.matrix(c(Z[c(1, 2), ],  Z))
        
        cor(X, Z)
        
        
        Y.full <- Y
        X.full <- X
        
        Y.test <- as.matrix(Y.full[(nrow(Y) - (t.test)*2 + 1):nrow(Y), ])
        X.test <- X.full[(nrow(X) - (t.test)*2 + 1):nrow(X), ]
        Y.train <- as.matrix(Y.full[1 : (nrow(Y) - (t.test)*2 ), ])
        X.train <- X.full[1 : (nrow(X) - (t.test)*2 ), ]
        
        I.test <- I[(T + 1): (T + t.test)]
        R.test <- R[(T + 1): (T + t.test)]
        
        
        
        Y_std.train <- sd(Y.train)
        Y_s.train <- scale(Y.train, center = FALSE, scale = apply(Y.train, 2, sd, na.rm = TRUE))
        
        X_std.train <- apply(X.train, MARGIN = 2, FUN=sd)
        X_s.train <- scale(X.train, center = FALSE, scale = apply(X.train, 2, sd, na.rm = TRUE))
        
        p.x <- ncol(X_s.train)
        p.y <- ncol(Y_s.train)
        n <- nrow(X_s.train)
        
        tol <- 10^(-4); # tolerance 
        max.iteration <- 200; # max number of iteration for the LASSO solution
        method <- c("MLR")
        p <- 2
        b_n <- p * b_t
        HBIC = TRUE
        gamma.val <- 10
        temp <- tbfl(method, Y_s.train, X_s.train, lambda.1.cv = lambda.1, lambda.2.cv = 0, 
                     max.iteration = max.iteration, tol = tol, block.size = b_n,  HBIC = HBIC, gamma.val = gamma.val)
        
        #change point date
        cp.final <- temp$cp.final 
        c(1:n)[floor( (cp.final-1) / p) + 1]
        cp.date <- c(1:n)[floor( (cp.final-1) / p) + 1]
        if(sim == 1){
            pts.final[[j.1]] <- cp.date;
        }
        
        
    }
    if(sim == 4){
        cp <- c(1, temp$cp.final, n + 1)
        m <- length(cp) - 1
        X.new.new <- matrix(0, nrow = n, ncol = m*p.x)
        for(i in 1:m){
            X.new.new[cp[i]: (cp[i+1]-1), (p.x*(i-1)+1) : (p.x*i) ] <- X[cp[i]: (cp[i+1]-1), ]
        }
        
        est.1 <- lm(Y ~ X.new.new  - 1)
        
        lm.res[[j.1]] <- est.1
    }
    if(sim == 5){
        cp <- c(1, temp$cp.final, n + 1)
        m <- length(cp) - 1
        X.new.2 <- matrix(0, nrow = n, ncol = m*p.x + 1)
        for(i in 1:m){
            X.new.2[cp[i]: (cp[i+1]-1), (p.x*(i-1)+1) : (p.x*i) ] <- X[cp[i]: (cp[i+1]-1), ]
        }
        X.new.2[, m*p.x + 1] <- Z
        
        est.2 <- lm(Y ~ X.new.2  - 1)
        Y.hat.2 <- est.2$fitted.values
        summary(est.2)
        lm.res[[j.1]] <- est.2
    }
    
    if(sim  %in% c(1, 3)){
        
        cp <- c(1, temp$cp.final, n + 1)
        m <- length(cp) - 1
        X.new.new <- matrix(0, nrow = n, ncol = m*p.x)
        for(i in 1:m){
            X.new.new[cp[i]: (cp[i+1]-1), (p.x*(i-1)+1) : (p.x*i) ] <- X.train[cp[i]: (cp[i+1]-1), ]
        }
        
        est.1 <- lm(Y.train ~ X.new.new  - 1)
        summary(est.1 )
        
        Y.hat.1 <- est.1$fitted.values
        
        Y.temp <- Y.train[cp[m]: (n), ]
        X.temp <- X.train[cp[m]: (n), ]
        est.temp <- lm(Y.temp ~ X.temp  - 1)
        est.temp
        Y.hat.1.new <- X.test%*%c(est.temp$coefficients) 
        
        R.hat.1.new <- rep(0, t.test)
        for(i in 1:t.test){
            R.hat.1.new[i] <- R[T + (i - 1)] + Y.hat.1.new[(i - 1)*2 + 1]
        }
        
        I.hat.1.new <- rep(0, t.test)
        for(i in 1:t.test){
            I.hat.1.new[i] <- I[T + (i - 1)] + Y.hat.1.new[(i - 1)*2 + 2]
        }
        
        MRPE_1_new <- mean(  abs ( (     c(R.hat.1.new, I.hat.1.new) - c(R.test, I.test) )  /c(R.test, I.test)  )[c(R.test, I.test) > 0]  )
        MRPE_1_new_I <- mean(  abs ( (  c(I.hat.1.new) - c(I.test) )  /c(I.test)  )[c(I.test) > 0]  )
        MRPE_1_new_R <- mean(  abs ( (     c(R.hat.1.new) - c(R.test)     )  /c(R.test)  )[c(R.test) > 0]  )
        
        MRPE_1_new_Delta <- mean(  abs ( (  c(Y.hat.1.new) - c(Y.test)  )  /c(Y.test)  )[c(Y.test) > 0]  )
        MRPE_1_new_I_Delta <- mean(  abs ( (c(Y.hat.1.new[seq(2, 2*t.test, 2)]) - c(Y.test[seq(2, 2*t.test, 2)])  )  /c(Y.test[seq(2, 2*t.test, 2)])  )[c(Y.test[seq(2, 2*t.test, 2)]) > 0]  )
        MRPE_1_new_R_Delta <- mean(  abs ( (c(Y.hat.1.new[seq(1, 2*t.test, 2)]) - c(Y.test[seq(1, 2*t.test, 2)])  )  /c(Y.test[seq(1, 2*t.test, 2)])  )[c(Y.test[seq(1, 2*t.test, 2)]) > 0]  )
        
    
        MRPE_1_Delta.final[j.1] <- MRPE_1_new_Delta; MRPE_1_I_Delta.final[j.1] <- MRPE_1_new_I_Delta; MRPE_1_R_Delta.final[j.1] <- MRPE_1_new_R_Delta; 
        MRPE_1.final[j.1] <- MRPE_1_new; MRPE_1_I.final[j.1] <- MRPE_1_new_I; MRPE_1_R.final[j.1] <- MRPE_1_new_R; 
        
        
        ###################################
        ########  model 2
        ###################################
        cp <- c(1, temp$cp.final, n + 1)
        m <- length(cp) - 1
        X.new.2 <- matrix(0, nrow = n, ncol = m*p.x + 1)
        for(i in 1:m){
            X.new.2[cp[i]: (cp[i+1]-1), (p.x*(i-1)+1) : (p.x*i) ] <- X.train[cp[i]: (cp[i+1]-1), ]
        }
        X.new.2[, m*p.x + 1] <- Z[1:n, ]
        
        est.2 <- lm(Y.train[, ] ~ X.new.2[, ] - 1)
        summary(est.2)
        if(sim == 3){
            lm.res[[j.1]] <- est.2
        }
        
        Y.hat.2 <- est.2 $fitted.values
        
        beta.t.est <- est.2$coefficients[m*p.x - 1]
        gamma.t.est <- est.2$coefficients[m*p.x ]
        alpha.t.est <- est.2$coefficients[m*p.x + 1]
        Y.hat.2.new <- X.test%*%c(beta.t.est, gamma.t.est) + as.matrix(Z[(n+1): nrow(Z), ])%*%c(alpha.t.est)
        
        
        R.hat.2.new <- rep(0, t.test)
        for(i in 1:t.test){
            R.hat.2.new[i] <- R[T + (i - 1)] + Y.hat.2.new[(i - 1)*2 + 1]
        }
        
        I.hat.2.new <- rep(0, t.test)
        for(i in 1:t.test){
            I.hat.2.new[i] <- I[T + (i - 1)] + Y.hat.2.new[(i - 1)*2 + 2]
        }
        
        MRPE_2_new <- mean(  abs ( (     c(R.hat.2.new, I.hat.2.new) - c(R.test, I.test) )  /c(R.test, I.test)  )[c(R.test, I.test) > 0]  )
        MRPE_2_new_I <- mean(  abs ( (  c(I.hat.2.new) - c(I.test) )  /c(I.test)  )[c(I.test) > 0]  )
        MRPE_2_new_R <- mean(  abs ( (     c(R.hat.2.new) - c(R.test)     )  /c(R.test)  )[c(R.test) > 0]  )
        
        MRPE_2_new_Delta <- mean(  abs ( (  c(Y.hat.2.new) - c(Y.test)  )  /c(Y.test)  )[c(Y.test) > 0]  )
        MRPE_2_new_I_Delta <- mean(  abs ( (c(Y.hat.2.new[seq(2, 2*t.test, 2)]) - c(Y.test[seq(2, 2*t.test, 2)])  )  /c(Y.test[seq(2, 2*t.test, 2)])  )[c(Y.test[seq(2, 2*t.test, 2)]) > 0]  )
        MRPE_2_new_R_Delta <- mean(  abs ( (c(Y.hat.2.new[seq(1, 2*t.test, 2)]) - c(Y.test[seq(1, 2*t.test, 2)])  )  /c(Y.test[seq(1, 2*t.test, 2)])  )[c(Y.test[seq(1, 2*t.test, 2)]) > 0]  )
        
        
        MRPE_2_Delta.final[j.1] <- MRPE_2_new_Delta; MRPE_2_I_Delta.final[j.1] <- MRPE_2_new_I_Delta; MRPE_2_R_Delta.final[j.1] <- MRPE_2_new_R_Delta; 
        MRPE_2.final[j.1] <- MRPE_2_new; MRPE_2_I.final[j.1] <- MRPE_2_new_I; MRPE_2_R.final[j.1] <- MRPE_2_new_R; 
        
        ###################################
        ########  model 3
        ###################################
        Delta.R.hat <- rep(0, T - 1)
        Delta.R <- rep(0, T - 1)
        for(i in 2:T){
            Delta.R.hat[i - 1] <- Y.hat.2[(i - 2)*2 + 1]
            Delta.R[i - 1] <- Y[(i - 2)*2 + 1]
        }
        Delta.R.hat <- Delta.R.hat
        Delta.R <- Delta.R
        
        Delta.I.hat <- rep(0, T - 1)
        Delta.I <- rep(0, T - 1)
        for(i in 2:T){
            Delta.I.hat[i - 1] <- Y.hat.2[(i - 2)*2 + 2]
            Delta.I[i - 1] <- Y[(i - 2)*2 + 2]
        }
        Delta.I.hat <- Delta.I.hat
        Delta.I <- Delta.I
        
        residual_Delta.I <- Delta.I - Delta.I.hat 
        residual_Delta.R <- Delta.R - Delta.R.hat 
        
        res <- c()
        for(i in 1:n){
            res <- c(res,  Y[i] - Y.hat.2[i])
        }
        
        residual.matrix <- cbind(residual_Delta.R, residual_Delta.I)
        # print("estimated sample variance for hat residual:")
        # print(var(residual.matrix))
        
        var <- VARselect(residual.matrix, lag.max = 7, type = "none")
        
        var$selection
        # choose the p by BIC
        p.est <- var$selection["SC(n)"]
        var1 <- VAR(residual.matrix, p = p.est, type = "none")
        norm.res1 <-normality.test(var1)
        norm.res1
        Sigma.est <- summary(var1)$covres
        coef.matrix <- Bcoef(var1)
        coef.matrix
        var.coef.res[[j.1]] <- coef.matrix
        #fitted the residual
        residual.hat <- fitted(var1)
        #vectorize the fitted residuals
        residual.hat.vec <- rep(0, nrow(residual.hat)*2)
        #fitted residual of Delta R
        residual.hat.vec[seq(1, nrow(residual.hat)*2, 2)] <- residual.hat[, 1]
        #fitted residual of Delta I
        residual.hat.vec[seq(2, nrow(residual.hat)*2, 2)] <- residual.hat[, 2]

        Y.hat.3 <- Y.hat.2 + c(rep(0, p.est*2),  residual.hat.vec)
        
        ##predict the residual 
        predict.temp <- predict(var1, n.ahead = t.test)$fcst
        #vectorize the fitted residuals
        residual.test.hat.vec <- rep(0, t.test*2)
        #fitted residual of Delta R
        residual.test.hat.vec[seq(1, t.test*2, 2)] <- predict.temp$residual_Delta.R[, 1]
        #fitted residual of Delta I
        residual.test.hat.vec[seq(2, t.test*2, 2)] <- predict.temp$residual_Delta.I[, 1]
        
        Y.hat.3.new <- Y.hat.2.new +  residual.test.hat.vec
        
        R.hat.3.new <- rep(0, t.test)
        for(i in 1:t.test){
            R.hat.3.new[i] <- R[T + (i - 1)] + Y.hat.3.new[(i - 1)*2 + 1]
        }
        
        I.hat.3.new <- rep(0, t.test)
        for(i in 1:t.test){
            I.hat.3.new[i] <- I[T + (i - 1)] + Y.hat.3.new[(i - 1)*2 + 2]
        }
        
        MRPE_3_new <- mean(  abs ( (     c(R.hat.3.new, I.hat.3.new) - c(R.test, I.test) )  /c(R.test, I.test)  )[c(R.test, I.test) > 0]  )
        MRPE_3_new_I <- mean(  abs ( (  c(I.hat.3.new) - c(I.test) )  /c(I.test)  )[c(I.test) > 0]  )
        MRPE_3_new_R <- mean(  abs ( (     c(R.hat.3.new) - c(R.test)     )  /c(R.test)  )[c(R.test) > 0]  )
        
        MRPE_3_new_Delta <- mean(  abs ( (  c(Y.hat.3.new) - c(Y.test)  )  /c(Y.test)  )[c(Y.test) > 0]  )
        MRPE_3_new_I_Delta <- mean(  abs ( (c(Y.hat.3.new[seq(2, 2*t.test, 2)]) - c(Y.test[seq(2, 2*t.test, 2)])  )  /c(Y.test[seq(2, 2*t.test, 2)])  )[c(Y.test[seq(2, 2*t.test, 2)]) > 0]  )
        MRPE_3_new_R_Delta <- mean(  abs ( (c(Y.hat.3.new[seq(1, 2*t.test, 2)]) - c(Y.test[seq(1, 2*t.test, 2)])  )  /c(Y.test[seq(1, 2*t.test, 2)])  )[c(Y.test[seq(1, 2*t.test, 2)]) > 0]  )
        
        MRPE_3_Delta.final[j.1] <- MRPE_3_new_Delta; MRPE_3_I_Delta.final[j.1] <- MRPE_3_new_I_Delta; MRPE_3_R_Delta.final[j.1] <- MRPE_3_new_R_Delta; 
        MRPE_3.final[j.1] <- MRPE_3_new; MRPE_3_I.final[j.1] <- MRPE_3_new_I; MRPE_3_R.final[j.1] <- MRPE_3_new_R; 
    
    }
}

if(sim %in% c(2, 6)){
    filename <- paste0("Sim_", sim, "_bn_", b_t, "_a_", a, "_b_", b, "_sd_", sd.log^2,  ".RData")
    
}else if(sim %in% c(3)){
    filename <- paste0("Sim_", sim, "_bn_", b_t, "_a_", a,".RData")
    
}else{
    filename <- paste0("Sim_", sim, "_bn_", b_t, ".RData")
    
}

save.image(filename)






#####################################
######## Summary ###################
#####################################
rm(list=ls(all=TRUE))
gc()

sim = 3
a = 0.5
library("xtable")
if(sim == 5 | sim == 1 | sim == 3){
    tt <- sim
    alpha.res <- c()
    for(ii in c( 4, 8, 12)){
        filename <- paste0("Sim_", tt, "_bn_", ii, ".RData")
        load(filename)
        alpha_est <- c()
        beta_est_1 <- c(); beta_est_2 <- c()
        gamma_est_1 <- c(); gamma_est_2 <- c()
        for(j.1 in 1:N){
            temp.coef <- lm.res[[j.1]]$coefficients
            beta_est_1 <- c(beta_est_1, temp.coef[1])
            beta_est_2 <- c(beta_est_2, temp.coef[3])
            gamma_est_1 <- c(gamma_est_1, temp.coef[2])
            gamma_est_2 <- c(gamma_est_2, temp.coef[4])
            alpha_est <- c(alpha_est, temp.coef[length(temp.coef)])
        }
        alpha.res <- rbind(alpha.res,
                           c(round(mean(beta_est_1), 4), round(sd(beta_est_1), 4) ),
                           c(round(mean(beta_est_2), 4), round(sd(beta_est_2), 4) ),
                           c(round(mean(gamma_est_1), 4), round(sd(gamma_est_1), 4) ) ,
                           c(round(mean(gamma_est_2), 4), round(sd(gamma_est_2), 4) ) ,
                           c(round(mean(alpha_est), 4), round(sd(alpha_est), 4) ) )
    }
    alpha.res <- cbind(c(rep(c("$\\beta_1$", "$\\beta_2$", "$\\gamma_1$", "$\\gamma_2$", "$\\alpha$"), 3)), alpha.res)
    alpha.res <- cbind(c("\\multirow{ 5}{*}{model 2 ($b_n = 4$)}", rep("", 4), 
                         "\\multirow{ 5}{*}{model 2 ($b_n = 8$)}", rep("", 4),
                         "\\multirow{ 5}{*}{model 2 ($b_n = 12$)}", rep("", 4)), 
                       alpha.res)
    alpha.res <- cbind(rep("", 15), 
                       alpha.res)
    table.alpha_res <- xtable(alpha.res, hline.after = c(1, 2), digits = 4)
    print("estiamted alpha")
    print(table.alpha_res, include.rownames = FALSE, include.colnames = FALSE, sanitize.text.function = identity)
    
}

if(sim == 1){
    tt <-sim
    filename <- paste0("Sim_", tt, "_bn_4.RData")
    load(filename)
    MRPE.res <- c()
    MRPE.Delta.res <- c()
    for(ii in c( 4, 8, 12)){
        filename <- paste0("Sim_", tt, "_bn_", ii,".RData")
        load(filename)
        MRPE.res <- rbind(MRPE.res,
                          round(c(mean(MRPE_1.final), mean(MRPE_1_I.final), mean(MRPE_1_R.final) ), 6))
        MRPE.Delta.res <- rbind(MRPE.Delta.res,
                          round(c(mean(MRPE_1_Delta.final), mean(MRPE_1_I_Delta.final), mean(MRPE_1_R_Delta.final) ), 6))
    }
    for(ii in c( 4, 8, 12)){
        filename <- paste0("Sim_", tt, "_bn_", ii, ".RData")
        load(filename)
        MRPE.res <- rbind(MRPE.res,
                          round(c(mean(MRPE_2.final), mean(MRPE_2_I.final), mean(MRPE_2_R.final) ), 6))
        MRPE.Delta.res <- rbind(MRPE.Delta.res,
                                round(c(mean(MRPE_2_Delta.final), mean(MRPE_2_I_Delta.final), mean(MRPE_2_R_Delta.final) ), 6))
        
    }
    for(ii in c( 4, 8, 12)){
        filename <- paste0("Sim_", tt, "_bn_", ii,".RData")
        load(filename)
        MRPE.res <- rbind(MRPE.res,
                          round(c(mean(MRPE_3.final), mean(MRPE_3_I.final), mean(MRPE_3_R.final) ), 6))
        MRPE.Delta.res <- rbind(MRPE.Delta.res,
                                round(c(mean(MRPE_2_Delta.final), mean(MRPE_2_I_Delta.final), mean(MRPE_2_R_Delta.final) ), 6))
        
    }
    
    MRPE.res <- cbind(c( "model 1 ($b_n = 4$)", "model 1 ($b_n = 8$)", "model 1 ($b_n = 12$)",
                         "model 2 ($b_n = 4$)", "model 2 ($b_n = 8$)", "model 2 ($b_n = 12$)" ,
                         "model 3 ($b_n = 4$)", "model 3 ($b_n = 8$)", "model 3 ($b_n = 12$)" ), MRPE.res)
    MRPE.res <- cbind(rep("", 9), MRPE.res)
    MRPE.Delta.res <- cbind(c( "model 1 ($b_n = 4$)", "model 1 ($b_n = 8$)", "model 1 ($b_n = 12$)",
                         "model 2 ($b_n = 4$)", "model 2 ($b_n = 8$)", "model 2 ($b_n = 12$)" ,
                         "model 3 ($b_n = 4$)", "model 3 ($b_n = 8$)", "model 3 ($b_n = 12$)" ), MRPE.Delta.res)
    MRPE.Delta.res <- cbind(rep("", 9), MRPE.Delta.res)
    table.MRPE_res <- xtable(MRPE.res, hline.after = c(1,2), digits = 6)
    print("MRPE")
    print(table.MRPE_res, include.rownames = FALSE, include.colnames = FALSE, sanitize.text.function = identity)
    
    
    table.MRPE.Delta_res <- xtable(MRPE.Delta.res, hline.after = c(1,2), digits = 6)
    print("MRPE.Delta")
    print(table.MRPE.Delta_res, include.rownames = FALSE, include.colnames = FALSE, sanitize.text.function = identity)
    
}

if(sim == 3){
    tt <-sim
    filename <- paste0("Sim_", tt, "_bn_4", "_a_", a,  ".RData")
    load(filename)
    MRPE.res <- c()
    for(ii in c( 4, 8, 12)){
        filename <- paste0("Sim_", tt, "_bn_", ii, "_a_", a, ".RData")
        load(filename)
        MRPE.res <- rbind(MRPE.res,
                          round(c(mean(MRPE_3.final.true, na.rm =TRUE), mean(MRPE_3_I.final.true, na.rm =TRUE), mean(MRPE_3_R.final.true, na.rm =TRUE) ), 6))
    }
    for(ii in c( 4, 8, 12)){
        filename <- paste0("Sim_", tt, "_bn_", ii, "_a_", a, ".RData")
        load(filename)
        MRPE.res <- rbind(MRPE.res,
                          round(c(mean(MRPE_3.final, na.rm =TRUE), mean(MRPE_3_I.final, na.rm =TRUE), mean(MRPE_3_R.final, na.rm =TRUE) ), 6))
    }
    
    MRPE.res <- cbind(c( "model 3 ($b_n = 4$)", "model 3 ($b_n = 8$)", "model 3 ($b_n = 12$)",
                         "model 3 ($b_n = 4$)", "model 3 ($b_n = 8$)", "model 3 ($b_n = 12$)"), MRPE.res)
    MRPE.res <- cbind(c("\\multirow{ 3}{*}{Transformed by $u(t)$}", rep("", 2), 
            "\\multirow{ 3}{*}{Not transformed}", rep("", 2)), MRPE.res )
    table.MRPE_res <- xtable(MRPE.res, hline.after = c(1,2), digits = 6)
    print("MRPE")
    print(table.MRPE_res, include.rownames = FALSE, include.colnames = FALSE, sanitize.text.function = identity)
    
    
}

if(sim == 1){
    var.coef.vec <- unlist(var.coef.res)
    var.coef.mean <- matrix(0, ncol  = 2, nrow = 2)
    var.coef.mean[1, 1] <- mean(var.coef.vec[seq(1, length(var.coef.vec), 4)])
    var.coef.mean[2, 1] <- mean(var.coef.vec[seq(2, length(var.coef.vec), 4)])
    var.coef.mean[1, 2] <- mean(var.coef.vec[seq(3, length(var.coef.vec), 4)])
    var.coef.mean[2, 2] <- mean(var.coef.vec[seq(4, length(var.coef.vec), 4)])
    print(var.coef.mean)
    
}
if( sim == 1){
    filename <- paste0("Sim_", sim ,"_varcoef_est.pdf")
    pdf(filename, width = 11, height = 8.5)
    ylim_max <- 0.15
    par(mar = c(4., 4.5, 1.5, 1))
    print(plot.ar.matrix(var.coef.mean, p = 1))
    dev.off()
}



tt <- sim
library(xtable)
detection.full <- c()
for(ii in c(4, 8, 12)){
    filename <- paste0("Sim_", tt, "_bn_", ii,".RData")
    load(filename)
    
    m0 <- length(brk)-1
    pts.check <- vector("list", N);
    for(j.1 in 1:N){
        pts.check[[j.1]] <- rep(0, m0)
        temp <- pts.final[[j.1]]
        if(length(temp)>0){
            for(i in 1: length(temp)){
                if (  (temp[i] < (brk[1] + (1/5)*(brk[2] - brk[1]) ) ) & (temp[i]> (brk[1] - (1/5)*(brk[1] - 0) ) ) ){
                    pts.check[[j.1]][1] <- temp[i]
                }
                if(m0 == 2){
                    if (  (temp[i] < (brk[2] + (1/5)*(brk[3] - brk[2]) ) ) & (temp[i]> (brk[2] - (1/5)*(brk[2] - brk[1]) ) ) ){
                        pts.check[[j.1]][2] <- temp[i]
                    }
                }
                
            }
        }
    }
    
    detection <- matrix(0, m0, 5)
    for(i in 1:(m0)){
        detection[(i),1] <- c(i); detection[(i),2] <- c(brk[i]/T); loc <- rep(0,N);
        for(j in 1:N){
            temp <- pts.check[[j]]; l <- length(temp); loc[j] <- temp[i];
        }
        loc <- loc[which(loc!=0)]; T.new <- length(loc); detection[(i),3] <- mean(loc/T);
        detection[(i),4] <- sd(loc/T); detection[(i),5] <- T.new/N;
    }
    
    for(i in 1:(m0)){
        for(j in 2:5){
            detection[i,j] <- round(as.numeric(detection[i,j]), digits = 4)
        }
    }
    detection.full <- rbind(detection.full, detection)
}

detection.full <- rbind(c("change points" , "truth", "mean", "std", "selection rate" ), detection.full)
detection.full <- cbind(c("&", rep("&model 1 ($b_n = 4$)", m0), rep("&model 1 ($b_n = 8$)", m0), rep("&model 1 ($b_n = 12$)", m0)), detection.full)
detection.res <- xtable(detection.full, hline.after = c(1,2))
print(detection.res, include.rownames = FALSE, include.colnames = FALSE, hline.after = c(1,1), sanitize.text.function = identity)




rm(list=ls(all=TRUE))
gc()
library(xtable)
sim = 4
if(sim %in% c(4) ){
    tt <- sim
    alpha.res <- c()
    for(ii in c(4, 8, 12)){
        filename <- paste0("Sim_", tt, "_bn_", ii,  ".RData")
        load(filename)
        beta_est_1 <- c(); beta_est_2 <- c(); beta_est_3 <- c()
        gamma_est_1 <- c(); gamma_est_2 <- c(); gamma_est_3 <- c()
        for(j.1 in 1:N){
            temp.coef <- lm.res[[j.1]]$coefficients
            beta_est_1 <- c(beta_est_1, temp.coef[1])
            beta_est_2 <- c(beta_est_2, temp.coef[3])
            beta_est_3 <- c(beta_est_3, temp.coef[5])
            gamma_est_1 <- c(gamma_est_1, temp.coef[2])
            gamma_est_2 <- c(gamma_est_2, temp.coef[4])
            gamma_est_3 <- c(gamma_est_3, temp.coef[6])
        }
        alpha.res <- rbind(alpha.res,
                           c(round(mean(beta_est_1), 4), round(sd(beta_est_1), 4) ),
                           c(round(mean(beta_est_2), 4), round(sd(beta_est_2), 4) ),
                           c(round(mean(beta_est_3, na.rm = TRUE), 4), round(sd(beta_est_3, na.rm = TRUE), 4) ),
                           c(round(mean(gamma_est_1), 4), round(sd(gamma_est_1), 4) ) ,
                           c(round(mean(gamma_est_2), 4), round(sd(gamma_est_2), 4) ) ,
                           c(round(mean(gamma_est_3, na.rm = TRUE), 4), round(sd(gamma_est_3, na.rm = TRUE), 4) )
        )
    }
    alpha.res <- cbind(c(rep(c(beta_1, beta_2, beta_3,
                               gamma_1, gamma_2, gamma_3), 3)), alpha.res)
    alpha.res <- cbind(c(rep(c("$\\beta_1$", "$\\beta_2$", "$\\beta_3$",
                               "$\\gamma_1$", "$\\gamma_2$", "$\\gamma_3$"), 3)), alpha.res)
    alpha.res <- cbind(c("\\multirow{ 6}{*}{model 1 ($b_n = 4$)}", rep("", 5), 
                         "\\multirow{ 6}{*}{model 1 ($b_n = 8$)}", rep("", 5),
                         "\\multirow{ 6}{*}{model 1 ($b_n = 12$)}", rep("", 5)), 
                       alpha.res)
    table.alpha_res <- xtable(alpha.res, hline.after = c(1, 2), digits = 4)
    print("estiamted alpha")
    print(table.alpha_res, include.rownames = FALSE, include.colnames = FALSE, sanitize.text.function = identity)
    
}


sim = 6
a = 0.5
b = 0
sd.log = 0.005^(0.5)


sim = 2
a = 0.05
b = 10
sd.log = 0.005^(0.5)


if(sim %in% c(6, 2) ){
    tt <- sim
    alpha.res <- c()
    for(ii in c( 4, 8, 12)){
        filename <- paste0("Sim_", tt, "_bn_", ii, "_a_", a, "_b_", b, "_sd_", sd.log^2,  ".RData")
        load(filename)
        beta_est_1 <- c(); beta_est_2 <- c(); beta_est_3 <- c()
        gamma_est_1 <- c(); gamma_est_2 <- c(); gamma_est_3 <- c()
        for(j.1 in 1:N){
            temp.coef <- lm.res[[j.1]]$coefficients
            beta_est_1 <- c(beta_est_1, temp.coef[1])
            beta_est_2 <- c(beta_est_2, temp.coef[3])
            beta_est_3 <- c(beta_est_3, temp.coef[5])
            gamma_est_1 <- c(gamma_est_1, temp.coef[2])
            gamma_est_2 <- c(gamma_est_2, temp.coef[4])
            gamma_est_3 <- c(gamma_est_3, temp.coef[6])
        }
        alpha.res <- rbind(alpha.res,
                           c(round(mean(beta_est_1), 4), round(sd(beta_est_1), 4) ),
                           c(round(mean(beta_est_2), 4), round(sd(beta_est_2), 4) ),
                           c(round(mean(beta_est_3, na.rm = TRUE), 4), round(sd(beta_est_3, na.rm = TRUE), 4) ),
                           c(round(mean(gamma_est_1), 4), round(sd(gamma_est_1), 4) ) ,
                           c(round(mean(gamma_est_2), 4), round(sd(gamma_est_2), 4) ) ,
                           c(round(mean(gamma_est_3, na.rm = TRUE), 4), round(sd(gamma_est_3, na.rm = TRUE), 4) ) ,
                           c(round(mean(unlist(a.final)), 4), round(sd(unlist(a.final)), 4) )
                           )
    }
    alpha.res <- cbind(c(rep(c(beta_1, beta_2, beta_3,
                               gamma_1, gamma_2, gamma_3,
                               a), 3)), alpha.res)
    alpha.res <- cbind(c(rep(c("$\\beta_1$", "$\\beta_2$", "$\\beta_3$",
                               "$\\gamma_1$", "$\\gamma_2$", "$\\gamma_3$",
                               "$a$" ), 3)), alpha.res)
    alpha.res <- cbind(c("\\multirow{ 7}{*}{model 1 ($b_n = 4$)}", rep("", 6), 
                         "\\multirow{ 7}{*}{model 1 ($b_n = 8$)}", rep("", 6),
                         "\\multirow{ 7}{*}{model 1 ($b_n = 12$)}", rep("", 6)), 
                       alpha.res)
    # alpha.res <- cbind(rep("", 24), alpha.res)
    table.alpha_res <- xtable(alpha.res, hline.after = c(1, 2), digits = 4)
    print("estiamted alpha")
    print(table.alpha_res, include.rownames = FALSE, include.colnames = FALSE, sanitize.text.function = identity)
    
}





rm(list=ls(all=TRUE))
gc()
library(xtable)

sim = 6
a = 0.5
b = 0
sd.log = 0.005^(0.5)


sim = 2
a = 0.05
b = 10
sd.log = 0.005^(0.5)



tt <- sim
library(xtable)
detection.full <- c()
for(ii in c(4, 8, 12)){
    filename <- paste0("Sim_", tt, "_bn_", ii, "_a_", a, "_b_", b, "_sd_", sd.log^2,  ".RData")
    
    load(filename)
    
    m0 <- length(brk)-1
    pts.check <- vector("list", N);
    for(j.1 in 1:N){
        pts.check[[j.1]] <- rep(0, m0)
        temp <- pts.final[[j.1]]
        if(length(temp)>0){
            for(i in 1: length(temp)){
                if (  (temp[i] < (brk[1] + (1/5)*(brk[2] - brk[1]) ) ) & (temp[i]> (brk[1] - (1/5)*(brk[1] - 0) ) ) ){
                    pts.check[[j.1]][1] <- temp[i]
                }
                if(m0 == 2){
                    if (  (temp[i] < (brk[2] + (1/5)*(brk[3] - brk[2]) ) ) & (temp[i]> (brk[2] - (1/5)*(brk[2] - brk[1]) ) ) ){
                        pts.check[[j.1]][2] <- temp[i]
                    }
                }
                
            }
        }
    }
    
    detection <- matrix(0, m0, 5)
    for(i in 1:(m0)){
        detection[(i),1] <- c(i); detection[(i),2] <- c(brk[i]/T); loc <- rep(0,N);
        for(j in 1:N){
            temp <- pts.check[[j]]; l <- length(temp); loc[j] <- temp[i];
        }
        loc <- loc[which(loc!=0)]; T.new <- length(loc); detection[(i),3] <- mean(loc/T);
        detection[(i),4] <- sd(loc/T); detection[(i),5] <- T.new/N;
    }
    
    for(i in 1:(m0)){
        for(j in 2:5){
            detection[i,j] <- round(as.numeric(detection[i,j]), digits = 4)
        }
    }
    detection.full <- rbind(detection.full, detection)
}

detection.full <- rbind(c("change points" , "truth", "mean", "std", "selection rate" ), detection.full)
detection.full <- cbind(c("&", rep("&model 1 ($b_n = 4$)", m0), rep("&model 1 ($b_n = 8$)", m0), rep("&model 1 ($b_n = 12$)", m0)), detection.full)
detection.res <- xtable(detection.full, hline.after = c(1,2))
print(detection.res, include.rownames = FALSE, include.colnames = FALSE, hline.after = c(1,1), sanitize.text.function = identity)



sim = 3
a = 0.5
tt <- sim
library(xtable)
detection.full <- c()
for(ii in c(4, 8, 12)){
    filename <- paste0("Sim_", tt, "_bn_", ii, "_a_", a,  ".RData")
    
    load(filename)
    
    m0 <- length(brk)-1
    pts.check <- vector("list", N);
    for(j.1 in 1:N){
        pts.check[[j.1]] <- rep(0, m0)
        temp <- pts.final[[j.1]]
        if(length(temp)>0){
            for(i in 1: length(temp)){
                if (  (temp[i] < (brk[1] + (1/5)*(brk[2] - brk[1]) ) ) & (temp[i]> (brk[1] - (1/5)*(brk[1] - 0) ) ) ){
                    pts.check[[j.1]][1] <- temp[i]
                }
                if(m0 == 2){
                    if (  (temp[i] < (brk[2] + (1/5)*(brk[3] - brk[2]) ) ) & (temp[i]> (brk[2] - (1/5)*(brk[2] - brk[1]) ) ) ){
                        pts.check[[j.1]][2] <- temp[i]
                    }
                }
                
            }
        }
    }
    
    detection <- matrix(0, m0, 5)
    for(i in 1:(m0)){
        detection[(i),1] <- c(i); detection[(i),2] <- c(brk[i]/T); loc <- rep(0,N);
        for(j in 1:N){
            temp <- pts.check[[j]]; l <- length(temp); loc[j] <- temp[i];
        }
        loc <- loc[which(loc!=0)]; T.new <- length(loc); detection[(i),3] <- mean(loc/T);
        detection[(i),4] <- sd(loc/T); detection[(i),5] <- T.new/N;
    }
    
    for(i in 1:(m0)){
        for(j in 2:5){
            detection[i,j] <- round(as.numeric(detection[i,j]), digits = 4)
        }
    }
    detection.full <- rbind(detection.full, detection)
}

detection.full <- rbind(c("change points" , "truth", "mean", "std", "selection rate" ), detection.full)
detection.full <- cbind(c("&", rep("&model 1 ($b_n = 4$)", m0), rep("&model 1 ($b_n = 8$)", m0), rep("&model 1 ($b_n = 12$)", m0)), detection.full)
detection.res <- xtable(detection.full, hline.after = c(1,2))
print(detection.res, include.rownames = FALSE, include.colnames = FALSE, hline.after = c(1,1), sanitize.text.function = identity)




if(sim %in% c(3) ){
    tt <- sim
    alpha.res <- c()
    for(ii in c( 4, 8, 12)){
        filename <- paste0("Sim_", tt, "_bn_", ii, "_a_", a,   ".RData")
        load(filename)
        alpha_est <- c()
        beta_est_1 <- c(); beta_est_2 <- c();
        gamma_est_1 <- c(); gamma_est_2 <- c(); 
        for(j.1 in 1:N){
            temp.coef <- lm.res[[j.1]]$coefficients
            beta_est_1 <- c(beta_est_1, temp.coef[1])
            beta_est_2 <- c(beta_est_2, temp.coef[3])
            gamma_est_1 <- c(gamma_est_1, temp.coef[2])
            gamma_est_2 <- c(gamma_est_2, temp.coef[4])
            alpha_est <- c(alpha_est, temp.coef[length(temp.coef)])
        }
        alpha.res <- rbind(alpha.res,
                           c(round(mean(beta_est_1), 4), round(sd(beta_est_1), 4) ),
                           c(round(mean(beta_est_2), 4), round(sd(beta_est_2), 4) ),
                           c(round(mean(gamma_est_1), 4), round(sd(gamma_est_1), 4) ) ,
                           c(round(mean(gamma_est_2), 4), round(sd(gamma_est_2), 4) ) ,
                           c(round(mean(alpha_est), 4), round(sd(alpha_est), 4) ),
                           c(round(mean(unlist(a.final)), 4), round(sd(unlist(a.final)), 4) )
        )
    }
    alpha.res <- cbind(c(rep(c(beta_1, beta_2, 
                               gamma_1, gamma_2, 
                               alpha,
                               a), 3)), alpha.res)
    alpha.res <- cbind(c(rep(c("$\\beta_1$", "$\\beta_2$", 
                               "$\\gamma_1$", "$\\gamma_2$", 
                               "$\\alpha$",
                               "$a$" ), 3)), alpha.res)
    alpha.res <- cbind(c("\\multirow{ 6}{*}{model 1 ($b_n = 4$)}", rep("", 5), 
                         "\\multirow{ 6}{*}{model 1 ($b_n = 8$)}", rep("", 5),
                         "\\multirow{ 6}{*}{model 1 ($b_n = 12$)}", rep("", 5)), 
                       alpha.res)
    table.alpha_res <- xtable(alpha.res, hline.after = c(1, 2), digits = 4)
    print("estiamted alpha")
    print(table.alpha_res, include.rownames = FALSE, include.colnames = FALSE, sanitize.text.function = identity)
    
}



rm(list=ls(all=TRUE))
gc()
library(xtable)
sim <- 2
a = 0.05
b = 10
sd.log = 0.005^(0.5)
tt <- sim
library(xtable)
MRPE.res <- c()
for(ii in c(4, 8, 12)){
    filename <- paste0("Sim_", tt, "_bn_", ii, "_a_", a, "_b_", b, "_sd_", sd.log^2,  ".RData")
    load(filename)
    MRPE_1.final <- rep(0, N); MRPE_1_I.final <- rep(0, N); MRPE_1_R.final <- rep(0, N); 
    for(j.1 in 1:N){
        set.seed(12345*j.1)
        t.test <- 20
        
        I <- rep(0, (T + t.test))
        R <- rep(0, (T + t.test))
        beta_t <- c()
        gamma_t <- c()
        I[1] <- 1
        for(i in 2:(T + t.test)){
            if(i > T){
                temp.1 <- rlnorm(1, meanlog = log(beta[T-1]), sdlog = sd.log )
                temp.2 <- rlnorm(1, meanlog = log(gamma[T-1]), sdlog = sd.log)
            }else{
                temp.1 <- rlnorm(1, meanlog = log(beta[i-1]), sdlog = sd.log )
                temp.2 <- rlnorm(1, meanlog = log(gamma[i-1]), sdlog = sd.log)
            }
            
            
            beta_t <- c(beta_t, temp.1)
            gamma_t <- c(gamma_t, temp.2)
            if(i == 2){
                I[i] <- temp.1*I[i-1] - temp.2[1]*I[i-1] + I[i-1]
                R[i] <- temp.2[1]*I[i-1] + R[i-1] 
            }else{
                I[i] <- temp.1*I[i-1] - temp.2*I[i-1] + I[i-1] 
                R[i] <- temp.2*I[i-1] + R[i-1]  
            }
            
        }
        
        
        T.full <- T + t.test
        I.true <- I
        R.true <- R
        
        a <- 1/100*5
        b <- 10
        # if there is underreport issue
        for(t in 1:T.full){
            rate <- 1/(1+ b*exp(-a*(t-1)) )
            print(rate)
            if(t < 2){
                I[t] <- I.true[t]
                R[t] <- R.true[t]
                
            }else{
                I[t] <- (I.true[t]-I.true[t-1])*rate + I[t-1]
            }
            
            
        }
        
        I.obs <- I
        R.obs <- R
        
        
        a.val <- a.final[[j.1]]
        b.val <- b
        I <-  I.obs
        R <-  R.obs
        for(t in 2:T.full){
            rate <- (1 + b.val*exp(-a.val*(t-1))  )
            print(rate)
            I[t] <- (I.obs[t] - I.obs[t-1])*rate + I[t-1]
            
        }
        
        y.list <- vector("list", T + t.test - 1);
        x.list <- vector("list", T + t.test - 1);
        
        for(i in 2:(T + t.test) ){
            y.list[[i-1]] <- matrix(c(R[i] - R[i-1], I[i] - I[i-1]), 2, 1);
            x.temp <- matrix(0, 2, 2);
            x.temp[1,2] <- I[i-1];
            x.temp[2,1] <- I[i-1];
            x.temp[2,2] <- -I[i-1];
            x.list[[i-1]] <- x.temp;
        }
        
        Y <- y.list[[1]];
        for(i in 2:(T + t.test -1)){
            Y <- rbind(Y, y.list[[i]])
        }
        
        X <- x.list[[1]];
        for(i in 2:(T + t.test -1)){
            X <- rbind(X, x.list[[i]])
        }
        
        Y.full <- Y
        X.full <- X
        
        Y.test <- as.matrix(Y.full[(nrow(Y) - (t.test)*2 + 1):nrow(Y), ])
        X.test <- X.full[(nrow(X) - (t.test)*2 + 1):nrow(X), ]
        Y.train <- as.matrix(Y.full[1 : (nrow(Y) - (t.test)*2 ), ])
        X.train <- X.full[1 : (nrow(X) - (t.test)*2 ), ]
        
        
        I.test <- I.obs[(T + 1): (T + t.test)]
        R.test <- R.obs[(T + 1): (T + t.test)]
        
        
        cp <- c(1, pts.final[[j.1]], n + 1)
        m <- length(cp) - 1
        Y.hat.1.new <- X.test%*%c(lm.res[[j.1]]$coefficients[c(2*m-1, m*2)]) 
        
        R.hat.1.new <- rep(0, t.test)
        for(i in 1:t.test){
            R.hat.1.new[i] <- R[T + (i - 1)] + Y.hat.1.new[(i - 1)*2 + 1]
        }
        
        I.hat.1.new <- rep(0, t.test)
        for(i in 1:t.test){
            rate <- 1/(1+ b.val*exp(-a.val*((T+i)-1)) )
            # print(rate)
            I.hat.1.new[i] <- I.obs[T + (i - 1)] + Y.hat.1.new[(i - 1)*2 + 2]*rate
        }
        
        MRPE_1_new <- mean(  abs ( (     c(R.hat.1.new, I.hat.1.new) - c(R.test, I.test) )  /c(R.test, I.test)  )[c(R.test, I.test) > 0]  )
        MRPE_1_new_I <- mean(  abs ( (  c(I.hat.1.new) - c(I.test) )  /c(I.test)  )[c(I.test) > 0]  )
        MRPE_1_new_R <- mean(  abs ( (     c(R.hat.1.new) - c(R.test)     )  /c(R.test)  )[c(R.test) > 0]  )
        
        MRPE_1.final[j.1] <- MRPE_1_new; 
        MRPE_1_I.final[j.1] <- MRPE_1_new_I; 
        MRPE_1_R.final[j.1] <- MRPE_1_new_R; 
        
    }
    
    MRPE.res <- rbind(MRPE.res,
                      round(c(mean(MRPE_1.final, na.rm =TRUE), 
                              mean(MRPE_1_I.final, na.rm =TRUE), 
                              mean(MRPE_1_R.final, na.rm =TRUE) ), 6))
    
}
MRPE.res <- cbind(c( "model 1 ($b_n = 4$)", "model 1 ($b_n = 8$)", "model 1 ($b_n = 12$)"), MRPE.res)
table.MRPE_res <- xtable(MRPE.res, hline.after = c(1,2), digits = 6)
print("MRPE")
print(table.MRPE_res, include.rownames = FALSE, include.colnames = FALSE, sanitize.text.function = identity)


