
#' Plot the coefficient matrix
plot.ar.matrix <- function (phi, p, name = NULL) {
  B <- phi
  if (nrow(B) == 1) {
    B <- matrix(B[, 1:ncol(B)], nrow = 1)
  }
  else {
    B <- B[, 1:ncol(B)]
  }
  k <- nrow(B)
  s1 <- 0
  m <- 0
  s <- 0
  s <- s + s1
  text <- c()
  for (i in 1:p) {
    text1 <- as.expression(bquote(bold(Phi)^(.(i))))
    text <- append(text, text1)
  }
  if (m > 0) {
    for (i in (p + 1):(p + s + 1)) {
      text1 <- as.expression(bquote(bold(beta)^(.(i - p - 
                                                    s1))))
      text <- append(text, text1)
    }
  }
  f <- function(m) t(m)[, nrow(m):1]
  rgb.palette <- colorRampPalette(c("blue", "white", "red"), space = "Lab")
  at <- seq(k/2 + 0.5, p * (k) + 0.5, by = k)
  if (m > 0) {
    at2 <- seq(p * k + m/2 + 0.5, p * k + s * m + 0.5, by = m)
  }
  else {
    at2 = c()
  }
  at <- c(at, at2)
  se2 = seq(1.75, by = k, length = k)
  L2 <- levelplot(as.matrix(f(B)), at =seq( -1, 1, length=101),col.regions = rgb.palette(100), 
                  colorkey = NULL, xlab = NULL, ylab = NULL, main = list(label = name, 
                                                                         cex = 1), panel = function(...) {
                                                                           panel.levelplot(...)
                                                                           panel.abline(a = NULL, b = 1, h = seq(1.5, m * s + 
                                                                                                                   p * k + 0.5, by = 1), v = seq(1.5, by = 1, length = p * 
                                                                                                                                                   k + m * s), lwd = 0.5)
                                                                           bl1 <- seq(k + 0.5, p * k + 0.5, by = k)
                                                                           b23 <- seq(p * k + 0.5, p * k + 0.5 + s * m, by = m)
                                                                           b1 <- c(bl1, b23)
                                                                           panel.abline(a = NULL, b = 1, v = p * k + 0.5, lwd = 3)
                                                                           panel.abline(a = NULL, b = 1, v = b1, lwd = 2)
                                                                         }, scales = list(x = list(alternating = 1, labels = text, 
                                                                                                   cex = 2, at = at, tck = c(0, 0)), y = list(alternating = 0, 
                                                                                                                                              tck = c(0, 0))))
  return(L2)
}


#' Plot the linear coefficient matrix
plot.coef.matrix <- function (phi, p, name = NULL) {
    B <- phi
    if (nrow(B) == 1) {
        B <- matrix(B[, 1:ncol(B)], nrow = 1)
    }
    else {
        B <- B[, 1:ncol(B)]
    }
    k <- nrow(B)
    k.x <- ncol(B)/p
    k.y <- nrow(B)
    print(k.x)
    print(k.y)
    
    s1 <- 0
    m <- 0
    s <- 0
    s <- s + s1
    text <- c()
    for (i in 1:p) {
        text1 <- as.expression(bquote(bold(beta)^(.(i))))
        text <- append(text, text1)
    }
    if (m > 0) {
        for (i in (p + 1):(p + s + 1)) {
            text1 <- as.expression(bquote(bold(beta)^(.(i - p - 
                                                            s1))))
            text <- append(text, text1)
        }
    }
    f <- function(m) t(m)[, nrow(m):1]
    rgb.palette <- colorRampPalette(c("blue", "white", "red"), space = "Lab")
    at <- seq(k.x/2 + 0.5, p * (k.x) + 0.5, by = k.x)
    if (m > 0) {
        at2 <- seq(p * k + m/2 + 0.5, p * k + s * m + 0.5, by = m)
    }
    else {
        at2 = c()
    }
    at <- c(at, at2)
    se2 = seq(1.75, by = k, length = k)
    L2 <- levelplot(as.matrix(f(B)),  at =seq( -max(max(abs(phi))), max(max(abs(phi))), length=101),col.regions = rgb.palette(100), 
                    colorkey = NULL, xlab = NULL, ylab = NULL, main = list(label = name, 
                                                                           cex = 1), panel = function(...) {
                                                                               panel.levelplot(...)
                                                                               panel.abline(a = NULL, b = 1, h = seq(1.5, m * s + 
                                                                                                                         p * k.x + 0.5, by = 1), v = seq(1.5, by = 1, length = p * 
                                                                                                                                                           k.x + m * s), lwd = 0.5)
                                                                               bl1 <- seq(k.x + 0.5, p * k.x + 0.5, by = k.x)
                                                                               b23 <- seq(p * k.x + 0.5, p * k.x + 0.5 + s * m, by = m)
                                                                               b1 <- c(bl1, b23)
                                                                               panel.abline(a = NULL, b = 1, v = p * k.x + 0.5, lwd = 3)
                                                                               panel.abline(a = NULL, b = 1, v = b1, lwd = 2)
                                                                           }, scales = list(x = list(alternating = 1, labels = text, 
                                                                                                     cex = 2, at = at, tck = c(0, 0)), y = list(alternating = 0, 
                                                                                                                                                tck = c(0, 0))))
    return(L2)
}


#' Plot the cross-validation score 
mspe.plot <- function(pred.error,lambda,tune.final=NULL,jj){
    plot( lambda, pred.error, type = 'o', col = "blue", main=c(jj))
    abline(v=tune.final)
}


#' BIC  and HBIC function        
BIC <- function(residual, phi, gamma.val = 10){
  p.y <- length(phi[, 1]); 
  p.x <- length(phi[1, ]);
  T.new <- length(residual[1, ]); 
  print("p.x"); print(p.x); 
  print("p.y"); print(p.y); 
  print("T.new"); print(T.new);
  # count : non-zero coefficient
  count <- 0;
  for (i in 1:p.y){
    for (j in 1:p.x){
      if(phi[i,j] != 0){
        count <- count + 1;
      }
    }
  }
  print("nonzero count"); print(count)
  
  sigma.hat <- 0*diag(p.y);
  for(i in 1:T.new){sigma.hat <- sigma.hat +  residual[,i]%*%t(residual[,i]);  }
  sigma.hat <- (1/(T.new))*sigma.hat;
  ee.temp <- min(eigen(sigma.hat)$values); 
  if(ee.temp <= 10^(-8)){
    print("nonpositive eigen values!")
    sigma.hat <- sigma.hat + (2.0)*(abs(ee.temp) + 10^(-3))*diag(p.y);
  }
  # print(ee.temp)
  #print(det(sigma.hat))
  log.det <- log(det(sigma.hat)); 
  print(log.det)
  print(log(T.new)*count/T.new)
  print(2*gamma.val*log(p.x*p.y)*count/T.new)
  return(list(BIC = log.det + log(T.new)*count/T.new , HBIC = log.det + 2*gamma.val*log(p.x*p.y)*count/T.new))
}

#' Simulate the linear regression model data with break points
lm.sim.break <- function (nobs, px, cnst = NULL, phi = NULL, theta = NULL, sigma, sigma_x =1, brk = nobs+1) {
    if (!is.matrix(sigma)) 
        sigma = as.matrix(sigma)
    #k = nrow(sigma)
    p.y <-  nrow(sigma)
    p.x <- px
    m <- length(brk)
    nT <- nobs
    
    # error term
    data_e = rmvnorm(nT, rep(0, p.y), sigma)
    # data x
    data_x = rmvnorm(nT, rep(0, p.x), sigma_x*diag(p.x))
    # data y
    data_y = matrix(0, nT, p.y)
    if (length(cnst) == 0) 
        cnst = rep(0, p.y)
    if (m == 1){
        for (it in 1:nT) {
            tmp = matrix(data_e[it, ], 1, p.y)
            tmp_x = matrix(data_x[it, ], 1, p.x)
            phj = phi[, 1:p.x]
            if (p.y == 1){
                tmp = tmp + tmp_x %*% phj
            }else{
                tmp = tmp + tmp_x %*% t(phj)
            }
            data_y[it, ] = cnst + tmp
        }
    }
    
    if (m > 1){
        for (it in 1:(brk[1]-1)) {
            tmp = matrix(data_e[it, ], 1, p.y)
            tmp_x = matrix(data_x[it, ], 1, p.x)
            #idx = (i - 1) * p.x
            phj = phi[, 1:p.x]
            if (p.y == 1){
                tmp = tmp + tmp_x %*% phj
            }else{
                tmp = tmp + tmp_x %*% t(phj)
            }
            #tmp = tmp + tmp_x %*% t(phj)
            data_y[it, ] = cnst + tmp
        }
        for ( mm in 1:(m-1)){
            for (it in (brk[mm]):(brk[mm+1]-1) ) {
                tmp = matrix(data_e[it, ], 1, p.y)
                tmp_x = matrix(data_x[it, ], 1, p.x)
                phj = phi[, (mm*p.x + 1):(mm*p.x+ p.x)]
                if (p.y == 1){
                    tmp = tmp + tmp_x %*% phj
                }else{
                    tmp = tmp + tmp_x %*% t(phj)
                }
                #tmp = tmp + tmp_x %*% t(phj)
                data_y[it, ] = cnst + tmp
            }
        }
    }
    
    data_y = data_y[1:nT, ]
    data_x = data_x[1:nT, ]
    data_e = data_e[1:nT, ]
    lmsim <- list(series_y = data_y, series_x = data_x, noises = data_e)
}



#' Main function for change point detection
################################################################
#MvLR: Multivariate Linear Regression
#MLR: Multiple Linear Regression
#VAR: Vector autoregression
tbfl <- function(method, data_y, data_x = NULL, lambda.1.cv = NULL, lambda.2.cv = NULL, q = 1, max.iteration = 100, tol = 10^(-2), block.size = NULL, blocks = NULL, fixed_index = NULL, HBIC = FALSE, gamma.val = NULL){
  method.full <- c("MvLR", "MLR", "VAR");
  if ( !(method %in% method.full) ){print("ERROR: incorrect method name!"); break; }
  T  <- length(data_y[,1]);
  ############# block size and blocks ###########
  if(is.null(block.size) && is.null(blocks) ){
    block.size = floor(sqrt(T));
    blocks <- seq(1,T+1,block.size);
  }else if( !is.null(block.size) && is.null(blocks)){
    blocks <- seq(1,T+1,block.size);
  }else if(!is.null(block.size) && !is.null(blocks)){
    #check if the block.size and blocks match
    n.new <- length(blocks) - 1;
    blocks.size.check <- sapply(c(1:n.new), function(jjj) blocks[jjj+1] - blocks[jjj]  );
    if( sum(blocks.size.check[1: (length(blocks.size.check)-1 )] != block.size ) >0 ){
      stop("Error: The block.size and blocks can't match!")
    }
  }
  
  if(blocks[length(blocks)] < T+1){
    blocks <- c(blocks[-length(blocks)],T+1)
  }
  
  n.new <- length(blocks) - 1;
  blocks.size <- sapply(c(1:n.new), function(jjj) blocks[jjj+1] - blocks[jjj]  );
  #print(blocks.size)
  
  #sample the cv index for cross-validation
  #bbb <- floor(n.new/5);
  #aaa <- sample(1:5, 1);
  bbb <- floor(n.new/4);
  aaa <- 4;
  cv.index <- seq(aaa, n.new, floor(n.new/bbb)); 
  cv.l <- length(cv.index); 
  print(cv.index)
  
  
  if (method == 'MvLR' |  method =="MLR"){
    if (is.null(data_x)){
      print("ERROR: empty predictor data!"); break;
    }
    p.y <- length(data_y[1,]); p.x <- length(data_x[1,]);
    
    ############# Tuning parameter ################
    if(is.null(lambda.1.cv)){
      lambda.1.max <- lambda_warm_up_lm(data_y, data_x, blocks, cv.index)$lambda_1_max
      if(blocks[2] <= (p.x + p.y) ){
        epsilon <-  10^(-3)
      }
      if(blocks[2] >= (p.x + p.y) ){
        epsilon <-  10^(-4)
      }
      nlam <- 10 
      lambda.1.min <-  lambda.1.max*epsilon
      delata.lam <- (log(lambda.1.max)-log(lambda.1.min))/(nlam -1)
      lambda.1.cv <-  sapply(1:(nlam), function(jjj) lambda.1.min*exp(delata.lam*(nlam-jjj)))
    }
    
    if(is.null(lambda.2.cv)){
      lambda.2.cv <-  c(10*sqrt( (log(p.x) + log(p.y)  )/T),1*sqrt((log(p.x) + log(p.y)  )/T),0.10*sqrt((log(p.x) + log(p.y)  )/T))
    }
    
    ####################################################
    ########## first step       #######################
    ####################################################
    temp.first <- lm.first.step.blocks(data_y, data_x, lambda.1.cv, lambda.2.cv, 
                                       max.iteration = max.iteration, tol = tol, blocks , cv.index, HBIC = HBIC, gamma.val = gamma.val)
    cp.first <- temp.first$pts.list;
    cl.number <- length(cp.first);
    beta.est <- temp.first$beta.full
    temp.first.all<- temp.first
    ####################################################
    ########## second step       #######################
    ####################################################
    if(length(cp.first) > 0){
      temp.second <- lm.second.step.search(data_y, data_x, max.iteration = max.iteration, tol = tol, cp.first, beta.est, blocks)
      temp.second.all <- temp.second
      cp.final <- temp.second$cp.final;
    }else{
      cp.final <- c()
      print('no change points!')
    }
    return(list(cp.first = cp.first, beta.est = beta.est, cp.final = cp.final ))
  }
  if (method == 'VAR'){
     p <- length(data_y[1,]);
     
     ############# Tuning parameter ################
     if(is.null(lambda.1.cv)){
       lambda.1.max <- lambda_warm_up_var(data_y, q, blocks, cv.index)$lambda_1_max
       if(blocks[2] <= 2*p ){
         epsilon <-  10^(-3)
       }
       if(blocks[2] >= 2*p ){
         epsilon <-  10^(-4)
       }
       nlam <- 10 
       lambda.1.min <-  lambda.1.max*epsilon
       delata.lam <- (log(lambda.1.max)-log(lambda.1.min))/(nlam -1)
       lambda.1.cv <-  sapply(1:(nlam), function(jjj) lambda.1.min*exp(delata.lam*(nlam-jjj)))
     }
     
     if(is.null(lambda.2.cv)){
       lambda.2.cv <-  c(10*sqrt(log(p)/T),1*sqrt(log(p)/T),0.10*sqrt(log(p)/T))
     }
     
     ####################################################
     ########## first step       #######################
     ####################################################
     temp.first <- var.first.step.blocks(data_y, lambda.1.cv, lambda.2.cv, q= q,  max.iteration = max.iteration, tol = tol, blocks , cv.index)
     cp.first <- temp.first$pts.list;
     cl.number <- length(cp.first);
     beta.est <- temp.first$phi.full
     temp.first.all<- temp.first
     ####################################################
     ########## second step       #######################
     ####################################################
     if(length(cp.first) > 0){
       temp.second<- var.second.step.search(data_y,  q= q, max.iteration = max.iteration, tol = tol, cp.first, beta.est, blocks)
       temp.second.all<- temp.second
       cp.final<- temp.second$cp.final;
     }else{
       cp.final <- c()
       print('no change points!')
     }
     return(list(cp.first = cp.first, beta.est = beta.est, cp.final =cp.final ))
  }

}
  


lm.first.step.blocks <- function(data_y, data_x, lambda1, lambda2, max.iteration = max.iteration, tol = tol,  blocks, cv.index, fixed_index = NULL, nonfixed_index = NULL, HBIC = FALSE, gamma.val = NULL){
  
  #create the tuning parmaeter combination of lambda1 and lambda2
  lambda.full <- expand.grid(lambda1, lambda2)
  kk <- length(lambda.full[, 1]);
  
  n.new <- length(blocks) - 1;
  blocks.size <- sapply(c(1:n.new), function(jjj) blocks[jjj+1] - blocks[jjj]  );
  cv.l <- length(cv.index); 
  cv <- rep(0, kk); 
  phi.final <- vector("list", kk);
  phi.final.2 <- vector("list", kk);
  
  data.y.temp <- data_y; data.x.temp <- data_x
  T <- length(data.y.temp[, 1]); 
  p.y <- length(data.y.temp[1, ]); p.x.all <- length(data.x.temp[1, ]);
  p.x <- p.x.all - length(fixed_index);
  
  flag.full <- rep(0,kk);
  
  for (i in 1:kk) {
    print(i)
    print("##################lambda1:")
    print(lambda.full[i,1])
    print("##################lambda2:")
    print(lambda.full[i,2])
    if(!is.null(fixed_index)){
      print("Some coefficients are constant!")
      if ( i == 1){
        test <- lm_partial_break_fit_block(data.y.temp,data.x.temp, lambda.full[i,1],lambda.full[i,2], max_iteration = max.iteration, tol = tol,
        initial_phi =  0.0+matrix(0.0, p.y, p.x*n.new), initial_phi_2 =  0.0+matrix(0.0,p.y,(p.x.all-p.x)), blocks = blocks, cv.index, fixed_index, nonfixed_index)
        flag.full[i] <- test$flag;
      }
      if ( i > 1 ){
        initial.phi <- phi.final[[i-1]]
        initial.phi.2 <- phi.final.2[[i-1]]
        if(max(abs(phi.final[[(i-1)]])) > 10^3  ){initial.phi <- 0*phi.final[[(i-1)]];}
        test <- lm_partial_break_fit_block(data.y.temp, data.x.temp, lambda.full[i,1], lambda.full[i,2], max_iteration = max.iteration, tol = tol,
                                           initial_phi =  initial.phi, initial_phi_2 =  initial.phi.2, blocks = blocks, cv.index, fixed_index, nonfixed_index)
        flag.full[i] <- test$flag;
      }
      #print(test$phi.hat)
      #print(test$phi.hat.2)
      phi.hat.full <- test$phi.hat;
      phi.final[[i]] <- phi.hat.full;
      phi.hat.full.2 <- test$phi.hat.2;
      phi.final.2[[i]] <- phi.hat.full.2;
      
      phi.full.all <- vector("list",n.new);
      forecast <- matrix(0,p.y,T);
      forecast.new <- matrix(0,p.y,cv.l);
      
      phi.hat <- phi.hat.full;
      phi.full.all[[1]] <- matrix(phi.hat[,(1):(p.x)], ncol = p.x);
      forecast[, (blocks[1]):(blocks[2]-1)] <- pred.block(t(data_x[, nonfixed_index]), phi.full.all[[1]], blocks[1], p.x, p.y, blocks[2] - blocks[1]);
      forecast[, (blocks[1]):(blocks[2]-1)] <- forecast[, (blocks[1]):(blocks[2]-1)] + phi.hat.full.2%*%t(as.matrix(data_x[(blocks[1]):(blocks[2]-1), fixed_index]))
      
      for(i.1 in 2:n.new){
        phi.full.all[[i.1]] <- matrix(phi.full.all[[i.1-1]] + phi.hat[,((i.1-1)*p.x+1):(i.1*p.x)], ncol = p.x);
        forecast[, (blocks[i.1]):(blocks[i.1+1]-1)] <- pred.block(t(data_x[, nonfixed_index]), phi.full.all[[i.1]], blocks[i.1], p.x, p.y, blocks[i.1+1] - blocks[i.1]);
        forecast[, (blocks[i.1]):(blocks[i.1+1]-1)] <- forecast[, (blocks[i.1]):(blocks[i.1+1]-1)] + phi.hat.full.2%*%t(as.matrix(data_x[(blocks[i.1]):(blocks[i.1+1]-1), fixed_index]))
        
        # forecast[, (blocks[i.1]):(blocks[i.1+1]-1)] <- pred.block(t(data_x[, nonfixed_index]), phi.full.all[[i.1-1]], blocks[i.1], p.x, p.y, blocks[i.1+1]-blocks[i.1]);
      }
      
      forecast.new <- matrix(0,p.y,cv.l);
      for(j in (1):cv.l){
        forecast.new[, j] <- pred(t(data_x[, nonfixed_index]), phi.full.all[[(cv.index[j])]], blocks[cv.index[j]+1]-1, p.x, p.y)
        forecast.new[, j] <- forecast.new[, j] + phi.hat.full.2%*%as.matrix(data_x[blocks[cv.index[j]+1]-1, fixed_index])
      }
      
      temp.index <- rep(0,cv.l);
      for(ff in 1:cv.l){temp.index[ff] <- blocks[cv.index[ff]+1]-1;}
      # cv[i] <- (1/(p.y*cv.l))*sum( (forecast.new - t(data_y[temp.index,])  )^2 );
      
      print("different cv formula!")
      cv[i] <- (1/(p.y*cv.l))*sum( ( (forecast.new - t(data_y[temp.index, ]))/t(data_y[temp.index, ])  )^2);
      
      
      print("============cv-result=======================")
      print(cv[i])
      print("====================================")
      
      
    }
    if(is.null(fixed_index)){
      print("All coefficients are piecewise constant!")
      if ( i == 1){
        test <- lm_break_fit_block(data.y.temp, data.x.temp, lambda.full[i,1], lambda.full[i,2], max_iteration = max.iteration, tol = tol, initial_phi =  0.0+matrix(0.0,p.y,p.x*n.new), blocks = blocks, cv.index)
        flag.full[i] <- test$flag;
      }
      if ( i > 1 ){
        initial.phi <- phi.final[[i-1]]
        if(max(abs(phi.final[[(i-1)]])) > 10^3  ){initial.phi <- 0*phi.final[[(i-1)]];}
        test <- lm_break_fit_block(data.y.temp,data.x.temp, lambda.full[i,1], lambda.full[i,2], max_iteration = max.iteration, tol = tol, initial_phi =  initial.phi, blocks = blocks, cv.index)
        flag.full[i] <- test$flag;
      }
      #print(test$phi.hat)
      phi.hat.full <- test$phi.hat;
      phi.final[[i]] <- phi.hat.full;
      
      
      #forecast the time series based on the estimated matrix Phi (beta for the linear regression model)
      #and compute the forecast error
      phi.full.all <- vector("list", n.new);
      forecast <- matrix(0,p.y,T);
      forecast.new <- matrix(0,p.y,cv.l);
      
      phi.hat <- phi.hat.full;
      phi.full.all[[1]] <- matrix(phi.hat[,(1):(p.x)], ncol = p.x);
      forecast[, (blocks[1]):(blocks[2]-1)] <- pred.block(t(data_x), phi.full.all[[1]], blocks[1], p.x, p.y, blocks[2]-blocks[1]);
      for(i.1 in 2:n.new){
        phi.full.all[[i.1]] <- matrix(phi.full.all[[i.1-1]] + phi.hat[,((i.1-1)*p.x+1):(i.1*p.x)], ncol = p.x);
        forecast[, (blocks[i.1]):(blocks[i.1+1]-1)] <- pred.block(t(data_x), phi.full.all[[i.1]], blocks[i.1], p.x, p.y, blocks[i.1+1]-blocks[i.1]);
        # forecast[, (blocks[i.1]):(blocks[i.1+1]-1)] <- pred.block(t(data_x), phi.full.all[[i.1-1]], blocks[i.1], p.x, p.y, blocks[i.1+1]-blocks[i.1]);
      }
      forecast.new <- matrix(0, p.y, cv.l);
      for(j in (1):cv.l){
        forecast.new[, j] <- pred(t(data_x), phi.full.all[[(cv.index[j])]], blocks[cv.index[j]+1]-1, p.x, p.y)
      }
      temp.index <- rep(0,cv.l);
      for(ff in 1:cv.l){temp.index[ff] <- blocks[cv.index[ff] + 1] - 1;}
      
      # cv[i] <- (1/(p.y*cv.l))*sum( (forecast.new - t(data_y[temp.index,])  )^2 );
      print("different cv formula!")
      print(forecast.new)
      print(t(data_y[temp.index, ]))
      print((forecast.new - t(data_y[temp.index, ]))/t(data_y[temp.index, ]))
      cv[i] <- 1/(p.y*cv.l)*sum( ( ( (forecast.new - t(data_y[temp.index, ]))/t(data_y[temp.index, ]))[ (t(data_y[temp.index, ])) > 0 ]   )^2);
      
      print("============cv-result=======================")
      print(cv[i])
      print("====================================")
    }
    
  }
  
  
  
  lll <- min(which(cv==min(cv)));
  
  mspe.plot(cv, c(1:kk), jj="1")
  abline(v = seq(length(lambda1), length(lambda1)*(length(lambda2)-1), length.out =length(lambda2)-1)+0.5)
  abline(v= lll, col="red")
  
  phi.hat.full <- phi.final[[lll]];
  beta.fixed.full <- phi.final.2[[lll]];

  #jumps.sq is the L2 norm square
  #jumps.l1 is the L1 norm
  # again, note that here the phi.hat.full is the estimated theta in the paper
  jumps.l2 <- rep(0,n.new);
  for(i in c(2:n.new)){
    jumps.l2[i] <- (sum((phi.hat.full[,((i-1)*p.x+1):(i*p.x)] )^2 ));
  }
  jumps.l1 <- rep(0,n.new);
  for(i in c(2:n.new)){
    jumps.l1[i] <- (sum(abs(phi.hat.full[,((i-1)*p.x+1):(i*p.x)] ) ));
  }
  
  print(jumps.l2)
  print(jumps.l1)
  plot(jumps.l2, type = 'o', main= 'l2 norm')
  # plot(jumps.l1,type = 'o', main= 'l1 norm')
  
  #ignore the large jump at the boundary!!!!!!!!!!!!!!!!!!!!
  print('use l2 norm!')
  jumps <- jumps.l2
  # print('use l1 norm!')
  # jumps <- jumps.l1
  #ignore_num <- round(100/mean(blocks.size))
  ignore_num <- min(3, round(100/mean(blocks.size)))
  jumps[1:ignore_num]  <- 0
  jumps[(length(jumps)-(ignore_num-1)):length(jumps)] <- 0
  
  plot(jumps, type = 'o', main= 'l2 norm')
  
  ##################################################################  

  # use BIC to determine the k-means
  BIC.diff <- 1
  BIC.old <- 10^8
  pts.sel <- c()
  loc.block.full <- c()
  while(BIC.diff > 0 & length(unique(jumps)) > 1 ){
    pts.sel.old <- pts.sel
    #use kmeans to hard threshold the jumps
    if( length(unique(jumps)) > 2 ){
      print("consider 2 clusters for fit.2")
      clus.2 <- kmeans(jumps, centers = 2); fit.2 <- clus.2$betweenss/clus.2$totss; print(fit.2);
      if(fit.2 < 0.20){
        print("no significant jumps!!")
        pts.sel <- c(pts.sel);
      }
      if( fit.2 >= 0.20 ){
        loc <- clus.2$cluster;
        if( clus.2$centers[1] > clus.2$centers[2]  ){
          loc.block <- which(loc==1);
        }
        if( clus.2$centers[1] < clus.2$centers[2]  ){
          loc.block <- which(loc==2);
        }
        pts.sel <- c(pts.sel, blocks[loc.block]);
        loc.block.full <- c(loc.block.full, loc.block)
      }
    }
    if( length(unique(jumps)) <= 2 ){
      if(length(unique(jumps)) == 2){
        loc.block <- which.max(jumps)
        pts.sel <- c(pts.sel, blocks[loc.block])
        loc.block.full <- c(loc.block.full, loc.block)
      }else{
        pts.sel <- c(pts.sel);
      }
    }

    print("pts.sel:"); print(sort(pts.sel))

    phi.hat.full.new <- phi.hat.full
    for(i in 2:n.new){
      if(!(i %in% loc.block.full)){
        phi.hat.full.new[, ((i-1)*p.x+1):(i*p.x)] <- matrix(0, p.y, p.x)
      }
    }


    phi.full.all.new <- vector("list", n.new);
    phi.full.all.new.temp <- vector("list", n.new);
    forecast.all.new <- matrix(0, p.y, T);

    phi.hat.new <- phi.hat.full.new;
    phi.full.all.new[[1]] <- matrix(phi.hat.new[, (1):(p.x)], ncol = p.x);
    phi.full.all.new.temp[[1]] <- matrix(phi.hat.new[, (1):(p.x)], ncol = p.x);
    
    if(!is.null(fixed_index)){
      forecast.all.new[, (blocks[1]):(blocks[2]-1)] <- pred.block(t(data_x[, nonfixed_index]), phi.full.all.new[[1]], blocks[1], p.x, p.y, blocks[2] - blocks[1]);
      forecast.all.new[, (blocks[1]):(blocks[2]-1)] <- forecast.all.new[, (blocks[1]):(blocks[2]-1)] + beta.fixed.full %*%t(as.matrix(data_x[(blocks[1]):(blocks[2]-1), fixed_index]))

      for(i.1 in 2:n.new){
        phi.full.all.new[[i.1]] <- matrix(phi.full.all.new[[i.1-1]] + phi.hat.new[,((i.1-1)*p.x+1):(i.1*p.x)], ncol = p.x);
        forecast.all.new[, (blocks[i.1]):(blocks[i.1+1]-1)] <- pred.block(t(data_x[, nonfixed_index]), phi.full.all.new[[i.1]], blocks[i.1], p.x, p.y, blocks[i.1+1] - blocks[i.1]);
        forecast.all.new[, (blocks[i.1]):(blocks[i.1+1]-1)] <- forecast.all.new[, (blocks[i.1]):(blocks[i.1+1]-1)] + beta.fixed.full %*%t(as.matrix(data_x[(blocks[i.1]):(blocks[i.1+1]-1), fixed_index]))
      }
    }
    
    
    if(is.null(fixed_index)){

      forecast.all.new[, (blocks[1]):(blocks[2]-1)] <- pred.block(t(data_x), phi.full.all.new[[1]],
                                                                  blocks[1], p.x, p.y, blocks[2] - blocks[1]);

      # for(i.1 in 2:n.new){
      #   phi.full.all.new[[i.1]] <- matrix(phi.full.all.new[[i.1-1]] + phi.hat.new[,((i.1-1)*p.x+1):(i.1*p.x)], ncol = p.x);
      #   forecast.all.new[, (blocks[i.1]):(blocks[i.1+1]-1)] <- pred.block(t(data_x), phi.full.all.new[[i.1]],
      #                                                                     blocks[i.1], p.x, p.y, blocks[i.1+1] - blocks[i.1]);
      # }
      print("loc.block.full:")
      print(loc.block.full)
      # print(phi.hat.full)
      for(i.1 in 2:n.new){
        phi.full.all.new.temp[[i.1]] <- matrix(phi.full.all.new.temp[[i.1-1]] + phi.hat.full[, ((i.1-1)*p.x+1):(i.1*p.x)], ncol = p.x);
          
      }
      for(i.1 in 2:n.new){
        #phi.full.all.new.temp keeps adding
        # phi.full.all.new.temp[[i.1]] <- matrix(phi.full.all.new.temp[[i.1-1]] + phi.hat.full[, ((i.1-1)*p.x+1):(i.1*p.x)], ncol = p.x);
        if((i.1 %in% loc.block.full)){
          print("there is a jump")
          phi.full.all.new[[i.1]] <- phi.full.all.new.temp[[i.1]]
        }
        if(!(i.1 %in% loc.block.full)){
          # print("not a jump")
          phi.full.all.new[[i.1]] <- phi.full.all.new[[i.1-1]]
          
          # 2020/12/23: use the middle estimation (which is more stable) 
          temp.loc <- c(1, loc.block.full, n.new+1)
          for(i.2 in 1:length(loc.block.full)){
            if(i.1 < temp.loc[i.2+1] & i.1 > temp.loc[i.2] ){
              idx <- floor( (temp.loc[i.2+1] + temp.loc[i.2] )/2)
              # print(i.1)
              # print('idx:')
              # print(idx)
              phi.full.all.new[[i.1]] <- phi.full.all.new.temp[[idx]] 
              
            }
          }
          if(i.1 > loc.block.full[length(loc.block.full)]){
            idx <- floor( (n.new + loc.block.full[length(loc.block.full)] )/2)
            # print(i.1)
            # print('idx:')
            # print(idx)
            phi.full.all.new[[i.1]] <- phi.full.all.new.temp[[idx]] 
            
          }
        }
        
          
          
        forecast.all.new[, (blocks[i.1]):(blocks[i.1+1]-1)] <- pred.block(t(data_x), phi.full.all.new[[i.1]],
                                                                          blocks[i.1], p.x, p.y, blocks[i.1+1] - blocks[i.1]);

        
      }
      

    }
    residual <- t(data.y.temp[(1:T), ]) - forecast.all.new;
    # print(phi.full.all.new)
    # print(phi.full.all.new.temp)
    # print(residual)

    # if(HBIC == TRUE | state.name %in% c("Florida", "California", "Texas", "South Carolina" )){
    if(HBIC == TRUE){
      print("Change to HBIC!")
      print('gamma value:')
      print(gamma.val)
      BIC.new <- BIC(residual, phi = phi.hat.full.new, gamma.val = gamma.val)$HBIC
    }else{
      BIC.new <- BIC(residual, phi = phi.hat.full.new )$BIC
    }
    print("BIC.new:"); print(BIC.new)
    BIC.diff <- BIC.old - BIC.new
    print("BIC.diff:");print(BIC.diff)
    BIC.old <- BIC.new
    if(BIC.diff <= 0){
      pts.sel <- sort(pts.sel.old)
      break
    }
    jumps[loc.block] <- 0
    plot(jumps, type = 'o', main= 'l2 norm')
  }

  print(pts.sel)
  
  if ( length(pts.sel) == 0){cp.final <- c(); pts.list <-  vector("list", 0);}
  if ( length(pts.sel) > 0){
    cp.final <- pts.sel;
    # REMOVE BOUNDARY POINTS and other cleaning
    # cp.final <- cp.final[which(cp.final > 3*mean(blocks.size))];
    # cp.final <- cp.final[which(cp.final < (T-3*mean(blocks.size)))];
    cp.final <- cp.final[which(cp.final > sum(blocks.size[1:3]))];
    cp.final <- cp.final[which(cp.final < (T-sum(blocks.size[(length(blocks.size)-2):length(blocks.size)])))];
    cp.final <- sort(cp.final);
    print(cp.final)
    
    
    # if there are multipler change points
    # use the k-means clustering 
    if(length(cp.final) > 5){
    # if(length(cp.final) > 4){
      print(fviz_nbclust(matrix(cp.final, length(cp.final), 1), kmeans, nstart = 25, method = "gap_stat", k.max = min(10, length(cp.final)-1), nboot = 100)+
              labs(subtitle = "Gap statistic method"))
      cl <- fviz_nbclust(matrix(cp.final, length(cp.final), 1), kmeans, nstart = 25, method = "gap_stat", k.max = min(10, length(cp.final)-1), nboot = 100)+
        labs(subtitle = "Gap statistic method")
      
      cl.data <- cl$data;
      gap <- cl.data$gap;
      se <- cl.data$SE.sim;
      i.cl <- 0;
      # while (i.cl < (length(gap)-1)) {
      #   i.cl <- i.cl + 1;
      #   if( gap[i.cl] > gap[i.cl+1] - se[i.cl+1] ){cl.number <- i.cl; break;}
      # }
      # while (i.cl < (length(gap) - 1)) {
      #   i.cl <- i.cl + 1;
      #   if( gap[i.cl] >= gap[i.cl + 1] ){cl.number <- i.cl; break;}
      # }
      cl.number <- which.max(gap)
      #cl.number
      
      cl.final <- kmeans(cp.final, centers = cl.number);
      pts.list <-  vector("list", cl.number);
      loc.new <- cl.final$cluster;
      cl.reorder = c(1:cl.number)[order(cl.final$centers)]
      for (i in c(1:cl.number)) {
        pts.i <- cp.final[which(loc.new==cl.reorder[i])]
        print(paste0("Cluster: ", i))
        print(pts.i)
        pts.list[[i]] <- pts.i
      }
    }
    
    #!!!!!!!!!!!!!!!!need  ajustment!!!!!!
    if(length(cp.final) <= 5 & length(cp.final) > 1 ){
    # if(length(cp.final) <= 4 & length(cp.final) > 1 ){
      print("small number of cp !!!!")
      cl.number <- length(cp.final);
      loc.new <- rep(1,length(cp.final))
      
      for (i in 2:length(cp.final)){
        # if (cp.final[i]-cp.final[i-1] <= max(3*mean(blocks.size))){
        if (cp.final[i]-cp.final[i-1] <= max(2*mean(blocks.size))){
          cl.number <-cl.number-1 
          loc.new[i] <-loc.new[i-1]
        }else{
          loc.new[i] <- i
        }
      }
      
      pts.list <-  vector("list",cl.number);
      #for (i in unique(loc.new)) {
      loc.new.unique <- unique(loc.new)
      for (i in 1:length(loc.new.unique)) {
        pts.i <- cp.final[which(loc.new==loc.new.unique[i])]
        print(paste0("Cluster: ", i))
        print(pts.i)
        pts.list[[i]] <- pts.i
      }
    }
    
    
    if(length(cp.final) == 1 ){
      cl.number <- length(cp.final);
      loc.new <- rep(1,length(cp.final))
      pts.list <-  vector("list",cl.number);
      for (i in unique(loc.new)) {
        pts.i <- cp.final[which(loc.new==i)]
        print(paste0("Cluster: ", i))
        print(pts.i)
        pts.list[[i]] <- pts.i
      }
    }
    
  }
  
  #compute the estimated beta
  phi.par.sum <- vector("list",n.new);
  phi.par.sum[[1]] <- phi.hat.full[,1:(p.x)];
  for(i in 2:n.new){
    phi.par.sum[[i]] <- phi.par.sum[[i-1]] + phi.hat.full[,((i-1)*p.x+1):(i*p.x)];
  }
  
  #plot(blocks[1:n.new],jumps,main = "JUMPS.FULL", type = "o")
  
  print("First step DONE!!!!+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++")
  return(list(jumps.l2 = jumps.l2, jumps.l1 = jumps.l1, theta = phi.hat.full, theta.full = phi.final,
              pts.list = pts.list, beta.full = phi.par.sum, beta.fixed.est = beta.fixed.full ))
  
}




lm.second.step.search <- function(data_y,data_x, max.iteration = max.iteration, tol = tol,  cp.first, beta.est, blocks){

  T <- length(data_y[,1]); p.y <- length(data_y[1,]); p.x <- length(data_x[1,]);
  
  n.new <- length(blocks) - 1;
  blocks.size <- sapply(c(1:n.new), function(jjj) blocks[jjj+1] - blocks[jjj]  );
  
  
  cp.search <- cp.first;
  cl.number <- length(cp.first)
  
  cp.list <- vector("list",cl.number+2);
  cp.list[[1]] <- c(1);
  cp.list[[cl.number+2]] <- c(T+1);
  
  cp.index.list <- vector("list",cl.number+2);
  cp.index.list[[1]] <- c(1);
  cp.index.list[[cl.number+2]] <- c(n.new+1);
  
  for (i in 1:cl.number) {
    cp.list[[i+1]] <- cp.first[[i]]
    cp.index.list[[i+1]] <- match(cp.first[[i]], blocks)
  }
  
  
  cp.search <- rep(0,cl.number);
  cp.list.full <- cp.list
  
  beta.hat.list <- vector("list", cl.number+1)
  for(i in 1:(cl.number)){
    idx <- floor((min(cp.index.list[[i+1]]) + max(cp.index.list[[i]]))/2);
    beta.hat.list[[i]] <- beta.est[[idx]]
    
    if(length(cp.list[[i+1]]) >1){
      cp.list.full[[i+1]] <- c((cp.list[[i+1]][1] + 1)  :  (cp.list[[i+1]][length(cp.list[[i+1]])]-1  ) )
    }
    if(length(cp.list[[i+1]]) == 1){
      cp.list.full[[i+1]] <- c((cp.list[[i+1]][1] -  (blocks.size[cp.index.list[[i+1]][1] ]) + 1) :  (cp.list[[i+1]][length(cp.list[[i+1]])] +   (blocks.size[cp.index.list[[i+1]][length(cp.list[[i+1]])] ]) -1   ) )
    }
    
    
    #compare the SSE of first num and last num
    num  <- cp.list.full[[i+1]][1]
    lb.1 <- min(cp.list[[i+1]]) - (blocks.size[cp.index.list[[i+1]][1] ]);
    ub.1 <- num - 1;
    len.1 <- ub.1 - lb.1 + 1;
    idx.1 <- floor((min(cp.index.list[[i+1]]) + max(cp.index.list[[i]]))/2) ;
    beta.hat <- beta.est[[idx.1]]
    forecast <- sapply(c(lb.1:ub.1), function(jjj) pred(t(data_x), matrix(beta.hat, ncol = p.x), jjj, p.x, p.y)  )
    if(len.1 == 1){
      temp.1 <- sum( (data_y[lb.1:ub.1,]-forecast)^2 );
    }else{
      temp.1 <- sum( (t(data_y[lb.1:ub.1,])-forecast)^2 );
    }
    
    lb.2 <- num ;
    ub.2 <- max(cp.list[[i+1]]) +   (blocks.size[cp.index.list[[i+1]][length(cp.list[[i+1]])] ]) -1;
    len.2 <- ub.2 - lb.2 + 1;
    idx.2 <- floor((min(cp.index.list[[i+2]]) + max(cp.index.list[[i+1]]))/2) ;
    beta.hat <- beta.est[[idx.2]]
    forecast <- sapply(c(lb.2:ub.2), function(jjj) pred(t(data_x), matrix(beta.hat, ncol = p.x), jjj, p.x, p.y)  )
    if(len.2 == 1){
      temp.2 <- sum( ( data_y[lb.2:ub.2,]-forecast)^2 );
    }else{
      temp.2 <- sum( (t(data_y[lb.2:ub.2,])-forecast)^2 );
    }
    sse1 <- temp.1 + temp.2;
    
    
    num  <- cp.list.full[[i+1]][length(cp.list.full[[i+1]])]
    lb.1 <- min(cp.list[[i+1]]) - (blocks.size[cp.index.list[[i+1]][1] ]);
    ub.1 <- num - 1;
    len.1 <- ub.1 - lb.1 + 1;
    idx.1 <- floor((min(cp.index.list[[i+1]]) + max(cp.index.list[[i]]))/2) ;
    beta.hat <- beta.est[[idx.1]]
    forecast <- sapply(c(lb.1:ub.1), function(jjj) pred(t(data_x), matrix(beta.hat, ncol = p.x), jjj, p.x, p.y)  )
    if(len.1 == 1){
      temp.1 <- sum( (data_y[lb.1:ub.1,]-forecast)^2 );
    }else{
      temp.1 <- sum( (t(data_y[lb.1:ub.1,])-forecast)^2 );
    }
    
    lb.2 <- num ;
    ub.2 <- max(cp.list[[i+1]]) +   (blocks.size[cp.index.list[[i+1]][length(cp.list[[i+1]])] ]) -1;
    len.2 <- ub.2 - lb.2 + 1;
    idx.2 <- floor((min(cp.index.list[[i+2]]) + max(cp.index.list[[i+1]]))/2) ;
    beta.hat <- beta.est[[idx.2]]
    forecast <- sapply(c(lb.2:ub.2), function(jjj) pred(t(data_x), matrix(beta.hat, ncol = p.x), jjj, p.x, p.y)  )
    if(len.2 == 1){
      temp.2 <- sum( ( data_y[lb.2:ub.2,]-forecast)^2 );
    }else{
      temp.2 <- sum( (t(data_y[lb.2:ub.2,])-forecast)^2 );
    }
    sse2 <- temp.1 + temp.2;
    
    
    
    if(sse1 <= sse2){
      sse.full <- 0;
      ii <- 0
      print(cp.list.full[[i+1]] )
      for(num in cp.list.full[[i+1]]  ){
        ii <- ii + 1 
        lb.1 <- min(cp.list[[i+1]]) - (blocks.size[cp.index.list[[i+1]][1] ]);
        ub.1 <- num - 1;
        len.1 <- ub.1 - lb.1 + 1;
        idx.1 <- floor((min(cp.index.list[[i+1]]) + max(cp.index.list[[i]]))/2) ;
        beta.hat <- beta.est[[idx.1]]
        forecast <- sapply(c(lb.1:ub.1), function(jjj) pred(t(data_x), matrix(beta.hat, ncol = p.x), jjj, p.x, p.y)  )
        if(len.1 == 1){
          temp.1 <- sum( (data_y[lb.1:ub.1,]-forecast)^2 );
          
        }else{
          temp.1 <- sum( (t(data_y[lb.1:ub.1,])-forecast)^2 );
        }
        
        
        lb.2 <- num ;
        ub.2 <- max(cp.list[[i+1]]) +   (blocks.size[cp.index.list[[i+1]][length(cp.list[[i+1]])] ]) -1;
        len.2 <- ub.2 - lb.2 + 1;
        idx.2 <- floor((min(cp.index.list[[i+2]]) + max(cp.index.list[[i+1]]))/2) ;
        beta.hat <- beta.est[[idx.2]]
        forecast <- sapply(c(lb.2:ub.2), function(jjj) pred(t(data_x),matrix(beta.hat,ncol = p.x), jjj, p.x, p.y)  )
        if(len.2 == 1){
          temp.2 <- sum( (data_y[lb.2:ub.2,]-forecast)^2 );
        }else{
          temp.2 <- sum( (t(data_y[lb.2:ub.2,])-forecast)^2 );
        }
        sse.full[ii] <- temp.1 + temp.2;
        print(ii)
        print(sse.full[ii])
        if(ii >= min(20, length(cp.list.full[[i+1]])) && sse.full[ii] >=  quantile(sse.full,0.20) ){
          break
        }
      }   
      cp.search[i] <- cp.list.full[[i+1]][min(which(sse.full == min(sse.full)))];
      
    }
    if(sse1 > sse2){
      sse.full <- 0;
      ii <- 0
      print(rev(cp.list.full[[i+1]]) )
      for(num in rev(cp.list.full[[i+1]])  ){
        ii <- ii + 1 
        lb.1 <- min(cp.list[[i+1]]) - (blocks.size[cp.index.list[[i+1]][1] ]);
        ub.1 <- num - 1;
        len.1 <- ub.1 - lb.1 + 1;
        idx.1 <- floor((min(cp.index.list[[i+1]]) + max(cp.index.list[[i]]))/2) ;
        beta.hat <- beta.est[[idx.1]]
        forecast <- sapply(c(lb.1:ub.1), function(jjj) pred(t(data_x), matrix(beta.hat, ncol = p.x), jjj, p.x, p.y)  )
        if(len.1 == 1){
          temp.1 <- sum( (data_y[lb.1:ub.1,]-forecast)^2 );
          
        }else{
          temp.1 <- sum( (t(data_y[lb.1:ub.1,])-forecast)^2 );
        }
        
        
        lb.2 <- num ;
        ub.2 <- max(cp.list[[i+1]]) +   (blocks.size[cp.index.list[[i+1]][length(cp.list[[i+1]])] ]) -1;
        len.2 <- ub.2 - lb.2 + 1;
        idx.2 <- floor((min(cp.index.list[[i+2]]) + max(cp.index.list[[i+1]]))/2) ;
        beta.hat <- beta.est[[idx.2]]
        forecast <- sapply(c(lb.2:ub.2), function(jjj) pred(t(data_x),matrix(beta.hat,ncol = p.x), jjj, p.x, p.y)  )
        if(len.2 == 1){
          temp.2 <- sum( (data_y[lb.2:ub.2,]-forecast)^2 );
        }else{
          temp.2 <- sum( (t(data_y[lb.2:ub.2,])-forecast)^2 );
        }
        sse.full[ii] <- temp.1 + temp.2;
        print(ii)
        print(sse.full[ii])
        if(ii >= min(20, length(cp.list.full[[i+1]])) && sse.full[ii] >=  quantile(sse.full,0.20) ){
          break
        }
      }   
      cp.search[i] <- cp.list.full[[i+1]][length(cp.list.full[[i+1]]) + 1 - min(which(sse.full == min(sse.full)))];
      
    }
    
  }
  
  print("cp.final:")
  print(cp.search)
  print("Second step DONE!!!++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++")
  
  return(list(cp.final = cp.search, beta.hat.list = beta.hat.list))
}


################################################
#' Generating non-stationary ARMA data.
#' 
#' @param nobs number of time points
#' @param arlags the true AR order
#' @param malags the true MA order
#' @param cnst the constant
#' @param phi parameter matrix of the AR model
#' @param theta parameter matrix of the MA model
#' @param sigma covariance matrix of the white noise
#' @return Matrice of time series data and white noise data 

var.sim.break <- function (nobs, arlags = NULL, malags = NULL, cnst = NULL, phi = NULL, theta = NULL, skip = 200, sigma, brk = nobs+1) {
  if (!is.matrix(sigma)) 
    sigma = as.matrix(sigma)
  k <- nrow(sigma); m <- length(brk); nT <- nobs + skip
  
  #generate multivariate normal distributed data as the white noise data
  at <- rmvnorm(nT, rep(0, k), sigma)
  
  #generate the ARMA time series data
  nar <- length(arlags); p <- 0
  if (nar > 0) {
    arlags <- sort(arlags)
    p <- arlags[nar]
  }
  
  nma <- length(malags); q <- 0
  if (nma > 0) {
    malags <- sort(malags)
    q <- malags[nma]
  }
  
  ist = max(p, q) + 1
  zt = matrix(0, nT, k)
  if (length(cnst) == 0) 
    cnst = rep(0, k)
  if (m == 1){
    for (it in ist:nT) {
      tmp = matrix(at[it, ], 1, k)
      if (nma > 0) {
        for (j in 1:nma) {
          jdx = (j - 1) * k
          thej = theta[, (jdx + 1):(jdx + k)]
          atm = matrix(at[it - malags[j], ], 1, k)
          tmp = tmp - atm %*% t(thej)
        }
      }
      if (nar > 0) {
        for (i in 1:nar) {
          idx = (i - 1) * k
          phj = phi[, (idx + 1):(idx + k)]
          ztm = matrix(zt[it - arlags[i], ], 1, k)
          tmp = tmp + ztm %*% t(phj)
        }
      }
      zt[it, ] = cnst + tmp
    }
  }
  
  #if there are some break points
  if (m > 1){
    for (it in ist:(skip+brk[1]-1)) {
      tmp = matrix(at[it, ], 1, k)
      if (nma > 0) {
        for (j in 1:nma) {
          jdx = (j - 1) * k
          thej = theta[, (jdx + 1):(jdx + k)]
          atm = matrix(at[it - malags[j], ], 1, k)
          tmp = tmp - atm %*% t(thej)
        }
      }
      if (nar > 0) {
        for (i in 1:nar) {
          idx = (i - 1) * k
          phj = phi[, (idx + 1):(idx + k)]
          ztm = matrix(zt[it - arlags[i], ], 1, k)
          tmp = tmp + ztm %*% t(phj)
        }
      }
      zt[it, ] = cnst + tmp
    }
    for ( mm in 1:(m-1)){
      for (it in (skip+brk[mm]):(skip+brk[mm+1]-1) ) {
        tmp = matrix(at[it, ], 1, k)
        if (nma > 0) {
          for (j in 1:nma) {
            jdx = (j - 1) * k
            thej = theta[, (jdx + 1):(jdx + k)]
            atm = matrix(at[it - malags[j], ], 1, k)
            tmp = tmp - atm %*% t(thej)
          }
        }
        if (nar > 0) {
          for (i in 1:nar) {
            idx = (i - 1) * k
            phj = phi[, ((mm)*p*k+idx + 1):((mm)*p*k+idx + k)]
            ztm = matrix(zt[it - arlags[i], ], 1, k)
            tmp = tmp + ztm %*% t(phj)
          }
        }
        zt[it, ] = cnst + tmp
      }
    }
  }
  
  zt = zt[(1 + skip):nT, ]
  at = at[(1 + skip):nT, ]
  VARMAsim <- list(series = zt, noises = at)
}


var.first.step.blocks <- function(data_y, lambda1, lambda2, q,  max.iteration, tol,  blocks, cv.index){
  
  n.new <- length(blocks) - 1;
  blocks.size <- sapply(c(1:n.new), function(jjj) blocks[jjj+1] - blocks[jjj]  );
  
  #create the tuning parmaeter combination of lambda1 and lambda2
  lambda.full <- expand.grid(lambda1,lambda2)
  kk <- length(lambda.full[,1]);
  
  cv.l <- length(cv.index); 
  cv <- rep(0,kk); 
  phi.final <- vector("list",kk);
  
  data.y.temp <- data_y;
  T <- length(data.y.temp[,1]); 
  p <- length(data.y.temp[1,]); 
  
  flag.full <- rep(0,kk);
  
  for (i in 1:kk) {
    print(i)
    print("##################lambda1:")
    print(lambda.full[i,1])
    print("##################lambda2:")
    print(lambda.full[i,2])
    if ( i == 1){
      test <- var_break_fit_block(data.y.temp, lambda.full[i,1],lambda.full[i,2], q, max_iteration = max.iteration, tol = tol, initial_phi =  0.0+matrix(0.0,p,p*q*n.new), blocks = blocks, cv.index)
      flag.full[i] <- test$flag;
    }
    if ( i > 1 ){
      initial.phi <- phi.final[[i-1]]
      if(max(abs(phi.final[[(i-1)]])) > 10^3  ){initial.phi <- 0*phi.final[[(i-1)]];}
      test <- var_break_fit_block(data.y.temp, lambda.full[i,1], lambda.full[i,2], q, max_iteration = max.iteration, tol = tol, initial_phi =  initial.phi, blocks = blocks, cv.index)
      flag.full[i] <- test$flag;
    }
    
    phi.hat.full <- test$phi.hat;
    phi.final[[i]] <- phi.hat.full;
    
    
    #forecast the time series based on the estimated matrix Phi (beta for the linear regression model)
    #and compute the forecast error
    phi.full.all <- vector("list",n.new);
    forecast <- matrix(0,p,T);
    phi.hat <- phi.hat.full;
    phi.full.all[[1]] <- phi.hat[,(1):(p*q)];
    for(i.1 in 2:n.new){
      phi.full.all[[i.1]] <- phi.full.all[[i.1-1]] + phi.hat[,((i.1-1)*p*q+1):(i.1*p*q)];
      #forecast[,(blocks[i.1]):(blocks[i.1+1]-1)] <- pred.block.var(t(data_y), phi.full.all[[i.1-1]], q, blocks[i.1], p, blocks[i.1+1]-blocks[i.1]);
    }
    forecast.new <- matrix(0,p,cv.l);
    for(j in (1):cv.l){
      forecast.new[,j] <- pred.var(t(data_y), phi.full.all[[(cv.index[j])]], q, blocks[cv.index[j]+1]-1, p, 1)
    }
    temp.index <- rep(0,cv.l);
    for(ff in 1:cv.l){temp.index[ff] <- blocks[cv.index[ff]+1]-1;}
    cv[i] <- (1/(p*cv.l))*sum( (forecast.new - t(data_y[temp.index,])  )^2 );
    
    
    print("============cv-result=======================")
    print(cv[i])
    print("====================================")
  }
  
  
  
  lll <- min(which(cv==min(cv)));
  ind.new <- 0;
  lll <- lll - ind.new;
  
  mspe.plot(cv, c(1:kk), jj="1")
  abline(v = seq(length(lambda1), length(lambda1)*(length(lambda2)-1), length.out =length(lambda2)-1)+0.5)
  abline(v= lll, col="red")
  
  
  phi.hat.full <- phi.final[[lll]];
  
  #jumps.sq is the L2 norm square
  #jumps.l1 is the L1 norm
  # again, note that here the phi.hat.full is the estimated theta in the paper
  jumps.l2 <- rep(0,n.new);
  for(i in c(2:n.new)){
    jumps.l2[i] <- (sum((phi.hat.full[,((i-1)*p*q+1):(i*p*q)] )^2 ));
  }
  jumps.l1 <- rep(0,n.new);
  for(i in c(2:n.new)){
    jumps.l1[i] <- (sum(abs(phi.hat.full[,((i-1)*p*q+1):(i*p*q)] ) ));
  }
  
  print(jumps.l2)
  #print(jumps.l1)
  plot(jumps.l2,type = 'o', main= 'l2 norm')
  #plot(jumps.l1,type = 'o', main= 'l1 norm')
  
  #ignore the large jump at the boundary!!!!!!!!!!!!!!!!!!!!
  print('use l2 norm!')
  jumps <- jumps.l2
  #print('use l1 norm!')
  #jumps <- jumps.l1
  # ignore_num <- round(100/mean(blocks.size))
  ignore_num <- min(3, round(100/mean(blocks.size)))
  jumps[1:ignore_num]  <- 0
  jumps[(length(jumps)-(ignore_num-1)):length(jumps)]  <- 0
  
  plot(jumps, type = 'o', main= 'l2 norm')
  
  ##################################################################
  #use kmeans to hard threshold the jumps
  if( length(unique(jumps)) > 2 ){
    print("consider 2 clusters for fit.2")
    clus.2 <- kmeans(jumps, centers = 2); fit.2 <- clus.2$betweenss/clus.2$totss; print(fit.2);
    if(fit.2 < 0.20){
      print("no significant jumps!!")
      pts.sel <- c();
    }
    if( fit.2 >= 0.20 ){
      loc <- clus.2$cluster;
      if( clus.2$centers[1] > clus.2$centers[2]  ){pts.sel <- blocks[which(loc==1)];}
      if( clus.2$centers[1] < clus.2$centers[2]  ){pts.sel <- blocks[which(loc==2)];}
      
    }
  }
  if( length(unique(jumps)) <= 2 ){
    pts.sel <- c();
  }
  
  
  if ( length(pts.sel) == 0){cp.final <- c(); pts.list <-  vector("list",0);}
  if ( length(pts.sel) > 0){
    cp.final <- pts.sel;
    # REMOVE BOUNDARY POINTS and other cleaning
    cp.final <- cp.final[which(cp.final > 3*mean(blocks.size))];
    cp.final <- cp.final[which(cp.final < (T-3*mean(blocks.size)))];
    cp.final <- sort(cp.final);
    print(cp.final)
    
    
    # if there are multipler change points
    # use the p-means clustering 
    if(length(cp.final) > 4){
      print(fviz_nbclust(matrix(cp.final,length(cp.final), 1), kmeans, nstart = 25,  method = "gap_stat", k.max = min(10, length(cp.final)-1), nboot = 100)+
              labs(subtitle = "Gap statistic method"))
      cl <- fviz_nbclust(matrix(cp.final,length(cp.final), 1), kmeans, nstart = 25,  method = "gap_stat", k.max = min(10, length(cp.final)-1), nboot = 100)+
        labs(subtitle = "Gap statistic method")
      
      cl.data <- cl$data;
      gap <- cl.data$gap;
      se <- cl.data$SE.sim;
      i.cl <- 0;
      # while (i.cl < (length(gap)-1)) {
      #   i.cl <- i.cl + 1;
      #   if( gap[i.cl] > gap[i.cl+1] - se[i.cl+1] ){cl.number <- i.cl; break;}
      # }
      cl.number <- which.max(gap)
      #cl.number
      
      cl.final <- kmeans(cp.final, centers = cl.number);
      pts.list <-  vector("list",cl.number);
      loc.new <- cl.final$cluster;
      cl.reorder = c(1:cl.number)[order(cl.final$centers)]
      for (i in c(1:cl.number)) {
        pts.i <- cp.final[which(loc.new==cl.reorder[i])]
        print(paste0("Cluster: ", i))
        print(pts.i)
        pts.list[[i]] <- pts.i
      }
    }
    
    #!!!!!!!!!!!!!!!!need  ajustment!!!!!!
    if(length(cp.final) <= 4 & length(cp.final) > 1 ){
      print("small number of cp !!!!")
      cl.number <- length(cp.final);
      loc.new <- rep(1,length(cp.final))
      
      for (i in 2:length(cp.final)){
        if (cp.final[i]-cp.final[i-1]<= max(3*mean(blocks.size))){
          cl.number <-cl.number-1 
          loc.new[i] <-loc.new[i-1]
        }else{
          loc.new[i] <- i
        }
      }
      
      pts.list <-  vector("list",cl.number);
      #for (i in unique(loc.new)) {
      loc.new.unique <- unique(loc.new)
      for (i in 1:length(loc.new.unique)) {
        pts.i <- cp.final[which(loc.new==loc.new.unique[i])]
        print(paste0("Cluster: ", i))
        print(pts.i)
        pts.list[[i]] <- pts.i
      }
    }
    
    
    if(length(cp.final) == 1 ){
      cl.number <- length(cp.final);
      loc.new <- rep(1,length(cp.final))
      pts.list <-  vector("list", cl.number);
      for (i in unique(loc.new)) {
        pts.i <- cp.final[which(loc.new==i)]
        print(paste0("Cluster: ", i))
        print(pts.i)
        pts.list[[i]] <- pts.i
      }
    }
    if(length(cp.final) == 0 ){
      pts.list <-  vector("list", 0);
    }
    
  }
  
  #compute the estimated phi
  phi.par.sum <- vector("list",n.new);
  phi.par.sum[[1]] <- phi.hat.full[,1:(p*q)];
  for(i in 2:n.new){
    phi.par.sum[[i]] <- phi.par.sum[[i-1]] + phi.hat.full[,((i-1)*p*q+1):(i*p*q)];
  }
  
  
  #plot(blocks[1:n.new],jumps,main = "JUMPS.FULL", type = "o")
  
  print("First step DONE!!!!+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++")
  return(list(jumps.l2 = jumps.l2, jumps.l1 = jumps.l1, theta = phi.hat.full, theta.full = phi.final,
              pts.list = pts.list, phi.full = phi.par.sum))
  
}



var.second.step.search <- function(data_y, q, max.iteration = max.iteration, tol = tol,  cp.first, beta.est, blocks){
  
  T <- length(data_y[,1]); p.y <- length(data_y[1,]); 
  
  n.new <- length(blocks) - 1;
  blocks.size <- sapply(c(1:n.new), function(jjj) blocks[jjj+1] - blocks[jjj]  );
  
  
  cp.search <- cp.first;
  cl.number <- length(cp.first)
  
  cp.list <- vector("list",cl.number+2);
  cp.list[[1]] <- c(1);
  cp.list[[cl.number+2]] <- c(T+1);
  
  cp.index.list <- vector("list",cl.number+2);
  cp.index.list[[1]] <- c(1);
  cp.index.list[[cl.number+2]] <- c(n.new+1);
  
  for (i in 1:cl.number) {
    cp.list[[i+1]] <- cp.first[[i]]
    cp.index.list[[i+1]] <- match(cp.first[[i]], blocks)
  }
  
  
  cp.search <- rep(0,cl.number);
  cp.list.full <- cp.list
  
  phi.hat.list <- vector("list", cl.number+1)
  for(i in 1:(cl.number)){
    idx <- floor((min(cp.index.list[[i+1]]) + max(cp.index.list[[i]]))/2);
    phi.hat.list[[i]] <- beta.est[[idx]]
    
    
    
    if(length(cp.list[[i+1]]) >1){
      cp.list.full[[i+1]] <- c((cp.list[[i+1]][1] +1) :  (cp.list[[i+1]][length(cp.list[[i+1]])] -1 ) )
    }
    if(length(cp.list[[i+1]]) == 1){
      cp.list.full[[i+1]] <- c((cp.list[[i+1]][1]-  (blocks.size[cp.index.list[[i+1]][1] ])+1 ) :  (cp.list[[i+1]][length(cp.list[[i+1]])] +   (blocks.size[cp.index.list[[i+1]][length(cp.list[[i+1]])] ]) -1  ) )
    }
    #print(cp.list.full[[i+1]] )
    
    #compare the SSE of first num and last num
    num  = cp.list.full[[i+1]][1]
    lb.1 <- min(cp.list[[i+1]]) - (blocks.size[cp.index.list[[i+1]][1] ]);
    ub.1 <- num - 1;
    len.1 <- ub.1 - lb.1 + 1;
    idx.1 <- floor((min(cp.index.list[[i+1]]) + max(cp.index.list[[i]]))/2) ;
    phi.hat <- beta.est[[idx.1]]
    forecast <- sapply(c(lb.1:ub.1), function(jjj) pred.var(t(data_y), matrix(phi.hat, ncol = p*q), q, jjj, p, 1)  )
    if(len.1 == 1){
      temp.1 <- sum( (data_y[lb.1:ub.1,]-forecast)^2 );
    }else{
      temp.1 <- sum( (t(data_y[lb.1:ub.1,])-forecast)^2 );
    }

  
    lb.2 <- num ;
    ub.2 <- max(cp.list[[i+1]]) +   (blocks.size[cp.index.list[[i+1]][length(cp.list[[i+1]])] ]) -1;
    len.2 <- ub.2 - lb.2 + 1;
    idx.2 <- floor((min(cp.index.list[[i+2]]) + max(cp.index.list[[i+1]]))/2) ;
    phi.hat <- beta.est[[idx.2]]
    forecast <- sapply(c(lb.2:ub.2), function(jjj) pred.var(t(data_y), matrix(phi.hat, ncol = p*q), q, jjj, p, 1)  )
    if(len.2 == 1){
      temp.2 <- sum( ( data_y[lb.2:ub.2,]-forecast)^2 );
    }else{
      temp.2 <- sum( (t(data_y[lb.2:ub.2,])-forecast)^2 );
    }

    sse1 <- temp.1 + temp.2;
  
    
    num  <- cp.list.full[[i+1]][length(cp.list.full[[i+1]])]
    lb.1 <- min(cp.list[[i+1]]) - (blocks.size[cp.index.list[[i+1]][1] ]);
    ub.1 <- num - 1;
    len.1 <- ub.1 - lb.1 + 1;
    idx.1 <- floor((min(cp.index.list[[i+1]]) + max(cp.index.list[[i]]))/2) ;
    phi.hat <- beta.est[[idx.1]]
    forecast <- sapply(c(lb.1:ub.1), function(jjj) pred.var(t(data_y), matrix(phi.hat, ncol = p*q), q, jjj, p, 1)  )
    if(len.1 == 1){
      temp.1 <- sum( (data_y[lb.1:ub.1,]-forecast)^2 );
    }else{
      temp.1 <- sum( (t(data_y[lb.1:ub.1,])-forecast)^2 );
    }
    
    lb.2 <- num ;
    ub.2 <- max(cp.list[[i+1]]) +   (blocks.size[cp.index.list[[i+1]][length(cp.list[[i+1]])] ]) -1;
    len.2 <- ub.2 - lb.2 + 1;
    idx.2 <- floor((min(cp.index.list[[i+2]]) + max(cp.index.list[[i+1]]))/2) ;
    phi.hat <- beta.est[[idx.2]]
    forecast <- sapply(c(lb.2:ub.2), function(jjj) pred.var(t(data_y), matrix(phi.hat, ncol = p*q), q, jjj, p, 1)  )
    if(len.2 == 1){
      temp.2 <- sum( ( data_y[lb.2:ub.2,]-forecast)^2 );
    }else{
      temp.2 <- sum( (t(data_y[lb.2:ub.2,])-forecast)^2 );
    }
    sse2 <- temp.1 + temp.2;
    
    
    if(sse1 <= sse2){
      sse.full <- 0;
      ii <- 0
      print(cp.list.full[[i+1]]) 
      for(num in cp.list.full[[i+1]]  ){
        ii <- ii + 1 
        lb.1 <- min(cp.list[[i+1]]) - (blocks.size[cp.index.list[[i+1]][1] ]);
        ub.1 <- num - 1;
        len.1 <- ub.1 - lb.1 + 1;
        idx.1 <- floor((min(cp.index.list[[i+1]]) + max(cp.index.list[[i]]))/2) ;
        phi.hat <- beta.est[[idx.1]]
        forecast <- sapply(c(lb.1:ub.1), function(jjj) pred.var(t(data_y), matrix(phi.hat, ncol = p*q), q, jjj, p, 1)  )
        if(len.1 == 1){
          temp.1 <- sum( (data_y[lb.1:ub.1,]-forecast)^2 );
        }else{
          temp.1 <- sum( (t(data_y[lb.1:ub.1,])-forecast)^2 );
        }
        
        lb.2 <- num ;
        ub.2 <- max(cp.list[[i+1]]) +   (blocks.size[cp.index.list[[i+1]][length(cp.list[[i+1]])] ]) -1;
        len.2 <- ub.2 - lb.2 + 1;
        idx.2 <- floor((min(cp.index.list[[i+2]]) + max(cp.index.list[[i+1]]))/2) ;
        phi.hat <- beta.est[[idx.2]]
        forecast <- sapply(c(lb.2:ub.2), function(jjj) pred.var(t(data_y), matrix(phi.hat, ncol = p*q), q, jjj, p, 1)  )
        if(len.2 == 1){
          temp.2 <- sum( ( data_y[lb.2:ub.2,]-forecast)^2 );
        }else{
          temp.2 <- sum( (t(data_y[lb.2:ub.2,])-forecast)^2 );
        }
        sse.full[ii] <- temp.1 + temp.2;
        print(ii)
        print(sse.full[ii])
        if(ii >= min(20, length(cp.list.full[[i+1]])) && sse.full[ii] >=  quantile(sse.full, 0.20) ){
          break
        }
      }   
      cp.search[i] <- cp.list.full[[i+1]][min(which(sse.full == min(sse.full)))];
    }
    if(sse1 > sse2){
      sse.full <- 0;
      ii <- 0
      print(rev(cp.list.full[[i+1]]) )
      for(num in rev(cp.list.full[[i+1]])  ){
        ii <- ii + 1 
        lb.1 <- min(cp.list[[i+1]]) - (blocks.size[cp.index.list[[i+1]][1] ]);
        ub.1 <- num - 1;
        len.1 <- ub.1 - lb.1 + 1;
        idx.1 <- floor((min(cp.index.list[[i+1]]) + max(cp.index.list[[i]]))/2) ;
        phi.hat <- beta.est[[idx.1]]
        forecast <- sapply(c(lb.1:ub.1), function(jjj) pred.var(t(data_y), matrix(phi.hat, ncol = p*q), q, jjj, p, 1)  )
        if(len.1 == 1){
          temp.1 <- sum( (data_y[lb.1:ub.1,]-forecast)^2 );
        }else{
          temp.1 <- sum( (t(data_y[lb.1:ub.1,])-forecast)^2 );
        }
        
        lb.2 <- num ;
        ub.2 <- max(cp.list[[i+1]]) +   (blocks.size[cp.index.list[[i+1]][length(cp.list[[i+1]])] ]) -1;
        len.2 <- ub.2 - lb.2 + 1;
        idx.2 <- floor((min(cp.index.list[[i+2]]) + max(cp.index.list[[i+1]]))/2) ;
        phi.hat <- beta.est[[idx.2]]
        forecast <- sapply(c(lb.2:ub.2), function(jjj) pred.var(t(data_y), matrix(phi.hat, ncol = p*q), q, jjj, p, 1)  )
        if(len.2 == 1){
          temp.2 <- sum( ( data_y[lb.2:ub.2,]-forecast)^2 );
        }else{
          temp.2 <- sum( (t(data_y[lb.2:ub.2,])-forecast)^2 );
        }
        
        sse.full[ii] <- temp.1 + temp.2;
        print(ii)
        print(sse.full[ii])
        if(ii >= min(20, length(cp.list.full[[i+1]])) && sse.full[ii] >=  quantile(sse.full, 0.20) ){
          break
        }
      }
      cp.search[i] <- cp.list.full[[i+1]][length(cp.list.full[[i+1]]) + 1 - min(which(sse.full == min(sse.full)))];
    }
    
  }
  
  print("cp.final:")
  print(cp.search)
  print("Second step DONE!!!++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++")
  
  return(list(cp.final = cp.search, phi.hat.list = phi.hat.list))
}


################################################################
######prediction function
################################################################
#' Prediction function 1
pred.block.var <- function(Y,phi,q,T,p,h){
  concat.Y <- matrix(0,p,q+h); concat.Y[,1:q] <- Y[,(T-q):(T-1)];
  for ( j in 1:h){
    temp <- matrix(0,p,1);
    for (i in 1:q){temp <- temp +  phi[,((i-1)*p+1):(i*p)]%*%concat.Y[,q+j-i];}
    concat.Y[,q+j] <- temp; 
  }
  return(as.matrix(concat.Y[,(q+1):(q+h)]))
}

#' Prediction function 2
pred.var <- function(Y,phi,q,T,p,h){
  concat.Y <- matrix(0,p,q+h); concat.Y[,1:q] <- Y[,(T-q):(T-1)];
  for ( j in 1:h){
    temp <- matrix(0,p,1);
    for (i in 1:q){temp <- temp +  phi[,((i-1)*p+1):(i*p)]%*%concat.Y[,q+j-i];}
    concat.Y[,q+j] <- temp; 
  }
  return(as.matrix(concat.Y[,q+h]))
}


pred <- function(X, phi, j, p.x, p.y, h =1){
  concat.X <- matrix(0, p.x, 1);
  concat.Y <- matrix(0, p.y, 1); 
  concat.X[,1] <- as.matrix(X[,j]);
  temp <- matrix(0, p.y, 1);
  temp <- temp +  phi[, 1:p.x]%*%concat.X[, 1];
  concat.Y[, 1] <- temp; 
  return(as.matrix(concat.Y[, 1]))
}


#h: block.size
#j: start point
pred.block <- function(X, phi, j, p.x, p.y, h){
  concat.X <- matrix(0,p.x,h);
  concat.Y <- matrix(0,p.y,h); 
  concat.X[,1:h] <- as.matrix(X[,(j):(j+h-1)]);
  for ( i in 1:h){
    temp <- matrix(0,p.y,1);
    temp <- temp +  phi[,(1):(p.x)]%*%concat.X[,i];
    concat.Y[,i] <- temp; 
  }
  return(as.matrix(concat.Y[,1:h]))
}
