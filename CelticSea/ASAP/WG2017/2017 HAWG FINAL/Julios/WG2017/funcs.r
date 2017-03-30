slm2.sr <- function(x,y){

   #cat("slm2.sr\n")

   #number of data points
   n <- length(x)

   #cat(n,"data points\n")

   #vector of unique x (SSB) points
   xu <- unique(sort(x))

   #number of unique x (SSB) points
   nu <- length(xu)

   #cat(nu,"unique data points\n")

   #if there are less than 5 points, analysis is not possible
   if (! nu >= 4) warning("Segmentation not possible")

   #Fit the segmented model

   #vector of results
   seg.mod <- vector("numeric", 10)

   #loop through dataset, fitting all possible unconstrained two-line models
   for (i in 2:(nu-2)){

      #calculate midpoint between x (SSB) values
      delta <- (xu[i]+xu[i+1])/2

      #call changepoint function, without constraining i.e. do not force the lines to meet at 
      #delta, rather return a new value (mod.delta) describing this point
      mod <- changepoint.sr(x,y,delta,F)

      #if the two fitted lines intersect between xt and xtt (the bounding x coordinated of delta)
      #the set the constrained flag for this model to TRUE

      if (mod$delta >= mod$xt & mod$delta <= mod$xtt) {
         mod$const <- T
      }

      #add values for this model to seg.mod
      seg.mod <- c(seg.mod, as.vector(unlist(mod)[1:9]), i)

   }

   #cast seg.mod to a data frame
   seg.mod <- as.data.frame(matrix(seg.mod, byrow=T, ncol=10)[-1,])

   #and assign the names
   names(seg.mod) <- c("xt","xtt","alpha1","beta1","alpha2","beta2","ssq","delta", "const","index")   

   #check to see if there are 1 or more models where the delta lies between the adjacent x values
   #if so, select that with the minimum ssq and assign into min.const, otherwise use a default vector
   #min.const holds the values for the model with the smallest restricted ssq

   if (length(seg.mod$const[seg.mod$const>0]) > 0){
      min.const <- seg.mod[match(min(seg.mod$ssq[seg.mod$const>0]), seg.mod$ssq),]
   } else {
      min.const <- as.data.frame(matrix(c(rep(NA,6), 1e+20, NA, 1, 0),ncol=10))
      names(min.const) <- c("xt","xtt","alpha1","beta1","alpha2","beta2","ssq","delta", "const", "index")   
   }

   #segmented models where the two lines meet outside of the xt-xtt interval
   if (length(seg.mod$const[seg.mod$const == 0]) > 0){
   
      while (length(seg.mod$const[seg.mod$const==0]) > 0){
	
	 #select model with the minimum ssq and assign to min.unconst
	 #this is the smalled unconstrained ssq
	 min.unconst <- seg.mod[match(min(seg.mod$ssq[seg.mod$const==0]), seg.mod$ssq),]

	 #update this model to indicate that it is constrained
	 seg.mod <- seg.mod[- match(min.unconst$index, seg.mod$index),]
	 seg.mod$const[match(min.unconst$index, seg.mod$index)] <- 1
	 
	 #constrain the unrestricted model with the smallest ssq to meet
	 #first at xt and then at xtt. This gives two new models - m1 and m2
	 m1 <- changepoint.sr(x=x, y=y, delta=min.unconst$xt, constrained=T)
	 m2 <- changepoint.sr(x=x, y=y, delta=min.unconst$xtt, constrained=T)
	
	 #Select the model which has the smallest restricted ssq from m1 and m2 
	 #and add it to the restricted list

	 if (m1$ssq<=m2$ssq){
	    seg.mod <- rbind(seg.mod, c(unlist(m1)[1:9], 0))
	 } else {  
	    seg.mod <- rbind(seg.mod, c(unlist(m2)[1:9], 0))
	 } 
      
	 #update minimum constrained model to be that model with the smallest ssq
	 min.const <- seg.mod[match(min(seg.mod$ssq[seg.mod$const>0]), seg.mod$ssq),]
     
      }
   
   } else {
   
      #default value
      min.unconst <- as.data.frame(matrix(c(rep(NA,6), 1e+20, NA, 1, 0),ncol=10))
      names(min.unconst) <- c("xt","xtt","alpha1","beta1","alpha2","beta2","ssq","delta", "const", "index")   
   
   }

#take the smallest restricted ssq as the best model and its parameters as the 
#least squares estimates for the slopes and changepoint
min.const

}



changepoint.sr <- function(x, y, delta, constrained=T){

   #calculate change point details for a given x and y series,
   #a delta (the dividing point on the x scale)
   #if constrained the two fitted lines are forced to meet at point delta
   #if not the meeting point is calculated and returned for comparison with
   #xt and xtt (thw two bounding x coords of the supplied delta)

   #cat("changepoint.sr\n")

   #length of x vector (SSB)
   n<-length(x)
   #vector of values less than or equal to the supplied delta
   half<-(x<= delta)
   #length of this vector
   t<-length(x[half])

   if(constrained){
      
      xmat <- cbind(rep(1,n))
      ymat <- c(log(y[half]), log(y[!half]))
      m <- lm(ymat ~ xmat - 1, offset=c(log(x[half]), rep(log(delta), n-t)) )
      beta <- as.vector(coef(m))
      beta <- exp(beta) 
      beta <- c(0, beta[1], beta[1]*delta, 0)
      ypred <- predict(m)
      res <- ymat - ypred #log scale
      ssq <- sum(residuals(m)^2) #log scale

   } else {

      xmat <- cbind(c(rep(1,t), rep(0,n-t)), c(rep(0,t), rep(1,n-t)))
      ymat <- c(log(y[half]), log(y[!half]))
      off<-c(log(x[half]),rep(0,n-t))
      m <- lm(ymat ~ xmat -1, offset=c(log(x[half]), rep(0, n-t)))
      beta <- as.vector(coef(m))
      beta <- exp(beta) 
      beta <- c(0, beta[1], beta[2], 0)
      ypred <- predict(m)
      res <- ymat - ypred  
      ssq <- sum(residuals(m)^2)
      delta <- beta[3] / beta[2] # calcula el punto de corte!!  
   }

   #cat("xt,xtt,ssq,delta",max(x[half]),min(x[!half]),ssq,delta,"\n")

   #output vector
   out <- list(xt=max(x[half]), 
               xtt=min(x[!half]), 
	       alpha1=beta[1], 
	       beta1=beta[2], 
	       alpha2=beta[3], 
	       beta2=beta[4], 
	       ssq=ssq, 
	       delta=delta, 
	       const=constrained, 
	       pred=ypred, 
	       res=res)

   #return this
   out
}


slm.sr <- function(x, y, grid=100){
  #for a unique cutpoint delta!! and a unique independent variable (y ~ x)
  #weights=1
  #grid >1
  
  call <- match.call()
  if (length(x) != length(y)) error ("x and y must have the same length")
  n <- length(x)
  delta.grid <- seq(min(x), max(x), length=grid)
  out <- vector("numeric", 9)
  for (i in 2:(grid-1)){
    delta <- delta.grid[i]
    m <- changepoint.sr(x=x, y=y, delta=delta, constrained=T)
    out <- c(out, as.vector(unlist(m)[1:9]))
 }
 
 out <- as.data.frame(matrix(out, byrow=T, ncol=9)[-1,])
 names(out) <- c("xt","xtt","alpha1","beta1","alpha2","beta2","ssq","delta", "const")
#, paste("pred", 1:n), paste("res", 1:n))   
 plot(out$delta, out$ssq, type="l")
 mi <- min(out$ssq) 
 out <- out[match(mi, out$ssq),] 
 out 
}


boot.sr <- function(x, y, iter, alpha=0.05){
  n <- length(x)

  #1 line model
  m1 <- lm(log(y)~1)
  m1.ssq <- sum(residuals(m1)^2)
  pred <- predict(m1) # en escala log

  #2 line model
  m2 <- slm2.sr(x,y)
  m2 <- changepoint.sr(x,y,delta=m2$delta, constrained=T)
  #residuals for best fitting 2-line mode
  res <- as.vector(unlist(m2)[(10+n):(9+2*n)]) # en escala log

  beta.obs <- c(m2$alpha1, m2$beta1, m2$alpha2, m2$beta2, m2$delta)
  
  #calc f-statistic
  f.obs <- (m1.ssq - m2$ssq)*(n-2)/m2$ssq
  f.all <- f.obs

  beta <- c(m2$alpha1, m2$beta1, m2$alpha2, m2$beta2, m2$delta)
  
  #bootstrap cycle
  for (i in 1:iter){
    #print(i)
    #using the best fitting 1-line model, add and error term, sampled with replacemene
    #from the set of residuals for the 2-line model
    yboot <- pred + sample(res)
    yboot <- as.vector(exp(yboot))
    m1boot <- lm(log(yboot)~1)
    m1boot.ssq <- sum(residuals(m1boot)^2)
    #use original x values and new y values
    m2boot <- slm2.sr(x,yboot)
    #add new f statistic to the array
    f.all <- c(f.all, (m1boot.ssq - m2boot$ssq)*(n-2)/m2boot$ssq)
    #add the new line parameters to the array beta
    beta <- c(beta, m2boot$alpha1, m2boot$beta1, m2boot$alpha2, m2boot$beta2, m2boot$delta)    
  }
  #remove f-statistics of less than 0
  f.all[f.all<0] <- 0
  #calculate p number
  p <- (length(f.all[f.all > f.all[1]]) / (iter+1))

  #returned list
  out <- list(beta=matrix(beta, byrow=T, ncol=5), beta.obs=beta.obs, f.obs=f.obs, p.value=p, f.all=f.all)
  out
}




