rm(list=ls()) 
source("parameters.R")
source('funPhi.R')
source('funPsi.R')
source('funQ.R')




# Theta2 <- solve(Theta0)

#------------------------------- calculate w_k --------------------------------
max_Train <- 10

w_list <- matrix(0,3,max_Train)
c_list <- matrix(0,5,max_Train)

TimeTrain  <-  1
ctrl <- c(0, -0.2)
dw <- c(1,2,2)



start_time <- Sys.time() # get start time

while((TimeTrain < max_Train) ){
# while((TimeTrain <= max_Train) & norm(t(dw),'i') > 0.01){
  
#------------------------------- system initialization ---------------------
  x <- matrix(0,n,N)
  uout <- matrix(0,m,N)
  X <- matrix(0,(nn.N2),N)
  Qx <- matrix(0,1,N)
  
  x1 <- x01
  x2 <- x02
  x[,1] <- c(x1,x2)
  t <- 0 
  
#------------------------------- system before learning -----------------------
  for (i in 1:(N-1)){
    
    #   u <- 0
    #   for (noisei in 1:100){
    #     u <- u + .0001*sin(noisei/5*t)
    #   }
    
    #   u <- 1.6*sin(5*t)
    
    alpha <- cbind(x1,x2)%*%ctrl
    u <- -2*x2+1*sin(5*t)
    
    dTheta <- funPsi(x1,x2,u)*dt - funPsi(x1,x2,alpha)*dt
    dx1 <- (-.5*x1^3-x1-2*x2)*dt  
    dx2 <- (1/8*x2^3-x2+1/2*u^3)*dt
    
    X[,i+1] <- X[,i] + dTheta
    Qx[,i+1] <-Qx[,i] + funQ(x1,x2,alpha)*dt
    x1 <- x1 + dx1
    x2 <- x2 + dx2
    x[,i+1] <- c(x1,x2)
    uout[,i+1] <- u
    t <- t+dt
  }     
  

  
#-------------------------------generate online data --------------------------------
  
  Theta1 <- matrix(0,5,5)
  Xi1 <- matrix(0,5,1)
  # temp <- matrix(0,floor(N/T),nn.N1)
  
  for (i in 1:floor(N/T)){
    Phitemp <- funPhi(x[1,i*T],x[2,i*T])-funPhi(x[1,1+(i-1)*T],x[2,1+(i-1)*T])
    Qxtemp <- Qx[,i*T] - Qx[,1+(i-1)*T]
    # temp[i,] <- Phitemp
    Thetatemp <- X[,i*T] - X[,1+(i-1)*T]
    
    temp <- cbind(Phitemp, t(Thetatemp[(nn.N2-1):nn.N2]))
    
    Theta1 <- Theta1 + t(temp) %*% (temp)
    Xi1 <- Xi1 + t(temp) %*% Qxtemp
    
    # Phitemp2 <- funPhi(x[1,1+(i-1)*T],x[2,1+(i-1)*T])
    #   Qx <- funQ(x[1,1+(i-1)*T],x[2,1+(i-1)*T])
    #   Psitemp <- funPsi(x[1,1+(i-1)*T],x[2,1+(i-1)*T])
    
    # Theta0 <- Theta0 + t(Phitemp2) %*% Phitemp2
    #   right1 <- right1 + t(Phitemp2) %*% Qx
    #   right2 <- right2 + kronecker(t(Phitemp2), t(Psitemp) %*% R %*% Psitemp)
  }
  
  lc <- -solve(Theta1, Xi1)
  # print(lc)
  
  
  w <- lc[1:(nn.N1)]
  c <- lc[(nn.N1+1):5]
  ctrl <- lc[4:5]
#   c <- lc[nn.N2]
  
  # Xi2 <- matrix(0,nn.N1,1)
#  temp <- matrix(0,floor(N/T),1)
#   dw <- Theta2 %*% right1-kronecker(Inn,t(c)) %*% right2 %*% c)
#   dw <- l + Theta2 %*% (right1-kronecker(Inn,t(c)) %*% right2 %*% c)
  
#   for (i in 1:floor(N/T)){
#     xtem <- x[1,1+(i-1)*T]
#     ytem <- x[2,1+(i-1)*T]
#     alpha <- -cbind(xtem,ytem)%*%c
# #     alpha <- -xtem*ytem*c/4
#     
#     Phitemp <- funPhi(xtem,ytem)
#     Psitemp <- funPsi(xtem,ytem,alpha)
#     Qx <- funQ(xtem,ytem,alpha)
#     
#     Xi2 <- Xi2 + t(Phitemp) %*% (Psitemp %*% lc+Qx)
# #     temp[i,] <- (Psitemp %*% lc+Qx)
#   }
  
  # dw <- Theta2 %*% Xi2
  # w <- w + dw/(TimeTrain+10)
  
  w_list[,TimeTrain+1] <- w
  c_list[,TimeTrain+1] <- lc
  
  dw <- w-w_list[,TimeTrain]

#   w <- w + dw/2000
  
  TimeTrain <- TimeTrain +1
  print(TimeTrain)
  
}#end repeat

total_time <- Sys.time() - start_time # calculate difference
print(total_time) # print in nice format


print("w final value")
print(w)
      

#--------------------------------------plot all the trajectory----------------------------

source('PI_my_plot.R')
# 
#  
save.image(file="PI_simulationData.RData")
