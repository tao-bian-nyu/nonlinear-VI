rm(list=ls()) 
source("parameters.R")
source('funPhi.R')
source('funPsi.R')
source('funQ.R')

#------------------------------- system initialization ---------------------
start_time <- Sys.time() # get start time

x <- matrix(0,n,N)
uout <- matrix(0,m,N)
X <- matrix(0,(nn.N2),N)

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
  u <- -k0*x2+1*sin(5*t)
  
  dTheta <- funPsi(x1,x2,u)*dt
  dx1 <- (-.5*x1^3-x1-2*x2)*dt  
  dx2 <- (1/8*x2^3-x2+1/2*u^3)*dt
  
  X[,i+1] <- X[,i] + dTheta
  x1 <- x1 + dx1
  x2 <- x2 + dx2
  x[,i+1] <- c(x1,x2)
  uout[,i+1] <- u
  t <- t+dt
}     

#-------------------------------generate online data --------------------------------

Theta1 <- matrix(0,nn.N2,nn.N2)
Theta0 <- matrix(0,nn.N1,nn.N1)
Xi1 <- matrix(0,nn.N2,nn.N1)
temp <- matrix(0,floor(N/T),nn.N1)


for (i in 1:floor(N/T)){
  Phitemp <- funPhi(x[1,i*T],x[2,i*T])-funPhi(x[1,1+(i-1)*T],x[2,1+(i-1)*T])
  temp[i,] <- Phitemp
  Thetatemp <- X[,i*T] - X[,1+(i-1)*T]
  
  Theta1 <- Theta1 + Thetatemp %*% t(Thetatemp)
  Xi1 <- Xi1 + Thetatemp %*% Phitemp
  
  Phitemp2 <- funPhi(x[1,1+(i-1)*T],x[2,1+(i-1)*T])
#   Qx <- funQ(x[1,1+(i-1)*T],x[2,1+(i-1)*T])
#   Psitemp <- funPsi(x[1,1+(i-1)*T],x[2,1+(i-1)*T])
  
  Theta0 <- Theta0 + t(Phitemp2) %*% Phitemp2
#   right1 <- right1 + t(Phitemp2) %*% Qx
#   right2 <- right2 + kronecker(t(Phitemp2), t(Psitemp) %*% R %*% Psitemp)
}

Theta <- solve(Theta1, Xi1)
Theta2 <- solve(Theta0)

#------------------------------- calculate w_k --------------------------------

TimeTrain  <-  1

# while((TimeTrain*h < sf)){
dw <- In


while((TimeTrain < max_Train) ){
  
# while((TimeTrain < 1e5) & norm(dw,'i') > 0.01){
  
  Xi2 <- matrix(0,nn.N1,1)
  
  
  lc <- Theta %*% w
  c <- lc[(nn.N2-1):(nn.N2)]
#   c <- lc[nn.N2]
  
  
#  temp <- matrix(0,floor(N/T),1)
#   dw <- Theta2 %*% right1-kronecker(Inn,t(c)) %*% right2 %*% c)
#   dw <- l + Theta2 %*% (right1-kronecker(Inn,t(c)) %*% right2 %*% c)
  
  for (i in 1:floor(N/T)){
    xtem <- x[1,1+(i-1)*T]
    ytem <- x[2,1+(i-1)*T]
    alpha <- -cbind(xtem,ytem)%*%c
#     alpha <- -xtem*ytem*c/4
    
    Phitemp <- funPhi(xtem,ytem)
    Psitemp <- funPsi(xtem,ytem,alpha)
    Qx <- funQ(xtem,ytem,alpha)
    
    Xi2 <- Xi2 + t(Phitemp) %*% (Psitemp %*% lc+Qx)
#     temp[i,] <- (Psitemp %*% lc+Qx)
  }
  
  dw <- Theta2 %*% Xi2
  
  
#   print(w)
#   print(lc)
  
#   dw <- qr.solve(Theta2, Xi2)
  
  # print(c)
  
  w <- w + dw/(TimeTrain+10)
  w_list[,TimeTrain+1] <- w
  c_list[,TimeTrain+1] <- lc
  
  # print(w)
  
#   w <- w + dw/2000
  
  TimeTrain <- TimeTrain +1
  print(TimeTrain)
  
  total_time <- Sys.time() - start_time # calculate difference
}#end repeat

print(total_time) # print in nice format


print("w final value")
print(w)


#------------------------------- system after learning calculating ---------------------

xtraj2 <- matrix(0,n,N1)
uouttraj2 <- matrix(0,m,N1)
uouttraj2[,1] <- u
xtraj2[,1] <- c(x1,x2)

t <- t.end1

for (i in 1:(N1-1)){
  
  u <- -k0*x2+sin(5*t)
  
  dx1 <- (-.5*x1^3-x1-2*x2)*dt  
  dx2 <- (1/8*x2^3-x2+1/2*u^3)*dt
  
  x1 <- x1 + dx1
  x2 <- x2 + dx2
  xtraj2[,i+1] <- c(x1,x2)
  uouttraj2[,i+1] <- u
  t <- t+dt
}

x <- cbind(x,xtraj2)
uout <- cbind(uout,uouttraj2)
 
#------------------------------- system after learning new u---------------------

xtraj2 <- matrix(0,n,N2)
uouttraj2 <- matrix(0,m,N2)

x1 <- x01
x2 <- x02

xtraj2[,1] <- c(x1,x2)
uouttraj2[,1] <- u

t <- t.mid

for (i in 1:(N2-1)){
  
#   u <- funPsi(x1,x2) %*% c
  u <- -cbind(x1,x2)%*%c
  
  dx1 <- (-.5*x1^3-x1-2*x2)*dt  
  dx2 <- (1/8*x2^3-x2+1/2*u^3)*dt
  
  x1 <- x1 + dx1
  x2 <- x2 + dx2
  xtraj2[,i+1] <- c(x1,x2)
  uouttraj2[,i+1] <- u
  t <- t+dt
}

xall1 <- cbind(x,xtraj2)
uall1 <- cbind(uout,uouttraj2)

#------------------------------- system after learning no u---------------------

xtraj3 <- matrix(0,n,N2)
uouttraj3 <- matrix(0,m,N2)

x1 <- x01
x2 <- x02

xtraj3[,1] <- c(x1,x2)

t <- t.mid

for (i in 1:(N2-1)){
  
  u <- -k0*x2+1*sin(5*(t-t.mid))
  
  dx1 <- (-.5*x1^3-x1-2*x2)*dt  
  dx2 <- (1/8*x2^3-x2+1/2*u^3)*dt
  
  x1 <- x1 + dx1
  x2 <- x2 + dx2
  xtraj3[,i+1] <- c(x1,x2)
  t <- t+dt
}

xall2 <- cbind(x,xtraj3)

#-------------------------------generate data frames--------------------------------

time <- seq(t.start,t.end2,length=N+N1+N2)
time_samp <- time[seq(1, length(time), samp)]

x_p <- xall1[1,]
x_p_samp <- x_p[seq(1, length(x_p), samp)]
p_samp1 <- data.frame(time_samp, x_p_samp)

x_p <- xall2[1,]
x_p_samp <- x_p[seq(1, length(x_p), samp)]
p_samp2 <- data.frame(time_samp, x_p_samp)
      
y_v <- xall1[2,]
y_v_samp <- y_v[seq(1, length(y_v), samp)]
v_samp1 <- data.frame(time_samp, y_v_samp)

y_v <- xall2[2,]
y_v_samp <- y_v[seq(1, length(y_v), samp)]
v_samp2 <- data.frame(time_samp, y_v_samp)

uinput <- uall1[1,]
uinput_samp <- uinput[seq(1, length(uinput), samp)]
input_samp <- data.frame(time_samp, uinput_samp)
      

#--------------------------------------plot all the trajectory----------------------------

source('my_plot.R')
# 
#  
save.image(file="simulationData.RData")
