rm(list=ls()) 
source("parameters.R")
source('funPhi.R')
source('funPsi.R')
source('funQ.R')

#------------------------------- system initialization ---------------------
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
  
  if (abs(x1)>10^8 || abs(x2)>10^8){
    x1 <- x01
    x2 = x02
    
  }
  t <- t+dt
}     

#-------------------------------generate online data --------------------------------

Theta1 <- matrix(0,nn.N2,nn.N2)
Theta0 <- matrix(0,nn.N1,nn.N1)

Theta1vec <- matrix(0,nn.N2,floor(N/T))
Theta0vec <- matrix(0,nn.N1,floor(N/T))


Xi1 <- matrix(0,nn.N2,nn.N1)
temp <- matrix(0,floor(N/T),nn.N1)

for (i in 1:floor(N/T)){
  Phitemp <- funPhi(x[1,i*T],x[2,i*T])-funPhi(x[1,1+(i-1)*T],x[2,1+(i-1)*T])
  temp[i,] <- Phitemp
  Thetatemp <- X[,i*T] - X[,1+(i-1)*T]
  
  Theta1 <- Theta1 + t(t(Thetatemp)) %*% t(Thetatemp)
  Theta1vec[,i] <- eigen(Theta1)$values/i*(eigen(Theta1)$values/i>0)
  Xi1 <- Xi1 + Thetatemp %*% Phitemp
  
  Phitemp2 <- funPhi(x[1,1+(i-1)*T],x[2,1+(i-1)*T])
#   Qx <- funQ(x[1,1+(i-1)*T],x[2,1+(i-1)*T])
#   Psitemp <- funPsi(x[1,1+(i-1)*T],x[2,1+(i-1)*T])
  
  Theta0 <- Theta0 + t(Phitemp2) %*% Phitemp2
  Theta0vec[,i] <- eigen(Theta0)$values/i
#   right1 <- right1 + t(Phitemp2) %*% Qx
#   right2 <- right2 + kronecker(t(Phitemp2), t(Psitemp) %*% R %*% Psitemp)
}

Theta <- solve(Theta1, Xi1)
Theta2 <- solve(Theta0)

#------------------------------- calculate w_k --------------------------------

      

#--------------------------------------plot all the trajectory----------------------------

# Theta0plot = data.frame(M=1:floor(N/T),t(abs(Theta0vec)))
# Theta0plot = melt(Theta0plot, id.var = "M")
# names(Theta0plot) = c("M", "eigenvalues","value")
# plot_Theta0 = ggplot(data=Theta0plot, aes(x=M, y=value, colour=eigenvalues)) + geom_line() + ylim(0, 100)

postscript("foo.eps", horizontal = FALSE, onefile = FALSE, paper = "special")
plot(apply(t((Theta0vec)),1,min),type='l',xlab="M", ylab="min_eigen")
dev.off

plot(apply(t(abs(Theta1vec)),1,min),type='l',xlab="M", ylab="min_eigen")


# Theta1plot = data.frame(M=1:floor(N/T),t(abs(Theta1vec)))
# Theta1plot = melt(Theta1plot, id.var = "M")
# names(Theta1plot) = c("M", "eigenvalues","value")
# plot_Theta1 = ggplot(data=Theta1plot, aes(x=M, y=value, colour=eigenvalues)) + geom_line()

# plot_input <- plot_input + 
#   geom_line(data=input_samp, aes(x=time, y=Input, colour='Control input', linetype='Control input'), size=.5) + 
#   scale_linetype_manual("", values=1) + scale_color_manual("", values='red') +
#   ylab("Input") + xlab(expression(t)) + theme_bw()

# postscript("sim_Plot.eps", paper="special",height=6, width=8, horizontal=FALSE )    
# grid.arrange(plot_angle, 
#   plot_velocity, 
#   plot_input,
#   nrow = 3)
# dev.off()
# 
#  

