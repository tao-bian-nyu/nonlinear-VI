#-------------------------------------load packages---------------------------------

library(ggplot2)
library(gdata)
library(grid)
library(gridExtra)
library(reshape)
library(Matrix)
library(matrixcalc)
#-------------------------------------simulation parameters---------------------------------
dt <- .00001
t.start <- 0
t.end1 <- 1.8
t.mid <- 2
t.end2 <- 10

N <- floor((t.end1-t.start)/dt)      
N1 <- floor((t.mid-t.end1)/dt)
N2 <- floor((t.end2-t.mid)/dt)

nn.N1 <- 3
nn.N2 <- 16
n <- 2; m <- 1;                                               # dim of x and u
T <- 300                                                     # integration interval <- T*dt
samp <- 200                                                        # plot sampling 

In <- diag(1,n)  
Im <- diag(1,m)
# Inn <- diag(1,nn.N) 

#--------------------------------cost parameters----------------------------------
sf <- 1

#-------------------------------initial values----------------------------
max_Train <- 30

# w <- c(1,0,1)
w <- c(0,0,0)
w_list <- matrix(0,3,max_Train)
c_list <- matrix(0,16,max_Train)

# w <- c(0,0,0,0,0,0,0,0)
wopt <- c(0,0,1,0,1,0,0,0,0,0,0,0,0,0)
# w=wopt

k0 <- 0.0
# k0 <- 0
x01 <- -2.9
x02 <- -2.9                  #initial states

# x01 <- 2
# x02 <- 0.2                   #initial states
