 rm(list=ls(all=TRUE)) 

source("parameters.R")

# Popt <- rbind(c(814.84403,      474.95864,        24.44994),
# 		      c(474.95864,      448.98124,        58.07778),
# 		      c(24.44994,       58.07778,         42.93808))


num_opt  <-  30000                                             # num of optimal updating loops

# alpha <- 1

#------------------------------LQR (r) parameters------------------------
#    dt <- 1/2^15
#     T <- .3
    
    noise <- rnorm(num_opt, mean = 0, sd = 1) * .0                   # Adding simulation noise
    

      P0 <- diag(runif(n, min=.1, max=1))
#       P0 <- matrix(0,n,n)
#       P0 <- -In*1
       P <- P0
#       P <- diag(c(300,200,20))  
     
     
# 	  P <- rbind(c(700,      100,        2),
# 			     c(100,      600,        3),
# 				 c(  2,        3,        6))
# 
 
 C <- 5
 
 bound <- 0
 for (i in 1:(num_opt)){
   	     	
#    Pold <- P
   	  P <- P + 1/(i+0) * (t(A) %*% P + P %*% A + Q - P%*%B%*%solve(R)%*%t(B)%*%P +
                         noise[i]*In )  
       
#        if (norm(P)>bound){
#          bound <- i
# #          bound <- bound + 1/i
#          P <- diag(runif(n, min=0.01, max=1))
# #          P <- diag(c(1,1,1,1,1,1))
# #          Pold -> P
#        }
# 
# if (min(eigen(P)$value)<0){
#   P <- diag(runif(n, min=.01, max=1))
# }       
       
if ((norm(P)>bound)|(min(eigen(P)$value)<0)){
               bound <- bound + 10
                   P <- diag(runif(n, min=.01, max=1))
            }

#    	  P <- P + 1/i * (t(A-B%*%K0) %*% P + P %*% (A-B%*%K0) + Q + t(K0)%*%(R)%*%K0+noise[i]*In)

		   print(c("round",i))
		   print("P value")
		   print(P)
# 		   print("P-P_opt norm")
# 		   print(norm(P-Popt))
# 		   print("P eigenvalues")
# 		   print(eigen(P)$value)
# 		   print("P-P_opt eigenvalues")
# 		   print(eigen(P-Popt)$value)

 }
