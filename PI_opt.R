 rm(list=ls()) 

source("parameters.R")

#------------------------------LQR (r) parameters------------------------
 

 



K <- cbind( -5.646779e-13, 4.083251e-13, 1.257792e-12)*10^5
    
  
 for (i in 1:(num_opt)){
		 barA <- kronecker(In,t(A-B %*% K)) + kronecker(t(A-B %*% K),In)
		   
         Vector <- solve(barA, matrix(-Q - t(K) %*% R %*% K, nrow=(n*n)))
	  
	        P <- matrix(Vector, nrow=n)
	        K <- solve(R, (t(B) %*% P))
			
		   print("P value")
		   print(P)
		   print("K_value")
		   print(K)

 }
 
 
