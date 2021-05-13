rm()

source("parameters.R")

#------------------------------LQR (r) parameters------------------------
 

 




K <- K0
    
  
 for (i in 1:(num_opt)){
		 barA <- kronecker(In,t(A-B %*% K)) +
		         kronecker(t(A-B %*% K),In)
		   
         Vector <- solve(barA, matrix(-Q - t(K) %*% R %*% K, nrow=(n*n)))
	  
	        P <- matrix(Vector, nrow=n)
	        K <- solve(R, (t(B) %*% P))
		    
			
		   print("P value")
		   print(P)
		   print("K_value")
		   print(K)

 }
 
 
