myRLS=function(d, epsilon, phi){
#solve w from phi*w=d, using recursive least square (RLS) method 

ss = dim(phi) 
 K = ss[2]
 N = ss[1] 
 

w = matrix(0,K,1) 

P = 1/epsilon*diag(1,K) 
 
 for (n in 1:N){
 	ss = 1/(1+t(phi[n,]) %*% P %*% phi[n,]) 
 	M = P %*% phi[n,] %*% t(phi[n,]) %*% P
 	P = P - M * ss[1]
    g = P %*% phi[n,]
    alpha = d[n] - t(w) %*% phi[n,]
    w = w + g %*% alpha
 }
 w

}








