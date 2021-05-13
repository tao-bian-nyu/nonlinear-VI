funPsi = function (x,y,u){
  
  output <- cbind(x,y,x^2,x*y,y^2,
    x^3,x^2*y,x*y^2,y^3,
    x^4,x^3*y,x^2*y^2,x*y^3,y^4,
    x*u^3,y*u^3)

#   output <- cbind(x^2,x*y,y^2,
#     x^4,
#     x^4*y^4,
#     x*y*u^3)
  
}
  