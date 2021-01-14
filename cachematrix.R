#The idea of the assignment is to create 2 functions
# that can work together with the objective to cache the inverse of a matrix
# so that computational resources are not wasted.


#The makeCacheMatrix function sets and gets the data of the matrix and it's inverse value
#sets and gets it's inverse
makeCacheMatrix=function(x=matrix()){
  invert=NULL
  set=function(y){
    x<<-y
    invert<<-NULL
  }
  get=function() x
  set_inv=function(inv) invert<<-inv
  get_inv=function() invert
  list(set=set,get=get,set_inv=set_inv,get_inv=get_inv)
  
}

#The cacheSolve has the ability of first checking if the inverse is previously defined, if
# it is, then it doesn't repeat the process of calculating it, but if isn't null then it
#calculates it's value

cacheSolve=function(x,...){
  invert=x$get_inv()
  if (!is.null(invert)){
    message("getting cached data")
    return(invert)
  }
  data=x$get()
  invert<-solve(data,...)
  x$set_inv(invert)
  invert
  
}