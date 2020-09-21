## library(MASS) is used to calculate inverse of all squared and non squared matrix

library(MASS)
makeCacheMatrix <- function(x = matrix()) {
  inv<-NULL ##initializing inverse as null
  set<-function(y){
    x<<-y
    inv<<-NULL
  }
  get<-function()x ##function to get matrix
  setInverse<-function(inverse){inv<<-inverse}
  getInverse<-function() {inv}
list(set=set, get=get, setInverse=setInverse, getInverse=getInverse)
}


## Write a short comment describing this function
##this is used to get the cached data

cacheSolve <- function(x, ...) ##gets cached data
  {
  inv<-x$getInverse()
  if(!is.null(inv)) ##checking if inv is null
    {
    message("getting cached data")
    return(inv) ##returns inverse matrix
  }
  mat<-x$get()
  inv<-solve(mat,...) ##calculates inverse
  x$setInverse(inv)
  inv ##returns the inverse matrix of x
}
