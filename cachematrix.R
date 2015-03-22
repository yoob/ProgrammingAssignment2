## Utilities for computing and caching 
## the inverse of a matrix
## These caching tools avoid recomputing a matrix inverse that has 
## been already computed
##
##
## Example :
##
##  > m <- matrix(c(3, 5 , 1, 10), 2, 2)
##  > mCache <- makeCacheMatrix(m)
##  > cacheSolve(mCache) # outputs the computed inverse (1st call)
##  > cacheSolve(mCache) # outputs the inverse from cache (2nd call)
##  


## Builds an object for caching the matrix inverse
## In reality, a list of functions to handle the caching functionality
makeCacheMatrix <- function(x = matrix()) {

  i <- NULL
  
  set <- function(y){
    x <<- y
    i <<- NULL
  }
  
  get <- function(){
    x
  }
  
  setInverse <- function(inverse){
    i <<- inverse
  }
  
  getInverse <- function(x){
    i
  }
  
  list(set = set, get = get, setInverse = setInverse, getInverse = getInverse)

}


## Given a matrix cache, returns the matrix inverse wrapped by 
## the cache object:
## - from cache if the inverse has been already computed
## - by computing the inverse. In this case the cache is updated
cacheSolve <- function(x, ...) {

  i <- x$getInverse()
  
  if(!is.null(i)){
    return(i)
  }
  
  data <- x$get()
  i <- solve(data, ...)
  x$setInverse(i)
  
  i
  
}
