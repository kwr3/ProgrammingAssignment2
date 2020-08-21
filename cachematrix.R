## These functions handle caching
## makeCacheMatrix - gets, sets, setsinverse, and getsinverse
## cacheSolve calculates the inverse from the cache and then stores it

## This function uses lexical scoping to create a matrix cache

makeCacheMatrix <- function(x = matrix()) {
  
  invrs <- NULL
  
  set <- function(y){
    x <<- y
    invrs <<- NULL
  }
  
  get <- function() x
  setinverse <- function(solve) invrs <<- solve
  
  getinverse <- function() invrs
  list(set = set, get = get, setinverse = setinverse, getinverse = getinverse)
}


## This function calculates the inverse if one doesn't already exist in the
## cache

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  invrs <- x$getinverse()
  if(!is.null(invrs))
  {
    message("getting cached data")
    return(invrs)
  }
  
  mat <-  x$get()
  invrs <- solve(mat, ...)
  x$setinverse(invrs)
  invrs
}
