## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function

makeCacheMatrix <- function(x = matrix()) {
  mInv <- NULL
  
  set <- function(y)
  {
    x <<- y
    mInv <<- NULL
  }
  
  get <- function() x
  
  setInverse <- function( inv) mInv <<- inv
  getInverse <- function() mInv
  
  list(set = set, get = get, setInverse = setInverse,  getInverse = getInverse)
  
}


## Write a short comment describing this function

cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x'
  
  m <- x$getInverse()
  if( !is.null((m)))
  {
    message("gettin cached data")
    return(m)
  }
  data <- x$get()
  m <- solve(data)
  x$setInverse(m)
  m
}





