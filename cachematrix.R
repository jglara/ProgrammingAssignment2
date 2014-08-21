## This pair of functions calculate the inverse of a matrix, but use a cached value to
## avoid recalculting when not needed
## 

## This function receives a matrix as an argument and returns a list of functions:
##  getMatrix: get the matrix
##  getInverse: get the inverse of the matrix (or null if not setted yet)
##  setInverse: set the inverse of the matrix

makeCacheMatrix <- function(x = matrix()) {
  inverse <- NULL
  getMatrix <- function() x
  getInverse <- function() inverse
  setInverse <- function(inverseArg) {
    inverse <<- inverseArg
  }
  
  list(getMatrix = getMatrix, getInverse = getInverse, setInverse =setInverse)
  
}


## This function takes the list returned by makeCacheMatrix to calculate and cache the inverse
## of the underlying matrix
cacheSolve <- function(cacheMatrix, ...) {
    ## Return a matrix that is the inverse of 'x'
  
    inv <- cacheMatrix$getInverse()
    if (!is.null(inv)) {
      message("getting cached data")
      return(inv)
    } 
    
    m <- cacheMatrix$getMatrix()
    m_inv <- solve(m)
    cacheMatrix$setInverse(m_inv)
    m_inv
}
