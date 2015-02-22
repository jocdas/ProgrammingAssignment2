## The inverseMatrix function calculates the inverse of a matrix.
## While doing this, it uses an cache object to prevent repeated 
## recalculation of the same values.
##
## Example usage:
## source("cachematrix.R")
## iMatrix <- matrix(c(1,2,3,4,9,9,9,9,9),3,3)
## inverseMatrix(iMatrix)


## This function creates an object of the matrix class, with methods
## get, set, setsolve, getsolve.

makeCacheMatrix <- function(x = matrix()) {
  m <- NULL
  
  #methods for this object
  set <- function(y) {
    x <<- y
    m <<- NULL
  }
  get <- function() x
  setsolve <- function(solve) {
    m <<- solve
  }
  getsolve <- function() m
  
  #list the methods
  list(set = set, get = get,
       setsolve = setsolve,
       getsolve = getsolve)
}


## The cacheSolve function checks if the matrix has been previously
## solved, and then either gets the already computed value or generate it
## saves it for future use.

cacheSolve <- function(x, ...) {
  m <- x$getsolve()
  
  #Check the cache
  if(!is.null(m)) {
    message("getting cached data")
    return(m)
  }
  
  #If the cache is empty, get the matrix, solve it, cache it and return it.
  data <- x$get()    
  m <- solve(data, ...)
  x$setsolve(m)
  m
}
  
  
## The inverseMatrix function use the cacheSolve and makeCacheMatrix to
## compute the inverse of a matrix. The supplied matrix is assumed to be
## invertible.

inverseMatrix <- function(x) {
    cache<-makeCacheMatrix(x)
    cacheSolve(cache)
}
