## file: cachematrix.R
## Functions that manipulate a matrix in order to calculate its inverse. 
## In order to improve performance, if the inverse of the matrix has been 
## previously calculated, it is stored in a cache matrix. 

## This function creates a special "matrix" object that can cache its inverse. 
## Requires a matrix as an input parameter.
## Returns a list composed of the following elements: 
## set the matrix data, get the matrix data
## setinverse to store the matrix inverse 
## getinverse to return the matrix inverse

makeCacheMatrix <- function(x = matrix()) {
  inverse <- NULL
  set <- function(y) {
    x <<- y
    inverse <<- NULL
  }
  get <- function() x
  setinverse <- function(solve) inverse <<- solve
  getinverse <- function() inverse
  list(set = set, get = get,
       setinverse = setinverse,
       getinverse = getinverse)

}


## This function computes the inverse of the special "matrix" returned by makeCacheMatrix. 
## If the inverse has already been calculated (and the matrix has not changed), 
## then the cachesolve should retrieve the inverse from the cache.
## Requires a matrix returned by makeCacheMatrix.
## Returns the inverse of the matrix (cached or not)
cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  inverse <- x$getinverse()
  if(!is.null(inverse)) {
    message("getting cached data")
    return(inverse)
  }
  data <- x$get()
  inverse <- solve(data, ...)
  x$setinverse(inverse)
  inverse
}
