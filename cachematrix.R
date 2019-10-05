## The 2 functions below cache the inverse of a matrix.

## The makeCacheMatrix function creates a special matrix object that can cache its inverse.

makeCacheMatrix <- function(x = matrix()) {
  invr <- NULL
  set <- function(y){
    x <<- y
    invr <<- NULL
  }
  get <- function() x
  setInverse <- function(solveMatrix) invr <<- solveMatrix
  getInverse <- function() invr
  list(set = set, get = get, setInverse = setInverse, getInverse = getInverse)
  
}


## The cacheSolve function computes the inverse of the special matrix returned by makeCacheMatrix above.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  invr <- x$getInverse()
  if(!is.null(invr)){
    message("getting cached data")
    return(invr)
  }
  data <- x$get()
  invr <- solve(data)
  x$setInverse(invr)
  invr
  }
