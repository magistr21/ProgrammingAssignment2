## System of 2 functions for calculate matrix inversion and cache computations

## Function for cashing computations and set list of functions

makeCacheMatrix <- function(x = data.frame()) {
  inverse <- NULL

#set functions

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


## Function for calculating matrix inverse or recieve cashed inverse

cacheSolve <- function(x, ...) {
  inverse <- x$getinverse()

##if inverse already calculated then use cache

  if(!is.null(inverse)) {
    message("getting cached data")
    return(inverse)
  }

##else calculate inverse

  data <- x$get()
  inverse <- solve(data, ...)
  x$setinverse(inverse)
  return(inverse)
}


