## This function provides caching for matrix inversion
## Solution is based on code for vector based cache
## This function will create cache enabled matrix

makeCacheMatrix <- function(x = matrix()) {
  m <- NULL
  set <- function(y) {
    x <<- y
    m <<- NULL
  }
  get <- function() x
  setinverse <- function(inv) m <<- inv
  getinverse <- function() m
  list(set = set, get = get,
       setinverse = setinverse,
       getinverse = getinverse)
}

## This function provides cached values of inversed matrix
## In case there is no inversed value in cache, it
## will inverse matrix using solve function

## You can test it following way
## > z <- makeCacheMatrix(rbind(c(1, -1/4), c(-1/4, 1)) )
## > cacheSolve(z)
##           [,1]      [,2]
## [1,] 1.0666667 0.2666667
## [2,] 0.2666667 1.0666667
## > cacheSolve(z)
## getting cached data
##          [,1]      [,2]
## [1,] 1.0666667 0.2666667
## [2,] 0.2666667 1.0666667

cacheSolve <- function(x, ...) {
  m <- x$getinverse()
  if(!is.null(m)) {
    message("getting cached data")
    return(m)
  }
  data <- x$get()
  m <- solve(data, ...)
  x$setinverse(m)
  m
}
