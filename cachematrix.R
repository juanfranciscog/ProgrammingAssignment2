## Function makeCacheMatrix creates a matrix that:
##   1. sets the value of the matrix
##   2. gets the value of the matrix
##   3. sets the value of the inverse
##   4. gest the value of the inverse

makeCacheMatrix <- function(x = matrix()) {
  m <- NULL
  set <- function(y) {
    x <<- y
    m <<- NULL
    }
  get <- function() x
  setinverse <- function(solve) m <<- inverse
  getinverse <- function(solve) m
    list(set = set, get = get,
         setinverse = setinverse,
         getinverse = getinverse)
}



## Function cacheSolve calculates the inverse of the special "matrix" created by makeCacheMatrix
## It first checks to see if the inverese has already been calculated. 
## If so, it gets the inverse from the cache and skips the computation. 
## Otherwise, it calculates the inverse of the data and sets the value of the inverse 
## in the cache via the setinverse function.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
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
