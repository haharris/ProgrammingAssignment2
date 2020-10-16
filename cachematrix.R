## These functions calculate the inverse of a specified matrix, and
## store that inverse in cache.  If the inverse has already been 
## solved and is in the cache, the value from cache will be returned.

## This function creates a "matrix" object and creates a cache for the
## inverse of a specified matrix input.  For this function, the input "x"
## should be the solveable matrix of interest, if "x" is not specified,
## an empty matrix will be created.

makeCacheMatrix <- function(x = matrix()) {
  m <- NULL
  set <- function(y) {
    x <<- y
    m <<- NULL
  }
  get <- function() x
  setsolve <- function(solve) m <<- solve
  getsolve <- function() m
  list(set = set, get = get,
       setsolve = setsolve,
       getsolve = getsolve)
}


## This function solves the inverse of the matrix returned by 
## makeCacheMatrix function.  If the inverse has already
## been solved and stored in cache this function will return 
## the inverse from cache.

cacheSolve <- function(x, ...) {
  m <- x$getsolve()
  if(!is.null(m)) {
    message("getting cached data")
    return(m)
  }
  data <- x$get()
  m <- solve(data, ...)
  x$setsolve(m)
  m
}
