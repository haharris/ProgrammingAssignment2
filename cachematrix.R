## These functions calculate the inverse of a specified matrix, and
## store that inverse in cache.  If the inverse has already been 
## solved and is in the cache, the value from cache will be returned.

## This function creates a "matrix" object and creates a cache for the
## inverse of a specified matrix input.  In order to return the inverse
## stored in the cache, the cacheSolve function needs to be run on the 
## matrix that was run through makeCacheMatrix.
## (example input: a <- makeCacheMarix(b) **where "b" is some already
## defined/specified solveable matrix**)

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


## This function computes the inverse of the matrix returned by 
## makeCacheMatrix function.  If the inverse has already
## been solved and stored in cache created by makeCacheMatrix 
## this function will return the inverse from cache.
## (example input: cacheSolve(a) **see above for "a"**; will return
## the inverse of the matrix b, either newly calculated or from cache)

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
