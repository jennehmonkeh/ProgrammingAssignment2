## 
## 
## function makeCacheMatrix: Initialize a matrix function with a matrix
## the matrix must be able to be inverted or an error will occur

makeCacheMatrix <- function(x = matrix()) {
  m <- NULL
  # set reinitizializes x & m, resets the cached matrix
  set <- function(y) {
    x <<- y
    m <<- NULL
  }
  ## return the original matrix
  get <- function() x
  ## use ftn "solve" to invert the matrix
  setinv <- function(solve) m <<- solve
  ## return the cached inverted matrix or null if empty
  getinv <- function() m
  ## create the S3 Object "list"
  ## names the functions available and the associated variables in memory:
  list(set = set, get = get,
       setinv = setinv,
       getinv = getinv)
}


## function cacheSolve: return the inverse of the matrix and cache it
## if matrix already in the cache, return the cache

cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x'
  m <- x$getinv()
  if(!is.null(m)) {
    message("getting cached data")
    return(m)
  }
  data <- x$get()
  m <- solve(data, ...)
  x$setinv(m)
  m
}
