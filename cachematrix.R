# Matrix inversion is usually a costly computation and there may be some benefit to caching the inverse of a matrix rather than 
# computing it repeatedly. The `<<-` operator is used here to assign a value to an object in an environment that is different from 
# the current environment. Below are two functions that are used to create a special object that stores a matrix and caches its inverse.

# This function creates a special "matrix" object that can cache its inverse.

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


# This function computes the inverse of the special "matrix" returned by `makeCacheMatrix` above. If the inverse has already been 
# calculated (and the matrix has not changed), then `cacheSolve` should retrieve the inverse from the cache.

cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x'
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
