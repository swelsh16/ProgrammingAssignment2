## These functions allow for "caching" a matrix's inverse, to save time on computation when we need to refer to the inverse repeatedly.  
## So instead of needing to calculate it every time, the inverse is saved for future reference, so that when it's needed, it can be called up.

makeCacheMatrix <- function(x = matrix()) { 
  m <- NULL
  set <- function(y) {
    x <<- y
    m <<- NULL
  }
  get <- function() x
  setInverse <- function(inverse) m <<- inverse
  getInverse <- function() m
  list(set = set, get = get,
       setInverse = setInverse,
       getInverse = getInverse)
}


## If the inverse of a matrix has already been calculated by makeCacheMatrix, cacheSolve retrieves the inverse rather than recalculating it. 
## If it has not yet been calculated, it calculates it using the solve function.

cacheSolve <- function(x, ...) {
  m <- x$getInverse()
  if(!is.null(m)) {
    message("getting cached data")
    return(m)
  }
  matrix <- x$get()
  m <- solve(matrix)
  x$setInverse(m)
  m
}
