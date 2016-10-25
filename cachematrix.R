## The following functions allow for the caching of an inverse of a matrix

## This function allows for the creation of a matrix that can cache its inverse
makeCacheMatrix <- function(x = matrix()) {
	inv <- NULL
      set <- function(y) {
      	x <<- y
      	inv <<- NULL
      }
      get <- function() x
      setInverse <- function(inverse) inv <<- inverse
      getInverse <- function() inv
      list(set = set, get = get,
           setInverse = setInverse,
           getInverse = getInverse)
}


## This function returns the inverse of a cacheMatrix. If the inverse is already 
## calculated and cached then it simply returns the cached value.
cacheSolve <- function(x, ...) {
      ## Return a matrix that is the inverse of 'x'
	inv <- x$getInverse()
      if(!is.null(inv)) {
      	message("getting cached data")
      	return(inv)
      }
      data <- x$get()
      inv <- solve(data, ...)
      x$setInverse(inv)
      inv
}
