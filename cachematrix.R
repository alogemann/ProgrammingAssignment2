## The following functions calculate the inverse of a given matrix
## 

## The first function sets a list of functions to set a special matrix that will be cached 
## after the inverse is calculated and that can be retrieved if needed.

makeCacheMatrix <- function(x = matrix()) {
                      m <- NULL
                      set <- function(y) {
                        x <<- y
                        m <<- NULL
                      }
                      get <- function() x
                      setinverse <- function(mean) m <<- mean
                      getinverse <- function() m
                      list(set = set, get = get,
                           setinverse = setinverse,
                           getinverse = getinverse)
  }


## the second function is called with the object of the first and calculates the inverse of the given matrix
## and caches the result, or gives the cached result if the inverse is already stored.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
                      m <- x$getinverse()
                      if(!is.null(m)) {
                        print("getting cached data")
                        return(m)
                      }
                      data <- x$get()
                      m <- solve(data, ...)
                      x$setinverse(m)
                      m
}
