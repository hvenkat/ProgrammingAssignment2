## Creates a matrix that can cache its inverse

makeCacheMatrix <- function(x = matrix()) {
      inv <- NULL
      set <- function(y) {
            x <<- y
            inv <<- NULL
      }
      get <- function() x
      setinv <- function(solve) inv <<- solve
      getinv <- function() inv
      list(set = set, get = get,
           setinv = setinv,
           getinv= getinv)
}



## Solves the matrix. If a cache is present in the object created from the previous function, use that instead.

cacheSolve <- function(x, ...) {
      inv <- x$getinv()
      if(!is.null(inv)) {
        message("getting cached matrix...")
        return(inv)
      }
      data <- x$get()
      inv <- solve(data, ...)
      x$setinv(inv)
      inv
}