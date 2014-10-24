## These functions cache the inverse of a matrix
## in order to improve performance when calling Solve on that matrix.

## This function constructs get and set functions that can be used to
## cache the inverse of a matrix

makeCacheMatrix <- function(x = matrix()) {
      s <- NULL
      set <- function(y) {
            x <<- y
            s <<- NULL
      }
      get <- function() x
      setmatrix <- function(solve) s <<- solve
      getmatrix <- function() s
      list(set = set, get = get,
           setmatrix = setmatrix,
           getmatrix = getmatrix)
}


## This function first checks to see if the matrix has been cached
## If so, it retrieves the cached matrix and returns it
## if not, it calculates the inverse and caches it before returning it

cacheSolve <- function(x, ...) {
      ## Return a matrix that is the inverse of 'x'
      s <- x$getmatrix()
      if(!is.null(s)) {
            message("getting cached data")
            return(s)
      }
      data <- x$get()
      s <- solve(data, ...)
      x$setmatrix(s)
      s
}
