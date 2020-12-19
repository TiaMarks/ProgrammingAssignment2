## These functions combined together can create a matrix
## and cache its inverse.

## makeCacheMatrix can be used to create a matrix
## that is ready to get its inverse.

makeCacheMatrix <- function(x = matrix()) {
        s <- NULL
        set <- function(y) {
          x <<- y
          s <<- NULL
        }
        get <- function() x
        setsolve <- function(solve) s <<- solve
        getsolve <- function() s
        list(set = set, get = get,
             setsolve = setsolve,
             getsolve = getsolve)
}


## cacheSolve uses the function Solve to get the inverse of a given matrix
## and then caches it.

cacheSolve <- function(x, ...) {
        s <- x$getsolve()
        if(!is.null(s)) {
          message("getting cached data")
          return(s)
        }
        data <- x$get()
        s <- solve(data, ...)
        x$setsolve(s)
        s
}
