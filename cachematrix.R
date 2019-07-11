## These functions 

## this function creates function type 'makeCacheMatrix' and saves the matrix
## with lexical scoping

browser()
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


## this function checks to see if the the matrix 'x' is already cached

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
