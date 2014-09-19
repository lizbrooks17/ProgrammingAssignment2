## These functions cache and return the inverse of a matrix. Future calls
## are able to return the cached value, without having to re-calculate the 
## inverse.

## Based on sample code in ProgrammingAssignment2 by Roger D. Peng.

## makeCacheMatrix takes a matrix and returns a list containing functions 
## that allow you to access the matrix and it's inverse
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


## given a list created by makeCacheMatrix, returns the inverse of the 
## matrix represented by that list
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
