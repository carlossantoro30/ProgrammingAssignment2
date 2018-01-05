##This functions can cache an inverse of a matrix.

##      This function create a list of functions that allow R to
#  cache the inverse matrix.
makeCacheMatrix <- function(x = matrix()) {
        s <- NULL
        set <- function(y) {
                x <<- y
                m <<- NULL
        }
        get <- function() x
        setinv <- function(inv) s <<- inv
        getinv <- function() s
        list(set = set, get = get,
             setinv = setinv,
             getinv = getinv)
}

# This function get the cached matrix or invert a matrix and cache it.
cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        s <- x$getinv()
        if(!is.null(s)) {
                message("getting cached data")
                return(s)
        }
        data <- x$get()
        s <- solve(data, ...)
        x$setinv(s)
        s
}
