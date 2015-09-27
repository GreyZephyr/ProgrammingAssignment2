## A cacheMatrix is a data structure that represents a matrix and it's inverse. 
## A cacheMatrix is created from a matrix with makeCacheMatrix. 
## The inverse is cached upon the first call of cacheSolve. cacheSolve calculates
## the inverse of cacheMatrix.

## matrix -> cacheMatrix Turns a matrix into a cacheMatrix

makeCacheMatrix <- function(x = matrix()) {
        m <- NULL
        set <- function(y) {
                x <<- y
                m <<- NULL
        }
        get <- function() x
        setmatrix <- function(solve) m <<- solve
        getmatrix <- function() m
        list(set = set, get = get,
             setmatrix = setmatrix,
             getmatrix = getmatrix)
}



## cacheMatrix -> matrix. Returns the inverse of a cacheMatrix

cacheSolve <- function(x=matrix(), ...) {
        ## Return a matrix that is the inverse of 'x'
        m <- x$getmatrix()
        if(!is.null(m)) {
                message("getting cached data")
                return(m)
        }
        data <- x$get()
        m <- solve(data, ...)
        x$setmatrix(m)
        m
}
