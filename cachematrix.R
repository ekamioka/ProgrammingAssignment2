## Matrix inversion is usually a costly computation and there may be some benefit to caching the inverse of a matrix
## rather than computing it repeatedly. So this pair of functions caches the inverse of a matrix.

## This function creates a special "matrix" object that can cache its inverse.

makeCacheMatrix <- function(x = matrix()) {
        m <- NULL
        set <- function(y) {
                x <<- y
                m <<- NULL
        }
        get <- function() x
        
        invm <- function(solve) m <<- solve
        getm <- function() m
        
        list(set = set, get = get,
             invm = invm,
             getm = getm)
}

## This function computes the inverse of the special "matrix" returned by makeCacheMatrix above. 
## If the inverse has already been calculated (and the matrix has not changed), then the 
## should retrieve the inverse from the cache.

cacheSolve <- function(x, ...) {
        m <- x$getm()
        if(!is.null(m)) {
                message("getting cached data")
                return(m)
        }
        data <- x$get()
        m <- solve(data, ...)
        x$invm(m)
        m
}
