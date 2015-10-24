##***************************************************************************##
## The makeCacheMatrix function creates a special "matrix" object that       ##
## caches its inverse.                                                       ##
##                                                                           ##
## The cacheSolve function computes the inverse of the special "matrix"      ##
## returned by the makeCacheMatrix function; and if the inverse has been     ##
## calculated before and the matrix has not changed, then makeCacheMatrix    ##
## will retrieve the inverse from the cache instead of computing it.         ##
##***************************************************************************##



## The following makeCacheMatrix function creates a list to set and get the value
## of the matrix and its inverse.

makeCacheMatrix <- function(x = matrix()) {
        i <- NULL
        set <- function(y) {
                x <<- y
                i <<- NULL
        }
        get <- function() x
        setinverse <- function(inverse) i <<- inverse
        getinverse <- function() i
        list(set = set, get = get,
             setinverse = setinverse,
             getinverse = getinverse)
}

## The following cacheSolve function runs an if-loop to check if the inverse of
## the matrix provided by the previous function has been calculated; if so, it
## retrives the inverse from the cache and skips the computation; if not, it
## computes the inverse and sets the inverse in the cache via setinverse function.

cacheSolve <- function(x, ...) {
        i <- x$getinverse()
        if(!is.null(i)) {
                message("getting cached data")
                return(i)
        }
        data <- x$get()
        i <- solve(x, ...)
        x$setinverse(i)
        i
}