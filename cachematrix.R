## The provided functions create a matrix which knows how to
## cache it's inverse when needed.
## Usage:
##  m = makeCacheMatrix(mymatrix)
##  cacheSolve(m)  # computes and caches on the first call
##  cacheSolve(m)  # returns cache on subsequent calls


## Make a version of the matrix which is used to save the cache of the inverse

makeCacheMatrix <- function(x = matrix()) {
    inverse.cache <- NULL
    set <- function(y) {
        x <<- y
        inverse.cache <<- NULL
    }
    get <- function() x
    setinverse <- function(inverse) inverse.cache <<- inverse
    getinverse <- function() inverse.cache

    # return the list of functions that allow the cache to be manipulated
    list(set = set,
         get = get,
         setinverse = setinverse,
         getinverse = getinverse)
    
}


## Return the cached inverse of a matrix or
## compute and cache the inverse if not cached.

cacheSolve <- function(x, ...) {
    # see if we have the inverse cached
    inverse <- x$getinverse()
    if(!is.null(inverse)) {
        message("using cached inverse")
    } else {
        message("computing and caching inverse")
        # not cached, get the matrix and invert it
        data <- x$get()
        inverse <- solve(data, ...)
        # save the inverse in the cache
        x$setinverse(inverse)
    }
    inverse
}
